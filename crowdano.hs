-- Crowdfunding contract implemented using the [[Plutus]] interface.
-- This is the fully parallel version that collects all contributions
-- in a single transaction.
--
-- Note [Transactions in the crowdfunding campaign] explains the structure of
-- this contract on the blockchain.

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

import           Control.Applicative         (Applicative (pure))
import           Control.Monad               (void)
import           Ledger                      (PubKeyHash, ScriptContext (..), TxInfo (..), Validator, pubKeyHash, txId,
                                              valueSpent)
import qualified Ledger                      as Ledger
import qualified Ledger.Ada                  as Ada
import qualified Ledger.Contexts             as V
import qualified Ledger.Interval             as Interval
import qualified Ledger.Scripts              as Scripts
import           Ledger.Slot                 (Slot, SlotRange)
import qualified Ledger.Typed.Scripts        as Scripts
import           Ledger.Value                (Value)
import qualified Ledger.Value                as Value
import           Playground.Contract
import           Plutus.Contract             as Contract
import qualified Plutus.Contract.Constraints as Constraints
import qualified Plutus.Contract.Typed.Tx    as Typed
import qualified PlutusTx                    as PlutusTx
import           PlutusTx.Prelude            hiding (Applicative (..), Semigroup (..), (<$>))
import           Prelude                     (Semigroup (..), (<$>))
import qualified Prelude                     as Haskell
import qualified Wallet.Emulator             as Emulator
import qualified Data.Map                    as Map
import           Data.Text                   (Text)

data Campaign = Campaign
    { campaignDeadline           :: Slot
    , campaignTarget             :: Value
    , campaignCollectionDeadline :: Slot
    , campaignOwner              :: PubKeyHash
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

PlutusTx.makeLift ''Campaign

data CampaignAction = Collect | Refund

PlutusTx.unstableMakeIsData ''CampaignAction
PlutusTx.makeLift ''CampaignAction

type CrowdfundingSchema =
    BlockchainActions
        .\/ Endpoint "schedule collection" Campaign
        .\/ Endpoint "contribute" Contribution

data Contribution = Contribution
        { cmpDeadline           :: Slot
        , cmpTarget             :: Value
        , cmpCollectionDeadline :: Slot
        , cmpOwner              :: PubKeyHash
        , contribValue               :: Value
        } deriving stock (Haskell.Eq, Show, Generic)
          deriving anyclass (ToJSON, FromJSON, ToSchema)

mkCampaign :: Slot -> Value -> Slot -> PubKeyHash -> Campaign
mkCampaign ddl target collectionDdl ownerPubKeyHash =
    Campaign
        { campaignDeadline = ddl
        , campaignTarget   = target
        , campaignCollectionDeadline = collectionDdl
        , campaignOwner = ownerPubKeyHash
        }

collectionRange :: Campaign -> SlotRange
collectionRange cmp =
    Interval.interval (campaignDeadline cmp) (campaignCollectionDeadline cmp)

refundRange :: Campaign -> SlotRange
refundRange cmp =
    Interval.from (campaignCollectionDeadline cmp)

data Crowdfunding
instance Scripts.ScriptType Crowdfunding where
    type instance RedeemerType Crowdfunding = CampaignAction
    type instance DatumType Crowdfunding = PubKeyHash

scriptInstance :: Campaign -> Scripts.ScriptInstance Crowdfunding
scriptInstance = Scripts.validatorParam @Crowdfunding
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.wrapValidator

{-# INLINABLE validRefund #-}
validRefund :: Campaign -> PubKeyHash -> TxInfo -> Bool
validRefund campaign contributor txinfo =
    traceIfFalse "Invalid slot" (Interval.contains (refundRange campaign) (txInfoValidRange txinfo))
    && traceIfFalse "Not signed by contributor" (txinfo `V.txSignedBy` contributor)

validCollection :: Campaign -> TxInfo -> Bool
validCollection campaign txinfo =
    traceIfFalse "Invalid Range" (collectionRange campaign `Interval.contains` txInfoValidRange txinfo)
    && traceIfFalse "Target not reached" (valueSpent txinfo `Value.geq` campaignTarget campaign)
    && traceIfFalse "Not Owner" (txinfo `V.txSignedBy` campaignOwner campaign)

mkValidator :: Campaign -> PubKeyHash -> CampaignAction -> ScriptContext -> Bool
mkValidator c con act p = case act of
    Refund  -> validRefund c con (scriptContextTxInfo p)
    Collect -> validCollection c (scriptContextTxInfo p)

contributionScript :: Campaign -> Validator
contributionScript = Scripts.validatorScript . scriptInstance

campaignAddress :: Campaign -> Ledger.ValidatorHash
campaignAddress = Scripts.validatorHash . contributionScript

crowdfunding :: AsContractError e => Contract () CrowdfundingSchema e ()
crowdfunding = contribute `select` scheduleCollection

contribute :: AsContractError e => Contract () CrowdfundingSchema e ()
contribute = do
    Contribution{cmpDeadline, cmpTarget, cmpCollectionDeadline, cmpOwner, contribValue} <- endpoint @"contribute"
    let cmp = mkCampaign cmpDeadline cmpTarget cmpCollectionDeadline cmpOwner
    contributor <- pubKeyHash <$> ownPubKey
    let inst = scriptInstance cmp
        tx = Constraints.mustPayToTheScript contributor contribValue
                <> Constraints.mustValidateIn (Ledger.interval 1 (campaignDeadline cmp))
    txid <- fmap txId (submitTxConstraints inst tx)

    utxo <- watchAddressUntil (Scripts.scriptAddress inst) (campaignCollectionDeadline cmp)


    let flt Ledger.TxOutRef{txOutRefId} _ = txid Haskell.== txOutRefId
        tx' = Typed.collectFromScriptFilter flt utxo Refund
                <> Constraints.mustValidateIn (refundRange cmp)
                <> Constraints.mustBeSignedBy contributor
    if Constraints.modifiesUtxoSet tx'
    then void (submitTxConstraintsSpending inst utxo tx')
    else pure ()

ownFunds :: AsContractError e => Campaign -> Contract () CrowdfundingSchema e Value
ownFunds cmp = do
    let inst = scriptInstance cmp
    utxos <- utxoAt (Scripts.scriptAddress inst)
    let v = mconcat $ Map.elems $ V.txOutValue . Ledger.txOutTxOut <$> utxos
    return v

scheduleCollection :: AsContractError e => Contract () CrowdfundingSchema e ()
scheduleCollection = do
    cmp <- endpoint @"schedule collection"
    let inst = scriptInstance cmp
    _ <- awaitSlot (campaignDeadline cmp)
    unspentOutputs <- utxoAt (Scripts.scriptAddress inst)
    v <- ownFunds cmp
    let fee = (Ada.divide (Ada.fromValue v) 100) * 2

    let tx = Typed.collectFromScript unspentOutputs Collect
            <> Constraints.mustPayToPubKey (campaignOwner cmp) (v - (Ada.toValue fee))
            <> Constraints.mustPayToPubKey (pubKeyHash $ Emulator.walletPubKey (Emulator.Wallet 3)) (Ada.toValue fee)
            <> Constraints.mustValidateIn (collectionRange cmp)
    void $ submitTxConstraintsSpending inst unspentOutputs tx

endpoints :: AsContractError e => Contract () CrowdfundingSchema e ()
endpoints = crowdfunding

mkSchemaDefinitions ''CrowdfundingSchema

$(mkKnownCurrencies [])
