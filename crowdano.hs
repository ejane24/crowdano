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

import           Control.Monad        hiding (fmap)
import           Data.Aeson           (ToJSON, FromJSON)
import           Data.Default         as Default
import           Data.Map             as Map
import           Data.Text            (Text)
import           Data.Void            (Void)
import           GHC.Generics         (Generic)
import qualified Ledger.Interval      as Interval
import           Ledger.TimeSlot      as TimeSlot
import qualified Prelude              as Haskell
import           Plutus.Contract
import           PlutusTx             (Data (..))
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup(..), unless, (<$>))
import           Ledger               (POSIXTime, POSIXTimeRange, PubKeyHash, 
                                       Validator, txId, TxInfo, ScriptContext,
                                       pubKeyHash)
import qualified Ledger               hiding (singleton)
import           Ledger.Constraints   as Constraints
import qualified Ledger.Typed.Scripts as Scripts hiding (validatorHash)
import           Ledger.Ada           as Ada
import           Ledger.Value         (Value)
import qualified Ledger.Value         as Value
import qualified Ledger.Scripts       as Scripts
import           Playground.Contract  (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
import qualified Plutus.Contract.Typed.Tx    as Typed
import           Playground.TH        (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types     (KnownCurrency (..))
import           Prelude              (IO, Semigroup (..), Show (..), String, Int, (<$>))
import           Text.Printf          (printf)
import qualified Wallet.Emulator      as Emulator

data Campaign = Campaign
    { campaignDeadline           :: POSIXTime
    , campaignTarget             :: Value
    , campaignCollectionDeadline :: POSIXTime
    , campaignOwner              :: PubKeyHash
    , initialFunding             :: Value
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

PlutusTx.makeLift ''Campaign

data CampaignAction = Collect | Refund

PlutusTx.unstableMakeIsData ''CampaignAction
PlutusTx.makeLift ''CampaignAction

type CrowdfundingSchema =
    Endpoint "schedule collection" Campaign
        .\/ Endpoint "contribute" Contribution

data Contribution = Contribution
        { cmpDeadline           :: POSIXTime
        , cmpTarget             :: Value
        , cmpCollectionDeadline :: POSIXTime
        , cmpOwner              :: PubKeyHash
        , initFunding           :: Value
        , contribValue               :: Value
        } deriving stock (Haskell.Eq, Show, Generic)
          deriving anyclass (ToJSON, FromJSON, ToSchema)

data ContribData = ContribData
        { contributors :: [String]
        }

PlutusTx.makeLift ''ContribData

mkCampaign :: POSIXTime -> Value -> POSIXTime -> PubKeyHash -> Value -> Campaign
mkCampaign ddl target collectionDdl ownerPubKeyHash initFundingAmt =
    Campaign
        { campaignDeadline = ddl
        , campaignTarget   = target
        , campaignCollectionDeadline = collectionDdl
        , campaignOwner = ownerPubKeyHash
        , initialFunding = initFundingAmt
        }

{-# INLINABLE collectionRange #-}
collectionRange :: Campaign -> POSIXTimeRange
collectionRange cmp =
    Interval.interval (campaignDeadline cmp) (campaignCollectionDeadline cmp - 1)

{-# INLINABLE refundRange #-}
refundRange :: Campaign -> POSIXTimeRange
refundRange cmp =
    Interval.from (campaignCollectionDeadline cmp)

data Crowdfunding
instance Scripts.ValidatorTypes Crowdfunding where
    type instance RedeemerType Crowdfunding = CampaignAction
    type instance DatumType Crowdfunding = PubKeyHash

scriptInstance :: Campaign -> Scripts.TypedValidator Crowdfunding
scriptInstance cmp = Scripts.mkTypedValidator @Crowdfunding
    ($$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode cmp)
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.wrapValidator

{-# INLINABLE validRefund #-}
validRefund :: Campaign -> PubKeyHash -> TxInfo -> Bool
validRefund campaign contributor txinfo =
    traceIfFalse "Invalid slot" (refundRange campaign `Interval.contains` Ledger.txInfoValidRange txinfo)
    && traceIfFalse "Not signed by contributor" (txinfo `Ledger.txSignedBy` contributor)

validCollection :: Campaign -> TxInfo -> Bool
validCollection campaign txinfo = True
--    traceIfFalse "Invalid Range" (collectionRange campaign `Interval.contains` Ledger.txInfoValidRange txinfo)
--    && traceIfFalse "Target not reached" (Ledger.valueSpent txinfo `Value.geq` campaignTarget campaign)
--    && traceIfFalse "Not Owner" (txinfo `Ledger.txSignedBy` campaignOwner campaign)

{-# INLINABLE mkValidator #-}
mkValidator :: Campaign -> PubKeyHash -> CampaignAction -> ScriptContext -> Bool
mkValidator c con act p = case act of
    Refund  -> validRefund c con (Ledger.scriptContextTxInfo p)
    Collect -> validCollection c (Ledger.scriptContextTxInfo p)

contributionScript :: Campaign -> Validator
contributionScript = Scripts.validatorScript . scriptInstance

campaignAddress :: Campaign -> Ledger.ValidatorHash
campaignAddress = Scripts.validatorHash . contributionScript

crowdfunding :: AsContractError e => Contract () CrowdfundingSchema e ()
crowdfunding = contribute `select` scheduleCollection

contribute :: AsContractError e => Contract () CrowdfundingSchema e ()
contribute = do
    Contribution{cmpDeadline, cmpTarget, cmpCollectionDeadline, cmpOwner, initFunding, contribValue} <- endpoint @"contribute"
    let cmp = mkCampaign cmpDeadline cmpTarget cmpCollectionDeadline cmpOwner initFunding
    contributor <- pubKeyHash <$> ownPubKey
    let inst = scriptInstance cmp
        tx = Constraints.mustPayToTheScript contributor contribValue
                <> Constraints.mustValidateIn (Ledger.interval 1 (campaignDeadline cmp))
    txid <- fmap txId (submitTxConstraints inst tx)

    utxo <- watchAddressUntilTime (Scripts.validatorAddress inst) $ (campaignCollectionDeadline cmp)

    let flt Ledger.TxOutRef{txOutRefId} _ = txid Haskell.== txOutRefId
        tx' = Typed.collectFromScriptFilter flt utxo Refund
                <> Constraints.mustValidateIn (refundRange cmp)
                <> Constraints.mustBeSignedBy contributor
    if Constraints.modifiesUtxoSet tx'
    then do
        logInfo @Text "Claiming Refund"
        void (submitTxConstraintsSpending inst utxo tx')
    else pure ()

ownFunds :: AsContractError e => Campaign -> Contract () CrowdfundingSchema e Value
ownFunds cmp = do
    let inst = scriptInstance cmp
    utxos <- utxoAt (Scripts.validatorAddress inst)
    let v = mconcat $ Map.elems $ Ledger.txOutValue . Ledger.txOutTxOut <$> utxos
    return v

--getContributors :: AsContractError e => Campaign -> Contract () CrowdfundingSchema e ContribData
--getContributors cmp = do
--    let inst = scriptInstance cmp
--    utxos <- utxoAt (Scripts.validatorAddress inst)
--    let outputs = Map.elems $ Ledger.txOutputs . Ledger.txOutTxTx <$> utxos
--        a = Map.map (Map.map Ledger.txOutAddress) (outputs)
--        b = Map.map (Map.map Ledger.addressCredential) a
--        c = Map.map (Map.map (substring 17 . show)) b
--        d = Map.map (!! 0) c
--        contribs = ContribData{contributors = d}
--    return contribs

--substring :: Int -> String -> String
--substring i s = ( drop i s )


scheduleCollection :: AsContractError e => Contract () CrowdfundingSchema e ()
scheduleCollection = do
    cmp <- endpoint @"schedule collection"
    let inst = scriptInstance cmp
    logInfo @String "Campaign Started"
    logInfo $ (TimeSlot.slotToBeginPOSIXTime Default.def 15)
    _ <- awaitTime $ campaignDeadline cmp
    logInfo @String "Retrieving"
    unspentOutputs <- utxoAt (Scripts.validatorAddress inst)
    v <- ownFunds cmp
    logInfo v
--    contribs <- getContributors cmp
    let fee = (Ada.divide (Ada.fromValue v) 100) * 2
    logInfo unspentOutputs
    let tx = Typed.collectFromScript unspentOutputs Collect
            <> Constraints.mustPayToPubKey (campaignOwner cmp) (initialFunding cmp)
            <> Constraints.mustPayToPubKey (pubKeyHash $ Emulator.walletPubKey (Emulator.Wallet 3)) (Ada.toValue fee)
            <> Constraints.mustPayToTheScript (campaignOwner cmp) (v - ((initialFunding cmp) + (Ada.toValue fee)))
            <> Constraints.mustValidateIn (collectionRange cmp)
    void $ submitTxConstraintsSpending inst unspentOutputs tx
    logInfo @String "Collected funds"

endpoints :: AsContractError e => Contract () CrowdfundingSchema e ()
endpoints = crowdfunding

mkSchemaDefinitions ''CrowdfundingSchema

mkKnownCurrencies []
