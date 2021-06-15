-- License: CC-BY-NC-SA
-- Crowdano plutus smart contract

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:debug-context #-}

import           Control.Applicative      (Applicative (..))
import           Control.Monad            (void)
import           Data.Aeson               (FromJSON, ToJSON)
import           Data.Text                (Text)
import qualified Data.Text                as Text
import           GHC.Generics             (Generic)

import           Ledger                   (PubKeyHash, Slot, SlotRange, Validator, txId)
import qualified Ledger
import qualified Ledger.Ada               as Ada
import qualified Ledger.Constraints       as Constraints
import           Ledger.Contexts          as V
import qualified Ledger.Interval          as Interval
import qualified Ledger.Scripts           as Scripts
import qualified Ledger.TimeSlot          as TimeSlot
import qualified Ledger.Typed.Scripts     as Scripts hiding (validatorHash)
import           Ledger.Value             (Value)
import           Plutus.Contract
import qualified Plutus.Contract.Typed.Tx as Typed
import           Plutus.Trace.Emulator    (ContractHandle, EmulatorTrace)
import qualified Plutus.Trace.Emulator    as Trace
import qualified PlutusTx
import           PlutusTx.Prelude         hiding (Applicative (..), Semigroup (..), return, (<$>), (>>), (>>=))
import           Prelude                  (Semigroup (..))
import qualified Prelude                  as Haskell
import           Schema                   (ToArgument, ToSchema)
import           Wallet.Emulator          (Wallet (..))
import qualified Wallet.Emulator          as Emulator


module Crowdano where

data Campaign = Campaign {
    campignDeadline :: Slot,
    campaignTarget :: Value,
    campaignOwner :: PubKeyHash,
    campaignInitialFunding :: Value
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

PlutusTx.makeLift ''Campaign

data CampaignAction = ScheduleCollection | Contribute

PlutusTx.unstableMakeIsData ''CampaignAction
PlutusTx.makeLift ''CampaignAction 

type CrowdfundingSchema = 
    BlockchainActions
        .\/ Endpoint "Start Vote" Request
        .\/ Endpoint "Vote" Vote
        .\/ Endpoint "Schedule Collection" ()
        .\/ Endpoint "Contribute" Contribution

newtype Contribution = Contribution {
    contribValue :: Value
    } deriving stock (Haskell.Eq, Show, Generic)
      deriving anyclass (ToJSON, FromJSON, IotsType, ToSchema, ToArgument)

mkCampaign :: Slot -> Value -> Wallet -> Value -> Campaign
mkCampaign deadline target ownerWallet initialFunding = 
    Campaign {
        campaignDeadline = deadline,
        campaignTarget = target,
        campaignOwner = PubKeyHash $ Emulator.walletPubKey ownerWallet,
        campaignInitialFunding = initialFunding
        }

data Crowdfunding
instance Scripts.ValidatorTypes Crowdfunding where
    type instance RedeemerType Crowdfunding = CampaignAction
    type instance DatumType Crowdfunding = PubKeyHash

typedValidator :: Campaign -> Scripts.TypedValidator Crowdfunding
typedValidator = Scripts.mkTypedValidatorParam @Crowdfunding
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.wrapValidator

{-# INLINABLE validRefund #-}
validRefund :: Campaign -> PubKeyHash -> TxInfo -> Bool
validRefund campaign contributor txinfo =
    -- Check that the transaction falls in the refund range of the campaign
    Interval.contains (refundRange campaign) (TimeSlot.posixTimeRangeToSlotRange $ txInfoValidRange txinfo)
    -- Check that the transaction is signed by the contributor
    && (txinfo `V.txSignedBy` contributor)

{-# INLINABLE validCollection #-}
validCollection :: Campaign -> TxInfo -> Bool
validCollection campaign txinfo =
    -- Check that the transaction falls in the collection range of the campaign
    (collectionRange campaign `Interval.contains` TimeSlot.posixTimeRangeToSlotRange (txInfoValidRange txinfo))
    -- Check that the transaction is signed by the campaign owner
    && (txinfo `V.txSignedBy` campaignOwner campaign)

{-# INLINABLE mkValidator #-}
mkValidator :: Campaign -> PubKeyHash -> CampaignAction -> ScriptContext -> Bool
mkValidator c con act ScriptContext{scriptContextTxInfo} = case act of
    ScheduleCollection -> validCollection c scriptContextTxInfo
    Contribute -> validContribution c con scriptContextTxInfo

contributionScript :: Campaign -> Validator
contributionScript = Scripts.validatorScript . typedValidator

campaignAddress :: Campaign -> Ledger.ValidatorHash
campaignAddress = Scripts.validatorHash . contributionScript
