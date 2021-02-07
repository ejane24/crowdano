-- License: CC-BY-NC-SA
-- Crowdano plutus smart contract

module Crowdano where

import qualified Language.PlutusTx as PlutusTx
import Ledger

data Campaign = Campaign {
    campignDeadline :: Slot,
    campaignTarget :: Value,
    campaignOwner :: PubKeyHash,
    campaignInitialFunding :: Value
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

PlutusTx.makeLift ''Campaign

data CampaignAction = StartVote | Vote | Collect | Contribute

PlutusTx.makeIsData ''CampaignAction
PlutusTx.makeLift ''CampaignAction 

type CrowdfundingSchema = 
    BlockchainActions
        .\/ Endpoint "Start Vote" Request
        .\/ Endpoint "Vote" Vote
        .\/ Endpoint "Collect"
        .\/ Endpoint "Contribute" Contribution

newtype Request = Request {
    requestAmount :: Value
    } deriving stock (Haskell.Eq, Show, Generic)
      deriving anyclass (ToJSON, FromJSON, IotsType, ToSchema, ToArgument)

newtype Vote = Vote {
    voteInFavour :: Bool
    } deriving stock (Haskell.Eq, Show, Generic)
      deriving anyclass (ToJSON, FromJSON, IotsType, ToSchema, ToArgument)

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


