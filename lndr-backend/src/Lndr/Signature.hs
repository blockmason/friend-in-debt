module Lndr.Signature where

import           Data.Text (Text)
import qualified Data.Text as T
import           Lndr.Types
import           Lndr.Util
import           Network.Ethereum.Web3.Address
import qualified Network.Ethereum.Util as EU

class VerifiableSignature a where
     recoverSigner :: a -> Either String Address
     recoverSigner x = fmap textToAddress . EU.ecrecover (extractSignature x) . generateHash $ x

     extractSignature :: a -> Text

     generateHash :: a -> Text

     generateSignature :: a -> Text -> Either String Text
     generateSignature request = EU.ecsign (generateHash request)

instance VerifiableSignature NickRequest where
    extractSignature (NickRequest _ _ sig) = sig

    generateHash (NickRequest addr nick _) = EU.hashText . T.concat $
                                                stripHexPrefix <$> [ T.pack (show addr)
                                                                   , bytesEncode nick
                                                                   ]

-- AddFriendRequest
-- RemoveFriendRequest

instance VerifiableSignature PushRequest where
    extractSignature (PushRequest _ _ _ sig) = sig

    generateHash (PushRequest channelID platform addr _) = EU.hashText . T.concat $
        (stripHexPrefix . bytesEncode) <$> [ channelID,  platform , T.pack (show addr) ]
