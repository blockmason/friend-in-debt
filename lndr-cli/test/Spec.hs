{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}

module Main where

import           Control.Concurrent (threadDelay)
import qualified Data.Text.Lazy as LT
import           Lndr.CLI.Args
import           Lndr.EthInterface (textToAddress, hashCreditRecord)
import           Lndr.Types
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit hiding (Test)

-- TODO get rid of this once version enpoint point works
ucacAddr = "0x7899b83071d9704af0b132859a04bb1698a3acaf"

testUrl = "http://localhost:80"
testPrivkey1 = "7231a774a538fce22a329729b03087de4cb4a1119494db1c10eae3bb491823e7"
testPrivkey2 = "f581608ccd4dcd78e341e464b86f268b77ee2673acc705023e64eeb5a4e31490"
testAddress1 = textToAddress . userFromSK . LT.fromStrict $ testPrivkey1
testAddress2 = textToAddress . userFromSK . LT.fromStrict $ testPrivkey2
testSearch = "test"
testNick1 = "test1"
testNick2 = "test2"


main :: IO ()
main = defaultMain tests


tests :: [Test]
tests = [ testGroup "Nicks"
            [ testCase "setting nicks and friends" nickTest
            ]
        , testGroup "Credits"
            [ testCase "lend money to friend" basicLendTest
            ]
        , testGroup "Admin"
            [ testCase "get and set gas price" basicGasTest
            ]
        , testGroup "Notifications"
            [ testCase "registerChannel" basicNotificationsTest
            ]
        ]


nickTest :: Assertion
nickTest = do
    -- check that nick is available
    nickTaken <- takenNick testUrl testNick1
    assertBool "after db reset all nicks are available" (not nickTaken)
    -- set nick for user1
    httpCode <- setNick testUrl (NickRequest testAddress1 testNick1 "")
    assertEqual "add friend success" 204 httpCode
    -- check that test nick is no longer available
    nickTaken <- takenNick testUrl testNick1
    assertBool "nicks already in db are not available" nickTaken
    -- check that nick for user1 properly set
    queriedNick <- getNick testUrl testAddress1
    assertEqual "nick is set and queryable" queriedNick testNick1
    -- fail to set identical nick for user2
    httpCode <- setNick testUrl (NickRequest testAddress2 testNick1 "")
    assertBool "duplicate nick is rejected with user error" (httpCode /= 204)
    -- change user1 nick
    httpCode <- setNick testUrl (NickRequest testAddress1 testNick2 "")
    assertEqual "change nick success" 204 httpCode
    -- check that user1's nick was successfully changed
    queriedNick <- getNick testUrl testAddress1
    assertEqual "nick is set and queryable" queriedNick testNick2

    -- set user2's nick
    httpCode <- setNick testUrl (NickRequest testAddress2 testNick1 "")
    assertEqual "previously used nickname is settable" 204 httpCode

    fuzzySearchResults <- searchNick testUrl testSearch
    assertEqual "search returns both results" 2 $ length fuzzySearchResults

    -- user1 adds user2 as a friend
    httpCode <- addFriend testUrl testAddress1 testAddress2
    assertEqual "add friend success" 204 httpCode
    -- verify that friend has been added
    friends <- getFriends testUrl testAddress1
    print friends
    -- threadDelay 1000000 -- delay one second
    -- assertEqual "friend properly added" [testAddress2] ((\(NickInfo addr _) -> addr) <$> friends)


basicLendTest :: Assertion
basicLendTest = do
    let testCredit = CreditRecord testAddress1 testAddress2 100 "dinner" testAddress1 0 "" ""
        creditHash = hashCreditRecord ucacAddr (Nonce 0) testCredit
    -- user1 submits pending credit to user2
    httpCode <- submitCredit testUrl ucacAddr testPrivkey1 testCredit
    assertEqual "lend success" 204 httpCode

    -- user1 checks pending transactions
    creditRecords1 <- checkPending testUrl testAddress1
    assertEqual "one pending record found for user1" 1 (length creditRecords1)

    -- user2 checks pending transactions
    creditRecords2 <- checkPending testUrl testAddress2
    assertEqual "one pending record found for user2" 1 (length creditRecords2)

    -- user2 rejects pending transaction
    httpCode <- rejectCredit testUrl testPrivkey1 creditHash
    assertEqual "reject success" 204 httpCode

    -- user2 has 0 pending records post-rejection
    creditRecords2 <- checkPending testUrl testAddress2
    assertEqual "zero pending records found for user2" 0 (length creditRecords2)

    -- user1 attempts same credit again
    httpCode <- submitCredit testUrl ucacAddr testPrivkey1 testCredit
    assertEqual "lend success" 204 httpCode

    -- user2 accepts user1's pending credit
    httpCode <- submitCredit testUrl ucacAddr testPrivkey2 (testCredit { submitter = testAddress2 })
    assertEqual "borrow success" 204 httpCode

    -- user1's checks that he has pending credits and one verified credit
    creditRecords1 <- checkPending testUrl testAddress1
    assertEqual "zero pending records found for user1" 0 (length creditRecords1)

    verifiedRecords1 <- getTransactions testUrl testAddress1
    assertEqual "one pending record found for user1" 1 (length verifiedRecords1)


basicGasTest :: Assertion
basicGasTest = do
    price <- getGasPrice testUrl

    -- double gas price
    httpCode <- setGasPrice testUrl testAddress1 (price * 2)
    assertEqual "add friend success" 204 httpCode

    -- check that gas price has been doubled
    newPrice <- getGasPrice testUrl
    assertEqual "gas price doubled" newPrice (price * 2)

basicNotificationsTest :: Assertion
basicNotificationsTest = do
    initReq <- HTTP.parseRequest $ url ++ "/register_push/" ++ show testAddress1
    let req = HTTP.setRequestBodyJSON (PushRequest "31279004-103e-4ba8-b4bf-65eb3eb81859" "ios") $ HTTP.setRequestMethod "POST" initReq
    httpCode <- HTTP.getResponseStatusCode <$> HTTP.httpJSON req
    assertEqual "register channel success" 204 httpCode
