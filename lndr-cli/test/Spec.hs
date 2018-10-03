{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Concurrent             (threadDelay)
import           Control.Monad.Trans.Maybe
import           Data.Either                    (isRight)
import           Data.Either.Combinators        (fromRight)
import           Data.Maybe                     (fromMaybe)
import qualified Data.Map                       as M
import qualified Data.Text.Lazy                 as LT
import           Lndr.CLI.Actions
import           Lndr.Config
import           Lndr.EthereumInterface
import           Lndr.NetworkStatistics
import           Lndr.Signature
import           Lndr.Types
import           Lndr.Util                      ( parseIssueCreditInput,
                                                  textToAddress,
                                                  addHexPrefix)
import           Network.Ethereum.Web3
import qualified Network.Ethereum.Web3.Eth      as Eth
import           Network.Ethereum.Web3.Types
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit                     hiding (Test)
import qualified Text.EmailAddress              as Email
import           System.Directory

testUrl = "http://localhost:7402"
testPrivkey0  =  "7920ca01d3d1ac463dfd55b5ddfdcbb64ae31830f31be045ce2d51a305516a37"
testPrivkey1  =  "bb63b692f9d8f21f0b978b596dc2b8611899f053d68aec6c1c20d1df4f5b6ee2"
testPrivkey2  =  "2f615ea53711e0d91390e97cdd5ce97357e345e441aa95d255094164f44c8652"
testPrivkey3  =  "7d52c3f6477e1507d54a826833169ad169a56e02ffc49a1801218a7d87ca50bd"
testPrivkey4  =  "6aecd44fcb79d4b68f1ee2b2c706f8e9a0cd06b0de4729fe98cfed8886315256"
testPrivkey5  =  "686e245584fdf696abd739c0e66ac6e01fc4c68babee20c7124566e118b2a634"
testPrivkey6  =  "9fd4ab25e1699bb252f4d5c4510a135db34b3adca8baa03194ad5cd6faa13a1d"
testPrivkey7  =  "e8445efa4e3349c3c74fd6689553f93b55aca723115fb777e1e6f4db2a0a82ca"
testPrivkey8  =  "56901d80abc6953d1dc01de2f077b75260f49a3304f665b57ed13514a7e2a2bc"
testPrivkey9  =  "edc63d0e14b29aaa26c7585e962f93abb59bd7d8b01b585e073dc03d052a000b"
testPrivkey10 = "07690ee125a0f79ed899b0f13933885048afd890d5fcb03d988a49fcfd04afc4"
testPrivkey11 = "7784267bcfad13a4a38fddce9a5ad440ecfa4334a5c068aaac2b2edf6178c80a"
testAddress0  = textToAddress . userFromSK . LT.fromStrict $ testPrivkey0
testAddress1  = textToAddress . userFromSK . LT.fromStrict $ testPrivkey1
testAddress2  = textToAddress . userFromSK . LT.fromStrict $ testPrivkey2
testAddress3  = textToAddress . userFromSK . LT.fromStrict $ testPrivkey3
testAddress4  = textToAddress . userFromSK . LT.fromStrict $ testPrivkey4
testAddress5  = textToAddress . userFromSK . LT.fromStrict $ testPrivkey5
testAddress6  = textToAddress . userFromSK . LT.fromStrict $ testPrivkey6
testAddress7  = textToAddress . userFromSK . LT.fromStrict $ testPrivkey7
testAddress8  = textToAddress . userFromSK . LT.fromStrict $ testPrivkey8
testAddress9  = textToAddress . userFromSK . LT.fromStrict $ testPrivkey9
testAddress10 = textToAddress . userFromSK . LT.fromStrict $ testPrivkey10
testAddress11 = textToAddress . userFromSK . LT.fromStrict $ testPrivkey11
testSearch = "test"
testNick1 = "test1"
testNick2 = "test2"
testEmailText = "will@blockmason.io"
testEmail = fromMaybe (error "bad test email") $ Email.emailAddressFromText testEmailText


loadUcacs = do
    (ConfigResponse ucacAddresses _ _ _ _) <- getConfig testUrl
    let Just ucacAddrAUD = M.lookup "AUD" ucacAddresses
        Just ucacAddrCAD = M.lookup "CAD" ucacAddresses
        Just ucacAddrCHF = M.lookup "CHF" ucacAddresses
        Just ucacAddrCNY = M.lookup "CNY" ucacAddresses
        Just ucacAddrDKK = M.lookup "DKK" ucacAddresses
        Just ucacAddrEUR = M.lookup "EUR" ucacAddresses
        Just ucacAddrGBP = M.lookup "GBP" ucacAddresses
        Just ucacAddrHKD = M.lookup "HKD" ucacAddresses
        Just ucacAddrIDR = M.lookup "IDR" ucacAddresses
        Just ucacAddrILS = M.lookup "ILS" ucacAddresses
        Just ucacAddrINR = M.lookup "INR" ucacAddresses
        Just ucacAddrJPY = M.lookup "JPY" ucacAddresses
        Just ucacAddrKRW = M.lookup "KRW" ucacAddresses
        Just ucacAddrMYR = M.lookup "MYR" ucacAddresses
        Just ucacAddrNOK = M.lookup "NOK" ucacAddresses
        Just ucacAddrNZD = M.lookup "NZD" ucacAddresses
        Just ucacAddrRUB = M.lookup "RUB" ucacAddresses
        Just ucacAddrSEK = M.lookup "SEK" ucacAddresses
        Just ucacAddrSGD = M.lookup "SGD" ucacAddresses
        Just ucacAddrTHB = M.lookup "THB" ucacAddresses
        Just ucacAddrTRY = M.lookup "TRY" ucacAddresses
        Just ucacAddr = M.lookup "USD" ucacAddresses
        Just ucacAddrVND = M.lookup "VND" ucacAddresses
    return (ucacAddrAUD, ucacAddrCAD, ucacAddrCHF, ucacAddrCNY, ucacAddrDKK, ucacAddrEUR, ucacAddrGBP, ucacAddrHKD, ucacAddrIDR, ucacAddrILS, ucacAddrINR, ucacAddrJPY, ucacAddrKRW, ucacAddrMYR, ucacAddrNOK, ucacAddrNZD, ucacAddrRUB, ucacAddrSEK, ucacAddrSGD, ucacAddrTHB, ucacAddrTRY, ucacAddr, ucacAddrVND)


main :: IO ()
main = defaultMain tests


tests :: [Test]
tests = [ testGroup "Nicks"
            [ testCase "setting nicks and friends" nickTest ]
        , testGroup "Credits"
            [ testCase "lend money to friend" basicLendTest
            , testCase "settlement" basicSettlementTest
            , testCase "PayPal request notification" payPalRequestTests
            , testCase "PayPal settlement" basicPayPalTest
            ]
        , testGroup "Notifications"
            [ testCase "registerChannel" basicNotificationsTest
            , testCase "deleteChannel" deleteNotificationsTest
            ]
        , testGroup "Authentication"
            [ testCase "nick signing" nickSignTest
            ]
        , testGroup "Utils"
            [ testCase "parseIssueCreditInput" parseCreditInputTest
            ]
        , testGroup "Multi Transaction"
            [ testCase "multiSettlementLendTest" multiSettlementLendTest
            ]
        , testGroup "Multi Settlement"
            [ testCase "advancedSettlementTest" advancedSettlementTest
            ]
        ]


nickTest :: Assertion
nickTest = do
    -- check that nick is available
    nickTaken <- takenNick testUrl testNick1
    assertBool "after db reset all nicks are available" (not nickTaken)
    -- set nick for user1
    httpCode <- setNick testUrl testPrivkey3 (NickRequest testAddress3 testNick1 "")
    assertEqual "set nick success" 204 httpCode
    -- check that test nick is no longer available
    nickTaken <- takenNick testUrl testNick1
    assertBool "nicks already in db are not available" nickTaken
    -- check that nick for user1 properly set
    queriedNick <- getNick testUrl testAddress3
    assertEqual "nick is set and queryable" queriedNick testNick1
    -- fail to set identical nick for user2
    httpCode <- setNick testUrl testPrivkey4 (NickRequest testAddress4 testNick1 "")
    assertBool "duplicate nick is rejected with user error" (httpCode /= 204)
    -- change user1 nick
    httpCode <- setNick testUrl testPrivkey3 (NickRequest testAddress3 testNick2 "")
    assertEqual "change nick success" 204 httpCode
    -- check that user1's nick was successfully changed
    queriedNick <- getNick testUrl testAddress3
    assertEqual "nick is set and queryable" queriedNick testNick2

    -- set user2's nick
    httpCode <- setNick testUrl testPrivkey4 (NickRequest testAddress4 testNick1 "")
    assertEqual "previously used nickname is settable" 204 httpCode

    fuzzySearchResults <- searchNick testUrl testSearch
    assertEqual "search returns both results" 2 $ length fuzzySearchResults

    -- user1 adds user2 as a friend
    httpCode <- addFriend testUrl testAddress3 testAddress4
    assertEqual "add friend success" 204 httpCode

    -- verify that friend has not been added yet
    friends <- getFriends testUrl testAddress3
    assertEqual "target user is not yet a friend to the requesting user" [] friends

    -- verify that friend has not been added yet
    friends <- getFriends testUrl testAddress4
    assertEqual "requesting user is not yet a friend to the target user" [] friends

    -- verify that user2 has a friend request from user1
    friends <- getFriendRequests testUrl testAddress4
    assertEqual "target user has friend request from requesting user" [UserInfo testAddress3 (Just testNick2)] friends

    -- verify that user1 does not have a friend request from user2
    friends <- getFriendRequests testUrl testAddress3
    assertEqual "requesting user does not have friend request from target user" [] friends

    -- verify that user2 does not have an outbound friend request to user1
    friends <- getOutboundFriendRequests testUrl testAddress4
    assertEqual "target user does not have outbound friend request from requesting user" [] friends

    -- verify that user1 has an outbound friend request to user2
    friends <- getOutboundFriendRequests testUrl testAddress3
    assertEqual "requesting user has an outbound friend request to target user" [UserInfo testAddress4 (Just testNick1)] friends

    -- user2 confirms user1 as a friend
    httpCode <- addFriend testUrl testAddress4 testAddress3
    assertEqual "target user confirms friend request" 204 httpCode

    -- verify that friend has been added for user1
    friends <- getFriends testUrl testAddress3
    assertEqual "target user is a friend to the requesting user" [UserInfo testAddress4 (Just testNick1)] friends

    -- verify that friend has been added for user2
    friends <- getFriends testUrl testAddress4
    assertEqual "requesting user is a friend to the target user" [UserInfo testAddress3 (Just testNick2)] friends

    -- user3 removes user4 from friends
    removeFriend testUrl testAddress3 testAddress4

    -- verify that friend has been removed
    friends <- getFriends testUrl testAddress3
    assertEqual "friend properly removed" [] friends

    -- verify that no friend request is present
    friends <- getFriendRequests testUrl testAddress3
    assertEqual "friend request not present for half-confirmed friendship" [] friends

    -- EMAIL TESTS

    -- check that email isn't taken
    emailTaken <- takenEmail testUrl testEmailText
    assertBool "after db reset all emails are available" (not emailTaken)

    -- set email for user1
    httpCode <- setEmail testUrl testPrivkey3 (EmailRequest testAddress3 testEmail "")
    assertEqual "set email success" 204 httpCode

    -- check that testEmail is no longer available
    emailTaken <- takenEmail testUrl testEmailText
    assertBool "emails already in db are not available" emailTaken

    -- check that email for user3 properly set
    queriedEmail <- getEmail testUrl testAddress3
    assertEqual "email is set and queryable" queriedEmail testEmail


basicLendTest :: Assertion
basicLendTest = do
    (ucacAddrAUD, ucacAddrCAD, ucacAddrCHF, ucacAddrCNY, ucacAddrDKK, ucacAddrEUR, ucacAddrGBP, ucacAddrHKD, ucacAddrIDR, ucacAddrILS, ucacAddrINR, ucacAddrJPY, ucacAddrKRW, ucacAddrMYR, ucacAddrNOK, ucacAddrNZD, ucacAddrRUB, ucacAddrSEK, ucacAddrSGD, ucacAddrTHB, ucacAddrTRY, ucacAddr, ucacAddrVND) <- loadUcacs

    let testAmount = 100
        testCredit' = CreditRecord testAddress1 testAddress2 testAmount "USD test 1" testAddress1 0 "" "" ucacAddr Nothing Nothing Nothing
        badTestCredit' = CreditRecord testAddress1 testAddress1 testAmount "dinner bad" testAddress1 0 "" "" ucacAddr Nothing Nothing Nothing

        creditHash = generateHash testCredit'
        testCredit = testCredit' { hash = creditHash }
        badTestCredit = badTestCredit' { hash = generateHash badTestCredit' }

    -- user1 fails to submit pending credit to himself
    httpCode <- submitCredit testUrl testPrivkey1 badTestCredit
    assertEqual "user1 cannot lend to himself" 400 httpCode

    -- user1 submits pending credit to user2
    httpCode <- submitCredit testUrl testPrivkey1 testCredit
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
    httpCode <- submitCredit testUrl testPrivkey1 testCredit
    assertEqual "first lend success" 204 httpCode

    -- user2 accepts user1's pending credit
    httpCode <- submitCredit testUrl testPrivkey2 (testCredit { submitter = testAddress2 })
    assertEqual "first borrow success" 204 httpCode

    -- user1's checks that he has pending credits and one verified credit
    creditRecords1 <- checkPending testUrl testAddress1
    assertEqual "zero pending records found for user1" 0 (length creditRecords1)

    verifiedRecords1 <- getTransactions testUrl testAddress1
    assertEqual "one verified record found for user1" 1 (length verifiedRecords1)

    balance <- getBalance testUrl testAddress1 "USD"
    assertEqual "user1's total balance is correct" testAmount balance

    twoPartyBalance <- getTwoPartyBalance testUrl testAddress1 testAddress2 "USD"
    assertEqual "user1's two-party balance is correct" testAmount twoPartyBalance

    balance <- getBalance testUrl testAddress2 "USD"
    assertEqual "user2's total balance is correct" (-testAmount) balance

    twoPartyBalance <- getTwoPartyBalance testUrl testAddress2 testAddress1 "USD"
    assertEqual "user2's two-party balance is correct" (-testAmount) twoPartyBalance

    -- user1's counterparties list is [user2]
    counterparties <- getCounterparties testUrl testAddress1
    assertEqual "user1's counterparties properly calculated" [testAddress2] counterparties

    -- user1 is friends with user2
    friends <- fmap addr <$> getFriends testUrl testAddress1
    assertEqual "user1's friends properly calculated" [testAddress2] friends

    -- user2 is friends with user1
    friends <- fmap addr <$> getFriends testUrl testAddress2
    assertEqual "user2's friends properly calculated" [testAddress1] friends

    -- user1 and user2 create credit on JPY ucac
    let jpyValue = 200000
        testCreditJPY' = CreditRecord testAddress2 testAddress1 jpyValue "JPY test 1" testAddress2 1 "" "" ucacAddrJPY Nothing Nothing Nothing
        creditHashJPY = generateHash testCreditJPY'
        testCreditJPY = testCreditJPY' { hash = creditHashJPY }

    -- user2 submits pending credit to user1
    httpCode <- submitCredit testUrl testPrivkey2 testCreditJPY
    assertEqual "second lend success" 204 httpCode

    -- user1 accepts user2's pending credit
    httpCode <- submitCredit testUrl testPrivkey1 (testCreditJPY { submitter = testAddress1 })
    assertEqual "second borrow success" 204 httpCode

    -- user2 has a correct total JPY balance
    balance <- getBalance testUrl testAddress2 "JPY"
    assertEqual "user2's total jpy balance is correct" jpyValue balance

    balance <- getTwoPartyBalance testUrl testAddress1 testAddress2 "JPY"
    assertEqual "user2 & user1's two-party total balance is correct" (-jpyValue) balance


basicSettlementTest :: Assertion
basicSettlementTest = do
    (ucacAddrAUD, ucacAddrCAD, ucacAddrCHF, ucacAddrCNY, ucacAddrDKK, ucacAddrEUR, ucacAddrGBP, ucacAddrHKD, ucacAddrIDR, ucacAddrILS, ucacAddrINR, ucacAddrJPY, ucacAddrKRW, ucacAddrMYR, ucacAddrNOK, ucacAddrNZD, ucacAddrRUB, ucacAddrSEK, ucacAddrSGD, ucacAddrTHB, ucacAddrTRY, ucacAddr, ucacAddrVND) <- loadUcacs

    let testAmount = 2939
        testCredit' = CreditRecord testAddress5 testAddress6 testAmount "settlement" testAddress5 0 "" "" ucacAddr (Just "ETH") Nothing Nothing
        creditHash = generateHash testCredit'
        testCredit = testCredit' { hash = creditHash }

    -- user5 submits pending settlement credit to user6
    httpCode <- submitCredit testUrl testPrivkey5 testCredit
    assertEqual "lend (settle) success" 204 httpCode

    -- check that pending settlement is registered in test
    (SettlementsResponse pendingSettlements bilateralPendingSettlements) <- getPendingSettlements testUrl testAddress5
    assertEqual "pre-confirmation: get pending settlements success" 1 (length pendingSettlements)
    assertEqual "pre-confirmation: get bilateral pending settlements success" 0 (length bilateralPendingSettlements)

    -- user6 accepts user5's pending settlement credit
    httpCode <- submitCredit testUrl testPrivkey6 (testCredit { submitter = testAddress6 })
    assertEqual "borrow (settle) success" 204 httpCode

    (SettlementsResponse pendingSettlements bilateralPendingSettlements) <- getPendingSettlements testUrl testAddress5
    assertEqual "post-confirmation: get pending settlements success" 0 (length pendingSettlements)
    assertEqual "post-confirmation: get bilateral pending settlements success" 1 (length bilateralPendingSettlements)

    let settleAmount = fmap Quantity . settlementAmount . creditRecord $ head bilateralPendingSettlements

    -- user5 transfers eth to user6
    txHashE <- runWeb3 $ Eth.sendTransaction $ Call (Just testAddress5)
                                                        testAddress6
                                                        (Just 21000)
                                                        Nothing
                                                        settleAmount
                                                        Nothing

    let txHash = fromRight (error "error sending eth") txHashE

    httpCode <- getTxHashFail testUrl creditHash
    assertEqual "404 upon hash not found error" 404 httpCode

    -- user5 verifies that he has made the settlement credit
    httpCode <- verifySettlement testUrl creditHash txHash testPrivkey5
    assertEqual "verification success" 204 httpCode

    -- ensure that tx registers in blockchain w/ a 10 second pause and
    -- heartbeat has time to verify its validity
    threadDelay (20 * 10 ^ 6)

    (SettlementsResponse pendingSettlements bilateralPendingSettlements) <- getPendingSettlements testUrl testAddress5
    assertEqual "post-verification: get pending settlements success" 0 (length pendingSettlements)
    assertEqual "post-verification: get bilateral pending settlements success" 0 (length bilateralPendingSettlements)

    balance <- getBalance testUrl testAddress5 "USD"
    assertEqual "user5's total balance is correct" testAmount balance

    balance <- getBalance testUrl testAddress6 "USD"
    assertEqual "user5's total balance is correct" (-testAmount) balance

    gottenTxHash <- getTxHash testUrl creditHash
    assertEqual "successful txHash retrieval" txHash (addHexPrefix gottenTxHash)


basicPayPalTest :: Assertion
basicPayPalTest = do
    (ucacAddrAUD, ucacAddrCAD, ucacAddrCHF, ucacAddrCNY, ucacAddrDKK, ucacAddrEUR, ucacAddrGBP, ucacAddrHKD, ucacAddrIDR, ucacAddrILS, ucacAddrINR, ucacAddrJPY, ucacAddrKRW, ucacAddrMYR, ucacAddrNOK, ucacAddrNZD, ucacAddrRUB, ucacAddrSEK, ucacAddrSGD, ucacAddrTHB, ucacAddrTRY, ucacAddr, ucacAddrVND) <- loadUcacs

    let testAmount = 100
        testCredit' = CreditRecord testAddress10 testAddress11 testAmount "PAYPAL TEST 1" testAddress10 0 "" "" ucacAddrGBP (Just "PAYPAL") Nothing Nothing

        creditHash = generateHash testCredit'
        testCredit = testCredit' { hash = creditHash }

    -- user1 submits paypal tx to user2
    httpCode <- submitCredit testUrl testPrivkey10 testCredit
    assertEqual "initial lend success" 204 httpCode

    -- user1 checks pending transactions
    creditRecords1 <- checkPending testUrl testAddress10
    assertEqual "one pending record found for user1" 1 (length creditRecords1)

    -- user2 checks pending transactions
    creditRecords2 <- checkPending testUrl testAddress11
    assertEqual "one pending record found for user2" 1 (length creditRecords2)

    -- user2 rejects pending transaction
    httpCode <- rejectCredit testUrl testPrivkey10 creditHash
    assertEqual "reject success" 204 httpCode

    -- user2 has 0 pending records post-rejection
    creditRecords2 <- checkPending testUrl testAddress11
    assertEqual "zero pending records found for user2" 0 (length creditRecords2)

    -- user1 attempts same credit again
    httpCode <- submitCredit testUrl testPrivkey10 testCredit
    assertEqual "PayPal lend success" 204 httpCode

    -- user2 accepts user1's pending credit
    httpCode <- submitCredit testUrl testPrivkey11 (testCredit { submitter = testAddress11 })
    assertEqual "PayPal borrow success" 204 httpCode

    -- user1's checks that he has pending credits and two verified credits
    creditRecords1 <- checkPending testUrl testAddress10
    assertEqual "zero pending records found for user1" 0 (length creditRecords1)

    verifiedRecords1 <- getTransactions testUrl testAddress10
    assertEqual "one verified record found for user1" 1 (length verifiedRecords1)

    balance <- getBalance testUrl testAddress10 "GBP"
    assertEqual "user1's total balance is correct" testAmount balance

    twoPartyBalance <- getTwoPartyBalance testUrl testAddress10 testAddress11 "GBP"
    assertEqual "user1's two-party balance is correct" testAmount twoPartyBalance

    balance <- getBalance testUrl testAddress11 "GBP"
    assertEqual "user2's total balance is correct" (-testAmount) balance

    twoPartyBalance <- getTwoPartyBalance testUrl testAddress11 testAddress10 "GBP"
    assertEqual "user2's two-party balance is correct" (-testAmount) twoPartyBalance


basicNotificationsTest :: Assertion
basicNotificationsTest = do
    httpCode <- registerChannel testUrl testPrivkey1 (PushRequest "31279004-103e-4ba8-b4bf-65eb3eb81859" "ios" testAddress1 "")
    assertEqual "register channel success" 204 httpCode

    httpCode <- registerChannel testUrl testPrivkey1 (PushRequest "31279004-103e-4ba8-b4bf-65eb3eb81859" "ios" testAddress1 "")
    assertEqual "able to register twice with the same address and device" 204 httpCode

    httpCode <- registerChannel testUrl testPrivkey2 (PushRequest "31279004-103e-4ba8-b4bf-65eb3eb81859" "ios" testAddress2 "")
    assertEqual "able to register device with a different user" 204 httpCode

    httpCode <- registerChannel testUrl testPrivkey2 (PushRequest "79312004-103e-4ba8-b4bf-65eb3eb59818" "android" testAddress2 "")
    assertEqual "able to register user with a different device" 204 httpCode


deleteNotificationsTest :: Assertion
deleteNotificationsTest = do
    httpCode <- deleteChannel testUrl testPrivkey1 (PushRequest "31279004-103e-4ba8-b4bf-65eb3eb81859" "ios" testAddress1 "")
    assertEqual "delete channel success" 204 httpCode


parseCreditInputTest :: Assertion
parseCreditInputTest = do
        assertEqual "expected credit log" creditLog (IssueCreditLog "869a8f2c3d22be392618ed06c8f548d1d5b5aed6" "754952bfa2097104a07f4f347e513a1da576ac7a" "3a1ea286e419130d894c9fa0cf49898bc81f9a5a" 2617 0 "fedex            ")
        assertBool "good creditor sig" goodCreditorSig
        assertBool "good debtor sig" goodDebtorSig
    where (creditLog, _, goodCreditorSig, _, goodDebtorSig) = parseIssueCreditInput (Nonce 0) "0x0a5b410e000000000000000000000000869a8f2c3d22be392618ed06c8f548d1d5b5aed6000000000000000000000000754952bfa2097104a07f4f347e513a1da576ac7a0000000000000000000000003a1ea286e419130d894c9fa0cf49898bc81f9a5a0000000000000000000000000000000000000000000000000000000000000a394988c5614ea5a5807387af10ab3520ed0bb9e8edfcef07924b3630b119dccab12981d3efa478582c5c69a4c9ef5ccb3e019e52cd2b210cc0bc17133799d1f739000000000000000000000000000000000000000000000000000000000000001cd58818da99967a502e7abc5cc74b3569063c7670924d22e25c329b560298cc5566e07b7c87b7a8685f954410480fe2f5cd08dbcb84552dcc3b4475e8469f1b12000000000000000000000000000000000000000000000000000000000000001c6665646578202020202020202020202020202020202020202020202020202020"


nickSignTest :: Assertion
nickSignTest = assertEqual "expected nick request signature" nickSignature (Right "6c965e1e501c18eedaf8af07dcdee651e0569efd017211be7faf10454c11bcf9611c8e9feaca7ac8ac400cce441db84ecbeb6f817fd8f40e321b33c3a0f4b21d1b")
    where
        unsignedNickRequest = NickRequest testAddress1 "testNick" ""
        nickSignature = generateSignature unsignedNickRequest testPrivkey1


multiSettlementLendTest :: Assertion
multiSettlementLendTest = do
    (ucacAddrAUD, ucacAddrCAD, ucacAddrCHF, ucacAddrCNY, ucacAddrDKK, ucacAddrEUR, ucacAddrGBP, ucacAddrHKD, ucacAddrIDR, ucacAddrILS, ucacAddrINR, ucacAddrJPY, ucacAddrKRW, ucacAddrMYR, ucacAddrNOK, ucacAddrNZD, ucacAddrRUB, ucacAddrSEK, ucacAddrSGD, ucacAddrTHB, ucacAddrTRY, ucacAddr, ucacAddrVND) <- loadUcacs

    let testAmount1 = 100
        testAmount2 = 50
        testCredits' = [ ( CreditRecord testAddress7 testAddress8 testAmount1 "multiSettlement good 1" testAddress7 0 "" "" ucacAddr Nothing Nothing Nothing )
            , ( CreditRecord testAddress8 testAddress7 testAmount2 "multiSettlement good 2" testAddress7 1 "" "" ucacAddrJPY Nothing Nothing Nothing ) ]
        badTestCredits' = [ ( CreditRecord testAddress7 testAddress7 testAmount1 "multiSettlement bad 1" testAddress7 0 "" "" ucacAddr Nothing Nothing Nothing )
            , ( CreditRecord testAddress7 testAddress7 testAmount2 "multiSettlement bad 2" testAddress7 1 "" "" ucacAddrJPY Nothing Nothing Nothing ) ]

        -- creditHash =  testCredit'
        testHashes = fmap generateHash testCredits'
        testCredits = fmap (\credit -> credit { hash = generateHash credit } ) testCredits'
        testCredits2 = fmap (\credit -> credit { submitter = testAddress8 } ) testCredits
        badTestCredits = fmap (\credit -> credit { hash = generateHash credit } ) badTestCredits'

    -- user1 fails to submit pending credit to himself
    httpCode <- submitMultiSettlement testUrl testPrivkey7 badTestCredits
    assertEqual "user1 cannot lend to himself" 400 httpCode

    -- user1 submits pending credit to user2
    httpCode <- submitMultiSettlement testUrl testPrivkey7 testCredits
    assertEqual "lend success" 204 httpCode

    -- user1 checks pending transactions
    creditRecords1 <- checkPending testUrl testAddress7
    assertEqual "two pending records found for user1" 2 (length creditRecords1)

    -- user2 checks pending transactions
    creditRecords2 <- checkPending testUrl testAddress8
    assertEqual "two pending records found for user2" 2 (length creditRecords2)

    -- user2 rejects pending transactions
    httpCode <- rejectCredit testUrl testPrivkey7 ( head testHashes )
    assertEqual ("reject success (first)" ++ show ( head testHashes ) ++ " : " ++ show ( hash (head testCredits) ) ) 204 httpCode
    httpCode <- rejectCredit testUrl testPrivkey7 ( last testHashes )
    assertEqual "reject success (second)" 204 httpCode

    -- user2 has 0 pending records post-rejection
    creditRecords2 <- checkPending testUrl testAddress8
    assertEqual "zero pending records found for user2" 0 (length creditRecords2)

    -- user1 attempts same credit again
    httpCode <- submitMultiSettlement testUrl testPrivkey7 testCredits
    assertEqual "multi-settlement lend success" 204 httpCode

    -- user2 accepts user1's pending credit
    httpCode <- submitMultiSettlement testUrl testPrivkey8 testCredits2
    assertEqual "multi-settlement borrow success" 204 httpCode

    -- user1's checks that he has pending credits and one verified credit
    creditRecords1 <- checkPending testUrl testAddress7
    assertEqual "zero pending records found for user1" 0 (length creditRecords1)

    verifiedRecords1 <- getTransactions testUrl testAddress7
    assertEqual "two verified record found for user1" 2 (length verifiedRecords1)

    balance <- getBalance testUrl testAddress7 "USD"
    assertEqual "user1's total USD balance is correct" testAmount1 balance

    balance <- getBalance testUrl testAddress7 "JPY"
    assertEqual "user1's total JPY balance is correct" (-testAmount2) balance

    twoPartyBalance <- getTwoPartyBalance testUrl testAddress7 testAddress8 "USD"
    assertEqual "user1's two-party balance is correct" testAmount1 twoPartyBalance

    twoPartyBalance <- getTwoPartyBalance testUrl testAddress7 testAddress8 "JPY"
    assertEqual "user1's two-party balance is correct" (-testAmount2) twoPartyBalance

    balance <- getBalance testUrl testAddress8 "USD"
    assertEqual "user2's total balance is correct" (-testAmount1) balance

    balance <- getBalance testUrl testAddress8 "JPY"
    assertEqual "user2's total balance is correct" testAmount2 balance

    twoPartyBalance <- getTwoPartyBalance testUrl testAddress8 testAddress7 "USD"
    assertEqual "user2's two-party balance is correct" (-testAmount1) twoPartyBalance

    twoPartyBalance <- getTwoPartyBalance testUrl testAddress8 testAddress7 "JPY"
    assertEqual "user2's two-party balance is correct" testAmount2 twoPartyBalance

    -- user1's counterparties list is [user2]
    counterparties <- getCounterparties testUrl testAddress7
    assertEqual "user1's counterparties properly calculated" [testAddress8] counterparties

    -- user1 is friends with user2
    friends <- fmap addr <$> getFriends testUrl testAddress7
    assertEqual "user1's friends properly calculated" [testAddress8] friends

    -- user2 is friends with user1
    friends <- fmap addr <$> getFriends testUrl testAddress8
    assertEqual "user2's friends properly calculated" [testAddress7] friends


advancedSettlementTest :: Assertion
advancedSettlementTest = do
    (ucacAddrAUD, ucacAddrCAD, ucacAddrCHF, ucacAddrCNY, ucacAddrDKK, ucacAddrEUR, ucacAddrGBP, ucacAddrHKD, ucacAddrIDR, ucacAddrILS, ucacAddrINR, ucacAddrJPY, ucacAddrKRW, ucacAddrMYR, ucacAddrNOK, ucacAddrNZD, ucacAddrRUB, ucacAddrSEK, ucacAddrSGD, ucacAddrTHB, ucacAddrTRY, ucacAddr, ucacAddrVND) <- loadUcacs

    let testAmount1 = 2939
        testAmount2 = 1039
        testCredits' = [ ( CreditRecord testAddress9 testAddress0 testAmount1 "advanced settlement 1" testAddress9 0 "" "" ucacAddr (Just "ETH") Nothing Nothing )
            , ( CreditRecord testAddress0 testAddress9 testAmount2 "advanced settlement 2" testAddress9 1 "" "" ucacAddrJPY (Just "ETH") Nothing Nothing ) ]

        -- creditHash =  testCredit'
        testHashes = fmap generateHash testCredits'
        testCredits = fmap (\credit -> credit { hash = generateHash credit } ) testCredits'
        testCredits2 = fmap (\credit -> credit { submitter = testAddress0 } ) testCredits

    let creditHashes = fmap generateHash testCredits'

    -- user5 submits pending credit to user6
    httpCode <- submitMultiSettlement testUrl testPrivkey9 testCredits
    assertEqual "lend (settle) success" 204 httpCode

    -- check that pending settlement is registered in test
    (SettlementsResponse pendingSettlements bilateralPendingSettlements) <- getPendingSettlements testUrl testAddress9
    assertEqual "pre-confirmation: get pending settlements success" 2 (length pendingSettlements)
    assertEqual "pre-confirmation: get bilateral pending settlements success" 0 (length bilateralPendingSettlements)

     -- user6 accepts user5's pending settlement credit
    httpCode <- submitMultiSettlement testUrl testPrivkey0 testCredits2
    assertEqual "borrow (settle) success" 204 httpCode

    (SettlementsResponse pendingSettlements bilateralPendingSettlements) <- getPendingSettlements testUrl testAddress9
    assertEqual "post-confirmation: get pending settlements success" 0 (length pendingSettlements)
    assertEqual "post-confirmation: get bilateral pending settlements success" 2 (length bilateralPendingSettlements)

    let settleAmount1 = fmap Quantity . settlementAmount . creditRecord $ head bilateralPendingSettlements
        settleAmount2 = fmap Quantity . settlementAmount . creditRecord $ last bilateralPendingSettlements
        totalSettleAmount = abs $ (fromMaybe 0 settleAmount1) - (fromMaybe 0 settleAmount2)

    -- user5 transfers eth to user6
    txHashE <- runWeb3 $ Eth.sendTransaction $ Call (Just testAddress9)
                                                        testAddress0
                                                        (Just 21000)
                                                        Nothing
                                                        (Just totalSettleAmount)
                                                        Nothing

    let txHash = fromRight (error "error sending eth") txHashE

    -- user5 verifies that he has made the settlement credit
    httpCode1 <- verifySettlement testUrl (head creditHashes) txHash testPrivkey5
    assertEqual "verification success" 204 httpCode

    httpCode2 <- verifySettlement testUrl (last creditHashes) txHash testPrivkey5
    assertEqual "verification success" 204 httpCode

    -- ensure that tx registers in blockchain w/ a 10 second pause and
    -- heartbeat has time to verify its validity
    threadDelay (20 * 10 ^ 6)

    (SettlementsResponse pendingSettlements bilateralPendingSettlements) <- getPendingSettlements testUrl testAddress9
    assertEqual "post-verification: get pending settlements success" 0 (length pendingSettlements)
    assertEqual "post-verification: get bilateral pending settlements success" 0 (length bilateralPendingSettlements)

    balance <- getBalance testUrl testAddress9 "USD"
    assertEqual "user5's total USD balance is correct" testAmount1 balance

    balance <- getBalance testUrl testAddress0 "USD"
    assertEqual "user6's total USD balance is correct" (-testAmount1) balance

    gottenTxHash <- getTxHash testUrl (head creditHashes)
    assertEqual "successful txHash retrieval" txHash (addHexPrefix gottenTxHash)

    -- This test passes locally but not in TravisCI
    -- (numDbCredits, numBlockchainCredits, _, _) <- consistencyCheck
    -- assertEqual "correct number of transactions (8) on blockchain" 8 numBlockchainCredits
    -- assertEqual "correct number of transactions (8) in db" 8 numDbCredits


payPalRequestTests :: Assertion
payPalRequestTests = do
    httpCode1 <- requestPayPal testUrl testPrivkey1 ( PayPalRequest testAddress0 testAddress1 "" )
    assertEqual "paypal notification request returns 204" 204 httpCode1

    payPalRequests1 <- retrievePayPalRequests testUrl testAddress0
    assertEqual "user0 PayPal requests has length of 1" 1 (length payPalRequests1)

    payPalRequests2 <- retrievePayPalRequests testUrl testAddress1
    assertEqual "user1 PayPal requests has length of 1" 1 (length payPalRequests2)

    httpCode2 <- deletePayPalRequest testUrl testPrivkey1 ( PayPalRequest testAddress0 testAddress1 "" )
    assertEqual "paypal notification request returns 204" 204 httpCode1

    payPalRequests3 <- retrievePayPalRequests testUrl testAddress0
    assertEqual "user0 PayPal requests has length of 1" 0 (length payPalRequests3)

    payPalRequests4 <- retrievePayPalRequests testUrl testAddress1
    assertEqual "user0 PayPal requests has length of 1" 0 (length payPalRequests4)
