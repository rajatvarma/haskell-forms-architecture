{-# LANGUAGE OverloadedStrings #-}

import Test.WebDriver
import Test.WebDriver.Commands.Wait
import qualified Data.Text as T
import Control.Monad.Trans (liftIO)
import Control.Monad (filterM, replicateM_, forM_)
import System.IO (hFlush, stdout)
import Control.Exception (catch, SomeException)
import System.Random (randomRIO)
import Data.List (isInfixOf)

sendInput :: String -> WD ()
sendInput input = do
    liftIO $ do
        putStrLn $ "Sending input: " ++ input
        hFlush stdout
    inputField <- findElem (ByCSS "input[type='text']")
    sendKeys (T.pack input) inputField
    submit inputField
    liftIO $ do
        putStrLn "Waiting for input field to disappear..."
        hFlush stdout
    waitWhile 10 (not <$> isDisplayed inputField)
    liftIO $ do
        putStrLn "Input field disappeared."
        hFlush stdout


clickRadioButton :: String -> String -> WD ()
clickRadioButton name value = do
    radioButtons <- findElems (ByCSS $ T.concat ["input[type='radio'][name='", T.pack name, "']"])
    radioButton <- head <$> filterM (\rb -> do
            val <- attr rb "value"
            return $ val == Just (T.pack value)
        ) radioButtons
    click radioButton

randomString :: Int -> IO String
randomString len = mapM (const $ randomRIO ('a', 'z')) [1..len]

randomRadioButtonValue :: IO String
randomRadioButtonValue = show <$> (randomRIO (1, 5) :: IO Int)  -- Assuming the radio button values are from 1 to 4

readEmailsFromFile :: FilePath -> IO [String]
readEmailsFromFile filePath = lines <$> readFile filePath

main :: IO ()
main = do
    let config = defaultConfig { wdCapabilities = defaultCaps { browser = firefox } }
    emails <- readEmailsFromFile "tests.txt"  -- Read emails from file
    putStrLn "JHI"
    catch (runSession config $ do
        liftIO $ putStrLn "WEB"
        forM_ emails $ \email -> do  -- Use each email from the list
            openPage "http://localhost:3000"
            name <- liftIO $ randomString 10
            searchInput <- findElem ( ByName "name" )
            sendKeys (T.pack name) searchInput
            searchInput <- findElem ( ByName "email" )
            sendKeys (T.pack email) searchInput  -- Use the email from the file
            q1Value <- liftIO randomRadioButtonValue
            clickRadioButton "q1" q1Value
            q2Value <- liftIO randomRadioButtonValue
            clickRadioButton "q2" q2Value
            q3Value <- liftIO randomRadioButtonValue
            clickRadioButton "q3" q3Value
            q4Value <- liftIO randomRadioButtonValue
            clickRadioButton "q4" q4Value
            submit searchInput
            url <- getCurrentURL
            (if "/input" `isInfixOf` url
                then liftIO $ putStrLn (email++" accepted.")
                else liftIO $ putStrLn (email++" not accepted."))
            ) handleException

handleException :: SomeException -> IO ()
handleException ex = do
    putStrLn "BAD"
    putStrLn $ "Caught exception: " ++ show ex
    hFlush stdout