{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
import          Control.Monad.Logger (runStderrLoggingT)
import          Control.Applicative
import          Data.Text           (Text)
import          Yesod
-- import          Yesod.Static (StaticR)\
import          Database.Persist
import          Database.Persist.Sqlite
import          Data.Text (unpack, pack, replicate)
import          Data.List (unionBy, elemIndex)
import          Data.Maybe (fromMaybe)
import          Data.Csv as Csv
import          qualified Data.ByteString.Lazy as BL
import          qualified Data.ByteString.Char8 as CB
import          qualified Data.Vector as V
import          qualified Data.Text as T
import          qualified Data.Text.Lazy.Encoding as TLE
import Data.Char (isAlpha, isAlphaNum)



share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|

Review
    userName Text
    userEmail Text
    qOne Int
    qTwo Int
    qThree Int
    qFour Int
    deriving Show
|]

instance Csv.ToNamedRecord Review where
    toNamedRecord (Review un ue q1 q2 q3 q4) = Csv.namedRecord
        [ "User Name" Csv..= un
        , "User Email" Csv..= ue
        , "Q1" Csv..= q1
        , "Q2" Csv..= q2
        , "Q3" Csv..= q3
        , "Q4" Csv..= q4
        ]

responseHeader :: Csv.Header
responseHeader = V.fromList $ map CB.pack ["User Name", "User Email", "Q1", "Q2", "Q3", "Q4"]

responsesToCsv :: [Review] -> BL.ByteString
responsesToCsv responses = Csv.encodeByName responseHeader (responses)

data App = App
    { persistConfig :: SqliteConf
    , connPool :: ConnectionPool
    }

mkYesod "App" [parseRoutes|
/ HomeR GET
/input InputR GET
/admin AdminR GET
/csvExport CsvExportR  GET
|]


instance Yesod App

instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB = defaultRunDB persistConfig connPool
instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner connPool

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

data State = Start | PreAt Int | At | PostAt Int | Dot | PostDot Int | Invalid deriving (Eq)

transition :: State -> Char -> State
transition Start '@' = Invalid
transition Start '.' = Invalid
transition Start c 
    | isAlphaNum c = PreAt 1
    | otherwise = Invalid
transition (PreAt n) '@' 
    | n > 1 = At
    | otherwise = Invalid
transition (PreAt n) c 
    | isAlphaNum c = PreAt (n+1)
    | otherwise = Invalid
transition At '.' = Invalid
transition At c 
    | isAlphaNum c = PostAt 1
    | otherwise = Invalid
transition (PostAt n) '@' = Invalid
transition (PostAt n) '.' 
    | n > 1 = Dot
    | otherwise = Invalid
transition (PostAt n) c 
    | isAlphaNum c = PostAt (n+1)
    | otherwise = Invalid
transition Dot '.' = Invalid
transition Dot c 
    | isAlpha c = PostDot 1
    | otherwise = Invalid
transition (PostDot n) '.' = Invalid
transition (PostDot n) c 
    | isAlpha c = PostDot (n+1)
    | otherwise = Invalid
transition Invalid _ = Invalid

validateEmail :: String -> Bool
validateEmail email = finalState == PostDot 1
    where 
        finalState = foldl transition Start email

validateName :: String -> Bool
validateName = all isAlpha

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
    toWidget [lucius|
        body {
            background-color: #C6DF76;
            padding: 2%;
            font-family: "Inter";
        }
        .modal {
            position: absolute;
            z-index: 10;
            top: 0;
            left: 0;
            width: 100vw;
            height: 100vh;
            background-color: #000A;
        }
        .modal p {
            text-align: center;
            background-color: #FFF;
            width: 50vw;
            position: absolute;
            left: 25vw;
            padding: 25px;
            top: 15vh;
        }
        h2 {
            color: #000;
        }
        form {
            background-color: #FFF;
            padding: 10px;
            width: 65vw;
            border: 3px solid black;
            box-shadow: 10px 10px 0px #0007;
        }
        input[type="text"] {
            border-bottom: 2px solid black;
            border-left: none;
            border-top: none;
            border-right: none;
        }
        input[type="submit"] {
            background-color: #9747FF;
            font-family: "Inter";
            font-weight: 700;
            padding: 10px 30px;
            margin-top: 50px;
            border: 2px solid black;
        }
    |]
    setTitle "Watch Feedback Review"
    msg <- getMessage
    [whamlet|
        $maybe m <- msg 
            <div .modal>
                    <p>#{m}

        <h2>Feedback Survey</h2>
        <form action=@{InputR}>
            <h3> About you
            <p>
                My name is
                <input type=text name=name>
                and my email is 
                <input type=text name=email>
                <br>
                <br>
                <br>
            <h3> About the watch
            <br>
                Satisfied with look and feel?
                <br>
                <input type=radio name=q1 value=1>1
                <input type=radio name=q1 value=2>2
                <input type=radio name=q1 value=3>3
                <input type=radio name=q1 value=4>4
                <input type=radio name=q1 value=5>5
                <br>
                Satisfied with battery backup?
                <br>
                <input type=radio name=q2 value=1>1
                <input type=radio name=q2 value=2>2
                <input type=radio name=q2 value=3>3
                <input type=radio name=q2 value=4>4
                <input type=radio name=q2 value=5>5
                <br>
                Satisfied with raise to wake?
                <br>
                <input type=radio name=q3 value=1>1
                <input type=radio name=q3 value=2>2
                <input type=radio name=q3 value=3>3
                <input type=radio name=q3 value=4>4
                <input type=radio name=q3 value=5>5
                <br>
                Satisfied with notification support?
                <br>
                <input type=radio name=q4 value=1>1
                <input type=radio name=q4 value=2>2
                <input type=radio name=q4 value=3>3
                <input type=radio name=q4 value=4>4
                <input type=radio name=q4 value=5>5
                <br>
                <input type=submit value="Submit">
    |]

getInputR :: Handler Html
getInputR = do
    review <- runInputGet $ Review
                <$> ireq textField "name"
                <*> ireq textField "email"
                <*> ireq intField "q1"
                <*> ireq intField "q2"
                <*> ireq intField "q3"
                <*> ireq intField "q4"
    let email = reviewUserEmail review
    let name = reviewUserName review
    if validateName (unpack name)
        then do 
            if validateEmail (unpack email)
                then do
                    reviewId <- runDB $ insert review
                    defaultLayout [whamlet|<p>#{show review}|]
                else do
                    setMessage "Invalid email address"
                    redirect HomeR
        else do
            setMessage "Invalid name"
            redirect HomeR 

histogramLine :: Int -> Text
histogramLine count = Data.Text.replicate count (pack "🟩")

countOccurrences :: Ord a => [a] -> [(a, Int)]
countOccurrences xs = [(head x, length x) | x <- group $ quicksort xs]

fullRange :: [(Int, Int)]
fullRange = [(i, 0) | i <- [1..5]]

mergeHistograms :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
mergeHistograms fullRange histogramData = 
    [ (value, fromMaybe 0 (Prelude.lookup value histogramData)) | (value, _) <- fullRange ]

getCsvExportR :: Handler TypedContent
getCsvExportR = do
    rl <- runDB $ selectList ([] :: [Filter Review]) []
    let responses = map (\(Entity _ review) -> Review (reviewUserName review) (reviewUserEmail review) (reviewQOne review) (reviewQTwo review) (reviewQThree review) (reviewQFour review)) rl
    let csvData = responsesToCsv responses
    sendResponse (TypedContent "text/csv" (toContent (TLE.decodeUtf8 csvData)))

getAdminR :: Handler Html
getAdminR = do
    rl <- runDB $ selectList ([] :: [Filter Review]) []
    -- 
    let q1Responses = map (fromIntegral . reviewQOne . entityVal) rl
    let q1Mean = mean q1Responses
    let q1Median = median q1Responses
    let q1Mode = mode q1Responses
    let q1Histogram = mergeHistograms fullRange $ countOccurrences (map round q1Responses)
    -- 
    let q2Responses = map (fromIntegral . reviewQTwo . entityVal) rl
    let q2Mean = mean q2Responses
    let q2Median = median q2Responses
    let q2Mode = mode q2Responses
    let q2Histogram = mergeHistograms fullRange $ countOccurrences (map round q2Responses)
    -- 
    let q3Responses = map (fromIntegral . reviewQThree . entityVal) rl
    let q3Mean = mean q3Responses
    let q3Median = median q3Responses
    let q3Mode = mode q3Responses
    let q3Histogram = mergeHistograms fullRange $ countOccurrences (map round q3Responses)
    -- 
    let q4Responses = map (fromIntegral . reviewQFour . entityVal) rl
    let q4Mean = mean q4Responses
    let q4Median = median q4Responses
    let q4Mode = mode q4Responses
    let q4Histogram = mergeHistograms fullRange $ countOccurrences (map round q4Responses)

    defaultLayout $ do 
        toWidget [lucius|
            body {
                background-color: #C6DF76;
                padding: 2%;
                font-family: "Inter";
            }
            ul {
                height: 2vh;
                overflow-y: scroll;
            }
            .stats {
                display: flex;
                flex-direction: row;
                justify-content: space-between;
                align-items: center;
                background-color: #FFF;
                border: 2px solid black;
                margin-top: 5px;
                padding-left: 15px;
            }
            .stats h3 {
                width: 15vw;
            }
            .stats-inner {
                display: flex;
                flex-direction: row;
                justify-content: space-evenly;
                width: 30vw;
            }
            .container {
                background-color: #FFF;
                padding: 10px;
                border: 3px black solid;
                box-shadow: 10px 10px 0px #0009;
            }
            .histogram {
                width: 30vw;
                font-size: 10px;
            }
            button {
                background-color: #DD4470;
                font-family: "Inter";
                font-weight: 700;
                border: 2px solid black;
                padding: 5px 20px;
            }
        |]
        [whamlet|
            <h1>Admin Dashboard
            <button onclick="window.location.href='@{CsvExportR}'">Download all responses as CSV</button>
            <h2>Feedback on:
            <br>
            <div.container>
                <div.stats>
                    <h3>Look and Feel
                    <div.stats-inner>    
                        <p>Mean: #{q1Mean}
                        <p>Median: #{q1Median}
                        <p>Mode: #{q1Mode}
                    <div.histogram>
                        $forall (value, count) <- q1Histogram
                            <p>#{value}: #{histogramLine count}
                <div.stats>
                    <h3>Battery Backup
                    <div.stats-inner>
                        <p>Mean: #{q2Mean}
                        <p>Median: #{q2Median}
                        <p>Mode: #{q2Mode}
                    <div.histogram>
                        $forall (value, count) <- q2Histogram
                            <p>#{value}: #{histogramLine count}
                <div.stats>
                    <h3>Raise to wake
                    <div.stats-inner>
                        <p>Mean: #{q3Mean}
                        <p>Median: #{q3Median}
                        <p>Mode: #{q3Mode}
                    <div.histogram>
                        $forall (value, count) <- q3Histogram
                            <p>#{value}: #{histogramLine count}
                <div.stats>
                    <h3>Notification Support
                    <div.stats-inner>
                        <p>Mean: #{q4Mean}
                        <p>Median: #{q4Median}
                        <p>Mode: #{q4Mode}
                    <div.histogram>
                        $forall (value, count) <- q4Histogram
                            <p>#{value}: #{histogramLine count}
    |]

maximumBy :: (a -> a -> Ordering) -> [a] -> a
maximumBy _ [] = error "maximumBy: empty list"
maximumBy cmp (x:xs) = foldl maxBy x xs
    where
        maxBy x y = if cmp x y == GT then x else y

comparing :: Ord a => (b -> a) -> b -> b -> Ordering
comparing p x y = compare (p x) (p y)

group :: Eq a => [a] -> [[a]]
group [] = []
group (x:xs) = (x : takeWhile (==x) xs) : group (dropWhile (==x) xs)

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort [y | y <- xs, y <= x]
                   ++ [x] ++
                   quicksort [y | y <- xs, y > x]

mean :: [Double] -> Double
mean xs = sum xs / fromIntegral (length xs)

median :: [Double] -> Double
median xs = if odd n then quicksort xs !! mid else mean [quicksort xs !! mid, quicksort xs !! (mid - 1)]
    where n = length xs
          mid = n `div` 2

mode :: Ord a => [a] -> a
mode xs = head $ maximumBy (comparing length) $ group $ quicksort xs

main :: IO ()
main = runStderrLoggingT $ do
    pool <- createSqlitePool "finalproject.sqlite3" 5
    let config = SqliteConf "finalproject.sqlite3" 5
    let app = App config pool
    runSqlPool (runMigration migrateAll) pool
    liftIO $ warp 3000 app