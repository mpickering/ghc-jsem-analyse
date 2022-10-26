{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
module Main(main) where

import Data.Aeson (Value, encode, ToJSON, toJSON, object, (.=), FromJSON, decodeStrict, encodeFile)
import Data.String
import Data.Text (Text, append)
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text.Lazy as TL
--import Text.Blaze.Html
import qualified Text.Blaze.Html5            as H hiding (main, map)
import Text.Blaze.Html5            (Html, AttributeValue, (!), meta, script, body, h1, preEscapedToHtml, link, docTypeHtml)
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.String
import Data.List
import Data.Text.Encoding
import Data.FileEmbed
import GHC.Generics
import GHC.RTS.Events hiding (process, name, time)
import System.Environment
import Debug.Trace
import qualified Data.Map.Strict as Map
import qualified Data.Map.Merge.Strict as Map
import Data.Ord
import qualified Data.Set as Set
import Text.Printf
import qualified Data.OrdPSQ as PS
import Data.Maybe (fromMaybe, catMaybes)
import Data.Tuple (swap)
import Control.Monad (forM_, forM, guard)
import Text.Read ()
import Data.Coerce
import System.Directory
import System.FilePath
import Text.Printf

import Graphics.Vega.VegaLite


data Mode = Simple | Hypothetical

main :: IO ()
main = do
  [dir] <- getArgs
  els <- listDirectory dir
  res <- parseMany dir els
  render "owned.html" owned res
  render "free.html" free res
  render "pending.html" pending res
  let res2 =  simulate (minimum (concatMap (map time . jsems) res)) res
  let elapsed = round (elapsed_time res2 / 1000 )
      (elapsed_m :: Int, elapsed_s) = elapsed `divMod` 60
  putStrLn (printf "Build took %d minutes and %d seconds" elapsed_m elapsed_s )
  putStrLn (printf "Total units built %d" (length res))
  putStrLn "Owned"
  showMap (elapsed_time res2) (time_owned res2)
  putStrLn "Free"
  showMap (elapsed_time res2) (time_free res2)
  putStrLn "Pending"
  showMap (elapsed_time res2) (time_pending res2)



parseMany :: FilePath -> [FilePath] -> IO [Result]
parseMany  dir fps = fmap catMaybes . forM fps $ \fp -> do

  Right !events <- readEventLogFromFile (dir </> fp)
  let !EL{..} = process fp events
      start_event = JSem offset (JSemMessage "start" 0 0 0)
      end_event   = JSem (offset + last_time) (JSemMessage "end" 0 0 0)
  -- This is a simple check to see if the eventlog was from compiling a package or not
--  print (unitTimes result)
  return $ if null (jsems result)
    then  Nothing
    else Just (result { jsems = start_event : end_event : jsems result })



data EL = EL { offset :: Double
             , last_time :: Double
             , result :: !Result }

data JSemEvent = JSem { time :: Double, message :: JSemMessage } deriving Show


data Result = Result { unit_name :: Maybe Text, jsems :: ![JSemEvent] } deriving Show

updateResult :: (Result -> Result) -> EL -> EL
updateResult f el = el { result = f (result el) }

nanoToMilli :: Integral n => n -> Double
nanoToMilli n = (fromIntegral n) / 1_000_000

process :: FilePath -> EventLog -> EL
process fp (EventLog _ (Data es)) =
  let res@EL{..} = foldl' processEvent (EL (error fp) (error fp) (Result Nothing [])) es
  {-
      times = mkDurationModule (match (spans result))
      min_time = minimum (map durationStart times)
      total_time = sum (map duration times)
      [unit] = Map.keys (units result)
  in res {result = result { unitTimes = Map.map (\_ -> Duration unit min_time total_time) (units result)  }}
  -}
  in res

processEvent :: EL -> Event -> EL
processEvent el (Event t ei _) =
  let el' = processEventInfo t ei el
  in el' { last_time = nanoToMilli t }

processEventInfo :: Timestamp -> EventInfo -> EL -> EL
processEventInfo t (WallClockTime _ s ns) el = el { offset = ((fromIntegral s * 1_000) + (nanoToMilli ns)) - nanoToMilli t }
processEventInfo t (ProgramArgs _ l) el =
  case parseUnitId l of
    Just uid -> updateResult (\r -> r { unit_name = Just uid }) el
    Nothing -> el
processEventInfo t (UserMessage m) el = do
  let o = offset el
  case parseMessage m of
    Just m' -> updateResult (\r -> r { jsems = (JSem (nanoToMilli t + o) m'): jsems r }) el
    Nothing -> el
processEventInfo _ _ el = el

parseUnitId [] = Nothing
parseUnitId [_] = Nothing
parseUnitId ("-this-unit-id":uid:xs) = Just uid
parseUnitId (x:xs) = parseUnitId xs

parseMessage :: Text -> Maybe JSemMessage
parseMessage t
  | "jsem:" `T.isPrefixOf` t = decodeStrict (encodeUtf8 (T.drop 5 t))
  | otherwise = Nothing


data JSemMessage = JSemMessage { name :: String, owned, free, pending :: Int } deriving (Generic, FromJSON, Show)


render :: FilePath -> (JSemMessage -> Int) -> [Result] -> IO ()
render fname sel res =
  let lines =
                 [  object [( "uid" .= uid )
                          , "time" .= time
                           , ("num" .= (fromIntegral (sel message) :: Int) ) ]
                 | Result{..} <- res
                 , Just uid <- [unit_name]
                 , JSem{..} <- jsems
                 ]
  in do
    let f = TL.fromStrict (decodeASCII $(embedFile "owned_template.html"))
    let res = TL.replace "MODULES_DATA" (TL.decodeASCII (encode lines)) f
    TL.writeFile fname res

{-
      select_ = selection  . select "sel" Multi [ BindLegend (BLField "uid") ]



      enc = encoding
              . position X [ PName "time", PmType Quantitative ]
              . position Y [ PName "num", PmType Quantitative, PStack StZero ]
              . color [ MName "uid" ]

      bkg = background "rgba(0, 0, 0, 0.05)"

  in toHtmlFile fname $ toVegaLite [ select_ [], width 1000, height 1000, bkg, cars, mark Area [MTooltip TTEncoding], enc [] ]
  -}

{-
render2 :: [Result] -> IO ()
render2 res =
  let cars =  dataFromRows [] . foldr (.) id
                 [ dataRow [ ( "uid", Str uid )
                           , ( "time", Number time )
                           , ( "owned", Number (fromIntegral (owned message)) )
                           , ( "pending", Number (fromIntegral (pending message)) ) ]
                 | Result{..} <- res
                 , Just uid <- [unit_name]
                 , JSem{..} <- jsems
                 ] $ []

      trans = transform . stack "owned" [] "owned_1" "owned_2" []

      -- . Graphics.Vega.VegaLite.filter (FSelection "sel")

--      select_ = selection  . select "sel" Multi [ BindLegend (BLField "uid") ]



      enc = encoding
              . position X [ PName "time", PmType Quantitative ]
              . position Y [ PName "owned", PmType Quantitative ]
              . color [ MName "uid" ]

      enc2 = encoding
              . position X [ PName "time", PmType Quantitative ]
              . position Y [ PName "pending", PmType Quantitative ]
              . color [ MName "uid" ]

      bkg = background "rgba(0, 0, 0, 0.05)"

      reso = resolve . resolution (RScale [ ( ChY, Independent ) ])

      c1 = asSpec [ reso [], width 1000, height 1000, trans [], bkg, mark Area [MTooltip TTEncoding], enc []  ]

      c2 = asSpec [ reso [], width 1000, height 1000, trans [], bkg, mark Area [MTooltip TTEncoding], enc2 [] ]

  in toHtmlFile "render2.html" $ toVegaLite [reso [], cars,  layer [ c1, c2]]
  -}



toDeltas :: [JSemEvent] -> [JSemEvent]
toDeltas es = let ts = sortBy (comparing time) es
              in zipWith go ts (tail ts)
  where

    go (JSem t (JSemMessage s1 d1 d2 d3)) (JSem t2 (JSemMessage s2 d4 d5 d6)) =
      JSem t2 (JSemMessage (s1 ++ "->" ++ s2) (d4 - d1) (d5 - d2) (d6 - d3))


showMap :: Double -> Map.Map Int Double -> IO ()
showMap tot m =
  mapM_ (\(k, v) -> putStrLn $ (show k) ++ ": " ++ (printf "%0.*f" ( 2 ::Int) ((v / tot) * 100)) ++ "%") (Map.assocs m)

data SimState = SimState { cur_time :: !Double, tot_owned, tot_pending, tot_free :: !Int }

data SimResult = SimResult { elapsed_time :: !Double, time_owned, time_pending, time_free :: !(Map.Map Int Double) } deriving Show

simulate :: Double -> [Result] -> SimResult
simulate start_time rs =
  snd $ foldl' simulate_loop (SimState start_time 0 0 0, SimResult 0 Map.empty Map.empty Map.empty) (sortBy (comparing time) (concatMap (toDeltas . jsems) rs))


simulate_loop :: (SimState, SimResult) -> JSemEvent -> (SimState, SimResult)
simulate_loop (ss, sr) (JSem t (JSemMessage _n od fd pd)) =
    let new_time = t
        duration = t - (cur_time ss)

        !sr' = SimResult {
              elapsed_time = elapsed_time sr + duration
              , time_owned = Map.insertWith (+) (tot_owned ss) duration (time_owned sr)
              , time_pending = Map.insertWith (+) (tot_pending ss) duration (time_pending sr)
              , time_free = Map.insertWith (+) (tot_free ss) duration (time_free sr)
              }

        !ss' = SimState new_time (tot_owned ss + od) (tot_pending ss + pd) (tot_free ss + fd)

    in (ss', sr')

