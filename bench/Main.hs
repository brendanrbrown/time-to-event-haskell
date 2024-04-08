{-# LANGUAGE StrictData #-}
module Main where

import Test.Tasty.Bench qualified as Bench
import Data.Csv (decodeByName, Header, FromNamedRecord)
import Data.ByteString.Lazy qualified as ByteString
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Vector.Storable qualified as Storable
import Data.Either (either) 
import CoxPH (
  coxph
  , Delta(ObservedEvent)
  , CoxPHMethod(Breslow)
  , ScaleCovariateIndicator(ScaleCovariateNo)
  )
import Data.Text (Text)
import GHC.Generics (Generic)

data CoxPHArgs = CoxPHArgs {
  n :: Int
  , minTimes :: Storable.Vector Double
  , observed :: Vector Delta
  , designMat :: Vector (Storable.Vector Double)
  , weights :: Storable.Vector Double
  , offsets :: Storable.Vector Double
  , strata :: Storable.Vector Int
  , beta0 :: Storable.Vector Double
  , scale :: Vector ScaleCovariateIndicator
  , method :: CoxPHMethod
  , maxIter :: Int
  , tolerance :: Double
} deriving (Show)

data BenchData = BenchData {
  id :: Double
  , age :: Double
  , trt :: Int
  , risk :: Double
  , time :: Double
  , status :: Double
} deriving (Show, Eq, Generic)

runCoxPH :: CoxPHArgs -> Either Text (Storable.Vector Double)
runCoxPH args =
  coxph
  (minTimes args)
  (observed args)
  (designMat args)
  (weights args)
  (offsets args)
  (strata args)
  (beta0 args)
  (scale args)
  (method args)
  (maxIter args)
  (tolerance args)

--  TODO: improve this
-- All variables except id and trt
-- ids of outer Vector are column ids, ie outer vector
-- has length ncols.
dataToDesignMat :: Vector BenchData -> Vector (Storable.Vector Double)
dataToDesignMat = Vector.fromList . 
  map Storable.fromList . foldr op [[], [], [], []]
  where 
    op (BenchData {age, risk, time, status}) [as, rs, ts, ss] =
      [age : as, risk : rs, time : ts, status : ss]

instance FromNamedRecord BenchData

-- | Decode the data into rows.
decodeData :: FilePath -> IO CoxPHArgs
decodeData f = ByteString.readFile f >>=
  either fail (pure . processData . snd) . decodeByName

processData :: Vector BenchData -> CoxPHArgs
processData d = CoxPHArgs { 
  n
  -- TODO: minTimes needs to be computed
  , minTimes = Storable.replicate n 0  
  , observed = Vector.replicate n ObservedEvent
  , designMat = dataToDesignMat d
  , offsets = Storable.replicate n 0
  , weights = Storable.replicate n 1
  , strata = Storable.replicate p 0
  , beta0 = Storable.replicate p 0
  , scale = Vector.replicate p ScaleCovariateNo
  , method = Breslow
  , maxIter = 10000
  , tolerance = 0.01
  }
  where
    -- See dataToDesignMat
    p = 4
    n = Vector.length d

benchCoxPh :: Vector (String, Vector Double) -> [Bench.Benchmark]
benchCoxPh d = undefined
  where
    n = Vector.length d

main :: IO ()
main = do --Bench.defaultMain 
  d <- decodeData "./data/bench-data.csv"
  --either (fail . show) print $ runCoxPH d
  -- TODO: index out of bouds error (999, 4), since cols should be indexed from 0..3
  -- Trace is no help
  print $ Vector.length $ designMat d
  print $ Storable.length $ Vector.head $ designMat d
 

