{-# LANGUAGE DeriveDataTypeable #-}
-- | A lightweight implementation of Hspec's API.
module Test.Hspec where

import           Control.Monad
import           Data.Monoid
import           Data.Typeable
import qualified Control.Exception as E
import           Control.Monad.Trans.Writer
import           System.Exit

data SpecTree = SpecGroup String [SpecTree]
              | SpecExample String (IO Result)

data Result = Success | Failure String
  deriving (Eq, Show)

type Spec = Writer [SpecTree] ()

describe :: String -> Spec -> Spec
describe label = tell . return . SpecGroup label . execWriter

context :: String -> Spec -> Spec
context = describe

it :: String -> Expectation -> Spec
it label = tell . return . SpecExample label . evaluateExpectation

-- | Summary of a test run.
data Summary = Summary {
  summaryExamples :: Int
, summaryFailures :: Int
} deriving (Eq, Show)

instance Monoid Summary where
  mempty = Summary 0 0
  (Summary x1 x2) `mappend` (Summary y1 y2) = Summary (x1 + y1) (x2 + y2)

runSpec :: Spec -> IO Summary
runSpec = runForrest . execWriter
  where
    runForrest :: [SpecTree] -> IO Summary
    runForrest = fmap mconcat . mapM runTree

    runTree :: SpecTree -> IO Summary
    runTree spec = case spec of
      SpecExample s x -> do
        r <- x
        case r of
          Success   -> return (Summary 1 0)
          Failure _ -> return (Summary 1 1)
      SpecGroup s xs  -> do
        runForrest xs

hspec :: Spec -> IO ()
hspec spec = do
  Summary total failures <- runSpec spec
  putStrLn (show total ++ " example(s), " ++ show failures ++ " failure(s)")
  when (failures /= 0) exitFailure

type Expectation = IO ()

infix 1 `shouldBe`, `shouldReturn`

shouldBe :: (Show a, Eq a) => a -> a -> Expectation
actual `shouldBe` expected =
  expect ("expected: " ++ show expected ++ "\n but got: " ++ show actual) (actual == expected)

shouldReturn :: (Show a, Eq a) => IO a -> a -> Expectation
action `shouldReturn` expected = action >>= (`shouldBe` expected)

expect :: String -> Bool -> Expectation
expect label f
  | f         = return ()
  | otherwise = E.throwIO (ExpectationFailure label)

data ExpectationFailure = ExpectationFailure String
  deriving (Show, Eq, Typeable)

instance E.Exception ExpectationFailure

evaluateExpectation :: Expectation -> IO Result
evaluateExpectation action = (action >> return Success)
  `E.catches` [
  -- Re-throw AsyncException, otherwise execution will not terminate on SIGINT
  -- (ctrl-c).  All AsyncExceptions are re-thrown (not just UserInterrupt)
  -- because all of them indicate severe conditions and should not occur during
  -- normal operation.
    E.Handler $ \e -> E.throw (e :: E.AsyncException)

  , E.Handler $ \(ExpectationFailure err) -> return (Failure err)
  , E.Handler $ \e -> (return . Failure) ("*** Exception: " ++ show (e :: E.SomeException))
  ]
