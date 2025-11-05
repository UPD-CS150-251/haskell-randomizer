{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Functor law" #-}
{-# HLINT ignore "Redundant <&>" #-}

module Main where

import Control.Arrow ((>>>))
import Control.Monad (forM_)
import Data.Function ((&))
import Data.Functor ((<&>))
import qualified Data.List as List
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import List.Shuffle (shuffleIO)
import System.Environment (getArgs)
import System.FilePath.Glob (glob)

basepath :: Text
basepath = "data/"

newtype Student = Student Text
  deriving (Show, Eq, Ord)

data LabGroup
  = Solo Student
  | Pair Student Student
  | Trio Student Student Student
  deriving (Show, Eq, Ord)

mkLabPair :: Student -> Student -> LabGroup
mkLabPair a b = if a < b then Pair a b else Pair b a

mkLabTrio :: Student -> Student -> Student -> LabGroup
mkLabTrio a b c =
  case List.sort [a, b, c] of
    [x, y, z] -> Trio x y z
    _ -> error "mkLabTrio: impossible pattern"

main :: IO ()
main = do
  getArgs <&> fmap T.pack >>= \case
    [] -> showErrorMsg
    [_] -> showErrorMsg
    (requirement : sections) -> do
      forM_ sections $ \section -> do
        generateNewLabGroups requirement section
 where
  showErrorMsg = putStrLn "Usage: cabal run . lab07 fru fwx1 fwx2 fyz"

generateNewLabGroups :: Text -> Text -> IO ()
generateNewLabGroups requirement section = do
  students <- getStudents section
  prevPairs <- getExistingGroups section
  groups <- makeNewLabGroups students prevPairs
  let output = groupsToText . Set.toList $ groups

  T.writeFile outputPath output
 where
  outputPath = T.unpack (basepath <> requirement <> "-" <> section <> ".txt")

  groupsToText :: [LabGroup] -> Text
  groupsToText xs = T.intercalate "\n" $ fmap groupToText xs

  groupToText :: LabGroup -> Text
  groupToText (Solo (Student x)) = x
  groupToText (Pair (Student x) (Student y)) = T.intercalate ", " [x, y]
  groupToText (Trio (Student x) (Student y) (Student z)) = T.intercalate ", " [x, y, z]

makeNewLabGroups :: [Student] -> Set LabGroup -> IO (Set LabGroup)
makeNewLabGroups students prevPairs = do
  shuffled <- shuffleIO students
  let groups = pairAllAdjacent shuffled

  if any (`elem` prevPairs) groups
    then
      makeNewLabGroups students prevPairs
    else
      pure $ Set.fromList groups
 where
  pairAllAdjacent :: [Student] -> [LabGroup]
  pairAllAdjacent [] = []
  pairAllAdjacent [x] = [Solo x]
  pairAllAdjacent [x, y, z] = [Trio x y z]
  pairAllAdjacent (x : y : rest) = mkLabPair x y : pairAllAdjacent rest

getStudents :: Text -> IO [Student]
getStudents section =
  filepath
    & T.unpack
    & T.readFile
    <&> (T.strip >>> T.toUpper)
    <&> T.splitOn "\n"
    <&> fmap Student
 where
  filepath = basepath <> section <> ".txt"

getExistingGroups :: Text -> IO (Set LabGroup)
getExistingGroups section =
  sectionPaths
    >>= mapM T.readFile
    <&> T.concat
    <&> (T.strip >>> T.toUpper)
    <&> T.splitOn "\n"
    <&> fmap (T.splitOn ", ")
    <&> fmap (fmap Student)
    <&> mapMaybe maybeMkPair
    <&> Set.fromList
 where
  globPattern :: String
  globPattern = T.unpack basepath <> "*-" <> T.unpack section <> ".txt"

  sectionPaths :: IO [FilePath]
  sectionPaths = glob globPattern

  maybeMkPair :: [Student] -> Maybe LabGroup
  maybeMkPair [a, b] = Just $ mkLabPair a b
  maybeMkPair _ = Nothing
