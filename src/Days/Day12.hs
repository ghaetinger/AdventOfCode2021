module Days.Day12 where

import           Data.List
import           Data.List.Split

import           Data.Bifunctor

import           Data.Map                       ( Map )
import qualified Data.Map                      as Map

import           Data.Set                       ( Set
                                                , member
                                                )
import qualified Data.Set                      as Set
import           GHC.Unicode                    ( isLower )

import           Util.ListTools

firstQuestion :: String -> IO Int
firstQuestion filename = do
  routeMap <- readRoute filename
  let routes =
        (rmdups . filter (\path -> path !! (length path - 1) == "end"))
          $ constructRoutes ((filterStartRoutes . filterEndRoutes) routeMap)
                            Set.empty
                            "start"
  return (length routes)

secondQuestion :: String -> IO Int
secondQuestion filename = do
  routeMap <- readRoute filename
  let routes =
        (rmdups . filter (\path -> path !! (length path - 1) == "end"))
          $ constructRoutesAllowDups
              ((filterStartRoutes . filterEndRoutes) routeMap)
              False
              Set.empty
              "start"
  return (length routes)

filterStartRoutes :: Map String (Set String) -> Map String (Set String)
filterStartRoutes =
  Map.fromList
    . map
        (Data.Bifunctor.second (Set.fromList . filter (/= "start") . Set.toList)
        )
    . Map.toList

filterEndRoutes :: Map String (Set String) -> Map String (Set String)
filterEndRoutes = Map.delete "end"

constructRoutes
  :: Map String (Set String) -> Set String -> String -> [[String]]
constructRoutes m visited s
  | s `member` visited = [[]]
  | otherwise = [s] : concatMap
    (map (s :) . constructRoutes
      m
      (if isCaveSmall s then Set.insert s visited else visited)
    )
    ((Set.toList . Map.findWithDefault Set.empty s) m)

constructRoutesAllowDups
  :: Map String (Set String) -> Bool -> Set String -> String -> [[String]]
constructRoutesAllowDups m b visited s
  | b = constructRoutes m visited s
  | otherwise = [s] : concatMap
    (map (s :) . constructRoutesAllowDups
      m
      (isCaveSmall s && s `member` visited)
      (if isCaveSmall s then Set.insert s visited else visited)
    )
    ((Set.toList . Map.findWithDefault Set.empty s) m)

isCaveSmall :: String -> Bool
isCaveSmall = all isLower

readRoute :: String -> IO (Map String (Set String))
readRoute filename = do
  contents <- readFile filename
  return
    (( foldl
         (\rmap (orig, end) ->
           ( Map.insertWith Set.union orig (Set.singleton end)
             . Map.insertWith Set.union end (Set.singleton orig)
             )
             rmap
         )
         Map.empty
     . concatMap getLineConnections
     . lines
     )
      contents
    )

getLineConnections :: String -> [(String, String)]
getLineConnections =
  (\ls -> zip (head ls : take (length ls - 1) ls) (tail ls)) . splitOn "-"
