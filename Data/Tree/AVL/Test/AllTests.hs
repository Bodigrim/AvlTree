{-# OPTIONS -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Tree.AVL.Test.AllTests
-- Copyright   :  (c) Adrian Hey 2004,2005,2006,2007
-- License     :  BSD3
--
-- Maintainer  :  http://homepages.nildram.co.uk/~ahey/em.png
-- Stability   :  unstable
-- Portability :  portable
--
-- This module contains a large set of fairly comprehensive but extremely
-- time consuming tests of AVL tree functions (not based on QuickCheck).
--
-- They can all be run using 'allTests', or they can be run individually.
-----------------------------------------------------------------------------
module Data.Tree.AVL.Test.AllTests
(allTests
,testReadPath
,testIsBalanced
,testIsSorted
,testSize
,testClipSize
,testGenWrite
,testGenPush
,testPushL
,testPushR
,testGenDel
,testAssertDelL
,testAssertDelR
,testAssertPopL
,testPopHL
,testAssertPopR
,testGenAssertPop
,testFlatten
,testJoin
,testJoinHAVL
,testConcatAVL
,testFlatConcat
,testFoldrAVL
,testFoldrAVL'
,testFoldlAVL
,testFoldlAVL'
,testFoldr1AVL
,testFoldr1AVL'
,testFoldl1AVL
,testFoldl1AVL'
,testMapAccumLAVL
,testMapAccumRAVL
,testMapAccumLAVL'
,testMapAccumRAVL'
#ifdef __GLASGOW_HASKELL__
,testMapAccumLAVL''
,testMapAccumRAVL''
#endif
,testSplitAtL
,testFilterViaList
,testFilterAVL
,testMapMaybeViaList
,testMapMaybeAVL
,testTakeL
,testDropL
,testSplitAtR
,testTakeR
,testDropR
,testSpanL
,testTakeWhileL
,testDropWhileL
,testSpanR
,testTakeWhileR
,testDropWhileR
,testRotateL
,testRotateR
,testRotateByL
,testRotateByR
,testGenForkL
,testGenForkR
,testGenFork
,testGenTakeLE
,testGenTakeGT
,testGenTakeGE
,testGenTakeLT
,testGenUnion
,testGenUnionMaybe
,testGenIntersection
,testGenIntersectionMaybe
,testGenIntersectionAsListL
,testGenIntersectionMaybeAsListL
,testGenDifference
,testGenDifferenceMaybe
,testGenSymDifference
,testGenIsSubsetOf
,testGenIsSubsetOfBy
,testCompareHeight
,testShowReadEq
-- Zipper tests
,testGenOpenClose
,testDelClose
,testOpenLClose
,testOpenRClose
,testMoveL
,testMoveR
,testInsertL
,testInsertMoveL
,testInsertR
,testInsertMoveR
,testInsertTreeL
,testInsertTreeR
,testDelMoveL
,testDelMoveR
,testDelAllL
,testDelAllR
,testDelAllCloseL
,testDelAllIncCloseL
,testDelAllCloseR
,testDelAllIncCloseR
,testZipSize
,testGenTryOpenLE
,testGenTryOpenGE
,testGenOpenEither
,testBAVLtoZipper
) where

import Data.COrdering
import Data.Tree.AVLX

import Data.List(insert,mapAccumL,mapAccumR)
import System.Exit(exitFailure)

#ifdef __GLASGOW_HASKELL__
import GHC.Base
#include "ghcdefs.h"
#else
#include "h98defs.h"
#endif


-- import Debug.Trace(trace)
-- import System.IO.Unsafe(unsafePerformIO)

-- | Run every test in this module (takes a very long time).
allTests :: IO ()
allTests =
 do testReadPath
    testIsBalanced
    testIsSorted
    testSize
    testClipSize
    testGenWrite
    testGenPush
    testPushL
    testPushR
    testGenDel
    testAssertDelL
    testAssertDelR
    testAssertPopL
    testPopHL
    testAssertPopR
    testGenAssertPop
    testFlatten
    testJoin
    testJoinHAVL
    testConcatAVL
    testFlatConcat
    testFoldrAVL
    testFoldrAVL'
    testFoldlAVL
    testFoldlAVL'
    testFoldr1AVL
    testFoldr1AVL'
    testFoldl1AVL
    testFoldl1AVL'
    testMapAccumLAVL
    testMapAccumRAVL
    testMapAccumLAVL'
    testMapAccumRAVL'
#ifdef __GLASGOW_HASKELL__
    testMapAccumLAVL''
    testMapAccumRAVL''
#endif
    testSplitAtL
    testFilterViaList
    testFilterAVL
    testMapMaybeViaList
    testMapMaybeAVL
    testTakeL
    testDropL
    testSplitAtR
    testTakeR
    testDropR
    testSpanL
    testTakeWhileL
    testDropWhileL
    testSpanR
    testTakeWhileR
    testDropWhileR
    testRotateL
    testRotateR
    testRotateByL
    testRotateByR
    testGenForkL
    testGenForkR
    testGenFork
    testGenTakeLE
    testGenTakeGT
    testGenTakeGE
    testGenTakeLT
    testGenUnion
    testGenUnionMaybe
    testGenIntersection
    testGenIntersectionMaybe
    testGenIntersectionAsListL
    testGenIntersectionMaybeAsListL
    testGenDifference
    testGenDifferenceMaybe
    testGenSymDifference
    testGenIsSubsetOf
    testGenIsSubsetOfBy
    testCompareHeight
    testShowReadEq
-- Zipper tests
    testGenOpenClose
    testDelClose
    testOpenLClose
    testOpenRClose
    testMoveL
    testMoveR
    testInsertL
    testInsertMoveL
    testInsertR
    testInsertMoveR
    testInsertTreeL
    testInsertTreeR
    testDelMoveL
    testDelMoveR
    testDelAllL
    testDelAllR
    testDelAllCloseL
    testDelAllIncCloseL
    testDelAllCloseR
    testDelAllIncCloseR
    testZipSize
    testGenTryOpenLE
    testGenTryOpenGE
    testGenOpenEither
    testBAVLtoZipper


-- | Test isBalanced is capable of failing for a few non-AVL trees.
testIsBalanced :: IO ()
testIsBalanced = do title "isBalanced"
                    if or [isBalanced t | t <- nonAVLs] then failed else passed
 where nonAVLs :: [AVL Int]
       nonAVLs = [Z E 0 (Z E 0 E)
                 ,Z (Z E 0 E) 0 E
                 ,N E 0 E
                 ,P E 0 E
                 ]

-- | Test isSorted is capable of failing for a few non-sorted trees.
testIsSorted :: IO ()
testIsSorted = do title "isSorted"
                  if or [isSorted compare (asTreeL l) | l <- nonSorted] then failed else passed
 where nonSorted = ["AA","BA"
                   ,"AAA","ABA","ABB","AAB"
                   ,"AABC","ACBA","ABCC","ABBB","AAAB"
                   ]

-- | Test size function
testSize :: IO ()
testSize = do title "size"
              exhaustiveTest test (take 6 allAVL)
           where test _ s t = size t == s

-- | Test clipSize function
testClipSize :: IO ()
testClipSize = do title "clipSize"
                  exhaustiveTest test (take 6 allAVL)
               where test _ s t = all (== Nothing) [clipSize n t | n <- [0..s-1 ]] &&
                                  all (== Just s ) [clipSize n t | n <- [s..s+10]]

-- | Test genWrite function
testGenWrite :: IO ()
testGenWrite = do title "genWrite"
                  exhaustiveTest test (take 5 allNonEmptyAVL)
               where test _ s t = all test_ [0..s-1]
                      where test_ n = let t_ = genWrite (withCC' (+) n) t
                                      in isBalanced t_ && (asListL t_ == [0..n-1]++(n+n):[n+1..s-1])


-- | Test genPush function
testGenPush :: IO ()
-- Also exercises: mapAVL' and genContains
testGenPush = do title "genPush"
                 exhaustiveTest test (take 6 allAVL)
              where test h s t = all oddTest odds && all evenTest evens
                     where t_ = mapAVL' (\n -> 2*n+1) t        -- t_ elements are odd, 1,3..2*s-1
                           odds  = [1,3..2*s-1]
                           evens = [0,2..2*s  ]
                           oddTest  n = let t__ = push n t_     -- Should yield identical trees
                                            s__ = size   t__
                                            h__ = ASINT(height t__)
                                        in (s__ == s) && (isSortedOK compare t__) && (h__== h)
                           evenTest n = let t__ = push n t_
                                            s__ = size   t__
                                            h__ = ASINT(height t__)
                                        in (s__ == s+1) && (isSortedOK compare t__) && (h__-h <= 1) && (t__ `contains` n)
                           push e = genPush (sndCC e) e
                           contains avl e = genContains avl (compare e)

-- | Test genDel function
testGenDel :: IO ()
testGenDel = do title "genDel"
                exhaustiveTest test (take 5 allNonEmptyAVL)
             where test h s t = all oddTest odds && all evenTest evens
                    where t_ = mapAVL' (\n -> 2*n+1) t        -- t_ elements are odd, 1,3..2*s-1
                          odds  = [1,3..2*s-1]
                          evens = [0,2..2*s  ]
                          oddTest  n = let t__ = del n t_
                                       in case checkHeight t__ of
                                          Just h_ -> (h-h_<=1) && (insert n (asListL t__) == odds)
                                          Nothing -> False
                          evenTest n = let t__ = del n t_
                                       in case checkHeight t__ of
                                          Just h_ -> (h==h_) && (asListL t__ == odds)
                                          Nothing -> False
                          del e = genDel (compare e)

-- | Test genAssertPop function
testGenAssertPop :: IO ()
testGenAssertPop =
 do title "genAssertPop"
    exhaustiveTest test (take 5 allNonEmptyAVL)
 where test h s t = all testElem elems
        where elems = [0,1..s-1]
              testElem n = let (n_,t_) = genAssertPop (fstCC n) t
                           in case checkHeight t_ of
                              Just h_ -> (h-h_<=1) && (insert n_ (asListL t_) == elems)
                              Nothing -> False

-- | Test pushL function
-- Also exercises: asListL
testPushL :: IO ()
testPushL = do title "pushL"
               exhaustiveTest test (take 6 allAVL)
            where test h _ t = let t_ = 0 `pushL` t
                               in case checkHeight t_ of
                                  Just h_ | (h_==h+1) || (h_==h)  -> asListL t_ == (0 : asListL t)
                                  _                               -> False

-- | Test pushR function
-- Also exercises: asListR
testPushR :: IO ()
testPushR = do title "pushR"
               exhaustiveTest test (take 6 allAVL)
            where test h s t = let t_ = t `pushR` s
                               in case checkHeight t_ of
                                  Just h_ | (h_==h+1) || (h_==h)  -> asListR t_ == (s : asListR t)
                                  _                               -> False

-- | Test assertDelL function
-- Also exercises: asListL
testAssertDelL :: IO ()
testAssertDelL =
 do title "assertDelL"
    exhaustiveTest test (take 5 allNonEmptyAVL)
 where test h _ t = let t_ = assertDelL t
                    in case checkHeight t_ of
                       Just h_ | (h_==h-1) || (h_==h)  -> asListL t_ == (tail $ asListL t)
                       _                               -> False

-- | Test delR function
-- Also exercises: asListR
testAssertDelR :: IO ()
testAssertDelR =
 do title "assertDelR"
    exhaustiveTest test (take 5 allNonEmptyAVL)
 where test h _ t = let t_ = assertDelR t
                    in case checkHeight t_ of
                       Just h_ | (h_==h-1) || (h_==h)  -> asListR t_ == (tail $ asListR t)
                       _                               -> False

-- | Test assertPopL function
-- Also exercises: asListL
testAssertPopL :: IO ()
testAssertPopL =
 do title "assertPopL"
    exhaustiveTest test (take 5 allNonEmptyAVL)
 where test h _ t = let (v,t_) = assertPopL t
                    in case checkHeight t_ of
                       Just h_ | (h_==h-1) || (h_==h)  -> (v : asListL t_) == asListL t
                       _                               -> False

-- | Test popHL function
-- This test can only be run if popHL and HAVL are not hidden.
-- However, popHL is exercised by indirectly by testConcatAVL anyway
testPopHL :: IO ()
testPopHL = do title "popHL"
               exhaustiveTest test (take 5 allNonEmptyAVL)
            where test _ _ t = let UBT3(v, t_,h) = popHL t
                               in case checkHeight t_ of
                                  Just h_ | (h_== ASINT(h)) -> (v : asListL t_) == asListL t
                                  _                          -> False


-- | Test assertPopR function
-- Also exercises: asListR
testAssertPopR :: IO ()
testAssertPopR =
 do title "assertPopR"
    exhaustiveTest test (take 5 allNonEmptyAVL)
 where test h _ t = let (t_,v) = assertPopR t
                    in case checkHeight t_ of
                       Just h_ | (h_==h-1) || (h_==h)  -> (v : asListR t_) == asListR t
                       _                               -> False

-- | Test flatten function
-- Also exercises: asListL,replicateAVL
testFlatten :: IO ()
testFlatten = do title "flatten"
                 exhaustiveTest test (take 6 allAVL)
              where test _ _ t = let t_ = flatten t
                                 in isBalanced t_ && (asListL t == asListL t_)

-- | Test foldrAVL
testFoldrAVL :: IO ()
testFoldrAVL = do title "foldrAVL"
                  exhaustiveTest test (take 6 allAVL)
               where test _ s t = foldrAVL (:) [] t == [0..s-1]
-- | Test foldrAVL'
testFoldrAVL' :: IO ()
testFoldrAVL' = do title "foldrAVL'"
                   exhaustiveTest test (take 6 allAVL)
                where test _ s t = foldrAVL' (:) [] t == [0..s-1]
-- | Test foldlAVL
testFoldlAVL :: IO ()
testFoldlAVL = do title "foldlAVL"
                  exhaustiveTest test (take 6 allAVL)
               where test _ s t = foldlAVL (flip (:)) [] t == [s-1,s-2..0]
-- | Test foldlAVL'
testFoldlAVL' :: IO ()
testFoldlAVL' = do title "foldlAVL'"
                   exhaustiveTest test (take 6 allAVL)
                where test _ s t = foldlAVL' (flip (:)) [] t == [s-1,s-2..0]
-- | Test foldr1AVL
testFoldr1AVL :: IO ()
testFoldr1AVL = do title "foldr1AVL"
                   exhaustiveTest test (take 5 allNonEmptyAVL)
                where test _ s t = foldr1AVL (-) t == foldr1 (-) [0..s-1]
-- | Test foldr1AVL'
testFoldr1AVL' :: IO ()
testFoldr1AVL' = do title "foldr1AVL'"
                    exhaustiveTest test (take 5 allNonEmptyAVL)
                 where test _ s t = foldr1AVL' (-) t == foldr1 (-) [0..s-1]
-- | Test foldl1AVL
testFoldl1AVL :: IO ()
testFoldl1AVL = do title "foldl1AVL"
                   exhaustiveTest test (take 5 allNonEmptyAVL)
                where test _ s t = foldl1AVL (-) t == foldl1 (-) [0..s-1]
-- | Test foldl1AVL'
testFoldl1AVL' :: IO ()
testFoldl1AVL' = do title "foldl1AVL'"
                    exhaustiveTest test (take 5 allNonEmptyAVL)
                 where test _ s t = foldl1AVL' (-) t == foldl1 (-) [0..s-1]

-- | Test mapAccumLAVL
testMapAccumLAVL :: IO ()
testMapAccumLAVL = do title "mapAccumLAVL"
                      exhaustiveTest test (take 6 allAVL)
 where test _ _ t = let (nt,t') = mapAccumLAVL f 0 t
                        (nl,l ) = mapAccumL f 0 (asListL t)
                    in (nt==nl) && ((asListL t') == l) && (isSortedOK compare t')
       f acc n = (acc+n,n+1)

-- | Test mapAccumRAVL
testMapAccumRAVL :: IO ()
testMapAccumRAVL = do title "mapAccumRAVL"
                      exhaustiveTest test (take 6 allAVL)
 where test _ _ t = let (nt,t') = mapAccumRAVL f 0 t
                        (nl,l ) = mapAccumR f 0 (asListL t)
                    in (nt==nl) && ((asListL t') == l) && (isSortedOK compare t')
       f acc n = (acc+n,n+1)

-- | Test mapAccumLAVL'
testMapAccumLAVL' :: IO ()
testMapAccumLAVL' = do title "mapAccumLAVL'"
                       exhaustiveTest test (take 6 allAVL)
 where test _ _ t = let (nt,t') = mapAccumLAVL' f 0 t
                        (nl,l ) = mapAccumL f 0 (asListL t)
                    in (nt==nl) && ((asListL t') == l) && (isSortedOK compare t')
       f acc n = (acc+n,n+1)

-- | Test mapAccumRAVL'
testMapAccumRAVL' :: IO ()
testMapAccumRAVL' = do title "mapAccumRAVL'"
                       exhaustiveTest test (take 6 allAVL)
 where test _ _ t = let (nt,t') = mapAccumRAVL' f 0 t
                        (nl,l ) = mapAccumR f 0 (asListL t)
                    in (nt==nl) && ((asListL t') == l) && (isSortedOK compare t')
       f acc n = (acc+n,n+1)

#ifdef __GLASGOW_HASKELL__
-- | Test mapAccumLAVL''
testMapAccumLAVL'' :: IO ()
testMapAccumLAVL'' = do title "mapAccumLAVL''"
                        exhaustiveTest test (take 6 allAVL)
 where test _ _ t = let (nt,t') = mapAccumLAVL'' f_ 0 t
                        (nl,l ) = mapAccumL f 0 (asListL t)
                    in (nt==nl) && ((asListL t') == l) && (isSortedOK compare t')
       f_ acc n = UBT2(acc+n,n+1)
       f  acc n =     (acc+n,n+1)

-- | Test mapAccumRAVL''
testMapAccumRAVL'' :: IO ()
testMapAccumRAVL'' = do title "mapAccumRAVL''"
                        exhaustiveTest test (take 6 allAVL)
 where test _ _ t = let (nt,t') = mapAccumRAVL'' f_ 0 t
                        (nl,l ) = mapAccumR f 0 (asListL t)
                    in (nt==nl) && ((asListL t') == l) && (isSortedOK compare t')
       f_ acc n = UBT2(acc+n,n+1)
       f  acc n =     (acc+n,n+1)
#endif

-- | Test the join function
testJoin :: IO ()
testJoin = let trees = take num $ concatMap (\(_,ts) -> ts) allAVL
               num   = 2000
           in do title "join"
                 putStrLn $ "Testing " ++ show (num*num) ++ " tree pairs.."
                 if and [test l $ mapAVL (ls+) r | (l,ls) <- trees, (r,_) <- trees] then passed else failed
              where test l r = let j = l `join` r
                               in  isBalanced j && (asListL j == l `toListL` asListL r)

-- | Test the joinHAVL function
testJoinHAVL :: IO ()
testJoinHAVL = let trees = take num $ concatMap (\(_,ts) -> ts) allAVL
                   num   = 2000
               in do title "joinHAVL"
                     putStrLn $ "Testing " ++ show (num*num) ++ " tree pairs.."
                     if and [test l $ mapAVL (ls+) r | (l,ls) <- trees, (r,_) <- trees] then passed else failed
                  where test l r = let (HAVL j hj) = (toHAVL l) `joinHAVL` (toHAVL r)
                                   in  case checkHeight j of
                                       Nothing  -> False
                                       Just hj_ -> (ASINT(hj) == hj_) && (asListL j == l `toListL` asListL r)

-- | Test the concatAVL function.
testConcatAVL :: IO ()
testConcatAVL = let trees = take num $ concatMap (\(_,ts) -> ts) allAVL
                    num   = 2000
                in do title "concatAVL"
                      putStrLn $ "Testing " ++ show (num*num) ++ " tree pairs.."
                      if others && and [test ls l $ mapAVL (\n -> n+(ls+1)) r
                                       | (l,ls) <- trees, (r,_) <- trees]
                         then passed else failed
                where test ls l r = let j = concatAVL $ [empty,empty,l,empty,singleton ls,empty,r,empty,empty]
                                    in  isBalanced j && (asListL j == l `toListL` (ls:asListL r))
                      others =    all (isEmpty . concatAVL) [[],[empty],[empty,empty],[empty,empty,empty]]
                               && (all test1 $ concatMap (\ss -> [ss,"":ss,"Z":ss])
                                    [[""]
                                    ,["A"]
                                    ,["","A","BC","","D","","EFGH","I"]
                                    ]
                                  )
                      test1 ss = let t = concatAVL $ map asTreeL ss
                                 in isBalanced t && (asListL t == concat ss)

-- | Test the flatConcat function.
testFlatConcat :: IO ()
testFlatConcat = let trees = take num $ concatMap (\(_,ts) -> ts) allAVL
                     num   = 2000
                 in do title "flatConcat"
                       putStrLn $ "Testing " ++ show (num*num) ++ " tree pairs.."
                       if others && and [test ls l $ mapAVL (\n -> n+(ls+1)) r
                                        | (l,ls) <- trees, (r,_) <- trees]
                          then passed else failed
                 where test ls l r = let j = flatConcat $ [empty,empty,l,empty,singleton ls,empty,r,empty,empty]
                                     in  isBalanced j && (asListL j == l `toListL` (ls:asListL r))
                       others =    all (isEmpty . flatConcat) [[],[empty],[empty,empty],[empty,empty,empty]]
                                && (all test1 $ concatMap (\ss -> [ss,"":ss,"Z":ss])
                                     [[""]
                                     ,["A"]
                                     ,["","A","BC","","D","","EFGH","I"]
                                     ]
                                   )
                       test1 ss = let t = flatConcat $ map asTreeL ss
                                  in isBalanced t && (asListL t == concat ss)

-- | Test the filterViaList function
testFilterViaList :: IO ()
testFilterViaList = do title "filterViaList"
                       exhaustiveTest test (take 6 allAVL)
                    where test _ s t = all testit [0..s] -- n==s should yield unmodified tree
                           where testit n = let t' = filterViaList (/= n) t
                                            in (isSortedOK compare t') && (asListL t' == ([0..n-1]++[n+1..s-1]))

-- | Test the filterAVL function
testFilterAVL :: IO ()
testFilterAVL = do title "filterAVL"
                   exhaustiveTest test (take 6 allAVL)
                where test _ s t = all testit [0..s] -- n==s should yield unmodified tree
                       where testit n = let t' = filterAVL (/= n) t
                                        in (isSortedOK compare t') && (asListL t' == ([0..n-1]++[n+1..s-1]))

-- | Test the mapMaybeViaList function
testMapMaybeViaList :: IO ()
testMapMaybeViaList = do title "mapMaybeViaList"
                         exhaustiveTest test (take 6 allAVL)
                      where test _ s t = all testit [0..s] -- n==s should yield unmodified tree
                             where testit n = let t' = mapMaybeViaList (\m -> if m==n then Nothing else Just m) t
                                              in (isSortedOK compare t') && (asListL t' == ([0..n-1]++[n+1..s-1]))

-- | Test the mapMaybeAVL function
testMapMaybeAVL :: IO ()
testMapMaybeAVL = do title "mapMaybeAVL"
                     exhaustiveTest test (take 6 allAVL)
                  where test _ s t = all testit [0..s] -- n==s should yield unmodified tree
                         where testit n = let t' = mapMaybeAVL (\m -> if m==n then Nothing else Just m) t
                                          in (isSortedOK compare t') && (asListL t' == ([0..n-1]++[n+1..s-1]))

-- | Test splitAtL function
testSplitAtL :: IO ()
testSplitAtL = do title "splitAtL"
                  exhaustiveTest test (take 6 allAVL)
               where test _ s t = all splitTest0 [0..s-1] && all splitTest1 [s]
                      where tlist = asListL t
                            splitTest0 n = case splitAtL n t of
                                           Left  _     -> False
                                           Right (l,r) -> (isBalanced l) && (isBalanced r) &&
                                                          (size l == n) && (size r == s-n) &&
                                                          (l `toListL` asListL r) == tlist
                            splitTest1 n = case splitAtL n t of
                                           Left  s_ -> s_==s
                                           Right _  -> False

-- | Test takeL function
testTakeL :: IO ()
testTakeL = do title "takeL"
               exhaustiveTest test (take 6 allAVL)
            where test _ s t = all takeTest0 [0..s-1] && all takeTest1 [s]
                   where takeTest0 n = case takeL n t of
                                       Left  _ -> False
                                       Right l -> (isBalanced l) && (asListL l) == [0..n-1]
                         takeTest1 n = case takeL n t of
                                       Left  s_ -> s_==s
                                       Right _  -> False

-- | Test dropL function
testDropL :: IO ()
testDropL = do title "dropL"
               exhaustiveTest test (take 6 allAVL)
            where test _ s t = all dropTest0 [0..s-1] && all dropTest1 [s]
                   where dropTest0 n = case dropL n t of
                                       Left  _ -> False
                                       Right r -> (isBalanced r) && (asListL r) == [n..s-1]
                         dropTest1 n = case dropL n t of
                                       Left  s_ -> s_==s
                                       Right _  -> False

-- | Test splitAtR function
testSplitAtR :: IO ()
testSplitAtR = do title "splitAtR"
                  exhaustiveTest test (take 6 allAVL)
               where test _ s t = all splitTest0 [0..s-1] && all splitTest1 [s]
                      where tlist = asListR t
                            splitTest0 n = case splitAtR n t of
                                           Left  _     -> False
                                           Right (l,r) -> (isBalanced l) && (isBalanced r) &&
                                                          (size r == n) && (size l == s-n) &&
                                                          (r `toListR` asListR l) == tlist
                            splitTest1 n = case splitAtR n t of
                                           Left  s_ -> s_==s
                                           Right _  -> False

-- | Test takeR function
testTakeR :: IO ()
testTakeR = do title "takeR"
               exhaustiveTest test (take 6 allAVL)
            where test _ s t = all takeTest0 [0..s-1] && all takeTest1 [s]
                   where takeTest0 n = case takeR n t of
                                       Left  _ -> False
                                       Right r -> (isBalanced r) && (asListL r) == [s-n..s-1]
                         takeTest1 n = case takeR n t of
                                       Left  s_ -> s_==s
                                       Right _  -> False

-- | Test dropR function
testDropR :: IO ()
testDropR = do title "dropR"
               exhaustiveTest test (take 6 allAVL)
            where test _ s t = all dropTest0 [0..s-1] && all dropTest1 [s]
                   where dropTest0 n = case dropR n t of
                                       Left  _ -> False
                                       Right l -> (isBalanced l) && (asListL l) == [0..(s-1)-n]
                         dropTest1 n = case dropR n t of
                                       Left  s_ -> s_==s
                                       Right _  -> False

-- | Test spanL function
testSpanL :: IO ()
testSpanL = do title "spanL"
               exhaustiveTest test (take 6 allAVL)
            where test _ s t = all spanTest [0..s]
                   where tlist = asListL t
                         spanTest n = let (l ,r ) = spanL (<n) t
                                          (l_,r_) = span  (<n) tlist
                                      in (isBalanced l) && (isBalanced r) &&
                                         (asListL l == l_) && (asListL r == r_)

-- | Test takeWhileL function
testTakeWhileL :: IO ()
testTakeWhileL = do title "takeWhileL"
                    exhaustiveTest test (take 6 allAVL)
                 where test _ s t = all spanTest [0..s]
                        where tlist = asListL t
                              spanTest n = let l  = takeWhileL (<n) t
                                               l_ = takeWhile  (<n) tlist
                                           in (isBalanced l) && (asListL l == l_)

-- | Test dropWhileL function
testDropWhileL :: IO ()
testDropWhileL = do title "dropWhileL"
                    exhaustiveTest test (take 6 allAVL)
                 where test _ s t = all spanTest [0..s]
                        where tlist = asListL t
                              spanTest n = let r  = dropWhileL (<n) t
                                               r_ = dropWhile  (<n) tlist
                                           in (isBalanced r) && (asListL r == r_)

-- | Test spanR function
testSpanR :: IO ()
testSpanR = do title "spanR"
               exhaustiveTest test (take 6 allAVL)
            where test _ s t = all spanTest [0..s]
                   where tlist = asListR t
                         spanTest n = let (l ,r ) = spanR (>=n) t
                                          (l_,r_) = span  (>=n) tlist
                                      in (isBalanced l) && (isBalanced r) &&
                                         (asListR l == r_) && (asListR r == l_)

-- | Test takeWhileR function
testTakeWhileR :: IO ()
testTakeWhileR = do title "takeWhileR"
                    exhaustiveTest test (take 6 allAVL)
                 where test _ s t = all spanTest [0..s]
                        where tlist = asListR t
                              spanTest n = let r  = takeWhileR (>=n) t
                                               r_ = takeWhile  (>=n) tlist
                                           in (isBalanced r) && (asListR r == r_)

-- | Test dropWhileR function
testDropWhileR :: IO ()
testDropWhileR = do title "dropWhileR"
                    exhaustiveTest test (take 6 allAVL)
                 where test _ s t = all spanTest [0..s]
                        where tlist = asListR t
                              spanTest n = let l  = dropWhileR (>=n) t
                                               l_ = dropWhile  (>=n) tlist
                                           in (isBalanced l) && (asListR l == l_)

-- | Test rotateL function
testRotateL :: IO ()
testRotateL = do title "rotateL"
                 exhaustiveTest test (take 6 allAVL)
              where test _ s t = all isOK rotations
                     where rotations = take s $ tail $ iterate (mapAVL' (\n -> (n-1) `mod` s) . rotateL) t
                           isOK t_ = (isBalanced t_) && (asListL t_ == tlist)
                           tlist   = asListL t
-- | Test rotateR function
testRotateR :: IO ()
testRotateR = do title "rotateR"
                 exhaustiveTest test (take 6 allAVL)
              where test _ s t = all isOK rotations
                     where rotations = take s $ tail $ iterate (mapAVL' (\n -> (n+1) `mod` s) . rotateR) t
                           isOK t_ = (isBalanced t_) && (asListL t_ == tlist)
                           tlist   = asListL t

-- | Test rotateByL function
testRotateByL :: IO ()
testRotateByL = do title "rotateByL"
                   exhaustiveTest test (take 6 allAVL)
                where test _ s t = all isOK $ map rotateIt [-1..s]
                       where rotateIt n = mapAVL' (\n_ -> (n_-n) `mod` s) $ rotateByL t n
                             isOK t_ = (isBalanced t_) && (asListL t_ == tlist)
                             tlist   = asListL t

-- | Test rotateByR function
testRotateByR :: IO ()
testRotateByR = do title "rotateByR"
                   exhaustiveTest test (take 6 allAVL)
                where test _ s t = all isOK $ map rotateIt [-1..s]
                       where rotateIt n = mapAVL' (\n_ -> (n_+n) `mod` s) $ rotateByR t n
                             isOK t_ = (isBalanced t_) && (asListL t_ == tlist)
                             tlist   = asListL t

-- | Test genForkL function
testGenForkL :: IO ()
testGenForkL = do title "genForkL"
                  exhaustiveTest test (take 6 allAVL)
               where test _ s t = all testForkL [-1..s-1]
                      where tlist = asListL t
                            testForkL n = let (l,r) = genForkL (compare n) t
                                          in (isBalanced l) && (isBalanced r) &&
                                             (size l == n+1) && (size r == s-(n+1)) &&
                                             (l `toListL` asListL r == tlist)

-- | Test genForkR function
testGenForkR :: IO ()
testGenForkR = do title "genForkR"
                  exhaustiveTest test (take 6 allAVL)
               where test _ s t = all testForkR [0..s]
                      where tlist = asListL t
                            testForkR n = let (l,r) = genForkR (compare n) t
                                          in (isBalanced l) && (isBalanced r) &&
                                             (size l == n) && (size r == s-n) &&
                                             (l `toListL` asListL r == tlist)


-- | Test genFork function
testGenFork :: IO ()
testGenFork = do title "genFork"
                 exhaustiveTest test (take 6 allAVL)
              where test _ s t = all testFork0 [0..s-1] && testFork1 (-1) && testFork2 s
                      where tlist = asListL t
                            testFork0 n = let (l,mbn,r) = genFork (fstCC n) t
                                          in case mbn of
                                             Just n_ -> (n_==n) && (isBalanced l) && (isBalanced r) &&
                                                        (size l == n) && (size r == s-(n+1)) &&
                                                        (l `toListL` (n : asListL r) == tlist)
                                             _       -> False
                            testFork1 n = let (l,mbn,r) = genFork (fstCC n) t
                                          in case mbn of
                                             Nothing -> (isEmpty l) && (isBalanced r) && (asListL r == tlist)
                                             _       -> False
                            testFork2 n = let (l,mbn,r) = genFork (fstCC n) t
                                          in case mbn of
                                             Nothing -> (isEmpty r) && (isBalanced l) && (asListL l == tlist)
                                             _       -> False

-- | Test genTakeLE function
testGenTakeLE :: IO ()
testGenTakeLE = do title "genTakeLE"
                   exhaustiveTest test (take 6 allAVL)
                where test _ s t = all testTakeLE [-1..s-1]
                       where testTakeLE n = let l = genTakeLE (compare n) t
                                            in (isBalanced l) && (asListL l == [0..n])

-- | Test genTakeLT function
testGenTakeLT :: IO ()
testGenTakeLT = do title "genTakeLT"
                   exhaustiveTest test (take 6 allAVL)
                where test _ s t = all testTakeLT [0..s]
                       where testTakeLT n = let l = genTakeLT (compare n) t
                                            in (isBalanced l) && (asListL l == [0..n-1])

-- | Test genTakeGT function
testGenTakeGT :: IO ()
testGenTakeGT = do title "genTakeGT"
                   exhaustiveTest test (take 6 allAVL)
                where test _ s t = all testTakeGT [-1..s-1]
                       where testTakeGT n = let r = genTakeGT (compare n) t
                                            in (isBalanced r) && (asListL r == [n+1..s-1])

-- | Test genTakeGE function
testGenTakeGE :: IO ()
testGenTakeGE = do title "genTakeGE"
                   exhaustiveTest test (take 6 allAVL)
                where test _ s t = all testTakeGE [0..s]
                       where testTakeGE n = let r = genTakeGE (compare n) t
                                            in (isBalanced r) && (asListL r == [n..s-1])

-- | Test the genUnion function
testGenUnion :: IO ()
testGenUnion = let trees = take num $ concatMap (\(_,ts) -> ts) allAVL
                   num   = 1000
               in do title "genUnion"
                     putStrLn $ "Testing " ++ show (num*num) ++ " tree pairs.."
                     if and [test l ls r rs | (l,ls) <- trees, (r,rs) <- trees] then passed else failed
                  where test  l ls r rs = all (\f -> f l ls r rs) [test1,test2,test3]
                        test1 l ls r rs = let u = unionFst l r
                                          in isBalanced u && (asListL u == [0 .. max ls rs - 1])
                        test2 l ls r rs = and  [test2_ n $ mapAVL' (n+) r | n <- [(-rs)..ls]]
                         where test2_ n r_ = let u = unionFst l r_
                                             in isBalanced u && (asListL u == [min n 0 .. max ls (rs+n) - 1])
                        test3 l ls r rs = let l_ = mapAVL' (\n -> n+n  ) l -- even
                                              r_ = mapAVL' (\n -> n+n+1) r -- odd
                                              u  = unionFst l_ r_
                                          in isSortedOK compare u && (size u == ls+rs)
                        unionFst = genUnion fstCC


-- | Test the genSymDifference function
testGenSymDifference :: IO ()
testGenSymDifference =
 let trees = take num $ concatMap (\(_,ts) -> ts) allAVL
     num   = 1000
 in do title "genSymDifference"
       putStrLn $ "Testing " ++ show (num*num) ++ " tree pairs.."
       if and [test l ls r rs | (l,ls) <- trees, (r,rs) <- trees] then passed else failed
    where test  l ls r rs = all (\f -> f l ls r rs) [test1,test2,test3]
          test1 l ls r rs = let u = symDiff l r
                            in isBalanced u && (asListL u == [min ls rs .. max ls rs - 1])
          test2 l ls r rs = and  [test2_ n $ mapAVL' (n+) r | n <- [(-rs)..ls]]
           where test2_ n r_ = let u = symDiff l r_
                               in isBalanced u && (asListL u == [min n  0      .. max n  0      - 1] ++
                                                                [min ls (rs+n) .. max ls (rs+n) - 1])
          test3 l ls r rs = let l_ = mapAVL' (\n -> n+n  ) l -- even
                                r_ = mapAVL' (\n -> n+n+1) r -- odd
                                u  = symDiff l_ r_
                            in isSortedOK compare u && (size u == ls+rs)
          symDiff = genSymDifference compare

-- | Test the genUnionMaybe function
testGenUnionMaybe :: IO ()
testGenUnionMaybe = let trees = take num $ concatMap (\(_,ts) -> ts) allAVL
                        num   = 1000
                    in do title "genUnionMaybe"
                          putStrLn $ "Testing " ++ show (num*num) ++ " tree pairs.."
                          if and [test l ls r rs | (l,ls) <- trees, (r,rs) <- trees] then passed else failed
                       where test  l ls r rs = all (\f -> f l ls r rs) [test1,test2,test3]
                             test1 l ls r rs = let u = onion l r
                                                   mn = min ls rs
                                                   mx = max ls rs
                                               in isBalanced u && (asListL u == [0,2 .. mn - 1] ++ [mn .. mx-1])
                             test2 l ls r rs = and  [test2_ n $ mapAVL' (n+) r | n <- [(-rs)..ls]]
                              where test2_ n r_ = let u = onion l r_
                                                      n0 = min n 0
                                                      n1 = max n 0
                                                      n2 = min ls (rs+n)
                                                      n3 = max ls (rs+n)
                                                  in isBalanced u && (asListL u == [n0 .. n1-1]
                                                                                ++ filter even [n1 .. n2-1]
                                                                                ++ [n2..n3-1]
                                                                     )
                             test3 l ls r rs = let l_ = mapAVL' (\n -> n+n  ) l -- even
                                                   r_ = mapAVL' (\n -> n+n+1) r -- odd
                                                   u  = onion l_ r_
                                               in isSortedOK compare u && (size u == ls+rs)
                             onion = genUnionMaybe (withCC' com)
                             com a _ = if even a then Just a else Nothing

-- | Test the genIntersection function
testGenIntersection :: IO ()
testGenIntersection = let trees = take num $ concatMap (\(_,ts) -> ts) allAVL
                          num   = 1000
                      in do title "genIntersection"
                            putStrLn $ "Testing " ++ show (num*num) ++ " tree pairs.."
                            if and [test l ls r rs | (l,ls) <- trees, (r,rs) <- trees] then passed else failed
                         where test  l ls r rs = all (\f -> f l ls r rs) [test1,test2,test3]
                               test1 l ls r rs = let u = genIntersection fstCC l r
                                                 in isBalanced u && (asListL u == [0 .. min ls rs - 1])
                               test2 l ls r rs = and  [test2_ n $ mapAVL' (n+) r | n <- [(-rs)..ls]]
                                where test2_ n r_ = let u = genIntersection fstCC l r_
                                                    in isBalanced u && (asListL u == [max n 0 .. min ls (rs+n) - 1])
                               test3 l _  r _  = let l_ = mapAVL' (\n -> n+n  ) l -- even
                                                     r_ = mapAVL' (\n -> n+n+1) r -- odd
                                                     u  = genIntersection fstCC l_ r_
                                                 in isEmpty u

-- | Test the genIntersectionMaybe function
testGenIntersectionMaybe :: IO ()
testGenIntersectionMaybe = let trees = take num $ concatMap (\(_,ts) -> ts) allAVL
                               num   = 1000
                           in do title "genIntersectionMaybe"
                                 putStrLn $ "Testing " ++ show (num*num) ++ " tree pairs.."
                                 if and [test l ls r rs | (l,ls) <- trees, (r,rs) <- trees] then passed else failed
                              where test  l ls r rs = all (\f -> f l ls r rs) [test1,test2,test3]
                                    test1 l ls r rs = let u = insect l r
                                                          mn = min ls rs
                                                      in isBalanced u && (asListL u == [0,2 .. mn - 1])
                                    test2 l ls r rs = and  [test2_ n $ mapAVL' (n+) r | n <- [(-rs)..ls]]
                                     where test2_ n r_ = let u = insect l r_
                                                             n1 = max n 0
                                                             n2 = min ls (rs+n)
                                                         in isBalanced u && (asListL u == filter even [n1 .. n2-1])
                                    test3 l _  r _  = let l_ = mapAVL' (\n -> n+n  ) l -- even
                                                          r_ = mapAVL' (\n -> n+n+1) r -- odd
                                                          u  = insect l_ r_
                                                      in isEmpty u
                                    insect = genIntersectionMaybe (withCC' com)
                                    com a _ = if even a then Just a else Nothing

-- | Test the genIntersectionAsListL function
testGenIntersectionAsListL :: IO ()
testGenIntersectionAsListL =
 let trees = take num $ concatMap (\(_,ts) -> ts) allAVL
     num   = 1000
 in do title "genIntersectionAsListL"
       putStrLn $ "Testing " ++ show (num*num) ++ " tree pairs.."
       if and [test l ls r rs | (l,ls) <- trees, (r,rs) <- trees] then passed else failed
    where test  l ls r rs = all (\f -> f l ls r rs) [test1,test2,test3]
          test1 l ls r rs = let u = genIntersectionAsListL fstCC l r
                            in u == [0 .. min ls rs - 1]
          test2 l ls r rs = and  [test2_ n $ mapAVL' (n+) r | n <- [(-rs)..ls]]
           where test2_ n r_ = let u = genIntersectionAsListL fstCC l r_
                               in u == [max n 0 .. min ls (rs+n) - 1]
          test3 l _  r _  = let l_ = mapAVL' (\n -> n+n  ) l -- even
                                r_ = mapAVL' (\n -> n+n+1) r -- odd
                                u  = genIntersectionAsListL fstCC l_ r_
                            in null u

-- | Test the genIntersectionMaybeAsListL function
testGenIntersectionMaybeAsListL :: IO ()
testGenIntersectionMaybeAsListL =
 let trees = take num $ concatMap (\(_,ts) -> ts) allAVL
     num   = 1000
 in do title "genIntersectionMaybeAsListL"
       putStrLn $ "Testing " ++ show (num*num) ++ " tree pairs.."
       if and [test l ls r rs | (l,ls) <- trees, (r,rs) <- trees] then passed else failed
    where test  l ls r rs = all (\f -> f l ls r rs) [test1,test2,test3]
          test1 l ls r rs = let u = insect l r
                                mn = min ls rs
                            in u == [0,2 .. mn - 1]
          test2 l ls r rs = and  [test2_ n $ mapAVL' (n+) r | n <- [(-rs)..ls]]
           where test2_ n r_ = let u = insect l r_
                                   n1 = max n 0
                                   n2 = min ls (rs+n)
                               in u == filter even [n1 .. n2-1]
          test3 l _  r _  = let l_ = mapAVL' (\n -> n+n  ) l -- even
                                r_ = mapAVL' (\n -> n+n+1) r -- odd
                                u  = insect l_ r_
                            in null u
          insect = genIntersectionMaybeAsListL (withCC' com)
          com a _ = if even a then Just a else Nothing

-- | Test the genDifference function
testGenDifference :: IO ()
testGenDifference = let trees = take num $ concatMap (\(_,ts) -> ts) allAVL
                        num   = 1000
                    in do title "genDifference"
                          putStrLn $ "Testing " ++ show (num*num) ++ " tree pairs.."
                          if and [test l ls r rs | (l,ls) <- trees, (r,rs) <- trees] then passed else failed
                       where test  l ls r rs = all (\f -> f l ls r rs) [test1,test2,test3]
                             test1 l ls r rs = let u = difference l r
                                               in isBalanced u && (asListL u == [rs .. ls - 1])
                             test2 l ls r rs = and  [test2_ n $ mapAVL' (n+) r | n <- [(-rs)..ls]]
                              where test2_ n r_ = let u = difference l r_
                                                  in isBalanced u && (asListL u == [0 .. n-1] ++ [rs+n .. ls-1])
                             test3 l ls r rs = let l_ = mapAVL' (\n -> n+n  ) l -- even
                                                   r_ = mapAVL' (\n -> n+n+1) r -- odd
                                                   u  = difference l r_
                                                   u_ = difference l_ r_
                                                   mn = min (ls-1) (2*rs-1)
                                               in isBalanced u  &&
                                                  (asListL u == filter even [0..mn] ++ [mn+1..ls-1]) &&
                                                  isBalanced u_ && (u_ == l_)
                             difference = genDifference compare

-- | Test the genDifferenceMaybe function
testGenDifferenceMaybe :: IO ()
testGenDifferenceMaybe =
 let trees = take num $ concatMap (\(_,ts) -> ts) allAVL
     num   = 1000
 in do title "genDifferenceMaybe"
       putStrLn $ "Testing " ++ show (num*num) ++ " tree pairs.."
       if and [test l ls r rs | (l,ls) <- trees, (r,rs) <- trees] then passed else failed
    where c m n = case compare m n of
                  LT -> Lt
                  EQ -> if even m then (Eq Nothing) else (Eq (Just m))
                  GT -> Gt
          test  l ls r rs = all (\f -> f l ls r rs) [test1,test2,test3]
          test1 l ls r rs = let mn = min (ls-1) (rs-1)
                                u = genDifferenceMaybe c l r
                            in isBalanced u && (asListL u == filter odd [0..mn] ++ [mn+1..ls-1])
          test2 l ls r rs = and  [test2_ n $ mapAVL' (n+) r | n <- [(-rs)..ls]]
           where test2_ n r_ = let u = genDifferenceMaybe c l r_
                                   n0 = max 0 n
                                   n1 = min (ls-1) (rs+n-1)
                               in isBalanced u &&
                                  (asListL u == [0..n0-1] ++ filter odd [n0..n1] ++ [n1+1..ls-1])
          test3 l ls r rs = let l_ = mapAVL' (\n -> n+n+1) l -- odd
                                r_ = mapAVL' (\n -> n+n  ) r -- even
                                u  = genDifferenceMaybe c l r_
                                u_ = genDifferenceMaybe c l_ r_
                                mn = min (ls-1) (2*rs-2)
                                mx = max (mn+1) 0
                                listfil = filter odd [0..mn]
                                listrem = [mx..ls-1]
                            in isBalanced u && isBalanced u_ && (u_ == l_) &&
                               (asListL u == listfil ++ listrem)

-- | Test the genIsSubsetOf function
testGenIsSubsetOf :: IO ()
testGenIsSubsetOf = let trees = take num $ concatMap (\(_,ts) -> ts) allAVL
                        num   = 1000
                    in do title "genIsSubsetOf"
                          putStrLn $ "Testing " ++ show (num*num) ++ " tree pairs.."
                          if and [test l ls r rs | (l,ls) <- trees, (r,rs) <- trees] then passed else failed
                       where test  l ls r rs = all (\f -> f l ls r rs) [test1,test2]
                             test1 l ls r rs = (l `isSubsetOf` r == (ls<=rs)) &&
                                               (r `isSubsetOf` l == (rs<=ls))
                             test2 l ls r rs = and  [test2_ n $ mapAVL' (n+) r | n <- [(-rs)..ls]]
                              where test2_ n r_ = (l  `isSubsetOf` r_ == ((n<=0) && (rs+n>=ls))) &&
                                                  (r_ `isSubsetOf` l  == ((n>=0) && (rs+n<=ls)))
                             isSubsetOf = genIsSubsetOf compare

-- | Test the genIsSubsetOfBy function
testGenIsSubsetOfBy :: IO ()
testGenIsSubsetOfBy = let trees = take num $ concatMap (\(_,ts) -> ts) allAVL
                          num   = 1000
                      in do title "genIsSubsetOfBy"
                            putStrLn $ "Testing " ++ show (num*num) ++ " tree pairs.."
                            if and [test l ls r rs | (l,ls) <- trees, (r,rs) <- trees] then passed else failed
                            -- test1 & test2 chack same behaviour as genIsSubsetOf
                            -- test3 checks behviour for comarison functions that may return (Eq False)
                         where test  l ls r rs = all (\f -> f l ls r rs) [test1,test2,test3]
                               test1 l ls r rs = (l `isSubsetOf` r == (ls<=rs)) &&
                                                 (r `isSubsetOf` l == (rs<=ls))
                               test2 l ls r rs = and  [test2_ n $ mapAVL' (n+) r | n <- [(-rs)..ls]]
                                where test2_ n r_ = (l  `isSubsetOf` r_ == ((n<=0) && (rs+n>=ls))) &&
                                                    (r_ `isSubsetOf` l  == ((n>=0) && (rs+n<=ls)))
                               isSubsetOf    = genIsSubsetOfBy (withCC (\_ _ -> True  ))
                               test3 l ls r rs = and [test3_ n | n <- [0..max ls rs]]
                                where test3_ n = (l `isSubsetOf'` r == ((ls<=rs) && (n>=ls))) &&
                                                 (r `isSubsetOf'` l == ((rs<=ls) && (n>=rs)))
                                       where isSubsetOf' = genIsSubsetOfBy (withCC (\m _ -> m /= n))


-- | Test compareHeight function
testCompareHeight :: IO ()
testCompareHeight = let trees = take num $ concatMap (\(h,ts) -> [(t,h)|(t,_)<-ts]) allAVL
                        num   = 10000
                    in do title "compareHeight"
                          putStrLn $ "Testing " ++ show (num*num) ++ " tree pairs.."
                          if and [test l lh r rh | (l,lh) <- trees, (r,rh) <- trees] then passed else failed
                       where test l lh r rh = compareHeight l r == compare lh rh

-- | Test Zipper open\/close
testGenOpenClose :: IO ()
testGenOpenClose = do title "Zipper open/close"
                      exhaustiveTest test (take 5 allNonEmptyAVL)
                   where test _ s t = all test_ [0..s-1]
                          where test_ n = let z  = genAssertOpen (compare n) t
                                              t_ = close z
                                          in (getCurrent z == n) && (isBalanced t_) && (asListL t_ == [0..s-1])
-- | Test Zipper delClose
testDelClose :: IO ()
testDelClose = do title "Zipper delClose"
                  exhaustiveTest test (take 5 allNonEmptyAVL)
                where test _ s t = all test_ [0..s-1]
                       where test_ n = let t_ = delClose $ genAssertOpen (compare n) t
                                       in (isBalanced t_) -- && (insert n (asListL t_) == [0..s-1])

-- | Test Zipper assertOpenL\/close
testOpenLClose :: IO ()
testOpenLClose = do title "Zipper assertOpenL/close"
                    exhaustiveTest test (take 5 allNonEmptyAVL)
                 where test _ s t = let z  = assertOpenL t
                                        t_ = close z
                                    in (getCurrent z == 0) && (isBalanced t_) && (asListL t_ == [0..s-1])

-- | Test Zipper assertOpenR\/close
testOpenRClose :: IO ()
testOpenRClose = do title "Zipper assertOpenR/close"
                    exhaustiveTest test (take 5 allNonEmptyAVL)
                 where test _ s t = let z  = assertOpenR t
                                        t_ = close z
                                    in (getCurrent z == s-1) && (isBalanced t_) && (asListL t_ == [0..s-1])

-- | Test Zipper assertMoveL\/isRightmost
testMoveL :: IO ()
testMoveL = do title "Zipper assertMoveL/isRightmost"
               exhaustiveTest test (take 5 allNonEmptyAVL)
            where test _ s t = let zavls@(z:zs) = take s $ iterate assertMoveL (assertOpenR t)
                               in (map getCurrent zavls == reverse [0..s-1]) && (all test_ zavls) &&
                                  (isRightmost z) && (not $ any isRightmost zs)
                   where test_ zavl = let t_ = close zavl
                                      in (isBalanced t_) && (asListL t_ == [0..s-1])

-- | Test Zipper assertMoveR\/isLeftmost
testMoveR :: IO ()
testMoveR = do title "Zipper assertMoveR/isLeftmost"
               exhaustiveTest test (take 5 allNonEmptyAVL)
            where test _ s t = let zavls@(z:zs) = take s $ iterate assertMoveR (assertOpenL t)
                               in (map getCurrent zavls == [0..s-1]) && (all test_ zavls) &&
                                  (isLeftmost z) && (not $ any isLeftmost zs)
                   where test_ zavl = let t_ = close zavl
                                      in (isBalanced t_) && (asListL t_ == [0..s-1])

-- | Test Zipper insertL
testInsertL :: IO ()
testInsertL = do title "Zipper insertL"
                 exhaustiveTest test (take 5 allNonEmptyAVL)
              where test _ s t = all test_ [0..s-1]
                     where test_ n = let z  = insertL s $ genAssertOpen (compare n) t
                                         t_ = close z
                                     in (getCurrent z == n) && (isBalanced t_) &&
                                        (asListL t_ == [0..n-1] ++ s:[n..s-1])
-- | Test Zipper insertMoveL
testInsertMoveL :: IO ()
testInsertMoveL = do title "Zipper insertMoveL"
                     exhaustiveTest test (take 5 allNonEmptyAVL)
                  where test _ s t = all test_ [0..s-1]
                         where test_ n = let z  = insertMoveL s $ genAssertOpen (compare n) t
                                             t_ = close z
                                         in (getCurrent z == s) && (isBalanced t_) &&
                                            (asListL t_ == [0..n-1] ++ s:[n..s-1])

-- | Test Zipper insertR
testInsertR :: IO ()
testInsertR = do title "Zipper insertR"
                 exhaustiveTest test (take 5 allNonEmptyAVL)
              where test _ s t = all test_ [0..s-1]
                     where test_ n = let z  = insertR (genAssertOpen (compare n) t) s
                                         t_ = close z
                                     in (getCurrent z == n) && (isBalanced t_) &&
                                        (asListL t_ == [0..n] ++ s:[(n+1)..s-1])

-- | Test Zipper insertMoveR
testInsertMoveR :: IO ()
testInsertMoveR = do title "Zipper insertMoveR"
                     exhaustiveTest test (take 5 allNonEmptyAVL)
                  where test _ s t = all test_ [0..s-1]
                         where test_ n = let z  = insertMoveR (genAssertOpen (compare n) t) s
                                             t_ = close z
                                         in (getCurrent z == s) && (isBalanced t_) &&
                                            (asListL t_ == [0..n] ++ s:[(n+1)..s-1])

-- | Test Zipper insertTreeL
testInsertTreeL :: IO ()
testInsertTreeL = do title "Zipper insertTreeL"
                     exhaustiveTest test (take 5 allNonEmptyAVL)
                  where test _ s t = all test_ [0..s-1]
                         where test_ n = let z  = insertTreeL t $ genAssertOpen (compare n) t
                                             t_ = close z
                                         in (getCurrent z == n) && (isBalanced t_) &&
                                            (asListL t_ == [0..n-1] ++ [0..s-1] ++ [n..s-1])

-- | Test Zipper insertTreeR
testInsertTreeR :: IO ()
testInsertTreeR = do title "Zipper insertTreeR"
                     exhaustiveTest test (take 5 allNonEmptyAVL)
                  where test _ s t = all test_ [0..s-1]
                         where test_ n = let z  = insertTreeR (genAssertOpen (compare n) t) t
                                             t_ = close z
                                         in (getCurrent z == n) && (isBalanced t_) &&
                                            (asListL t_ == [0..n] ++ [0..s-1] ++ [n+1..s-1])
-- | Test Zipper assertDelMoveL
testDelMoveL :: IO ()
testDelMoveL = do title "Zipper assertDelMoveL"
                  exhaustiveTest test (take 5 allNonEmptyAVL)
               where test _ s t = let zavls = take s $ iterate assertDelMoveL $ insertR (assertOpenR t) s
                                  in (map getCurrent zavls == reverse [0..s-1]) &&
                                     (and $ zipWith test_ zavls $ reverse [0..s-1])
                      where test_ zavl s_ = let t_ = close zavl
                                            in (isBalanced t_) && (asListL t_ == [0..s_] ++ [s])

-- | Test Zipper assertDelMoveR
testDelMoveR :: IO ()
testDelMoveR = do title "Zipper assertDelMoveR"
                  exhaustiveTest test (take 5 allNonEmptyAVL)
               where test _ s t = let zavls = take s $ iterate assertDelMoveR $ insertL s $ assertOpenL t
                                  in (map getCurrent zavls == [0..s-1]) &&
                                     (and $ zipWith test_ zavls [0..s-1])
                      where test_ zavl s_ = let t_ = close zavl
                                            in (isBalanced t_) && (asListL t_ == s:[s_..s-1])

-- | Test Zipper delAllL
testDelAllL :: IO ()
testDelAllL = do title "Zipper delAllL"
                 exhaustiveTest test (take 5 allNonEmptyAVL)
              where test _ s t = all test_ [0..s-1]
                     where test_ n = let z   = delAllL $ genAssertOpen (compare n) t
                                         t_  = close z
                                         t__ = close $ insertTreeL t z
                                     in (isBalanced t_ ) && (asListL t_  == [n..s-1]) &&
                                        (isBalanced t__) && (asListL t__ == [0..s-1] ++ [n..s-1])

-- | Test Zipper delAllR
testDelAllR :: IO ()
testDelAllR = do title "Zipper delAllR"
                 exhaustiveTest test (take 5 allNonEmptyAVL)
              where test _ s t = all test_ [0..s-1]
                     where test_ n = let z   = delAllR $ genAssertOpen (compare n) t
                                         t_  = close z
                                         t__ = close $ insertTreeR z t
                                     in (isBalanced t_ ) && (asListL t_  == [0..n]) &&
                                        (isBalanced t__) && (asListL t__ == [0..n] ++ [0..s-1])

-- | Test Zipper delAllCloseL
testDelAllCloseL :: IO ()
testDelAllCloseL = do title "Zipper delAllCloseL"
                      exhaustiveTest test (take 5 allNonEmptyAVL)
                   where test _ s t = all test_ [0..s-1]
                          where test_ n = let t_   = delAllCloseL $ genAssertOpen (compare n) t
                                          in (isBalanced t_ ) && (asListL t_  == [n..s-1])

-- | Test Zipper delAllIncCloseL
testDelAllIncCloseL :: IO ()
testDelAllIncCloseL = do title "Zipper delAllIncCloseL"
                         exhaustiveTest test (take 5 allNonEmptyAVL)
                      where test _ s t = all test_ [0..s-1]
                             where test_ n = let t_   = delAllIncCloseL $ genAssertOpen (compare n) t
                                             in (isBalanced t_ ) && (asListL t_  == [n+1..s-1])

-- | Test Zipper delAllCloseR
testDelAllCloseR :: IO ()
testDelAllCloseR = do title "Zipper delAllCloseR"
                      exhaustiveTest test (take 5 allNonEmptyAVL)
                   where test _ s t = all test_ [0..s-1]
                          where test_ n = let t_   = delAllCloseR $ genAssertOpen (compare n) t
                                          in (isBalanced t_ ) && (asListL t_  == [0..n])

-- | Test Zipper delAllIncCloseR
testDelAllIncCloseR :: IO ()
testDelAllIncCloseR = do title "Zipper delAllIncCloseR"
                         exhaustiveTest test (take 5 allNonEmptyAVL)
                      where test _ s t = all test_ [0..s-1]
                             where test_ n = let t_   = delAllIncCloseR $ genAssertOpen (compare n) t
                                             in (isBalanced t_ ) && (asListL t_  == [0..n-1])

-- | Test Zipper sizeL\/sizeR\/sizeZAVL
testZipSize :: IO ()
testZipSize = do title "Zipper sizeL/sizeR/sizeZAVL"
                 exhaustiveTest test (take 5 allNonEmptyAVL)
              where test _ s t = all test_ [0..s-1]
                     where test_ n = let z = genAssertOpen (compare n) t
                                     in (sizeL z == n) && (sizeR z == (s-1)-n) && (sizeZAVL z == s)

-- | Test Zipper genTryOpenGE
testGenTryOpenGE :: IO ()
testGenTryOpenGE = do title "Zipper genTryOpenGE"
                      exhaustiveTest test (take 5 allNonEmptyAVL)
                   where test _ s t = let t_ = mapAVL' (2*) t
                                      in all (testE t_) [0,2..2*s-2] && all (testO t_) [(-1),1..2*s-3]
                          where testE t_ n = let Just z = tryOpenGE n t_
                                                 t__    = close z
                                          in (getCurrent z == n) && (isBalanced t__) && (asListL t__ == [0,2..2*s-2])
                                testO t_ n = let Just z = tryOpenGE n t_
                                                 t__    = close z
                                          in (getCurrent z == n+1) && (isBalanced t__) && (asListL t__ == [0,2..2*s-2])
                                tryOpenGE a = genTryOpenGE (compare a)

-- | Test Zipper genTryOpenLE
testGenTryOpenLE :: IO ()
testGenTryOpenLE = do title "Zipper genTryOpenLE"
                      exhaustiveTest test (take 5 allNonEmptyAVL)
                   where test _ s t = let t_ = mapAVL' (2*) t
                                      in all (testE t_) [0,2..2*s-2] && all (testO t_) [1,3..2*s-1]
                          where testE t_ n = let Just z = tryOpenLE n t_
                                                 t__    = close z
                                          in (getCurrent z == n) && (isBalanced t__) && (asListL t__ == [0,2..2*s-2])
                                testO t_ n = let Just z = tryOpenLE n t_
                                                 t__    = close z
                                          in (getCurrent z == n-1) && (isBalanced t__) && (asListL t__ == [0,2..2*s-2])
                                tryOpenLE a = genTryOpenLE (compare a)

-- | Test Zipper genOpenEither (also tests fill and fillClose)
testGenOpenEither :: IO ()
testGenOpenEither = do title "Zipper genOpenEither"
                       exhaustiveTest test (take 6 allAVL)
                    where test _ s t = let t_ = mapAVL' (2*) t
                                       in all (testE t_) [0,2..2*s-2] && all (testO t_) [-1,1..2*s-1]
                           where testE t_ n = let Right z = openEither n t_
                                                  t__     = close z
                                              in (getCurrent z == n) && (isBalanced t__) && (asListL t__ == [0,2..2*s-2])
                                 testO t_ n = let Left p = openEither n t_
                                                  t__    = close (fill n p)
                                                  t___   = fillClose n p
                                              in (isBalanced t__) && (isBalanced t___) && (t__ == t___) &&
                                                 (asListL t__ == ([0,2..n-1] ++ n : [n+1,n+3..2*s-2]))
                                 openEither a = genOpenEither (compare a)



-- | Test anyBAVLtoEither
testBAVLtoZipper :: IO ()
testBAVLtoZipper = do title "BAVLtoZipper"
                      exhaustiveTest test (take 6 allAVL)
                   where test _ s t = let t_ = mapAVL' (2*) t
                                      in all (testE t_) [0,2..2*s-2] && all (testO t_) [-1,1..2*s-1]
                          where testE t_ n = let bavl = openBAVL n t_
                                                 Right z = anyBAVLtoEither bavl
                                                 t__ = close z
                                             in (getCurrent z == n) && (isBalanced t__) && (asListL t__ == [0,2..2*s-2])
                                testO t_ n = let bavl = openBAVL n t_
                                                 Left p = anyBAVLtoEither bavl
                                                 t__   = fillClose n p
                                             in (isBalanced t__) && (asListL t__ == ([0,2..n-1] ++ n : [n+1,n+3..2*s-2]))
                                openBAVL e = genOpenBAVL (compare e)


-- | Test Show,Read,Eq instances
testShowReadEq :: IO ()
testShowReadEq = do title "ShowReadEq"
                    exhaustiveTest test (take 5 allAVL)  -- No need to get carried away with this one
                 where test _ _ t = t == (read $ show t)

-- | Test readPath
testReadPath :: IO ()
testReadPath = do title "ReadPath"
                  if all test [0..100] then passed else failed
               where test n = let ASINT(n_)=n in (n == readPath n_ pathTree)

title :: String -> IO ()
title str = let titl = "* Test " ++ str ++ " *"
                mark = replicate (length titl) '*'
            in  putStrLn "" >> putStrLn mark >> putStrLn titl >> putStrLn mark

passed :: IO ()
passed = putStrLn "Passed"

failed :: IO ()
failed = do putStrLn "!! FAILED !!"
            exitFailure

