{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Tree.AVL.List
-- Copyright   :  (c) Adrian Hey 2004,2005
-- License     :  BSD3
--
-- Maintainer  :  http://homepages.nildram.co.uk/~ahey/em.png
-- Stability   :  stable
-- Portability :  portable
-----------------------------------------------------------------------------
module Data.Tree.AVL.List
(-- * List related utilities for AVL trees

 -- ** Converting AVL trees to Lists (fixed element order).
 -- | These functions are lazy and allow normal lazy list processing
 -- style to be used (without necessarily converting the entire tree
 -- to a list in one gulp).
 asListL,toListL,asListR,toListR,

 -- ** Converting Lists to AVL trees (fixed element order)
 asTreeLenL,asTreeL,
 asTreeLenR,asTreeR,

 -- ** Converting unsorted Lists to sorted AVL trees
 genAsTree,

 -- ** \"Pushing\" unsorted Lists in sorted AVL trees
 genPushList,

 -- * Some analogues of common List functions
 reverseAVL,mapAVL,mapAVL',
 mapAccumLAVL  ,mapAccumRAVL  ,
 mapAccumLAVL' ,mapAccumRAVL' ,
#ifdef __GLASGOW_HASKELL__
 mapAccumLAVL'',mapAccumRAVL'',
#endif
#if __GLASGOW_HASKELL__ > 604
 traverseAVL,
#endif
 replicateAVL,
 filterAVL,mapMaybeAVL,
 filterViaList,mapMaybeViaList,
 partitionAVL,

 -- ** Folds
 -- | Note that unlike folds over lists ('foldr' and 'foldl'), there is no
 -- significant difference between left and right folds in AVL trees, other
 -- than which side of the tree each starts with.
 -- Therefore this library provides strict and lazy versions of both.
 foldrAVL,foldrAVL',foldr1AVL,foldr1AVL',foldr2AVL,foldr2AVL',
 foldlAVL,foldlAVL',foldl1AVL,foldl1AVL',foldl2AVL,foldl2AVL',
 foldrAVL_UINT,

 -- * \"Flattening\" AVL trees
 -- | These functions can be improve search times by reducing a tree of given size to
 -- the minimum possible height.
 flatten,
 flatReverse,flatMap,flatMap',

 -- * AVL tree based sorting of Lists
 -- | Nothing to do with AVL trees really. But using AVL trees do give an O(n.(log n)) sort
 -- algorithm for free, so here it is. These functions all consume the entire
 -- input list to construct a sorted AVL tree and then read the elements out as a list (lazily).
 genSortAscending,genSortDescending,
) where

import Prelude -- so haddock finds the symbols there

#if __GLASGOW_HASKELL__ > 604
import Control.Applicative hiding (empty)
#endif

import Data.COrdering
import Data.Tree.AVL.Types(AVL(..),empty)
import Data.Tree.AVL.Size(size)
import Data.Tree.AVL.Push(genPush)
import Data.Tree.AVL.Internals.HJoin(spliceH,joinH)

import Data.Bits(shiftR,(.&.))
import Data.List(foldl')

#ifdef __GLASGOW_HASKELL__
import GHC.Base
#include "ghcdefs.h"
#else
#include "h98defs.h"
#endif

-- | List AVL tree contents in left to right order.
-- The resulting list in ascending order if the tree is sorted.
--
-- Complexity: O(n)
asListL  :: AVL e -> [e]
asListL avl = toListL avl []

-- | Join the AVL tree contents to an existing list in left to right order.
-- This is a ++ free function which behaves as if defined thusly..
--
-- > avl `toListL` as = (asListL avl) ++ as
--
-- Complexity: O(n)
toListL :: AVL e -> [e] -> [e]
toListL  E        es = es
toListL (N l e r) es = toListL' l e r es
toListL (Z l e r) es = toListL' l e r es
toListL (P l e r) es = toListL' l e r es
toListL' :: AVL e -> e -> AVL e -> [e] -> [e]
toListL'   l e r  es = toListL l (e:(toListL r es))

-- | List AVL tree contents in right to left order.
-- The resulting list in descending order if the tree is sorted.
--
-- Complexity: O(n)
asListR  :: AVL e -> [e]
asListR avl = toListR avl []

-- | Join the AVL tree contents to an existing list in right to left order.
-- This is a ++ free function which behaves as if defined thusly..
--
-- > avl `toListR` as = (asListR avl) ++ as
--
-- Complexity: O(n)
toListR :: AVL e -> [e] -> [e]
toListR  E        es = es
toListR (N l e r) es = toListR' l e r es
toListR (Z l e r) es = toListR' l e r es
toListR (P l e r) es = toListR' l e r es
toListR' :: AVL e -> e -> AVL e -> [e] -> [e]
toListR'   l e r  es = toListR r (e:(toListR l es))

-- | The AVL equivalent of 'foldr' on lists. This is a the lazy version (as lazy as the folding function
-- anyway). Using this version with a function that is strict in it's second argument will result in O(n)
-- stack use. See 'foldrAVL'' for a strict version.
--
-- It behaves as if defined..
--
-- > foldrAVL f a avl = foldr f a (asListL avl)
--
-- For example, the 'asListL' function could be defined..
--
-- > asListL = foldrAVL (:) []
--
-- Complexity: O(n)
foldrAVL :: (e -> a -> a) -> a -> AVL e -> a
foldrAVL f = foldU where
 foldU a  E        = a
 foldU a (N l e r) = foldV a l e r
 foldU a (Z l e r) = foldV a l e r
 foldU a (P l e r) = foldV a l e r
 foldV a    l e r  = foldU (f e (foldU a r)) l

-- | The strict version of 'foldrAVL', which is useful for functions which are strict in their second
-- argument. The advantage of this version is that it reduces the stack use from the O(n) that the lazy
-- version gives (when used with strict functions) to O(log n).
--
-- Complexity: O(n)
foldrAVL' :: (e -> a -> a) -> a -> AVL e -> a
foldrAVL' f = foldU where
 foldU a  E        = a
 foldU a (N l e r) = foldV a l e r
 foldU a (Z l e r) = foldV a l e r
 foldU a (P l e r) = foldV a l e r
 foldV a    l e r  = let a'  = foldU a r
                         a'' = f e a'
                     in a' `seq` a'' `seq` foldU a'' l

-- | The AVL equivalent of 'foldr1' on lists. This is a the lazy version (as lazy as the folding function
-- anyway). Using this version with a function that is strict in it's second argument will result in O(n)
-- stack use. See 'foldr1AVL'' for a strict version.
--
-- > foldr1AVL f avl = foldr1 f (asListL avl)
--
-- This function raises an error if the tree is empty.
--
-- Complexity: O(n)
foldr1AVL :: (e -> e -> e) -> AVL e -> e
foldr1AVL f = foldU where
 foldU  E        = error "foldr1AVL: Empty Tree"
 foldU (N l e r) = foldV l e r  -- r can't be E
 foldU (Z l e r) = foldW l e r  -- r might be E
 foldU (P l e r) = foldW l e r  -- r might be E
 -- Use this when r can't be E
 foldV l e r     = foldrAVL f (f e (foldU r)) l
 -- Use this when r might be E
 foldW l e  E           = foldrAVL f e l
 foldW l e (N rl re rr) = foldrAVL f (f e (foldV rl re rr)) l -- rr can't be E
 foldW l e (Z rl re rr) = foldX l e rl re rr                  -- rr might be E
 foldW l e (P rl re rr) = foldX l e rl re rr                  -- rr might be E
 -- Common code for foldW (Z and P cases)
 foldX l e rl re rr = foldrAVL f (f e (foldW rl re rr)) l

-- | The strict version of 'foldr1AVL', which is useful for functions which are strict in their second
-- argument. The advantage of this version is that it reduces the stack use from the O(n) that the lazy
-- version gives (when used with strict functions) to O(log n).
--
-- Complexity: O(n)
foldr1AVL' :: (e -> e -> e) -> AVL e -> e
foldr1AVL' f = foldU where
 foldU  E        = error "foldr1AVL': Empty Tree"
 foldU (N l e r) = foldV l e r  -- r can't be E
 foldU (Z l e r) = foldW l e r  -- r might be E
 foldU (P l e r) = foldW l e r  -- r might be E
 -- Use this when r can't be E
 foldV l e r     = let a  = foldU r
                       a' = f e a
                   in a `seq` a' `seq` foldrAVL' f a' l
 -- Use this when r might be E
 foldW l e  E           = foldrAVL' f e l
 foldW l e (N rl re rr) = let a  = foldV rl re rr       -- rr can't be E
                              a' = f e a
                          in a `seq` a' `seq` foldrAVL' f a' l
 foldW l e (Z rl re rr) = foldX l e rl re rr            -- rr might be E
 foldW l e (P rl re rr) = foldX l e rl re rr            -- rr might be E
 -- Common code for foldW (Z and P cases)
 foldX l e rl re rr = let a  = foldW rl re rr
                          a' = f e a
                      in a `seq` a' `seq` foldrAVL' f a' l

-- | This fold is a hybrid between 'foldrAVL' and 'foldr1AVL'. As with 'foldr1AVL', it requires
-- a non-empty tree, but instead of treating the rightmost element as an initial value, it applies
-- a function to it (second function argument) and uses the result instead. This allows
-- a more flexible type for the main folding function (same type as that used by 'foldrAVL').
-- As with 'foldrAVL' and 'foldr1AVL', this function is lazy, so it's best not to use it with functions
-- that are strict in their second argument. See 'foldr2AVL'' for a strict version.
--
-- Complexity: O(n)
foldr2AVL :: (e -> a -> a) -> (e -> a) -> AVL e -> a
foldr2AVL f g = foldU where
 foldU  E        = error "foldr2AVL: Empty Tree"
 foldU (N l e r) = foldV l e r  -- r can't be E
 foldU (Z l e r) = foldW l e r  -- r might be E
 foldU (P l e r) = foldW l e r  -- r might be E
 -- Use this when r can't be E
 foldV l e r     = foldrAVL f (f e (foldU r)) l
 -- Use this when r might be E
 foldW l e  E           = foldrAVL f (g e) l
 foldW l e (N rl re rr) = foldrAVL f (f e (foldV rl re rr)) l -- rr can't be E
 foldW l e (Z rl re rr) = foldX l e rl re rr                  -- rr might be E
 foldW l e (P rl re rr) = foldX l e rl re rr                  -- rr might be E
 -- Common code for foldW (Z and P cases)
 foldX l e rl re rr = foldrAVL f (f e (foldW rl re rr)) l

-- | The strict version of 'foldr2AVL', which is useful for functions which are strict in their second
-- argument. The advantage of this version is that it reduces the stack use from the O(n) that the lazy
-- version gives (when used with strict functions) to O(log n).
--
-- Complexity: O(n)
foldr2AVL' :: (e -> a -> a) -> (e -> a) -> AVL e -> a
foldr2AVL' f g = foldU where
 foldU  E        = error "foldr2AVL': Empty Tree"
 foldU (N l e r) = foldV l e r  -- r can't be E
 foldU (Z l e r) = foldW l e r  -- r might be E
 foldU (P l e r) = foldW l e r  -- r might be E
 -- Use this when r can't be E
 foldV l e r     = let a  = foldU r
                       a' = f e a
                   in a `seq` a' `seq` foldrAVL' f a' l
 -- Use this when r might be E
 foldW l e  E           = let a = g e in a `seq` foldrAVL' f a l
 foldW l e (N rl re rr) = let a  = foldV rl re rr              -- rr can't be E
                              a' = f e a
                          in a `seq` a' `seq` foldrAVL' f a' l
 foldW l e (Z rl re rr) = foldX l e rl re rr                   -- rr might be E
 foldW l e (P rl re rr) = foldX l e rl re rr                   -- rr might be E
 -- Common code for foldW (Z and P cases)
 foldX l e rl re rr = let a  = foldW rl re rr
                          a' = f e a
                      in a `seq` a' `seq` foldrAVL' f a' l


-- | The AVL equivalent of 'foldl' on lists. This is a the lazy version (as lazy as the folding function
-- anyway). Using this version with a function that is strict in it's first argument will result in O(n)
-- stack use. See 'foldlAVL'' for a strict version.
--
-- > foldlAVL f a avl = foldl f a (asListL avl)
--
-- For example, the 'asListR' function could be defined..
--
-- > asListR = foldlAVL (flip (:)) []
--
-- Complexity: O(n)
foldlAVL :: (a -> e -> a) -> a -> AVL e -> a
foldlAVL f = foldU where
 foldU a  E        = a
 foldU a (N l e r) = foldV a l e r
 foldU a (Z l e r) = foldV a l e r
 foldU a (P l e r) = foldV a l e r
 foldV a    l e r  = foldU (f (foldU a l) e) r

-- | The strict version of 'foldlAVL', which is useful for functions which are strict in their first
-- argument. The advantage of this version is that it reduces the stack use from the O(n) that the lazy
-- version gives (when used with strict functions) to O(log n).
--
-- Complexity: O(n)
foldlAVL' :: (a -> e -> a) -> a -> AVL e -> a
foldlAVL' f = foldU where
 foldU a  E        = a
 foldU a (N l e r) = foldV a l e r
 foldU a (Z l e r) = foldV a l e r
 foldU a (P l e r) = foldV a l e r
 foldV a    l e r  = let a'  = foldU a l
                         a'' = f a' e
                     in a' `seq` a'' `seq` foldU a'' r

-- | The AVL equivalent of 'foldl1' on lists. This is a the lazy version (as lazy as the folding function
-- anyway). Using this version with a function that is strict in it's first argument will result in O(n)
-- stack use. See 'foldl1AVL'' for a strict version.
--
-- > foldl1AVL f avl = foldl1 f (asListL avl)
--
-- This function raises an error if the tree is empty.
--
-- Complexity: O(n)
foldl1AVL :: (e -> e -> e) -> AVL e -> e
foldl1AVL f = foldU where
 foldU  E        = error "foldl1AVL: Empty Tree"
 foldU (N l e r) = foldW l e r  -- l might be E
 foldU (Z l e r) = foldW l e r  -- l might be E
 foldU (P l e r) = foldV l e r  -- l can't be E
 -- Use this when l can't be E
 foldV l e r     = foldlAVL f (f (foldU l) e) r
 -- Use this when l might be E
 foldW  E           e r = foldlAVL f e r
 foldW (N ll le lr) e r = foldX ll le lr e r                  -- ll might be E
 foldW (Z ll le lr) e r = foldX ll le lr e r                  -- ll might be E
 foldW (P ll le lr) e r = foldlAVL f (f (foldV ll le lr) e) r -- ll can't be E
 -- Common code for foldW (Z and P cases)
 foldX ll le lr e r = foldlAVL f (f (foldW ll le lr) e) r

-- | The strict version of 'foldl1AVL', which is useful for functions which are strict in their first
-- argument. The advantage of this version is that it reduces the stack use from the O(n) that the lazy
-- version gives (when used with strict functions) to O(log n).
--
-- Complexity: O(n)
foldl1AVL' :: (e -> e -> e) -> AVL e -> e
foldl1AVL' f = foldU where
 foldU  E        = error "foldl1AVL': Empty Tree"
 foldU (N l e r) = foldW l e r  -- l might be E
 foldU (Z l e r) = foldW l e r  -- l might be E
 foldU (P l e r) = foldV l e r  -- l can't be E
 -- Use this when l can't be E
 foldV l e r     = let a  = foldU l
                       a' = f a e
                   in a `seq` a' `seq` foldlAVL' f a' r
 -- Use this when l might be E
 foldW  E           e r = foldlAVL' f e r
 foldW (N ll le lr) e r = foldX ll le lr e r                  -- ll might be E
 foldW (Z ll le lr) e r = foldX ll le lr e r                  -- ll might be E
 foldW (P ll le lr) e r = let a  = foldV ll le lr             -- ll can't be E
                              a' = f a e
                          in a `seq` a' `seq` foldlAVL' f a' r
 -- Common code for foldW (Z and P cases)
 foldX ll le lr e r = let a  = foldW ll le lr
                          a' = f a e
                      in a `seq` a' `seq` foldlAVL' f a' r

-- | This fold is a hybrid between 'foldlAVL' and 'foldl1AVL'. As with 'foldl1AVL', it requires
-- a non-empty tree, but instead of treating the leftmost element as an initial value, it applies
-- a function to it (second function argument) and uses the result instead. This allows
-- a more flexible type for the main folding function (same type as that used by 'foldlAVL').
-- As with 'foldlAVL' and 'foldl1AVL', this function is lazy, so it's best not to use it with functions
-- that are strict in their first argument. See 'foldl2AVL'' for a strict version.
--
-- Complexity: O(n)
foldl2AVL :: (a -> e -> a) -> (e -> a) -> AVL e -> a
foldl2AVL f g = foldU where
 foldU  E        = error "foldl2AVL: Empty Tree"
 foldU (N l e r) = foldW l e r  -- l might be E
 foldU (Z l e r) = foldW l e r  -- l might be E
 foldU (P l e r) = foldV l e r  -- l can't be E
 -- Use this when l can't be E
 foldV l e r     = foldlAVL f (f (foldU l) e) r
 -- Use this when l might be E
 foldW  E           e r = foldlAVL f (g e) r
 foldW (N ll le lr) e r = foldX ll le lr e r                  -- ll might be E
 foldW (Z ll le lr) e r = foldX ll le lr e r                  -- ll might be E
 foldW (P ll le lr) e r = foldlAVL f (f (foldV ll le lr) e) r -- ll can't be E
 -- Common code for foldW (Z and P cases)
 foldX ll le lr e r = foldlAVL f (f (foldW ll le lr) e) r

-- | The strict version of 'foldl2AVL', which is useful for functions which are strict in their first
-- argument. The advantage of this version is that it reduces the stack use from the O(n) that the lazy
-- version gives (when used with strict functions) to O(log n).
--
-- Complexity: O(n)
foldl2AVL' :: (a -> e -> a) -> (e -> a) -> AVL e -> a
foldl2AVL' f g = foldU where
 foldU  E        = error "foldl2AVL': Empty Tree"
 foldU (N l e r) = foldW l e r  -- l might be E
 foldU (Z l e r) = foldW l e r  -- l might be E
 foldU (P l e r) = foldV l e r  -- l can't be E
 -- Use this when l can't be E
 foldV l e r     = let a  = foldU l
                       a' = f a e
                   in a `seq` a' `seq` foldlAVL' f a' r
 -- Use this when l might be E
 foldW  E           e r = let a = g e in a `seq` foldlAVL' f a r
 foldW (N ll le lr) e r = foldX ll le lr e r                  -- ll might be E
 foldW (Z ll le lr) e r = foldX ll le lr e r                  -- ll might be E
 foldW (P ll le lr) e r = let a  = foldV ll le lr             -- ll can't be E
                              a' = f a e
                          in a `seq` a' `seq` foldlAVL' f a' r
 -- Common code for foldW (Z and P cases)
 foldX ll le lr e r = let a  = foldW ll le lr
                          a' = f a e
                      in a `seq` a' `seq` foldlAVL' f a' r

-- | This is a specialised version of 'foldrAVL'' for use with an
-- /unboxed/ Int accumulator (with GHC). Defaults to boxed Int
-- for other Haskells.
--
-- Complexity: O(n)
foldrAVL_UINT :: (e -> UINT -> UINT) -> UINT -> AVL e -> UINT
#ifdef __GLASGOW_HASKELL__
foldrAVL_UINT f = foldU where
 foldU a  E        = a
 foldU a (N l e r) = foldV a l e r
 foldU a (Z l e r) = foldV a l e r
 foldU a (P l e r) = foldV a l e r
 foldV a    l e r  = foldU (f e (foldU a r)) l
#else
foldrAVL_UINT = foldrAVL' -- Strict version!
{-# INLINE foldrAVL_UINT #-}
#endif

-- | The AVL equivalent of 'Data.List.mapAccumL' on lists.
-- It behaves like a combination of 'mapAVL' and 'foldlAVL'.
-- It applies a function to each element of a tree, passing an accumulating parameter from
-- left to right, and returning a final value of this accumulator together with the new tree.
--
-- Using this version with a function that is strict in it's first argument will result in
-- O(n) stack use. See 'mapAccumLAVL'' for a strict version.
--
-- Complexity: O(n)
mapAccumLAVL :: (z -> a -> (z, b)) -> z -> AVL a -> (z, AVL b)
mapAccumLAVL f z ta = case mapAL z ta of
                      UBT2(zt,tb) -> (zt,tb)
 where mapAL z_  E          = UBT2(z_,E)
       mapAL z_ (N la a ra) = mapAL' z_ N la a ra
       mapAL z_ (Z la a ra) = mapAL' z_ Z la a ra
       mapAL z_ (P la a ra) = mapAL' z_ P la a ra
       {-# INLINE mapAL' #-}
       mapAL' z' c la a ra = case mapAL z' la of
                             UBT2(zl,lb) -> let (za,b) = f zl a
                                            in case mapAL za ra of
                                               UBT2(zr,rb) -> UBT2(zr, c lb b rb)

-- | This is a strict version of 'mapAccumLAVL', which is useful for functions which
-- are strict in their first argument. The advantage of this version is that it reduces
-- the stack use from the O(n) that the lazy version gives (when used with strict functions)
-- to O(log n).
--
-- Complexity: O(n)
mapAccumLAVL' :: (z -> a -> (z, b)) -> z -> AVL a -> (z, AVL b)
mapAccumLAVL' f z ta = case mapAL z ta of
                       UBT2(zt,tb) -> (zt,tb)
 where mapAL z_  E          = UBT2(z_,E)
       mapAL z_ (N la a ra) = mapAL' z_ N la a ra
       mapAL z_ (Z la a ra) = mapAL' z_ Z la a ra
       mapAL z_ (P la a ra) = mapAL' z_ P la a ra
       {-# INLINE mapAL' #-}
       mapAL' z' c la a ra = case mapAL z' la of
                             UBT2(zl,lb) -> case f zl a of
                                            (za,b) -> case mapAL za ra of
                                                      UBT2(zr,rb) -> UBT2(zr, c lb b rb)


-- | The AVL equivalent of 'Data.List.mapAccumR' on lists.
-- It behaves like a combination of 'mapAVL' and 'foldrAVL'.
-- It applies a function to each element of a tree, passing an accumulating parameter from
-- right to left, and returning a final value of this accumulator together with the new tree.
--
-- Using this version with a function that is strict in it's first argument will result in
-- O(n) stack use. See 'mapAccumRAVL'' for a strict version.
--
-- Complexity: O(n)
mapAccumRAVL :: (z -> a -> (z, b)) -> z -> AVL a -> (z, AVL b)
mapAccumRAVL f z ta = case mapAR z ta of
                      UBT2(zt,tb) -> (zt,tb)
 where mapAR z_  E          = UBT2(z_,E)
       mapAR z_ (N la a ra) = mapAR' z_ N la a ra
       mapAR z_ (Z la a ra) = mapAR' z_ Z la a ra
       mapAR z_ (P la a ra) = mapAR' z_ P la a ra
       {-# INLINE mapAR' #-}
       mapAR' z' c la a ra = case mapAR z' ra of
                             UBT2(zr,rb) -> let (za,b) = f zr a
                                            in case mapAR za la of
                                               UBT2(zl,lb) -> UBT2(zl, c lb b rb)

-- | This is a strict version of 'mapAccumRAVL', which is useful for functions which
-- are strict in their first argument. The advantage of this version is that it reduces
-- the stack use from the O(n) that the lazy version gives (when used with strict functions)
-- to O(log n).
--
-- Complexity: O(n)
mapAccumRAVL' :: (z -> a -> (z, b)) -> z -> AVL a -> (z, AVL b)
mapAccumRAVL' f z ta = case mapAR z ta of
                       UBT2(zt,tb) -> (zt,tb)
 where mapAR z_  E          = UBT2(z_,E)
       mapAR z_ (N la a ra) = mapAR' z_ N la a ra
       mapAR z_ (Z la a ra) = mapAR' z_ Z la a ra
       mapAR z_ (P la a ra) = mapAR' z_ P la a ra
       {-# INLINE mapAR' #-}
       mapAR' z' c la a ra = case mapAR z' ra of
                             UBT2(zr,rb) -> case f zr a of
                                            (za,b) -> case mapAR za la of
                                                      UBT2(zl,lb) -> UBT2(zl, c lb b rb)

------------------------------------------------------------------------------------------------
-- These two functions attempt to make the strict mapAccums more efficient and reduce heap
-- burn rate with ghc by using an accumulating function that returns an unboxed pair.
------------------------------------------------------------------------------------------------
#ifdef __GLASGOW_HASKELL__
-- | Glasgow Haskell only. Similar to 'mapAccumLAVL'' but uses an unboxed pair in the
-- accumulating function.
--
-- Complexity: O(n)
mapAccumLAVL''
               :: (z -> a -> UBT2(z, b)) -> z -> AVL a -> (z, AVL b)
mapAccumLAVL'' f z ta = case mapAL z ta of
                        UBT2(zt,tb) -> (zt,tb)
 where mapAL z_  E          = UBT2(z_,E)
       mapAL z_ (N la a ra) = mapAL' z_ N la a ra
       mapAL z_ (Z la a ra) = mapAL' z_ Z la a ra
       mapAL z_ (P la a ra) = mapAL' z_ P la a ra
       {-# INLINE mapAL' #-}
       mapAL' z' c la a ra = case mapAL z' la of
                             UBT2(zl,lb) -> case f zl a of
                                            UBT2(za,b) -> case mapAL za ra of
                                                          UBT2(zr,rb) -> UBT2(zr, c lb b rb)

-- | Glasgow Haskell only. Similar to 'mapAccumRAVL'' but uses an unboxed pair in the
-- accumulating function.
--
-- Complexity: O(n)
mapAccumRAVL''
               :: (z -> a -> UBT2(z, b)) -> z -> AVL a -> (z, AVL b)
mapAccumRAVL'' f z ta = case mapAR z ta of
                        UBT2(zt,tb) -> (zt,tb)
 where mapAR z_  E          = UBT2(z_,E)
       mapAR z_ (N la a ra) = mapAR' z_ N la a ra
       mapAR z_ (Z la a ra) = mapAR' z_ Z la a ra
       mapAR z_ (P la a ra) = mapAR' z_ P la a ra
       {-# INLINE mapAR' #-}
       mapAR' z' c la a ra = case mapAR z' ra of
                             UBT2(zr,rb) -> case f zr a of
                                            UBT2(za,b) -> case mapAR za la of
                                                          UBT2(zl,lb) -> UBT2(zl, c lb b rb)

#endif
------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------

-- | Convert a list of known length into an AVL tree, such that the head of the list becomes
-- the leftmost tree element. The resulting tree is flat (and also sorted if the supplied list
-- is sorted in ascending order).
--
-- If the actual length of the list is not the same as the supplied length then
-- an error will be raised.
--
-- Complexity: O(n)
asTreeLenL :: Int -> [e] -> AVL e
asTreeLenL n es = case subst (replicateAVL n ()) es of
                  UBT2(tree,es_) -> case es_ of
                                    [] -> tree
                                    _  -> error "asTreeLenL: List too long."
 where
 -- Substitute template values for real values taken from the list
 subst  E        as = UBT2(E,as)
 subst (N l _ r) as = subst' N l r as
 subst (Z l _ r) as = subst' Z l r as
 subst (P l _ r) as = subst' P l r as
 {-# INLINE subst' #-}
 subst' f l r as = case subst l as of
                   UBT2(l_,xs) -> case xs of
                                  a:as' -> case subst r as' of
                                           UBT2(r_,as__) -> let t_ = f l_ a r_
                                                            in t_ `seq` UBT2(t_,as__)
                                  []    -> error "asTreeLenL: List too short."


-- | As 'asTreeLenL', except the length of the list is calculated internally, not supplied
-- as an argument.
--
-- Complexity: O(n)
asTreeL :: [e] -> AVL e
asTreeL es = asTreeLenL (length es) es

-- | Convert a list of known length into an AVL tree, such that the head of the list becomes
-- the rightmost tree element. The resulting tree is flat (and also sorted if the supplied list
-- is sorted in descending order).
--
-- If the actual length of the list is not the same as the supplied length then
-- an error will be raised.
--
-- Complexity: O(n)
asTreeLenR :: Int -> [e] -> AVL e
asTreeLenR n es = case subst (replicateAVL n ()) es of
                  UBT2(tree,es_) -> case es_ of
                                    [] -> tree
                                    _  -> error "asTreeLenR: List too long."
 where
 -- Substitute template values for real values taken from the list
 subst  E        as = UBT2(E,as)
 subst (N l _ r) as = subst' N l r as
 subst (Z l _ r) as = subst' Z l r as
 subst (P l _ r) as = subst' P l r as
 {-# INLINE subst' #-}
 subst' f l r as = case subst r as of
                   UBT2(r_,xs) -> case xs of
                                  a:as' -> case subst l as' of
                                           UBT2(l_,as__) -> let t_ = f l_ a r_
                                                            in t_ `seq` UBT2(t_,as__)
                                  []    -> error "asTreeLenR: List too short."

-- | As 'asTreeLenR', except the length of the list is calculated internally, not supplied
-- as an argument.
--
-- Complexity: O(n)
asTreeR :: [e] -> AVL e
asTreeR es = asTreeLenR (length es) es

-- | Reverse an AVL tree (swaps and reverses left and right sub-trees).
-- The resulting tree is the mirror image of the original.
--
-- Complexity: O(n)
reverseAVL :: AVL e -> AVL e
reverseAVL  E        = E
reverseAVL (N l e r) = let l' = reverseAVL l
                           r' = reverseAVL r
                       in  l' `seq` r' `seq` P r' e l'
reverseAVL (Z l e r) = let l' = reverseAVL l
                           r' = reverseAVL r
                       in  l' `seq` r' `seq` Z r' e l'
reverseAVL (P l e r) = let l' = reverseAVL l
                           r' = reverseAVL r
                       in  l' `seq` r' `seq` N r' e l'

-- | Apply a function to every element in an AVL tree. This function preserves the tree shape.
-- There is also a strict version of this function ('mapAVL'').
--
-- N.B. If the tree is sorted the result of this operation will only be sorted if
-- the applied function preserves ordering (for some suitable ordering definition).
--
-- Complexity: O(n)
mapAVL :: (a -> b) -> AVL a -> AVL b
mapAVL f = map' where
 map'  E        = E
 map' (N l a r) = let l' = map' l
                      r' = map' r
                  in  l' `seq` r' `seq` N l' (f a) r'
 map' (Z l a r) = let l' = map' l
                      r' = map' r
                  in  l' `seq` r' `seq` Z l' (f a) r'
 map' (P l a r) = let l' = map' l
                      r' = map' r
                  in  l' `seq` r' `seq` P l' (f a) r'

-- | Similar to 'mapAVL', but the supplied function is applied strictly.
--
-- Complexity: O(n)
mapAVL' :: (a -> b) -> AVL a -> AVL b
mapAVL' f = map' where
 map'  E        = E
 map' (N l a r) = let l' = map' l
                      r' = map' r
                      b  = f a
                  in  b `seq` l' `seq` r' `seq` N l' b r'
 map' (Z l a r) = let l' = map' l
                      r' = map' r
                      b  = f a
                  in  b `seq` l' `seq` r' `seq` Z l' b r'
 map' (P l a r) = let l' = map' l
                      r' = map' r
                      b  = f a
                  in  b `seq` l' `seq` r' `seq` P l' b r'

#if __GLASGOW_HASKELL__ > 604
traverseAVL :: Applicative f => (a -> f b) -> AVL a -> f (AVL b)
traverseAVL _f E = pure E
traverseAVL f (N l v r) = N <$> traverseAVL f l <*> f v <*> traverseAVL f r
traverseAVL f (Z l v r) = Z <$> traverseAVL f l <*> f v <*> traverseAVL f r
traverseAVL f (P l v r) = P <$> traverseAVL f l <*> f v <*> traverseAVL f r
#endif

-- | Construct a flat AVL tree of size n (n>=0), where all elements are identical.
--
-- Complexity: O(log n)
replicateAVL :: Int -> e -> AVL e
replicateAVL m e = rep m where -- Functional spaghetti follows :-)
 rep n | odd n = repOdd n -- n is odd , >=1
 rep n         = repEvn n -- n is even, >=0
 -- n is known to be odd (>=1), so left and right sub-trees are identical
 repOdd n      = let sub = rep (n `shiftR` 1) in sub `seq` Z sub e sub
 -- n is known to be even (>=0)
 repEvn n | n .&. (n-1) == 0 = repP2 n -- treat exact powers of 2 specially, traps n=0 too
 repEvn n      = let nl = n `shiftR` 1 -- size of left subtree  (odd or even)
                     nr = nl - 1       -- size of right subtree (even or odd)
                 in if odd nr
                    then let l = repEvn nl           -- right sub-tree is odd , so left is even (>=2)
                             r = repOdd nr
                         in l `seq` r `seq` Z l e r
                    else let l = repOdd nl           -- right sub-tree is even, so left is odd (>=2)
                             r = repEvn nr
                         in l `seq` r `seq` Z l e r
 -- n is an exact power of 2 (or 0), I.E. 0,1,2,4,8,16..
 repP2 0       = E
 repP2 1       = Z E e E
 repP2 n       = let nl = n `shiftR` 1 -- nl is also an exact power of 2
                     nr = nl - 1       -- nr is one less that an exact power of 2
                     l  = repP2 nl
                     r  = repP2M1 nr
                 in  l `seq` r `seq` P l e r -- BF=+1
 -- n is one less than an exact power of 2, I.E. 0,1,3,7,15..
 repP2M1 0     = E
 repP2M1 n     = let sub = repP2M1 (n `shiftR` 1) in sub `seq` Z sub e sub

-- | Flatten an AVL tree, preserving the ordering of the tree elements.
--
-- Complexity: O(n)
flatten :: AVL e -> AVL e
flatten t = asTreeLenL (size t) (asListL t)

-- | Similar to 'flatten', but the tree elements are reversed. This function has higher constant
-- factor overhead than 'reverseAVL'.
--
-- Complexity: O(n)
flatReverse :: AVL e -> AVL e
flatReverse t = asTreeLenL (size t) (asListR t)

-- | Similar to 'mapAVL', but the resulting tree is flat.
-- This function has higher constant factor overhead than 'mapAVL'.
--
-- Complexity: O(n)
flatMap :: (a -> b) -> AVL a -> AVL b
flatMap f t = asTreeLenL (size t) (map f (asListL t))

-- | Same as 'flatMap', but the supplied function is applied strictly.
--
-- Complexity: O(n)
flatMap' :: (a -> b) -> AVL a -> AVL b
flatMap' f t = asTreeLenL (size t) (map' f (asListL t)) where
 map' _ []     = []
 map' g (a:as) = let b = g a in b `seq` (b : map' f as)

-- | Remove all AVL tree elements which do not satisfy the supplied predicate.
-- Element ordering is preserved. The resulting tree is flat.
-- See 'filterAVL' for an alternative implementation which is probably more efficient.
--
-- Complexity: O(n)
filterViaList :: (e -> Bool) -> AVL e -> AVL e
filterViaList p t = filter' [] 0 (asListR t) where
 filter' se n []     = asTreeLenL n se
 filter' se n (e:es) = if p e then  let n'=n+1  in  n' `seq` filter' (e:se) n' es
                              else  filter' se n es

-- | Remove all AVL tree elements which do not satisfy the supplied predicate.
-- Element ordering is preserved.
--
-- Complexity: O(n)
filterAVL :: (e -> Bool) -> AVL e -> AVL e
filterAVL p t0 = case filter_ L(0) t0 of UBT3(_,t_,_) -> t_  -- Work with relative heights!!
 where filter_ h t = case t of
                     E       -> UBT3(False,E,h)
                     N l e r -> f l DECINT2(h) e r DECINT1(h)
                     Z l e r -> f l DECINT1(h) e r DECINT1(h)
                     P l e r -> f l DECINT1(h) e r DECINT2(h)
        where f l hl e r hr =                     case filter_ hl l of
                              UBT3(bl,l_,hl_)  -> case filter_ hr r of
                               UBT3(br,r_,hr_) -> if p e
                                                  then if bl || br
                                                       then case spliceH l_ hl_ e r_ hr_ of
                                                            UBT2(t_,h_) -> UBT3(True,t_,h_)
                                                       else UBT3(False,t,h)
                                                  else case joinH l_ hl_ r_ hr_ of
                                                       UBT2(t_,h_) -> UBT3(True,t_,h_)

-- | Partition an AVL tree using the supplied predicate. The first AVL tree in the
-- resulting pair contains all elements for which the predicate is True, the second
-- contains all those for which the predicate is False. Element ordering is preserved.
-- Both of the resulting trees are flat.
--
-- Complexity: O(n)
partitionAVL :: (e -> Bool) -> AVL e -> (AVL e, AVL e)
partitionAVL p t = part 0 [] 0 [] (asListR t) where
 part nT lstT nF lstF []     = let avlT = asTreeLenL nT lstT
                                   avlF = asTreeLenL nF lstF
                               in (avlT,avlF) -- Non strict in avlT, avlF !!
 part nT lstT nF lstF (e:es) = if p e then let nT'=nT+1 in nT' `seq` part nT' (e:lstT) nF     lstF  es
                                      else let nF'=nF+1 in nF' `seq` part nT     lstT  nF' (e:lstF) es

-- | Remove all AVL tree elements for which the supplied function returns 'Nothing'.
-- Element ordering is preserved. The resulting tree is flat.
-- See 'mapMaybeAVL' for an alternative implementation which is probably more efficient.
--
-- Complexity: O(n)
mapMaybeViaList :: (a -> Maybe b) -> AVL a -> AVL b
mapMaybeViaList f t = map' [] 0 (asListR t) where
 map' sb n []     = asTreeLenL n sb
 map' sb n (a:as) = case f a of
                    Just b  -> let n'=n+1  in  n' `seq` map' (b:sb) n' as
                    Nothing -> map' sb n as

-- | Remove all AVL tree elements for which the supplied function returns 'Nothing'.
-- Element ordering is preserved.
--
-- Complexity: O(n)
mapMaybeAVL :: (a -> Maybe b) -> AVL a -> AVL b
mapMaybeAVL f t0 = case mapMaybe_ L(0) t0 of UBT2(t_,_) -> t_  -- Work with relative heights!!
 where mapMaybe_ h t = case t of
                       E       -> UBT2(E,h)
                       N l a r -> m l DECINT2(h) a r DECINT1(h)
                       Z l a r -> m l DECINT1(h) a r DECINT1(h)
                       P l a r -> m l DECINT1(h) a r DECINT2(h)
        where m l hl a r hr =                  case mapMaybe_ hl l of
                              UBT2(l_,hl_)  -> case mapMaybe_ hr r of
                               UBT2(r_,hr_) -> case f a of
                                               Just b  -> spliceH l_ hl_ b r_ hr_
                                               Nothing ->   joinH l_ hl_   r_ hr_

-- | Invokes 'genPushList' on the empty AVL tree.
--
-- Complexity: O(n.(log n))
{-# INLINE genAsTree #-}
genAsTree :: (e -> e -> COrdering e) -> [e] -> AVL e
genAsTree c = genPushList c empty

-- | Push the elements of an unsorted List in a sorted AVL tree using the supplied combining comparison.
--
-- Complexity: O(n.(log (m+n))) where n is the list length, m is the tree size.
genPushList :: (e -> e -> COrdering e) -> AVL e -> [e] -> AVL e
genPushList c avl = foldl' addElem avl
 where addElem t e = genPush (c e) e t

-- | Uses the supplied combining comparison to sort list elements into ascending order.
-- Multiple occurences of the same element are eliminated (they are combined in some way).
--
-- @'genSortAscending' c = 'asListL' . 'genAsTree' c@
--
-- Complexity: O(n.(log n))
{-# INLINE genSortAscending #-}
genSortAscending :: (e -> e -> COrdering e) -> [e] -> [e]
genSortAscending c = asListL . genAsTree c

-- | Uses the supplied combining comparison to sort list elements into descending order.
-- Multiple occurences of the same element are eliminated (they are combined in some way).
--
-- @'genSortDescending' c = 'asListR' . 'genAsTree' c@
--
-- Complexity: O(n.(log n))
{-# INLINE genSortDescending #-}
genSortDescending :: (e -> e -> COrdering e) -> [e] -> [e]
genSortDescending c = asListR . genAsTree c


