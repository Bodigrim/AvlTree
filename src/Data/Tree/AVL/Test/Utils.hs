-- |
-- Copyright   :  (c) Adrian Hey 2004,2005
-- License     :  BSD3
--
--
-- 'AVL' tree related test and verification utilities.
module Data.Tree.AVL.Test.Utils
        (-- * Correctness checking.
         isBalanced,isSorted,isSortedOK,
         -- * Tree parameter utilities.
         minElements,maxElements,
        ) where

import Data.Tree.AVL.Internals.Types(AVL(..))

import GHC.Base
#include "ghcdefs.h"

-- | Verify that a tree is height balanced and that the BF of each node is correct.
--
-- Complexity: O(n)
isBalanced :: AVL e -> Bool
isBalanced t = not (isTrue# (cH t EQL L(-1)))

-- Local utility, returns height if balanced, -1 if not
cH :: AVL e -> UINT
cH  E        = L(0)
cH (N l _ r) = cH_ L(1) l r -- (hr-hl) = 1
cH (Z l _ r) = cH_ L(0) l r -- (hr-hl) = 0
cH (P l _ r) = cH_ L(1) r l -- (hl-hr) = 1
cH_ :: UINT -> AVL e -> AVL e -> UINT
cH_ delta l r = let hl = cH l
                in if isTrue# (hl EQL L(-1)) then hl
                                   else let hr = cH r
                                        in if isTrue# (hr EQL L(-1)) then hr
                                                           else if isTrue# (SUBINT(hr,hl) EQL delta) then INCINT1(hr)
                                                                                           else L(-1)

-- | Verify that a tree is sorted.
--
-- Complexity: O(n)
isSorted :: (e -> e -> Ordering) -> AVL e -> Bool
isSorted  c = isSorted' where
 isSorted'  E        = True
 isSorted' (N l e r) = isSorted'' l e r
 isSorted' (Z l e r) = isSorted'' l e r
 isSorted' (P l e r) = isSorted'' l e r
 isSorted''   l e r  = (isSortedU l e) && (isSortedL e r)
 -- Verify tree is sorted and rightmost element is less than an upper limit (ul)
 isSortedU  E        _  = True
 isSortedU (N l e r) ul = isSortedU' l e r ul
 isSortedU (Z l e r) ul = isSortedU' l e r ul
 isSortedU (P l e r) ul = isSortedU' l e r ul
 isSortedU'   l e r  ul = case c e ul of
                          LT -> (isSortedU l e) && (isSortedLU e r ul)
                          _  -> False
 -- Verify tree is sorted and leftmost element is greater than a lower limit (ll)
 isSortedL  _   E        = True
 isSortedL  ll (N l e r) = isSortedL' ll l e r
 isSortedL  ll (Z l e r) = isSortedL' ll l e r
 isSortedL  ll (P l e r) = isSortedL' ll l e r
 isSortedL' ll    l e r  = case c e ll of
                           GT -> (isSortedLU ll l e) && (isSortedL e r)
                           _  -> False
 -- Verify tree is sorted and leftmost element is greater than a lower limit (ll)
 -- and rightmost element is less than an upper limit (ul)
 isSortedLU  _   E        _  = True
 isSortedLU  ll (N l e r) ul = isSortedLU' ll l e r ul
 isSortedLU  ll (Z l e r) ul = isSortedLU' ll l e r ul
 isSortedLU  ll (P l e r) ul = isSortedLU' ll l e r ul
 isSortedLU' ll    l e r  ul = case c e ll of
                               GT -> case c e ul of
                                     LT -> (isSortedLU ll l e) && (isSortedLU e r ul)
                                     _  -> False
                               _  -> False
-- isSorted ends --
-------------------

-- | Verify that a tree is sorted, height balanced and the BF of each node is correct.
--
-- Complexity: O(n)
isSortedOK :: (e -> e -> Ordering) -> AVL e -> Bool
isSortedOK c t = (isBalanced t) && (isSorted c t)

-- | Detetermine the minimum number of elements in an AVL tree of given height.
-- This function satisfies this recurrence relation..
--
-- @
-- minElements 0 = 0
-- minElements 1 = 1
-- minElements h = 1 + minElements (h-1) + minElements (h-2)
--            -- = Some weird expression involving the golden ratio
-- @
minElements :: Int -> Integer
minElements 0 = 0
minElements 1 = 1
minElements h = minElements' 0 1 h where
 minElements' n1 n2 2 = 1 + n1 + n2
 minElements' n1 n2 m = minElements' n2 (1 + n1 + n2) (m-1)

-- | Detetermine the maximum number of elements in an AVL tree of given height.
-- This function satisfies this recurrence relation..
--
-- @
-- maxElements 0 = 0
-- maxElements h = 1 + 2 * maxElements (h-1) -- = 2^h-1
-- @
maxElements :: Int -> Integer
maxElements 0 = 0
maxElements h = maxElements' 0 h where
 maxElements' n1 1 = 1 + 2*n1
 maxElements' n1 m = maxElements' (1 + 2*n1) (m-1)
