-- |
-- Copyright   :  (c) Adrian Hey 2004,2005
-- License     :  BSD3
--
-- A few simple utility functions.
{-# OPTIONS_HADDOCK hide #-}
module Data.Tree.AVL.Utils
        ( -- * Simple AVL related utilities
         empty,isEmpty,isNonEmpty,singleton,pair,tryGetSingleton,

        ) where

import Data.Tree.AVL.Internals.Types (AVL(..))

-- | The empty AVL tree.
{-# INLINE empty #-}
empty :: AVL e
empty = E

-- | Returns 'True' if an AVL tree is empty.
--
-- Complexity: O(1)
isEmpty :: AVL e -> Bool
isEmpty E = True
isEmpty _ = False
{-# INLINE isEmpty #-}

-- | Returns 'True' if an AVL tree is non-empty.
--
-- Complexity: O(1)
isNonEmpty :: AVL e -> Bool
isNonEmpty E = False
isNonEmpty _ = True
{-# INLINE isNonEmpty #-}

-- | Creates an AVL tree with just one element.
--
-- Complexity: O(1)
singleton :: e -> AVL e
singleton e = Z E e E
{-# INLINE singleton #-}

-- | Create an AVL tree of two elements, occuring in same order as the arguments.
pair :: e -> e -> AVL e
pair e0 e1 = P (Z E e0 E) e1 E
{-# INLINE pair #-}

-- | If the AVL tree is a singleton (has only one element @e@) then this function returns @('Just' e)@.
-- Otherwise it returns Nothing.
--
-- Complexity: O(1)
tryGetSingleton :: AVL e -> Maybe e
tryGetSingleton (Z E e _) = Just e -- Right subtree must be E too, but no need to waste time checking
tryGetSingleton _         = Nothing
{-# INLINE tryGetSingleton #-}
