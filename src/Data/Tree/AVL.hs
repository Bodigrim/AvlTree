{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Copyright   :  (c) Adrian Hey 2004,2008
-- License     :  BSD3
--
-- Many of the functions defined by this package make use of generalised comparison functions
-- which return a variant of the Prelude 'Prelude.Ordering' data type: 'Data.COrdering.COrdering'. These
-- are refered to as \"combining comparisons\". (This is because they combine \"equal\"
-- values in some manner defined by the user.)
--
-- The idea is that using this simple mechanism you can define many practical and
-- useful variations of tree (or general set) operations from a few generic primitives,
-- something that would not be so easy using plain 'Prelude.Ordering' comparisons
-- (overloaded or otherwise).
--
-- Functions which involve searching a tree really only require a single argument
-- function which takes the current tree element value as argument and returns
-- an 'Prelude.Ordering' or 'Data.COrdering.COrdering' to direct the next stage of the search down
-- the left or right sub-trees (or stop at the current element). For documentation
-- purposes, these functions are called \"selectors\" throughout this library.
-- Typically a selector will be obtained by partially applying the appropriate
-- combining comparison with the value or key being searched for. For example..
--
-- @
-- mySelector :: Int -> Ordering               Tree elements are Ints
-- -- or
-- mySelector :: (key, val) -> COrdering val   Tree elements are (key, val) pairs
-- @
--
module Data.Tree.AVL
(-- * Types
 AVL,

 -- * Simple AVL related utilities
 empty,isEmpty,isNonEmpty,singleton,pair,tryGetSingleton,

 -- * Reading from AVL trees

 -- ** Reading from extreme left or right
 assertReadL,tryReadL,
 assertReadR,tryReadR,

 -- ** Reading from /sorted/ AVL trees
 assertRead,tryRead,tryReadMaybe,defaultRead,

 -- ** Simple searches of /sorted/ AVL trees
 contains,

 -- * Writing to AVL trees
 -- | These functions alter the content of a tree (values of tree elements) but not the structure
 -- of a tree.

 -- ** Writing to extreme left or right
 -- | I'm not sure these are likely to be much use in practice, but they're
 -- simple enough to implement so are included for the sake of completeness.
 writeL,tryWriteL,writeR,tryWriteR,

 -- ** Writing to /sorted/ trees
 write,writeFast,tryWrite,writeMaybe,tryWriteMaybe,

 -- * \"Pushing\" new elements into AVL trees
 -- | \"Pushing\" is another word for insertion. (c.f \"Popping\".)

 -- ** Pushing on extreme left or right
 pushL,pushR,

 -- ** Pushing on /sorted/ AVL trees
 push,push',pushMaybe,pushMaybe',

 -- * Deleting elements from AVL trees

 -- ** Deleting from extreme left or right
 delL,delR,assertDelL,assertDelR,tryDelL,tryDelR,

 -- ** Deleting from /sorted/ trees
 delete,deleteFast,deleteIf,deleteMaybe,

 -- * \"Popping\" elements from AVL trees
 -- | \"Popping\" means reading and deleting a tree element in a single operation.

 -- ** Popping from extreme left or right
 assertPopL,assertPopR,tryPopL,tryPopR,

 -- ** Popping from /sorted/ trees
 assertPop,tryPop,assertPopMaybe,tryPopMaybe,assertPopIf,tryPopIf,

 -- * Set operations
 -- | Functions for manipulating AVL trees which represent ordered sets (I.E. /sorted/ trees).
 -- Note that although many of these functions work with a variety of different element
 -- types they all require that elements are sorted according to the same criterion (such
 -- as a field value in a record).

 -- ** Union
 union,unionMaybe,disjointUnion,unions,

 -- ** Difference
 difference,differenceMaybe,symDifference,

 -- ** Intersection
 intersection,intersectionMaybe,

 -- *** Intersection with the result as a list
 -- | Sometimes you don\'t want intersection to give a tree, particularly if the
 -- resulting elements are not orderered or sorted according to whatever criterion was
 -- used to sort the elements of the input sets.
 --
 -- The reason these variants are provided for intersection only (and not the other
 -- set functions) is that the (tree returning) intersections always construct an entirely
 -- new tree, whereas with the others the resulting tree will typically share sub-trees
 -- with one or both of the originals. (Of course the results of the others can easily be
 -- converted to a list too if required.)
 intersectionToList,intersectionAsList,
 intersectionMaybeToList,intersectionMaybeAsList,

 -- ** \'Venn diagram\' operations
 -- | Given two sets A and B represented as sorted AVL trees, the venn operations evaluate
 -- components @A-B@, @A.B@ and @B-A@. The intersection part may be obtained as a List
 -- rather than AVL tree if required.
 --
 -- Note that in all cases the three resulting sets are /disjoint/ and can safely be re-combined
 -- after most \"munging\" operations using 'disjointUnion'.
 venn,vennMaybe,

 -- *** \'Venn diagram\' operations with the intersection component as a List.
 -- | These variants are provided for the same reasons as the Intersection as List variants.
 vennToList,vennAsList,
 vennMaybeToList,vennMaybeAsList,

 -- ** Subset
 isSubsetOf,isSubsetOfBy,

 -- * The AVL Zipper
 -- | An implementation of \"The Zipper\" for AVL trees. This can be used like
 -- a functional pointer to a serial data structure which can be navigated
 -- and modified, without having to worry about all those tricky tree balancing
 -- issues. See JFP Vol.7 part 5 or <http://haskell.org/haskellwiki/Zipper>.
 --
 -- Notes about efficiency:
 --
 -- The functions defined here provide a useful way to achieve those awkward
 -- operations which may not be covered by the rest of this package. They're
 -- reasonably efficient (mostly O(log n) or better), but zipper flexibility
 -- is bought at the expense of keeping path information explicitly as a heap
 -- data structure rather than implicitly on the stack. Since heap storage
 -- probably costs more, zipper operations will are likely to incur higher
 -- constant factors than equivalent non-zipper operations (if available).
 --
 -- Some of the functions provided here may appear to be weird combinations of
 -- functions from a more logical set of primitives. They are provided because
 -- they are not really simple combinations of the corresponding primitives.
 -- They are more efficient, so you should use them if possible (e.g combining
 -- deleting with Zipper closing).
 --
 -- Also, consider using the t'BAVL' as a cheaper alternative if you don't
 -- need to navigate the tree.

 -- ** Types
 ZAVL,PAVL,

 -- ** Opening
 assertOpenL,assertOpenR,
 tryOpenL,tryOpenR,
 assertOpen,tryOpen,
 tryOpenGE,tryOpenLE,
 openEither,

 -- ** Closing
 close,fillClose,

 -- ** Manipulating the current element.
 getCurrent,putCurrent,applyCurrent,applyCurrent',

 -- ** Moving
 assertMoveL,assertMoveR,tryMoveL,tryMoveR,

 -- ** Inserting elements
 insertL,insertR,insertMoveL,insertMoveR,fill,

 -- ** Deleting elements
 delClose,
 assertDelMoveL,assertDelMoveR,tryDelMoveR,tryDelMoveL,
 delAllL,delAllR,
 delAllCloseL,delAllCloseR,
 delAllIncCloseL,delAllIncCloseR,

 -- ** Inserting AVL trees
 insertTreeL,insertTreeR,

 -- ** Current element status
 isLeftmost,isRightmost,
 sizeL,sizeR,

 -- ** Operations on whole zippers
 sizeZAVL,

 -- ** A cheaper option is to use BAVL
 -- | These are a cheaper but more restrictive alternative to using the full Zipper.
 -- They use \"Binary Paths\" (Ints) to point to a particular element of an 'AVL' tree.
 -- Use these when you don't need to navigate the tree, you just want to look at a
 -- particular element (and perhaps modify or delete it). The advantage of these is
 -- that they don't create the usual Zipper heap structure, so they will be faster
 -- (and reduce heap burn rate too).
 --
 -- If you subsequently decide you need a Zipper rather than a BAVL then some conversion
 -- utilities are provided.

 -- *** Types
 BAVL,

 -- *** Opening and closing
 openBAVL,closeBAVL,

 -- *** Inspecting status
 fullBAVL,emptyBAVL,tryReadBAVL,readFullBAVL,

 -- *** Modifying the tree
 pushBAVL,deleteBAVL,

 -- *** Converting to BAVL to Zipper
 -- | These are O(log n) operations but with low constant factors because no comparisons
 -- are required (and the tree nodes on the path will most likely still be in cache as
 -- a result of opening the BAVL in the first place).
 fullBAVLtoZAVL,emptyBAVLtoPAVL,anyBAVLtoEither,

 -- * Joining AVL trees
 join,concatAVL,flatConcat,

 -- * List related utilities for AVL trees

 -- ** Converting AVL trees to Lists (fixed element order).
 -- | These functions are lazy and allow normal lazy list processing
 -- style to be used (without necessarily converting the entire tree
 -- to a list in one gulp).
 asListL,toListL,asListR,toListR,

 -- ** Converting Lists to AVL trees (fixed element order)
 asTreeLenL,asTreeL,
 asTreeLenR,asTreeR,

 -- ** Converting unsorted Lists to sorted AVL trees
 asTree,

 -- ** \"Pushing\" unsorted Lists in sorted AVL trees
 pushList,

 -- * Some analogues of common List functions
 reverse,map,map',
 mapAccumL  ,mapAccumR  ,
 mapAccumL' ,mapAccumR' ,
 replicate,
 filter,mapMaybe,
 filterViaList,mapMaybeViaList,
 partition,
 traverseAVL,

 -- ** Folds
 -- | Note that unlike folds over lists ('foldr' and 'foldl'), there is no
 -- significant difference between left and right folds in AVL trees, other
 -- than which side of the tree each starts with.
 -- Therefore this library provides strict and lazy versions of both.
 foldr,foldr',foldr1,foldr1',foldr2,foldr2',
 foldl,foldl',foldl1,foldl1',foldl2,foldl2',

         -- ** (GHC Only)
         mapAccumL'',mapAccumR'', foldrInt#,

 -- * Some clones of common List functions
 -- | These are a cure for the horrible @O(n^2)@ complexity the noddy Data.List definitions.
 nub,nubBy,

 -- * \"Flattening\" AVL trees
 -- | These functions can be improve search times by reducing a tree of given size to
 -- the minimum possible height.
 flatten,
 flatReverse,flatMap,flatMap',

 -- * Splitting AVL trees

 -- ** Taking fixed size lumps of tree
 -- | Bear in mind that the tree size (s) is not stored in the AVL data structure, but if it is
 -- already known for other reasons then for (n > s\/2) using the appropriate complementary
 -- function with argument (s-n) will be faster.
 -- But it's probably not worth invoking 'Data.Tree.AVL.Internals.Types.size' for no reason other than to
 -- exploit this optimisation (because this is O(s) anyway).
 splitAtL,splitAtR,takeL,takeR,dropL,dropR,

 -- ** Rotations
 -- | Bear in mind that the tree size (s) is not stored in the AVL data structure, but if it is
 -- already known for other reasons then for (n > s\/2) using the appropriate complementary
 -- function with argument (s-n) will be faster.
 -- But it's probably not worth invoking 'Data.Tree.AVL.Internals.Types.size' for no reason other than to exploit this optimisation
 -- (because this is O(s) anyway).
 rotateL,rotateR,popRotateL,popRotateR,rotateByL,rotateByR,

 -- ** Taking lumps of tree according to a supplied predicate
 spanL,spanR,takeWhileL,dropWhileL,takeWhileR,dropWhileR,

 -- ** Taking lumps of /sorted/ trees
 -- | Prepare to get confused. All these functions adhere to the same Ordering convention as
 -- is used for searches. That is, if the supplied selector returns LT that means the search
 -- key is less than the current tree element. Or put another way, the current tree element
 -- is greater than the search key.
 --
 -- So (for example) the result of the 'takeLT' function is a tree containing all those elements
 -- which are less than the notional search key. That is, all those elements for which the
 -- supplied selector returns GT (not LT as you might expect). I know that seems backwards, but
 -- it's consistent if you think about it.
 forkL,forkR,fork,
 takeLE,dropGT,
 takeLT,dropGE,
 takeGT,dropLE,
 takeGE,dropLT,

 -- * AVL tree size utilities
 size,addSize,clipSize,
 addSize#,size#,

-- * AVL tree height utilities
 height,addHeight,compareHeight,

 -- * Low level Binary Path utilities
 -- | This is the low level (unsafe) API used by the t'BAVL' type
 BinPath(..),findFullPath,findEmptyPath,openPath,openPathWith,readPath,writePath,insertPath,deletePath,

 -- * Correctness checking
 isBalanced,isSorted,isSortedOK,

 -- * Tree parameter utilities
 minElements,maxElements,
) where

import Prelude ()
import Data.Functor (Functor, fmap)
import Data.Traversable (Traversable, traverse)
import Data.Tree.AVL.Internals.Types hiding (E,N,P,Z)
import Data.Tree.AVL.Utils
import Data.Tree.AVL.Size
import Data.Tree.AVL.Height
import Data.Tree.AVL.Read
import Data.Tree.AVL.Write
import Data.Tree.AVL.Push
import Data.Tree.AVL.Delete
import Data.Tree.AVL.List
import Data.Tree.AVL.Join
import Data.Tree.AVL.Split
import Data.Tree.AVL.Set
import Data.Tree.AVL.Zipper
import Data.Tree.AVL.Test.Utils(isBalanced,isSorted,isSortedOK,minElements,maxElements)
import Data.Tree.AVL.BinPath(BinPath(..),findFullPath,findEmptyPath,openPath,openPathWith,readPath,writePath,insertPath)
import Data.Tree.AVL.Internals.DelUtils(deletePath)

-- This definition has been placed here
-- to avoid introducing cyclic dependency between Types.hs and List.hs
instance Functor AVL where
 fmap = map           -- The lazy version.

instance Traversable AVL where
    traverse = traverseAVL
