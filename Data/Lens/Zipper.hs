{-# LANGUAGE TypeOperators
           , MultiParamTypeClasses, FlexibleContexts
           , TypeFamilies
           , FlexibleInstances #-}
module Data.Lens.Zipper (
{- |
   We provide a simple, heterogenous, fully type-checked, generic zipper
   implementation. This flavor of zipper doesn\'t use \"left\" and \"right\" for
   navigation, and instead relies on lenses to indicate a child type to \"move
   to\".
-}

  -- * Zipper type
    Zipper(..)
  -- ** Zipper history 
{- |
   These three types make up the heterogenous history stack, and the programmer
   should never need to use them directly. They come together with 'Zipper' to
   form types that look like, e.g.
    
   > -- a zipper on a Tree, with focus on a leaf "a" of a 2nd level subtree
   > z :: Zipper (Top :> Tree a :> Tree a) a
    
   This zipper works conceptually like the \"breacrumbs\" navigation UI design
   pattern, and the types reflect this visually.
    
   Nevertheless user-provided type annotations should almost never be
   necessary, so these will probably never appear in your code.
-}
  , Top(..) , (:>)(..) , Hist(..)

  -- * Zipper operations
  , zipper , close
  -- ** Motions
  , move , moveP , moveUp
  -- ** Focus
{- |
   In addition to these, 'viewf' can be used to view the focus.
-}
  , focus , setf , modf

 ) where

{- TODO
-      - either switch to new lens lib, or add TH to your own
-      - excellent rewrite rules
-          - first look at core output of simple example
-          - add rules one-by-one, looking at core
-      - change moveP -> pmove?
-      - consider a newtype-wrapped submodule encapsulating monad return value
-          what we really want is a notation like:
-              move x
-              move y
-              foc <- move z
-              moveUp 
-              modf (+foc)
-          quasiquotation, or can we shoe-horn this into proc notation? 
-          otherwise add those combinators (in notes)
-      - more advanced motions a.la. pez?
-      - better demos
-      - pretty Show instance for Zipper
-}

import Data.Yall.Lens
import Control.Monad.Identity

-- | Our zipper type, parameterized by a 'focus' and \"history stack\",
-- supporting completely type-checked zipper operations.
data Zipper st b = Zipper { hist  :: st b , viewf :: b }
data (:>) st b c = Snoc (st b) (c -> b) 
data Top a = Top

-- | A lens on the focus of the zipper.
focus :: Zipper st b :-> b
focus = lens viewf $ \z b-> z{ viewf = b }

-- | Set the zipper focus
--
-- > setf = set focus
setf :: Zipper st b -> b -> Zipper st b
setf = set focus

-- | Modify the zipper focus
--
-- > modf = modify focus
modf :: (b -> b) -> Zipper st b -> Zipper st b
modf = modify focus

-- | \"enter\" a data type. Move the 'focus' with 'move' and 'moveUp'. Exit
-- the zipper with 'close'.
--
-- > zipper = Zipper Top
zipper :: a -> Zipper Top a
zipper = Zipper Top


class Hist st a c  where
     runHist :: st c -> (c -> a)
-- our only use of TypeFamilies. Thanks to Daniel Wagner for this trick:
instance a ~ b => Hist Top a b where
     runHist _ = id
instance (Hist st a b) => Hist ((:>) st b) a c where
     runHist (Snoc st' cb) = runHist st' . cb

-- | exit the zipper, rebuilding the structure @a@:
--
-- > close (Zipper st b) = runHist st b
close :: (Hist st a b)=> Zipper st b -> a
close (Zipper st b) = runHist st b



-- | navigate to a child element indicated by the passed lens, returning the
-- new Zipper in the monad @m@. This will be 'Maybe' when the standard (':~>')
-- Lens is used. For pure lenses, use 'moveP'.
move :: (Monad m)=> LensM m b c -> Zipper st b -> m (Zipper (st :> b) c)
move l (Zipper st a) = 
    liftM (uncurry $ Zipper . Snoc st . fmap runIdentity) (runLens l a)

-- | navigate to a child element indicated by the passed pure lens
--
-- > moveP l = runIdentity . move l
moveP :: (b :-> c) -> Zipper st b -> Zipper (st :> b) c
moveP l = runIdentity . move l


-- | navigate up a level in a zipper not already at 'Top'
--
-- > moveUp (Zipper (Snoc st cont) c) = Zipper st $ cont c
moveUp :: Zipper (st :> b) c -> Zipper st b
moveUp (Zipper (Snoc st cont) c) = Zipper st $ cont c
