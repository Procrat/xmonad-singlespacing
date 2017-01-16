{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module XMonad.Layout.SingleSpacing (
    spacing
) where


import           Control.Arrow                (second)

import           Graphics.X11                 (Rectangle (..))

import qualified XMonad.Layout.LayoutModifier as LM
import           XMonad.Util.XUtils           (fi)


newtype SingleSpacing a = SingleSpacing Int deriving (Read, Show)

instance LM.LayoutModifier SingleSpacing a where
    pureModifier (SingleSpacing pixels) screen _ windows =
        (map (second $ shrink pixels screen) windows, Nothing)

-------------------------------------------------------------------------------
spacing :: Int -> l a -> LM.ModifiedLayout SingleSpacing l a
spacing pixels = LM.ModifiedLayout $ SingleSpacing pixels

shrink :: Int -> Rectangle -> Rectangle -> Rectangle
shrink pixels (Rectangle screenX screenY _ _)
              (Rectangle winX winY winWidth winHeight) =
    Rectangle (winX + offsetX) (winY + offsetY)
              (winWidth - fi pixels - fi offsetX)
              (winHeight - fi pixels - fi offsetY)
    where offsetX = if winX == screenX then fi pixels else 0
          offsetY = if winY == screenY then fi pixels else 0
