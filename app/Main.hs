{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import           Data.Monoid
import           Lib

main :: IO ()
main = putStrLn $ showPlane 10 $ fmap getSum $ convoluteSpread $ convoluteSpread $ fmap Sum $ plane
  where
    fillZero :: Zipper Int
    fillZero = Zipper (repeat 0) 0 (repeat 0)

    plane :: Plane Int
    plane = Plane $ Zipper
        { before  = repeat fillZero
        , current = fillZero { current = 1 }
        , after   = repeat fillZero}

convoluteSpread :: Monoid a => Plane a -> Plane a
convoluteSpread = extend spread


spread :: Monoid a => Plane a -> a
spread (Plane (Zipper ((Zipper as b cs) : farAbove) (Zipper (d : ds) e (f : fs)) ((Zipper gs h is) : farBelow)))
    = mconcat [b, d, e, f, h]


data Zipper a = Zipper
    { before  :: [a]
    , current :: a
    , after   :: [a] }
    deriving (Functor)

moveBefore, moveAfter :: Zipper a -> Zipper a
moveBefore zipper@Zipper { before = a : as, current = b, after = cs }
    = zipper { before = as, current = a, after = b : cs }
moveAfter  zipper@Zipper { before = as, current = b, after = c : cs }
    = zipper { before = b : as, current = c, after = cs }

showZipper :: Show a => Int -> Zipper a -> String
showZipper n Zipper{..} = show =<< reverse (take n before) ++ [current] ++ take n after

-- | Outer Zipper: Up/down
-- Inner Zipper: Left/Right
newtype Plane a = Plane { unPlane :: Zipper (Zipper a) }
    deriving (Functor)

moveLeft, moveRight, moveUp, moveDown :: Plane a -> Plane a
moveLeft  = Plane . fmap moveBefore . unPlane
moveRight = Plane . fmap moveAfter  . unPlane
moveUp    = Plane . moveBefore      . unPlane
moveDown  = Plane . moveAfter       . unPlane


showPlane :: Show a => Int -> Plane a -> String
showPlane n (Plane Zipper{..}) = unlines . fmap (showZipper n) $
    reverse (take n before) ++ [current] ++ take n after




class Comonad w where
    extract :: w a -> a
    extend :: (w a -> b) -> w a -> w b

instance Comonad Zipper where
    extract = current
    extend f zipper = fmap f $ Zipper
        { before  = iterate1 moveBefore zipper
        , current = zipper
        , after   = iterate1 moveAfter zipper }
      where
        iterate1 f x = tail (iterate f x)

instance Comonad Plane where
    extract = current . current . unPlane
    extend f plane = fmap f $ Plane $ Zipper
        { before  = fmap foo (iterate1 moveUp plane)
        , current = foo plane
        , after   = fmap foo (iterate1 moveDown plane) }
      where
        foo p = Zipper
            { before = iterate1 moveLeft p
            , current = p
            , after = iterate1 moveRight p }
        iterate1 f x = tail (iterate f x)


newtype Diagram = Diag ((Char, Char, Char), (Char, Char, Char), (Char, Char, Char))

fromString :: String -> Diagram
fromString [a, b, c, d, e, f, g, h, i] = Diag ((a, b, c), (d, e, f), (g, h, i))
fromString _ = undefined

diagrams :: [(Diagram, Char)]
diagrams = fmap (\(a, b) -> (fromString a, b))
    [ ( "   \
        \---\
        \   "
      , '─' )

    , ( "   \
        \ --\
        \   "
      , '─' )

    , ( "   \
        \ --\
        \   "
      , '─' )

    ]

