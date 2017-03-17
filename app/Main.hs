{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import           Data.Maybe
import           Data.Monoid
import           Lib
import           System.Environment

main :: IO ()
main = do
    input <- getArgs >>= \case
        []       -> getContents
        file : _ -> readFile file
    let plane = planeFromList' ' ' (lines input)
    putStrLn $ unlines $ planeToList 30 $ extend (substituteChar diagrams) $ plane

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
showZipper n zipper = zipperToList n zipper >>= show

zipperToList :: Int -> Zipper a -> [a]
zipperToList n Zipper{..} = current : take n after

zipperOf :: a -> Zipper a
zipperOf a = Zipper { before = repeat a, current = a, after = repeat a }

emptyZipper :: Monoid a => Zipper a
emptyZipper = zipperOf mempty

zipperFromList' :: a -> [a] -> Zipper a
zipperFromList' a = \case
    []     -> zipperOf a
    b : bs -> (zipperOf a) { current = b, after = bs ++ repeat a }

zipperFromList :: Monoid a => [a] -> Zipper a
zipperFromList = zipperFromList' mempty

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
    current : take n after

planeToList :: Int -> Plane a -> [[a]]
planeToList n (Plane Zipper{..}) = fmap (zipperToList n) $
    current : take n after


planeOf :: a -> Plane a
planeOf a = Plane $ Zipper { before = repeat (zipperOf a), current = zipperOf a, after = repeat (zipperOf a) }

emptyPlane :: Monoid a => Plane a
emptyPlane = planeOf mempty

planeFromList :: Monoid a => [[a]] -> Plane a
planeFromList = planeFromList' mempty

planeFromList' :: a -> [[a]] -> Plane a
planeFromList' a = \case
    []       -> planeOf a
    as : ass -> Plane $ (zipperOf (zipperOf a)) { current = zipperFromList' a as, after = fmap (zipperFromList' a) ass ++ repeat (zipperOf a) }




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

toString :: Diagram -> String
toString (Diag ((a, b, c), (d, e, f), (g, h, i))) = [a, b, c, d, e, f, g, h, i]

lookupDiagram :: Diagram -> [(Diagram, Char)] -> Maybe Char
lookupDiagram pattern mappings = case filter (satisfies pattern) mappings of
    []    -> Nothing
    a : _ -> Just (snd a)
  where
    satisfies :: Diagram -> (Diagram, Char) -> Bool
    satisfies diagram@(Diag(_, (_, a1, _), _)) (pattern@(Diag(_, (_, a2, _), _)), _)
      = a1 `connectsLike` a2 && (and $ zipWith connectsLike (toString diagram) (toString pattern))

connectsLike :: Char -> Char -> Bool
char `connectsLike` pattern = case pattern of
    '-'  -> char `elem` ['-', '+', '\'', '.', '>', '<']
    '+'  -> char `elem` ['+', '\'', '.']
    '|'  -> char `elem` ['|', '+', '\'', '.', '^', 'v']
    '.'  -> char `elem` ['\'', '.']
    '\''-> char `elem` ['\'', '.']
    '#'  -> char `elem` ['#']
    '<'  -> char `elem` ['<']
    '>'  -> char `elem` ['>']
    '^'  -> char `elem` ['^']
    'v'  -> char `elem` ['v']
    ' '  -> True
    _    -> False

diagrams :: [(Diagram, Char)]
diagrams = reverse $ fmap (\(a, b) -> (fromString a, b))
    [ ( "   \
        \ --\
        \   ", '─' )

    , ( "   \
        \-- \
        \   ", '─' )

    , ( "   \
        \ | \
        \ | ", '│' )

    , ( " | \
        \ | \
        \   ", '│' )

    , ( " | \
        \-+ \
        \   ", '┘' )

    , ( " | \
        \ +-\
        \   ", '└' )

    , ( "   \
        \ +-\
        \ | ", '┌' )

    , ( "   \
        \-+ \
        \ | ", '┐' )

    , ( " | \
        \ +-\
        \ | ", '├' )

    , ( " | \
        \-+ \
        \ | ", '┤' )

    , ( "   \
        \-+-\
        \ | ", '┬' )

    , ( " | \
        \-+-\
        \   ", '┴' )

    , ( " | \
        \-+-\
        \ | ", '┼' )

    , ( "   \
        \ .-\
        \ | ", '╭' )

    , ( "   \
        \-. \
        \ | ", '╮' )

    , ( " | \
        \-' \
        \   ", '╯' )

    , ( " | \
        \ '-\
        \   ", '╰' )

    , ( "   \
        \ # \
        \ # ", '█' )

    , ( "   \
        \## \
        \   ", '█' )

    , ( "   \
        \ ##\
        \   ", '█' )

    , ( " # \
        \ # \
        \   ", '█' )

    , ( "   \
        \ - \
        \ # ", '▄' )

    , ( " # \
        \ - \
        \   ", '▀' )

    , ( "   \
        \ |#\
        \   ", '▐' )

    , ( "   \
        \#| \
        \   ", '▌' )

    , ( "   \
        \ +#\
        \ ##", '▟' )

    , ( "   \
        \-> \
        \   ", '▷' )

    , ( "   \
        \ <-\
        \   ", '◁' )

    , ( "   \
        \ ^ \
        \ | ", '△' )

    , ( " | \
        \ v \
        \   ", '▽' )
    ]

substituteChar :: [(Diagram, Char)] -> Plane Char -> Char
substituteChar mappings = \case
    Plane ( Zipper ((Zipper (a : as) b (c : cs)) : _)
                   (Zipper (d : ds) e (f : fs))
                   ((Zipper (g : gs) h (i : is)) : _) )
        -> fromMaybe e (lookupDiagram (fromString [a, b, c, d, e, f, g, h, i]) mappings)
    _ -> undefined
