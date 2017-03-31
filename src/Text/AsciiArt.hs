{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

module Text.AsciiArt where

import           Control.Comonad
import           Data.Maybe



--------------------------------------------------------------------------------
-- * Zipper
--------------------------------------------------------------------------------

-- | The 'Zipper' is assumed to be infinite, i.e. filled with empty values
-- outside the defined area.
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

-- | Renders the 'current' and the @n - 1@ elements 'after' as list.
zipperToList :: Int -> Zipper a -> [a]
zipperToList n Zipper{..} = current : take (n - 1) after

-- | An infinite 'Zipper' filled with @a@s.
zipperOf :: a -> Zipper a
zipperOf a = Zipper { before = repeat a, current = a, after = repeat a }

-- | Takes a list and creates a 'Zipper' from it. The 'current' element will be
-- the 'head' of the list, and 'after' that 'tail'. The rest will be filled with
-- @a@s to an infinite 'Zipper'.
zipperFromList :: a -> [a] -> Zipper a
zipperFromList a = \case
    []     -> zipperOf a
    b : bs -> (zipperOf a) { current = b, after = bs ++ repeat a }


instance Comonad Zipper where
    extract = current
    extend f zipper = fmap f $ Zipper
        { before  = iterate1 moveBefore zipper
        , current = zipper
        , after   = iterate1 moveAfter zipper }
      where
        iterate1 f x = tail (iterate f x)



--------------------------------------------------------------------------------
-- * Plane (two-dimensional 'Zipper')
--------------------------------------------------------------------------------

-- | A plane is a 'Zipper' of 'Zipper's. The outer layer zips through lines
-- (up/down), the inner layer through columns (left/right).
-- Like the 'Zipper', the 'Plane' is assumed to be infinite in all directions.
newtype Plane a = Plane { unPlane :: Zipper (Zipper a) }
    deriving (Functor)

moveLeft, moveRight, moveUp, moveDown :: Plane a -> Plane a
moveLeft  = Plane . fmap moveBefore . unPlane
moveRight = Plane . fmap moveAfter  . unPlane
moveUp    = Plane . moveBefore      . unPlane
moveDown  = Plane . moveAfter       . unPlane


-- | Renders @m@ lines and @n@ columns as nested list.
planeToList :: Int -> Int -> Plane a -> [[a]]
planeToList m n (Plane Zipper{..}) = fmap (zipperToList n) $
    current : take (m - 1) after

-- | An infinite 'Plane' filled with @a@s.
planeOf :: a -> Plane a
planeOf a = Plane $ Zipper
    { before  = repeat (zipperOf a)
    , current = zipperOf a
    , after   = repeat (zipperOf a) }

-- | Create a 'Plane' from a list of lists, filling the rest with @a@s in all
-- directions.
planeFromList :: a -> [[a]] -> Plane a
planeFromList a = \case
    []       -> planeOf a
    as : ass -> Plane $ (zipperOf (zipperOf a))
        { current = zipperFromList a as
        , after = fmap (zipperFromList a) ass ++ repeat (zipperOf a) }


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



--------------------------------------------------------------------------------
-- * Patterns
--------------------------------------------------------------------------------

newtype Pattern = Pattern ((Char, Char, Char), (Char, Char, Char), (Char, Char, Char))


patternFromString :: String -> Pattern
patternFromString [a, b, c, d, e, f, g, h, i] = Pattern ((a, b, c), (d, e, f), (g, h, i))
patternFromString _ = undefined

patternToString :: Pattern -> String
patternToString (Pattern ((a, b, c), (d, e, f), (g, h, i))) = [a, b, c, d, e, f, g, h, i]

-- | Find the 'Char' to replace the center of a 'Pattern'.
lookupPattern :: Pattern -> Maybe Char
lookupPattern pattern = case filter (satisfies pattern) patterns of
    []    -> Nothing
    a : _ -> Just (snd a)
  where
    satisfies :: Pattern -> (Pattern, Char) -> Bool
    satisfies diagram@(Pattern(_, (_, a1, _), _)) (pattern@(Pattern(_, (_, a2, _), _)), _)
        = a1 `connectsLike` a2
        && (and $ zipWith connectsLike (patternToString diagram) (patternToString pattern))

-- | Whether a character can connect to another character. For example, @+@
-- connects both horizontally (like @-@) and vertically (like @|@), so it
-- 'connectsLike' @-@, @|@, and of course like itself.
connectsLike :: Char -> Char -> Bool
char `connectsLike` pattern = case pattern of
    '-'   -> char `elem` ['-', '>', '<', '─'] || char `connectsLike` '+'
    '|'   -> char `elem` ['|', '^', 'v', '│'] || char `connectsLike` '+'
    '+'   -> char `elem` [ '+'
                         , '└', '┘', '┌', '┐'
                         , '├', '┤', '┬', '┴', '┼' ]
                         || char `connectsLike` '.'
    '.'   -> char `elem` [ '\'', '.'
                         , '╭', '╮', '╯', '╰' ]
    '\''  -> char `connectsLike` '.'
    ' '   -> True
    other -> char == other

-- | The actual pattern definitions. For convenience, the simple patterns are at
-- the top, and more complex ones at the bottom. 'lookupPattern' will first try
-- the most complex pattern and work its way to the simpler patterns, thus
-- avoiding to choose a simpler pattern and forgetting some connection.
patterns :: [(Pattern, Char)]
patterns = reverse $ fmap (\(a, b) -> (patternFromString a, b))
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



--------------------------------------------------------------------------------
-- * Transforming ASCII to Unicode
--------------------------------------------------------------------------------

-- | Match the 'current' element and its eight neighbours against the defined
-- 'patterns' and choose the 'Char' from the matching 'Pattern'.
substituteChar :: Plane Char -> Char
substituteChar = \case
    Plane ( Zipper ((Zipper (a : as) b (c : cs)) : _)
                   ( Zipper (d : ds) e (f : fs))
                   ((Zipper (g : gs) h (i : is)) : _) )
        -> fromMaybe e (lookupPattern (patternFromString [a, b, c, d, e, f, g, h, i]))
    _ -> undefined -- We assume an infinite Zipper!

-- | Transform a 'Plane' of ASCII characters to an equivalent plane where the
-- ASCII box drawings have been replaced by their Unicode counterpart.
--
-- This function is a convolution with 'substituteChar' using the 'Comonad'ic
-- 'extend'.
renderAsciiToUnicode :: Plane Char -> Plane Char
renderAsciiToUnicode = extend substituteChar
