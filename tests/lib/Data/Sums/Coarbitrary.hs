-- Do not warn about the defaulting of variant function
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Data.Sums.Coarbitrary where

import Data.Sums
import Test.QuickCheck

s0 :: S0 -> Gen b -> Gen b
s0 _ = variant 0

s1
  :: (a -> Gen r -> Gen r)
  -> S1 a
  -> Gen r
  -> Gen r
s1 fa = caseS1 (\a -> variant 0 . fa a)

s2
  :: (a -> Gen r -> Gen r)
  -> (b -> Gen r -> Gen r)
  -> S2 a b
  -> Gen r
  -> Gen r
s2 fa fb = caseS2 (\a -> variant 0 . fa a)
                  (\b -> variant 1 . fb b)

s3
  :: (a -> Gen r -> Gen r)
  -> (b -> Gen r -> Gen r)
  -> (c -> Gen r -> Gen r)
  -> S3 a b c
  -> Gen r
  -> Gen r
s3 fa fb fc
  = caseS3 (\a -> variant 0 . fa a)
           (\b -> variant 1 . fb b)
           (\c -> variant 2 . fc c)

s4
  :: (a -> Gen r -> Gen r)
  -> (b -> Gen r -> Gen r)
  -> (c -> Gen r -> Gen r)
  -> (d -> Gen r -> Gen r)
  -> S4 a b c d
  -> Gen r
  -> Gen r
s4 fa fb fc fd
  = caseS4 (\a -> variant 0 . fa a)
           (\b -> variant 1 . fb b)
           (\c -> variant 2 . fc c)
           (\d -> variant 3 . fd d)

s5
  :: (a -> Gen r -> Gen r)
  -> (b -> Gen r -> Gen r)
  -> (c -> Gen r -> Gen r)
  -> (d -> Gen r -> Gen r)
  -> (e -> Gen r -> Gen r)
  -> S5 a b c d e
  -> Gen r
  -> Gen r
s5 fa fb fc fd fe
  = caseS5 (\a -> variant 0 . fa a)
           (\b -> variant 1 . fb b)
           (\c -> variant 2 . fc c)
           (\d -> variant 3 . fd d)
           (\e -> variant 4 . fe e)

s6
  :: (a -> Gen r -> Gen r)
  -> (b -> Gen r -> Gen r)
  -> (c -> Gen r -> Gen r)
  -> (d -> Gen r -> Gen r)
  -> (e -> Gen r -> Gen r)
  -> (f -> Gen r -> Gen r)
  -> S6 a b c d e f
  -> Gen r
  -> Gen r
s6 fa fb fc fd fe ff
  = caseS6 (\a -> variant 0 . fa a)
           (\b -> variant 1 . fb b)
           (\c -> variant 2 . fc c)
           (\d -> variant 3 . fd d)
           (\e -> variant 4 . fe e)
           (\f -> variant 5 . ff f)

s7
  :: (a -> Gen r -> Gen r)
  -> (b -> Gen r -> Gen r)
  -> (c -> Gen r -> Gen r)
  -> (d -> Gen r -> Gen r)
  -> (e -> Gen r -> Gen r)
  -> (f -> Gen r -> Gen r)
  -> (g -> Gen r -> Gen r)
  -> S7 a b c d e f g
  -> Gen r
  -> Gen r
s7 fa fb fc fd fe ff fg
  = caseS7 (\a -> variant 0 . fa a)
           (\b -> variant 1 . fb b)
           (\c -> variant 2 . fc c)
           (\d -> variant 3 . fd d)
           (\e -> variant 4 . fe e)
           (\f -> variant 5 . ff f)
           (\g -> variant 6 . fg g)

s8
  :: (a -> Gen r -> Gen r)
  -> (b -> Gen r -> Gen r)
  -> (c -> Gen r -> Gen r)
  -> (d -> Gen r -> Gen r)
  -> (e -> Gen r -> Gen r)
  -> (f -> Gen r -> Gen r)
  -> (g -> Gen r -> Gen r)
  -> (h -> Gen r -> Gen r)
  -> S8 a b c d e f g h
  -> Gen r
  -> Gen r
s8 fa fb fc fd fe ff fg fh
  = caseS8 (\a -> variant 0 . fa a)
           (\b -> variant 1 . fb b)
           (\c -> variant 2 . fc c)
           (\d -> variant 3 . fd d)
           (\e -> variant 4 . fe e)
           (\f -> variant 5 . ff f)
           (\g -> variant 6 . fg g)
           (\h -> variant 7 . fh h)

s9
  :: (a -> Gen r -> Gen r)
  -> (b -> Gen r -> Gen r)
  -> (c -> Gen r -> Gen r)
  -> (d -> Gen r -> Gen r)
  -> (e -> Gen r -> Gen r)
  -> (f -> Gen r -> Gen r)
  -> (g -> Gen r -> Gen r)
  -> (h -> Gen r -> Gen r)
  -> (i -> Gen r -> Gen r)
  -> S9 a b c d e f g h i
  -> Gen r
  -> Gen r
s9 fa fb fc fd fe ff fg fh fi
  = caseS9 (\a -> variant 0 . fa a)
           (\b -> variant 1 . fb b)
           (\c -> variant 2 . fc c)
           (\d -> variant 3 . fd d)
           (\e -> variant 4 . fe e)
           (\f -> variant 5 . ff f)
           (\g -> variant 6 . fg g)
           (\h -> variant 7 . fh h)
           (\i -> variant 8 . fi i)

s10
  :: (a -> Gen r -> Gen r)
  -> (b -> Gen r -> Gen r)
  -> (c -> Gen r -> Gen r)
  -> (d -> Gen r -> Gen r)
  -> (e -> Gen r -> Gen r)
  -> (f -> Gen r -> Gen r)
  -> (g -> Gen r -> Gen r)
  -> (h -> Gen r -> Gen r)
  -> (i -> Gen r -> Gen r)
  -> (j -> Gen r -> Gen r)
  -> S10 a b c d e f g h i j
  -> Gen r
  -> Gen r
s10 fa fb fc fd fe ff fg fh fi fj
  = caseS10 (\a -> variant 0 . fa a)
           (\b -> variant 1 . fb b)
           (\c -> variant 2 . fc c)
           (\d -> variant 3 . fd d)
           (\e -> variant 4 . fe e)
           (\f -> variant 5 . ff f)
           (\g -> variant 6 . fg g)
           (\h -> variant 7 . fh h)
           (\i -> variant 8 . fi i)
           (\j -> variant 9 . fj j)

s11
  :: (a -> Gen r -> Gen r)
  -> (b -> Gen r -> Gen r)
  -> (c -> Gen r -> Gen r)
  -> (d -> Gen r -> Gen r)
  -> (e -> Gen r -> Gen r)
  -> (f -> Gen r -> Gen r)
  -> (g -> Gen r -> Gen r)
  -> (h -> Gen r -> Gen r)
  -> (i -> Gen r -> Gen r)
  -> (j -> Gen r -> Gen r)
  -> (k -> Gen r -> Gen r)
  -> S11 a b c d e f g h i j k
  -> Gen r
  -> Gen r
s11 fa fb fc fd fe ff fg fh fi fj fk
  = caseS11 (\a -> variant 0 . fa a)
           (\b -> variant 1 . fb b)
           (\c -> variant 2 . fc c)
           (\d -> variant 3 . fd d)
           (\e -> variant 4 . fe e)
           (\f -> variant 5 . ff f)
           (\g -> variant 6 . fg g)
           (\h -> variant 7 . fh h)
           (\i -> variant 8 . fi i)
           (\j -> variant 9 . fj j)
           (\k -> variant 10 . fk k)

s12
  :: (a -> Gen r -> Gen r)
  -> (b -> Gen r -> Gen r)
  -> (c -> Gen r -> Gen r)
  -> (d -> Gen r -> Gen r)
  -> (e -> Gen r -> Gen r)
  -> (f -> Gen r -> Gen r)
  -> (g -> Gen r -> Gen r)
  -> (h -> Gen r -> Gen r)
  -> (i -> Gen r -> Gen r)
  -> (j -> Gen r -> Gen r)
  -> (k -> Gen r -> Gen r)
  -> (l -> Gen r -> Gen r)
  -> S12 a b c d e f g h i j k l
  -> Gen r
  -> Gen r
s12 fa fb fc fd fe ff fg fh fi fj fk fl
  = caseS12 (\a -> variant 0 . fa a)
           (\b -> variant 1 . fb b)
           (\c -> variant 2 . fc c)
           (\d -> variant 3 . fd d)
           (\e -> variant 4 . fe e)
           (\f -> variant 5 . ff f)
           (\g -> variant 6 . fg g)
           (\h -> variant 7 . fh h)
           (\i -> variant 8 . fi i)
           (\j -> variant 9 . fj j)
           (\k -> variant 10 . fk k)
           (\l -> variant 11 . fl l)

s13
  :: (a -> Gen r -> Gen r)
  -> (b -> Gen r -> Gen r)
  -> (c -> Gen r -> Gen r)
  -> (d -> Gen r -> Gen r)
  -> (e -> Gen r -> Gen r)
  -> (f -> Gen r -> Gen r)
  -> (g -> Gen r -> Gen r)
  -> (h -> Gen r -> Gen r)
  -> (i -> Gen r -> Gen r)
  -> (j -> Gen r -> Gen r)
  -> (k -> Gen r -> Gen r)
  -> (l -> Gen r -> Gen r)
  -> (m -> Gen r -> Gen r)
  -> S13 a b c d e f g h i j k l m
  -> Gen r
  -> Gen r
s13 fa fb fc fd fe ff fg fh fi fj fk fl fm
  = caseS13 (\a -> variant 0 . fa a)
           (\b -> variant 1 . fb b)
           (\c -> variant 2 . fc c)
           (\d -> variant 3 . fd d)
           (\e -> variant 4 . fe e)
           (\f -> variant 5 . ff f)
           (\g -> variant 6 . fg g)
           (\h -> variant 7 . fh h)
           (\i -> variant 8 . fi i)
           (\j -> variant 9 . fj j)
           (\k -> variant 10 . fk k)
           (\l -> variant 11 . fl l)
           (\m -> variant 12 . fm m)

s14
  :: (a -> Gen r -> Gen r)
  -> (b -> Gen r -> Gen r)
  -> (c -> Gen r -> Gen r)
  -> (d -> Gen r -> Gen r)
  -> (e -> Gen r -> Gen r)
  -> (f -> Gen r -> Gen r)
  -> (g -> Gen r -> Gen r)
  -> (h -> Gen r -> Gen r)
  -> (i -> Gen r -> Gen r)
  -> (j -> Gen r -> Gen r)
  -> (k -> Gen r -> Gen r)
  -> (l -> Gen r -> Gen r)
  -> (m -> Gen r -> Gen r)
  -> (n -> Gen r -> Gen r)
  -> S14 a b c d e f g h i j k l m n
  -> Gen r
  -> Gen r
s14 fa fb fc fd fe ff fg fh fi fj fk fl fm fn
  = caseS14 (\a -> variant 0 . fa a)
           (\b -> variant 1 . fb b)
           (\c -> variant 2 . fc c)
           (\d -> variant 3 . fd d)
           (\e -> variant 4 . fe e)
           (\f -> variant 5 . ff f)
           (\g -> variant 6 . fg g)
           (\h -> variant 7 . fh h)
           (\i -> variant 8 . fi i)
           (\j -> variant 9 . fj j)
           (\k -> variant 10 . fk k)
           (\l -> variant 11 . fl l)
           (\m -> variant 12 . fm m)
           (\n -> variant 12 . fn n)

s15
  :: (a -> Gen r -> Gen r)
  -> (b -> Gen r -> Gen r)
  -> (c -> Gen r -> Gen r)
  -> (d -> Gen r -> Gen r)
  -> (e -> Gen r -> Gen r)
  -> (f -> Gen r -> Gen r)
  -> (g -> Gen r -> Gen r)
  -> (h -> Gen r -> Gen r)
  -> (i -> Gen r -> Gen r)
  -> (j -> Gen r -> Gen r)
  -> (k -> Gen r -> Gen r)
  -> (l -> Gen r -> Gen r)
  -> (m -> Gen r -> Gen r)
  -> (n -> Gen r -> Gen r)
  -> (o -> Gen r -> Gen r)
  -> S15 a b c d e f g h i j k l m n o
  -> Gen r
  -> Gen r
s15 fa fb fc fd fe ff fg fh fi fj fk fl fm fn fo
  = caseS15 (\a -> variant 0 . fa a)
           (\b -> variant 1 . fb b)
           (\c -> variant 2 . fc c)
           (\d -> variant 3 . fd d)
           (\e -> variant 4 . fe e)
           (\f -> variant 5 . ff f)
           (\g -> variant 6 . fg g)
           (\h -> variant 7 . fh h)
           (\i -> variant 8 . fi i)
           (\j -> variant 9 . fj j)
           (\k -> variant 10 . fk k)
           (\l -> variant 11 . fl l)
           (\m -> variant 12 . fm m)
           (\n -> variant 12 . fn n)
           (\o -> variant 12 . fo o)

