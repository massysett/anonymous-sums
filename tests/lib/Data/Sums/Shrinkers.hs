module Data.Sums.Shrinkers where

import Data.Sums

s0 :: S0 -> [S0]
s0 _ = []

s1 :: (a -> [a]) -> S1 a -> [S1 a]
s1 f (S1a a) = map S1a $ f a

s2 :: (a -> [a]) -> (b -> [b]) -> S2 a b -> [S2 a b]
s2 fa fb = caseS2 (map S2a . fa) (map S2b . fb)

s3
  :: (a -> [a])
  -> (b -> [b])
  -> (c -> [c])
  -> S3 a b c
  -> [S3 a b c]
s3 fa fb fc = caseS3 (map S3a . fa) (map S3b . fb) (map S3c. fc)

s4
  :: (a -> [a])
  -> (b -> [b])
  -> (c -> [c])
  -> (d -> [d])
  -> S4 a b c d
  -> [S4 a b c d]
s4 fa fb fc fd = caseS4 (map S4a . fa) (map S4b . fb) (map S4c . fc)
   (map S4d . fd)

s5
  :: (a -> [a])
  -> (b -> [b])
  -> (c -> [c])
  -> (d -> [d])
  -> (e -> [e])
  -> S5 a b c d e
  -> [S5 a b c d e]
s5 fa fb fc fd fe = caseS5 (map S5a . fa) (map S5b . fb) (map S5c . fc)
   (map S5d . fd) (map S5e . fe)

s6
  :: (a -> [a])
  -> (b -> [b])
  -> (c -> [c])
  -> (d -> [d])
  -> (e -> [e])
  -> (f -> [f])
  -> S6 a b c d e f
  -> [S6 a b c d e f]
s6 fa fb fc fd fe ff
  = caseS6 (map S6a . fa) (map S6b . fb) (map S6c . fc)
   (map S6d . fd) (map S6e . fe) (map S6f . ff)

s7
  :: (a -> [a])
  -> (b -> [b])
  -> (c -> [c])
  -> (d -> [d])
  -> (e -> [e])
  -> (f -> [f])
  -> (g -> [g])
  -> S7 a b c d e f g
  -> [S7 a b c d e f g]
s7 fa fb fc fd fe ff fg
  = caseS7 (map S7a . fa) (map S7b . fb) (map S7c . fc)
   (map S7d . fd) (map S7e . fe) (map S7f . ff) (map S7g . fg)

s8
  :: (a -> [a])
  -> (b -> [b])
  -> (c -> [c])
  -> (d -> [d])
  -> (e -> [e])
  -> (f -> [f])
  -> (g -> [g])
  -> (h -> [h])
  -> S8 a b c d e f g h
  -> [S8 a b c d e f g h]
s8 fa fb fc fd fe ff fg fh
  = caseS8 (map S8a . fa) (map S8b . fb) (map S8c . fc)
   (map S8d . fd) (map S8e . fe) (map S8f . ff) (map S8g . fg)
   (map S8h . fh)


s9
  :: (a -> [a])
  -> (b -> [b])
  -> (c -> [c])
  -> (d -> [d])
  -> (e -> [e])
  -> (f -> [f])
  -> (g -> [g])
  -> (h -> [h])
  -> (i -> [i])
  -> S9 a b c d e f g h i
  -> [S9 a b c d e f g h i]
s9 fa fb fc fd fe ff fg fh fi
  = caseS9 (map S9a . fa) (map S9b . fb) (map S9c . fc)
   (map S9d . fd) (map S9e . fe) (map S9f . ff) (map S9g . fg)
   (map S9h . fh) (map S9i . fi)


s10
  :: (a -> [a])
  -> (b -> [b])
  -> (c -> [c])
  -> (d -> [d])
  -> (e -> [e])
  -> (f -> [f])
  -> (g -> [g])
  -> (h -> [h])
  -> (i -> [i])
  -> (j -> [j])
  -> S10 a b c d e f g h i j
  -> [S10 a b c d e f g h i j]
s10 fa fb fc fd fe ff fg fh fi fj
  = caseS10 (map S10a . fa) (map S10b . fb) (map S10c . fc)
   (map S10d . fd) (map S10e . fe) (map S10f . ff) (map S10g . fg)
   (map S10h . fh) (map S10i . fi) (map S10j . fj)


s11
  :: (a -> [a])
  -> (b -> [b])
  -> (c -> [c])
  -> (d -> [d])
  -> (e -> [e])
  -> (f -> [f])
  -> (g -> [g])
  -> (h -> [h])
  -> (i -> [i])
  -> (j -> [j])
  -> (k -> [k])
  -> S11 a b c d e f g h i j k
  -> [S11 a b c d e f g h i j k]
s11 fa fb fc fd fe ff fg fh fi fj fk
  = caseS11 (map S11a . fa) (map S11b . fb) (map S11c . fc)
   (map S11d . fd) (map S11e . fe) (map S11f . ff) (map S11g . fg)
   (map S11h . fh) (map S11i . fi) (map S11j . fj) (map S11k . fk)


s12
  :: (a -> [a])
  -> (b -> [b])
  -> (c -> [c])
  -> (d -> [d])
  -> (e -> [e])
  -> (f -> [f])
  -> (g -> [g])
  -> (h -> [h])
  -> (i -> [i])
  -> (j -> [j])
  -> (k -> [k])
  -> (l -> [l])
  -> S12 a b c d e f g h i j k l
  -> [S12 a b c d e f g h i j k l]
s12 fa fb fc fd fe ff fg fh fi fj fk fl
  = caseS12 (map S12a . fa) (map S12b . fb) (map S12c . fc)
   (map S12d . fd) (map S12e . fe) (map S12f . ff) (map S12g . fg)
   (map S12h . fh) (map S12i . fi) (map S12j . fj) (map S12k . fk)
   (map S12l . fl)


s13
  :: (a -> [a])
  -> (b -> [b])
  -> (c -> [c])
  -> (d -> [d])
  -> (e -> [e])
  -> (f -> [f])
  -> (g -> [g])
  -> (h -> [h])
  -> (i -> [i])
  -> (j -> [j])
  -> (k -> [k])
  -> (l -> [l])
  -> (m -> [m])
  -> S13 a b c d e f g h i j k l m
  -> [S13 a b c d e f g h i j k l m]
s13 fa fb fc fd fe ff fg fh fi fj fk fl fm
  = caseS13 (map S13a . fa) (map S13b . fb) (map S13c . fc)
   (map S13d . fd) (map S13e . fe) (map S13f . ff) (map S13g . fg)
   (map S13h . fh) (map S13i . fi) (map S13j . fj) (map S13k . fk)
   (map S13l . fl) (map S13m . fm)


s14
  :: (a -> [a])
  -> (b -> [b])
  -> (c -> [c])
  -> (d -> [d])
  -> (e -> [e])
  -> (f -> [f])
  -> (g -> [g])
  -> (h -> [h])
  -> (i -> [i])
  -> (j -> [j])
  -> (k -> [k])
  -> (l -> [l])
  -> (m -> [m])
  -> (n -> [n])
  -> S14 a b c d e f g h i j k l m n
  -> [S14 a b c d e f g h i j k l m n]
s14 fa fb fc fd fe ff fg fh fi fj fk fl fm fn
  = caseS14 (map S14a . fa) (map S14b . fb) (map S14c . fc)
   (map S14d . fd) (map S14e . fe) (map S14f . ff) (map S14g . fg)
   (map S14h . fh) (map S14i . fi) (map S14j . fj) (map S14k . fk)
   (map S14l . fl) (map S14m . fm) (map S14n . fn)


s15
  :: (a -> [a])
  -> (b -> [b])
  -> (c -> [c])
  -> (d -> [d])
  -> (e -> [e])
  -> (f -> [f])
  -> (g -> [g])
  -> (h -> [h])
  -> (i -> [i])
  -> (j -> [j])
  -> (k -> [k])
  -> (l -> [l])
  -> (m -> [m])
  -> (n -> [n])
  -> (o -> [o])
  -> S15 a b c d e f g h i j k l m n o
  -> [S15 a b c d e f g h i j k l m n o]
s15 fa fb fc fd fe ff fg fh fi fj fk fl fm fn fo
  = caseS15 (map S15a . fa) (map S15b . fb) (map S15c . fc)
   (map S15d . fd) (map S15e . fe) (map S15f . ff) (map S15g . fg)
   (map S15h . fh) (map S15i . fi) (map S15j . fj) (map S15k . fk)
   (map S15l . fl) (map S15m . fm) (map S15n . fn) (map S15o . fo)


