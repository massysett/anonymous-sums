module Data.Sums.Generators where

import Data.Sums
import Test.QuickCheck

s2
  :: (Int, Gen a)
  -> (Int, Gen b)
  -> Gen (S2 a b)
s2 (fa, ga) (fb, gb) = frequency [(fa, fmap S2a ga), (fb, fmap S2b gb)]

s3
  :: (Int, Gen a)
  -> (Int, Gen b)
  -> (Int, Gen c)
  -> Gen (S3 a b c)
s3 (fa, ga) (fb, gb) (fc, gc)
  = frequency [(fa, fmap S3a ga), (fb, fmap S3b gb), (fc, fmap S3c gc)]

s4
  :: (Int, Gen a)
  -> (Int, Gen b)
  -> (Int, Gen c)
  -> (Int, Gen d)
  -> Gen (S4 a b c d)
s4 (fa, ga) (fb, gb) (fc, gc) (fd, gd)
  = frequency [ (fa, fmap S4a ga), (fb, fmap S4b gb), (fc, fmap S4c gc)
              , (fd, fmap S4d gd) ]
s5
  :: (Int, Gen a)
  -> (Int, Gen b)
  -> (Int, Gen c)
  -> (Int, Gen d)
  -> (Int, Gen e)
  -> Gen (S5 a b c d e)
s5 (fa, ga) (fb, gb) (fc, gc) (fd, gd) (fe, ge)
  = frequency [ (fa, fmap S5a ga), (fb, fmap S5b gb), (fc, fmap S5c gc)
              , (fd, fmap S5d gd), (fe, fmap S5e ge) ]

s6
  :: (Int, Gen a)
  -> (Int, Gen b)
  -> (Int, Gen c)
  -> (Int, Gen d)
  -> (Int, Gen e)
  -> (Int, Gen f)
  -> Gen (S6 a b c d e f)
s6 (fa, ga) (fb, gb) (fc, gc) (fd, gd) (fe, ge) (ff, gf)
  = frequency [ (fa, fmap S6a ga), (fb, fmap S6b gb), (fc, fmap S6c gc)
              , (fd, fmap S6d gd), (fe, fmap S6e ge), (ff, fmap S6f gf) ]


s7
  :: (Int, Gen a)
  -> (Int, Gen b)
  -> (Int, Gen c)
  -> (Int, Gen d)
  -> (Int, Gen e)
  -> (Int, Gen f)
  -> (Int, Gen g)
  -> Gen (S7 a b c d e f g)
s7 (fa, ga) (fb, gb) (fc, gc) (fd, gd) (fe, ge) (ff, gf) (fg, gg)
  = frequency [ (fa, fmap S7a ga), (fb, fmap S7b gb), (fc, fmap S7c gc)
              , (fd, fmap S7d gd), (fe, fmap S7e ge), (ff, fmap S7f gf)
              , (fg, fmap S7g gg) ]

s8
  :: (Int, Gen a)
  -> (Int, Gen b)
  -> (Int, Gen c)
  -> (Int, Gen d)
  -> (Int, Gen e)
  -> (Int, Gen f)
  -> (Int, Gen g)
  -> (Int, Gen h)
  -> Gen (S8 a b c d e f g h)
s8 (fa, ga) (fb, gb) (fc, gc) (fd, gd) (fe, ge) (ff, gf) (fg, gg) (fh, gh)
  = frequency [ (fa, fmap S8a ga), (fb, fmap S8b gb), (fc, fmap S8c gc)
              , (fd, fmap S8d gd), (fe, fmap S8e ge), (ff, fmap S8f gf)
              , (fg, fmap S8g gg), (fh, fmap S8h gh) ]

s9
  :: (Int, Gen a)
  -> (Int, Gen b)
  -> (Int, Gen c)
  -> (Int, Gen d)
  -> (Int, Gen e)
  -> (Int, Gen f)
  -> (Int, Gen g)
  -> (Int, Gen h)
  -> (Int, Gen i)
  -> Gen (S9 a b c d e f g h i)
s9 (fa, ga) (fb, gb) (fc, gc) (fd, gd) (fe, ge) (ff, gf) (fg, gg) (fh, gh)
   (fi, gi)
  = frequency [ (fa, fmap S9a ga), (fb, fmap S9b gb), (fc, fmap S9c gc)
              , (fd, fmap S9d gd), (fe, fmap S9e ge), (ff, fmap S9f gf)
              , (fg, fmap S9g gg), (fh, fmap S9h gh), (fi, fmap S9i gi) ]

s10
  :: (Int, Gen a)
  -> (Int, Gen b)
  -> (Int, Gen c)
  -> (Int, Gen d)
  -> (Int, Gen e)
  -> (Int, Gen f)
  -> (Int, Gen g)
  -> (Int, Gen h)
  -> (Int, Gen i)
  -> (Int, Gen j)
  -> Gen (S10 a b c d e f g h i j)
s10 (fa, ga) (fb, gb) (fc, gc) (fd, gd) (fe, ge) (ff, gf) (fg, gg) (fh, gh)
   (fi, gi) (fj, gj)
  = frequency [ (fa, fmap S10a ga), (fb, fmap S10b gb), (fc, fmap S10c gc)
              , (fd, fmap S10d gd), (fe, fmap S10e ge), (ff, fmap S10f gf)
              , (fg, fmap S10g gg), (fh, fmap S10h gh), (fi, fmap S10i gi)
              , (fj, fmap S10j gj) ]
s11
  :: (Int, Gen a)
  -> (Int, Gen b)
  -> (Int, Gen c)
  -> (Int, Gen d)
  -> (Int, Gen e)
  -> (Int, Gen f)
  -> (Int, Gen g)
  -> (Int, Gen h)
  -> (Int, Gen i)
  -> (Int, Gen j)
  -> (Int, Gen k)
  -> Gen (S11 a b c d e f g h i j k)
s11 (fa, ga) (fb, gb) (fc, gc) (fd, gd) (fe, ge) (ff, gf) (fg, gg) (fh, gh)
   (fi, gi) (fj, gj) (fk, gk)
  = frequency [ (fa, fmap S11a ga), (fb, fmap S11b gb), (fc, fmap S11c gc)
              , (fd, fmap S11d gd), (fe, fmap S11e ge), (ff, fmap S11f gf)
              , (fg, fmap S11g gg), (fh, fmap S11h gh), (fi, fmap S11i gi)
              , (fj, fmap S11j gj), (fk, fmap S11k gk) ]

s12
  :: (Int, Gen a)
  -> (Int, Gen b)
  -> (Int, Gen c)
  -> (Int, Gen d)
  -> (Int, Gen e)
  -> (Int, Gen f)
  -> (Int, Gen g)
  -> (Int, Gen h)
  -> (Int, Gen i)
  -> (Int, Gen j)
  -> (Int, Gen k)
  -> (Int, Gen l)
  -> Gen (S12 a b c d e f g h i j k l)
s12 (fa, ga) (fb, gb) (fc, gc) (fd, gd) (fe, ge) (ff, gf) (fg, gg) (fh, gh)
   (fi, gi) (fj, gj) (fk, gk) (fl, gl)
  = frequency [ (fa, fmap S12a ga), (fb, fmap S12b gb), (fc, fmap S12c gc)
              , (fd, fmap S12d gd), (fe, fmap S12e ge), (ff, fmap S12f gf)
              , (fg, fmap S12g gg), (fh, fmap S12h gh), (fi, fmap S12i gi)
              , (fj, fmap S12j gj), (fk, fmap S12k gk), (fl, fmap S12l gl) ]

s13
  :: (Int, Gen a)
  -> (Int, Gen b)
  -> (Int, Gen c)
  -> (Int, Gen d)
  -> (Int, Gen e)
  -> (Int, Gen f)
  -> (Int, Gen g)
  -> (Int, Gen h)
  -> (Int, Gen i)
  -> (Int, Gen j)
  -> (Int, Gen k)
  -> (Int, Gen l)
  -> (Int, Gen m)
  -> Gen (S13 a b c d e f g h i j k l m)
s13 (fa, ga) (fb, gb) (fc, gc) (fd, gd) (fe, ge) (ff, gf) (fg, gg) (fh, gh)
   (fi, gi) (fj, gj) (fk, gk) (fl, gl) (fm, gm)
  = frequency [ (fa, fmap S13a ga), (fb, fmap S13b gb), (fc, fmap S13c gc)
              , (fd, fmap S13d gd), (fe, fmap S13e ge), (ff, fmap S13f gf)
              , (fg, fmap S13g gg), (fh, fmap S13h gh), (fi, fmap S13i gi)
              , (fj, fmap S13j gj), (fk, fmap S13k gk), (fl, fmap S13l gl)
              , (fm, fmap S13m gm) ]

s14
  :: (Int, Gen a)
  -> (Int, Gen b)
  -> (Int, Gen c)
  -> (Int, Gen d)
  -> (Int, Gen e)
  -> (Int, Gen f)
  -> (Int, Gen g)
  -> (Int, Gen h)
  -> (Int, Gen i)
  -> (Int, Gen j)
  -> (Int, Gen k)
  -> (Int, Gen l)
  -> (Int, Gen m)
  -> (Int, Gen n)
  -> Gen (S14 a b c d e f g h i j k l m n)
s14 (fa, ga) (fb, gb) (fc, gc) (fd, gd) (fe, ge) (ff, gf) (fg, gg) (fh, gh)
   (fi, gi) (fj, gj) (fk, gk) (fl, gl) (fm, gm) (fn, gn)
  = frequency [ (fa, fmap S14a ga), (fb, fmap S14b gb), (fc, fmap S14c gc)
              , (fd, fmap S14d gd), (fe, fmap S14e ge), (ff, fmap S14f gf)
              , (fg, fmap S14g gg), (fh, fmap S14h gh), (fi, fmap S14i gi)
              , (fj, fmap S14j gj), (fk, fmap S14k gk), (fl, fmap S14l gl)
              , (fm, fmap S14m gm), (fn, fmap S14n gn) ]

s15
  :: (Int, Gen a)
  -> (Int, Gen b)
  -> (Int, Gen c)
  -> (Int, Gen d)
  -> (Int, Gen e)
  -> (Int, Gen f)
  -> (Int, Gen g)
  -> (Int, Gen h)
  -> (Int, Gen i)
  -> (Int, Gen j)
  -> (Int, Gen k)
  -> (Int, Gen l)
  -> (Int, Gen m)
  -> (Int, Gen n)
  -> (Int, Gen o)
  -> Gen (S15 a b c d e f g h i j k l m n o)
s15 (fa, ga) (fb, gb) (fc, gc) (fd, gd) (fe, ge) (ff, gf) (fg, gg) (fh, gh)
   (fi, gi) (fj, gj) (fk, gk) (fl, gl) (fm, gm) (fn, gn) (fo, go)
  = frequency [ (fa, fmap S15a ga), (fb, fmap S15b gb), (fc, fmap S15c gc)
              , (fd, fmap S15d gd), (fe, fmap S15e ge), (ff, fmap S15f gf)
              , (fg, fmap S15g gg), (fh, fmap S15h gh), (fi, fmap S15i gi)
              , (fj, fmap S15j gj), (fk, fmap S15k gk), (fl, fmap S15l gl)
              , (fm, fmap S15m gm), (fn, fmap S15n gn), (fo, fmap S15o go) ]

