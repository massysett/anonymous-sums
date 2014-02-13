{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}

-- Text of this module generated by the generate-sums.hs
-- script, included with the source of the anonymous-sums
-- package.

-- | Anonymous sum types.
--
-- Provides functionality similar to that of tuples, but
-- for sum types rather than product types.  Less clumsy
-- than using nested 'Either'.

module Data.Sums where {
import Data.Typeable;
import GHC.Generics;

-- * Anonymous sum types

data S2 a b = S2a a | S2b b deriving (Eq, Ord, Read, Show, Generic, Typeable);

data S3 a b c = S3a a | S3b b | S3c c deriving (Eq, Ord, Read, Show, Generic, Typeable);

data S4 a b c d = S4a a | S4b b | S4c c | S4d d deriving (Eq, Ord, Read, Show, Generic, Typeable);

data S5 a b c d e = S5a a | S5b b | S5c c | S5d d | S5e e deriving (Eq, Ord, Read, Show, Generic, Typeable);

data S6 a b c d e f = S6a a | S6b b | S6c c | S6d d | S6e e | S6f f deriving (Eq, Ord, Read, Show, Generic, Typeable);

data S7 a b c d e f g = S7a a | S7b b | S7c c | S7d d | S7e e | S7f f | S7g g deriving (Eq, Ord, Read, Show, Generic, Typeable);

data S8 a b c d e f g h = S8a a | S8b b | S8c c | S8d d | S8e e | S8f f | S8g g | S8h h deriving (Eq, Ord, Read, Show, Generic);

data S9 a b c d e f g h i = S9a a | S9b b | S9c c | S9d d | S9e e | S9f f | S9g g | S9h h | S9i i deriving (Eq, Ord, Read, Show, Generic);

data S10 a b c d e f g h i j = S10a a | S10b b | S10c c | S10d d | S10e e | S10f f | S10g g | S10h h | S10i i | S10j j deriving (Eq, Ord, Read, Show, Generic);

data S11 a b c d e f g h i j k = S11a a | S11b b | S11c c | S11d d | S11e e | S11f f | S11g g | S11h h | S11i i | S11j j | S11k k deriving (Eq, Ord, Read, Show, Generic);

data S12 a b c d e f g h i j k l = S12a a | S12b b | S12c c | S12d d | S12e e | S12f f | S12g g | S12h h | S12i i | S12j j | S12k k | S12l l deriving (Eq, Ord, Read, Show, Generic);

data S13 a b c d e f g h i j k l m = S13a a | S13b b | S13c c | S13d d | S13e e | S13f f | S13g g | S13h h | S13i i | S13j j | S13k k | S13l l | S13m m deriving (Eq, Ord, Read, Show, Generic);

data S14 a b c d e f g h i j k l m n = S14a a | S14b b | S14c c | S14d d | S14e e | S14f f | S14g g | S14h h | S14i i | S14j j | S14k k | S14l l | S14m m | S14n n deriving (Eq, Ord, Read, Show, Generic);

data S15 a b c d e f g h i j k l m n o = S15a a | S15b b | S15c c | S15d d | S15e e | S15f f | S15g g | S15h h | S15i i | S15j j | S15k k | S15l l | S15m m | S15n n | S15o o deriving (Eq, Ord, Read, Show, Generic);

-- * Partitioning

partitionS2 :: [S2 a b] -> ([a], [b]);
partitionS2 = foldr fn ([], [])
where { fn it (as, bs) = case it of { S2a a -> (a:as, bs);
S2b b -> (as, b:bs);
};};

partitionS3 :: [S3 a b c] -> ([a], [b], [c]);
partitionS3 = foldr fn ([], [], [])
where { fn it (as, bs, cs) = case it of { S3a a -> (a:as, bs, cs);
S3b b -> (as, b:bs, cs);
S3c c -> (as, bs, c:cs);
};};

partitionS4 :: [S4 a b c d] -> ([a], [b], [c], [d]);
partitionS4 = foldr fn ([], [], [], [])
where { fn it (as, bs, cs, ds) = case it of { S4a a -> (a:as, bs, cs, ds);
S4b b -> (as, b:bs, cs, ds);
S4c c -> (as, bs, c:cs, ds);
S4d d -> (as, bs, cs, d:ds);
};};

partitionS5 :: [S5 a b c d e] -> ([a], [b], [c], [d], [e]);
partitionS5 = foldr fn ([], [], [], [], [])
where { fn it (as, bs, cs, ds, es) = case it of { S5a a -> (a:as, bs, cs, ds, es);
S5b b -> (as, b:bs, cs, ds, es);
S5c c -> (as, bs, c:cs, ds, es);
S5d d -> (as, bs, cs, d:ds, es);
S5e e -> (as, bs, cs, ds, e:es);
};};

partitionS6 :: [S6 a b c d e f] -> ([a], [b], [c], [d], [e], [f]);
partitionS6 = foldr fn ([], [], [], [], [], [])
where { fn it (as, bs, cs, ds, es, fs) = case it of { S6a a -> (a:as, bs, cs, ds, es, fs);
S6b b -> (as, b:bs, cs, ds, es, fs);
S6c c -> (as, bs, c:cs, ds, es, fs);
S6d d -> (as, bs, cs, d:ds, es, fs);
S6e e -> (as, bs, cs, ds, e:es, fs);
S6f f -> (as, bs, cs, ds, es, f:fs);
};};

partitionS7 :: [S7 a b c d e f g] -> ([a], [b], [c], [d], [e], [f], [g]);
partitionS7 = foldr fn ([], [], [], [], [], [], [])
where { fn it (as, bs, cs, ds, es, fs, gs) = case it of { S7a a -> (a:as, bs, cs, ds, es, fs, gs);
S7b b -> (as, b:bs, cs, ds, es, fs, gs);
S7c c -> (as, bs, c:cs, ds, es, fs, gs);
S7d d -> (as, bs, cs, d:ds, es, fs, gs);
S7e e -> (as, bs, cs, ds, e:es, fs, gs);
S7f f -> (as, bs, cs, ds, es, f:fs, gs);
S7g g -> (as, bs, cs, ds, es, fs, g:gs);
};};

partitionS8 :: [S8 a b c d e f g h] -> ([a], [b], [c], [d], [e], [f], [g], [h]);
partitionS8 = foldr fn ([], [], [], [], [], [], [], [])
where { fn it (as, bs, cs, ds, es, fs, gs, hs) = case it of { S8a a -> (a:as, bs, cs, ds, es, fs, gs, hs);
S8b b -> (as, b:bs, cs, ds, es, fs, gs, hs);
S8c c -> (as, bs, c:cs, ds, es, fs, gs, hs);
S8d d -> (as, bs, cs, d:ds, es, fs, gs, hs);
S8e e -> (as, bs, cs, ds, e:es, fs, gs, hs);
S8f f -> (as, bs, cs, ds, es, f:fs, gs, hs);
S8g g -> (as, bs, cs, ds, es, fs, g:gs, hs);
S8h h -> (as, bs, cs, ds, es, fs, gs, h:hs);
};};

partitionS9 :: [S9 a b c d e f g h i] -> ([a], [b], [c], [d], [e], [f], [g], [h], [i]);
partitionS9 = foldr fn ([], [], [], [], [], [], [], [], [])
where { fn it (as, bs, cs, ds, es, fs, gs, hs, is) = case it of { S9a a -> (a:as, bs, cs, ds, es, fs, gs, hs, is);
S9b b -> (as, b:bs, cs, ds, es, fs, gs, hs, is);
S9c c -> (as, bs, c:cs, ds, es, fs, gs, hs, is);
S9d d -> (as, bs, cs, d:ds, es, fs, gs, hs, is);
S9e e -> (as, bs, cs, ds, e:es, fs, gs, hs, is);
S9f f -> (as, bs, cs, ds, es, f:fs, gs, hs, is);
S9g g -> (as, bs, cs, ds, es, fs, g:gs, hs, is);
S9h h -> (as, bs, cs, ds, es, fs, gs, h:hs, is);
S9i i -> (as, bs, cs, ds, es, fs, gs, hs, i:is);
};};

partitionS10 :: [S10 a b c d e f g h i j] -> ([a], [b], [c], [d], [e], [f], [g], [h], [i], [j]);
partitionS10 = foldr fn ([], [], [], [], [], [], [], [], [], [])
where { fn it (as, bs, cs, ds, es, fs, gs, hs, is, js) = case it of { S10a a -> (a:as, bs, cs, ds, es, fs, gs, hs, is, js);
S10b b -> (as, b:bs, cs, ds, es, fs, gs, hs, is, js);
S10c c -> (as, bs, c:cs, ds, es, fs, gs, hs, is, js);
S10d d -> (as, bs, cs, d:ds, es, fs, gs, hs, is, js);
S10e e -> (as, bs, cs, ds, e:es, fs, gs, hs, is, js);
S10f f -> (as, bs, cs, ds, es, f:fs, gs, hs, is, js);
S10g g -> (as, bs, cs, ds, es, fs, g:gs, hs, is, js);
S10h h -> (as, bs, cs, ds, es, fs, gs, h:hs, is, js);
S10i i -> (as, bs, cs, ds, es, fs, gs, hs, i:is, js);
S10j j -> (as, bs, cs, ds, es, fs, gs, hs, is, j:js);
};};

partitionS11 :: [S11 a b c d e f g h i j k] -> ([a], [b], [c], [d], [e], [f], [g], [h], [i], [j], [k]);
partitionS11 = foldr fn ([], [], [], [], [], [], [], [], [], [], [])
where { fn it (as, bs, cs, ds, es, fs, gs, hs, is, js, ks) = case it of { S11a a -> (a:as, bs, cs, ds, es, fs, gs, hs, is, js, ks);
S11b b -> (as, b:bs, cs, ds, es, fs, gs, hs, is, js, ks);
S11c c -> (as, bs, c:cs, ds, es, fs, gs, hs, is, js, ks);
S11d d -> (as, bs, cs, d:ds, es, fs, gs, hs, is, js, ks);
S11e e -> (as, bs, cs, ds, e:es, fs, gs, hs, is, js, ks);
S11f f -> (as, bs, cs, ds, es, f:fs, gs, hs, is, js, ks);
S11g g -> (as, bs, cs, ds, es, fs, g:gs, hs, is, js, ks);
S11h h -> (as, bs, cs, ds, es, fs, gs, h:hs, is, js, ks);
S11i i -> (as, bs, cs, ds, es, fs, gs, hs, i:is, js, ks);
S11j j -> (as, bs, cs, ds, es, fs, gs, hs, is, j:js, ks);
S11k k -> (as, bs, cs, ds, es, fs, gs, hs, is, js, k:ks);
};};

partitionS12 :: [S12 a b c d e f g h i j k l] -> ([a], [b], [c], [d], [e], [f], [g], [h], [i], [j], [k], [l]);
partitionS12 = foldr fn ([], [], [], [], [], [], [], [], [], [], [], [])
where { fn it (as, bs, cs, ds, es, fs, gs, hs, is, js, ks, ls) = case it of { S12a a -> (a:as, bs, cs, ds, es, fs, gs, hs, is, js, ks, ls);
S12b b -> (as, b:bs, cs, ds, es, fs, gs, hs, is, js, ks, ls);
S12c c -> (as, bs, c:cs, ds, es, fs, gs, hs, is, js, ks, ls);
S12d d -> (as, bs, cs, d:ds, es, fs, gs, hs, is, js, ks, ls);
S12e e -> (as, bs, cs, ds, e:es, fs, gs, hs, is, js, ks, ls);
S12f f -> (as, bs, cs, ds, es, f:fs, gs, hs, is, js, ks, ls);
S12g g -> (as, bs, cs, ds, es, fs, g:gs, hs, is, js, ks, ls);
S12h h -> (as, bs, cs, ds, es, fs, gs, h:hs, is, js, ks, ls);
S12i i -> (as, bs, cs, ds, es, fs, gs, hs, i:is, js, ks, ls);
S12j j -> (as, bs, cs, ds, es, fs, gs, hs, is, j:js, ks, ls);
S12k k -> (as, bs, cs, ds, es, fs, gs, hs, is, js, k:ks, ls);
S12l l -> (as, bs, cs, ds, es, fs, gs, hs, is, js, ks, l:ls);
};};

partitionS13 :: [S13 a b c d e f g h i j k l m] -> ([a], [b], [c], [d], [e], [f], [g], [h], [i], [j], [k], [l], [m]);
partitionS13 = foldr fn ([], [], [], [], [], [], [], [], [], [], [], [], [])
where { fn it (as, bs, cs, ds, es, fs, gs, hs, is, js, ks, ls, ms) = case it of { S13a a -> (a:as, bs, cs, ds, es, fs, gs, hs, is, js, ks, ls, ms);
S13b b -> (as, b:bs, cs, ds, es, fs, gs, hs, is, js, ks, ls, ms);
S13c c -> (as, bs, c:cs, ds, es, fs, gs, hs, is, js, ks, ls, ms);
S13d d -> (as, bs, cs, d:ds, es, fs, gs, hs, is, js, ks, ls, ms);
S13e e -> (as, bs, cs, ds, e:es, fs, gs, hs, is, js, ks, ls, ms);
S13f f -> (as, bs, cs, ds, es, f:fs, gs, hs, is, js, ks, ls, ms);
S13g g -> (as, bs, cs, ds, es, fs, g:gs, hs, is, js, ks, ls, ms);
S13h h -> (as, bs, cs, ds, es, fs, gs, h:hs, is, js, ks, ls, ms);
S13i i -> (as, bs, cs, ds, es, fs, gs, hs, i:is, js, ks, ls, ms);
S13j j -> (as, bs, cs, ds, es, fs, gs, hs, is, j:js, ks, ls, ms);
S13k k -> (as, bs, cs, ds, es, fs, gs, hs, is, js, k:ks, ls, ms);
S13l l -> (as, bs, cs, ds, es, fs, gs, hs, is, js, ks, l:ls, ms);
S13m m -> (as, bs, cs, ds, es, fs, gs, hs, is, js, ks, ls, m:ms);
};};

partitionS14 :: [S14 a b c d e f g h i j k l m n] -> ([a], [b], [c], [d], [e], [f], [g], [h], [i], [j], [k], [l], [m], [n]);
partitionS14 = foldr fn ([], [], [], [], [], [], [], [], [], [], [], [], [], [])
where { fn it (as, bs, cs, ds, es, fs, gs, hs, is, js, ks, ls, ms, ns) = case it of { S14a a -> (a:as, bs, cs, ds, es, fs, gs, hs, is, js, ks, ls, ms, ns);
S14b b -> (as, b:bs, cs, ds, es, fs, gs, hs, is, js, ks, ls, ms, ns);
S14c c -> (as, bs, c:cs, ds, es, fs, gs, hs, is, js, ks, ls, ms, ns);
S14d d -> (as, bs, cs, d:ds, es, fs, gs, hs, is, js, ks, ls, ms, ns);
S14e e -> (as, bs, cs, ds, e:es, fs, gs, hs, is, js, ks, ls, ms, ns);
S14f f -> (as, bs, cs, ds, es, f:fs, gs, hs, is, js, ks, ls, ms, ns);
S14g g -> (as, bs, cs, ds, es, fs, g:gs, hs, is, js, ks, ls, ms, ns);
S14h h -> (as, bs, cs, ds, es, fs, gs, h:hs, is, js, ks, ls, ms, ns);
S14i i -> (as, bs, cs, ds, es, fs, gs, hs, i:is, js, ks, ls, ms, ns);
S14j j -> (as, bs, cs, ds, es, fs, gs, hs, is, j:js, ks, ls, ms, ns);
S14k k -> (as, bs, cs, ds, es, fs, gs, hs, is, js, k:ks, ls, ms, ns);
S14l l -> (as, bs, cs, ds, es, fs, gs, hs, is, js, ks, l:ls, ms, ns);
S14m m -> (as, bs, cs, ds, es, fs, gs, hs, is, js, ks, ls, m:ms, ns);
S14n n -> (as, bs, cs, ds, es, fs, gs, hs, is, js, ks, ls, ms, n:ns);
};};

partitionS15 :: [S15 a b c d e f g h i j k l m n o] -> ([a], [b], [c], [d], [e], [f], [g], [h], [i], [j], [k], [l], [m], [n], [o]);
partitionS15 = foldr fn ([], [], [], [], [], [], [], [], [], [], [], [], [], [], [])
where { fn it (as, bs, cs, ds, es, fs, gs, hs, is, js, ks, ls, ms, ns, os) = case it of { S15a a -> (a:as, bs, cs, ds, es, fs, gs, hs, is, js, ks, ls, ms, ns, os);
S15b b -> (as, b:bs, cs, ds, es, fs, gs, hs, is, js, ks, ls, ms, ns, os);
S15c c -> (as, bs, c:cs, ds, es, fs, gs, hs, is, js, ks, ls, ms, ns, os);
S15d d -> (as, bs, cs, d:ds, es, fs, gs, hs, is, js, ks, ls, ms, ns, os);
S15e e -> (as, bs, cs, ds, e:es, fs, gs, hs, is, js, ks, ls, ms, ns, os);
S15f f -> (as, bs, cs, ds, es, f:fs, gs, hs, is, js, ks, ls, ms, ns, os);
S15g g -> (as, bs, cs, ds, es, fs, g:gs, hs, is, js, ks, ls, ms, ns, os);
S15h h -> (as, bs, cs, ds, es, fs, gs, h:hs, is, js, ks, ls, ms, ns, os);
S15i i -> (as, bs, cs, ds, es, fs, gs, hs, i:is, js, ks, ls, ms, ns, os);
S15j j -> (as, bs, cs, ds, es, fs, gs, hs, is, j:js, ks, ls, ms, ns, os);
S15k k -> (as, bs, cs, ds, es, fs, gs, hs, is, js, k:ks, ls, ms, ns, os);
S15l l -> (as, bs, cs, ds, es, fs, gs, hs, is, js, ks, l:ls, ms, ns, os);
S15m m -> (as, bs, cs, ds, es, fs, gs, hs, is, js, ks, ls, m:ms, ns, os);
S15n n -> (as, bs, cs, ds, es, fs, gs, hs, is, js, ks, ls, ms, n:ns, os);
S15o o -> (as, bs, cs, ds, es, fs, gs, hs, is, js, ks, ls, ms, ns, o:os);
};};

-- * Case analysis

caseS2 :: (a -> z) -> (b -> z) -> S2 a b -> z;
caseS2 fa fb s2 = case s2 of {
S2a a -> fa a;
S2b b -> fb b;
};

caseS3 :: (a -> z) -> (b -> z) -> (c -> z) -> S3 a b c -> z;
caseS3 fa fb fc s3 = case s3 of {
S3a a -> fa a;
S3b b -> fb b;
S3c c -> fc c;
};

caseS4 :: (a -> z) -> (b -> z) -> (c -> z) -> (d -> z) -> S4 a b c d -> z;
caseS4 fa fb fc fd s4 = case s4 of {
S4a a -> fa a;
S4b b -> fb b;
S4c c -> fc c;
S4d d -> fd d;
};

caseS5 :: (a -> z) -> (b -> z) -> (c -> z) -> (d -> z) -> (e -> z) -> S5 a b c d e -> z;
caseS5 fa fb fc fd fe s5 = case s5 of {
S5a a -> fa a;
S5b b -> fb b;
S5c c -> fc c;
S5d d -> fd d;
S5e e -> fe e;
};

caseS6 :: (a -> z) -> (b -> z) -> (c -> z) -> (d -> z) -> (e -> z) -> (f -> z) -> S6 a b c d e f -> z;
caseS6 fa fb fc fd fe ff s6 = case s6 of {
S6a a -> fa a;
S6b b -> fb b;
S6c c -> fc c;
S6d d -> fd d;
S6e e -> fe e;
S6f f -> ff f;
};

caseS7 :: (a -> z) -> (b -> z) -> (c -> z) -> (d -> z) -> (e -> z) -> (f -> z) -> (g -> z) -> S7 a b c d e f g -> z;
caseS7 fa fb fc fd fe ff fg s7 = case s7 of {
S7a a -> fa a;
S7b b -> fb b;
S7c c -> fc c;
S7d d -> fd d;
S7e e -> fe e;
S7f f -> ff f;
S7g g -> fg g;
};

caseS8 :: (a -> z) -> (b -> z) -> (c -> z) -> (d -> z) -> (e -> z) -> (f -> z) -> (g -> z) -> (h -> z) -> S8 a b c d e f g h -> z;
caseS8 fa fb fc fd fe ff fg fh s8 = case s8 of {
S8a a -> fa a;
S8b b -> fb b;
S8c c -> fc c;
S8d d -> fd d;
S8e e -> fe e;
S8f f -> ff f;
S8g g -> fg g;
S8h h -> fh h;
};

caseS9 :: (a -> z) -> (b -> z) -> (c -> z) -> (d -> z) -> (e -> z) -> (f -> z) -> (g -> z) -> (h -> z) -> (i -> z) -> S9 a b c d e f g h i -> z;
caseS9 fa fb fc fd fe ff fg fh fi s9 = case s9 of {
S9a a -> fa a;
S9b b -> fb b;
S9c c -> fc c;
S9d d -> fd d;
S9e e -> fe e;
S9f f -> ff f;
S9g g -> fg g;
S9h h -> fh h;
S9i i -> fi i;
};

caseS10 :: (a -> z) -> (b -> z) -> (c -> z) -> (d -> z) -> (e -> z) -> (f -> z) -> (g -> z) -> (h -> z) -> (i -> z) -> (j -> z) -> S10 a b c d e f g h i j -> z;
caseS10 fa fb fc fd fe ff fg fh fi fj s10 = case s10 of {
S10a a -> fa a;
S10b b -> fb b;
S10c c -> fc c;
S10d d -> fd d;
S10e e -> fe e;
S10f f -> ff f;
S10g g -> fg g;
S10h h -> fh h;
S10i i -> fi i;
S10j j -> fj j;
};

caseS11 :: (a -> z) -> (b -> z) -> (c -> z) -> (d -> z) -> (e -> z) -> (f -> z) -> (g -> z) -> (h -> z) -> (i -> z) -> (j -> z) -> (k -> z) -> S11 a b c d e f g h i j k -> z;
caseS11 fa fb fc fd fe ff fg fh fi fj fk s11 = case s11 of {
S11a a -> fa a;
S11b b -> fb b;
S11c c -> fc c;
S11d d -> fd d;
S11e e -> fe e;
S11f f -> ff f;
S11g g -> fg g;
S11h h -> fh h;
S11i i -> fi i;
S11j j -> fj j;
S11k k -> fk k;
};

caseS12 :: (a -> z) -> (b -> z) -> (c -> z) -> (d -> z) -> (e -> z) -> (f -> z) -> (g -> z) -> (h -> z) -> (i -> z) -> (j -> z) -> (k -> z) -> (l -> z) -> S12 a b c d e f g h i j k l -> z;
caseS12 fa fb fc fd fe ff fg fh fi fj fk fl s12 = case s12 of {
S12a a -> fa a;
S12b b -> fb b;
S12c c -> fc c;
S12d d -> fd d;
S12e e -> fe e;
S12f f -> ff f;
S12g g -> fg g;
S12h h -> fh h;
S12i i -> fi i;
S12j j -> fj j;
S12k k -> fk k;
S12l l -> fl l;
};

caseS13 :: (a -> z) -> (b -> z) -> (c -> z) -> (d -> z) -> (e -> z) -> (f -> z) -> (g -> z) -> (h -> z) -> (i -> z) -> (j -> z) -> (k -> z) -> (l -> z) -> (m -> z) -> S13 a b c d e f g h i j k l m -> z;
caseS13 fa fb fc fd fe ff fg fh fi fj fk fl fm s13 = case s13 of {
S13a a -> fa a;
S13b b -> fb b;
S13c c -> fc c;
S13d d -> fd d;
S13e e -> fe e;
S13f f -> ff f;
S13g g -> fg g;
S13h h -> fh h;
S13i i -> fi i;
S13j j -> fj j;
S13k k -> fk k;
S13l l -> fl l;
S13m m -> fm m;
};

caseS14 :: (a -> z) -> (b -> z) -> (c -> z) -> (d -> z) -> (e -> z) -> (f -> z) -> (g -> z) -> (h -> z) -> (i -> z) -> (j -> z) -> (k -> z) -> (l -> z) -> (m -> z) -> (n -> z) -> S14 a b c d e f g h i j k l m n -> z;
caseS14 fa fb fc fd fe ff fg fh fi fj fk fl fm fn s14 = case s14 of {
S14a a -> fa a;
S14b b -> fb b;
S14c c -> fc c;
S14d d -> fd d;
S14e e -> fe e;
S14f f -> ff f;
S14g g -> fg g;
S14h h -> fh h;
S14i i -> fi i;
S14j j -> fj j;
S14k k -> fk k;
S14l l -> fl l;
S14m m -> fm m;
S14n n -> fn n;
};

caseS15 :: (a -> z) -> (b -> z) -> (c -> z) -> (d -> z) -> (e -> z) -> (f -> z) -> (g -> z) -> (h -> z) -> (i -> z) -> (j -> z) -> (k -> z) -> (l -> z) -> (m -> z) -> (n -> z) -> (o -> z) -> S15 a b c d e f g h i j k l m n o -> z;
caseS15 fa fb fc fd fe ff fg fh fi fj fk fl fm fn fo s15 = case s15 of {
S15a a -> fa a;
S15b b -> fb b;
S15c c -> fc c;
S15d d -> fd d;
S15e e -> fe e;
S15f f -> ff f;
S15g g -> fg g;
S15h h -> fh h;
S15i i -> fi i;
S15j j -> fj j;
S15k k -> fk k;
S15l l -> fl l;
S15m m -> fm m;
S15n n -> fn n;
S15o o -> fo o;
};

-- * Mapping

mapS2 :: (a -> a1) -> (b -> b1) -> S2 a b -> S2 a1 b1;
mapS2 a b = caseS2 (S2a . a) (S2b . b);

mapS3 :: (a -> a1) -> (b -> b1) -> (c -> c1) -> S3 a b c -> S3 a1 b1 c1;
mapS3 a b c = caseS3 (S3a . a) (S3b . b) (S3c . c);

mapS4 :: (a -> a1) -> (b -> b1) -> (c -> c1) -> (d -> d1) -> S4 a b c d -> S4 a1 b1 c1 d1;
mapS4 a b c d = caseS4 (S4a . a) (S4b . b) (S4c . c) (S4d . d);

mapS5 :: (a -> a1) -> (b -> b1) -> (c -> c1) -> (d -> d1) -> (e -> e1) -> S5 a b c d e -> S5 a1 b1 c1 d1 e1;
mapS5 a b c d e = caseS5 (S5a . a) (S5b . b) (S5c . c) (S5d . d) (S5e . e);

mapS6 :: (a -> a1) -> (b -> b1) -> (c -> c1) -> (d -> d1) -> (e -> e1) -> (f -> f1) -> S6 a b c d e f -> S6 a1 b1 c1 d1 e1 f1;
mapS6 a b c d e f = caseS6 (S6a . a) (S6b . b) (S6c . c) (S6d . d) (S6e . e) (S6f . f);

mapS7 :: (a -> a1) -> (b -> b1) -> (c -> c1) -> (d -> d1) -> (e -> e1) -> (f -> f1) -> (g -> g1) -> S7 a b c d e f g -> S7 a1 b1 c1 d1 e1 f1 g1;
mapS7 a b c d e f g = caseS7 (S7a . a) (S7b . b) (S7c . c) (S7d . d) (S7e . e) (S7f . f) (S7g . g);

mapS8 :: (a -> a1) -> (b -> b1) -> (c -> c1) -> (d -> d1) -> (e -> e1) -> (f -> f1) -> (g -> g1) -> (h -> h1) -> S8 a b c d e f g h -> S8 a1 b1 c1 d1 e1 f1 g1 h1;
mapS8 a b c d e f g h = caseS8 (S8a . a) (S8b . b) (S8c . c) (S8d . d) (S8e . e) (S8f . f) (S8g . g) (S8h . h);

mapS9 :: (a -> a1) -> (b -> b1) -> (c -> c1) -> (d -> d1) -> (e -> e1) -> (f -> f1) -> (g -> g1) -> (h -> h1) -> (i -> i1) -> S9 a b c d e f g h i -> S9 a1 b1 c1 d1 e1 f1 g1 h1 i1;
mapS9 a b c d e f g h i = caseS9 (S9a . a) (S9b . b) (S9c . c) (S9d . d) (S9e . e) (S9f . f) (S9g . g) (S9h . h) (S9i . i);

mapS10 :: (a -> a1) -> (b -> b1) -> (c -> c1) -> (d -> d1) -> (e -> e1) -> (f -> f1) -> (g -> g1) -> (h -> h1) -> (i -> i1) -> (j -> j1) -> S10 a b c d e f g h i j -> S10 a1 b1 c1 d1 e1 f1 g1 h1 i1 j1;
mapS10 a b c d e f g h i j = caseS10 (S10a . a) (S10b . b) (S10c . c) (S10d . d) (S10e . e) (S10f . f) (S10g . g) (S10h . h) (S10i . i) (S10j . j);

mapS11 :: (a -> a1) -> (b -> b1) -> (c -> c1) -> (d -> d1) -> (e -> e1) -> (f -> f1) -> (g -> g1) -> (h -> h1) -> (i -> i1) -> (j -> j1) -> (k -> k1) -> S11 a b c d e f g h i j k -> S11 a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1;
mapS11 a b c d e f g h i j k = caseS11 (S11a . a) (S11b . b) (S11c . c) (S11d . d) (S11e . e) (S11f . f) (S11g . g) (S11h . h) (S11i . i) (S11j . j) (S11k . k);

mapS12 :: (a -> a1) -> (b -> b1) -> (c -> c1) -> (d -> d1) -> (e -> e1) -> (f -> f1) -> (g -> g1) -> (h -> h1) -> (i -> i1) -> (j -> j1) -> (k -> k1) -> (l -> l1) -> S12 a b c d e f g h i j k l -> S12 a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1;
mapS12 a b c d e f g h i j k l = caseS12 (S12a . a) (S12b . b) (S12c . c) (S12d . d) (S12e . e) (S12f . f) (S12g . g) (S12h . h) (S12i . i) (S12j . j) (S12k . k) (S12l . l);

mapS13 :: (a -> a1) -> (b -> b1) -> (c -> c1) -> (d -> d1) -> (e -> e1) -> (f -> f1) -> (g -> g1) -> (h -> h1) -> (i -> i1) -> (j -> j1) -> (k -> k1) -> (l -> l1) -> (m -> m1) -> S13 a b c d e f g h i j k l m -> S13 a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1;
mapS13 a b c d e f g h i j k l m = caseS13 (S13a . a) (S13b . b) (S13c . c) (S13d . d) (S13e . e) (S13f . f) (S13g . g) (S13h . h) (S13i . i) (S13j . j) (S13k . k) (S13l . l) (S13m . m);

mapS14 :: (a -> a1) -> (b -> b1) -> (c -> c1) -> (d -> d1) -> (e -> e1) -> (f -> f1) -> (g -> g1) -> (h -> h1) -> (i -> i1) -> (j -> j1) -> (k -> k1) -> (l -> l1) -> (m -> m1) -> (n -> n1) -> S14 a b c d e f g h i j k l m n -> S14 a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1;
mapS14 a b c d e f g h i j k l m n = caseS14 (S14a . a) (S14b . b) (S14c . c) (S14d . d) (S14e . e) (S14f . f) (S14g . g) (S14h . h) (S14i . i) (S14j . j) (S14k . k) (S14l . l) (S14m . m) (S14n . n);

mapS15 :: (a -> a1) -> (b -> b1) -> (c -> c1) -> (d -> d1) -> (e -> e1) -> (f -> f1) -> (g -> g1) -> (h -> h1) -> (i -> i1) -> (j -> j1) -> (k -> k1) -> (l -> l1) -> (m -> m1) -> (n -> n1) -> (o -> o1) -> S15 a b c d e f g h i j k l m n o -> S15 a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1;
mapS15 a b c d e f g h i j k l m n o = caseS15 (S15a . a) (S15b . b) (S15c . c) (S15d . d) (S15e . e) (S15f . f) (S15g . g) (S15h . h) (S15i . i) (S15j . j) (S15k . k) (S15l . l) (S15m . m) (S15n . n) (S15o . o);

-- * Mapping in a Functor

mapS2f :: Functor ftr =>
(a -> ftr a1) -> (b -> ftr b1) -> S2 a b -> ftr (S2 a1 b1);
mapS2f a b = caseS2 (fmap S2a . a) (fmap S2b . b);

mapS3f :: Functor ftr =>
(a -> ftr a1) -> (b -> ftr b1) -> (c -> ftr c1) -> S3 a b c -> ftr (S3 a1 b1 c1);
mapS3f a b c = caseS3 (fmap S3a . a) (fmap S3b . b) (fmap S3c . c);

mapS4f :: Functor ftr =>
(a -> ftr a1) -> (b -> ftr b1) -> (c -> ftr c1) -> (d -> ftr d1) -> S4 a b c d -> ftr (S4 a1 b1 c1 d1);
mapS4f a b c d = caseS4 (fmap S4a . a) (fmap S4b . b) (fmap S4c . c) (fmap S4d . d);

mapS5f :: Functor ftr =>
(a -> ftr a1) -> (b -> ftr b1) -> (c -> ftr c1) -> (d -> ftr d1) -> (e -> ftr e1) -> S5 a b c d e -> ftr (S5 a1 b1 c1 d1 e1);
mapS5f a b c d e = caseS5 (fmap S5a . a) (fmap S5b . b) (fmap S5c . c) (fmap S5d . d) (fmap S5e . e);

mapS6f :: Functor ftr =>
(a -> ftr a1) -> (b -> ftr b1) -> (c -> ftr c1) -> (d -> ftr d1) -> (e -> ftr e1) -> (f -> ftr f1) -> S6 a b c d e f -> ftr (S6 a1 b1 c1 d1 e1 f1);
mapS6f a b c d e f = caseS6 (fmap S6a . a) (fmap S6b . b) (fmap S6c . c) (fmap S6d . d) (fmap S6e . e) (fmap S6f . f);

mapS7f :: Functor ftr =>
(a -> ftr a1) -> (b -> ftr b1) -> (c -> ftr c1) -> (d -> ftr d1) -> (e -> ftr e1) -> (f -> ftr f1) -> (g -> ftr g1) -> S7 a b c d e f g -> ftr (S7 a1 b1 c1 d1 e1 f1 g1);
mapS7f a b c d e f g = caseS7 (fmap S7a . a) (fmap S7b . b) (fmap S7c . c) (fmap S7d . d) (fmap S7e . e) (fmap S7f . f) (fmap S7g . g);

mapS8f :: Functor ftr =>
(a -> ftr a1) -> (b -> ftr b1) -> (c -> ftr c1) -> (d -> ftr d1) -> (e -> ftr e1) -> (f -> ftr f1) -> (g -> ftr g1) -> (h -> ftr h1) -> S8 a b c d e f g h -> ftr (S8 a1 b1 c1 d1 e1 f1 g1 h1);
mapS8f a b c d e f g h = caseS8 (fmap S8a . a) (fmap S8b . b) (fmap S8c . c) (fmap S8d . d) (fmap S8e . e) (fmap S8f . f) (fmap S8g . g) (fmap S8h . h);

mapS9f :: Functor ftr =>
(a -> ftr a1) -> (b -> ftr b1) -> (c -> ftr c1) -> (d -> ftr d1) -> (e -> ftr e1) -> (f -> ftr f1) -> (g -> ftr g1) -> (h -> ftr h1) -> (i -> ftr i1) -> S9 a b c d e f g h i -> ftr (S9 a1 b1 c1 d1 e1 f1 g1 h1 i1);
mapS9f a b c d e f g h i = caseS9 (fmap S9a . a) (fmap S9b . b) (fmap S9c . c) (fmap S9d . d) (fmap S9e . e) (fmap S9f . f) (fmap S9g . g) (fmap S9h . h) (fmap S9i . i);

mapS10f :: Functor ftr =>
(a -> ftr a1) -> (b -> ftr b1) -> (c -> ftr c1) -> (d -> ftr d1) -> (e -> ftr e1) -> (f -> ftr f1) -> (g -> ftr g1) -> (h -> ftr h1) -> (i -> ftr i1) -> (j -> ftr j1) -> S10 a b c d e f g h i j -> ftr (S10 a1 b1 c1 d1 e1 f1 g1 h1 i1 j1);
mapS10f a b c d e f g h i j = caseS10 (fmap S10a . a) (fmap S10b . b) (fmap S10c . c) (fmap S10d . d) (fmap S10e . e) (fmap S10f . f) (fmap S10g . g) (fmap S10h . h) (fmap S10i . i) (fmap S10j . j);

mapS11f :: Functor ftr =>
(a -> ftr a1) -> (b -> ftr b1) -> (c -> ftr c1) -> (d -> ftr d1) -> (e -> ftr e1) -> (f -> ftr f1) -> (g -> ftr g1) -> (h -> ftr h1) -> (i -> ftr i1) -> (j -> ftr j1) -> (k -> ftr k1) -> S11 a b c d e f g h i j k -> ftr (S11 a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1);
mapS11f a b c d e f g h i j k = caseS11 (fmap S11a . a) (fmap S11b . b) (fmap S11c . c) (fmap S11d . d) (fmap S11e . e) (fmap S11f . f) (fmap S11g . g) (fmap S11h . h) (fmap S11i . i) (fmap S11j . j) (fmap S11k . k);

mapS12f :: Functor ftr =>
(a -> ftr a1) -> (b -> ftr b1) -> (c -> ftr c1) -> (d -> ftr d1) -> (e -> ftr e1) -> (f -> ftr f1) -> (g -> ftr g1) -> (h -> ftr h1) -> (i -> ftr i1) -> (j -> ftr j1) -> (k -> ftr k1) -> (l -> ftr l1) -> S12 a b c d e f g h i j k l -> ftr (S12 a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1);
mapS12f a b c d e f g h i j k l = caseS12 (fmap S12a . a) (fmap S12b . b) (fmap S12c . c) (fmap S12d . d) (fmap S12e . e) (fmap S12f . f) (fmap S12g . g) (fmap S12h . h) (fmap S12i . i) (fmap S12j . j) (fmap S12k . k) (fmap S12l . l);

mapS13f :: Functor ftr =>
(a -> ftr a1) -> (b -> ftr b1) -> (c -> ftr c1) -> (d -> ftr d1) -> (e -> ftr e1) -> (f -> ftr f1) -> (g -> ftr g1) -> (h -> ftr h1) -> (i -> ftr i1) -> (j -> ftr j1) -> (k -> ftr k1) -> (l -> ftr l1) -> (m -> ftr m1) -> S13 a b c d e f g h i j k l m -> ftr (S13 a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1);
mapS13f a b c d e f g h i j k l m = caseS13 (fmap S13a . a) (fmap S13b . b) (fmap S13c . c) (fmap S13d . d) (fmap S13e . e) (fmap S13f . f) (fmap S13g . g) (fmap S13h . h) (fmap S13i . i) (fmap S13j . j) (fmap S13k . k) (fmap S13l . l) (fmap S13m . m);

mapS14f :: Functor ftr =>
(a -> ftr a1) -> (b -> ftr b1) -> (c -> ftr c1) -> (d -> ftr d1) -> (e -> ftr e1) -> (f -> ftr f1) -> (g -> ftr g1) -> (h -> ftr h1) -> (i -> ftr i1) -> (j -> ftr j1) -> (k -> ftr k1) -> (l -> ftr l1) -> (m -> ftr m1) -> (n -> ftr n1) -> S14 a b c d e f g h i j k l m n -> ftr (S14 a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1);
mapS14f a b c d e f g h i j k l m n = caseS14 (fmap S14a . a) (fmap S14b . b) (fmap S14c . c) (fmap S14d . d) (fmap S14e . e) (fmap S14f . f) (fmap S14g . g) (fmap S14h . h) (fmap S14i . i) (fmap S14j . j) (fmap S14k . k) (fmap S14l . l) (fmap S14m . m) (fmap S14n . n);

mapS15f :: Functor ftr =>
(a -> ftr a1) -> (b -> ftr b1) -> (c -> ftr c1) -> (d -> ftr d1) -> (e -> ftr e1) -> (f -> ftr f1) -> (g -> ftr g1) -> (h -> ftr h1) -> (i -> ftr i1) -> (j -> ftr j1) -> (k -> ftr k1) -> (l -> ftr l1) -> (m -> ftr m1) -> (n -> ftr n1) -> (o -> ftr o1) -> S15 a b c d e f g h i j k l m n o -> ftr (S15 a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1);
mapS15f a b c d e f g h i j k l m n o = caseS15 (fmap S15a . a) (fmap S15b . b) (fmap S15c . c) (fmap S15d . d) (fmap S15e . e) (fmap S15f . f) (fmap S15g . g) (fmap S15h . h) (fmap S15i . i) (fmap S15j . j) (fmap S15k . k) (fmap S15l . l) (fmap S15m . m) (fmap S15n . n) (fmap S15o . o);

}
