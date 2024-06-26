-- {-# LANGUAGE NoMonomorphismRestriction #-}
import Test.QuickCheck hiding((===))
import Control.Monad(liftM2)
import Control.Applicative((<$>),(<*>))

import Mon
import Reg
import RegExtra

main = do
       writeln "testing left unit"
       quickCheck leftUnit
       writeln "testing right unit"
       quickCheck rightUnit
       writeln "assoc"
       quickCheck assoc
       writeln "nullable unit"
       quickCheck nullableUnit
       writeln "nullable op"
       quickCheck nullableOp
       writeln "nullableSimpl"
       quickCheck nullableSimpl
       quickCheck emptySimpl

       writeln "=== reflexive"
       quickCheck equivRefl
       writeln "=== symmetric"
       writeln "Expect `Gave up' message"
       quickCheck equivSym
       writeln "=== transitive"
       writeln "Expect `Gave up' message"
       quickCheck equivTrans
       writeln "=== compatible"
       quickCheck equivCompatible

       writeln "cs ∈ L(y) && ε ∈ L(x) ==> cs ∈ L(x:>y)"
       quickCheck recLeftNul
       writeln "cs ∈ L(x) && ε ∈ L(y) ==> cs ∈ L(x:>y)"
       quickCheck recRightNul

       write "testRe1 accepts testStr1: "
       print $ accepts testRe1 testStr1
       write "testRe2 accepts testStr1: "
       print $ accepts testRe2 testStr1

       write "mayStart a (ab)*: "
       print $ mayStart A (Many (Lit A :> Lit B))
       write "mayStart b (ab)*: "
       print $ mayStart B (Many (Lit A :> Lit B))

       let emptyList = tail (A:[])

       write "match Empty []: "
       print $ match Empty emptyList

       write "match Empty [A]: "
       print $ match Empty [A]

       write "match Eps []: "
       print $ match Eps emptyList

       write "match Eps [A]: "
       print $ match Eps [A]

       let regex = ((Many (Lit A :> Lit B )) :| (Many (Lit A  :| (Lit B :> Lit A))))

       write "match (Lit A) [A]: " -- Just [A]
       print $ match (Lit A) [A]
       write "match (Lit A :> Lit B) [AAB]: " -- Just [AB]
       print $ match (Lit A :> Lit B) [A,A,B]
       write "match (ab)* + (a + ba)* [AAAAAABABAABABBA]: " -- Just [AAAAAABABAABA]
       print $ match regex [A,A,A,A,A,A,B,A,B,A,A,B,A,B,B,A]
       write "match (ab)* + (a + ba)* [ABBAB]: " -- Just [AB]
       print $ match regex [A,B,B,A,B]




leftUnit :: Reg AB -> Bool
leftUnit x = m1 <> x === x

rightUnit :: Reg AB -> Bool
rightUnit x =  x <> m1 === x

assoc :: Reg AB -> Reg AB -> Reg AB -> Bool
assoc x y z = (x<>y)<>z === x<>(y<>z)

nullableUnit :: Bool
nullableUnit = nullable m1

nullableOp :: Property
nullableOp = forAllNullable $ \x -> forAllNullable $ \y ->  nullable (x <> y)

iff :: Bool -> Bool -> Bool
iff a b = (a && b) || (not a && not b)

nullableSimpl, emptySimpl :: Reg AB -> Bool
nullableSimpl x = nullable x `iff` nullable (simpl x)
emptySimpl x = empty x `iff` empty (simpl x)

recLeftNul :: Reg AB -> Property
recLeftNul y = forAllNullable $ \x ->
               forAllMatching y $ \cs ->
               accepts y cs ==> accepts (x:>y) cs

recRightNul :: Reg AB -> Property
recRightNul x = forAllNullable $ \y ->
               forAllMatching x $ \cs ->
               accepts x cs ==> accepts (x:>y) cs

equivRefl :: Reg AB -> Bool
equivRefl r = r === r

equivSym :: Reg AB -> Reg AB -> Property
equivSym r1 r2 = r1 === r2 ==> r2 === r1

equivTrans :: Reg AB -> Reg AB -> Reg AB -> Property
equivTrans r1 r2 r3 = (r1 === r2) && (r2 === r3) ==> r1 === r3

equivCompatible :: AB -> AB -> Property
equivCompatible c d = (Lit c) === (Lit d) ==> c == d

testRe1 = Many digit <> string "ala"
testRe2 = Many digit <> Many letter <> Many digit

testStr1 = replicate 1000 '0' ++ "ala"

write = putStr
writeln = putStrLn

------------------------------------------------------------
-- Hic sunt leones
------------------------------------------------------------

instance Arbitrary AB where
  arbitrary = oneof [return A, return B]

shrinkReg :: (Eq c, Show c) => Reg c -> [Reg c]
shrinkReg r = if r == s then [] else [s] where s = simpl r

--liftR f x = liftM2 f x x

instance (Eq c,Arbitrary c, Show c) => Arbitrary (Reg c) where
  arbitrary = sized arb where
    arb 0 = oneof [return Eps, return Empty]
    arb 1 = Lit <$> arbitrary
    arb n = oneof [Many <$> arb2, liftM2 (:>) arb2 arb2, liftM2 (:|) arb2 arb2] where
      arb1 = arb (n-1)
      arb2 = arb (n `div` 2)

  shrink = shrinkReg

forAllNullable :: (Testable prop) => (Reg AB -> prop) -> Property
forAllNullable = forAll genNullableAB

genNullableAB :: Gen (Reg AB)
genNullableAB = genNullable

genNullable :: Gen (Reg AB)
genNullable = sized gn where
  gn 0 = return Eps
  gn n = oneof [
    Many <$> gab2,
    liftM2 (:>) gn2 gn2,
    liftM2 (:|) gab2 gn2,
    liftM2 (:|) gn2 gab2] where
      gn2 = gn (n `div` 2)
      gab2 = gAB (div n 2)
genRegAB :: Gen (Reg AB)
genRegAB = sized gAB

gAB 0 = return Empty
gAB 1 = elements [Eps, Lit A, Lit B]
gAB n = oneof [
  Many <$> gab2,
    liftM2 (:>) gab2 gab2,
    liftM2 (:|) gab2 gab2,
    liftM2 (:|) gab2 gab2] where
      gab2 = gAB (div n 2)


forAllMatching = forAll . genMatching
genMatching :: Reg AB -> Gen [AB]
genMatching r = sized (gm r)

-- Assume r nullable
genMatchingNullable = sized  . gm where
  gm r 0 = return []
  gm r n | n < 0 = return [] -- safety net
         | otherwise = do
             hd <- elements [A,B]
             let r' = der hd r
             if empty r' then gm r (n-1) else (hd:) <$> (gm r' (n-1))

liftCons :: AB -> Gen [AB] -> Gen [AB]
liftCons x g = (x:) <$> g -- do { xs <- g; return (x:xs) }

gm r 0 | nullable r = return []
       | otherwise = elements [[A],[B]]
gm r n = gmn (simpl r) where
    gmn Eps = return []
    gmn (Lit c) = return [c]
    gmn (r1 :| r2) = oneof [gmn r1, gmn r2]
    gmn (Lit c:> r) = (c:) <$> gm r (n-1)
    gmn (r1 :> r2) = do
      k <- choose (0,n)
      splitAt k (gm r1) (gm r2)
    gmn (Many r) = do
      k <- choose (0,n)
      if k == 0 then return [] else splitAt (n-k) (gm r) (gm (Many r))
    gmn _ = return []
    splitAt k g1 g2 =
      liftM2 (++) (g1 k) (g2 (n-k))
