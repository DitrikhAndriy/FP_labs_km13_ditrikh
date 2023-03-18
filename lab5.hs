{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}


data Nat = Zero | Succ Nat
    deriving Show

type family Add (n :: Nat) (m :: Nat) :: Nat
type instance Add Zero     m = m
type instance Add (Succ n) m = Succ (Add n m)

data Equality a b where
    R :: Equality a a

instance Show (Equality a b) where
    show R = "QED"

reflexivity :: Equality a a
reflexivity = R

symmetry :: Equality a b -> Equality b a
symmetry R = R

transitivity :: Equality a b -> Equality b c -> Equality a c
transitivity R R = R

data SNat (n :: Nat) where
    SZero :: SNat Zero
    SSucc :: SNat n -> SNat (Succ n)

-- for all x in Nat: 0 + x = x
natAxiom1 :: SNat x -> Equality (Add Zero x) x
natAxiom1 _ = R

-- for all x, y in Nat: (Succ x) + y = Succ (x + y)
natAxiom2 :: SNat x -> SNat y -> Equality (Add (Succ x) y) (Succ (Add x y))
natAxiom2 _ _ = R

univalence :: Equality a b -> (a ~ b => t) -> t
univalence R t = t

leftPlusZeroIsId :: SNat n -> Equality (Add Zero n) n
leftPlusZeroIsId _ = R

rightPlusZeroIsId :: SNat n -> Equality (Add n Zero) n
rightPlusZeroIsId SZero     = R
rightPlusZeroIsId (SSucc n) = univalence (rightPlusZeroIsId n) R

plusIsAssociative :: SNat x -> SNat y -> SNat z  -> Equality (Add x (Add y z)) (Add (Add x y) z)
plusIsAssociative SZero     _ _ = R
plusIsAssociative (SSucc x) y z =
    univalence (plusIsAssociative x y z) R

plusIsCommutative :: SNat x -> SNat y -> Equality (Add x y) (Add y x)
plusIsCommutative SZero SZero = R
plusIsCommutative SZero y     = univalence (rightPlusZeroIsId y) R
plusIsCommutative (SSucc x) y =
    step1 x y .-> step2 x y .-> step3 x y
    where
        (.->) = transitivity

        step1 :: SNat x -> SNat y  -> Equality (Add (Succ x) y) (Succ (Add x y))
        step1 x y = univalence (natAxiom2 x y) R

        step2 :: SNat x -> SNat y -> Equality (Succ (Add x y)) (Succ (Add y x))
        step2 x y = univalence (plusIsCommutative x y) R

        step3 :: SNat x -> SNat y -> Equality (Succ (Add y x)) (Add y (Succ x))
        step3 _ SZero     = R
        step3 x (SSucc y) = univalence (step3 x y) R



-- First Task
type family Mult (x :: Nat) (y :: Nat) :: Nat
type instance Mult x Zero     = Zero
type instance Mult x (Succ y) = (Add (Mult x y) x)

-- Second Task
rightMultZeroIsId :: SNat x -> Equality (Mult x Zero) Zero
rightMultZeroIsId _ = R

leftMultZeroIsId :: SNat x -> Equality (Mult Zero x) Zero
leftMultZeroIsId SZero     = R
leftMultZeroIsId (SSucc x) = univalence (leftMultZeroIsId x) R

-- Third Task
rightMultOneIsId :: SNat x -> Equality (Mult x (Succ Zero)) x
rightMultOneIsId SZero = R
rightMultOneIsId (SSucc x) = univalence (rightMultOneIsId x) R

leftMultOneIsId :: SNat x -> Equality (Mult (Succ Zero) x) x
leftMultOneIsId SZero = R
leftMultOneIsId (SSucc x) = univalence (leftMultOneIsId x) (lemma x)
    where
        lemma :: SNat x -> Equality (Add x (Succ Zero)) (Succ x)
        lemma SZero = R
        lemma (SSucc x) = univalence (lemma x) R

-- Fourth Task
multIsDistribut :: SNat x -> SNat y -> SNat z -> Equality (Mult (Add x y) z) (Add (Mult x z) (Mult y z))
multIsDistribut _ _ SZero     = R
multIsDistribut x y (SSucc z) = univalence (multIsDistribut x y z) (lemma x y z)
    where
        lemma :: SNat x -> SNat y -> SNat z -> Equality (Add (Add (Mult x z) (Mult y z)) (Add x y)) (Add (Add (Mult x z) x) (Add (Mult y z) y))
        lemma _ _ SZero = R


main :: IO ()
main = do
    -- Second Task
    print $ rightMultZeroIsId (SSucc SZero)
    print $ leftMultZeroIsId  (SSucc SZero)
    -- Third Task
    print $ rightMultOneIsId  (SSucc SZero)
    print $ leftMultOneIsId   (SSucc SZero)
    -- Fourth Task
    print $ multIsDistribut (SSucc SZero) (SSucc SZero) (SSucc SZero)