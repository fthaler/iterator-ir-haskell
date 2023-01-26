module IteratorIr (
    Iterator (It),
    deref,
    itZip2,
    shift,
    lift1,
    lift2,
    derefedNbShift,
    nbShift,
    reduce1,
    reduce2
) where

import Control.Lens.Internal.Context


data Iterator a b t = It (b -> t) a

instance IndexedFunctor Iterator where
    ifmap f (It g t) = It (f . g) t

instance IndexedComonad Iterator where
    iextract   (It f a) = f a
    iduplicate (It f a) = It (It f) a
    iextend g  (It f a) = It (g . It f) a

instance IndexedComonadStore Iterator where
    ipos (It _ a) = a
    ipeek b (It g _) = g b
    ipeeks f (It g a) = g (f a)
    iseek a (It g _) = It g a
    iseeks f (It g a) = It g (f a)
    iexperiment f (It g a) = g <$> f a


deref :: Iterator a a t -> t
deref = iextract  -- from IndexedComonad

shift :: (a -> b) -> Iterator a c t -> Iterator b c t
shift = iseeks  -- from IndexedComonadStore

lift1 :: (Iterator b c t -> r) -> Iterator a c t -> Iterator a b r
lift1 = iextend  -- from IndexedComonad

lift2 :: (Iterator b c t -> Iterator b c' t' -> r) -> Iterator a c t -> Iterator a c' t' -> Iterator a b r
lift2 g (It f a) (It f' a') = It (\x -> g (It f x) (It f' x)) a  -- TODO: a and a' must be identical!


derefedNbShift n c = iexperiment $ \x -> [c i x | i <- [0..n-1]]
nbShift n c = lift1 $ derefedNbShift n c

itZip2 = lift2 $ \x y -> (deref x, deref y)

reduce1 f i = foldl f i . deref
reduce2 f i a b = foldl (\x (y, z) -> f x y z) i $ zip (deref a) (deref b)

