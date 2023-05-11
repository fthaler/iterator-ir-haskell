module IteratorIr (
    Iterator (It),
    nb,
    deref,
    itZip2,
    itUnzip2,
    shift,
    lift,
    lift2,
    derefedNbShift,
    nbShift,
    reduce1,
    reduce2
) where

import Control.Lens.Internal.Context
import Data.Typeable (Typeable, typeOf)


data Iterator a b t = It (b -> t) a

instance (Show a, Typeable a, Typeable b, Typeable t) => Show (Iterator a b t) where
    showsPrec p (It f a) = showParen (p > 10) $
        showString (show (typeOf (It f a))) . showString " @ " . shows a

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
    iexperiment f (It g a) = fmap g (f a)


nb :: (a -> [b]) -> Int -> a -> b
nb c i = (!! i) . c

deref :: Iterator a a t -> t
deref = iextract  -- from IndexedComonad

shift :: (a -> b) -> Iterator a c t -> Iterator b c t
shift = iseeks  -- from IndexedComonadStore

lift :: (Iterator b c t -> r) -> Iterator a c t -> Iterator a b r
lift = iextend  -- from IndexedComonad

lift2 :: (Iterator b c t -> Iterator b c' t' -> r) -> Iterator a c t -> Iterator a c' t' -> Iterator a b r
lift2 g (It f a) (It f' a') = It (\x -> g (It f x) (It f' x)) a  -- TODO: a and a' must be identical!


derefedNbShift :: (a -> [b]) -> Iterator a b t -> [t]
derefedNbShift = iexperiment
nbShift :: (b -> [c]) -> Iterator a c t -> Iterator a b [t]
nbShift = lift . derefedNbShift

-- This looks like a Traversal optic (see: Control.Lens.Traversal)
-- neighborhood :: Traversal a b (Iterator a p t) (Iterator b p t)
-- neighborhood :: Applicative f => (a -> f b) -> Iterator a c t -> f (Iterator b c t)
neighborhood :: (a -> [b]) -> Iterator a c t -> [Iterator b c t]
neighborhood a2b (It acc a) = fmap (It acc) (a2b a)


itZip2 (It f a) (It f' a') = It (\x -> (f x, f' x)) a
-- or equivalently: itZip2 = lift2 $ \x y -> (deref x, deref y)
itUnzip2 i = (ifmap fst i, ifmap snd i)

reduce1 f i = foldl f i . deref
reduce2 f i a b = foldl (\x (y, z) -> f x y z) i $ zip (deref a) (deref b)
