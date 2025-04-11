{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Move brackets to avoid $" #-}
{-# LANGUAGE PatternSynonyms #-}

module Tydi.Data.Prefix where
import Clash.Explicit.Prelude hiding (
  (!!),at,
  head,last,
  zip,zip3,zip4,zip5,zip6,zip7,
  unzip,unzip3,unzip4,unzip5,unzip6,unzip7,
  zipWith,zipWith3,zipWith4,zipWith5,zipWith6,zipWith7,
  izipWith,
  indices,
  map,
  findIndex,elemIndex,
  traverse#,fold,foldr,ifoldr,
 )
import qualified Clash.Sized.Vector as V
-- import Data.Data (Data)
import Data.Foldable (foldr)

-- like Slice, but without a start index
data Prefix n a where
  Prefix' :: (n~n0+1) => {end::Index n,vec::Vec n a} -> Prefix n a

pattern Prefix :: (KnownNat n,n~n0+1) => Index n -> Vec n a -> Prefix n a
pattern Prefix end vec <- Prefix'{end,vec}  where
  Prefix end vec = prefix end vec
{-# COMPLETE Prefix #-}

deriving instance (KnownNat n,Lift a) => Lift (Prefix n a)
-- deriving instance Data (Prefix n a) -- TODO
-- deriving instance Generic (Prefix n a) -- TODO
deriving instance Bundle (Prefix n a)

prefix :: (KnownNat n,n~n0+1) => Index n -> Vec n a -> Prefix n a
prefix e v = Prefix'{end=e,vec=V.zipWith (\i x -> if i<=e then x else errorX "Outside of prefix range") indicesI v}

unsafeToVec :: Prefix n a -> Vec n a
unsafeToVec Prefix'{vec} = vec

strobed :: (KnownNat n) => Prefix n a -> Vec n (Maybe a)
strobed Prefix'{end,vec} = V.imap (\i x -> if i<=end then Just x else Nothing) vec

full :: (KnownNat n,n~n0+1) => Vec n a -> Prefix n a
full v = Prefix'{end=maxBound,vec=v}

-- (Lift, derived)

instance  (KnownNat n) => Functor (Prefix n) where
  fmap = map

instance  (KnownNat n,n~n+1) => Applicative (Prefix n) where
  pure x = full $ pure x
  (<*>) = zipWith ($)

instance (KnownNat n) => Foldable (Prefix n) where
  foldr f z Prefix'{end,vec} = foldr# f z 0 end vec

foldr# :: (KnownNat m) => (a -> b -> b) -> b -> Index m -> Index m -> Vec n a -> b
foldr# _ z _ _ Nil           = z -- should not be possible
foldr# f z i end (x `Cons` xs) | i<=end    = f x z
                               | otherwise = f x (foldr# f z (i+1) end xs)

instance (KnownNat n) => Traversable (Prefix n) where --TODO: what is this supposed to do?
  traverse f Prefix'{end,vec} = prefix end <$> traverse# f 0 end vec

{-# CLASH_OPAQUE traverse# #-}
-- {-# ANN traverse# hasBlackBox #-}
traverse# :: forall a f b n m . (Applicative f, KnownNat n, KnownNat m) => (a -> f b) -> Index m -> Index m -> Vec n a -> f (Vec n b)
traverse# _ _ _ Nil           = pure Nil
traverse# f i end (x `Cons` xs) = if i<=end then Cons <$> f x <*> traverse# f (i+1) end xs else pure $ repeat undefined

instance (KnownNat n, Eq a) => Eq (Prefix n a) where
  (==) p@Prefix'{} q@Prefix'{} = and (zipWith (==) p q)

instance (KnownNat n, Ord a) => Ord (Prefix n a) where
  compare x y = foldr f EQ $ zipWith compare x y
    where f EQ   keepGoing = keepGoing
          f done _         = done


--Data? derive?



instance (KnownNat n,Show a) => Show (Prefix n a) where
  showsPrec n = \case
    Prefix'{end,vec=vs} -> showParen (n > 5) (go 0 end vs)

   where
    go ::  (KnownNat n) => Index n -> Index n -> Vec m a -> ShowS
    go _ _ Nil = showString "Nil"
    go i end (x `Cons` xs)
     | i<end =
          showsPrec (5 + 1) x
        . showString " :> "
        . go (i+1) end xs
     | otherwise =
          showsPrec (5 + 1) x
        . showString " :> "
        . go' xs
    go' :: Vec m a -> ShowS
    go' Nil = showString "Nil"
    go' (_ `Cons` xs) =
          showString " - :> "
        . go' xs

-- Generic (derive)

instance (KnownNat n,Semigroup a) => Semigroup (Prefix n a) where
  (<>) = zipWith (<>)

instance  (KnownNat n,n~n0+1,Monoid a) => Monoid (Prefix n a) where
  mempty = full $ repeat mempty

-- Arbitrary? --TODO?
-- COarbitrary?

-- instance (Default a) => Default (Prefix n a) where --TODO
--   def = full $ repeat def

-- instance (NFData a) => NFData (Prefix n a) where -- TODO
--   rnf = ???

-- instance (NFDataX a) => NFDataX (Prefix n a) where --TODO
--   ???

-- instance (ShowX a) => ShowX (Prefix n a) where -- TODO
--   ???

-- Ixed ? lens package TODO

-- instance AutoReg (Prefix n a) where --TODO
--   ???

--LockStep? TODO



maxLength :: KnownNat n => Prefix n a -> Int
maxLength Prefix'{vec} = length vec
maxLengthS :: KnownNat n => Prefix n a -> SNat n
maxLengthS Prefix'{vec} = lengthS vec

(!!) :: (KnownNat n, Enum i) => Prefix n a -> i -> a
Prefix'{end,vec} !! i = if fromIntegral (fromEnum i) <= end then vec V.!! i else errorX "Index out of prefix range"
{-# INLINE (!!) #-}

(!!?) :: (KnownNat n, Enum i) => Prefix n a -> i -> Maybe a
Prefix'{end,vec} !!? i = if i' <= end then Just $ vec V.!! i else Nothing
  where i' = fromIntegral $ fromEnum i
{-# INLINE (!!?) #-}

head :: Prefix (n + 1) a -> a
head Prefix'{vec} = V.head vec

last :: KnownNat n => Prefix n a -> a
last Prefix'{end,vec} = vec V.!! end

at :: forall (n :: Natural) (m :: Natural) (m0 :: Natural) a. (KnownNat m,n+1<=m,n+1+m0~m) => SNat n -> Prefix m a -> Maybe a
at n Prefix'{end,vec} = if fromSNat n <= end then Just $ V.at n vec else Nothing

-- indices :: t -> Prefix n (Index n)
-- indices n = full $ V.indices n

-- incidesI = full $ indicesI

findIndex :: KnownNat n => (a -> Bool) -> Prefix n a -> Maybe (Index n)
findIndex f = ifoldr (\i a b -> if f a then Just i else b) Nothing

ifoldr ::  (KnownNat n) => (Index n -> a -> b -> b) -> b -> Prefix n a -> b
ifoldr f b xs = foldr (uncurry f) b (zip V.indicesI xs)

elemIndex :: (KnownNat n, Eq a) => a -> Prefix n a -> Maybe (Index n)
elemIndex x = findIndex (x ==)


--slicing

-- tail?
-- init?
-- take?
-- takeI?
-- drop?
-- dropI?
-- select?
-- selectI?

--subslice / subprefix
subPrefix :: KnownNat n => Index n -> Prefix n a -> Prefix n a
subPrefix e Prefix'{end,vec} = prefix (min e end) vec

--toSlice -- defined in Slice
--toPrefix = ???

-- not that useful actually
-- repeat x = full $ repeat x
-- iterate n = full $ iterate n
-- iterateI = full $ iterateI
-- generate :: SNat n -> (a -> a) -> a -> Prefix n a
-- generate n g s = full $ generate n g s
-- generateI = ???

-- resize = ???

shiftIn :: KnownNat n => Prefix n a -> a -> Prefix n a
shiftIn Prefix'{end,vec} x = Prefix'{end,vec=x +>> vec}
prepend ::  (KnownNat n) => Prefix n a -> a -> Prefix (n+1) a
prepend Prefix'{end,vec} x = Prefix'{end=end',vec=x:>vec} --prefix only
  where end' = (unpack $ resize $ pack end) + 1

replace :: (KnownNat n, Enum i) => i -> a -> Prefix n a -> Prefix n a
replace i y p@Prefix'{end,vec} = if fromIntegral (fromEnum i)<=end then Prefix'{end,vec=V.replace i y vec} else p

--reverse = --slice only

-- shiftLeft = ??? -- slice only?
-- shiftRight = ??? -- slice only?

map :: (KnownNat n) => (a -> b) -> Prefix n a -> Prefix n b
map f Prefix'{end,vec} = prefix end $ fmap f vec
-- imap = ??? --TODO
-- smap = ??? --TODO

--zip,3,4,5,6,7
type family VLength a :: Nat
type instance VLength (Vec n a) = n
type instance VLength (Prefix n a) = n

class (VLength a ~ VLength b) => Zippable a b where
  type Zipped a b
  zip :: a -> b -> Zipped a b

instance Zippable (Vec n a) (Vec n b) where
  type Zipped (Vec n a) (Vec n b) = Vec n (a,b)
  zip = V.zip
instance (KnownNat n) => Zippable (Prefix n a) (Vec n b) where
  type Zipped (Prefix n a) (Vec n b) = Prefix n (a,b)
  zip Prefix'{end,vec} v = prefix end $ V.zip vec v
instance (KnownNat n) => Zippable (Vec n a) (Prefix n b) where
  type Zipped (Vec n a) (Prefix n b) = Prefix n (a,b)
  zip v Prefix'{end,vec} = prefix end $ V.zip v vec
instance (KnownNat n) => Zippable (Prefix n a) (Prefix n b) where
  type Zipped (Prefix n a) (Prefix n b) = Prefix n (a,b)
  zip Prefix'{end,vec} Prefix'{end=end',vec=vec'} = prefix (min end end') $ V.zip vec vec'

zip3 :: (Zipped (Zipped a1 b1) b2 ~ f ((a2, b3), c), Functor f,  Zippable a1 b1, Zippable (Zipped a1 b1) b2) => a1 -> b1 -> b2 -> f (a2, b3, c)
zip3 a b = zipWith (\(p,q)         r -> (p,q,r))         (zip  a b)
zip4 :: (Zipped (f1 (a2, b3, c1)) b2 ~ f2 ((a, b5, c2), d),  Zipped (Zipped a1 b1) b4 ~ f1 ((a2, b3), c1), Functor f2,  Functor f1, Zippable a1 b1, Zippable (f1 (a2, b3, c1)) b2,  Zippable (Zipped a1 b1) b4) => a1 -> b1 -> b4 -> b2 -> f2 (a, b5, c2, d)
zip4 a b c = zipWith (\(p,q,r)       s -> (p,q,r,s))       (zip3 a b c)
zip5 :: (Zipped (f2 (a1, b5, c2, d1)) b1 ~ f ((a2, b2, c, d2), e),  Zipped (f1 (a3, b3, c1)) b4 ~ f2 ((a1, b5, c2), d1),  Zipped (Zipped a4 b6) b7 ~ f1 ((a3, b3), c1), Functor f,  Functor f2, Functor f1, Zippable a4 b6,  Zippable (f2 (a1, b5, c2, d1)) b1, Zippable (f1 (a3, b3, c1)) b4,  Zippable (Zipped a4 b6) b7) => a4 -> b6 -> b7 -> b4 -> b1 -> f (a2, b2, c, d2, e)
zip5 a b c d = zipWith (\(p,q,r,s)     t -> (p,q,r,s,t))     (zip4 a b c d)
zip6 :: (Zipped (f2 (a1, b5, c2, d1)) b3 ~ f1 ((a2, b2, c1, d2), e1),  Zipped (f3 (a3, b4, c3)) b8 ~ f2 ((a1, b5, c2), d1),  Zipped (f1 (a2, b2, c1, d2, e1)) b1 ~ f4 ((a, b9, c4, d, e2), f5),  Zipped (Zipped a4 b6) b7 ~ f3 ((a3, b4), c3), Functor f4,  Functor f1, Functor f2, Functor f3, Zippable a4 b6,  Zippable (f1 (a2, b2, c1, d2, e1)) b1,  Zippable (f2 (a1, b5, c2, d1)) b3, Zippable (f3 (a3, b4, c3)) b8,  Zippable (Zipped a4 b6) b7) => a4 -> b6 -> b7 -> b8 -> b3 -> b1 -> f4 (a, b9, c4, d, e2, f5)
zip6 a b c d e = zipWith (\(p,q,r,s,t)   u -> (p,q,r,s,t,u))   (zip5 a b c d e)
zip7 :: (Zipped (f1 (a2, b2, c1, d2, e1)) b1  ~ f4 ((a1, b9, c4, d1, e2), f5),  Zipped (f2 (a3, b5, c2, d3)) b3 ~ f1 ((a2, b2, c1, d2), e1),  Zipped (f4 (a1, b9, c4, d1, e2, f5)) b4  ~ f7 ((a4, b6, c, d4, e, f8), g),  Zipped (f3 (a5, b7, c3)) b8 ~ f2 ((a3, b5, c2), d3),  Zipped (Zipped a6 b11) b12 ~ f3 ((a5, b7), c3), Functor f7,  Functor f4, Functor f1, Functor f2, Functor f3, Zippable a6 b11,  Zippable (f4 (a1, b9, c4, d1, e2, f5)) b4,  Zippable (f1 (a2, b2, c1, d2, e1)) b1,  Zippable (f2 (a3, b5, c2, d3)) b3, Zippable (f3 (a5, b7, c3)) b8,  Zippable (Zipped a6 b11) b12) => a6 -> b11 -> b12 -> b8 -> b3 -> b1 -> b4 -> f7 (a4, b6, c, d4, e, f8, g)
zip7 a b c d e f = zipWith (\(p,q,r,s,t,u) v -> (p,q,r,s,t,u,v)) (zip6 a b c d e f)

-- zipWith  :: (Zipped (f a) (g b) ~ h (a, b), Zippable (f a) (g b)) => (a -> b -> z) -> f a -> g b -> h z
zipWith :: (Zipped a1 b1 ~ f (a2, b2), Functor f, Zippable a1 b1) => (a2 -> b2 -> b3) -> a1 -> b1 -> f b3
zipWith  f' a b           = uncurry  f' <$> zip  a b

zipWith3 :: (Zipped (Zipped a1 b1) b2 ~ f ((t1, t2), t3), Functor f,  Zippable a1 b1, Zippable (Zipped a1 b1) b2) => (t1 -> t2 -> t3 -> b) -> a1 -> b1 -> b2 -> f b
zipWith3 f' a b c         = uncurry3 f' <$> zip3 a b c
zipWith4 :: (Zipped (f1 (a2, b3, c1)) b2 ~ f ((t1, t2, t3), t4),  Zipped (Zipped a1 b1) b4 ~ f1 ((a2, b3), c1), Functor f,  Functor f1, Zippable a1 b1, Zippable (f1 (a2, b3, c1)) b2,  Zippable (Zipped a1 b1) b4) => (t1 -> t2 -> t3 -> t4 -> b) -> a1 -> b1 -> b4 -> b2 -> f b
zipWith4 f' a b c d       = uncurry4 f' <$> zip4 a b c d
zipWith5 :: (Zipped (f2 (a1, b5, c2, d1)) b1 ~ f ((t1, t2, t3, t4), t5),  Zipped (f1 (a3, b3, c1)) b4 ~ f2 ((a1, b5, c2), d1),  Zipped (Zipped a4 b6) b7 ~ f1 ((a3, b3), c1), Functor f,  Functor f2, Functor f1, Zippable a4 b6,  Zippable (f2 (a1, b5, c2, d1)) b1, Zippable (f1 (a3, b3, c1)) b4,  Zippable (Zipped a4 b6) b7) => (t1 -> t2 -> t3 -> t4 -> t5 -> b) -> a4 -> b6 -> b7 -> b4 -> b1 -> f b
zipWith5 f' a b c d e     = uncurry5 f' <$> zip5 a b c d e
zipWith6 :: (Zipped (f3 (a3, b4, c3)) b8 ~ f2 ((a1, b5, c2), d1),  Zipped (f1 (a2, b2, c1, d2, e1)) b1 ~ f ((t1, t2, t3, t4, t5), t6),  Zipped (f2 (a1, b5, c2, d1)) b3 ~ f1 ((a2, b2, c1, d2), e1),  Zipped (Zipped a4 b6) b7 ~ f3 ((a3, b4), c3), Functor f,  Functor f1, Functor f2, Functor f3, Zippable a4 b6,  Zippable (f1 (a2, b2, c1, d2, e1)) b1,  Zippable (f2 (a1, b5, c2, d1)) b3, Zippable (f3 (a3, b4, c3)) b8,  Zippable (Zipped a4 b6) b7) => (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> b) -> a4 -> b6 -> b7 -> b8 -> b3 -> b1 -> f b
zipWith6 f' a b c d e f   = uncurry6 f' <$> zip6 a b c d e f
zipWith7 :: (Zipped (f3 (a5, b7, c3)) b8 ~ f2 ((a3, b5, c2), d3),  Zipped (f2 (a3, b5, c2, d3)) b3 ~ f1 ((a2, b2, c1, d2), e1),  Zipped (f1 (a2, b2, c1, d2, e1)) b1  ~ f4 ((a1, b9, c4, d1, e2), f5),  Zipped (f4 (a1, b9, c4, d1, e2, f5)) b4  ~ f ((t1, t2, t3, t4, t5, t6), t7),  Zipped (Zipped a6 b11) b12 ~ f3 ((a5, b7), c3), Functor f,  Functor f4, Functor f1, Functor f2, Functor f3, Zippable a6 b11,  Zippable (f4 (a1, b9, c4, d1, e2, f5)) b4,  Zippable (f1 (a2, b2, c1, d2, e1)) b1,  Zippable (f2 (a3, b5, c2, d3)) b3, Zippable (f3 (a5, b7, c3)) b8,  Zippable (Zipped a6 b11) b12) => (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> b) -> a6 -> b11 -> b12 -> b8 -> b3 -> b1 -> b4 -> f b
zipWith7 f' a b c d e f g = uncurry7 f' <$> zip7 a b c d e f g

uncurry3 :: (t1 -> t2 -> t3 -> t4) -> (t1, t2, t3) -> t4
uncurry3 f' (a,b,c)         = f' a b c
uncurry4 :: (t1 -> t2 -> t3 -> t4 -> t5) -> (t1, t2, t3, t4) -> t5
uncurry4 f' (a,b,c,d)       = f' a b c d
uncurry5 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6) -> (t1, t2, t3, t4, t5) -> t6
uncurry5 f' (a,b,c,d,e)     = f' a b c d e
uncurry6 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7) -> (t1, t2, t3, t4, t5, t6) -> t7
uncurry6 f' (a,b,c,d,e,f)   = f' a b c d e f
uncurry7 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8) -> (t1, t2, t3, t4, t5, t6, t7) -> t8
uncurry7 f' (a,b,c,d,e,f,g) = f' a b c d e f g

izipWith :: (Zipped    (Vec (VLength (Zipped a b)) (Index (VLength (Zipped a b))))    (Zipped a b)  ~ f (t1, (t2, t3)),  Functor f, KnownNat (VLength (Zipped a b)), Zippable a b,  Zippable    (Vec (VLength (Zipped a b)) (Index (VLength (Zipped a b))))    (Zipped a b)) => (t1 -> t2 -> t3 -> b3) -> a -> b -> f b3
izipWith f as bs = zipWith (\i (a,b) -> f i a b) V.indicesI (zip as bs)

unzip  :: Prefix n (a,b) -> (Prefix n a, Prefix n b)
unzip  Prefix'{end,vec} = (Prefix'{end,vec=a},Prefix'{end,vec=b})
  where (a,b) = V.unzip vec
unzip3 :: Prefix n (a,b,c) -> (Prefix n a, Prefix n b, Prefix n c)
unzip3 Prefix'{end,vec} = (Prefix'{end,vec=a},Prefix'{end,vec=b},Prefix'{end,vec=c})
  where (a,b,c) = V.unzip3 vec
unzip4 :: Prefix n (a,b,c,d) -> (Prefix n a, Prefix n b, Prefix n c, Prefix n d)
unzip4 Prefix'{end,vec} = (Prefix'{end,vec=a},Prefix'{end,vec=b},Prefix'{end,vec=c},Prefix'{end,vec=d})
  where (a,b,c,d) = V.unzip4 vec
unzip5 :: Prefix n (a,b,c,d,e) -> (Prefix n a, Prefix n b, Prefix n c, Prefix n d, Prefix n e)
unzip5 Prefix'{end,vec} = (Prefix'{end,vec=a},Prefix'{end,vec=b},Prefix'{end,vec=c},Prefix'{end,vec=d},Prefix'{end,vec=e})
  where (a,b,c,d,e) = V.unzip5 vec
unzip6 :: Prefix n (a,b,c,d,e,f) -> (Prefix n a, Prefix n b, Prefix n c, Prefix n d, Prefix n e, Prefix n f)
unzip6 Prefix'{end,vec} = (Prefix'{end,vec=a},Prefix'{end,vec=b},Prefix'{end,vec=c},Prefix'{end,vec=d},Prefix'{end,vec=e},Prefix'{end,vec=f})
  where (a,b,c,d,e,f) = V.unzip6 vec
unzip7 :: Prefix n (a,b,c,d,e,f,g) -> (Prefix n a, Prefix n b, Prefix n c, Prefix n d, Prefix n e, Prefix n f, Prefix n g)
unzip7 Prefix'{end,vec} = (Prefix'{end,vec=a},Prefix'{end,vec=b},Prefix'{end,vec=c},Prefix'{end,vec=d},Prefix'{end,vec=e},Prefix'{end,vec=f},Prefix'{end,vec=g})
  where (a,b,c,d,e,f,g) = V.unzip7 vec


-- postscanl = ??? --TODO
-- postscanl1 = ???
-- postscanr = ???
-- postscanr1 = ???

-- mapAccumL = ???
-- mapAccumR = ???

-- seqV = ???
-- forceV = ???
-- forceVX = ???

-- autoReg
-- TODO

-- SHOCKWAVES
-- TODO
