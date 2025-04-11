{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}


module Tydi.Data.Slice (
  Slice,pattern Slice,
  slice,
  start,end,
  strobed,unsafeToVec,
  Zippable(..)
 ) where

import Clash.Explicit.Prelude hiding (
  slice,
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
import qualified Tydi.Data.Prefix as P
import           Tydi.Data.Prefix (Zippable,VLength,Zipped,zip,zipWith)
import qualified Tydi.Data.Range as R
import           Tydi.Data.Range hiding (full,start,end,range)
import           Data.Foldable (foldr)

-- closed interval, non-empty slice of a vector

data Slice n a where
  Slice' :: {range::Range n,vec::Vec n a} -> Slice n a

pattern Slice :: (KnownNat n,n~n0+1) => Range n -> Vec n a -> Slice n a
pattern Slice range vec <- Slice'{range,vec}  where
  Slice range vec = slice range vec
{-# COMPLETE Slice #-}


deriving instance (KnownNat n,Lift a) => Lift (Slice n a)
-- deriving instance Data (Slice n a) -- TODO
-- deriving instance Generic (Slice n a) -- TODO
deriving instance Bundle (Slice n a)

slice :: (KnownNat n) => Range n -> Vec n a -> Slice n a
slice r v = Slice' r (zipWith (\i x -> if R.contains r i then x else undefined) indicesI v)

strobed :: KnownNat n => Slice n a -> Vec n (Maybe a)
strobed (Slice' (Range s e) v) = zipWith (\i x -> if s<=i && i<=e then Just x else Nothing) indicesI v

unsafeToVec :: Slice n a -> Vec n a
unsafeToVec Slice'{vec} = vec

toSlice :: (KnownNat n,n~n0+1) => P.Prefix n a -> Slice n a
toSlice (P.Prefix e vec) = Slice' (Range 0 e) vec


start :: Slice n a -> Index n
start Slice'{range} = R.start range
end :: Slice n a -> Index n
end   Slice'{range} = R.end range

-- full -> slice 0 (n-1)
-- align -> aligns slice so start is at 0
-- sliceLength -> (e-s+1)

-- define !!, head, last, map, zipWithX, zipX, unzipX,foldr,foldl,foldr1,foldl1,fold, replace,reverse,length
-- bundle, unbundle



--rewrite using strobed
instance (Show a,KnownNat n) => Show (Slice n a) where
  showsPrec n Slice'{range,vec} = case vec of
    Nil -> showString "Nil"
    vs -> showParen (n > 5) (go 0 vs)

   where
    go :: Index n -> Vec m a -> ShowS
    go _ Nil = showString "Nil"
    go i (x `Cons` xs) =
        (if R.contains range i then showsPrec (5 + 1) x else showString "-")
      . showString " :> "
      . go (i+1) xs

instance (ShowX a,KnownNat n) => ShowX (Slice n a) where
  showsPrecX n vs =
    case isX vs of
      Right Slice'{vec=Nil} -> showString "Nil"
      Left _ -> showString "undefined"
      Right Slice'{range,vec} -> showParen (n > 5) (go range 0 vec)
   where
    go :: Range n -> Index n -> Vec m a -> ShowS
    go _ _ (isX -> Left _) = showString "undefined"
    go _ _ Nil = showString "Nil"
    go r i (x `Cons` xs) =
        (if R.contains r i then showsPrecX (5 + 1) x else showString "-")
      . showString " :> "
      . go r (i+1) xs


full :: (KnownNat n,n~n0+1) => Vec n a -> Slice n a
full v = Slice'{range=R.full,vec=v}

-- (Lift, derived)

instance  (KnownNat n) => Functor (Slice n) where
  fmap = map

instance  (KnownNat n,n~n+1) => Applicative (Slice n) where
  pure x = full $ pure x
  (<*>) = zipWith ($)

instance (KnownNat n) => Foldable (Slice n) where
  foldr f z Slice'{range,vec} = foldr# f z 0 range vec

foldr# :: (KnownNat m) => (a -> b -> b) -> b -> Index m -> Range m -> Vec n a -> b
foldr# _ z _ _ Nil           = z -- should not be possible
foldr# f z i r@(Range s e) (x `Cons` xs) | i<s       = foldr# f z (i+1) r xs
                                         | i<=e      = f x z
                                         | otherwise = f x (foldr# f z (i+1) r xs)

instance (KnownNat n) => Traversable (Slice n) where --TODO: what is this supposed to do?
  traverse f Slice'{range=range@(Range s e),vec} = slice range <$> traverse# f 0 s e vec

{-# CLASH_OPAQUE traverse# #-}
-- {-# ANN traverse# hasBlackBox #-}
traverse# :: forall a f b n m . (Applicative f, KnownNat n, KnownNat m) => (a -> f b) -> Index m -> Index m -> Index m -> Vec n a -> f (Vec n b)
traverse# _ _ _ _ Nil           = pure Nil
traverse# f i s e (x `Cons` xs) | i<s = Cons <$> f undefined <*> traverse# f (i+1) s e xs
                                | i<=e  = Cons <$> f x <*> traverse# f (i+1) s e xs
                                | otherwise = pure $ repeat undefined

instance (KnownNat n, Eq a) => Eq (Slice n a) where
  (==) p@Slice'{} q@Slice'{} = and (zipWith (==) p q)

instance (KnownNat n, Ord a) => Ord (Slice n a) where
  compare x y = foldr f EQ $ zipWith compare x y
    where f EQ   keepGoing = keepGoing
          f done _         = done


--Data? derive?



-- Generic (derive)

instance (KnownNat n,Semigroup a) => Semigroup (Slice n a) where
  (<>) = zipWith (<>)

instance  (KnownNat n,n~n0+1,Monoid a) => Monoid (Slice n a) where
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



maxLength :: KnownNat n => Slice n a -> Int
maxLength Slice'{vec} = length vec
maxLengthS :: KnownNat n => Slice n a -> SNat n
maxLengthS Slice'{vec} = lengthS vec

(!!) :: (KnownNat n, Enum i) => Slice n a -> i -> a
Slice'{range,vec} !! i = if R.contains range i' then vec V.!! i else errorX "Index out of slice range"
  where i' = fromIntegral $ fromEnum i
{-# INLINE (!!) #-}

(!!?) :: (KnownNat n, Enum i) => Slice n a -> i -> Maybe a
Slice'{range,vec} !!? i = if R.contains range i' then Just $ vec V.!! i else Nothing
  where i' = fromIntegral $ fromEnum i
{-# INLINE (!!?) #-}

head :: KnownNat n => Slice (n + 1) a -> a
head Slice'{range=Range s _,vec} =  vec V.!! s

last :: KnownNat n => Slice n a -> a
last Slice'{range=Range _ e,vec} = vec V.!! e

at :: forall (n :: Natural) (m :: Natural) (m0 :: Natural) a. (KnownNat m,n+1<=m,n+1+m0~m) => SNat n -> Slice m a -> Maybe a
at n Slice'{range=Range s e,vec} = if s<=i && i<=e then Just $ V.at n vec else Nothing
  where i = fromSNat n

-- indices :: t -> Prefix n (Index n)
-- indices n = full $ V.indices n

-- incidesI = full $ indicesI

findIndex :: KnownNat n => (a -> Bool) -> Slice n a -> Maybe (Index n)
findIndex f = ifoldr (\i a b -> if f a then Just i else b) Nothing

ifoldr ::  (KnownNat n) => (Index n -> a -> b -> b) -> b -> Slice n a -> b
ifoldr f b xs = foldr (uncurry f) b (zip V.indicesI xs)

elemIndex :: (KnownNat n, Eq a) => a -> Slice n a -> Maybe (Index n)
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
subSlice :: KnownNat n => Range n -> Slice n a -> Slice n a -- not safe! range may be empty!
subSlice (Range s e) Slice'{range=Range start end,vec} = slice (Range (max s start) (min e end)) vec

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

shiftIn :: KnownNat n => Slice n a -> a -> Slice n a
shiftIn Slice'{range=range@(Range start end),vec} x = Slice'{range,vec=V.replace start x (undefined +>> vec)}
-- prepend ::  (KnownNat n) => Slice n a -> a -> Slice (n+1) a
-- prepend Prefix{end,vec} x = Prefix{end=end',vec=x:>vec} --prefix only
--   where end' = (unpack $ resize $ pack end) + 1

replace :: (KnownNat n, Enum i) => i -> a -> Slice n a -> Slice n a
replace i y p@Slice'{range,vec} = Slice'{range,vec=V.replace i y' vec}
  where y' = if R.contains range $ fromIntegral (fromEnum i) then y else undefined

--reverse = --slice only

-- shiftLeft = ??? -- slice only?
-- shiftRight = ??? -- slice only?

map :: (KnownNat n) => (a -> b) -> Slice n a -> Slice n b
map f Slice'{range,vec} = slice range $ fmap f vec
-- imap = ??? --TODO
-- smap = ??? --TODO

--zip,3,4,5,6,7
type instance P.VLength (Slice n a) = n


-- unsafe!
instance (KnownNat n) => Zippable (Slice n a) (Slice n b) where
  type Zipped (Slice n a) (Slice n b) = Slice n (a,b)
  zip Slice'{range,vec} Slice'{range=range',vec=vec'} = subSlice range $ slice range' $ V.zip vec vec'
instance (KnownNat n,n~n0+1) => Zippable (Slice n a) (P.Prefix n b) where
  type Zipped (Slice n a) (P.Prefix n b) = Slice n (a,b)
  zip s p = zip s (toSlice p)
instance (KnownNat n,n~n0+1) => Zippable (P.Prefix n a) (Slice n b) where
  type Zipped (P.Prefix n a) (Slice n b) = Slice n (a,b)
  zip p = zip (toSlice p)

-- safe
instance (KnownNat n) => Zippable (Slice n a) (Vec n b) where
  type Zipped (Slice n a) (Vec n b) = Slice n (a,b)
  zip Slice'{range,vec} v = slice range $ V.zip vec v
instance (KnownNat n) => Zippable (Vec n a) (Slice n b) where
  type Zipped (Vec n a) (Slice n b) = Slice n (a,b)
  zip v Slice'{range,vec} = slice range $ V.zip v vec


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

unzip  :: Slice n (a,b) -> (Slice n a, Slice n b)
unzip  Slice'{range,vec} = (Slice'{range,vec=a},Slice'{range,vec=b})
  where (a,b) = V.unzip vec
unzip3 :: Slice n (a,b,c) -> (Slice n a, Slice n b, Slice n c)
unzip3 Slice'{range,vec} = (Slice'{range,vec=a},Slice'{range,vec=b},Slice'{range,vec=c})
  where (a,b,c) = V.unzip3 vec
unzip4 :: Slice n (a,b,c,d) -> (Slice n a, Slice n b, Slice n c, Slice n d)
unzip4 Slice'{range,vec} = (Slice'{range,vec=a},Slice'{range,vec=b},Slice'{range,vec=c},Slice'{range,vec=d})
  where (a,b,c,d) = V.unzip4 vec
unzip5 :: Slice n (a,b,c,d,e) -> (Slice n a, Slice n b, Slice n c, Slice n d, Slice n e)
unzip5 Slice'{range,vec} = (Slice'{range,vec=a},Slice'{range,vec=b},Slice'{range,vec=c},Slice'{range,vec=d},Slice'{range,vec=e})
  where (a,b,c,d,e) = V.unzip5 vec
unzip6 :: Slice n (a,b,c,d,e,f) -> (Slice n a, Slice n b, Slice n c, Slice n d, Slice n e, Slice n f)
unzip6 Slice'{range,vec} = (Slice'{range,vec=a},Slice'{range,vec=b},Slice'{range,vec=c},Slice'{range,vec=d},Slice'{range,vec=e},Slice'{range,vec=f})
  where (a,b,c,d,e,f) = V.unzip6 vec
unzip7 :: Slice n (a,b,c,d,e,f,g) -> (Slice n a, Slice n b, Slice n c, Slice n d, Slice n e, Slice n f, Slice n g)
unzip7 Slice'{range,vec} = (Slice'{range,vec=a},Slice'{range,vec=b},Slice'{range,vec=c},Slice'{range,vec=d},Slice'{range,vec=e},Slice'{range,vec=f},Slice'{range,vec=g})
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
