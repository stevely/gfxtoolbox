{-
 - AutoStorable.hs
 - By Steven Smith
 -}

{-# LANGUAGE TypeFamilies #-}

module Foreign.AutoStorable (
AutoStorable (..),
autoSizeOf,
autoAlignment,
autoPeek,
autoPoke,
T1 (..),
T2 (..),
T3 (..),
T4 (..),
T5 (..),
T6 (..),
T7 (..),
T8 (..)
) where

import Foreign.Storable
import Foreign.Ptr
import Data.List (foldl')
import Control.Monad.State.Strict

class AutoStorable a where
    type Tuple a
    toTuple :: a -> Tuple a
    fromTuple :: Tuple a -> a

autoSizeOf :: (AutoStorable a, Storable b, b ~ Tuple a) => a -> Int
autoSizeOf = sizeOf . toTuple

autoAlignment :: (AutoStorable a, Storable b, b ~ Tuple a) => a -> Int
autoAlignment = alignment . toTuple

autoPeek :: (AutoStorable a, Storable b, b ~ Tuple a) => Ptr a -> IO a
autoPeek ptr = fmap fromTuple (peek (castPtr ptr `asTypeOf` wrap (toTuple (unwrap ptr))))

autoPoke :: (AutoStorable a, Storable b, b ~ Tuple a) => Ptr a -> a -> IO ()
autoPoke ptr foo = poke (castPtr ptr `asTypeOf` wrap (toTuple foo)) (toTuple foo)

unwrap :: Ptr a -> a
unwrap = undefined

wrap :: a -> Ptr a
wrap = undefined

{-
peekWithOffset :: Storable b => Ptr a -> b -> IO (b, Ptr b)
peekWithOffset ptr b = do
    b' <- peek ptr'
    return (b', (ptr' `plusPtr` sizeOf b) `asTypeOf` wrap b)
  where
    ptr' = castPtr ptr `asTypeOf` wrap b

pokeWithOffset :: Storable b => Ptr a -> b -> IO (Ptr b)
pokeWithOffset ptr b = do
    poke ptr' b
    return ((ptr' `plusPtr` sizeOf b) `asTypeOf` wrap b)
  where
    ptr' = castPtr ptr `asTypeOf` wrap b

pokeAs :: Storable b => Ptr a -> b -> IO ()
pokeAs ptr b = do
    poke ptr' b
  where
    ptr' = castPtr ptr `asTypeOf` wrap b
-}

peekShift :: Storable b => b -> StateT (Ptr a) IO b
peekShift b = do
    ptr <- get
    b' <- lift $ peek (castPtr ptr `asTypeOf` wrap b)
    put (ptr `plusPtr` sizeOf b)
    return b'

pokeShift :: Storable b => b -> StateT (Ptr a) IO ()
pokeShift b = do
    ptr <- get
    lift $ poke (castPtr ptr `asTypeOf` wrap b) b
    put (ptr `plusPtr` sizeOf b)

runPeek :: Monad m => Ptr a -> StateT (Ptr a) m a -> m a
runPeek ptr st = evalStateT st ptr

runPoke :: Monad m => Ptr a -> StateT (Ptr a) m x -> m ()
runPoke ptr st = liftM (const ()) $ runStateT st ptr

data T1 a = T1 a
data T2 a b = T2 a b
data T3 a b c = T3 a b c
data T4 a b c d = T4 a b c d
data T5 a b c d e = T5 a b c d e
data T6 a b c d e f = T6 a b c d e f
data T7 a b c d e f g = T7 a b c d e f g
data T8 a b c d e f g h = T8 a b c d e f g h

instance Storable a => Storable (T1 a) where
    sizeOf ~(T1 a) = sizeOf a
    alignment ~(T1 a) = alignment a
    peek ptr = runPeek ptr $ do
        a <- peekShift a_
        return (T1 a)
      where
        ~(T1 a_) = unwrap ptr
    poke ptr (T1 a) = runPoke ptr $ do
        pokeShift a

instance (Storable a, Storable b) => Storable (T2 a b) where
    sizeOf ~(T2 a b) = sizeOf a + sizeOf b
    alignment ~(T2 a _) = alignment a
    peek ptr = runPeek ptr $ do
        a <- peekShift a_
        b <- peekShift b_
        return (T2 a b)
      where
        ~(T2 a_ b_) = unwrap ptr
    poke ptr (T2 a b) = runPoke ptr $ do
        pokeShift a
        pokeShift b

instance (Storable a, Storable b, Storable c) => Storable (T3 a b c) where
    sizeOf ~(T3 a b c) = sizeOf a + sizeOf b + sizeOf c
    alignment ~(T3 a _ _) = alignment a
    peek ptr = runPeek ptr $ do
        a <- peekShift a_
        b <- peekShift b_
        c <- peekShift c_
        return (T3 a b c)
      where
        ~(T3 a_ b_ c_) = unwrap ptr
    poke ptr (T3 a b c) = runPoke ptr $ do
        pokeShift a
        pokeShift b
        pokeShift c

instance (Storable a, Storable b, Storable c, Storable d)
    => Storable (T4 a b c d) where
    sizeOf ~(T4 a b c d) = sizeOf a + sizeOf b + sizeOf c + sizeOf d
    alignment ~(T4 a _ _ _) = alignment a
    peek ptr = runPeek ptr $ do
        a <- peekShift a_
        b <- peekShift b_
        c <- peekShift c_
        d <- peekShift d_
        return (T4 a b c d)
      where
        ~(T4 a_ b_ c_ d_) = unwrap ptr
    poke ptr (T4 a b c d) = runPoke ptr $ do
        pokeShift a
        pokeShift b
        pokeShift c
        pokeShift d

instance (Storable a, Storable b, Storable c, Storable d, Storable e)
    => Storable (T5 a b c d e) where
    sizeOf ~(T5 a b c d e) =
        sizeOf a + sizeOf b + sizeOf c + sizeOf d + sizeOf e
    alignment ~(T5 a _ _ _ _) = alignment a
    peek ptr = runPeek ptr $ do
        a <- peekShift a_
        b <- peekShift b_
        c <- peekShift c_
        d <- peekShift d_
        e <- peekShift e_
        return (T5 a b c d e)
      where
        ~(T5 a_ b_ c_ d_ e_) = unwrap ptr
    poke ptr (T5 a b c d e) = runPoke ptr $ do
        pokeShift a
        pokeShift b
        pokeShift c
        pokeShift d
        pokeShift e

instance
    (Storable a, Storable b, Storable c, Storable d, Storable e, Storable f)
    => Storable (T6 a b c d e f) where
    sizeOf ~(T6 a b c d e f) =
        sizeOf a + sizeOf b + sizeOf c + sizeOf d + sizeOf e + sizeOf f
    alignment ~(T6 a _ _ _ _ _) = alignment a
    peek ptr = runPeek ptr $ do
        a <- peekShift a_
        b <- peekShift b_
        c <- peekShift c_
        d <- peekShift d_
        e <- peekShift e_
        f <- peekShift f_
        return (T6 a b c d e f)
      where
        ~(T6 a_ b_ c_ d_ e_ f_) = unwrap ptr
    poke ptr (T6 a b c d e f) = runPoke ptr $ do
        pokeShift a
        pokeShift b
        pokeShift c
        pokeShift d
        pokeShift e
        pokeShift f

instance
    (Storable a, Storable b, Storable c, Storable d, Storable e, Storable f,
     Storable g)
    => Storable (T7 a b c d e f g) where
    sizeOf ~(T7 a b c d e f g) =
        sizeOf a + sizeOf b + sizeOf c + sizeOf d + sizeOf e + sizeOf f +
        sizeOf g
    alignment ~(T7 a _ _ _ _ _ _) = alignment a
    peek ptr = runPeek ptr $ do
        a <- peekShift a_
        b <- peekShift b_
        c <- peekShift c_
        d <- peekShift d_
        e <- peekShift e_
        f <- peekShift f_
        g <- peekShift g_
        return (T7 a b c d e f g)
      where
        ~(T7 a_ b_ c_ d_ e_ f_ g_) = unwrap ptr
    poke ptr (T7 a b c d e f g) = runPoke ptr $ do
        pokeShift a
        pokeShift b
        pokeShift c
        pokeShift d
        pokeShift e
        pokeShift f
        pokeShift g

instance
    (Storable a, Storable b, Storable c, Storable d, Storable e, Storable f,
     Storable g, Storable h)
    => Storable (T8 a b c d e f g h) where
    sizeOf ~(T8 a b c d e f g h) =
        sizeOf a + sizeOf b + sizeOf c + sizeOf d + sizeOf e + sizeOf f +
        sizeOf g + sizeOf h
    alignment ~(T8 a _ _ _ _ _ _ _) = alignment a
    peek ptr = runPeek ptr $ do
        a <- peekShift a_
        b <- peekShift b_
        c <- peekShift c_
        d <- peekShift d_
        e <- peekShift e_
        f <- peekShift f_
        g <- peekShift g_
        h <- peekShift h_
        return (T8 a b c d e f g h)
      where
        ~(T8 a_ b_ c_ d_ e_ f_ g_ h_) = unwrap ptr
    poke ptr (T8 a b c d e f g h) = runPoke ptr $ do
        pokeShift a
        pokeShift b
        pokeShift c
        pokeShift d
        pokeShift e
        pokeShift f
        pokeShift g
        pokeShift h

{-
instance (Storable a, Storable b) => Storable (a,b) where
    sizeOf ~(a,b) = sizeOf a + sizeOf b
    alignment ~(a,_) = alignment a
    peek ptr1 = do
        (a, ptr2) <- peekWithOffset ptr1 a_
        (b, _   ) <- peekWithOffset ptr2 b_
        return (a,b)
      where
        ~(a_,b_) = unwrap ptr1
    poke ptr1 (a,b) = do
        ptr2 <- pokeWithOffset ptr1 a
        pokeAs ptr2 b

instance (Storable a, Storable b, Storable c) => Storable (a,b,c) where
    sizeOf ~(a,b,c) = sizeOf a + sizeOf b + sizeOf c
    alignment ~(a,_,_) = alignment a
    peek ptr1 = do
        (a, ptr2) <- peekWithOffset ptr1 a_
        (b, ptr3) <- peekWithOffset ptr2 b_
        (c, _   ) <- peekWithOffset ptr3 c_
        return (a,b,c)
      where
        ~(a_,b_,c_) = unwrap ptr1
    poke ptr1 (a,b,c) = do
        ptr2 <- pokeWithOffset ptr1 a
        ptr3 <- pokeWithOffset ptr2 b
        pokeAs ptr3 c

instance (Storable a, Storable b, Storable c, Storable d)
    => Storable (a,b,c,d) where
    sizeOf ~(a,b,c,d) = sizeOf a + sizeOf b + sizeOf c + sizeOf d
    alignment ~(a,_,_,_) = alignment a
    peek ptr1 = do
        (a, ptr2) <- peekWithOffset ptr1 a_
        (b, ptr3) <- peekWithOffset ptr2 b_
        (c, ptr4) <- peekWithOffset ptr3 c_
        (d, _   ) <- peekWithOffset ptr4 d_
        return (a,b,c,d)
      where
        ~(a_,b_,c_,d_) = unwrap ptr1
    poke ptr1 (a,b,c,d) = do
        ptr2 <- pokeWithOffset ptr1 a
        ptr3 <- pokeWithOffset ptr2 b
        ptr4 <- pokeWithOffset ptr3 c
        pokeAs ptr4 d

instance (Storable a, Storable b, Storable c, Storable d, Storable e)
    => Storable (a,b,c,d,e) where
    sizeOf ~(a,b,c,d,e) = sum [sizeOf a, sizeOf b, sizeOf c, sizeOf d, sizeOf e]
    alignment ~(a,_,_,_,_) = alignment a
    peek ptr1 = do
        (a, ptr2) <- peekWithOffset ptr1 a_
        (b, ptr3) <- peekWithOffset ptr2 b_
        (c, ptr4) <- peekWithOffset ptr3 c_
        (d, ptr5) <- peekWithOffset ptr4 d_
        (e, _   ) <- peekWithOffset ptr5 e_
        return (a,b,c,d,e)
      where
        ~(a_,b_,c_,d_,e_) = unwrap ptr1
    poke ptr1 (a,b,c,d,e) = do
        ptr2 <- pokeWithOffset ptr1 a
        ptr3 <- pokeWithOffset ptr2 b
        ptr4 <- pokeWithOffset ptr3 c
        ptr5 <- pokeWithOffset ptr4 d
        pokeAs ptr5 e

instance (Storable a, Storable b, Storable c, Storable d, Storable e,
    Storable f) => Storable (a,b,c,d,e,f) where
    sizeOf ~(a,b,c,d,e,f) = sum [sizeOf a, sizeOf b, sizeOf c, sizeOf d,
        sizeOf e, sizeOf f]
    alignment ~(a,_,_,_,_,_) = alignment a
    peek ptr1 = do
        (a, ptr2) <- peekWithOffset ptr1 a_
        (b, ptr3) <- peekWithOffset ptr2 b_
        (c, ptr4) <- peekWithOffset ptr3 c_
        (d, ptr5) <- peekWithOffset ptr4 d_
        (e, ptr6) <- peekWithOffset ptr5 e_
        (f, _   ) <- peekWithOffset ptr6 f_
        return (a,b,c,d,e,f)
      where
        ~(a_,b_,c_,d_,e_,f_) = unwrap ptr1
    poke ptr1 (a,b,c,d,e,f) = do
        ptr2 <- pokeWithOffset ptr1 a
        ptr3 <- pokeWithOffset ptr2 b
        ptr4 <- pokeWithOffset ptr3 c
        ptr5 <- pokeWithOffset ptr4 d
        ptr6 <- pokeWithOffset ptr5 e
        pokeAs ptr6 f

instance (Storable a, Storable b, Storable c, Storable d, Storable e,
    Storable f, Storable g) => Storable (a,b,c,d,e,f,g) where
    sizeOf ~(a,b,c,d,e,f,g) = sum [sizeOf a, sizeOf b, sizeOf c, sizeOf d,
        sizeOf e, sizeOf f, sizeOf g]
    alignment ~(a,_,_,_,_,_,_) = alignment a
    peek ptr1 = do
        (a, ptr2) <- peekWithOffset ptr1 a_
        (b, ptr3) <- peekWithOffset ptr2 b_
        (c, ptr4) <- peekWithOffset ptr3 c_
        (d, ptr5) <- peekWithOffset ptr4 d_
        (e, ptr6) <- peekWithOffset ptr5 e_
        (f, ptr7) <- peekWithOffset ptr6 f_
        (g, _   ) <- peekWithOffset ptr7 g_
        return (a,b,c,d,e,f,g)
      where
        ~(a_,b_,c_,d_,e_,f_,g_) = unwrap ptr1
    poke ptr1 (a,b,c,d,e,f,g) = do
        ptr2 <- pokeWithOffset ptr1 a
        ptr3 <- pokeWithOffset ptr2 b
        ptr4 <- pokeWithOffset ptr3 c
        ptr5 <- pokeWithOffset ptr4 d
        ptr6 <- pokeWithOffset ptr5 e
        ptr7 <- pokeWithOffset ptr6 f
        pokeAs ptr7 g

instance (Storable a, Storable b, Storable c, Storable d, Storable e,
    Storable f, Storable g, Storable h) => Storable (a,b,c,d,e,f,g,h) where
    sizeOf ~(a,b,c,d,e,f,g,h) = sum [sizeOf a, sizeOf b, sizeOf c, sizeOf d,
        sizeOf e, sizeOf f, sizeOf g, sizeOf h]
    alignment ~(a,_,_,_,_,_,_,_) = alignment a
    peek ptr1 = do
        (a, ptr2) <- peekWithOffset ptr1 a_
        (b, ptr3) <- peekWithOffset ptr2 b_
        (c, ptr4) <- peekWithOffset ptr3 c_
        (d, ptr5) <- peekWithOffset ptr4 d_
        (e, ptr6) <- peekWithOffset ptr5 e_
        (f, ptr7) <- peekWithOffset ptr6 f_
        (g, ptr8) <- peekWithOffset ptr7 g_
        (h, _   ) <- peekWithOffset ptr8 h_
        return (a,b,c,d,e,f,g,h)
      where
        ~(a_,b_,c_,d_,e_,f_,g_,h_) = unwrap ptr1
    poke ptr1 (a,b,c,d,e,f,g,h) = do
        ptr2 <- pokeWithOffset ptr1 a
        ptr3 <- pokeWithOffset ptr2 b
        ptr4 <- pokeWithOffset ptr3 c
        ptr5 <- pokeWithOffset ptr4 d
        ptr6 <- pokeWithOffset ptr5 e
        ptr7 <- pokeWithOffset ptr6 f
        ptr8 <- pokeWithOffset ptr7 g
        pokeAs ptr8 h
-}

-- Test type

{-
data Foo = Foo Int Double

instance AutoStorable Foo where
    type Tuple Foo = (Int, Double)
    toTuple (Foo x y) = (x,y)
    fromTuple (x,y) = Foo x y

instance Storable Foo where
    sizeOf = autoSizeOf
    alignment = autoAlignment
    peek = autoPeek
    poke = autoPoke
-}
