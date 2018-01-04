{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Binary.IPS
    ( IPS
    , patch
    ) where

-------------------------------------------------------------------------------
import Prelude hiding (drop, splitAt)

-------------------------------------------------------------------------------
import Control.Applicative (some)
import Control.Monad.Fail (MonadFail)
import Data.Bits (Bits (..))
import Data.Semigroup ((<>))
import Data.Word (Word8, Word16, Word32)

-------------------------------------------------------------------------------
import Data.Binary (Binary (..))
import Data.Binary.Get (Get, getByteString, getWord8, getWord16be)
import Data.Binary.Put (putByteString, putWord8, putWord16be)
import qualified Data.ByteString as S (ByteString, replicate)
import qualified Data.ByteString.Lazy as L
    (ByteString
    , drop
    , fromStrict
    , splitAt
    )

-------------------------------------------------------------------------------
-- Should I support the version where after EOF there can be a 24 bit max file
-- size?

-------------------------------------------------------------------------------
data Header = Header deriving (Show, Eq)

-------------------------------------------------------------------------------
instance Binary Header where
    put _ = putByteString "PATCH"
    get = getByteString 5 >>= \case
        "PATCH" -> return Header
        _ -> fail "expected PATCH"

-------------------------------------------------------------------------------
data Footer = Footer deriving (Show, Eq)

-------------------------------------------------------------------------------
instance Binary Footer where
    put _ = putByteString "EOF"
    get = getByteString 3 >>= \case
        "EOF" -> return Footer
        _ -> fail "expected EOF"

-------------------------------------------------------------------------------
-- Inveriant (upheld by get): 0 <= Offset <= 2^24-1
-- Stored big-endian
newtype Offset = Offset Word32
    deriving (Show, Eq, Ord, Enum, Num, Real, Integral, Bits)

-------------------------------------------------------------------------------
instance Binary Offset where
    get = do
        x <- getWord8
        y <- getWord8
        z <- getWord8
        return (fromByte x 2 .|. fromByte y 1 .|. fromByte z 0)

    put (Offset n) = do
        putWord8 (toByte n 2)
        putWord8 (toByte n 1)
        putWord8 (toByte n 0)

-------------------------------------------------------------------------------
toByte :: (Bits a, Integral a) => a -> Int -> Word8
toByte n i = fromIntegral (n `shiftR` (8*i) .&. 0xff)

-------------------------------------------------------------------------------
fromByte :: (Bits a, Integral a) => Word8 -> Int -> a
fromByte n i = fromIntegral n `shiftL` (8*i)

-------------------------------------------------------------------------------
-- Invariant (upheld by get): 0 < Size <= 2^16-1
-- Stored big-endian
newtype Size = Size Word16
    deriving (Show, Eq, Ord, Enum, Num, Real, Integral, Bits)

-------------------------------------------------------------------------------
instance Binary Size where
    get = getWord16be >>= \case
        0 -> fail "size must be >0"
        n -> return  (Size n)

    put (Size n) = putWord16be n

-------------------------------------------------------------------------------
data Patch
    -- Lit invariant (upheld by get): Size == length(ByteString)
    = Lit !Offset !Size !S.ByteString
    | RLE !Offset !Size !Word8
    deriving (Show)

-------------------------------------------------------------------------------
instance Binary Patch where
    get = do
        offset <- get
        getWord16be >>= \case
            0 -> getRLEPatch offset
            n -> getLitPatch offset (Size n)

    put (Lit offset size value) = do
        put offset
        put size
        putByteString value
    put (RLE offset size value) = do
        put offset
        putWord16be 0
        put size
        putWord8 value

-------------------------------------------------------------------------------
getRLEPatch :: Offset -> Get Patch
getRLEPatch offset = do
    size <- get
    value <- getWord8
    return (RLE offset size value)

-------------------------------------------------------------------------------
getLitPatch :: Offset -> Size -> Get Patch
getLitPatch offset size = do
    value <- getByteString (fromIntegral size)
    return (Lit offset size value)

-------------------------------------------------------------------------------
-- Invariant (upheld by get): list is sorted by offset, no patches overlap
newtype IPS = IPS [Patch] deriving Show

-------------------------------------------------------------------------------
instance Binary IPS where
    get = do
        _ <- get :: Get Header
        patches <- some (get :: Get Patch)
        -- sort patches myself to make it more liberal in what it accepts?
        validPatches <- validated patches
        _ <- get :: Get Footer
        return (IPS validPatches)
    put (IPS patches) = do
        put Header
        mapM_ put patches
        put Footer

-------------------------------------------------------------------------------
-- Return a bytestring generated from a base bytestring and with the IPS
-- patches applied
patch :: IPS -> L.ByteString -> L.ByteString
patch (IPS patches) = run0 patches where
    run0 [] bytes = bytes
    run0 (p:ps) bytes
        | 0 == offset p = run (p:ps) (drop (size p) bytes)
        | otherwise = bs' <> run (p:ps) (drop (size p) bs'')
        where
            (bs', bs'') = splitAt (offset p) bytes

    -- Invariant: beginning of bytestring = data after first patch in arg list
    run [] bytes = bytes
    run [p] bytes = lvalue p <> bytes
    -- Invariant gap p q >= 0
    run (p0:p1:ps) bytes
        | gap p0 p1 == 0 =
            lvalue p0 <> run (p1:ps) (drop (size p1) bytes)
        | otherwise =
            lvalue p0 <> bs' <> run (p1:ps) (drop (size p1) bs'')
        where
            (bs', bs'') = splitAt (gap p0 p1) bytes

    drop = L.drop . fromIntegral

    splitAt :: Integral a => a -> L.ByteString -> (L.ByteString, L.ByteString)
    splitAt = L.splitAt . fromIntegral

    offset (Lit o _ _) = o
    offset (RLE o _ _) = o

    size (Lit _ s _) = s
    size (RLE _ s _) = s

    lvalue = L.fromStrict . value
    value (Lit _ _ x) = x
    value (RLE _ (Size n) x) = S.replicate (fromIntegral n) x

-------------------------------------------------------------------------------
validated :: MonadFail m => [Patch] -> m [Patch]
validated [] = return []
validated (p:ps) = go (p:ps) ps where
    go (x:xs) (y:ys)
        | gap x y >= 0 = fmap (x :) (go xs ys)
        | otherwise = fail "Patches are not sorted"
    go [x] _ = return [x]
    go _ _ = return []

-------------------------------------------------------------------------------
gap :: Patch -> Patch -> Int
gap (Lit o0 s0 _) (Lit o1 _ _) = gap' o1 o0 s0
gap (Lit o0 s0 _) (RLE o1 _ _) = gap' o1 o0 s0
gap (RLE o0 s0 _) (Lit o1 _ _) = gap' o1 o0 s0
gap (RLE o0 s0 _) (RLE o1 _ _) = gap' o1 o0 s0

-------------------------------------------------------------------------------
gap' :: (Integral a, Integral b, Integral c, Integral d) => a -> b -> c -> d
gap' a b c = fromIntegral a - (fromIntegral b + fromIntegral c)
