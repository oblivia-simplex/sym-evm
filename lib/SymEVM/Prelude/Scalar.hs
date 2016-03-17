module SymEVM.Prelude.Scalar
       (Scalar, toScalar, fromScalar, S256, toS256, fromS256, S5, toS5,
        fromS5)
       where

newtype Scalar = Scalar
    { fromScalar :: Integer
    } deriving (Show,Eq,Ord)

toScalar :: Integer -> Maybe Scalar
toScalar n = 
    if n > 0
        then Just $
             Scalar
             { fromScalar = n
             }
        else Nothing

newtype S256 = S256
    { fromS256 :: Integer
    } deriving (Show,Eq,Ord)

toS256 :: Integer -> Maybe S256
toS256 n = 
    if 0 < n && n < 2 ^ 256
        then Just $
             S256
             { fromS256 = n
             }
        else Nothing

newtype S5 = S5
    { fromS5 :: Integer
    } deriving (Show,Eq,Ord)

toS5 :: Integer -> Maybe S5
toS5 n = 
    if 0 < n && n < 2 ^ 5
        then Just $
             S5
             { fromS5 = n
             }
        else Nothing
