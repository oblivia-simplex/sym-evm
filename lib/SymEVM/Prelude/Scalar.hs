module SymEVM.Prelude.Scalar (Scalar, toScalar, fromScalar) where

newtype Scalar = Scalar { fromScalar :: Integer } deriving ( Show, Eq, Ord )

toScalar :: Integer -> Maybe Scalar
toScalar n =
    if n > 0 then
        Just $ Scalar { fromScalar = n }
    else
        Nothing

