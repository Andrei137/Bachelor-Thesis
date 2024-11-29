{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Types where
import MCPrelude

-- Set 1
newtype Gen a = Gen { runGen :: Seed -> (a, Seed) }

evalGen :: Gen a -> Seed -> a
evalGen g s = fst $ runGen g s

-- Set 2
data Maybe a = Nothing | Just a

instance Show a => Show (Maybe a) where
    -- show :: Maybe a -> String
    show Nothing = "Nothing"
    show (Just a) = "Just " ++ show a

instance Eq a => Eq (Maybe a) where
    -- (==) :: Maybe a -> Maybe a -> Bool
    Nothing == Nothing = True
    Just a == Just b = a == b
    _ == _ = False

-- Set 3
data Card = Card Int String

instance Show Card where
    show (Card rank suit) = show rank ++ suit
