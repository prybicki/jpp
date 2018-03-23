import Prelude hiding(Either(..))
data Either a b = Left a | Right b

-- *Main> let x = Left "a"
-- *Main> :type x
-- x :: Either [Char] b

instance Functor (Either e) where
  fmap f (Left l) = Left l
  fmap f (Right r) = Right (f r)

reverseRight = fmap reverse

class Functor f => Pointed f where
  pure :: a -> ta

  instance Pointed Tree


-- 4 opcjonalnie do domku
