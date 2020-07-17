module LectureEx where

import Control.Applicative

-- Exercises suggested in lecture
(*>) :: Applicative f => f a -> f b -> f b
fa *> fb = (id <$ fa) <*> fb

mapA :: Applicative f => (a -> f b) -> ([a] -> f [b])
mapA f = sequenceA . map f

sequenceA' :: Applicative f => [f a] -> f [a]
sequenceA' = foldr (liftA2 (:)) (pure [])

replicateA :: Applicative f => Int -> f a -> f [a]
replicateA = fmap . replicate
