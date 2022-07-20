module Misc.Compose where 

(-.) :: (a -> c -> d) -> (a -> b -> c) -> (a -> b -> d)
f -. g = \x -> f x . g x

(--.) :: (a -> b -> d -> e) -> (a -> b -> c -> d) -> (a -> b -> c -> e)
f --. g = \x y -> f x y . g x y