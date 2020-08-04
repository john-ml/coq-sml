let prod_uncurry f x y =
  f (Coq_pair (x, y))
