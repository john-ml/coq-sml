
let test_program f x y =
  (fun x k -> k x) ((fun x k -> k x) x (fun x0 -> (fun x -> x) (f x0))) (fun f0 ->
    (fun x k -> k x) y (fun x0 -> (fun x -> x) (f0 x0)))
