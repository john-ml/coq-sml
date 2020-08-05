(** val pythag : Int.int -> Int.int -> Int.int -> bool mL **)

let pythag a b c =
  (fun x k -> k x)
    ((fun x k -> k x) ((fun x k -> k x) ((fun x y -> x * y) a a) (fun x -> (fun x -> x) ((fun x y -> x + y) x)))
      (fun f -> (fun x k -> k x) ((fun x y -> x * y) b b) (fun x -> (fun x -> x) (f x)))) (fun ab ->
    (fun x k -> k x) ((fun x k -> k x) ((fun x y -> x * y) c c) (fun x -> (fun x -> x) ((fun x y -> x = y) x)))
      (fun f -> (fun x k -> k x) ab (fun x -> (fun x -> x) (f x))))
