
let test_program pre f x y post =
  (fun x k -> k x)
    ((fun x k -> k x) pre (fun _ ->
      (fun x k -> k x)
        ((fun x k -> k x) ((fun x k -> k x) x (fun x0 -> (fun x -> x) (f x0))) (fun f0 ->
          (fun x k -> k x) y (fun x0 -> (fun x -> x) (f0 x0)))) (fun x -> x))) (fun x0 ->
    (fun x k -> k x) post (fun _ -> (fun x -> x) x0))
