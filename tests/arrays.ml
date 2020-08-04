type unit0 =
| Tt

type nat =
| O
| S of nat

module type SigML =
 sig
  type 'x coq_ML

  val ret : 'a1 -> 'a1 coq_ML

  val bind : 'a1 coq_ML -> ('a1 -> 'a2 coq_ML) -> 'a2 coq_ML
 end

module ML =
 struct
  type 'x coq_ML = 'x

  (** val ret : 'a1 -> 'a1 coq_ML **)

  let ret x =
    x

  (** val bind : 'a1 coq_ML -> ('a1 -> 'a2 coq_ML) -> 'a2 coq_ML **)

  let bind x f =
    f x
 end

module Array =
 struct
  type 'a array = 'a Array.array
 end

(** val a_program : unit0 ML.coq_ML **)

let a_program =
  (fun a i x -> Array.update (a, i, x)) ((fun x y -> Array.array (x, y)) 3 O) 0 (S O)
