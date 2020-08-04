type __ = Obj.t
let __ = let rec f _ = Obj.repr f in Obj.repr f

type coq_Empty_set = unit (* empty inductive *)

(** val coq_Empty_set_rect : coq_Empty_set -> 'a1 **)

let coq_Empty_set_rect _ =
  assert false (* absurd case *)

(** val coq_Empty_set_rec : coq_Empty_set -> 'a1 **)

let coq_Empty_set_rec _ =
  assert false (* absurd case *)

type coq_unit =
| Coq_tt

(** val unit_rect : 'a1 -> coq_unit -> 'a1 **)

let unit_rect f _ =
  f

(** val unit_rec : 'a1 -> coq_unit -> 'a1 **)

let unit_rec f _ =
  f

type bool =
| Coq_true
| Coq_false

(** val bool_rect : 'a1 -> 'a1 -> bool -> 'a1 **)

let bool_rect f f0 = function
| Coq_true -> f
| Coq_false -> f0

(** val bool_rec : 'a1 -> 'a1 -> bool -> 'a1 **)

let bool_rec f f0 = function
| Coq_true -> f
| Coq_false -> f0

(** val andb : bool -> bool -> bool **)

let andb b1 b2 =
  match b1 with
  | Coq_true -> b2
  | Coq_false -> Coq_false

(** val orb : bool -> bool -> bool **)

let orb b1 b2 =
  match b1 with
  | Coq_true -> Coq_true
  | Coq_false -> b2

(** val implb : bool -> bool -> bool **)

let implb b1 b2 =
  match b1 with
  | Coq_true -> b2
  | Coq_false -> Coq_true

(** val xorb : bool -> bool -> bool **)

let xorb b1 b2 =
  match b1 with
  | Coq_true -> (match b2 with
                 | Coq_true -> Coq_false
                 | Coq_false -> Coq_true)
  | Coq_false -> b2

(** val negb : bool -> bool **)

let negb = function
| Coq_true -> Coq_false
| Coq_false -> Coq_true

(** val eq_true_rect : 'a1 -> bool -> 'a1 **)

let eq_true_rect f _ =
  f

(** val eq_true_rec : 'a1 -> bool -> 'a1 **)

let eq_true_rec f _ =
  f

(** val eq_true_rec_r : bool -> 'a1 -> 'a1 **)

let eq_true_rec_r _ h =
  h

(** val eq_true_rect_r : bool -> 'a1 -> 'a1 **)

let eq_true_rect_r _ h =
  h

type nat =
| O
| S of nat

(** val nat_rect : 'a1 -> (nat -> 'a1 -> 'a1) -> nat -> 'a1 **)

let rec nat_rect f f0 = function
| O -> f
| S n0 -> f0 n0 (nat_rect f f0 n0)

(** val nat_rec : 'a1 -> (nat -> 'a1 -> 'a1) -> nat -> 'a1 **)

let rec nat_rec f f0 = function
| O -> f
| S n0 -> f0 n0 (nat_rec f f0 n0)

type 'a option =
| Some of 'a
| None

(** val option_rect : ('a1 -> 'a2) -> 'a2 -> 'a1 option -> 'a2 **)

let option_rect f f0 = function
| Some x -> f x
| None -> f0

(** val option_rec : ('a1 -> 'a2) -> 'a2 -> 'a1 option -> 'a2 **)

let option_rec f f0 = function
| Some x -> f x
| None -> f0

(** val option_map : ('a1 -> 'a2) -> 'a1 option -> 'a2 option **)

let option_map f = function
| Some a -> Some (f a)
| None -> None

type ('a, 'b) sum =
| Coq_inl of 'a
| Coq_inr of 'b

(** val sum_rect : ('a1 -> 'a3) -> ('a2 -> 'a3) -> ('a1, 'a2) sum -> 'a3 **)

let sum_rect f f0 = function
| Coq_inl x -> f x
| Coq_inr x -> f0 x

(** val sum_rec : ('a1 -> 'a3) -> ('a2 -> 'a3) -> ('a1, 'a2) sum -> 'a3 **)

let sum_rec f f0 = function
| Coq_inl x -> f x
| Coq_inr x -> f0 x

type ('a, 'b) prod =
| Coq_pair of 'a * 'b

(** val prod_rect : ('a1 -> 'a2 -> 'a3) -> ('a1, 'a2) prod -> 'a3 **)

let prod_rect f = function
| Coq_pair (x, x0) -> f x x0

(** val prod_rec : ('a1 -> 'a2 -> 'a3) -> ('a1, 'a2) prod -> 'a3 **)

let prod_rec f = function
| Coq_pair (x, x0) -> f x x0

(** val fst : ('a1, 'a2) prod -> 'a1 **)

let fst = function
| Coq_pair (x, _) -> x

(** val snd : ('a1, 'a2) prod -> 'a2 **)

let snd = function
| Coq_pair (_, y) -> y

(** val prod_uncurry : (('a1, 'a2) prod -> 'a3) -> 'a1 -> 'a2 -> 'a3 **)

let prod_uncurry f x y =
  f (Coq_pair (x, y))

(** val prod_curry : ('a1 -> 'a2 -> 'a3) -> ('a1, 'a2) prod -> 'a3 **)

let prod_curry f = function
| Coq_pair (x, y) -> f x y

type 'a list =
| Coq_nil
| Coq_cons of 'a * 'a list

(** val list_rect : 'a2 -> ('a1 -> 'a1 list -> 'a2 -> 'a2) -> 'a1 list -> 'a2 **)

let rec list_rect f f0 = function
| Coq_nil -> f
| Coq_cons (y, l0) -> f0 y l0 (list_rect f f0 l0)

(** val list_rec : 'a2 -> ('a1 -> 'a1 list -> 'a2 -> 'a2) -> 'a1 list -> 'a2 **)

let rec list_rec f f0 = function
| Coq_nil -> f
| Coq_cons (y, l0) -> f0 y l0 (list_rec f f0 l0)

(** val length : 'a1 list -> nat **)

let rec length = function
| Coq_nil -> O
| Coq_cons (_, l') -> S (length l')

(** val app : 'a1 list -> 'a1 list -> 'a1 list **)

let rec app l m =
  match l with
  | Coq_nil -> m
  | Coq_cons (a, l1) -> Coq_cons (a, (app l1 m))

type comparison =
| Eq
| Lt
| Gt

(** val comparison_rect : 'a1 -> 'a1 -> 'a1 -> comparison -> 'a1 **)

let comparison_rect f f0 f1 = function
| Eq -> f
| Lt -> f0
| Gt -> f1

(** val comparison_rec : 'a1 -> 'a1 -> 'a1 -> comparison -> 'a1 **)

let comparison_rec f f0 f1 = function
| Eq -> f
| Lt -> f0
| Gt -> f1

(** val coq_CompOpp : comparison -> comparison **)

let coq_CompOpp = function
| Eq -> Eq
| Lt -> Gt
| Gt -> Lt

type coq_CompareSpecT =
| CompEqT
| CompLtT
| CompGtT

(** val coq_CompareSpecT_rect :
    (__ -> 'a1) -> (__ -> 'a1) -> (__ -> 'a1) -> comparison -> coq_CompareSpecT -> 'a1 **)

let coq_CompareSpecT_rect f f0 f1 _ = function
| CompEqT -> f __
| CompLtT -> f0 __
| CompGtT -> f1 __

(** val coq_CompareSpecT_rec : (__ -> 'a1) -> (__ -> 'a1) -> (__ -> 'a1) -> comparison -> coq_CompareSpecT -> 'a1 **)

let coq_CompareSpecT_rec f f0 f1 _ = function
| CompEqT -> f __
| CompLtT -> f0 __
| CompGtT -> f1 __

(** val coq_CompareSpec2Type : comparison -> coq_CompareSpecT **)

let coq_CompareSpec2Type = function
| Eq -> CompEqT
| Lt -> CompLtT
| Gt -> CompGtT

type 'a coq_CompSpecT = coq_CompareSpecT

(** val coq_CompSpec2Type : 'a1 -> 'a1 -> comparison -> 'a1 coq_CompSpecT **)

let coq_CompSpec2Type _ _ =
  coq_CompareSpec2Type

(** val identity_rect : 'a1 -> 'a2 -> 'a1 -> 'a2 **)

let identity_rect _ f _ =
  f

(** val identity_rec : 'a1 -> 'a2 -> 'a1 -> 'a2 **)

let identity_rec _ f _ =
  f

type coq_ID = __ -> __ -> __

(** val id : 'a1 -> 'a1 **)

let id x =
  x

type 'a coq_sig = 'a
  (* singleton inductive, whose constructor was exist *)

(** val sig_rect : ('a1 -> __ -> 'a2) -> 'a1 -> 'a2 **)

let sig_rect f s =
  f s __

(** val sig_rec : ('a1 -> __ -> 'a2) -> 'a1 -> 'a2 **)

let sig_rec f s =
  f s __

type 'a sig2 = 'a
  (* singleton inductive, whose constructor was exist2 *)

(** val sig2_rect : ('a1 -> __ -> __ -> 'a2) -> 'a1 sig2 -> 'a2 **)

let sig2_rect f s =
  f s __ __

(** val sig2_rec : ('a1 -> __ -> __ -> 'a2) -> 'a1 sig2 -> 'a2 **)

let sig2_rec f s =
  f s __ __

type ('a, 'p) sigT =
| Coq_existT of 'a * 'p

(** val sigT_rect : ('a1 -> 'a2 -> 'a3) -> ('a1, 'a2) sigT -> 'a3 **)

let sigT_rect f = function
| Coq_existT (x, x0) -> f x x0

(** val sigT_rec : ('a1 -> 'a2 -> 'a3) -> ('a1, 'a2) sigT -> 'a3 **)

let sigT_rec f = function
| Coq_existT (x, x0) -> f x x0

type ('a, 'p, 'q) sigT2 =
| Coq_existT2 of 'a * 'p * 'q

(** val sigT2_rect : ('a1 -> 'a2 -> 'a3 -> 'a4) -> ('a1, 'a2, 'a3) sigT2 -> 'a4 **)

let sigT2_rect f = function
| Coq_existT2 (x, x0, x1) -> f x x0 x1

(** val sigT2_rec : ('a1 -> 'a2 -> 'a3 -> 'a4) -> ('a1, 'a2, 'a3) sigT2 -> 'a4 **)

let sigT2_rec f = function
| Coq_existT2 (x, x0, x1) -> f x x0 x1

(** val proj1_sig : 'a1 -> 'a1 **)

let proj1_sig e =
  e

(** val sig_of_sig2 : 'a1 sig2 -> 'a1 **)

let sig_of_sig2 x =
  x

(** val projT1 : ('a1, 'a2) sigT -> 'a1 **)

let projT1 = function
| Coq_existT (a, _) -> a

(** val projT2 : ('a1, 'a2) sigT -> 'a2 **)

let projT2 = function
| Coq_existT (_, h) -> h

(** val sigT_of_sigT2 : ('a1, 'a2, 'a3) sigT2 -> ('a1, 'a2) sigT **)

let sigT_of_sigT2 x =
  Coq_existT ((let Coq_existT2 (a, _, _) = x in a), (let Coq_existT2 (_, p, _) = x in p))

(** val projT3 : ('a1, 'a2, 'a3) sigT2 -> 'a3 **)

let projT3 = function
| Coq_existT2 (_, _, c) -> c

(** val sig_of_sigT : ('a1, __) sigT -> 'a1 **)

let sig_of_sigT =
  projT1

(** val sigT_of_sig : 'a1 -> ('a1, __) sigT **)

let sigT_of_sig x =
  Coq_existT (x, __)

(** val sig2_of_sigT2 : ('a1, __, __) sigT2 -> 'a1 sig2 **)

let sig2_of_sigT2 x =
  projT1 (sigT_of_sigT2 x)

(** val sigT2_of_sig2 : 'a1 sig2 -> ('a1, __, __) sigT2 **)

let sigT2_of_sig2 x =
  Coq_existT2 ((sig_of_sig2 x), __, __)

(** val eq_sigT_rect : ('a1, 'a2) sigT -> ('a1, 'a2) sigT -> (__ -> __ -> 'a3) -> 'a3 **)

let eq_sigT_rect _ _ f =
  f __ __

(** val eq_sigT_rec : ('a1, 'a2) sigT -> ('a1, 'a2) sigT -> (__ -> __ -> 'a3) -> 'a3 **)

let eq_sigT_rec =
  eq_sigT_rect

(** val eq_sig_rect : 'a1 -> 'a1 -> (__ -> __ -> 'a2) -> 'a2 **)

let eq_sig_rect _ _ f =
  f __ __

(** val eq_sig_rec : 'a1 -> 'a1 -> (__ -> __ -> 'a2) -> 'a2 **)

let eq_sig_rec =
  eq_sig_rect

(** val eq_sigT2_rect : ('a1, 'a2, 'a3) sigT2 -> ('a1, 'a2, 'a3) sigT2 -> (__ -> __ -> __ -> 'a4) -> 'a4 **)

let eq_sigT2_rect _ _ f =
  f __ __ __

(** val eq_sigT2_rec : ('a1, 'a2, 'a3) sigT2 -> ('a1, 'a2, 'a3) sigT2 -> (__ -> __ -> __ -> 'a4) -> 'a4 **)

let eq_sigT2_rec =
  eq_sigT2_rect

(** val eq_sig2_rect : 'a1 sig2 -> 'a1 sig2 -> (__ -> __ -> __ -> 'a2) -> 'a2 **)

let eq_sig2_rect _ _ f =
  f __ __ __

(** val eq_sig2_rec : 'a1 sig2 -> 'a1 sig2 -> (__ -> __ -> __ -> 'a2) -> 'a2 **)

let eq_sig2_rec =
  eq_sig2_rect

type sumbool =
| Coq_left
| Coq_right

(** val sumbool_rect : (__ -> 'a1) -> (__ -> 'a1) -> sumbool -> 'a1 **)

let sumbool_rect f f0 = function
| Coq_left -> f __
| Coq_right -> f0 __

(** val sumbool_rec : (__ -> 'a1) -> (__ -> 'a1) -> sumbool -> 'a1 **)

let sumbool_rec f f0 = function
| Coq_left -> f __
| Coq_right -> f0 __

type 'a sumor =
| Coq_inleft of 'a
| Coq_inright

(** val sumor_rect : ('a1 -> 'a2) -> (__ -> 'a2) -> 'a1 sumor -> 'a2 **)

let sumor_rect f f0 = function
| Coq_inleft x -> f x
| Coq_inright -> f0 __

(** val sumor_rec : ('a1 -> 'a2) -> (__ -> 'a2) -> 'a1 sumor -> 'a2 **)

let sumor_rec f f0 = function
| Coq_inleft x -> f x
| Coq_inright -> f0 __

(** val coq_Choice : ('a1 -> 'a2) -> ('a1 -> 'a2) **)

let coq_Choice h =
  h

(** val coq_Choice2 : ('a1 -> ('a2, 'a3) sigT) -> ('a1 -> 'a2, 'a1 -> 'a3) sigT **)

let coq_Choice2 h =
  Coq_existT ((fun z -> projT1 (h z)), (fun z -> let s = h z in let Coq_existT (_, r) = s in r))

(** val bool_choice : ('a1 -> sumbool) -> ('a1 -> bool) **)

let bool_choice h z =
  match h z with
  | Coq_left -> Coq_true
  | Coq_right -> Coq_false

(** val dependent_choice : ('a1 -> 'a1) -> 'a1 -> (nat -> 'a1) **)

let rec dependent_choice h x0 = function
| O -> x0
| S n' -> h (dependent_choice h x0 n')

type 'a coq_Exc = 'a option

(** val value : 'a1 -> 'a1 option **)

let value x =
  Some x

(** val error : 'a1 option **)

let error =
  None

(** val except : __ -> 'a1 **)

let except _ =
  assert false (* absurd case *)

(** val absurd_set : __ -> 'a1 **)

let absurd_set _ =
  assert false (* absurd case *)

module ListNotations =
 struct
 end

(** val hd : 'a1 -> 'a1 list -> 'a1 **)

let hd default = function
| Coq_nil -> default
| Coq_cons (x, _) -> x

(** val hd_error : 'a1 list -> 'a1 option **)

let hd_error = function
| Coq_nil -> None
| Coq_cons (x, _) -> Some x

(** val tl : 'a1 list -> 'a1 list **)

let tl = function
| Coq_nil -> Coq_nil
| Coq_cons (_, m) -> m

(** val destruct_list : 'a1 list -> ('a1, 'a1 list) sigT sumor **)

let destruct_list = function
| Coq_nil -> Coq_inright
| Coq_cons (y, l0) -> Coq_inleft (Coq_existT (y, l0))

(** val in_dec : ('a1 -> 'a1 -> sumbool) -> 'a1 -> 'a1 list -> sumbool **)

let rec in_dec h a = function
| Coq_nil -> Coq_right
| Coq_cons (y, l0) -> let s = h y a in (match s with
                                        | Coq_left -> Coq_left
                                        | Coq_right -> in_dec h a l0)

(** val nth : nat -> 'a1 list -> 'a1 -> 'a1 **)

let rec nth n l default =
  match n with
  | O -> (match l with
          | Coq_nil -> default
          | Coq_cons (x, _) -> x)
  | S m -> (match l with
            | Coq_nil -> default
            | Coq_cons (_, t) -> nth m t default)

(** val nth_ok : nat -> 'a1 list -> 'a1 -> bool **)

let rec nth_ok n l default =
  match n with
  | O -> (match l with
          | Coq_nil -> Coq_false
          | Coq_cons (_, _) -> Coq_true)
  | S m -> (match l with
            | Coq_nil -> Coq_false
            | Coq_cons (_, t) -> nth_ok m t default)

(** val nth_in_or_default : nat -> 'a1 list -> 'a1 -> sumbool **)

let rec nth_in_or_default n l d =
  match l with
  | Coq_nil -> Coq_right
  | Coq_cons (_, l0) -> (match n with
                         | O -> Coq_left
                         | S n0 -> nth_in_or_default n0 l0 d)

(** val nth_error : 'a1 list -> nat -> 'a1 option **)

let rec nth_error l = function
| O -> (match l with
        | Coq_nil -> None
        | Coq_cons (x, _) -> Some x)
| S n0 -> (match l with
           | Coq_nil -> None
           | Coq_cons (_, l0) -> nth_error l0 n0)

(** val nth_default : 'a1 -> 'a1 list -> nat -> 'a1 **)

let nth_default default l n =
  match nth_error l n with
  | Some x -> x
  | None -> default

(** val remove : ('a1 -> 'a1 -> sumbool) -> 'a1 -> 'a1 list -> 'a1 list **)

let rec remove eq_dec x = function
| Coq_nil -> Coq_nil
| Coq_cons (y, tl0) ->
  (match eq_dec x y with
   | Coq_left -> remove eq_dec x tl0
   | Coq_right -> Coq_cons (y, (remove eq_dec x tl0)))

(** val last : 'a1 list -> 'a1 -> 'a1 **)

let rec last l d =
  match l with
  | Coq_nil -> d
  | Coq_cons (a, l0) -> (match l0 with
                         | Coq_nil -> a
                         | Coq_cons (_, _) -> last l0 d)

(** val removelast : 'a1 list -> 'a1 list **)

let rec removelast = function
| Coq_nil -> Coq_nil
| Coq_cons (a, l0) -> (match l0 with
                       | Coq_nil -> Coq_nil
                       | Coq_cons (_, _) -> Coq_cons (a, (removelast l0)))

(** val exists_last : 'a1 list -> ('a1 list, 'a1) sigT **)

let rec exists_last = function
| Coq_nil -> assert false (* absurd case *)
| Coq_cons (y, l0) ->
  (match l0 with
   | Coq_nil -> Coq_existT (Coq_nil, y)
   | Coq_cons (_, _) -> let s = exists_last l0 in let Coq_existT (l', s0) = s in Coq_existT ((Coq_cons (y, l')), s0))

(** val count_occ : ('a1 -> 'a1 -> sumbool) -> 'a1 list -> 'a1 -> nat **)

let rec count_occ eq_dec l x =
  match l with
  | Coq_nil -> O
  | Coq_cons (y, tl0) -> let n = count_occ eq_dec tl0 x in (match eq_dec y x with
                                                            | Coq_left -> S n
                                                            | Coq_right -> n)

(** val rev : 'a1 list -> 'a1 list **)

let rec rev = function
| Coq_nil -> Coq_nil
| Coq_cons (x, l') -> app (rev l') (Coq_cons (x, Coq_nil))

(** val rev_append : 'a1 list -> 'a1 list -> 'a1 list **)

let rec rev_append l l' =
  match l with
  | Coq_nil -> l'
  | Coq_cons (a, l0) -> rev_append l0 (Coq_cons (a, l'))

(** val rev' : 'a1 list -> 'a1 list **)

let rev' l =
  rev_append l Coq_nil

(** val concat : 'a1 list list -> 'a1 list **)

let rec concat = function
| Coq_nil -> Coq_nil
| Coq_cons (x, l0) -> app x (concat l0)

(** val list_eq_dec : ('a1 -> 'a1 -> sumbool) -> 'a1 list -> 'a1 list -> sumbool **)

let rec list_eq_dec eq_dec l l' =
  match l with
  | Coq_nil -> (match l' with
                | Coq_nil -> Coq_left
                | Coq_cons (_, _) -> Coq_right)
  | Coq_cons (y, l0) ->
    (match l' with
     | Coq_nil -> Coq_right
     | Coq_cons (a0, l1) -> (match eq_dec y a0 with
                             | Coq_left -> list_eq_dec eq_dec l0 l1
                             | Coq_right -> Coq_right))

(** val map : ('a1 -> 'a2) -> 'a1 list -> 'a2 list **)

let rec map f = function
| Coq_nil -> Coq_nil
| Coq_cons (a, t) -> Coq_cons ((f a), (map f t))

(** val flat_map : ('a1 -> 'a2 list) -> 'a1 list -> 'a2 list **)

let rec flat_map f = function
| Coq_nil -> Coq_nil
| Coq_cons (x, t) -> app (f x) (flat_map f t)

(** val fold_left : ('a1 -> 'a2 -> 'a1) -> 'a2 list -> 'a1 -> 'a1 **)

let rec fold_left f l a0 =
  match l with
  | Coq_nil -> a0
  | Coq_cons (b, t) -> fold_left f t (f a0 b)

(** val fold_right : ('a2 -> 'a1 -> 'a1) -> 'a1 -> 'a2 list -> 'a1 **)

let rec fold_right f a0 = function
| Coq_nil -> a0
| Coq_cons (b, t) -> f b (fold_right f a0 t)

(** val list_power : 'a1 list -> 'a2 list -> ('a1, 'a2) prod list list **)

let rec list_power l l' =
  match l with
  | Coq_nil -> Coq_cons (Coq_nil, Coq_nil)
  | Coq_cons (x, t) -> flat_map (fun f -> map (fun y -> Coq_cons ((Coq_pair (x, y)), f)) l') (list_power t l')

(** val existsb : ('a1 -> bool) -> 'a1 list -> bool **)

let rec existsb f = function
| Coq_nil -> Coq_false
| Coq_cons (a, l0) -> (match f a with
                       | Coq_true -> Coq_true
                       | Coq_false -> existsb f l0)

(** val forallb : ('a1 -> bool) -> 'a1 list -> bool **)

let rec forallb f = function
| Coq_nil -> Coq_true
| Coq_cons (a, l0) -> (match f a with
                       | Coq_true -> forallb f l0
                       | Coq_false -> Coq_false)

(** val filter : ('a1 -> bool) -> 'a1 list -> 'a1 list **)

let rec filter f = function
| Coq_nil -> Coq_nil
| Coq_cons (x, l0) -> (match f x with
                       | Coq_true -> Coq_cons (x, (filter f l0))
                       | Coq_false -> filter f l0)

(** val find : ('a1 -> bool) -> 'a1 list -> 'a1 option **)

let rec find f = function
| Coq_nil -> None
| Coq_cons (x, tl0) -> (match f x with
                        | Coq_true -> Some x
                        | Coq_false -> find f tl0)

(** val partition : ('a1 -> bool) -> 'a1 list -> ('a1 list, 'a1 list) prod **)

let rec partition f = function
| Coq_nil -> Coq_pair (Coq_nil, Coq_nil)
| Coq_cons (x, tl0) ->
  let Coq_pair (g, d) = partition f tl0 in
  (match f x with
   | Coq_true -> Coq_pair ((Coq_cons (x, g)), d)
   | Coq_false -> Coq_pair (g, (Coq_cons (x, d))))

(** val split : ('a1, 'a2) prod list -> ('a1 list, 'a2 list) prod **)

let rec split = function
| Coq_nil -> Coq_pair (Coq_nil, Coq_nil)
| Coq_cons (p, tl0) ->
  let Coq_pair (x, y) = p in
  let Coq_pair (left, right) = split tl0 in Coq_pair ((Coq_cons (x, left)), (Coq_cons (y, right)))

(** val combine : 'a1 list -> 'a2 list -> ('a1, 'a2) prod list **)

let rec combine l l' =
  match l with
  | Coq_nil -> Coq_nil
  | Coq_cons (x, tl0) ->
    (match l' with
     | Coq_nil -> Coq_nil
     | Coq_cons (y, tl') -> Coq_cons ((Coq_pair (x, y)), (combine tl0 tl')))

(** val list_prod : 'a1 list -> 'a2 list -> ('a1, 'a2) prod list **)

let rec list_prod l l' =
  match l with
  | Coq_nil -> Coq_nil
  | Coq_cons (x, t) -> app (map (fun y -> Coq_pair (x, y)) l') (list_prod t l')

(** val firstn : nat -> 'a1 list -> 'a1 list **)

let rec firstn n l =
  match n with
  | O -> Coq_nil
  | S n0 -> (match l with
             | Coq_nil -> Coq_nil
             | Coq_cons (a, l0) -> Coq_cons (a, (firstn n0 l0)))

(** val skipn : nat -> 'a1 list -> 'a1 list **)

let rec skipn n l =
  match n with
  | O -> l
  | S n0 -> (match l with
             | Coq_nil -> Coq_nil
             | Coq_cons (_, l0) -> skipn n0 l0)

(** val nodup : ('a1 -> 'a1 -> sumbool) -> 'a1 list -> 'a1 list **)

let rec nodup decA = function
| Coq_nil -> Coq_nil
| Coq_cons (x, xs) ->
  (match in_dec decA x xs with
   | Coq_left -> nodup decA xs
   | Coq_right -> Coq_cons (x, (nodup decA xs)))

(** val seq : nat -> nat -> nat list **)

let rec seq start = function
| O -> Coq_nil
| S len0 -> Coq_cons (start, (seq (S start) len0))

(** val coq_Exists_dec : 'a1 list -> ('a1 -> sumbool) -> sumbool **)

let rec coq_Exists_dec l pdec =
  match l with
  | Coq_nil -> Coq_right
  | Coq_cons (y, l0) -> (match coq_Exists_dec l0 pdec with
                         | Coq_left -> Coq_left
                         | Coq_right -> pdec y)

(** val coq_Forall_rect : 'a2 -> ('a1 -> 'a1 list -> __ -> 'a2) -> 'a1 list -> 'a2 **)

let coq_Forall_rect h h' = function
| Coq_nil -> h
| Coq_cons (y, l0) -> h' y l0 __

(** val coq_Forall_dec : ('a1 -> sumbool) -> 'a1 list -> sumbool **)

let rec coq_Forall_dec pdec = function
| Coq_nil -> Coq_left
| Coq_cons (y, l0) -> (match coq_Forall_dec pdec l0 with
                       | Coq_left -> pdec y
                       | Coq_right -> Coq_right)

(** val coq_Forall_Exists_dec : ('a1 -> sumbool) -> 'a1 list -> sumbool **)

let coq_Forall_Exists_dec =
  coq_Forall_dec

(** val repeat : 'a1 -> nat -> 'a1 list **)

let rec repeat x = function
| O -> Coq_nil
| S k -> Coq_cons (x, (repeat x k))

let ev x =
  let rec ev0 = function
    | O -> Coq_true
    | S n -> negb (odd n)
  and odd = function
    | O -> Coq_false
    | S n -> negb (ev0 n)
  in ev0 x

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

(** val lift2 : ('a1 -> 'a2 -> 'a3) -> 'a1 ML.coq_ML -> 'a2 ML.coq_ML -> 'a3 ML.coq_ML **)

let lift2 f ma mb =
  ML.bind ma (fun x -> ML.bind mb (fun y -> ML.ret (f x y)))

