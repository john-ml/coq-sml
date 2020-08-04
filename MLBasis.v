Require Extraction.

(* Some ML operations are impure, and thus must be encapsulated in a monad *)
Axiom ML : Type -> Type.
Axiom ml_ret : forall {A}, A -> ML A.
Axiom ml_bind : forall {A B}, ML A -> (A -> ML B) -> ML B.
Extract Constant ML "'a" => "'a".
Extract Inlined Constant ml_ret => "(fun x -> x)".
Extract Inlined Constant ml_bind => "(fun x k -> k x)".

Axiom int : Type.
Axiom zero : int.
Axiom three : int.
Extract Inlined Constant int => "int".
Extract Inlined Constant zero => "0".
Extract Inlined Constant three => "3".

(* Arrays *)
Module Array.
Axiom array : Type -> Type.
Axiom maxLen : int.
Axiom array' : forall {A}, int -> A -> array A.
Axiom update : forall {A}, array A -> int -> A -> ML unit.
Extract Constant array "'a" => "'a Array.array".
Extract Inlined Constant array' => "(fun x y -> Array.array (x, y))".
Extract Inlined Constant update => "(fun a i x -> Array.update (a, i, x))".
End Array.

(* Some tests *)

Definition lift2 {A B C} (f : A -> B -> C) (ma : ML A) (mb : ML B) : ML C :=
  ml_bind ma (fun x => ml_bind mb (fun y => ml_ret (f x y))).
Recursive Extraction lift2.

Definition a_program : ML unit := Array.update (Array.array' three 0) zero 1.
Recursive Extraction a_program.
