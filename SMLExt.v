Require Extraction.

Module Type SigML.
  Parameter ML : Type -> Type.
  Parameter ret : forall {A}, A -> ML A.
  Parameter bind : forall {A B}, ML A -> (A -> ML B) -> ML B.
  (* TODO: applicative operators and monad laws *)
End SigML.

Module ML : SigML.
  Definition ML : Type -> Type := fun A => A.
  Definition ret {A} (x : A) : ML A := x.
  Definition bind {A B} (x : ML A) (f : A -> ML B) : ML B := f x.
End ML.

Axiom int : Type.
Axiom zero : int.
Axiom three : int.

Module Array.
  Parameter array : Type -> Type.
  Parameter maxLen : int.
  Parameter array' : forall {A}, int -> A -> array A.
  Parameter update : forall {A}, array A -> int -> A -> ML.ML unit.
End Array.

Definition lift2 {A B C} (f : A -> B -> C) (ma : ML.ML A) (mb : ML.ML B) : ML.ML C :=
  ML.bind ma (fun x => ML.bind mb (fun y => ML.ret (f x y))).

Recursive Extraction lift2.

Definition a_program : ML.ML unit := Array.update (Array.array' three 0) zero 1.

Extract Inlined Constant int => "int".
Extract Inlined Constant zero => "0".
Extract Inlined Constant three => "3".
Extract Constant Array.array "'a" => "'a Array.array".
Extract Inlined Constant Array.array' => "(fun x y -> Array.array (x, y))".
Extract Inlined Constant Array.update => "(fun a i x -> Array.update (a, i, x))".

Recursive Extraction a_program.
