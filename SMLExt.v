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

Definition lift2 {A B C} (f : A -> B -> C) (ma : ML.ML A) (mb : ML.ML B) : ML.ML C :=
  ML.bind ma (fun x => ML.bind mb (fun y => ML.ret (f x y))).

Recursive Extraction lift2.
