Require Extraction.

(** * ML monad *)

Axiom ML : Type -> Set.
Axiom ml_ret : forall {A}, A -> ML A.
Axiom ml_bind : forall {A B}, ML A -> (A -> ML B) -> ML B.
Extract Constant ML "'a" => "'a".
Extract Inlined Constant ml_ret => "(fun x -> x)".
Extract Inlined Constant ml_bind => "(fun x k -> k x)".

Module MLNotation.
Notation "'ret!'" := ml_ret.
Notation "'let!' x ':=' c1 'in' c2" := (@ml_bind _ _ c1 (fun x => c2))
  (at level 61, c1 at next level, right associativity).
Notation "'let!' ' p ':=' c1 'in' c2" :=
  (@ml_bind _ _ c1 (fun x => match x with p => c2 end))
  (at level 61, p pattern, c1 at next level, right associativity).
Notation "f '=<<' m" := (ml_bind _ _ m f) (at level 53, right associativity).
Notation "f '<$>' m" := (let! x := m in ret! (f x)) (at level 52, left associativity).
Notation "mf '<*>' mx" := (let! f := mf in let! x := mx in ret! (f x)) (at level 52, left associativity).
Notation "mx '<*' my" := (let! x := mx in let! y := my in ret! x) (at level 52, left associativity).
Notation "mx '*>' my" := (let! x := mx in let! y := my in ret! y) (at level 52, left associativity).
End MLNotation.

Module TestMLNotation.
Import MLNotation.
Definition test_program {A B C D E} (pre : ML A) (f : B -> C -> ML D)
           (x : ML B) (y : ML C) (post : ML E) :=
  pre *> (f <$> x <*> y) <* post.
Extraction test_program.
End TestMLNotation.

(** * Booleans, pairs, options *)

Extract Inductive bool => "bool" [ "true" "false" ].
Extract Inductive prod => "(*)" [ "(fun x y -> (x, y))" ] "(fun k (x, y) -> k x y)".
Extract Inductive option => "option" [ "SOME" "NONE" ].

(** * Integers *)

Module Type INTEGER.
  Axiom int : Set.
  Axiom zero one : int.
  Axiom minInt maxInt : option int.
  Axiom add mul sub div mod quot rem : int -> int -> ML int.
  Axiom lt le gt ge : int -> int -> bool.
  Axiom neg abs : int -> int.
  Axiom min max : int -> int -> int.
  Axiom sameSign : int -> int -> bool.
End INTEGER.

Module Int <: INTEGER.
  Axiom int : Set.
  Axiom zero one : int.
  Axiom minInt maxInt : option int.
  Axiom add mul sub div mod quot rem : int -> int -> ML int.
  Axiom lt le eq gt ge : int -> int -> bool.
  Axiom neg abs : int -> int.
  Axiom min max : int -> int -> int.
  Axiom sameSign : int -> int -> bool.
  Extract Inlined Constant int => "Int.int".
  Extract Inlined Constant zero => "0".
  Extract Inlined Constant one => "1".
  Extract Inlined Constant minInt => "Int.minInt".
  Extract Inlined Constant maxInt => "Int.maxInt".
  Extract Inlined Constant add => "(fun x y -> x + y)".
  Extract Inlined Constant mul => "(fun x y -> x * y)".
  Extract Inlined Constant sub => "(fun x y -> x - y)".
  Extract Inlined Constant div => "(fun x y -> x / y)".
  Extract Inlined Constant mod => "(fun x y -> x mod y)".
  Extract Inlined Constant quot => "(fun x y -> x quot y)".
  Extract Inlined Constant rem => "(fun x y -> x rem y)".
  Extract Inlined Constant lt => "(fun x y -> x < y)".
  Extract Inlined Constant le => "(fun x y -> x <= y)".
  Extract Inlined Constant eq => "(fun x y -> x = y)".
  Extract Inlined Constant gt => "(fun x y -> x > y)".
  Extract Inlined Constant ge => "(fun x y -> x >= y)".
  Extract Inlined Constant neg => "(fun x -> ~x)".
  Extract Inlined Constant abs => "Int.abs".
  Extract Inlined Constant min => "(fun x y -> Int.min (x, y))".
  Extract Inlined Constant max => "(fun x y -> Int.max (x, y))".
  Extract Inlined Constant sameSign => "(fun x y -> Int.sameSign (x, y))".
End Int.

Module TestInt.
Import MLNotation.
Definition pythag (a b c : Int.int) : ML bool :=
  let! ab := Int.add <$> Int.mul a a <*> Int.mul b b in
  Int.eq <$> Int.mul c c <*> ab.
Extraction pythag.
End TestInt.

(** * Words *)


