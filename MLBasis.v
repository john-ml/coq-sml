Require Extraction.

(** * ML monad *)

Axiom ML : Type -> Set.
Axiom ml_ret : forall {A}, A -> ML A.
Axiom ml_bind : forall {A B}, ML A -> (A -> ML B) -> ML B.
Extract Constant ML "'a" => "'a".
Extract Inlined Constant ml_ret => "(fun x -> x)".
Extract Inlined Constant ml_bind => "(fun x k -> k x)".

Declare Scope ML_scope.
Delimit Scope ML_scope with ML.
Notation "'ret!'" := ml_ret : ML_scope.
Notation "'let!' x ':=' c1 'in' c2" := (@ml_bind _ _ c1 (fun x => c2))
  (at level 61, c1 at next level, right associativity) : ML_scope.
Notation "'let!' ' p ':=' c1 'in' c2" :=
  (@ml_bind _ _ c1 (fun x => match x with p => c2 end))
  (at level 61, p pattern, c1 at next level, right associativity) : ML_scope.
Notation "f '=<<' m" := (ml_bind _ _ m f) (at level 53, right associativity) : ML_scope.
Notation "f '<$>' m" := (let! x := m in ret! (f x))%ML (at level 52, left associativity) : ML_scope.
Notation "mf '<*>' mx" :=
  (let! f := mf in let! x := mx in ret! (f x))%ML
  (at level 52, left associativity) : ML_scope.
Notation "mx '<*' my" :=
  (let! x := mx in let! y := my in ret! x)%ML
  (at level 52, left associativity) : ML_scope.
Notation "mx '*>' my" :=
  (let! x := mx in let! y := my in ret! y)%ML
  (at level 52, left associativity) : ML_scope.

Module TestMLNotation.
Local Open Scope ML_scope.
Definition test_program {A B C D E} (pre : ML A) (f : B -> C -> ML D)
           (x : ML B) (y : ML C) (post : ML E) :=
  pre *> (f <$> x <*> y) <* post.
Extraction test_program.
End TestMLNotation.

(** * Booleans, pairs, options, lists *)

Extract Inductive bool => "bool" [ "true" "false" ].
Extract Inductive prod => "( * )" [ "(fun x y -> (x, y))" ] "(fun k (x, y) -> k x y)".
Extract Inductive option => "option" [ "SOME" "NONE" ].
Extract Inductive list => "list" [ "[]" "( :: )" ].

Module TestProd.
Definition test_prod := fun '(x, y, z) => x + y + z.
Extraction test_prod.
End TestProd.

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

Require Coq.ZArith.ZArith.
Module Int <: INTEGER.
  Axiom int'' : Set.
  (* We rely on the fact that singleton inductives extract to type aliases *)
  Inductive int' : Set := mk_int (_ : int'').
  Definition int : Set := int'.
  Axiom zero one : int.
  Axiom succ : int -> int.
  Axiom double : int -> int.
  Axiom minInt maxInt : option int.
  Axiom add mul sub div mod quot rem : int -> int -> ML int.
  Axiom lt le eq gt ge : int -> int -> bool.
  Axiom neg abs : int -> int.
  Axiom min max : int -> int -> int.
  Axiom sameSign : int -> int -> bool.
  Extract Inlined Constant int'' => "Int.int".
  Extract Inlined Constant zero => "0".
  Extract Inlined Constant one => "1".
  Extract Inlined Constant succ => "(fun x -> x+1)".
  Extract Inlined Constant double => "(fun x -> 2*x)".
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
  Import ZArith.
  Fixpoint int_of_pos (p : positive) : int :=
    match p with
    | xI p => succ (double (int_of_pos p))
    | xO p => double (int_of_pos p)
    | xH => one
    end.
  Definition int_of_Z (i : Z) : int :=
    match i with
    | Z0 => zero
    | Zpos p => int_of_pos p
    | Zneg p => neg (int_of_pos p)
    end.
  Definition Z_of_int (i : int) : option Z := None.
End Int.
Numeral Notation Int.int' Int.int_of_Z Int.Z_of_int : ML_scope.

Module TestInt.
Local Open Scope ML_scope.
Definition pythag (a b c : Int.int) : ML bool :=
  let! ab := Int.add <$> Int.mul a a <*> Int.mul b b in
  Int.eq <$> Int.mul c c <*> ab.
Extraction pythag.
Definition literal (n : Int.int) : ML Int.int := Int.sub n 20%ML.
Recursive Extraction literal.
End TestInt.

(** * Words *)

Module Type WORD.
  Axiom word : Set.
  Axiom zero one : word.
  Axiom succ : word -> word.
  Axiom double : word -> word.
  Axiom wordSize : Int.int.
  Axiom toInt : word -> Int.int.
  Axiom toIntX : word -> ML Int.int.
  Axiom fromInt : Int.int -> word.
  Axiom andb orb xorb : word -> word -> word.
  Axiom notb : word -> word.
  Axiom add sub mul : word -> word -> word.
  Axiom div mod : word -> word -> ML word.
  Axiom lt le gt ge : word -> word -> bool.
  Axiom compl : word -> word.
  Axiom min max : word -> word -> word.
End WORD.

Module Word <: WORD.
  Axiom word'' : Set.
  Inductive word' : Set := mk_word (_ : word'').
  Definition word : Set := word'.
  Axiom zero one : word.
  Axiom succ : word -> word.
  Axiom double : word -> word.
  Axiom wordSize : Int.int.
  Axiom toInt : word -> Int.int.
  Axiom toIntX : word -> ML Int.int.
  Axiom fromInt : Int.int -> word.
  Axiom andb orb xorb : word -> word -> word.
  Axiom notb : word -> word.
  Axiom add sub mul : word -> word -> word.
  Axiom div mod : word -> word -> ML word.
  Axiom lt le eq gt ge : word -> word -> bool.
  Axiom compl : word -> word.
  Axiom min max : word -> word -> word.
  Extract Inlined Constant word'' => "Word.word".
  Extract Inlined Constant zero => "(Word.fromInt 0)".
  Extract Inlined Constant one => "(Word.fromInt 1)".
  Extract Inlined Constant succ => "(fun x -> x + Word.fromInt 1)".
  Extract Inlined Constant double => "(fun x -> x << Word.fromInt 1)".
  Extract Inlined Constant wordSize => "Word.wordSize".
  Extract Inlined Constant toInt => "Word.toInt".
  Extract Inlined Constant toIntX => "Word.toIntX".
  Extract Inlined Constant fromInt => "Word.fromInt".
  Extract Inlined Constant andb => "(fun x y -> Word.andb (x, y))".
  Extract Inlined Constant orb => "(fun x y -> Word.orb (x, y))".
  Extract Inlined Constant xorb => "(fun x y -> Word.xorb (x, y))".
  Extract Inlined Constant notb => "Word.notb".
  Extract Inlined Constant add => "(fun x y -> Word.add (x, y))".
  Extract Inlined Constant sub => "(fun x y -> Word.sub (x, y))".
  Extract Inlined Constant mul => "(fun x y -> Word.mul (x, y))".
  Extract Inlined Constant div => "(fun x y -> Word.div (x, y))".
  Extract Inlined Constant mod => "(fun x y -> Word.mod (x, y))".
  Extract Inlined Constant lt => "(fun x y -> x < y)".
  Extract Inlined Constant le => "(fun x y -> x <= y)".
  Extract Inlined Constant eq => "(fun x y -> x = y)".
  Extract Inlined Constant gt => "(fun x y -> x > y)".
  Extract Inlined Constant ge => "(fun x y -> x >= y)".
  Extract Inlined Constant compl => "(fun x -> ~x)".
  Extract Inlined Constant min => "(fun x y -> Word.min (x, y))".
  Extract Inlined Constant max => "(fun x y -> Word.max (x, y))".
  Import ZArith.
  Fixpoint word_of_pos (p : positive) : word :=
    match p with
    | xI p => succ (double (word_of_pos p))
    | xO p => double (word_of_pos p)
    | xH => one
    end.
  Definition word_of_Z (i : Z) : option word :=
    match i with
    | Z0 => Some zero
    | Zpos p => Some (word_of_pos p)
    | Zneg p => None
    end.
  Definition Z_of_word (i : word) : option Z := None.
End Word.
Numeral Notation Word.word' Word.word_of_Z Word.Z_of_word : ML_scope.
