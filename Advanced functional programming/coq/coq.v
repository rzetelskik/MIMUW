Require Import Coq.Lists.List.

Set Implicit Arguments.

(* Zadanie zaliczeniowe z Coqa - ZPF 2022 - termin oddania 24.05.2022, 23:59

Fill the definitions and prove the lemmas given below 

It is not allowed to: 
1. change/erase given definitions and lemma statements,
   section header/footer, variable declaration, etc.,
2. introduce your own axioms, parameters, variables, hypotheses etc.,   
3. import other modules,
4. define Ltac tactics.

It is allowed to:
1. introduce your own definitions and auxiliary lemmas,
2. change the order of the lemmas to prove,
3. add comments. 

Proszę o przesyłanie rozwiązań emailem (daria@mimuw.edu.pl) przed
23:59 dnia 24.05.22. Rozwiązanie powinno być w jednym pliku nazwanym
własnym nazwiskiem (Nazwisko.v).

Autorem zadania jest Daria Walukiewicz-Chrząszcz.

*)

(* Zadanie będzie na temat definicji Prefix w Prop i Set, 
oraz jej związkach z ilist i Fin *)

(* Prefix w Prop *)

Section PrefixP.
Variable A : Set.
  
Inductive PrefixP : list A -> list A -> Prop :=
| pemptyp : forall l, PrefixP nil l
| pconsp : forall a l1 l2,  PrefixP l1 l2 -> PrefixP (cons a l1) (cons a l2).

Check PrefixP.
Check PrefixP_ind.

(* Udowodnij poniższe lematy *)

Lemma lprefP : forall (l1 l2: list A) 
 (p1: PrefixP l1 l2) (l: list A), PrefixP (l++l1) (l++l2).
Proof.
  intros.
  induction l; simpl; [ | constructor]; assumption.
Qed.

Lemma prefTlP: forall (a b: A) (l1 l2: list A) 
 (p: PrefixP(a::l1)(b::l2)), PrefixP l1 l2.
Proof.
  intros.
  inversion p.
  assumption.
Qed.

Lemma prefInvP: forall (a: A) (l1 l2: list A) (p: PrefixP (cons a l1) l2), 
 cons a (tl l2) = l2.
Proof.
  Print tl.
  intros.
  inversion p.
  simpl.
  reflexivity.
Qed.

Lemma prefprefP : forall (l1 l2: list A) 
(p1: PrefixP l1 l2) (l3: list A) (p2: PrefixP l2 l3), 
PrefixP l1 l3.
Proof.
  intros.
  induction p1.
  - apply pemptyp.
  - apply pconsp with a l1 l2 in p1.
    (* :( *)
Admitted.

Lemma prefFalseP: forall a l (d: PrefixP (a::l) nil), False.
Proof.
  intros.
  inversion d.
Qed.

(* Dla Prefix w Prop i dowodu w: PrefixP l1 l2 nie da się 
zdefiniować sufiksu l2 wyciętego przez l1 (czyli listy l, 
takiej że l1++l=l2) po budowie dowodu d. Powodem jest brak 
możliwości eliminacji niesingletonowego typu w Prop w typ w Set.
Inaczej, nie da się dokończyć tej definicji:

Fixpoint suf (l1 l2: list A) (w: PrefixP l1 l2) : list A.
destruct w.

albo inaczej

Fixpoint suf (l1 l2: list A) (w: PrefixP l1 l2) : list A := 
match w with

Będzie to możliwe (i będzie to najprostsza definicja sufiksu), 
gdy Prefix będzie definiowany w Set.
*)

Fixpoint sufP (l1 l2: list A) (w: PrefixP l1 l2) : list A.
Fail destruct w.
Abort.

(* Zdefiniuj sufiks listy l2 wycięty przez prefiks l1, 
czyli taką listę l, że l1++l=l2, na dwa sposoby

1. poprzez lemat zakończony Defined
*)
  
Lemma sufPl : forall (l1 l2: list A) (w: PrefixP l1 l2), list A.
Proof.
  induction l1; intros.
  - exact l2.
  - destruct l2.
    * exact nil.
    * specialize (IHl1 l2).
      exact (IHl1 (prefTlP w)).
Defined.

(*
2. poprzez fixpoint
*)

Fixpoint sufPf (l1 l2: list A): (PrefixP l1 l2) -> list A := 
  match l1 return ((PrefixP l1 l2) -> list A) with
  | nil => fun _ => l2
  | cons _ _ =>
    match l2 with
    | nil => fun p => nil
    | cons _ _ => fun p => sufPf (prefTlP p)
    end
  end.

(*
Udowodnij:
*)

Lemma sufPEq: forall (l1 l2: list A) (w: PrefixP l1 l2), 
sufPl w = sufPf w.
Proof.
  induction l1; intros.
  - unfold sufPl.
    simpl.
    reflexivity.
  - destruct l2.
    * inversion w.
    * apply IHl1.
Qed.

End PrefixP.


(* Prefix w Set *)

Section Prefix.
Variable A : Set.

Inductive Prefix : list A -> list A -> Set :=
| pempty : forall l, Prefix nil l
| pcons : forall a l1 l2,  Prefix l1 l2 -> Prefix (cons a l1) (cons a l2).

(* Zdefiniuj następujące funkcje, tak żeby testy, które 
są na końcu pliku dobrze się obliczały oraz żeby lematy 
na temat tych funkcji były prawdziwe. 

Ewentualnie, możesz to zrobić lematem, ale trzeba uważać 
jak definiuje się te funkcje*)

Lemma lpref: forall (l l1 l2: list A) 
 (p1: Prefix l1 l2), Prefix (l++l1) (l++l2).
Proof.
  intros.
  induction l; simpl; [ | constructor]; assumption.
Defined.

Lemma prefTl: forall (a b: A) (l1 l2: list A) 
 (p: Prefix(a::l1)(b::l2)), Prefix l1 l2.
Proof.
  intros.
  inversion p.
  assumption.
Defined.

Lemma prefInv: forall (a: A) (l1 l2: list A) (p: Prefix (cons a l1) l2), 
 cons a (tl l2) = l2.
Proof.
  intros.
  inversion p.
  simpl.
  reflexivity.
Defined.

Lemma prefpref: forall (l1 l2: list A) (p1: Prefix l1 l2) 
 (l3: list A) (p2: Prefix l2 l3), Prefix l1 l3.
Proof.
Admitted.

Lemma prefFalse: forall a l (d: Prefix (a::l) nil), False.
Proof.
  intros.
  inversion d.
Defined.

(* Napisz suff rekurencyjnie po budowie listy l1 poprzez
Fixpoint lub Lemma *)

(*
Lemma suff: forall (l1 l2: list A) (w: Prefix l1 l2), list A.
Proof.
  intros.
  induction l1.
  - apply l2.
  - apply IHl1.
    induction l2.
    * 
Admitted.
*)

Fixpoint suff (l1 l2: list A): (Prefix l1 l2) -> list A :=
  match l1 with
  | nil => fun _ => l2
  | cons _ _ =>
    match l2 with
    | nil => fun _ => nil
    | cons _ _ => fun p => suff (prefTl p)
    end
  end.

Print suff.

(* Napisz definicję suf termem - rekurencyjnie po budowie w. 
Uzupełnij 
*)

Fixpoint suf (l1 l2: list A) (w: Prefix l1 l2): list A :=
  match w with
  | pempty l => l
  | pcons _ p' => suf p'
  end.

(* Udowodnij, że dwie definicje sufiksu są ekstensjonalnie równe *)

Lemma sufEq: forall (l1 l2: list A) (w: Prefix l1 l2), 
suff w = suf w.
Proof.
  intros.
  induction w.
  - simpl.
    trivial.
  - apply IHw.
Qed.

(* Udowodnij kilka lematów, które uzasadniają, że suf i 
poprzednio zdefiniowane funkcje robią to co chcemy *)


Lemma suf_suf: forall (l1 l2: list A)(w: Prefix l1 l2), 
 l1++(suf w) = l2.
Proof.
  intros.
  induction w; simpl; [ | rewrite IHw]; reflexivity.
Qed.

Lemma lpref_suf: forall (l l1 l2: list A) (w: Prefix l1 l2), 
 suf w = suf (lpref l w).
Proof.
  intros.
  induction l; simpl; trivial.
Qed.

Lemma prefTl_suf: forall (a b: A) (l1 l2: list A) 
(p: Prefix(a::l1)(b::l2)), suf p = suf (prefTl p).
Proof.
  intros.
  pose proof (suf_suf p).
  simpl in *.
  inversion H.
  pose proof (suf_suf (prefTl p)).
  rewrite <- H2 in H0.
  SearchPattern (_ ++ _ = _ ++ _ -> _ = _).
  apply app_inv_head with A l1 ((suf (prefTl p))) (suf p) in H0.
  symmetry in H0.
  assumption.
Qed.

Lemma prepref_suf : forall (l1 l2: list A) (p1: Prefix l1 l2)
 (l3: list A) (p2: Prefix l2 l3) , (suf p1) ++ (suf p2) = suf (prefpref p1 p2).
Proof.
Admitted.

End Prefix.

(* ilist, Fin *)

Section ilist.
 
Variable A : Set.

Inductive ilist : nat -> Set :=
| Nil : ilist O
| Cons : forall n, A -> ilist n -> ilist (S n).

Inductive fin : nat -> Set :=
| First : forall n, fin (S n)
| Next : forall n, fin n -> fin (S n).


(* Napisz funkcję zadluz, która przetłumaczy zwykłą listę w 
listę z długością 
*)

Print list.

Fixpoint zadluz (l: list A): ilist (length l) := 
  match l with
  | nil => Nil
  | cons a l' => Cons a (zadluz l')
  end.

(*
i jej odwrotność, funkcję oddluz: 
*)

Fixpoint oddluz (n:nat) (l: ilist n): list A := 
  match l with
  | Nil => nil
  | Cons a l' => cons a (oddluz l')
  end.

(*
Udowodnij, że to są swoje odwrotności: 
*)

Lemma oz: forall (l: list A), oddluz (zadluz l) = l.
Proof.
  intros.
  induction l; simpl; [ | rewrite -> IHl]; trivial.
Qed.

Lemma rowne: forall (n:nat) (l: ilist n), n = length (oddluz l).
Proof.
  intros.
  induction l; simpl; [ | rewrite <- IHl]; trivial.
Qed.

Lemma zo: forall (n:nat) (l: ilist n), zadluz( oddluz l) = 
      match rowne l with 
         eq_refl => l
      end.
Proof.
Admitted.


(* Napisz funkcję suffin, która dla danej listy z długością n i 
indeksu z zakresu 0..n, obliczy sufiks od tej pozycji. 
Indeks First oznacza, że cała lista jest sufiksem (spójrz na 
przykłady na końcu pliku).
*)

Print fin.

Fixpoint suffin n (ls: ilist n): fin (S n) -> list A  :=
  match ls with
  | Nil => fun idx =>
    match idx in fin n' return (match n' with
                                | 0 => unit
                                | S _ => list A
                                end) with
    | First _ => nil
    | Next _ => nil
    end
  | Cons a l' => fun idx =>
    match idx in fin n' return (fin (pred n') -> list A) -> list A with
    | First _ => fun _ => oddluz ls
    | @Next n1 idx' => fun (suffin': fin n1 -> list A) => suffin' idx'
    end (suffin l')
  end.


End ilist.

(*  W ostatniej części należy zrobić z elementu (Prefix l1 l2) 
listę z długoscią n i element fin (S n), który pokazuje 
gdzie zaczyna się sufiks 

Następnie należy udowodnić lemat, że suf i suffin obliczają 
ten sam sufiks.
*)

Print prod.


Fixpoint oblicz (A: Set)(l1 l2: list A)(w: Prefix l1 l2) : 
         let n := length l2 in prod (ilist A n) (fin (S n)) := 
  match w with
  | pempty l' => let n := length l' in pair (zadluz l') (First n)
  | @pcons _ a _ _ w' => let (x, n') := oblicz w' in pair (Cons a x) (Next n')
  end.


Lemma suf_suffin: forall (A: Set)(l1 l2: list A)(w: Prefix l1 l2), 
     let (lr, fr) := oblicz w  in suf w = suffin lr fr.
Proof.
Admitted.


(* Testy do sekcji PrefixP *)


Eval compute in sufPl (pconsp 3 (pemptyp (4::5::nil))).
(*   = 4 :: 5 :: nil
     : list nat *)


Eval compute in sufPf (pconsp 2 (pconsp 3 (pemptyp (4::5::nil)))).
(*    = 4 :: 5 :: nil
     : list nat *)

(* Testy do sekcji Prefix *)

Eval compute in (lpref (1::2::nil) (pcons 3 (pempty (4::nil)))).
(*= pcons 1 (pcons 2 (pcons 3 (pempty (4 :: nil))))
     : Prefix ((1 :: 2 :: nil) ++ 3 :: nil)
         ((1 :: 2 :: nil) ++ 3 :: 4 :: nil) *)

Eval compute in (prefTl (pcons 2 (pcons 3 (pempty (4::5::nil))))).
(*    = pcons 3 (pempty (4 :: 5 :: nil))
     : Prefix (3 :: nil) (3 :: 4 :: 5 :: nil) *)

Eval compute in (prefpref (pcons 1 (pempty (2::nil))) 
 (pcons 1 (pcons 2 (pempty (3::4::nil))))).
(*= pcons 1 (pempty (2 :: 3 :: 4 :: nil))
     : Prefix (1 :: nil) (1 :: 2 :: 3 :: 4 :: nil) *)

Eval compute in suf (pcons 3 (pempty (4::5::nil))).
(*= 4 :: 5 :: nil
     : list nat *)

Eval compute in suff (pcons 3 (pempty (4::5::nil))).
(* = 4 :: 5 :: nil
     : list nat *)

Eval compute in suf (pcons 2 (pcons 3 (pempty (4::5::nil)))).
(*    = 4 :: 5 :: nil
     : list nat *)

(* Testy do sekcji ilist *)

Print Implicit First.
Arguments First {n}.
Print Implicit First.
Arguments Nil {A}.

Eval compute in (suffin (Cons 3 Nil ) (First)).
(*    = 3 :: nil
     : list nat
*)

Eval compute in (suffin (Cons 3 Nil ) (Next First)).
(*    = nil
     : list nat
*)

