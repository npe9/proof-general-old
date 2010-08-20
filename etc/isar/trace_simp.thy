(* This is a test of tracing output for Isabelle. *)

theory trace_simp imports Main begin

text {*
  this produces massive amount of simplifier trace, but terminates
  eventually: *}

declare [[trace_simp]]
ML {* reset quick_and_dirty *}

datatype ord = Zero | Succ ord | Limit "nat => ord"

(* testing comment here *)

text {* this one loops forever *}

lemma "ALL x. f x = g(f(g(x))) ==> f [] = f [] @ []"
  apply simp

