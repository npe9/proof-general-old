;;
;; Keyword classification tables for Isabelle/Isar.
;; This file generated by Isabelle -- DO NOT EDIT!
;;
;; $Id$
;;

(defconst isar-keywords-minor
  '("and"
    "as"
    "binder"
    "con_defs"
    "congs"
    "distinct"
    "files"
    "induction"
    "infixl"
    "infixr"
    "inject"
    "intrs"
    "is"
    "monos"
    "output"
    "simpset"))

(defconst isar-keywords-control
  '("break"
    "cannot_undo"
    "cd"
    "clear_undo"
    "exit"
    "kill"
    "kill_proof"
    "prev"
    "quit"
    "redo"
    "restart"
    "top"
    "undo"
    "undos_proof"
    "up"))

(defconst isar-keywords-diag
  '("ML"
    "commit"
    "help"
    "pr"
    "print_attributes"
    "print_binds"
    "print_facts"
    "print_methods"
    "print_syntax"
    "print_theorems"
    "print_theory"
    "prop"
    "pwd"
    "term"
    "thm"
    "typ"
    "update_thy"
    "use"
    "use_thy"
    "use_thy_only"))

(defconst isar-keywords-theory-begin
  '("context"
    "theory"
    "update_context"))

(defconst isar-keywords-theory-end
  '("end"))

(defconst isar-keywords-theory-heading
  '("chapter"
    "section"
    "subsection"
    "subsubsection"
    "title"))

(defconst isar-keywords-theory-decl
  '("arities"
    "axclass"
    "axioms"
    "classes"
    "classrel"
    "coinductive"
    "constdefs"
    "consts"
    "datatype"
    "defaultsort"
    "defer_recdef"
    "defs"
    "global"
    "inductive"
    "lemmas"
    "local"
    "nonterminals"
    "oracle"
    "parse_ast_translation"
    "parse_translation"
    "path"
    "primrec"
    "print_ast_translation"
    "print_translation"
    "recdef"
    "record"
    "rep_datatype"
    "setup"
    "syntax"
    "text"
    "theorems"
    "token_translation"
    "translations"
    "typed_print_translation"
    "typedecl"
    "types"))

(defconst isar-keywords-theory-goal
  '("instance"
    "lemma"
    "theorem"
    "typedef"))

(defconst isar-keywords-qed
  '("\\."
    "\\.\\."
    "by"))

(defconst isar-keywords-qed-block
  '("qed"
    "qed_with"))

(defconst isar-keywords-proof-goal
  '("have"
    "hence"
    "show"
    "thus"))

(defconst isar-keywords-proof-block
  '("next"
    "proof"
    "{{"
    "}}"))

(defconst isar-keywords-proof-chain
  '("finally"
    "from"
    "then"
    "with"))

(defconst isar-keywords-proof-decl
  '("also"
    "let"
    "note"))

(defconst isar-keywords-proof-asm
  '("assume"
    "fix"
    "presume"))

(defconst isar-keywords-proof-script
  '("apply"
    "back"
    "then_apply"))

(provide 'isar-keywords)
