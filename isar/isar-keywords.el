;;
;; Keyword classification tables for Isabelle/Isar.
;; This file generated by Isabelle -- DO NOT EDIT!
;;
;; $Id$
;;

(defconst isar-keywords-major
  '("\\."
    "\\.\\."
    "ML"
    "ML_command"
    "ML_setup"
    "ProofGeneral\\.context_thy_only"
    "ProofGeneral\\.inform_file_processed"
    "ProofGeneral\\.inform_file_retracted"
    "ProofGeneral\\.kill_proof"
    "ProofGeneral\\.restart"
    "ProofGeneral\\.try_context_thy_only"
    "ProofGeneral\\.undo"
    "also"
    "apply"
    "apply_end"
    "arities"
    "assume"
    "automaton"
    "axclass"
    "axioms"
    "back"
    "by"
    "cannot_undo"
    "case"
    "cd"
    "chapter"
    "classes"
    "classrel"
    "clear_undos"
    "coinductive"
    "commit"
    "constdefs"
    "consts"
    "context"
    "corollary"
    "datatype"
    "declare"
    "def"
    "defaultsort"
    "defer"
    "defer_recdef"
    "defs"
    "disable_pr"
    "done"
    "enable_pr"
    "end"
    "exit"
    "finally"
    "fix"
    "from"
    "global"
    "have"
    "header"
    "hence"
    "hide"
    "inductive"
    "inductive_cases"
    "init_toplevel"
    "instance"
    "judgment"
    "kill"
    "kill_thy"
    "lemma"
    "lemmas"
    "let"
    "local"
    "method_setup"
    "moreover"
    "next"
    "nonterminals"
    "note"
    "obtain"
    "oops"
    "oracle"
    "parse_ast_translation"
    "parse_translation"
    "pr"
    "prefer"
    "presume"
    "pretty_setmargin"
    "primrec"
    "print_antiquotations"
    "print_ast_translation"
    "print_attributes"
    "print_binds"
    "print_cases"
    "print_claset"
    "print_commands"
    "print_context"
    "print_facts"
    "print_methods"
    "print_simpset"
    "print_syntax"
    "print_theorems"
    "print_theory"
    "print_trans_rules"
    "print_translation"
    "proof"
    "prop"
    "pwd"
    "qed"
    "quit"
    "recdef"
    "recdef_tc"
    "record"
    "redo"
    "remove_thy"
    "rep_datatype"
    "sect"
    "section"
    "setup"
    "show"
    "sorry"
    "subsect"
    "subsection"
    "subsubsect"
    "subsubsection"
    "syntax"
    "term"
    "text"
    "text_raw"
    "then"
    "theorem"
    "theorems"
    "theory"
    "thm"
    "thm_deps"
    "thms_containing"
    "thus"
    "token_translation"
    "touch_all_thys"
    "touch_child_thys"
    "touch_thy"
    "translations"
    "txt"
    "txt_raw"
    "typ"
    "typed_print_translation"
    "typedecl"
    "typedef"
    "types"
    "ultimately"
    "undo"
    "undos_proof"
    "update_thy"
    "update_thy_only"
    "use"
    "use_thy"
    "use_thy_only"
    "welcome"
    "with"
    "{"
    "}"))

(defconst isar-keywords-minor
  '("actions"
    "and"
    "binder"
    "compose"
    "con_defs"
    "concl"
    "congs"
    "distinct"
    "files"
    "hide_action"
    "hints"
    "in"
    "induction"
    "infixl"
    "infixr"
    "initially"
    "inject"
    "inputs"
    "internals"
    "intros"
    "is"
    "monos"
    "output"
    "outputs"
    "overloaded"
    "post"
    "pre"
    "rename"
    "restrict"
    "signature"
    "states"
    "to"
    "transitions"
    "transrel"
    "using"
    "where"))

(defconst isar-keywords-control
  '("ProofGeneral\\.context_thy_only"
    "ProofGeneral\\.inform_file_processed"
    "ProofGeneral\\.inform_file_retracted"
    "ProofGeneral\\.kill_proof"
    "ProofGeneral\\.restart"
    "ProofGeneral\\.try_context_thy_only"
    "ProofGeneral\\.undo"
    "cannot_undo"
    "clear_undos"
    "exit"
    "init_toplevel"
    "kill"
    "quit"
    "redo"
    "undo"
    "undos_proof"))

(defconst isar-keywords-diag
  '("ML"
    "ML_command"
    "cd"
    "commit"
    "disable_pr"
    "enable_pr"
    "header"
    "kill_thy"
    "pr"
    "pretty_setmargin"
    "print_antiquotations"
    "print_attributes"
    "print_binds"
    "print_cases"
    "print_claset"
    "print_commands"
    "print_context"
    "print_facts"
    "print_methods"
    "print_simpset"
    "print_syntax"
    "print_theorems"
    "print_theory"
    "print_trans_rules"
    "prop"
    "pwd"
    "remove_thy"
    "term"
    "thm"
    "thm_deps"
    "thms_containing"
    "touch_all_thys"
    "touch_child_thys"
    "touch_thy"
    "typ"
    "update_thy"
    "update_thy_only"
    "use"
    "use_thy"
    "use_thy_only"
    "welcome"))

(defconst isar-keywords-theory-begin
  '("theory"))

(defconst isar-keywords-theory-switch
  '("context"))

(defconst isar-keywords-theory-end
  '("end"))

(defconst isar-keywords-theory-heading
  '("chapter"
    "section"
    "subsection"
    "subsubsection"))

(defconst isar-keywords-theory-decl
  '("ML_setup"
    "arities"
    "automaton"
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
    "hide"
    "inductive"
    "judgment"
    "lemmas"
    "local"
    "method_setup"
    "nonterminals"
    "oracle"
    "parse_ast_translation"
    "parse_translation"
    "primrec"
    "print_ast_translation"
    "print_translation"
    "recdef"
    "record"
    "rep_datatype"
    "setup"
    "syntax"
    "text"
    "text_raw"
    "theorems"
    "token_translation"
    "translations"
    "typed_print_translation"
    "typedecl"
    "types"))

(defconst isar-keywords-theory-script
  '("declare"
    "inductive_cases"))

(defconst isar-keywords-theory-goal
  '("corollary"
    "instance"
    "lemma"
    "recdef_tc"
    "theorem"
    "typedef"))

(defconst isar-keywords-qed
  '("\\."
    "\\.\\."
    "by"
    "done"
    "sorry"))

(defconst isar-keywords-qed-block
  '("qed"))

(defconst isar-keywords-qed-global
  '("oops"))

(defconst isar-keywords-proof-heading
  '("sect"
    "subsect"
    "subsubsect"))

(defconst isar-keywords-proof-goal
  '("have"
    "hence"
    "show"
    "thus"))

(defconst isar-keywords-proof-block
  '("next"
    "proof"))

(defconst isar-keywords-proof-open
  '("{"))

(defconst isar-keywords-proof-close
  '("}"))

(defconst isar-keywords-proof-chain
  '("finally"
    "from"
    "then"
    "ultimately"
    "with"))

(defconst isar-keywords-proof-decl
  '("also"
    "let"
    "moreover"
    "note"
    "txt"
    "txt_raw"))

(defconst isar-keywords-proof-asm
  '("assume"
    "case"
    "def"
    "fix"
    "presume"))

(defconst isar-keywords-proof-asm-goal
  '("obtain"))

(defconst isar-keywords-proof-script
  '("apply"
    "apply_end"
    "back"
    "defer"
    "prefer"))

(provide 'isar-keywords)
