;; lego-fontlock.el Font lock expressions for LEGO
;; Copyright (C) 1994, 1995, 1996, 1997 LFCS Edinburgh. 
;; Author: Healfdene Goguen, Thomas Kleymann and Dilip Sequiera
;; Maintainer: LEGO Team <lego@dcs.ed.ac.uk>

;; $Log$
;; Revision 1.1.2.2  1997/10/08 08:22:31  hhg
;; Updated undo, fixed bugs, more modularization
;;
;; Revision 1.1.2.1  1997/10/07 13:34:23  hhg
;; New structure to share as much as possible between LEGO and Coq.
;;
;;


(require 'proof-fontlock)

;; ----- keywords for font-lock.

(defvar lego-keywords-goal '("$?Goal"))

(defvar lego-keywords-save '("$?Save" "SaveFrozen" "SaveUnfrozen"))

(defvar lego-keywords
  (append lego-keywords-goal lego-keywords-save
	  '("allE" "allI" "andE" "andI" "Assumption" "Claim"
  "Constructors" "Cut" "Discharge" "DischargeKeep"
    "Double" "echo" "ElimOver" "exE" "exI" "Expand" "ExpAll"
    "ExportState" "Equiv" "Fields" "Freeze" "From" "Hnf" "Immed"
    "impE" "impI" "Import" "Induction" "Inductive" "Inversion" "Init"
    "intros" "Intros" "Module" "Next" "NoReductions" "Normal" "notE"
    "notI" "orE" "orIL" "orIR" "Parameters" "Qnify" "Qrepl" "Record"
    "Refine" "Relation" "Theorems" "Unfreeze")))

(defvar lego-tacticals '("Then" "Else" "Try" "Repeat" "For"))

;; ----- regular expressions for font-lock
(defvar lego-error-regexp "^\\(Error\\|Lego parser\\)"
  "A regular expression indicating that the LEGO process has
  identified an error.") 

(defvar lego-id proof-id)

(defvar lego-ids (concat lego-id "\\(\\s *,\\s *" lego-id "\\)*")
  "*For font-lock, we treat \",\" separated identifiers as one identifier
  and refontify commata using \\{lego-fixup-change}.")

(defun lego-decl-defn-regexp (char)
    (concat "\\[\\s *\\(" lego-ids
 "\\)\\s *\\(\\[[^]]+\\]\\s *\\)*" char))
; Examples
;              ^        ^^^^        ^^^^^^^^^^^^^^^^^^^^^^^  ^^^^
;              [        sort                                 =
;              [        sort        [n:nat]                  =
;              [        sort        [abbrev=...][n:nat]      =

(defvar lego-font-lock-terms
  (list

   ; lambda binders
     (list (lego-decl-defn-regexp "[:|]") 1
	   'font-lock-declaration-name-face)

     ; let binders
     (list (lego-decl-defn-regexp "=") 1 'font-lock-function-name-face)

     ; Pi and Sigma binders
     (list (concat "[{<]\\s *\\(" lego-ids "\\)") 1
	   'font-lock-declaration-name-face)
   
     ;; Kinds
     (cons (concat "\\<Prop\\>\\|\\<Type\\s *\\(("
		   lego-id ")\\)?") 'font-lock-type-face))
  "*Font-lock table for LEGO terms.")

(defvar lego-font-lock-keywords-1
   (append
    lego-font-lock-terms
    (list
     (cons (ids-to-regexp lego-keywords) 'font-lock-keyword-face)
     (cons (ids-to-regexp lego-tacticals) 'font-lock-tacticals-name-face)

     (list (concat "\\("
		   (ids-to-regexp lego-keywords-goal)
		   "\\)\\s *\\(" lego-id "\\)\\s *:")
             2 'font-lock-function-name-face)

     (list (concat "\\("
		   (ids-to-regexp lego-keywords-save)
		   "\\)\\s *\\(" lego-id "\\)")
             2 'font-lock-function-name-face))))
     
(provide 'lego-fontlock)
