(*
      Example proof document for Isabelle/Isar Proof General.
   
      $Id$
*)

theory "Example-Xsym" imports Main begin

text {* Proper proof text -- \textit{naive version}. *}

theorem and_comms: "A \<and> B \<longrightarrow> B \<and> A"
proof
  assume "A \<and> B"
  then show "B \<and> A"
  proof
    assume B and A
    then show ?thesis ..
 qed
qed

text {* Proper proof text -- \textit{advanced version}. *}
;  (setq font-lock-keywords (unicode-tokens2-font-lock-keywords))
; (setq font-lock-keywords 
  '("\\(\\\\<and>\\)"
      (0
	(unicode-tokens2-compose-symbol
	  '(("\\<and>" . "∧")))
	  'prepend)))

foob foob foo 

;; TESTS
;;
;; (setq font-lock-keywords '("\\(\\<foo\\>\\)"))  OK

;; (setq font-lock-keywords '(("\\(\\<foo\\>\\)" (1 proof-declaration-name-face t)))) YES

;; (setq font-lock-keywords '(("\\(\\<foo\\>\\)" (1 '(underline t))))) YES

;; alpha
;; (setq font-lock-keywords '(("\\(\\<alpha\\>\\)" 
;;      (0 (unicode-tokens2-font-lock-compose-symbol '(("alpha" . (bold . "α"))))))))  YES

;; (setq font-lock-keywords (unicode-tokens2-font-lock-keywords))

; (setq font-lock-keywords 
  '(("\\(\\\\<and>\\)"
      (0
	(unicode-tokens2-compose-symbol
	  '(("\\<and>" . "∧")))
	  'prepend))))

; \<and>
; \<longrightarrow>
; and 
; (print font-lock-keywords)

theorem "A \<and> B \<longrightarrow> B \<and> A"
proof
  assume "A \<and> B"
  then obtain B and A ..
  then show "B \<and> A" ..
qed

\<foo> 
text {* Unstructured proof script. *}

(*
(setq font-lock-keywords '(("\\(\\\\<and>\\)" 
  (0 (unicode-tokens2-font-lock-compose-symbol '(("\\<and>" . #x002227))) keep))))
*)
\<and> \<and>
theorem "A \<and> B \<longrightarrow> B \<and> A"
  apply (rule impI)
  apply (erule conjE)
  apply (rule conjI)
  apply assumption
  apply assumption
done
\<forall>
end
