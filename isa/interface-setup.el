;;
;; $Id$
;; 

(customize-set-variable
 'isabelle-prog-name
 (concat (getenv "ISABELLE") " " (getenv "PROOFGENERAL_LOGIC")))

(if (string-match "^polyml" (getenv "ML_SYSTEM"))
    (customize-set-variable
     'proof-shell-pre-interrupt-hook
     (lambda () (proof-shell-insert "f" nil))))
