;; proof-easy-config.el    Easy configuration for Proof General
;;
;; Copyright (C) 1999-2002  David Aspinall / LFCS.
;; Author:    David Aspinall <David.Aspinall@ed.ac.uk>
;; License:   GPL (GNU GENERAL PUBLIC LICENSE)
;;
;; $Id$
;;
;; Future version might copy settings instead; consider how best to
;; interface with customization mechanism so a new prover can be
;; configured by editing inside custom buffers.
;;
(require 'proof)

(defconst proof-easy-config-derived-modes-table
  '((""         "script"     proof-mode (proof-config-done))
    ("shell"    "shell"      proof-shell-mode (proof-shell-config-done))
    ("response" "response"   proof-response-mode (proof-response-config-done))
    ("goals"	"goals"      proof-goals-mode (proof-goals-config-done)))
  "A list of (PREFIXSYM SUFFIXNAME PARENT MODEBODY) for derived modes.")

(defun proof-easy-config-define-derived-modes ()
  (dolist (modedef proof-easy-config-derived-modes-table)
    (let* ((prefixsym (nth 0 modedef))
	   (suffixnm  (nth 1 modedef))
	   (parent    (nth 2 modedef))
	   (body      (nthcdr 3 modedef))
	   (modert    (concat (symbol-name proof-assistant-symbol)
			      "-" prefixsym))
	   (hyphen    (if (string-equal prefixsym "") "" "-"))
	   (mode      (intern (concat modert hyphen "mode")))
	   (modename  (concat proof-assistant " " suffixnm))
	   (varname   (intern (concat "proof-mode-for-" suffixnm)))
	   ;; FIXME: declare these variables in proof-config:
	   ;;	proof-{goals,resp,trace}-font-lock-keywords,
	   ;;   proof-{goals,resp,trace}-syntax-table-entries
	   ;; FIXME: in future versions, use these settings in *-config-done
	   ;;        to simplify elisp code elsewhere.
	   ;; FIXME: add imenu-generic-expression too
	   ;; 
	   (fntlcks   (intern (concat "proof-" suffixnm "-font-lock-keywords")))
	   (modsyn    (intern (concat "proof-" suffixnm "-syntax-table-entries")))
	   (fullbody  (append
		       (if (and (boundp fntlcks) (eval fntlcks))
			   (list `(setq font-lock-keywords ,fntlcks)))
		       (if (and (boundp modsyn) (eval modsyn))
			   (list `(let ((syn ,modsyn))
				    (while syn
				      (modify-syntax-entry 
				       (car syn) (cadr syn))
				      (setq syn (cddr syn))))))
		       body)))
      (eval
       `(define-derived-mode ,mode ,parent ,modename nil ,@fullbody))
      ;; Set proof-mode-for-script and friends
      ;; NB: top-level, so we don't need proof-pre-shell-start-hook.
      (set varname mode))))

(defun proof-easy-config-check-setup (sym name)
  "A number of simple checks."
  (let ((msg ""))
    ;; At the moment we just check that the symbol/name used
    ;; in the macro matches that in `proof-assistant-table'
    ;; and have the right type.
    (unless (symbolp sym)
      (error "proof-easy-config: first argument (%s) should be a symbol"
	     sym))
    (unless (stringp name)
      (error "proof-easy-config: second argument (%s) should be a string"
	     string))
    (cond
     ((or 
       (and (boundp 'proof-assistant) proof-assistant 
	    (not (equal proof-assistant ""))
	    (not (equal proof-assistant name))
	    (setq msg (format "\nproof-assistant name: \"%s\" doesn't match expected \"%s\"" 
			      proof-assistant name)))
       (and (boundp 'proof-assistant-symbol) proof-assistant-symbol
	    (not (eq proof-assistant-symbol sym))
	    (setq msg (format "\nproof-assistant symbol: '%s doesn't match expected '%s" 
			      proof-assistant-symbol sym))))
      (error "proof-easy-config: PG already in use or name/symbol mismatch %s" 
	     msg))
     (t
      ;; Setting these here is nice for testing: no need to get
      ;; proof-assistant-table right first.
      (customize-set-variable 'proof-assistant name)
      (customize-set-variable 'proof-assistant-symbol sym)))))

;;;###autoload
(defmacro proof-easy-config (sym name &rest body)
  "Configure Proof General for proof-assistant using BODY as a setq body.
The symbol SYM and string name NAME must match those given in
the `proof-assistant-table', which see."
  `(progn
     (proof-easy-config-check-setup ,sym ,name)
     (setq
      ,@body)
     (proof-easy-config-define-derived-modes)))
  
;; 
(provide 'proof-easy-config)

