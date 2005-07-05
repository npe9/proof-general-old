;; $State$ $Date$ $Revision$ 

(require 'proof)			; load generic parts
(require 'proof-config)

;; Adjust toolbar entries.  (Must be done
;; before proof-toolbar is loaded).

(if proof-running-on-XEmacs (setq phox-toolbar-entries
      (remassoc 'context phox-toolbar-entries)))


;; ======== User settings for PhoX ========
;;
;; Defining variables using customize is pretty easy.
;; You should do it at least for your prover-specific user options.
;;
;; proof-site provides us with two customization groups
;; automatically:  (based on the name of the assistant)
;;
;; 'phox        -  User options for PhoX Proof General
;; 'phox-config -  Configuration of PhoX Proof General
;;			   (constants, but may be nice to tweak)
;;
;; The first group appears in the menu
;;   ProofGeneral -> Customize -> PhoX 
;; The second group appears in the menu:
;;   ProofGeneral -> Internals -> PhoX config
;;

(defcustom phox-prog-name "phox -pg"
  "*Name of program to run PhoX."
  :type 'file
  :group 'phox)

; da: I commented this out, it should be defined automatically
; in proof-config.el.  If you want to change the default to t,
; then perhaps:  (custom-set-default phox-x-symbol-enable t)
;(defcustom phox-x-symbol-enable t
;  "*Whether to use x-symbol or not."
;  :type 'boolean
;  :group 'phox)


(defcustom phox-web-page
  "http://www.lama.univ-savoie.fr/~RAFFALLI/phox.html"
  "URL of web page for PhoX."
  :type 'string
  :group 'phox-config)

(defcustom phox-doc-dir 
  "/usr/local/doc/phox"
  "The name of the root documentation directory for PhoX."
  :type 'string
  :group 'phox-config)

(defcustom phox-lib-dir 
  "/usr/local/lib/phox"
  "The name of the root directory for PhoX libraries."
  :type 'string
  :group 'phox-config)

(defcustom phox-tags-program 
  (concat phox-doc-dir "/tools/phox_etags.sh")
  "Program to run to generate TAGS table for proof assistant."
  :type 'string
  :group 'phox-config)

(defcustom phox-tags-doc 
  t
  "*If non nil, tags table for PhoX text documentation is loaded."
  :type 'boolean
  :group 'phox-config)

(defcustom phox-etags 
  (concat phox-doc-dir "/tools/phox_etags.sh")
  "Command to build tags table."
  :type 'string
  :group 'phox-config)

(require 'phox-fun)
(require 'phox-font)
(require 'phox-extraction)
(require 'phox-tags)
(require 'phox-outline)
(require 'phox-pbrpm)

;; default for tags [da: moved here to fix compilation 15/02/03]

(if phox-tags-doc 
    (add-hook 'phox-mode-hook 'phox-tags-add-doc-table))


;; ----- PhoX specific menu

(defpgdefault menu-entries
  (cons
   phox-state-menu
   (cons
    phox-tags-menu
    (cons
     phox-extraction-menu
      nil)))
)

;;
;; ======== Configuration of generic modes ========
;;

(defun phox-config ()
  "Configure Proof General scripting for PhoX."
  (setq
   proof-terminal-char		?\.	; ends every command
   proof-script-command-end-regexp "[.]\\([ \t\n\r]\\)"
   proof-script-comment-start             "(*"
   proof-script-comment-end               "*)"
   proof-goal-command-regexp       
    "\\`\\(Local[ \t\n\r]+\\)?\\(goal[ \t\n\r]\\|pro\\(p\\(osition\\)?\\|ve_claim\\)\\|lem\\(ma\\)?\\|fact\\|cor\\(ollary\\)?\\|theo\\(rem\\)?\\)"
   proof-save-command-regexp       "\\`save"
   proof-goal-with-hole-regexp  
   (concat 
    "\\`\\(Local[ \t\n\r]+\\)?\\(pro\\(p\\(osition\\)?\\|ve_claim\\)\\(osition\\)?\\|lem\\(ma\\)?\\|fact\\|cor\\(ollary\\)?\\|theo\\(rem\\)?\\)"
    phox-strict-comments-regexp
    phox-ident-regexp)
   proof-goal-with-hole-result     16
   proof-save-with-hole-regexp     
   (concat 
    "\\`save"
    phox-strict-comments-regexp
    phox-ident-regexp)
   proof-save-with-hole-result     8
   proof-ignore-for-undo-count     "\\`\\(constraints\\|flag\\|goals\\|pri\\(nt\\(_sort\\)?\\|ority\\)\\|eshow\\|search\\|depend\\)"
   proof-non-undoables-regexp      "\\`\\(undo\\|abort\\)"
   proof-shell-error-regexp        "^\\([^ \n\t\r]* \\)?\\(\\(e\\|E\\)rror\\)\\|\\(\\(f\\|F\\)ailure\\)"
   proof-goal-command              "goal %s."
   proof-save-command              "save %s."
   proof-kill-goal-command         "abort."
   proof-showproof-command         "goals."
   proof-undo-n-times-cmd          "undo %s."
   proof-find-and-forget-fn        'phox-find-and-forget
   proof-find-theorems-command      "search \"%s\"."
   proof-auto-multiple-files       nil
   font-lock-keywords              (append phox-font-lock-keywords proof-xsym-font-lock-keywords)
   )
  (phox-init-syntax-table)
  (setq pbp-goal-command "intro %s;")
  (setq pbp-hyp-command "elim %s;")
;; the following is only useful for xemacs
  (define-key phox-mode-map [(meta ?.)] 'phox-complete-tag)
)

(defun phox-shell-config ()
  "Configure Proof General shell for PhoX."
  (setq
   ;proof-shell-cd-cmd              "cd \"%s\""
   proof-shell-prompt-pattern      "\\(>phox> \\)\\|\\(%phox% \\)"
   proof-shell-annotated-prompt-regexp  "\\(>phox> \\)\\|\\(%phox% \\)"
   proof-shell-interrupt-regexp    "Interrupt"
   proof-shell-start-goals-regexp  "^\\(Here \\(are\\|is\\) the goal\\)\\|\\([0-9]+ new goals?\\)\\|\\([0-9]+ goals? possibly instanced\\)"
   proof-shell-end-goals-regexp  "^End of goals."
   proof-shell-quit-cmd            "quit."
   proof-shell-restart-cmd         "restart."
   proof-shell-proof-completed-regexp   "^.*^proved"
))


;;
;; ======== Defining the derived modes ========
;;

;; The name of the script mode is always <proofsym>-script,
;; but the others can be whatever you like.
;;
;; The derived modes set the variables, then call the
;; <mode>-config-done function to complete configuration.

(define-derived-mode phox-mode proof-mode
    "PhoX script" nil
    (phox-config)
    (proof-config-done)
    (phox-setup-outline)
    (define-key phox-mode-map [(control j)] 
      'phox-assert-next-command-interactive)
    ;; with the previous binding,
    ;; it is nice to do : xmodmap -e "keysym KP_Enter = Linefeed"

    (define-key phox-mode-map [(control c) (meta d)] 
      'phox-delete-symbol-on-cursor)  
    (if phox-x-symbol-enable
	(progn
	  (setq x-symbol-language 'phox)
	  (x-symbol-mode t))) ; just to be sure
    (font-lock-mode t) ; just to be sure (not always activated on OSX ??

)

(define-derived-mode phox-shell-mode proof-shell-mode
   "PhoX shell" nil
   (phox-shell-config)
   (proof-shell-config-done))

(define-derived-mode phox-response-mode proof-response-mode
  "PhoX response" nil
  (setq 
   font-lock-keywords (append phox-font-lock-keywords proof-xsym-font-lock-keywords)
   proof-output-fontify-enable     t)
  (proof-response-config-done))

(define-derived-mode phox-goals-mode proof-goals-mode
  "PhoX goals" nil
  (setq 
   font-lock-keywords  (append phox-font-lock-keywords proof-xsym-font-lock-keywords)
   proof-output-fontify-enable     t)
  (proof-goals-config-done))

;; The response buffer and goals buffer modes defined above are
;; trivial.  In fact, we don't need t�o define them at all -- they
;; would simply default to "proof-response-mode" and "pg-goals-mode".

;; A more sophisticated instantiation might set font-lock-keywords to
;; add highlighting, or some of the proof by pointing markup
;; configuration for the goals buffer.

;; The final piece of magic here is a hook which configures settings
;; to get the proof shell running.  Proof General needs to know the
;; name of the program to run, and the modes for the shell, response,
;; and goals buffers.

(add-hook 'proof-pre-shell-start-hook 'phox-pre-shell-start)

(defun phox-pre-shell-start ()
  (setq proof-prog-name		phox-prog-name)
  (setq proof-mode-for-shell    'phox-shell-mode)
  (setq proof-mode-for-response 'phox-response-mode)
  (setq proof-mode-for-goals	'phox-goals-mode))

; completions
; dans completions.el
;(setq completion-min-length 6)
;(setq completion-prefix-min-length 3) les mots de moins de 6 caract�res
; ne sont pas pris en compte.  Les prefixes de moins de 3 caract�res ne
; sont pas non plus pris en compte.

; (set-variable 'phox-completion-table 
(defpgdefault completion-table
  (append phox-top-keywords phox-proof-keywords)
)

;;;
;;; X-Symbol
;;;

(defpgdefault x-symbol-language 'phox)

(eval-after-load "x-symbol-phox"
 ;; Add x-symbol tokens to phox-completion-table and rebuild
 ;; internal completion table if completion is already active
'(progn
   (defpgdefault completion-table
     (append (proof-ass completion-table)
	     (mapcar (lambda (xsym) (nth 2 xsym))
		     x-symbol-phox-table)))
   (setq proof-xsym-font-lock-keywords
	 x-symbol-phox-font-lock-keywords)
   (if (featurep 'completion)
       (proof-add-completions))))

(provide 'phox)


