;; isa.el Major mode for Isabelle proof assistant
;; Copyright (C) 1994-1998 LFCS Edinburgh. 
;;
;; Author:      David Aspinall <da@dcs.ed.ac.uk>
;; Maintainer:  Isabelle maintainer <isabelle@dcs.ed.ac.uk>

;;
;; $Id$
;;


(setq proof-tags-support nil)  ; we don't want it, no isatags prog.

;; Add Isabelle image onto splash screen
(custom-set-variables
 '(proof-splash-extensions
   '(list
     nil
     (proof-splash-display-image "isabelle_transparent" t))))
(require 'proof)
(require 'isa-syntax)

;; FIXME: outline should be autoloaded
(require 'outline)

;; To make byte compiler be quiet.
(eval-when-compile
  (require 'proof-shell)
  (require 'proof-script)
  (cond ((fboundp 'make-extent) (require 'span-extent))
	((fboundp 'make-overlay) (require 'span-overlay))))


;;; variable: proof-analyse-using-stack
;;;    proof-locked-region-empty-p, proof-shell-insert, pbp-mode,
;;;    proof-mark-buffer-atomic, proof-shell-invisible-command,
;;;    prev-span, span-property, next-span, proof-unprocessed-begin,
;;;    proof-config-done, proof-shell-config-done


;;;
;;; ======== User settings for Isabelle ========
;;;

;;; proof-site provides us with the cusomization groups
;;;
;;; 'isabelle         -  User options for Isabelle Proof General
;;; 'isabelle-config  -  Configuration of Isabelle Proof General
;;;			 (constants, but may be nice to tweak)

(defcustom isabelle-prog-name "isabelle"
  "*Name of program to run Isabelle."
  :type 'file
  :group 'isabelle)

(defcustom isabelle-indent 2
  "*Indentation degree in proof scripts.
Somewhat irrelevant for Isabelle because normal proof scripts have
no regular or easily discernable structure."
  :type 'number
  :group 'isabelle)

(defcustom isabelle-web-page
  ;; "http://www.cl.cam.ac.uk/Research/HVG/isabelle.html"
  "http://www.dcs.ed.ac.uk/home/isabelle"
  "URL of web page for Isabelle."
  :type 'string
  :group 'isabelle)

;;;
;;; ======== Configuration of generic modes ========
;;;

;; ===== outline mode

;;; FIXME: test and add more things here
(defcustom isa-outline-regexp
  (proof-ids-to-regexp '("goal" "Goal" "prove_goal"))
  "Outline regexp for Isabelle ML files"
  :type 'regexp
  :group 'isabelle-config)

;;; End of a command needs parsing to find, so this is approximate.
(defcustom isa-outline-heading-end-regexp ";[ \t\n]*"
  "Outline heading end regexp for Isabelle ML files."
  :type 'regexp
  :group 'isabelle-config)

;; FIXME: not sure about this one
(defvar isa-shell-outline-regexp "\370[ \t]*\\([0-9]+\\)\\.")
(defvar isa-shell-outline-heading-end-regexp "$")

;;; ---- end-outline



;;; NB!  Disadvantage of *not* shadowing variables is that user
;;; cannot override them.  It might be nice to override some
;;; variables, which ones?

(defun isa-mode-config-set-variables ()
  "Configure generic proof scripting mode variables for Isabelle."
  (setq
   proof-assistant-home-page	isabelle-web-page
   proof-mode-for-script	'isa-proofscript-mode
   ;; proof script syntax
   proof-terminal-char		?\;	; ends a proof
   proof-comment-start		"(*"	; comment in a proof
   proof-comment-end		"*)"	; 
   ;; proof engine output syntax
   proof-save-command-regexp    isa-save-command-regexp
   proof-save-with-hole-regexp  isa-save-with-hole-regexp
   ;; Next one used for func-menu.
   proof-goal-with-hole-regexp  isa-goal-with-hole-regexp
   proof-commands-regexp	(proof-ids-to-regexp isa-keywords)
   ;; proof engine commands (first three for menus, last for undo)
   proof-prf-string		"pr();"
   proof-goal-command		"Goal \"%s\";"
   proof-save-command		"qed \"%s\";"
   proof-ctxt-string		"ProofGeneral.show_context();"
   proof-help-string		"ProofGeneral.help();"
   proof-kill-goal-command	"ProofGeneral.kill_goal();"
   ;; command hooks
   proof-goal-command-p		'isa-goal-command-p
   proof-count-undos-fn		'isa-count-undos
   proof-find-and-forget-fn	'isa-find-and-forget
   proof-goal-hyp-fn		'isa-goal-hyp
   proof-state-preserving-p	'isa-state-preserving-p
   proof-parse-indent		'isa-parse-indent
   proof-stack-to-indent	'isa-stack-to-indent
   proof-shell-compute-new-files-list 'isa-shell-compute-new-files-list))


(defun isa-shell-mode-config-set-variables ()
  "Configure generic proof shell mode variables for Isabelle."
  (setq
   proof-shell-first-special-char	?\360

   proof-shell-annotated-prompt-regexp   ; ">\372"
   "^\\(val it = () : unit\n\\)?> "
   ;; non-annotation, with val it's: "^\\(val it = () : unit\n\\)?> "

   ;; This pattern is just for comint, it matches a range of
   ;; prompts from a range of MLs.
   proof-shell-prompt-pattern		"^2?[-=#>]>? *"

   ;; for issuing command, not used to track cwd in any way.
   proof-shell-cd			"cd \"%s\";"
   proof-shell-proof-completed-regexp   "No subgoals!"

   ;; FIXME: the next two are probably only good for NJ/SML
   proof-shell-error-regexp		"^.*Error:\\|^\\*\\*\\*"
   proof-shell-interrupt-regexp         "Interrupt"
   
   ;; nothing appropriate for: proof-shell-abort-goal-regexp

   ;; proof-shell-noise-regexp isn't used anywhere at the moment.
   proof-shell-noise-regexp	        "val it = () : unit\n"

   ;; matches names of assumptions
   proof-shell-assumption-regexp	isa-id
   ;; matches subgoal name
   proof-shell-goal-regexp		"\370[ \t]*\\([0-9]+\\)\\."

   proof-shell-wakeup-char		?\372
   proof-shell-start-goals-regexp	"\366"
   proof-shell-end-goals-regexp		"\367"
   proof-shell-goal-char	        ?\370
   ;; initial command configures Isabelle by hacking print functions.
   proof-shell-init-cmd
     (concat "use \"" proof-home-directory "isa/ProofGeneral.ML\";")
   proof-shell-eager-annotation-start   "\360\\|\362\\|\364"
   proof-shell-eager-annotation-end     "\361\\|\363\\|\365"

   ;; Tested values of proof-shell-eager-annotation-start: 
   ;; "^\\[opening \\|^###\\|^Reading\\|^Proof General\\|^Not reading"
   ;; "^---\\|^\\[opening "
   ;; could be last bracket on end of line, or with ### and ***.

   ;; === ANNOTATIONS  === ones below here are broken
   proof-shell-result-start	        "\372 Pbp result \373"
   proof-shell-result-end		"\372 End Pbp result \373"
   proof-analyse-using-stack		t
   proof-shell-start-char		?\372
   proof-shell-end-char			?\373
   proof-shell-field-char		?\374
   ;; NEW NEW for multiple files
   ;; === NEW NEW: multiple file stuff.  move elsewhere later.
   proof-shell-process-file 
   (cons
    ;; Theory loader output and verbose update() output.
    "Reading \"\\(.*\\)\"\\|Not reading \"\\(.*\\)\""
    (lambda (str)
      (or (match-string 1 str) 
	  (match-string 2 str))))
   ;; This is the output returned by a special command to
   ;; query Isabelle for outdated files.
   proof-shell-retract-files-regexp
   "Proof General, you can unlock the file \"\\(.*\\)\""
   proof-shell-compute-new-files-list 'isa-shell-compute-new-files-list
   )
  (add-hook 'proof-activate-scripting-hook 'isa-shell-hack-use-thy)
  )


;;;
;;;  use_thy and friends.
;;;
;;; Quite tricky to get these right.  By default, Isabelle's
;;; theory loader glues together theory and ML files whenever
;;; it can, but that's not what we want here.
;;;
;;; So we rely on some hacked versions.
;;;

(defcustom isa-usethy-notopml-command "use_thy_and_update \"%s\";"
  "Sent to Isabelle to process theory for this ML file, and all ancestors."
  :type 'string
  :group 'isabelle-config)

(defun isa-shell-hack-use-thy ()
  "Possibly issue  use_thy_no_topml command to Isabelle.
If the current buffer has an empty locked region, the interface is
about to send commands from it to Isabelle.  This function sends
a command to read any theory file corresponding to the current ML file.
This is a hook function for proof-activate-scripting-hook."
  (if (and
       (proof-locked-region-empty-p)
       ;; If we switch to this buffer and it *does* have a locked
       ;; region, we could check that no updates are needed and
       ;; unlock the whole buffer in case they were.  But that's
       ;; a bit messy.  Instead we assume that things must be
       ;; up to date, after all, the user wasn't allowed to edit
       ;; anything that this file depends on, was she?
       buffer-file-name
       (file-exists-p 
	(concat (file-name-sans-extension buffer-file-name) ".thy")))
      ;; Send a use_thy command if there is a corresponding .thy file.
      ;; Let Isabelle do the work of checking whether any work needs
      ;; doing.  Really this should be force_use_thy, too.
      (proof-shell-insert
       (format isa-usethy-notopml-command
	       (file-name-sans-extension buffer-file-name)))))

(defun isa-shell-compute-new-files-list (str)
  "Compute the new list of files read by the proof assistant.
This is called when Proof General spots output matching
proof-shell-retract-files-regexp."
  ;; This assertion is supposed to test that we only remove
  ;; what was in the list anyway.  But 
  ;;(assert (member (file-truename (match-string 1 str))
  ;;		  proof-included-files-list))
  (remove (file-truename (match-string 1 str)) 
	  proof-included-files-list))



;;
;;   Define the derived modes 
;;
(define-derived-mode isa-shell-mode proof-shell-mode
   "Isabelle shell" nil
   (isa-shell-mode-config))

(define-derived-mode isa-pbp-mode pbp-mode
  "Isabelle proofstate" nil
  (isa-pbp-mode-config))

(define-derived-mode isa-proofscript-mode proof-mode
   "Isabelle script" nil
   (isa-mode-config))

  



;; Automatically selecting theory mode or Proof General script mode.

(defun isa-mode ()
  "Mode for Isabelle buffers: either isa-proofscript-mode or thy-mode.
Files with extension .thy will be in thy-mode, otherwise we choose
isa-proofscript-mode."
  (interactive)
  (cond
   (;; Theory files only if they have the right extension
    (and (buffer-file-name)
	 (string-match ".thy" (buffer-file-name)))
    (thy-mode)
    ;; Hack for splash screen
    (if (memq 'proof-splash-timeout-waiter proof-mode-hook)
	(proof-splash-timeout-waiter))
    ;; Has this theory file already been loaded by Isabelle?
    ;; Colour it blue if so.  
    (and (member buffer-file-truename proof-included-files-list)
	 (proof-mark-buffer-atomic (current-buffer)))
    )
   (t 
    ;; Proof mode does that automatically.
    (isa-proofscript-mode))))

(eval-after-load 
 "thy-mode"
 ;; Extend theory mode keymap
 '(let ((map thy-mode-map))
(define-key map "\C-c\C-b" 'isa-process-thy-file)
(define-key map "\C-c\C-u" 'isa-retract-thy-file)))

(defun isa-process-thy-file (file)
  "Process the theory file FILE.  If interactive, use buffer-file-name."
  (interactive (list buffer-file-name))
  (proof-shell-invisible-command 
   (format isa-usethy-notopml-command
	   (file-name-sans-extension file))))

(defcustom isa-retract-thy-file-command "ProofGeneral.retract_thy_file \"%s\";"
  "Sent to Isabelle to forget theory file and descendants.
Resulting output from Isabelle will be parsed by Proof General."
  :type 'string
  :group 'isabelle-config)

(defcustom isa-retract-ML-file-command "ProofGeneral.retract_ML_file \"%s\";"
  "Sent to Isabelle to forget ML file and descendants.
Resulting output from Isabelle will be parsed by Proof General."
  :type 'string
  :group 'isabelle-config)

(defun isa-retract-thy-file (file)
  "Retract the theory file FILE. If interactive, use buffer-file-name."
  (interactive (list buffer-file-name))
  (proof-shell-invisible-command
   (format isa-retract-thy-file-command
	   (file-name-sans-extension file))))


;; Next portion taken from isa-load.el
;; isa-load.el,v 3.8 1998/09/01 

(defcustom isabelle-use-sml-mode
   (if (fboundp 'sml-mode) 'sml-mode)
  "*If non-nil, attempt to use sml-mode in ML section of theory files."
  :type 'boolean
  :group 'isabelle)

(defgroup thy nil
  "Customization of Isamode's theory editing mode"
  ;; :link '(info-link "(Isamode)Theory Files")
  :load 'thy-mode
  :group 'isabelle)

(autoload 'thy-mode "thy-mode" 
	  "Major mode for Isabelle theory files" t nil)

(autoload 'thy-find-other-file "thy-mode" 
	    "Find associated .ML or .thy file." t nil)

;; Key to switch to theory mode
(define-key isa-proofscript-mode-map 
  [(control c) (control o)] 'thy-find-other-file)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   Code that's Isabelle specific                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; FIXME: think about the next variable.  I've changed sense from
;;  LEGO and Coq's treatment.
(defcustom isa-not-undoable-commands-regexp
  (proof-ids-to-regexp '("undo" "back"))
  "Regular expression matching commands which are *not* undoable."
  :type 'regexp
  :group 'isabelle-config)

;; This next function is the important one for undo operations.
(defun isa-count-undos (span)
  "Count number of undos in a span, return the command needed to undo that far."
  (let ((ct 0) str i)
    (if (and span (prev-span span 'type)
	     (not (eq (span-property (prev-span span 'type) 'type) 'comment))
	     (isa-goal-command-p
	      (span-property (prev-span span 'type) 'cmd)))
	(concat "choplev 0" proof-terminal-string)
      (while span
	(setq str (span-property span 'cmd))
	(cond ((eq (span-property span 'type) 'vanilla)
	       (or (string-match isa-not-undoable-commands-regexp str)
		   (setq ct (+ 1 ct))))
	      ((eq (span-property span 'type) 'pbp)
	       ;; this case probably redundant for Isabelle, unless
	       ;; we think of some nice ways of matching non-undoable
	       ;; commands.
	       (cond ((not (string-match isa-not-undoable-commands-regexp str))
		      (setq i 0)
		      (while (< i (length str))
			(if (= (aref str i) proof-terminal-char)
			    (setq ct (+ 1 ct)))
			(setq i (+ 1 i))))
		     (t nil))))
	(setq span (next-span span 'type)))
      (concat "ProofGeneral.repeat_undo " (int-to-string ct) proof-terminal-string))))

(defun isa-goal-command-p (str)
  "Decide whether argument is a goal or not"
  (string-match isa-goal-command-regexp str)) ; this regexp defined in isa-syntax.el

;; Isabelle has no concept of a Linear context, so forgetting back
;; to the declaration of a particular something makes no real
;; sense.  Perhaps in the future there will be functions to remove
;; theorems from theories, but even then all we could do is
;; forget particular theorems one by one.  So we ought to search
;; backwards in isa-find-and-forget, rather than forwards as
;; the old code below does.

(defun isa-find-and-forget (span)
  "Return a command to be used to forget SPAN."
  (save-excursion
    ;; See if we are going to part way through a completely processed
    ;; buffer, in which case it should be removed from 
    ;; proof-included-files-list along with any other buffers
    ;; depending on it.  NB: perhaps this is called too often for
    ;; a bunch of spans in a region?
    (goto-char (point-max))
    (skip-chars-backward " \t\n") 
    (if (>= (proof-unprocessed-begin) (point))
	(format isa-retract-ML-file-command 
		(file-name-sans-extension 
		 (file-name-nondirectory
		  (buffer-file-name))))
      proof-no-command)))


;; BEGIN Old code  (taken from Coq.el)
;(defconst isa-keywords-decl-defn-regexp
;  (ids-to-regexp (append isa-keywords-decl isa-keywords-defn))
;  "Declaration and definition regexp.")
;(defun isa-find-and-forget (span)
;  (let (str ans)
;    (while (and span (not ans))
;      (setq str (span-property span 'cmd))
;      (cond
;       ((eq (span-property span 'type) 'comment))       

;       ((eq (span-property span 'type) 'goalsave)
;	(setq ans (concat isa-forget-id-command
;			  (span-property span 'name) proof-terminal-string)))

;       ((string-match (concat "\\`\\(" isa-keywords-decl-defn-regexp
;                              "\\)\\s-*\\(" proof-id "\\)\\s-*[\\[,:]") str)
;	(setq ans (concat isa-forget-id-command
;			  (match-string 2 str) proof-terminal-string)))
;       ;; If it's not a goal but it contains "Definition" then it's a
;       ;; declaration
;       ((and (not (isa-goal-command-p str))
;	     (string-match
;	      (concat "Definition\\s-+\\(" proof-id "\\)\\s-*:") str))
;	(setq ans (concat isa-forget-id-command
;			  (match-string 2 str) proof-terminal-string))))
;      (setq span (next-span span 'type)))
;      (or ans "COMMENT")))
; END old code 

(defvar isa-current-goal 1
  "Last goal that emacs looked at.")

;; Parse proofstate output.  Isabelle does not display
;; named hypotheses in the proofstate output:  they
;; appear as a list in each subgoal.  Ignore
;; that aspect for now and just return the
;; subgoal number.
(defun isa-goal-hyp ()
  (if (looking-at proof-shell-goal-regexp)
      (cons 'goal (match-string 1))))

(defun isa-state-preserving-p (cmd)
  "Non-nil if command preserves the proofstate."
  (string-match isa-not-undoable-commands-regexp cmd))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   Indentation                                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Sadly this is pretty pointless for Isabelle.
;; Proof scripts in Isabelle don't really have an easily-observed
;; block structure  -- a case split can be done by any obscure tactic,
;; and then solved in a number of steps that bears no relation to the
;; number of cases!  And the end is certainly not marked in anyway.
;; 
(defun isa-stack-to-indent (stack)
    (cond
   ((null stack) 0)
   ((null (car (car stack)))
    (nth 1 (car stack)))
   (t (save-excursion
	(goto-char (nth 1 (car stack)))
	(+ isabelle-indent (current-column))))))

(defun isa-parse-indent (c stack)
  "Indentation function for Isabelle.  Does nothing."
  stack)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   Isa shell startup and exit hooks                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun isa-pre-shell-start ()
  (setq proof-prog-name		isabelle-prog-name)
  (setq proof-mode-for-shell    'isa-shell-mode)
  (setq proof-mode-for-pbp	'isa-pbp-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   Configuring proof and pbp mode and setting up various utilities  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun isa-init-syntax-table ()
  "Set appropriate values for syntax table in current buffer."
  (modify-syntax-entry ?\$ ".")
  (modify-syntax-entry ?\/ ".")
  (modify-syntax-entry ?\\ ".")
  (modify-syntax-entry ?+  ".")
  (modify-syntax-entry ?-  ".")
  (modify-syntax-entry ?=  ".")
  (modify-syntax-entry ?%  ".")
  (modify-syntax-entry ?<  ".")
  (modify-syntax-entry ?>  ".")
  (modify-syntax-entry ?\& ".")
  (modify-syntax-entry ?_  "_")
  (modify-syntax-entry ?\' "_")
  (modify-syntax-entry ?\| ".")
  (modify-syntax-entry ?\* ". 23")
  (modify-syntax-entry ?\( "()1")
  (modify-syntax-entry ?\) ")(4"))

(defun isa-mode-config ()
  (isa-mode-config-set-variables)
  (isa-init-syntax-table)
  (setq font-lock-keywords isa-font-lock-keywords-1)
  (proof-config-done)
  ;; outline
  ;; FIXME: do we need to call make-local-variable here?
  (make-local-variable 'outline-regexp)
  (setq outline-regexp isa-outline-regexp)
  (make-local-variable 'outline-heading-end-regexp)
  (setq outline-heading-end-regexp isa-outline-heading-end-regexp)
  ;; tags
  ;  (and (boundp 'tag-table-alist)
  ;       (setq tag-table-alist
  ;	     (append '(("\\.ML$"  . isa-ML-file-tags-table)
  ;		       ("\\.thy$" . thy-file-tags-table))
  ;		     tag-table-alist)))
  (setq blink-matching-paren-dont-ignore-comments t))


;; This hook is added on load because proof shells can
;; be started from .thy (not in scripting mode) or .ML files.
(add-hook 'proof-pre-shell-start-hook 'isa-pre-shell-start nil t)

(defun isa-shell-mode-config ()
  "Configure Proof General proof shell for Isabelle."
  (isa-init-syntax-table)
  (isa-shell-mode-config-set-variables)
  (proof-shell-config-done))

;; FIXME: broken, of course, as is all PBP everywhere.
(defun isa-pbp-mode-config ()
  (setq pbp-change-goal "Show %s.")
  (setq pbp-error-regexp proof-shell-error-regexp))

(provide 'isa)
