;; coq.el Major mode for Coq proof assistant
;; Copyright (C) 1994 - 1998 LFCS Edinburgh. 
;; Author: Healfdene Goguen
;; Maintainer: Pierre Courtieu <courtieu@lri.fr>

;; $Id$ 

(require 'proof)
(require 'holes-load) ; in lib directory
;; coq-syntaxe is required below
;; ----- coq-shell configuration options

(defcustom coq-prog-name "coqtop -emacs"
  "*Name of program to run as Coq."
  :type 'string
  :group 'coq)

(defcustom coq-compile-file-command "coqc %s"
  "*Command to compile a coq file.
This is called when `coq-auto-compile-vos' is set, unless a Makefile
exists in the directory, in which case the `compile' command is run.
To disable coqc being called (and use only make), set this to nil."
  :type 'string
  :group 'coq)

;; Command to initialize the Coq Proof Assistant

(defcustom coq-default-undo-limit 100
  "Maximum number of Undo's possible when doing a proof."
  :type 'number
  :group 'coq)

(defconst coq-shell-init-cmd 
  (format "Set Undo %s. " coq-default-undo-limit))

;; da 15/02/03: moved setting of coq-version-is-vX to coq-syntax to
;; fix compilation

(require 'coq-syntax)
(require 'coq-indent)

;; Command to reset the Coq Proof Assistant
(defconst coq-shell-restart-cmd "Reset Initial.\n ")


;; NB: da: PG 3.5: added \n here to account for blank line
;; in Coq output.  (FIXME: Is this OK in Coq pre 8.x?)
;; Better result for shrinking windows, grabbing shell output
;; Pierre added the state number here, this is new in Coq.

; patch provisoire envoye a coqdev:
;(defvar coq-shell-prompt-pattern (concat "^\n?" proof-id " < "  "\\(?:\\*[0-9]+\\)?")
(defvar coq-shell-prompt-pattern 
  (concat "^\n?" proof-id " < "  "\\(?:\\*[0-9]+\\)?")
  "*The prompt pattern for the inferior shell running coq.")

;; FIXME da: this was disabled (set to nil) -- why?
;; da: 3.5: add experimetntal
(defvar coq-shell-cd 
  "Add LoadPath \"%s\"." ;; fixes unadorned Require (if .vo exists).
  "*Command of the inferior process to change the directory.")

(defvar coq-shell-abort-goal-regexp "Current goal aborted"
  "*Regexp indicating that the current goal has been abandoned.")

(defvar coq-shell-proof-completed-regexp "Subtree proved!"
  "*Regular expression indicating that the proof has been completed.")

(defvar coq-goal-regexp
  "\\(============================\\)\\|\\(subgoal [0-9]+ is:\\)\n")



;; Configuration

(setq tags-always-exact t) ; Tags is unusable with Coq library otherwise:

(defun coq-library-directory () 
  (let ((c (substring (shell-command-to-string "coqtop -where") 0 -1 )))
    (if (string-match c "not found")
	  "/usr/local/lib/coq"
      c)))


(defcustom coq-tags (concat (coq-library-directory) "/theories/TAGS")
  "the default TAGS table for the Coq library"
  :type 'string
  :group 'coq)

(defconst coq-interrupt-regexp "User Interrupt."
  "Regexp corresponding to an interrupt")

;; ----- web documentation

(defcustom coq-www-home-page "http://coq.inria.fr/"
  "Coq home page URL."
  :type 'string
  :group 'coq)


;; ----- coq specific menu is defined in coq-abbrev.el

(require 'coq-abbrev)

(defun coq-insert-section (s)
  (interactive  "sSection name: ")
  (let ((p (point)))
    (insert "Section " s ".\n#\nEnd " s ".")
    (holes-replace-string-by-holes-backward p)
    (goto-char p)
    (holes-set-point-next-hole-destroy))
)

(defconst module-kinds-table 
  '(("Module" 1) ("Module Type" 2) ("Declare Module" 3))
  "Enumerates the different kinds of modules.")

(defconst modtype-kinds-table
  '(("" 1) (":" 2) ("<:" 3))
  "Enumerates the different kinds of type information for modules.")

(defun coq-insert-module ()
  (interactive)
  (let* ((mods (completing-read "kind of module (tab to see list): " module-kinds-table))
         (s (read-string  "Module name: "))
         (typkind (completing-read "kind of type (optional, tab to see list): " modtype-kinds-table))
         (p (point)))
    (if (string-equal typkind "")
        (progn
          (insert mods " " s ".\n#\nEnd " s ".")
          (holes-replace-string-by-holes-backward p)
          (goto-char p))
      (insert mods " " s " " typkind " #.\n#\nEnd " s ".")
      (holes-replace-string-by-holes-backward p)
      (goto-char p)
      (holes-set-point-next-hole-destroy))
    )
  )
;  (completing-read "Section name: " )

;; ----- outline

(defvar coq-outline-regexp
  (concat "(\\*\\|" (proof-ids-to-regexp 
	   '(
"Tactic" "Axiom" "Parameter" "Parameters" "Variable" "Variables" "Syntax" "Grammar" "Syntactic" "Load" "Require" "Import" "Hint" "Hints" "Hypothesis" "Correctness" "Module" "Section" "Chapter" "Goal" "Lemma" "Theorem" "Fact" "Remark" "Record" "Inductive" "Mutual" "Definition" "Fixpoint" "Save" "Qed" "Defined" "End" "Coercion"))))

(defvar coq-outline-heading-end-regexp "\\*\)\n\\|\\.\n")

(defvar coq-shell-outline-regexp coq-goal-regexp)
(defvar coq-shell-outline-heading-end-regexp coq-goal-regexp)

(defconst coq-kill-goal-command "Abort. ")
(defconst coq-forget-id-command "Reset %s. ")
(defconst coq-back-n-command "Back %s. ") ; Pierre: added 



(defconst coq-state-changing-tactics-regexp 
  (proof-ids-to-regexp coq-state-changing-tactics))
(defconst coq-state-preserving-tactics-regexp 
  (proof-ids-to-regexp coq-state-preserving-tactics))
(defconst coq-tactics-regexp
  (proof-ids-to-regexp coq-tactics))
(defconst coq-state-changing-commands-regexp
  (proof-ids-to-regexp coq-keywords-state-changing-commands))
(defconst coq-state-preserving-commands-regexp 
  (proof-ids-to-regexp coq-keywords-state-preserving-commands))
(defconst coq-commands-regexp 
  (proof-ids-to-regexp coq-keywords-commands))
(defvar coq-retractable-instruct-regexp 
  (proof-ids-to-regexp coq-retractable-instruct))
(defvar coq-non-retractable-instruct-regexp
  (proof-ids-to-regexp coq-non-retractable-instruct))

(defvar coq-keywords-section
  '("Section" "Module\\-+Type" "Declare\\s-+Module" "Module"))

(defvar coq-section-regexp 
  (concat "\\(" (proof-ids-to-regexp coq-keywords-section) "\\)")
;  "\\(\\<Section\\>\\|\\<Module\\>\\-+\\<Type\\>\\|\\<Module\\>\\)"
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   Derived modes - they're here 'cos they define keymaps 'n stuff ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-and-compile 
  (define-derived-mode coq-shell-mode proof-shell-mode
    "coq-shell" nil
    (coq-shell-mode-config)))
  
(eval-and-compile 
  (define-derived-mode coq-response-mode proof-response-mode
  "CoqResp" nil
    (coq-response-config)))
 
(eval-and-compile
  (define-derived-mode coq-mode proof-mode
   "coq" nil
   (coq-mode-config)))

(eval-and-compile
  (define-derived-mode coq-goals-mode proof-goals-mode
    "CoqGoals" nil
    (coq-goals-mode-config)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   Code that's coq specific                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun coq-set-undo-limit (undos)
  (proof-shell-invisible-command (format "Set Undo %s. " undos)))

;; da: have now combined count-undos and find-and-forget, they're the
;; same now we deal with nested proofs and send general sequence
;; "Abort. ... Abort. BackTo n. Undo n."
;;

(defconst coq-keywords-decl-defn-regexp
  (proof-ids-to-regexp (append coq-keywords-decl coq-keywords-defn))
  "Declaration and definition regexp.")

(defun coq-proof-mode-p ()
  "Allows to know if we are currentlly in proof mode. 
Look at the last line of the *coq* buffer to see if the prompt is the
toplevel \"Coq <\". Returns nil if yes. This assumes that no
\"Resume\" or \"Suspend\" command has been used."
  (not (string-match "^Coq < " proof-shell-last-prompt)))


;; DA: rewrote to combine behaviour with count-undos, to work with
;; nested proofs.


(defun coq-is-comment-or-proverprocp (span) 
  (memq (span-property span 'type) '(comment proverproc)))
(defun coq-is-goalsave-p (span) (eq (span-property span 'type) 'goalsave))
(defun coq-is-module-equal-p (str) ;;cannot appear vith coq < 7.4
  (and (proof-string-match "\\`\\(Declare\\s-\\)?\\s-*\\<Module\\>.*:=" str)
       (not (proof-string-match "\\<with\\>" str))))
(defun coq-is-def-p (str) 
  (proof-string-match (concat "\\`Definition\\s-+\\(" proof-id "\\)\\s-*") str))
(defun coq-is-decl-defn-p (str)
  (proof-string-match 
   (concat "\\`\\(" coq-keywords-decl-defn-regexp "\\)\\s-*\\(" 
           proof-id "\\)\\s-*[\\[,:.]") str))

(defun coq-state-preserving-command-p (str)
  (proof-string-match (concat "\\`\\(" coq-state-preserving-commands-regexp "\\)") str))

(defun coq-command-p (str)
  (proof-string-match (concat "\\`\\(" coq-commands-regexp "\\)") str))

(defun coq-state-preserving-tactic-p (str)
  (proof-string-match 
   (concat "\\`\\(" 
           coq-state-preserving-tactics-regexp "\\)") str))

(defun coq-state-changing-tactic-p (str) ; unknown things are considered (state
                                        ;changing) tactics here
  (and (not (coq-command-p str)) 
       (not (coq-state-preserving-tactic-p str)))
  )
;  (proof-string-match 
;   (concat "\\(\\`\\|\\`abstract\\s-+\\)\\(" 
;           coq-state-changing-tactics-regexp "\\)") str))


(defun coq-state-changing-command-p (str)
  (proof-string-match (concat "\\`\\(" coq-state-changing-commands-regexp "\\)") str))

; if no second id --> name of the module/section is (match-string 2 str)
; otherwise it is (match-string 5 str) 
; to know if there is a second id: (match-string 2 str)="Type" ?
(defun coq-section-or-module-start-p (str)
  (proof-string-match 
   (concat "\\`" coq-section-regexp 
           "\\s-+\\(" proof-id "\\)\\(\\s-*\\(" proof-id "\\)\\)?") str)) 


;;New: using the statenumber inside the coq prompte to backtrack more easily
;;FIXME: make this more robust and faster
(defun coq-last-statenum (s)
  "reads the last prompt state number in the coq process buffer. It is supposed that
  we are already in the coq buffer."
  (let* ((lastprompt (or s (error "no prompt !!?")))
         (s (string-match "< \\*\\([0-9]+\\)�" lastprompt)))
    (string-to-int (match-string 1 lastprompt))
    )
  )

(defun coq-last-statenum-safe ()
  "See `coq-last-statenum'."
  (coq-last-statenum proof-shell-last-prompt)
  )

;; The state number we want to put in a span is the prompt number given *just before*
;; the command was sent. This variable remembers this number and will be updated when
;; used see coq-set-state-number. Initially 1 because Coq initial state has number 1.
(defvar coq-last-but-one-statenum 1)

(defun coq-get-span-statenum (span)
  "Returns the state number of the span."
  (span-property span 'statenum)
  )

(defun coq-set-span-statenum (span val)
  "Sets the state number of the span to val."
  (set-span-property span 'statenum val)
  )

(defun proof-last-locked-span () 
  (save-excursion ;; didn't found a way to avoid buffer switching
    (set-buffer proof-script-buffer)
    (span-at (- (proof-locked-end) 1) 'type)
    )
  )

;; Each time the state changes (hook below), (try to) put the state number in the
;; last locked span (will fail if there is already a number which should happen when
;; going back in the script).  The state number we put is not the last one because
;; the last one has been sent by Coq *after* the change. We use
;; `coq-last-but-one-statenum' instead and then update it.

(defun coq-set-state-number ()
  "Sets the last locked span's state number to the number found in the *last but one*
  prompt (variable `coq-last-but-one-statenum'), unless it has already a state
  number. Also updates `coq-last-but-one-statenum' to the last state number because
  next time this function will be called, a new prompt will be in
  `proof-shell-last-prompt'."
  (if (and proof-shell-last-prompt proof-script-buffer)
      (let ((sp (proof-last-locked-span)))
        (unless (or (not sp) (coq-get-span-statenum sp))
          (coq-set-span-statenum sp coq-last-but-one-statenum))
        (setq coq-last-but-one-statenum (coq-last-statenum-safe))
        )
    )
  )

;; This hook seems the one we want (if we are in V8.1 mode only):
(add-hook 'proof-state-change-hook 
          '(lambda () (if coq-version-is-V8-1 (coq-set-state-number))))


;; Simplified version of backtracking which uses state numbers
(defun coq-find-and-forget-v81 (span)
  (let (str ans (naborts 0) (nundos 0) 
            (span-staten (coq-get-span-statenum span)))
    ;; go from the top of the backtracked region to the bottom
    ;; and computes naborts and nundos
    (while (and span (not ans))
      (setq str (span-property span 'cmd))
      (cond
       ((coq-is-comment-or-proverprocp span)) ; do nothing for comments
       ;; unsaved goal --> increment naborts 
       ;; (modules are "goals" but not here (they don't need abort, just backto, even
       ;;  if unclosed))
       ((and (not (coq-is-goalsave-p span)) 
             (coq-goal-command-p str)
             (not (coq-section-or-module-start-p str))) 
        (incf naborts))
       ;; if nabort<>0 then current goal is actually aborted
       ((and (coq-proof-mode-p) (coq-state-changing-tactic-p str) (= naborts 0))
        (incf nundos))
       ;; default case: command or state preserving tactic, do nothing (BackTo will
       ;; deal with this)
       (t ())
       )
      ;;go to next span
      (setq span (next-span span 'type))
      )
    ;;let's build the backtrack command now:
    (let (last-staten)
      (setq last-staten (coq-last-statenum-safe))
      (setq ans
            (concat
             (if (> naborts 0)
                 ;; ugly, but blame Coq
                 (let ((aborts "Abort. "))
                   (while (> (decf naborts) 0) (setq aborts (concat "Abort. " aborts)))
                   aborts))
             (if (> nundos 0) 
                 (concat "Undo " (int-to-string nundos) ". "))
             (if (and span-staten last-staten (not (= span-staten last-staten))) 
                 (concat "BackTo " (int-to-string span-staten) ". "))
             ))
      (if (string-equal ans "") proof-no-command ; not here because if
        ;; we backtrack a state preserving command, we must do
        ;; *nothing*, not even a CR (in v74, no prompt is returned
        ;; with "\n")
        ans)
      )
    )
  )


(defun coq-find-and-forget-v80 (span)
  (let (str ans (naborts 0) (nbacks 0) (nundos 0))
    (while (and span (not ans))
      (setq str (span-property span 'cmd))
      (cond
       ((coq-is-comment-or-proverprocp span)) ; do nothing

       ;; Module <Type> T ... :=... . inside proof ->  like Definition...:=...  
       ;; (should actually be disallowed in coq)
       ; Should go in last cond, but I have a problem trying to avoid next cond to match.
       ((and (coq-proof-mode-p) (coq-is-module-equal-p str)) (incf nbacks))

       ;; FIXME: combine with coq-keywords-decl-defn-regexp case below?
       ;; [ Maybe not: Section is being treated as a _goal_ command
       ;;   now, so this test has to appear before the goalsave ]
       ((coq-section-or-module-start-p str)
        (if (equal (match-string 2 str) "Type") ;Module Type id: take the third word
            (setq ans (format coq-forget-id-command (match-string 5 str)))
          (setq ans (format coq-forget-id-command (match-string 2 str))))
        ;; If we're resetting to beginning of a section from inside, need to fix the
        ;; nesting depth.  FIXME: this is not good enough: the scanning loop exits
        ;; immediately but Reset has implicit Aborts which are not being counted
        ;; here.  Really we need to set the "master reset" command which subsumes the
        ;; others, but still count the depth.
        (unless (coq-is-goalsave-p span) (decf proof-nesting-depth)))

       ((coq-is-goalsave-p span)
        ;; da: try using just Back since "Reset" causes loss of proof state.
        (if (span-property span 'nestedundos)
            ;; Pierre: more robust when some unknown commands are not well backtracked
            (if (and (= (span-property span 'nestedundos) 0) (not (coq-proof-mode-p))) 
                (setq ans (format coq-forget-id-command (span-property span 'name)))
              (setq nbacks (+ 1 nbacks (span-property span 'nestedundos))))))
       
       ;; Unsaved goal commands: each time we hit one of these
       ;; we need to issue Abort to drop the proof state.
       ((coq-goal-command-p str) (incf naborts)) ; FIX: nundos<-0 ?

       ;; If we are already outside a proof, issue a Reset.  [ improvement would be
       ;; to see if the undoing will take us outside a proof, and use the first Reset
       ;; found if so: but this is tricky to co-ordinate with the number of Backs,
       ;; perhaps? ]
       ((and (not (coq-proof-mode-p));; (eq proof-nesting-depth 0) 
             (coq-is-decl-defn-p str))
        (setq ans (format coq-forget-id-command (match-string 2 str))))
		 
       ;; Outside a proof: cannot be a tactic, if unknown: do back 
       ;; (we may decide otherwise, it is false anyhow, use elisp 
       ;; vars instead for the perfect thing).
       ((and (not (coq-proof-mode-p)) (not (coq-state-preserving-command-p str)))
        (incf nbacks))

       ;; inside a proof: if known command then back if necessary, nothing otherwise,
       ;; if known "need not undo tactic" then nothing; otherwise : undo (unknown
       ;; tactics are considered needing undo, use elisp vars for the 1% remaining).
       ;; no undo if abort
       ((coq-proof-mode-p)
        (cond 
         ((coq-state-changing-command-p str) (incf nbacks))
         ((and (eq 0 naborts)
               (not (coq-state-preserving-command-p str))
               (not (coq-state-preserving-tactic-p str)))
          (incf nundos)))))
      (setq span (next-span span 'type)))

    ;; Now adjust proof-nesting depth according to the number of proofs we've passed
    ;; out of.  FIXME: really this adjustment should be on the successful completion
    ;; of the Abort commands, as a callback.
    (setq proof-nesting-depth (- proof-nesting-depth naborts))

    (setq ans
          (concat
           (if (stringp ans) ans)
           (if (> naborts 0)
               ;; ugly, but blame Coq
               (let ((aborts "Abort. "))
                 (while (> (decf naborts) 0)
                   (setq aborts (concat "Abort. " aborts)))
                 aborts))
           (if (> nbacks 0)
               (concat "Back " (int-to-string nbacks) ". "))
           (if (> nundos 0) 
               (concat "Undo " (int-to-string nundos) ". "))))

    (if (null ans) proof-no-command;; FIXME: this is an error really (assert nil); 
      (if (string-equal ans "") proof-no-command ; not here because if
						 ; we backtrack a state preserving command, we must do
						 ; *nothing*, not even a CR (in v74, no prompt is returned
						 ; with "\n")
      ans))))

;; I don't like this but it make compilation possible
(defun coq-find-and-forget (span)
 (cond 
  (coq-version-is-V8-1 (coq-find-and-forget-v81 span))
  (coq-version-is-V8-0 (coq-find-and-forget-v80 span))
  (t (coq-find-and-forget-v80 span)) ;; this is temporary
  )
 )  

(defvar coq-current-goal 1
  "Last goal that emacs looked at.")

(defun coq-goal-hyp ()
  (cond 
   ((looking-at "============================\n")
    (goto-char (match-end 0))
    (cons 'goal (int-to-string coq-current-goal)))
   ((looking-at "subgoal \\([0-9]+\\) is:\n")
    (goto-char (match-end 0))
    (cons 'goal (match-string 1))
    (setq coq-current-goal (string-to-int (match-string 1))))
   ((looking-at proof-shell-assumption-regexp)
    (cons 'hyp (match-string 1)))
   (t nil)))

(defun coq-state-preserving-p (cmd)
  (not (proof-string-match coq-retractable-instruct-regexp cmd)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   Commands specific to coq                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun coq-SearchIsos ()
  "Search a term whose type is isomorphic to given type
This is specific to coq-mode."
  (interactive)
  (let (cmd)
    (proof-shell-ready-prover) 
    (setq cmd (read-string "SearchPattern: " nil 'proof-minibuffer-history))
    (proof-shell-invisible-command (format "SearchPattern %s. " cmd))))


(defun coq-guess-or-ask-for-string (s)
  (let ((guess
			(if (region-exists-p) 
				 (buffer-substring-no-properties (region-beginning) (region-end))
				 (symbol-near-point))))
	 (read-string 
	  (if guess (concat s " (" guess "):")(concat s ":"))
	 nil 'proof-minibuffer-history guess))
  )

(defun coq-Print ()
  "Ask for an ident and print the corresponding term"
  (interactive)
  (let (cmd)
    (proof-shell-ready-prover) 
    (setq cmd (coq-guess-or-ask-for-string "Print"))
    (proof-shell-invisible-command
     (format "Print %s. " cmd))))

(defun coq-Check ()
  "Ask for a term and print its type"
  (interactive)
  (let (cmd)
    (proof-shell-ready-prover) 
    (setq cmd (coq-guess-or-ask-for-string "Check"))
    (proof-shell-invisible-command
     (format "Check %s. " cmd))))

(defun coq-Show ()
  "Ask for a number i and show the ith goal"
  (interactive)
  (let (cmd)
    (proof-shell-ready-prover) 
    (setq cmd (read-string "Show Goal number: " nil 'proof-minibuffer-history))
    (proof-shell-invisible-command
     (format "Show %s. " cmd))))


(proof-definvisible coq-PrintHint "Print Hint. ")

;; Items on show menu
(proof-definvisible coq-show-tree "Show Tree.")
(proof-definvisible coq-show-proof "Show Proof.")
(proof-definvisible coq-show-conjectures "Show Conjectures.")
(proof-definvisible coq-show-intros "Show Intros.")
;; Coq ref manual says of show intro: "with an appropriate Proof General macro"... can 
;; we have it to add to PG, please?


(defun coq-PrintHint ()
  "Print all hints applicable to the current goal"
  (interactive)
  (proof-shell-invisible-command "Print Hint. "))


(defun coq-end-Section ()
  "Ends a Coq section."
  (interactive)
  (let ((count 1)) ; The number of section already "Ended" + 1
    (let ((section 
	   (save-excursion 
	     (progn 
	       (while (and (> count 0) 
			   (search-backward-regexp 
			    "Chapter\\|Section\\|End" 0 t))
		 (if (char-equal (char-after (point)) ?E)
		     (setq count (1+ count))
		   (setq count (1- count))))
	       (buffer-substring-no-properties
          (progn (beginning-of-line) (forward-word 1) (point)) 
          (progn (end-of-line) (point)))))))
      (insert (concat "End" section)))))

(defun coq-Compile ()
  "compiles current buffer"
  (interactive)
  (let* ((n (buffer-name))
	 (l (string-match ".v" n)))
    (compile (concat "make " (substring n 0 l) ".vo"))))

(defun coq-intros ()
  "Insert successive Intros commands with names given by Show Intros.
Based on idea mentioned in Coq reference manual."
  (interactive)
  (let* ((shints  (proof-shell-invisible-cmd-get-result 
                   "Show Intros."))
         (intros  (replace-in-string shints "^\\([^\n]+\\)\n" "intros \\1.\n")))
    (proof-goto-end-of-locked)
    (unless (< (length shints) 2)       ;; empty response is just NL
      (newline)
      (let ((start (point)))
        (insert intros)
        (indent-region start (point) nil)))))

(proof-defshortcut coq-Apply         "Apply "   [(control ?a)])

;(proof-defshortcut coq-begin-Section "Section " [(control ?s)])
(define-key coq-keymap [(control ?i)] 'coq-intros)
(define-key coq-keymap [(control ?s)] 'coq-insert-section)
(define-key coq-keymap [(control ?m)] 'coq-insert-module)
(define-key coq-keymap [(control ?e)] 'coq-end-Section)
(define-key coq-keymap [(control ?o)] 'coq-SearchIsos)
(define-key coq-keymap [(control ?p)] 'coq-Print)
(define-key coq-keymap [(control ?c)] 'coq-Check)
(define-key coq-keymap [(control ?h)] 'coq-PrintHint)
;; da: I've moved this three buffer layout into the main code now,
;; making it default for three bufer mode.  The function 
;; `proof-layout-windows' lays out according to current display
;; mode: you can use this (C-c C-l) to do what binding below did.
;;(define-key coq-keymap [(control f3)] 'coq-three-b)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   Indentation                                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; FIXME mmw: code disabled; Is the new indentation scheme general
;; enough to handle Coq as well?

;;; "Case" is represented by 'c' on the stack, and
;;; "CoInductive" is represented by 'C'.
;(defun coq-stack-to-indent (stack)
;  (cond
;   ((null stack) 0)
;   ((null (car (car stack)))
;    (nth 1 (car stack)))
;   (t (let ((col (save-excursion
;		   (goto-char (nth 1 (car stack)))
;		   (current-column))))
;	(cond
;	 ((eq (car (car stack)) ?c)
;	  (save-excursion (move-to-column (current-indentation))
;			  (cond 
;			   ((eq (char-after (point)) ?|) (+ col 3))
;			   ((proof-looking-at "end") col)
;			   (t (+ col 5)))))	  
;	 ((or (eq (car (car stack)) ?I) (eq (car (car stack)) ?C))
;	  (+ col (if (eq ?| (save-excursion 
;			      (move-to-column (current-indentation))
;			      (char-after (point)))) 2 4)))
;	 (t (1+ col)))))))
;
;(defun coq-parse-indent (c stack)
;  (cond
;   ((eq c ?C)
;    (cond ((proof-looking-at "Case")
;	   (cons (list ?c (point)) stack))
;	  ((proof-looking-at "CoInductive")
;	   (forward-char (length "CoInductive"))
;	   (cons (list c (- (point) (length "CoInductive"))) stack))
;	  (t stack)))
;   ((and (eq c ?e) (proof-looking-at "end") (eq (car (car stack)) ?c))
;    (cdr stack))
;
;   ((and (eq c ?I) (proof-looking-at "Inductive"))
;    (forward-char (length "Inductive"))
;    (cons (list c (- (point) (length "Inductive"))) stack))
;
;   ((and (eq c ?.) (or (eq (car (car stack)) ?I) (eq (car (car stack)) ?C)))
;    (cdr stack))
;
;   (t stack)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    To guess the command line options   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun coq-guess-command-line (filename)
  "Guess the right command line options to compile FILENAME using `make -n'"
  (let ((dir (file-name-directory filename)))
    (if (file-exists-p (concat dir "Makefile"))
	(let* 
	    ((compiled-file (concat (substring 
				     filename 0 
				     (string-match ".v$" filename)) ".vo"))
	     (command (shell-command-to-string 
		       (concat  "cd " dir ";"
				"gmake -n -W " filename " " compiled-file
				"| sed s/coqc/coqtop/"))))
	  (concat 
	   (substring command 0 (string-match " [^ ]*$" command))
	   " -emacs"))
      coq-prog-name)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   Coq shell startup and exit hooks                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun coq-pre-shell-start ()
  (setq proof-prog-name (concat coq-prog-name
                                (if coq-translate-to-v8 " -translate")))
  (setq proof-mode-for-shell    'coq-shell-mode)
  (setq proof-mode-for-goals    'coq-goals-mode)
  (setq proof-mode-for-response 'coq-response-mode)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   Configuring proof and pbp mode and setting up various utilities  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun coq-mode-config ()

  (setq proof-terminal-char ?\.)
  (setq proof-script-command-end-regexp 
        "\\(?:[^.]\\|\\(?:\\.\\.\\)\\)\\.\\(\\s-\\|\\'\\)")
  (setq proof-script-comment-start "(*")
  (setq proof-script-comment-end "*)")
  (setq proof-unnamed-theorem-name "Unnamed_thm") ; Coq's default name

  (setq proof-assistant-home-page coq-www-home-page)

  (setq proof-mode-for-script 'coq-mode)

  (setq proof-guess-command-line 'coq-guess-command-line)

  ;; Commands sent to proof engine
  (setq proof-showproof-command "Show. "
	proof-context-command "Print All. "
	proof-goal-command "Goal %s. "
	proof-save-command "Save %s. "
	proof-find-theorems-command "Search %s. ")
;; FIXME da: Does Coq have a help or about command?
;;	proof-info-command "Help"

;; 3.4: this is no longer used: setting to nil
;; enforces use of find-and-forget always.
  (setq proof-kill-goal-command nil)

  (setq proof-goal-command-p 'coq-goal-command-p
        proof-find-and-forget-fn 'coq-find-and-forget
        pg-topterm-goalhyp-fn 'coq-goal-hyp
        proof-state-preserving-p 'coq-state-preserving-p)

  (setq proof-save-command-regexp coq-save-command-regexp
        proof-really-save-command-p 'coq-save-command-p ;pierre:deals with Proof <term>.
	proof-save-with-hole-regexp coq-save-with-hole-regexp
	proof-goal-with-hole-regexp coq-goal-with-hole-regexp
	proof-nested-undo-regexp coq-state-changing-commands-regexp
  proof-script-imenu-generic-expression coq-generic-expression
  )
  
  (setq	
;indentation is implemented in coq-indent.el
;   proof-indent-enclose-offset  (- proof-indent)
;   proof-indent-enclose-offset 0
;   proof-indent-close-offset 0
;   proof-indent-open-offset 0
   proof-indent-any-regexp      coq-indent-any-regexp
;   proof-indent-inner-regexp    coq-indent-inner-regexp
;   proof-indent-enclose-regexp  coq-indent-enclose-regexp
   proof-indent-open-regexp     coq-indent-open-regexp
   proof-indent-close-regexp    coq-indent-close-regexp
   )

  ;; span menu 
  (setq proof-script-span-context-menu-extensions 'coq-create-span-menu)

  (setq proof-shell-start-silent-cmd "Set Silent. "
        proof-shell-stop-silent-cmd "Unset Silent. ")

  (coq-init-syntax-table)
  (setq comment-quote-nested nil) ;; we can cope with nested comments

  ;; font-lock
  (setq font-lock-keywords coq-font-lock-keywords-1)
  ;;holes
  (holes-mode 1)

  (proof-config-done)

  ;; outline
  (make-local-variable 'outline-regexp)
  (setq outline-regexp coq-outline-regexp)

  (make-local-variable 'outline-heading-end-regexp)
  (setq outline-heading-end-regexp coq-outline-heading-end-regexp)

  ;; tags
  (and (boundp 'tag-table-alist)
       (setq tag-table-alist
             (append '(("\\.v$" . coq-tags)
                       ("coq"  . coq-tags))
                     tag-table-alist)))

  (setq blink-matching-paren-dont-ignore-comments t)
  ;; multiple file handling
  (setq proof-cannot-reopen-processed-files t
        ;; proof-shell-inform-file-retracted-cmd 'coq-retract-file
        proof-shell-require-command-regexp coq-require-command-regexp
        proof-done-advancing-require-function 'coq-process-require-command)

  ;; hooks and callbacks
  (add-hook 'proof-pre-shell-start-hook 'coq-pre-shell-start nil t)
  ;; FIXME: PG 3.5, next one shouldn't be needed but setting is
  ;; now lost in define-derived-mode for some reason.
  (add-hook 'proof-activate-scripting-hook 'proof-cd-sync nil t)
  (add-hook 'proof-deactivate-scripting-hook 'coq-maybe-compile-buffer nil t))
  


(defun coq-shell-mode-config ()
  (setq 
   proof-shell-prompt-pattern coq-shell-prompt-pattern
   proof-shell-cd-cmd coq-shell-cd
   proof-shell-filename-escapes '(("\\\\" . "\\\\") ("\""   . "\\\""))
   proof-shell-abort-goal-regexp coq-shell-abort-goal-regexp
   proof-shell-proof-completed-regexp coq-shell-proof-completed-regexp
   proof-shell-error-regexp coq-error-regexp
   proof-shell-interrupt-regexp coq-interrupt-regexp
   proof-shell-assumption-regexp coq-id
   pg-subterm-first-special-char ?\360
   proof-shell-wakeup-char ?\371 ; done: prompt
   ;; The next three represent path annotation information
   pg-subterm-start-char ?\372 ; not done
   pg-subterm-sep-char ?\373 ; not done
   pg-subterm-end-char ?\374 ; not done
   pg-topterm-char ?\375 ; done
   proof-shell-eager-annotation-start "\376\\|\\[Reinterning"
   proof-shell-eager-annotation-start-length 12
   proof-shell-eager-annotation-end "\377\\|done\\]" ; done
   proof-shell-annotated-prompt-regexp
   (concat proof-shell-prompt-pattern
	   (char-to-string proof-shell-wakeup-char)) ; done
   proof-shell-result-start "\372 Pbp result \373"
   proof-shell-result-end "\372 End Pbp result \373"
   proof-shell-start-goals-regexp "[0-9]+ subgoals?"
   proof-shell-end-goals-regexp proof-shell-annotated-prompt-regexp
   proof-shell-init-cmd  ; (concat
			 coq-shell-init-cmd
	                 ; Coq has no global settings?
			 ; (proof-assistant-settings-cmd))
   proof-shell-restart-cmd coq-shell-restart-cmd
   pg-subterm-anns-use-stack t)
  
  (coq-init-syntax-table)
  (setq font-lock-keywords coq-font-lock-keywords-1)
  (holes-mode 1)
  (proof-shell-config-done))

(defun coq-goals-mode-config ()
  (setq pg-goals-change-goal "Show %s. ")
  (setq pg-goals-error-regexp coq-error-regexp)
  (coq-init-syntax-table)
  (setq font-lock-keywords coq-font-lock-keywords-1)
  (holes-mode 1)
  (proof-goals-config-done))

(defun coq-response-config ()
   (coq-init-syntax-table)
   (setq font-lock-keywords coq-font-lock-keywords-1)
   (holes-mode 1)
   (proof-response-config-done))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Multiple file handling
;;
;; This is an imperfect attempt.  It really needs prover assistance.
;;
;; Experimental support for multiple files was first added 
;; for Coq 6.X based on discussions at TYPES 2000 between DA and PC.
;; Updated and simplified for Coq 8, PG 3.5 (22.04.04) by DA.

;; First note that coq-shell-cd is sent whenever we activate scripting, 
;; it extends the loadpath with the current directory.

;; When scripting is turned off, we compile the file to update the .vo.
(defun coq-maybe-compile-buffer ()
  "If the current buffer is completely processed, maybe compile it.
The attempt is made if `coq-auto-compile-vos' is non-nil.
This is a value for the `proof-deactivate-scripting-hook'."
  (if (and coq-auto-compile-vos
           (proof-locked-region-full-p)
           buffer-file-name)
      (progn
        ;; FIXME: in PG 4, this next step will be done inside
        ;; proof-register-possibly-new-processed-file.
        ;; NB: we might save dependent buffers too!
        (ignore-errors 
          (proof-save-some-buffers (list buffer)))
        ;; Now re-compile.
        ;; Coq users like Make, let's see if we have a Makefile
        ;; here and choose compile.
        (cond
         ((and (proof-try-require 'compile) 
               compile-command 
               (file-exists-p "Makefile"))
          ;; NB: compilation is run in the background
          ;; in this case!
          (let ((compilation-read-command nil))
            (call-interactively 'compile)))
         (coq-compile-file-command
          (message "Compiling %s..." buffer-file-name)
          (shell-command 
           ;; Could be run in the background here if we
           ;; added & to the end of the command.
           (format coq-compile-file-command buffer-file-name)))))))


;; Dependency management 1: when a buffer is retracted, we also
;; need to retract any children buffers.
;; To do that, we run coqdep on each of the processed files, 
;; and see whether the buffer being retracted is an ancestor.

(defun coq-ancestors-of (filename)
  "Return ancestor .v files of RFILENAME.
This is based on the output of coqtop FILENAME.
Currently this doesn't take the loadpath into account."
  ;; FIXME: is there any way to bring in the load path here in coqdep?
  ;; We might use Coq's "Locate File string." command to help.
  (let* ((filedir   (file-name-directory filename))
         (cdline    (shell-command-to-string 
                     (format "coqdep -I %s %s" filedir filename)))
         (matchdeps (string-match ": \\(.*\\)$" cdline))
         (deps      (and matchdeps
                         (split-string (match-string 1 cdline)))))
    (mapcar 
     ;; normalise to include directories, defaulting
     ;; to same dir.  Change .vo -> v
     (lambda (file)
       (concat 
        (if (file-name-directory file) "" filedir)
        (file-name-sans-extension file) ".v"))
     ;; first dep is vacuous: file itself
     (cdr-safe deps))))

;; FIXME: memoise here
(defun coq-all-ancestors-of (filename)
  "Return transitive closure of `coq-ancestors-of'."
  (let ((ancs   (coq-ancestors-of filename))
        all)
    (dolist (anc ancs)
      (setq all (union (cons anc
                             (coq-all-ancestors-of anc))
                       all
                       :test 'string-equal)))
    all))

;; FIXME: add other cases, move to coq-syntax
(defconst coq-require-command-regexp 
  (concat "Require\\s-+\\(" proof-id "\\)")  
  "Regular expression matching Require commands in Coq.
Group number 1 matches the name of the library which is required.")

(defun coq-process-require-command (span cmd)
  "Calculate the ancestors of a loaded file and lock them."
  ;; FIXME todo
  )

(defun coq-included-children (filename)
  "Return a list of children of FILENAME on `proof-included-files-list'."
  (let (children)
    (dolist (incf proof-included-files-list)
      ;; Compute all ancestors transitively: could be expensive
      ;; if we have a lot of included files with many ancestors.
      (let ((ancestors (coq-all-ancestors-of incf)))
        (if (member filename ancestors)
            (setq children (cons incf children)))))
    children))


;; Dependency management 2: when a "Require " is executed,
;; PG should lock all files whose .vo's are loaded.
;; This would be easy if Coq would output some handy
;; messages tracking dependencies in .vo's as it loads
;; those files.   But it doesn't.
;; FIXME: to do this we'll need to watch the
;; Require commands ourselves, and then *lock* their
;; ancestors. TBD.

(defun coq-process-file (rfilename)
  "Adjust the value of `proof-included-files-list' when processing RFILENAME."
  (if coq-auto-compile-vos
      (progn
        (add-to-list proof-included-files-list rfilename)
        ;; recursively call on ancestors: we hope coqdep doesn't give loop!
        (coq-process-file (coq-ancestors-of rfilename)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Flags and other settings for Coq.
;; These appear on the Coq -> Setting menu.
;;

; FIXME da: we should send this command only inside a proof,
; otherwise it gives an error message.  It should be on
; a different menu command.
;(defpacustom print-only-first-subgoal  nil
;  "Whether to just print the first subgoal in Coq."
;  :type 'boolean
;  :setting ("Focus. " . "Unfocus. "))

(defpacustom print-fully-explicit nil
  "*Print fully explicit terms."
  :type 'boolean
  :setting ("Set Printing All. " . "Unset Printing All. "))

(defpacustom print-coercions nil
  "*Print coercions."
  :type 'boolean
  :setting ("Set Printing Coercions. " . "Unset Printing Coercions. "))

(defpacustom print-match-wildcards t
  "*Print underscores for unused variables in matches."
  :type 'boolean
  :setting ("Set Printing Coercions. " . "Unset Printing Coercions. "))


(defpacustom time-commands nil
  "*Whether to display timing information for each command."
  :type 'boolean)

(defpacustom auto-compile-vos nil
  "Whether to automatically compile vos and track dependencies."
  :type 'boolean)

;; old adjustments:
;;  :eval (if coq-auto-compile-vos
;            (setq proof-shell-process-file
;                  coq-proof-shell-process-file
;                  proof-shell-inform-file-retracted-cmd
;                  coq-proof-shell-inform-file-retracted-cmd)
;          (setq proof-shell-inform-file-processed-cmd nil
;                proof-shell-process-file nil
;                proof-shell-inform-file-retracted-cmd nil)))

;; da: what a shame -translate is a command line flag and not a
;; command in Coq. Otherwise we could enable/disable interactively.  
;; As it is, this setting will only work between restarts.
;; Moreover, if we had collaborated on this we could easily have
;; implemented a hook to translate automatically in PG with some
;; extra markup.  Scanning the whitespace as formatted presently 
;; is messy.

(defpacustom translate-to-v8 nil
  "*Whether to use -translate argument to Coq"
  :type 'boolean)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Pre-processing of input string  (PG 3.5)
;;

(defun coq-preprocessing () ;; NB: dynamic scoping of `string'
  (cond
   (coq-time-commands
    (setq string (concat "Time " string)))))
      
(add-hook 'proof-shell-insert-hook 'coq-preprocessing)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Subterm markup -- it was added to Coq by Healf, but got removed.
;;                   Let's try faking something by regexp matching.

;; FIXME: not operational yet
(defun coq-fake-constant-markup ()
  "Markup constants in Coq goal output by matching on regexps.
This is a horrible and approximate way of doing subterm markup.
\(Code used to be in Coq, but got lost between versions 5 and 7).
This is a hook setting for `pg-after-fontify-output-hook' to
enable identifiers to be highlighted and allow useful 
mouse activation."
  (goto-char (point-min))
  (while (re-search-forward "\(\w+[^\w]\)" nil t)
    (replace-match "\372\200\373\\1\374" nil t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Context-senstive in-span menu additions 
;;

;; da: message to Pierre: I just put these in as examples,
;; maybe you can suggest some better commands to use on
;; `thm'.  (Check maybe not much use since appears before
;; in the buffer anyway)

(defun coq-create-span-menu (span idiom name)
  (if (string-equal idiom "proof")
      (let ((thm (span-property span 'name)))
        (list (vector
               "Check"
               `(proof-shell-invisible-command 
                 ,(format "Check %s." thm)))
              (vector
               "Print Proof"
               `(proof-shell-invisible-command 
                 ,(format "Print Proof %s." thm))))))
  (if (string-equal idiom "proof")
      (let ((thm (span-property span 'name)))
        (list (vector
               "Check"
               `(proof-shell-invisible-command 
                 ,(format "Check %s." thm))))))
  )


;load the default coq abbrev file if no coq abbrev file is already read

(if (and (boundp 'coq-mode-abbrev-table)
         (not (equal coq-mode-abbrev-table (make-abbrev-table))))
    (message "An abbrev table exists for coq, no default loaded")
  (progn
    (quietly-read-abbrev-file "coq-abbrev.el")
    (message "coq default abbreviations loaded"))
  )



(provide 'coq)


;;;   Local Variables: ***
;;;   tab-width:2 ***
;;;   fill-column: 85 ***
;;;   indent-tabs-mode:nil ***
;;;   End: ***


