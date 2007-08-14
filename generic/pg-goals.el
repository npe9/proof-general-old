;; pg-goals.el  Proof General goals buffer mode.
;;
;; Copyright (C) 1994-2002 LFCS Edinburgh. 
;; Authors:   David Aspinall, Yves Bertot, Healfdene Goguen,
;;            Thomas Kleymann and Dilip Sequeira
;; License:   GPL (GNU GENERAL PUBLIC LICENSE)
;;
;; $Id$
;;

;; A sub-module of proof-shell; assumes proof-script loaded.
(require 'pg-assoc)

(require 'bufhist)

;; Nuke some byte compiler warnings.
(eval-when-compile
  (require 'easymenu))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Goals buffer mode
;;

;;
;; The mode itself
;;
(eval-and-compile			; to define proof-goals-mode-map
(define-derived-mode proof-goals-mode proof-universal-keys-only-mode
  proof-general-name 
  "Mode for goals display.  
May enable proof-by-pointing or similar features.
\\{proof-goals-mode-map}"
  (setq proof-buffer-type 'goals)
  ;; font-lock-keywords isn't automatically buffer-local in Emacs 21.2
  (make-local-variable 'font-lock-keywords)
  (make-local-hook 'kill-buffer-hook)
  (add-hook 'kill-buffer-hook 'pg-save-from-death nil t)
  (easy-menu-add proof-goals-mode-menu proof-goals-mode-map)
  (easy-menu-add proof-assistant-menu proof-goals-mode-map)
  (proof-toolbar-setup)
  (erase-buffer)
  (buffer-disable-undo)
  (if proof-keep-response-history (bufhist-mode)) ; history for contents
  (set-buffer-modified-p nil)))

;;
;; Keys for goals buffer
;;
(define-key proof-goals-mode-map [q] 'bury-buffer)
(cond 
(proof-running-on-XEmacs
(define-key proof-goals-mode-map [(button2)] 'pg-goals-button-action)
(define-key proof-goals-mode-map [(control button2)] 'proof-undo-and-delete-last-successful-command)
;; button 2 is a nuisance on 2 button mice, so we'll do 1 as well.
;; Actually we better hadn't, people like to use it for cut and paste.
;; (define-key proof-goals-mode-map [(button1)] 'pg-goals-button-action)
;; (define-key proof-goals-mode-map [(control button1)] 'proof-undo-and-delete-last-successful-command)
;; C Raffalli: The next key on button3 will be remapped to proof by contextual 
;; menu by pg-pbrpm.el. In this case, control button3 is mapped to 
;; 'pg-goals-yank-subterm
(define-key proof-goals-mode-map [(button3)] 'pg-goals-yank-subterm))
(t
(define-key proof-goals-mode-map [mouse-2] 'pg-goals-button-action)
(define-key proof-goals-mode-map [C-mouse-2] 'proof-undo-and-delete-last-successful-command)
;; (define-key proof-goals-mode-map [mouse-1] 'pg-goals-button-action)
;; (define-key proof-goals-mode-map [C-mouse-1] 'proof-undo-and-delete-last-successful-command)
;; C Raffalli: The next key on button3 will be remapped to proof by contextual 
;; menu by pg-pbrpm.el. In this cans, control button3 is mapped to 
;; 'pg-goals-yank-subterm
(define-key proof-goals-mode-map [mouse-3] 'pg-goals-yank-subterm)))


;;
;; Menu for goals buffer
;;
(easy-menu-define proof-goals-mode-menu
		  proof-goals-mode-map
		  "Menu for Proof General goals buffer."
		  proof-aux-menu)


;;
;; The completion of init
;;
(defun proof-goals-config-done ()
  "Initialise the goals buffer after the child has been configured."
  (proof-font-lock-configure-defaults nil)
  (proof-x-symbol-config-output-buffer))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Goals buffer processing
;;
(defun pg-goals-display (string)
  "Display STRING in the proof-goals-buffer, properly marked up.
Converts term substructure markup into mouse-highlighted extents,
and properly fontifies STRING using proof-fontify-region."
  (save-excursion
    ;; Response buffer may be out of date. It may contain (error)
    ;; messages relating to earlier proof states
    
    ;; FIXME da: this isn't always the case.  In Isabelle
    ;; we get <WARNING MESSAGE> <CURRENT GOALS> output,
    ;; or <WARNING MESSAGE> <ORDINARY MESSAGE>.  Both times
    ;; <WARNING MESSAGE> would be relevant.
    ;; We should only clear the output that was displayed from
    ;; the *previous* prompt.
    
    ;; Erase the response buffer if need be, maybe removing the
    ;; window.  Indicate it should be erased before the next output.
    (proof-shell-maybe-erase-response t t)

    ;; Erase the goals buffer and add in the new string
    (set-buffer proof-goals-buffer)

    (unless (eq 0 (buffer-size))
      (bufhist-checkpoint-and-erase))
    ;; Only bother processing and displaying, etc, if string is
    ;; non-empty.
    (unless (string-equal string "")
      (insert string)

      (if pg-use-specials-for-fontify
	  ;; With special chars for fontification, do that first,
	  ;; but keep specials in case also used for subterm markup.
	  (proof-fontify-region (point-min) (point-max) 'keepspecials))
      (pg-goals-analyse-structure (point-min) (point-max))

      (unless pg-use-specials-for-fontify
	;; provers which use ordinary keywords to fontify output must
	;; do fontification second after subterm specials are removed.
	(proof-fontify-region (point-min) (point-max)))
      
      ;; Record a cleaned up version of output string
      (setq proof-shell-last-output 
	    (buffer-substring (point-min) (point-max)))
    
      (set-buffer-modified-p nil)	; nicety
      
      ;; Keep point at the start of the buffer.  
      (proof-display-and-keep-buffer 
       proof-goals-buffer (point-min)))))


(defun pg-goals-analyse-structure (start end)
  "Analyse the region between START and END for subterm and PBP markup.

For subterms, we can make nested regions in the concrete syntax into
active mouse-highlighting regions, each of which can be used to
communicate a selected term back to the prover.  The output text is
marked up with the annotation scheme:

 	 [  <annotation>      |       <subterm/concrete> ] 

         ^                    ^                          ^
	 |                    |                          |
pg-subterm-start-char  pg-subterm-sep-char  pg-subterm-end-char

The annotation is intended to indicate a node in the abstract syntax
tree inside the prover, which can be used to pick out the internal
representation of the term itself.  We assume that the annotation
takes the form of a sequence of characters:

   <length of shared prefix previous>  <ann1> <ann2> .. <annN>

Where each element <..> is a character which is *less* than
pg-subterm-first-special-char, but *greater* than 128.  Each 
character code has 128 subtracted to yield a number.  The first
character allows a simple optimisation by sharing a prefix of
the previous (left-to-right) subterm's annotation.  (See the
variable `pg-subterm-anns-use-stack' for an alternative 
optimisation.)

For subterm markup without communication back to the prover, the
annotation is not needed, but the first character must still be given.

For proof-by-pointing (PBP) oriented markup, `pg-topterm-regexp' and
`pg-topterm-goalhyplit-fn' should be set.  Together these allow
further active regions to be defined, corresponding to \"top elements\"
in the proof-state display.  A \"top element\" is currently assumed
to be either a hypothesis or a subgoal, and is assumed to occupy 
a line (or at least a line).  The markup is simply
  
                 & <goal or hypthesis line in proof state>
                 ^
		 |
   pg-topterm-regexp

And the function `pg-topterm-goalhyplit-fn' is called to do the
further analysis, to return an indication of the goal/hyp and
record a name value.  These values are used to construct PBP
commands which can be sent to the prover."
  (if (or pg-subterm-start-char pg-topterm-regexp) ;; markup for topterm alone
  (let* 
      ((cur start)
       (len (- end start))
       (ann (make-string len ?x)) ; (more than) enough space for longest ann'n
       (ap  0)
       c stack topl span)

    (while (< cur end)
      (setq c (char-after cur))
      (cond
       ;; Seen goal regexp: this is the start of a top extent
       ;; (assumption, goal, literal command)
       ((save-excursion
	  (goto-char cur)
	  (looking-at pg-topterm-regexp))
	(setq topl (cons cur topl))
	(setq ap 0))

       ;; Seen subterm start char: it's followed by a char
       ;; indicating the length of the annotation prefix
       ;; which can be shared with the previous subterm.
       ((and pg-subterm-start-char ;; ignore if subterm start not set
	     (= c pg-subterm-start-char))
	(incf cur)
	(if pg-subterm-anns-use-stack
	    (setq ap (- ap (- (char-after cur) 128)))
	  (setq ap (- (char-after cur) 128)))
	(incf cur)
	;; Now search for a matching end-annotation char, recording the 
	;; annotation found.
	(while (not (= (setq c (char-after cur)) pg-subterm-sep-char))
	  (aset ann ap (- c 128))
	  (incf ap)
	  (incf cur))
	;; Push the buffer pos and annotation
	(setq stack (cons cur 
			  (cons (substring ann 0 ap) stack))))

       ;; Seen a subterm end char, terminating an annotated region
       ;; in the concrete syntax.  We make a highlighting span now.
       ((and (consp stack) (= c pg-subterm-end-char))
	;; (consp stack) added to make the code more robust.
	;; [ Condition violated with lego/example.l and GNU Emacs 20.3 ]
	(setq span (make-span (car stack) cur))
	(set-span-property span 'mouse-face 'highlight)
	(set-span-property span 'goalsave (cadr stack));; FIXME: 'goalsave?
	;; (set-span-property span 'balloon-help helpmsg)
	(set-span-property span 'help-echo 'pg-goals-get-subterm-help)
	(if pg-subterm-anns-use-stack
	    ;; Pop annotation off stack
	    (progn
	      (setq ap 0)
	      (while (< ap (length (cadr stack)))
		(aset ann ap (aref (cadr stack) ap))
		(incf ap))))
	;; Finish popping annotations
	(setq stack (cdr (cdr stack)))))
      ;; On to next char
      (incf cur))
    
    ;; List of topterm beginning positions (goals/hyps) found
    (setq topl (reverse (cons end topl)))
    
    ;; Proof-by-pointing markup assumes "top" elements which define a
    ;; context for a marked-up (sub)term: we assume these contexts to
    ;; be either a subgoal to be solved or a hypothesis.  
    ;; Top element spans are only made if pg-topterm-goalhyplit-fn is set.
    ;; NB: If we want Coq pbp: (setq coq-current-goal 1) 
    (if pg-topterm-goalhyplit-fn
	(while (setq ap (car topl) 
		     topl (cdr topl))
	  (pg-goals-make-top-span ap (car topl))))

    ;; Finally: strip the specials.  This should
    ;; leave the spans in exactly the right place.
    (pg-assoc-strip-subterm-markup-buf start end))))


(defun pg-goals-make-top-span (start end)
  "Make a top span (goal/hyp area) for mouse active output."
  (let (span typname)
    (goto-char start)
    ;; skip the pg-topterm-regexp itself
    (if (looking-at pg-topterm-regexp) 
	(forward-char (- (match-end 0) (match-beginning 0))))
    ;; typname is expected to be a cons-cell of a type (goal/hyp)
    ;; with a name, retrieved from the text immediately following
    ;; the topterm-regexp annotation.
    (let ((topterm (point))) 
      (setq typname (funcall pg-topterm-goalhyplit-fn)) ;; should be non-nil!
      (goto-char topterm))
    (setq start (point))
    (if (eq (car-safe typname) 'lit)
	;; Use length of literal command for end point
	(forward-char (length (cdr typname)))
      ;; Otherwise, use start/end of line before next annotation/buffer end
      (goto-char start)
      (beginning-of-line)
      (setq start (point))
      (goto-char end) ;; next annotation/end of buffer
      (beginning-of-line)
      (backward-char))
    (setq span (make-span start (point)))
    (set-span-property span 'mouse-face 'highlight)
    (set-span-property span 'proof-top-element typname)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Commands to prover based on subterm markup (inc PBP).
;;
;;

;; Fairly specific to the mechanism implemented in LEGO
;; To make (more) sense of this code, you should read the
;; relevant LFCS tech report by tms, yb, and djs

(defun pg-goals-yank-subterm (event)
  "Copy the subterm indicated by the mouse-click EVENT.
This function should be bound to a mouse button in the Proof General
goals buffer.

The EVENT is used to find the smallest subterm around a point.  The
subterm is copied to the kill-ring, and immediately yanked (copied)
into the current buffer at the current cursor position.

In case the current buffer is the goals buffer itself, the yank
is not performed.  Then the subterm can be retrieved later by an
explicit yank."
  (interactive "e")
  (let (span)
    (save-window-excursion
      (save-excursion
	(mouse-set-point event)
	;; Get either the proof body or whole goalsave
	(setq span (or 
		    (span-at (point) 'proof)
		    (span-at (point) 'goalsave)))
	(if span (copy-region-as-kill
		  (span-start span) 
		  (span-end span)))))
    (if (and span (not (eq proof-buffer-type 'goals)))
	(yank))))

(defun pg-goals-button-action (event)
  "Construct a proof-by-pointing command based on the mouse-click EVENT.
This function should be bound to a mouse button in the Proof General
goals buffer.

The EVENT is used to find the smallest subterm around a point.  A
position code for the subterm is sent to the proof assistant, to ask
it to construct an appropriate proof command.  The command which is
constructed will be inserted at the end of the locked region in the
proof script buffer, and immediately sent back to the proof assistant.
If it succeeds, the locked region will be extended to cover the
proof-by-pointing command, just as for any proof command the 
user types by hand."
   (interactive "e")
   (mouse-set-point event)
   (pg-goals-construct-command))

;; Using the spans in a mouse behavior is quite simple: from the mouse
;; position, find the relevant span, then get its annotation and
;; produce a piece of text that will be inserted in the right buffer.

(defun proof-expand-path (string)
  (let ((a 0) (l (length string)) ls)
    (while (< a l) 
      (setq ls (cons (int-to-string 
		      (char-to-int (aref string a)))
		     (cons " " ls)))
      (incf a))
    (apply 'concat (nreverse ls))))

(defun pg-goals-construct-command ()
  ;; Examine the goals 
  (let* ((span (span-at (point) 'goalsave)) ;; goalsave means subgoal no/name
	 (top-span (span-at (point) 'proof-top-element))
	 top-info)
    (if (null top-span) ()
      (setq top-info (span-property top-span 'proof-top-element))
      (pop-to-buffer proof-script-buffer)
      (cond
       (span
	(proof-shell-invisible-command 
	 (format (if (eq 'hyp (car top-info)) pbp-hyp-command 
		                              pbp-goal-command)
		 (concat (cdr top-info) (proof-expand-path 
					 (span-property span 'goalsave))))))
       ((eq (car top-info) 'hyp)
	;; Switch focus to another subgoal; a non-scripting command
	(proof-shell-invisible-command
	 (format pbp-hyp-command (cdr top-info))))
       ((eq (car top-info) 'goal)
	;; A scripting command to change goal
	 (proof-insert-pbp-command
	  (format pg-goals-change-goal (cdr top-info))))
       ((eq (car top-info) 'lit)
	;; A literal command to insert
	(proof-insert-pbp-command (cdr top-info)))))))



(defun pg-goals-get-subterm-help (spanorwin &optional obj pos)
  "Return a help string for subterm, called via 'help-echo property."
  (let ((span  (or obj spanorwin))) ;; GNU Emacs vs XEmacs interface
    (if (and pg-subterm-help-cmd (span-live-p span))
	(or (span-property span 'cachedhelp) ;; already got
	    (progn
	      (if (proof-shell-available-p)
		  (let ((result 
			 (proof-shell-invisible-cmd-get-result 
			  (format pg-subterm-help-cmd (span-string span))
			  'ignorerrors)))
		    ;; FIXME: generalise, and make output readable
		    ;; (fontify?  does that work for GNU Emacs?
		    ;;  how can we do it away from a buffer?)
		    (setq result
			  (replace-in-string 
			   result 
			   (concat "\n\\|" pg-special-char-regexp) ""))		    
		    (set-span-property span 'cachedhelp result)
		    result)))))))



(provide 'pg-goals)
;; pg-goals.el ends here.
