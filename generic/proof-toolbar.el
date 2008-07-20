;;; proof-toolbar.el --- Toolbar for Proof General
;;
;; Copyright (C) 1998-2008  David Aspinall / LFCS.
;; Author:    David Aspinall <David.Aspinall@ed.ac.uk>
;; License:   GPL (GNU GENERAL PUBLIC LICENSE)
;;
;; $Id$
;;
;;
;; 1. edit-toolbar cannot edit proof toolbar (even in a proof mode)
;; Need a variable containing  a specifier or similar.
;; (defvar proof-toolbar-specifier nil
;;   "Specifier for proof toolbar.")
;; This doesn't seem worth fixing until XEmacs toolbar implementation
;; settles a bit.  Enablers don't work too well at the moment.

;; 2. It's a little bit tricky to add prover-specific items:
;; presently it must be done before this file is loaded.
;; We could improve on that by generating everything on-thy-fly
;; in proof-toolbar-setup.

;; 3. We could consider automatically disabling buttons which are
;; not configured for the prover, e.g. if proof-info-command is
;; not set, then the Info button should not be shown.
;;
;; See `proof-toolbar-entries-default' and `<PA>-toolbar-entries'
;; in pg-custom for the default generic toolbar and
;; the per-prover toolbar contents variable.
;;

;;; Code:
(eval-when-compile
  (require 'proof-utils))


;;; Commentary:
;; 

(require 'span)
(require 'proof-config)
(require 'proof-autoloads)

;;
;; Function, icon, button names
;; 

(defun proof-toolbar-function (token)
  (intern (concat "proof-toolbar-" (symbol-name token))))

(defun proof-toolbar-icon (token)
  (intern (concat "proof-toolbar-" (symbol-name token) "-icon")))

(defun proof-toolbar-enabler (token)
  (intern (concat "proof-toolbar-" (symbol-name token) "-enable-p")))

(defun proof-toolbar-function-with-enabler (token)
  (intern (concat "proof-toolbar-" (symbol-name token) "-with-enabler-p")))

;;
;; Now the toolbar icons and buttons
;; 

(defun proof-toolbar-make-icon (tle)
  "Make icon variable and icon list entry from a PA-toolbar-entries entry."
  (let* ((icon     (car tle))
	 (tooltip  (nth 2 tle))
	 (iconname (symbol-name icon))
	 (iconvar  (proof-toolbar-icon icon)))
    ;; first declare variable
    ;;  (eval
    ;;  `(defvar ,iconvar nil
    ;;  ,(concat
    ;;   "Glyph list for " iconname " button in Proof General toolbar.")))
    ;; FIXME: above doesn't quite work right.  However, we only lose
    ;; the docstring which is no big deal.
    ;; now the list entry
    (if tooltip
	(list (list iconvar iconname)))))
  

(defun proof-toolbar-make-toolbar-item (tle)
  "Make a toolbar button descriptor from a PA-toolbar-entries entry."
  (let*
      ((token	      (nth 0 tle))
       (includep      (or (< (length tle) 5) (eval (nth 4 tle))))
       (menuname      (and includep (nth 1 tle)))
       (tooltip       (and includep (nth 2 tle)))
       (existsenabler (nth 3 tle))
       (enablep	      (and proof-toolbar-use-button-enablers
			   (>= emacs-major-version 21)
			   existsenabler))
       (enabler	      (proof-toolbar-enabler token))
       (enableritem   (if enablep (list enabler) t))
       (buttonfn      (proof-toolbar-function token))
       (buttonfnwe    (proof-toolbar-function-with-enabler token))
       (icon	      (proof-toolbar-icon token))
       (actualfn
	(if (or enablep (not existsenabler))
	    buttonfn
	  ;; Add the enabler onto the function if necessary.
	  (eval `(defun ,buttonfnwe ()
		   (interactive)
		   (if (,enabler)
		       (call-interactively (quote ,buttonfn))
		     (message "Button \"%s\" disabled" ,menuname))))
	  buttonfnwe)))
    (if tooltip	;; no tooltip means menu-only item
	  (list (append (list icon actualfn token
			      :help tooltip)
			(if enabler (list :enable (list enabler))))))))
		


;;
;; Code for displaying and refreshing toolbar
;;

(defvar proof-toolbar nil
  "Proof mode toolbar button list.  Set in `proof-toolbar-build'.
For GNU Emacs, this holds a keymap.")

(deflocal proof-toolbar-itimer nil
  "itimer for updating the toolbar in the current buffer")

;;;###autoload
(defun proof-toolbar-setup ()
  "Initialize Proof General toolbar and enable it for current buffer.
If `proof-toolbar-enable' is nil, change the current buffer toolbar
to the default toolbar."
  (interactive)
  (if
   (and ;; Check toolbar support...
    window-system
    (or (and (featurep 'tool-bar)	; GNU Emacs tool-bar library
	     (or (image-type-available-p 'xpm)   ;; and XPM
		 (image-type-available-p 'png))) ;; or PNG
	(and (featurep 'toolbar)	; or XEmacs toolbar library
	     (featurep 'xpm))))		; and XPM support

      ;; Toolbar support is possible.
      (progn
	;; Check the toolbar has been built.
	(or proof-toolbar (proof-toolbar-build))

	;; Now see if user wants toolbar (this can be changed dyamically).
	(when proof-toolbar-enable
	    ;; Enable the toolbar in this buffer: make a local tool-bar-map
	    (set (make-local-variable 'tool-bar-map) proof-toolbar))
	
	(when (not proof-toolbar-enable)
	  (kill-local-variable 'tool-bar-map)) ;; remove local value

	;; Update the display
	(sit-for 0))))

(defun proof-toolbar-build ()
  "Build `proof-toolbar'."
  (let ((icontype ".xpm")	
	;; List is (VAR IMAGE). Toolbar code expects to find files IMAGE.xpm
	(proof-toolbar-icon-list
	 (apply 'append
		(mapcar 'proof-toolbar-make-icon
			(proof-ass toolbar-entries))))

	;; A toolbar descriptor evaluated in proof-toolbar-setup.
	;; Specifically, a list of sexps which evaluate to entries in
	;; a toolbar descriptor.  The default
	;; `proof-toolbar-default-button-list' works for any prover.
	(proof-toolbar-button-list
	 (apply 'append (mapcar 'proof-toolbar-make-toolbar-item
				(proof-ass toolbar-entries)))))

    ;; First set the button variables to glyphs (bit long-windedly).
    (mapcar
     (lambda (buttons)
       (set (car buttons)
	    (concat "epg-" (eval (cadr buttons)))))
     proof-toolbar-icon-list)

    (setq proof-toolbar (make-sparse-keymap))
    (add-to-list 'image-load-path proof-images-directory) ; rude?
    (dolist (but proof-toolbar-button-list)
      (apply
	 'tool-bar-local-item
	 (eval (nth 0 but))		; image filename
	 (nth 1 but)			; function symbol
	 (nth 2 but)			; dummy key
	 proof-toolbar
	 (nthcdr 3 but)))))		; remaining properties

;; Action to take after altering proof-toolbar-enable
(defalias 'proof-toolbar-enable 'proof-toolbar-setup)

;;;###autoload (autoload 'proof-toolbar-toggle "proof-toolbar")
(proof-deftoggle proof-toolbar-enable proof-toolbar-toggle)

(defvar proof-toolbar-enablers
   (mapcar (lambda (tle)
	     (list (proof-toolbar-enabler (car tle))))
	   (proof-ass toolbar-entries))
  "List of all toolbar's enablers")

(defvar proof-toolbar-enablers-last-state
  nil
  "Last state of the toolbar's enablers")


;;
;; =================================================================
;;
;;
;; GENERIC PROOF TOOLBAR BUTTON FUNCTIONS
;;
;; Defaults functions are provided below for: up, down, restart
;; Code for specific provers may define the symbols below to use
;; the other buttons: next, prev, goal, qed (images are provided).
;;
;;  proof-toolbar-next		   next function
;;  proof-toolbar-next-enable      enable predicate for next (or t)
;;
;; etc.
;;
;; To add support for more buttons or alter the default
;; images, <PA>-toolbar-entries should be adjusted.
;; See proof-config.el for that.
;;
;; Note that since the toolbar is displayed for goals and response
;; buffers too, enablers and command functions must potentially
;; switch buffer first.
;;
;;


;;
;; Undo button
;;

(defun proof-toolbar-undo-enable-p ()
  (proof-with-script-buffer
   (and (proof-shell-available-p)
	(> (proof-unprocessed-begin) (point-min)))))

(defalias 'proof-toolbar-undo 'proof-undo-last-successful-command)

;;
;; Delete button  (not actually on toolbar)
;;

(defun proof-toolbar-delete-enable-p ()
  (proof-with-script-buffer
   (and (not buffer-read-only)
	(proof-shell-available-p)
	(> (proof-unprocessed-begin) (point-min)))))

(defalias 'proof-toolbar-delete 'proof-undo-and-delete-last-successful-command)


;;
;; Lockedend button  (not actually on toolbar)
;;

(defun proof-toolbar-lockedend-enable-p ()
  t)

(defalias 'proof-toolbar-lockedend 'proof-goto-end-of-locked)




;;
;; Next button
;;

(defun proof-toolbar-next-enable-p ()
  (proof-with-script-buffer
   (not (proof-locked-region-full-p))))

(defalias 'proof-toolbar-next 'proof-assert-next-command-interactive)


;;
;; Goto button
;;

(defun proof-toolbar-goto-enable-p ()
  (eq proof-buffer-type 'script))

(defalias 'proof-toolbar-goto 'proof-goto-point)


;;
;; Retract button
;;

(defun proof-toolbar-retract-enable-p ()
  (proof-with-script-buffer
   (not (proof-locked-region-empty-p))))

(defalias 'proof-toolbar-retract 'proof-retract-buffer)


;;
;; Use button
;;

(defalias 'proof-toolbar-use-enable-p 'proof-toolbar-next-enable-p)
(defalias 'proof-toolbar-use 'proof-process-buffer)

;;
;; Restart button
;;

(defun proof-toolbar-restart-enable-p ()
  ;; Could disable this unless there's something running.
  ;; But it's handy to clearup extents, etc, I suppose.
  t)

(defalias 'proof-toolbar-restart 'proof-shell-restart)

;;
;; Goal button
;;

(defun proof-toolbar-goal-enable-p ()
  ;; Goals are always allowed, provided proof-goal-command is set.
  ;; Will crank up process if need be.
  ;; (Actually this should only be available when "no subgoals")
  proof-goal-command)


(defalias 'proof-toolbar-goal 'proof-issue-goal)


;;
;; QED button
;;

(defun proof-toolbar-qed-enable-p ()
  (proof-with-script-buffer
   (and proof-save-command
	proof-shell-proof-completed
	(proof-shell-available-p))))

(defalias 'proof-toolbar-qed 'proof-issue-save)

;;
;; State button
;;

(defun proof-toolbar-state-enable-p ()
  (proof-shell-available-p))
  
(defalias 'proof-toolbar-state 'proof-prf)

;;
;; Context button
;;

(defun proof-toolbar-context-enable-p ()
  (proof-shell-available-p))
  
(defalias 'proof-toolbar-context 'proof-ctxt)

;;
;; Info button
;;
;; Might as well enable it all the time; convenient trick to
;; start the proof assistant.

(defun proof-toolbar-info-enable-p ()
  t)

(defalias 'proof-toolbar-info 'proof-help)

;;
;; Command button
;;

(defun proof-toolbar-command-enable-p ()
  (proof-shell-available-p))

(defalias 'proof-toolbar-command 'proof-minibuffer-cmd)

;;
;; Help button
;;
 
(defun proof-toolbar-help-enable-p ()
  t)

(defun proof-toolbar-help ()
  (interactive)
  (info "ProofGeneral"))

;;
;; Find button
;;
 
(defun proof-toolbar-find-enable-p ()
  (proof-shell-available-p))

(defalias 'proof-toolbar-find 'proof-find-theorems)

;;
;; Visibility button (not on toolbar)
;;

(defun proof-toolbar-visibility-enable-p ()
  (span-property-safe (span-at (point) 'type) 'idiom))

(defalias 'proof-toolbar-visibility 'pg-toggle-visibility)

;;
;; Interrupt button
;; 

(defun proof-toolbar-interrupt-enable-p ()
  proof-shell-busy)

(defalias 'proof-toolbar-interrupt 'proof-interrupt-process)


;;
;; =================================================================
;;
;; Scripting menu built from toolbar functions
;;

(defun proof-toolbar-make-menu-item (tle)
  "Make a menu item from a PA-toolbar-entries entry."
  (let*
      ((token	  (car tle))
       (menuname  (cadr tle))
       (tooltip   (nth 2 tle))
       (enablep	  (nth 3 tle))
       (fnname	  (proof-toolbar-function token))
       ;; fnval: remove defalias to get keybinding onto menu; 
       ;; NB: function and alias must both be defined for this
       ;; to work!!
       (fnval	  (if (symbolp (symbol-function fnname))
		      (symbol-function fnname)
		    fnname)))
    (if menuname
	(list
	 (apply 'vector
	   (append
	    (list menuname fnval)
	    (if enablep
		(list ':active (list (proof-toolbar-enabler token))))))))))

;;;###autoload
(defun proof-toolbar-scripting-menu ()
  "Menu made from the Proof General toolbar commands."
  ;; Prevent evaluation too early here, otherwise this is called
  ;; during compilation when loading files with expanded easy-menu-define
  ;; (e.g. pg-response/proof-aux-menu call)
  (apply 'append
	 (mapcar 'proof-toolbar-make-menu-item
		 (proof-ass toolbar-entries))))

 
(provide 'proof-toolbar)

;;; proof-toolbar.el ends here
