;;; unicode-tokens.el --- Support for control and symbol tokens
;;
;; Copyright(C) 2008 David Aspinall / LFCS Edinburgh
;; Author:    David Aspinall <David.Aspinall@ed.ac.uk>
;; License:     GPL (GNU GENERAL PUBLIC LICENSE)
;;
;; $Id$
;;
;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; This is a replacement for X-Symbol for Proof General.
;;
;; Functions to display tokens that represent Unicode characters and
;; control code sequences for changing the layout.  Tokens are useful
;; for programs that do not understand a Unicode encoding.  
;; 

;; TODO:
;; -- Turning off does not work
;; -- Allow chars
;; -- allow further composition properties 
;; -- menu for control tokens (generated after init, major-mode specific?)
;; -- rotate tokens (cf old rotate glyphs)
;; -- insert tokens via numeric code (extra format string)
;; -- modify maths menu to filter menu and insert tokens
;; -- reverse lookup that optimistically converts unicode to tokens

(require 'cl)

;;
;; Variables that can be overridden in instances: symbol tokens
;;

(defvar unicode-tokens-token-symbol-map nil
  "Mapping of token names to compositions.
Each element is a list

  (TOKNAME COMPOSITION FONTSYMB ...)

A composition is either a single Unicode character string, or
where FONTSYMB is optional.

A composition is either a single Unicode character string, or
a cons cell (FONTSYMB . COMP) where COMP is a composition
and FONTSYMB is a symbol indicating a set of text properties, 
looked up in `unicode-tokens-fontsymb-properties'.")

(defvar unicode-tokens-token-format "%s"
  "Format string for formatting token a name into a token.
Will be regexp quoted for matching.  Not used for matching if
`unicode-tokens-token-variant-format-regexp' is set.
Also used to format shortcuts.")

(defvar unicode-tokens-token-variant-format-regexp nil 
  "A regular expression which matches a token variant.
Will not be regexp quoted, and after format is applied, must

An example would be: \\\\(%s\\\\)\\\\(:?\\w+\\\\)

which matches plain token strings optionally followed by a colon and
variant name.

If set, this variable is used instead of `unicode-tokens-token-format'.")
;; (setq ut-tvfr  "\\(%s\\)\\(:?\\w+\\)")
;; (string-match (format ut-tvfr ".*") "alpha:x") 

(defvar unicode-tokens-fontsymb-properties nil
 "Association list mapping a symbol to a list of text properties.
Used in `unicode-tokens-token-symbol-map', `unicode-tokens-control-regions',
and `unicode-tokens-control-characters'.")

(defvar unicode-tokens-shortcut-alist nil
  "An alist of keyboard shortcuts to unicode strings.
The alist is added to the input mode for tokens.
Behaviour is much like abbrev.")

;;
;; Variables that can be overridden in instances: control tokens
;;

;; TODO: docs
(defvar unicode-tokens-control-region-format-regexp nil)
(defvar unicode-tokens-control-char-format-regexp nil)
(defvar unicode-tokens-control-regions nil)
(defvar unicode-tokens-control-characters nil)


;;
;; Variables set in the mode
;;

(defvar unicode-tokens-alist nil)

(defvar unicode-tokens-composition-token-alist nil)

;;
;; Constants
;;

(defface unicode-tokens-script-font-face
  (cond
   ((eq window-system 'x) ; Linux/Unix
    '((t :family "PakTypeNaqsh")))  ; 
   ((or ; Mac
     (eq window-system 'ns)
     (eq window-system 'carbon))
    '((t :family "Lucida Calligraphy"))))
  "Script font face")

(defface unicode-tokens-fraktur-font-face
  (cond
   ((eq window-system 'x) ; Linux/Unix
    '((t :family "URW Bookman L"))) ;; not at all black letter!
   ((or ; Mac
     (eq window-system 'ns)
     (eq window-system 'carbon))
    '((t :family "Lucida Blackletter"))))
  "Fraktur font face")

(defface unicode-tokens-serif-font-face
  (cond
   ((eq window-system 'x) ; Linux/Unix
    '((t :family "Liberation Serif"))) 
   ((or ; Mac
     (eq window-system 'ns)
     (eq window-system 'carbon))
    '((t :family "Lucida"))))
  "Serif (roman) font face")

(defconst unicode-tokens-font-lock-extra-managed-props 
  '(composition help-echo display invisible)
  "Value for `font-lock-extra-managed-props' here.")

;;
;;; Code:
;;

;; Credit to Stefan Monnier for original version of this.
(defun unicode-tokens-font-lock-keywords ()
  "Calculate and return value for font-lock-keywords."
  (let ((alist nil))
     (dolist (x   unicode-tokens-token-symbol-map)
       (let ((sym  (car x))
	     (comp (cadr x)))
       (when (and (if (fboundp 'char-displayable-p)
 		     (reduce (lambda (x y) (and x (char-displayable-p y)))
 			     comp
 			     :initial-value t)
 		   t)
 		 (not (assoc sym alist))) ; Not yet in alist.
 	(push (cons
 	       (if unicode-tokens-token-variant-format-regexp 
		   sym
		 (format unicode-tokens-token-format sym))
 	       (cdr x))
 	      alist))))
     (when alist
       (setq unicode-tokens-alist alist)
       (if unicode-tokens-token-variant-format-regexp
	   ;; using variant regexps
	   `((,(format unicode-tokens-token-variant-format-regexp
		       (regexp-opt (mapcar 'car alist) t))
	      (0 (unicode-tokens-help-echo) 'prepend)
	      (0 (unicode-tokens-font-lock-compose-symbol 1)
		 'prepend))
	     ,@(unicode-tokens-control-font-lock-keywords))
	 ;; otherwise
	 `((,(regexp-opt (mapcar 'car alist) t)
		  (0 (unicode-tokens-help-echo) 'prepend)
		  (0 (unicode-tokens-font-lock-compose-symbol 0)
		     'prepend))
		 ,@(unicode-tokens-control-font-lock-keywords))))))

(defun unicode-tokens-help-echo ()
  "Return a help-echo text property to display the contents of match string"
    (list 'face nil 'help-echo (match-string 0)))
  
(defun unicode-tokens-font-lock-compose-symbol (match)
  "Compose a sequence of chars into a symbol, maybe returning a face property.
Regexp match data number MATCH selects the token name, while 0 matches the
whole expression. 
Token symbol is searched for in `unicode-tokens-alist'."
  (let* ((start   (match-beginning 0))
         (end     (match-end 0))
	 (compps  (cdr-safe (assoc (match-string match) 
				   unicode-tokens-alist)))
	 (props   (cdr-safe compps)))
    (if compps
	(compose-region start end (car compps)))
    (if (cdr-safe compps)
	(unicode-tokens-symbs-to-props (cdr-safe compps)))))

(defun unicode-tokens-symbs-to-props (symbs &optional facenil)
  (let (props p)
    (dolist (s symbs)
      (setq p (car-safe
	       (cdr-safe (assoc s unicode-tokens-fontsymb-properties))))
      (if (consp p)
	  (setq props (cons (car p) (cons (cadr p) props)))
	(setq props (cons p props))))
    (if (and facenil
	     (not (memq 'face props)))
	(setq props (append '(face nil) props)))
    props))

;; 
;; Control tokens: as "characters" CTRL <stuff>
;;                 and regions     BEGINCTRL <stuff> ENDCTRL
;;

(defvar unicode-tokens-show-controls nil
  "Non-nil supresses hiding of control tokens.")

(defun unicode-tokens-show-controls (&optional arg)
  "Toggle `unicode-tokens-show-controls'.  With ARG, turn on iff positive."
  (interactive "P")
  (setq unicode-tokens-show-controls
	(if (null arg) (not unicode-tokens-show-controls)
	  (> (prefix-numeric-value arg) 0)))
  (when unicode-tokens-show-controls
    (remove-from-invisibility-spec 'unicode-tokens-show-controls))
  (when (not unicode-tokens-show-controls)
    (add-to-invisibility-spec 'unicode-tokens-show-controls)))

(defun unicode-tokens-control-char (name s &rest props)
  `(,(format unicode-tokens-control-char-format-regexp s)
    (1 '(face nil invisible unicode-tokens-show-controls) prepend)
    (2 ',(unicode-tokens-symbs-to-props props t) prepend)))

(defun unicode-tokens-control-region (name start end &rest props)
  `(,(format unicode-tokens-control-region-format-regexp start end)
    (1 '(face nil invisible unicode-tokens-show-controls) prepend)
    (2 ',(unicode-tokens-symbs-to-props props t) prepend)
    (3 '(face nil invisible unicode-tokens-show-controls) prepend)))

(defun unicode-tokens-control-font-lock-keywords ()
  (append
   (mapcar (lambda (args) (apply 'unicode-tokens-control-char args))
 	   unicode-tokens-control-characters)
   (mapcar (lambda (args) (apply 'unicode-tokens-control-region args))
 	   unicode-tokens-control-regions)))

;;
;; Shortcuts for typing, using quail
;;
    
(defvar unicode-tokens-use-shortcuts t
  "Non-nil means use `unicode-tokens-shortcut-alist' if set.")

(defun unicode-tokens-use-shortcuts (&optional arg)
  "Toggle `unicode-tokens-use-shortcuts'.  With ARG, turn on iff positive."
  (interactive "P")
  (setq unicode-tokens-use-shortcuts
	(if (null arg) (not unicode-tokens-use-shortcuts)
	  (> (prefix-numeric-value arg) 0)))
  (if unicode-tokens-use-shortcuts
    (set-input-method "Unicode tokens")
    (set-input-method nil)))

(require 'quail)

(quail-define-package
 "Unicode tokens" "UTF-8" "u" t
 "Unicode characters input method using application specific token names"
 nil t nil nil nil nil nil ; max shortest, could try t
 nil nil nil t)

(defun unicode-tokens-map-ordering (s1 s2)
  "Ordering on (car S1, car S2): order longer strings first."
  (>= (length (car s1)) (length (car s2))))

(defun unicode-tokens-quail-define-rules ()
  "Define the token and shortcut input rules.
Calculated from `unicode-tokens-token-name-alist' and 
`unicode-tokens-shortcut-alist'.
Also sets `unicode-tokens-token-alist'."
  (let ((unicode-tokens-quail-define-rules 
	 (list 'quail-define-rules)))
;;;     (let ((ulist unicode-tokens-shortcut-alist)
;;; 	  ustring tokname token)
;;;       ;; input rules for shortcuts
;;;       (setq ulist (sort ulist 'unicode-tokens-map-ordering))
;;;       (while ulist
;;; 	(setq shortcut (caar ulist))
;;; 	(setq ustring (cdar ulist))
;;; 	;(setq token (format unicode-tokens-token-format tokname))
;;; 	(setq token ustring)
;;; 	(cond 
;;; 	 ;; Some error checking (but not enough)
;;; 	 ((eq (length tokname) 0)
;;; 	  (warn "Empty token name (mapped to \"%s\") in unicode tokens list"
;;; 		ustring))
;;; 	 ((eq (length ustring) 0)
;;; 	  (warn "Empty token mapping, ignoring token \"%s\" in unicode tokens list"
;;; 		token))
;;; 	 ((assoc token unicode-tokens-token-alist)
;;; 	  (warn "Duplicated token entry, ignoring subsequent mapping of %s" token))
;;; 	 ((rassoc ustring unicode-tokens-token-alist)
;;; 	  (warn "Duplicated target \"%s\", ignoring token %s" ustring token))
;;; 	 (t
;;; 	  (nconc unicode-tokens-quail-define-rules
;;; 		 (list (list token 
;;; 			     (vector ustring))))
;;; 	  (setq unicode-tokens-token-alist
;;; 		(nconc unicode-tokens-token-alist
;;; 		       (list (cons token ustring))))))
;;; 	(setq ulist (cdr ulist))))
    ;; make reverse map: convert longer ustring sequences first
    ;; NB: length no longer relevant for compositions?
    (setq unicode-tokens-composition-token-alist
	  (sort
	   (mapcar (lambda (c) (cons (cadr c) (car c))) 
		   unicode-tokens-token-symbol-map)
	   'unicode-tokens-map-ordering))
    ;; Reverse map used where?
    (let ((ulist (copy-list unicode-tokens-shortcut-alist))
	  ustring shortcut)
      (setq ulist (sort ulist 'unicode-tokens-map-ordering))
      (while ulist
	(setq shortcut (caar ulist))
	(setq ustring (cdar ulist))
	(nconc unicode-tokens-quail-define-rules
	       (list (list shortcut
			   (vector ustring))))
	(setq ulist (cdr ulist))))
    (eval unicode-tokens-quail-define-rules)))



;; 
;; Minor mode
;;

(defun unicode-tokens-initialise ()
  (let ((flks (unicode-tokens-font-lock-keywords)))
    (put 'unicode-tokens-font-lock-keywords major-mode flks)
    (unicode-tokens-quail-define-rules)
    flks))

(defvar unicode-tokens-mode-map (make-sparse-keymap)
  "Key map used for Unicode Tokens mode.")

(define-minor-mode unicode-tokens-mode
  "Minor mode for unicode token input." nil " Utoks"
  unicode-tokens-mode-map
  (let ((flks (get 'unicode-tokens-font-lock-keywords major-mode)))
    (when unicode-tokens-mode
      (unless flks
	(setq flks (unicode-tokens-initialise)))
      (make-variable-buffer-local 'font-lock-extra-managed-props)
      (make-variable-buffer-local 'unicode-tokens-alist)
      ;; make sure buffer can display 16 bit chars
      (if (and
	   (fboundp 'set-buffer-multibyte)
	   (not (buffer-base-buffer)))
	  (set-buffer-multibyte t))

      (add-to-invisibility-spec 'unicode-tokens-show-controls)
      (font-lock-add-keywords nil flks)

      (setq font-lock-extra-managed-props nil) 
      (mapcar 
       (lambda (p) (add-to-list 'font-lock-extra-managed-props p))
       unicode-tokens-font-lock-extra-managed-props)

      (if font-lock-fontified ; redo it
	  (font-lock-fontify-buffer))
	
      (if unicode-tokens-use-shortcuts
	  (set-input-method "Unicode tokens")))

    (when (not unicode-tokens-mode)
      ;; FIXME: removal doesn't work?.  But does in edebug.
      (when flks
	(font-lock-remove-keywords nil flks)
	(font-lock-fontify-buffer) 
	(setq font-lock-extra-managed-props nil) 
	(set-input-method nil)
	))))

  ;; Key bindings TODO
;;;    (define-key unicode-tokens-mode-map [(control ?,)]
;;;      'unicode-tokens-rotate-glyph-backward)
;;;    (define-key unicode-tokens-mode-map [(control ?.)]
;;;      'unicode-tokens-rotate-glyph-forward)

;;
;; Menu
;;

(easy-menu-define unicode-tokens-menu unicode-tokens-mode-map
   "Tokens menu"
    (cons "Tokens"
     ;; NB: would be better in menu only if engaged
     (list 
      ["Show control tokens" unicode-tokens-show-controls
       :style toggle
       :selected unicode-tokens-show-controls
       :active (or
		unicode-tokens-control-region-format-regexp
		unicode-tokens-control-char-format-regexp)
       :help "Prevent hiding of control tokens"]
      ["Enable shortcuts" unicode-tokens-use-shortcuts
       :style toggle
       :selected unicode-tokens-use-shortcuts
       :active unicode-tokens-shortcut-alist
       :help "Use short cuts for typing tokens"]
       (cons "Format escape"
 	    (mapcar 
 	     (lambda (fmt)
 	       (vector (car fmt)
 		       `(lambda () (interactive) (insert 
 				   (format unicode-tokens-token-format
 					   ,(cadr fmt))))
 		       :help (concat "Format next item as " 
 				     (downcase (car fmt)))))
 	     unicode-tokens-control-characters))
       (cons "Format region"
 	    (mapcar 
 	     (lambda (fmt)
 	       (vector (car fmt)
 		       `(lambda () (interactive)
 			 (apply 'unicode-tokens-annotate-region-with ,fmt))
 		       :help (concat "Format region as " 
 				     (downcase (car fmt)))
 		       :active 'mark-active))
 	     unicode-tokens-control-regions))
       ["Make fontsets" 
	(lambda () (interactive) (require 'pg-fontsets))
	:active (not (featurep 'pg-fontsets))
	:help "Define fontsets (for Options->Set fontsets)"])))

(defun unicode-tokens-annotate-region-with (name start end &rest props)
  ;; TODO: interactive
  (goto-char (region-end))
  (insert (format unicode-tokens-token-format end))
  (goto-char (region-beginning)
  (insert (format unicode-tokens-token-format start))))

	     
(provide 'unicode-tokens)

;;; unicode-tokens.el ends here
