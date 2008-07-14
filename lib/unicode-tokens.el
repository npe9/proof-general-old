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
;; -- allow further composition properties 
;; -- menu for control tokens (generated after init, major-mode specific?)
;; -- rotate tokens (cf old rotate glyphs)
;; -- insert tokens via numeric code (extra format string)
;; -- modify maths menu to filter menu and insert tokens
;; -- reverse lookup that optimistically converts unicode to tokens
;; -- allow searching for tokens? isearch-open-invisible prop

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
Will be regexp quoted.")

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
Used in `unicode-tokens-token-alist', `unicode-tokens-control-regions',
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

(defun unicode-tokens-control-char (s &rest props)
  `(,(format unicode-tokens-control-char-format-regexp s)
    (1 '(face nil invisible unicode-tokens-show-controls) prepend)
    (2 ',(unicode-tokens-symbs-to-props props t) prepend)))

(defun unicode-tokens-control-region (start end &rest props)
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
;; Minor mode
;;

(defvar unicode-tokens-mode-map (make-sparse-keymap)
  "Key map used for Unicode Tokens mode.")

(define-minor-mode unicode-tokens-mode
  "Minor mode for unicode token input." nil " Utoks"
  unicode-tokens-mode-map
  (let ((flks (get 'unicode-tokens-font-lock-keywords major-mode)))
    (when unicode-tokens-mode
      (unless flks
	(setq flks (unicode-tokens-font-lock-keywords))
	(put 'unicode-tokens-font-lock-keywords major-mode flks))
      
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
	  (font-lock-fontify-buffer)))

    (when (not unicode-tokens-mode)
      ;; FIXME: removal doesn't work?.  But does in edebug.
      (when flks
	(font-lock-remove-keywords nil flks)
	(font-lock-fontify-buffer) 
	(setq font-lock-extra-managed-props nil) 
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
	 '(["Show control tokens" unicode-tokens-show-controls
	    :style toggle
	    :selected unicode-tokens-show-controls
	    :active (or
		     unicode-tokens-control-region-format-regexp
		     unicode-tokens-control-char-format-regexp)
	    :help "Prevent hiding of control tokens"])))

;; TODO:
;; 	(cons "Format"
;; 	 (mapcar 
;; 	 (lambda (fmt)
;; 	   (vector fmt
;; 		   (unicode-tokens-annotate-region-with (downcase fmt))
;; 		   :help (concat "Format region as " (downcase fmt))
;; 		   :active 'mark-active)) ; XE? region-active-p
;; 	   '("Subscript" "Superscript" 
;; 	     "Bold" "Italic" "Script" "Fraktur" "Serif")))))
  
(provide 'unicode-tokens)

;;; unicode-tokens.el ends here
