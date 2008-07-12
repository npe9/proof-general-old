;;; NOTE: this is a temporary file, for testing a new variant of the unicode 
;; tokens, based on font-lock.   It has the advantage that the buffer contents
;; is not altered, but the disadvantage that cut-and-paste do not work.

;;; unicode-tokens2.el --- Support for editing tokens for Unicode characters
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
;; This is a partial replacement for X-Symbol for Proof General.
;; STATUS: experimental.  
;;
;; Functions to help insert tokens that represent Unicode characters
;; and control code sequences for changing the layout.  Tokens are
;; useful for programs that do not understand a Unicode encoding.
;; 

;; TODO:
;; -- Turning off does not work, composition properties 

(require 'cl)

(require 'unicode-chars)		; list of Unicode characters

;;
;; Variables that should be set
;;

(defvar unicode-tokens2-token-alist nil
  "Mapping of token names to compositions.
A composition is either a single Unicode character string, or
a cons cell (FONTSYMB . COMP) where COMP is a composition
and FONTSYMB is a symbol indicating a set of text properties, 
looked up in `unicode-tokens2-fontsymb-properties'.")

(defvar unicode-tokens2-token-format "%s"
  "Format string for formatting token names into token.")

(defvar unicode-tokens2-fontsymb-properties nil
 "Association list mapping symbols to text properties.
See `unicode-tokens2-token-alist'.")

(defvar unicode-tokens2-font-lock-extra-managed-props nil
  "Extra values for `font-lock-keywords' to add along with tokens.")

(defvar unicode-tokens2-extra-font-lock-keywords nil
  "Extra values for `font-lock-keywords' to add along with tokens.")

(defvar unicode-tokens2-shortcut-alist nil
  "An alist of keyboard shortcuts to unicode strings.
The alist is added to the input mode for tokens.
Behaviour is much like abbrev.")

;;
;; Variables initialised in unicode-tokens2-initialise 
;;

(defvar unicode-tokens2-font-lock-keywords nil)

(defvar unicode-tokens2-filtered-token-alist nil)

;;
;;; Code:
;;

(defun unicode-tokens2-set-font-lock-keywords ()
  "Calculate value for `unicode-tokens2-font-lock-keywords'."
  ;; First, symbols.  Credit Stefan Monnier
  (let ((alist nil))
    (dolist (x   unicode-tokens2-token-alist)
      (when (and (if (fboundp 'char-displayable-p)
		     (reduce (lambda (x y) (and x (char-displayable-p y)))
			     (if (stringp (cdr x)) (cdr x) (cddr x))
			     :initial-value t)
		   t)
		 (not (assoc (car x) alist))) ; Not yet in alist.
	(push (cons
	       (format unicode-tokens2-token-format (car x))
	       (cdr x))
	      alist)))
    (when alist
      (setq unicode-tokens2-filtered-token-alist alist)
      (setq unicode-tokens2-font-lock-keywords
	    `((,(regexp-opt (mapcar 'car alist) t)
	       (0 (unicode-tokens2-font-lock-compose-symbol)
		  'prepend))
	      ,@unicode-tokens2-extra-font-lock-keywords)))))

(defun unicode-tokens2-font-lock-compose-symbol ()
  "Compose a sequence of chars into a symbol, maybe returning a face property.
Regexp match data 0 points to the chars.
Token symbol is searched for in `unicode-tokens2-filtered-token-alist'."
  (let* ((start (match-beginning 0))
         (end   (match-end 0))
	 (comp  (cdr-safe (assoc (match-string 0) 
				 unicode-tokens2-filtered-token-alist))))
    (cond
     ((stringp comp)
      (compose-region start end comp)
      nil)
     ((consp comp)
      (compose-region start end (cdr comp))
      (assoc (car comp)
	     unicode-tokens2-fontsymb-properties)))))
    
;; 
;; Minor mode
;;

(defvar unicode-tokens2-mode-map (make-sparse-keymap)
  "Key map used for Unicode Tokens mode.")

(define-minor-mode unicode-tokens2-mode
  "Minor mode for unicode token input." nil " Utoks2"
  unicode-tokens2-mode-map
  (unless unicode-tokens2-font-lock-keywords
    (unicode-tokens2-initialise))
  (make-variable-buffer-local 'font-lock-extra-managed-props)
  (when unicode-tokens2-mode
    ;; make sure buffer can display 16 bit chars
    (if (and
	 (fboundp 'set-buffer-multibyte)
	 (not (buffer-base-buffer)))
	(set-buffer-multibyte t)))
  (when unicode-tokens2-mode
    (setq old-proof-font-lock-keywords proof-font-lock-keywords)
    (setq proof-font-lock-keywords 
	  (append unicode-tokens2-font-lock-keywords
		  proof-font-lock-keywords))
    (font-lock-add-keywords nil
			    unicode-tokens2-font-lock-keywords)
    (font-lock-fontify-buffer)
    ;; NB! assumes this was nil on entry
    (setq font-lock-extra-managed-props '(composition))
    (mapcar 
     (lambda (p) (add-to-list 'font-lock-extra-managed-props p))
     unicode-tokens2-font-lock-extra-managed-props))
  (when (not unicode-tokens2-mode)
    ;; FIXME: removal doesn't work?
    (font-lock-remove-keywords nil
			       unicode-tokens2-font-lock-keywords)
    (setq proof-font-lock-keywords 
	  old-proof-font-lock-keywords)
    ;; NB! assumes this was nil on entry
    (font-lock-fontify-buffer)
    (setq font-lock-extra-managed-props nil)))

(defun unicode-tokens2-initialise ()
  "Initialise mode."
  (unicode-tokens2-set-font-lock-keywords))

  ;; Key bindings
;;;    (define-key unicode-tokens2-mode-map [(control ?,)]
;;;      'unicode-tokens2-rotate-glyph-backward)
;;;    (define-key unicode-tokens2-mode-map [(control ?.)]
;;;      'unicode-tokens2-rotate-glyph-forward)

;;
;; Menu
;;

;; (easy-menu-define unicode-tokens-menu unicode-tokens-mode-map
;;   "Tokens menu"
;;   (cons "Tokens"
;; 	(cons "Format"
;; 	 (mapcar 
;; 	 (lambda (fmt)
;; 	   (vector fmt
;; 		   (unicode-tokens2-annotate-region-with (downcase fmt))
;; 		   :help (concat "Format region as " (downcase fmt))
;; 		   :active 'mark-active)) ; XE? region-exists-p
;; 	   '("Subscript" "Superscript" 
;; 	     "Bold" "Italic" "Script" "Fraktur" "Serif")))))
  
(provide 'unicode-tokens2)

;;; unicode-tokens2.el ends here
