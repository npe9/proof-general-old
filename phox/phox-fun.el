;; $State$ $Date$ $Revision$ 
;; syntax

(setq
 phox-forget-id-command "del %s.\n"
 phox-forget-proof-command "del_proof %s.\n"
 phox-forget-new-elim-command "edel elim %s.\n"
 phox-forget-new-intro-command "edel intro %s.\n"
 phox-forget-new-rewrite-command "edel rewrite %s.\n"
 phox-forget-close-def-command "edel closed %s.\n"
; phox-comments-regexp : a sequence of comments and white spaces
 phox-comments-regexp "[ \n\t\r]*\\((\\*\\([^*]\\|\\(\\*[^)]\\)\\)*\\*)[ \n\t\r]*\\)*"
; phox-strict-comments-regexp : a not empty sequence of comments and white spaces
 phox-strict-comments-regexp "\\([ \n\t\r]+\\((\\*\\([^*]\\|\\(\\*[^)]\\)\\)*\\*)[ \n\t\r]*\\)*\\|\\((\\*\\([^*]\\|\\(\\*[^)]\\)\\)*\\*)[ \n\t\r]*\\)+\\)"
 phox-ident-regexp "\\(\\([^() \n\t\r=\\[.]\\|\\(\\.[^() \n\t\r]\\)\\)+\\)"
 phox-inductive-option "\\(\\[[^]]*]\\)?"
 phox-spaces-regexp "[ \n\t\r]*"
 phox-sy-definition-regexp (concat 
   "\\(Cst\\|def\\)"
   phox-comments-regexp
   "\\(\\(rInfix\\|lInfix\\|Infix\\|Prefix\\|Postfix\\)[^\"]+\"\\([^\"]+\\)\\)") 
 phox-sy-inductive-regexp (concat 
   "Inductive"
   phox-comments-regexp
   phox-inductive-option
   phox-comments-regexp
   "\\(\\(rInfix\\|lInfix\\|Infix\\|Prefix\\|Postfix\\)[^\"]+\"\\([^\"]+\\)\\)") 
 phox-inductive-regexp (concat 
   "Inductive"
   phox-comments-regexp
   phox-inductive-option
   phox-comments-regexp
   phox-ident-regexp) 
 phox-data-regexp (concat 
   "Data"
   phox-comments-regexp
   phox-inductive-option
   phox-comments-regexp
   phox-ident-regexp) 
 phox-definition-regexp (concat
   "\\(Cst\\|def\\(_thlist\\)?\\|claim\\|Sort\\)"
   phox-comments-regexp
   phox-ident-regexp)
 phox-prove-claim-regexp (concat
   "prove_claim"
   phox-comments-regexp
   phox-ident-regexp)
 phox-new-elim-regexp (concat
   "new_elim\\([^.]\\|\\(\\.[^ \n\t\r]\\)\\)*[ \n\t\r)]"
   phox-ident-regexp)
 phox-new-intro-regexp (concat
   "new_intro\\([^.]\\|\\(\\.[^ \n\t\r]\\)\\)*[ \n\t\r)]"
   phox-ident-regexp)
 phox-new-rewrite-regexp (concat
   "new_rewrite\\([^.]\\|\\(\\.[^ \n\t\r]\\)\\)*[ \n\t\r)]"
   phox-ident-regexp)
 phox-close-def-regexp (concat
   "close_def"
   phox-comments-regexp
   "\\(\\([^.]\\|\\(\\.[^ \n\t\r]\\)\\)+\\)[. \n\t\r]")
)


(defun phox-init-syntax-table ()
  "Set appropriate values for syntax table in current buffer."
;; useful for using forward-word
  (modify-syntax-entry ?_  "w")
  (modify-syntax-entry ?\. "w")
;; Configure syntax table for block comments
  (modify-syntax-entry ?\* ". 23")
  (modify-syntax-entry ?\( "()1")
  (modify-syntax-entry ?\) ")(4")
;; � compl�ter �ventuellement
)


;; completions :

(defvar phox-top-keywords
'(
"goal"
"restart"
"quit"
"theorem"
"claim"
"cst"
"Cst"
"def"
"def_thlist"
"del"
"del_proof"
"Sort"
"close_def"
"edel"
"new_elim"
"new_intro"
"new_rewrite"
"Data"
"Inductive"
"add_path"
"Import"
"include"
"Use"
"tex_syntax"
"depend"
"eshow"
"flag"
"path"
"print"
"print_sort"
"priority"
"prove_claim"
"search"
"compile"
"tdef"
"eval"
"output"
"compile"
"compute"
"Local"
)
"Names of phox top commands."
)

(defvar phox-proof-keywords
'(
"axiom"
"elim"
"intro"
"intros"
"apply"
"by_absurd"
"from"
"left"
"lefts"
"prove"
"use"
"auto"
"trivial"
"rewrite"
"rewrite_hyp"
"unfold"
"unfold_hyp"
"constraints"
"instance"
"lock"
"unlock"
"goals"
"after"
"next"
"select"
"local"
"rename"
"rmh"
"slh"
"abort"
"save"
"undo"
"Try"
)
"Name of phox proof commands."
)



(defun phox-find-and-forget (span)
  (let (str ans tmp (lsp -1))
    (while span 
      (setq str (span-property span 'cmd))

      (cond

       ((eq (span-property span 'type) 'comment))       

       ((eq (span-property span 'type) 'proof)
	(if (proof-string-match phox-prove-claim-regexp str)
	    (setq ans (concat (format phox-forget-proof-command 
				      (match-string 4 str)) ans))
	  (setq ans (concat (format phox-forget-id-command
				    (span-property span 'name)) ans))))

       ((proof-string-match phox-new-elim-regexp str)
	(setq ans 
	      (concat (format phox-forget-new-elim-command 
				  (match-string 3 str)) ans)))

       ((proof-string-match phox-new-intro-regexp str)
	(setq ans 
	      (concat (format phox-forget-new-intro-command 
				  (match-string 3 str)) ans)))

       ((proof-string-match phox-new-rewrite-regexp str)
	(setq ans 
	      (concat (format phox-forget-new-rewrite-command 
				  (match-string 3 str)) ans)))

       ((proof-string-match phox-close-def-regexp str)
	(setq ans 
	      (concat (format phox-forget-close-def-command 
				  (match-string 4 str)) ans)))

       ((proof-string-match phox-sy-definition-regexp str)
	(setq ans 
	      (concat (format phox-forget-id-command 
				  (concat "$" (match-string 7 str))) ans)))

       ((proof-string-match phox-sy-inductive-regexp str)
	(setq ans 
	      (concat (format phox-forget-id-command 
				  (concat "$" (match-string 10 str))) ans)))

       ((proof-string-match phox-inductive-regexp str)
	(setq ans 
	      (concat (format phox-forget-id-command 
				  (match-string 8 str)) ans)))

       ((proof-string-match phox-data-regexp str)
	(setq
	  name (match-string 8 str)
          sname (concat (downcase (substring name 0 1)) 
			(substring name 1 nil))
	  ans (concat (format phox-forget-id-command 
			      sname) ans)))

       ((proof-string-match phox-definition-regexp str)
	(setq ans (concat (format phox-forget-id-command 
				      (match-string 6 str)) ans))))

      (setq lsp (span-start span))
      (setq span (next-span span 'type)))

      (or ans proof-no-command)))

;;
;; Doing commands
;;

(defun phox-assert-next-command-interactive ()
  "Process until the end of the next unprocessed command after point.
If inside a comment, just process until the start of the comment."
  (interactive)
  (message "test")
  (if (and (> (point) 1) (char-equal (char-before (point)) ?\.)) (insert "\n"))
  (proof-with-script-buffer
   (proof-maybe-save-point
    (goto-char (proof-queue-or-locked-end))
    (proof-assert-next-command))
   (proof-maybe-follow-locked-end)))

;;--------------------------------------------------------------------------;;
;; Obtaining some informations on the system.
;;
;;--------------------------------------------------------------------------;;
;; All the commands in section "Obtaining some informations on the
;; system."  (see cmd_top.tex) are associated to a
;; function, and appear in the submenu  "State" [pr].

(defun phox-depend-theorem(theorem)
  "Interactive function : 
ask for a string and  and send a depend command to PhoX for it.

Gives the list of all axioms which have been used to prove the theorem."

(interactive "stheorem: ")
(proof-shell-invisible-command (concat "depend " theorem)))

(defun phox-eshow-extlist(extlist)
  "Interactive function : 
ask for a string and send an eshow command to PhoX for it. 

Shows the given extension-list.  Possible extension lists are : rewrite
(the list of rewriting rules introduced by the new_rewrite command),
elim, intro, (the introduction and elimination rules introduced by the
new_elim and new_intro {-t} commands), closed (closed definitions
introduced by the close_def command) and tex (introduced by the
tex_syntax command)."

(interactive "sextension list: ")
(proof-shell-invisible-command (concat "eshow " extlist)))

(defun phox-flag-name(name)
"Interactive function : 
ask for a string and send a flag command  to PhoX for it.

  Print the value of an internal flag of the
  system. The different flags are listed in the doc, see flag."

(interactive "sname: ")
(proof-shell-invisible-command (concat "flag " name)))


(defun phox-path()
"Interactive function : 
 send a path command to PhoX.

  Prints the list of all paths. This path list is used to find
  files when using the include command."


(interactive)
(proof-shell-invisible-command  "path"))

(defun phox-print-expression(expr)
"Interactive function : 
ask for a string and send a print command  to PhoX for it.

  In case argument expr
  is a closed expression of the language in use, prints it and gives its
  sort, gives an (occasionally) informative error message otherwise. In
  case expr is a defined expression (constant, theorem ...) 
  gives  the definition."

(interactive "sexpr: ")
(proof-shell-invisible-command (concat "print " expr)))

(defun phox-print-sort-expression(expr)
"Interactive function : 
ask for a string and send a print_sort command  to PhoX for it.

  Similar to print, but gives more information on sorts of bounded
  variable in expressions."

(interactive "sexpr: ")
(proof-shell-invisible-command (concat "print_sort " expr)))


(defun phox-priority-symbols-list(symblist)
"Interactive function : 
ask for a string and send a priority command  to PhoX for it.

  Print the priority of the given symbols. If no symbol are
  given, print the priority of all infix and prefix symbols.
."

(interactive "slist of symbols (possibly empty): ")
(proof-shell-invisible-command (concat "priority" symblist)))


(defun phox-search-string(string type)
  "Interactive function: 
ask for a string and possibly a type and send a search command to PhoX for it.

 Prints the list of all loaded symbols which have type and whose name
 contains the string. If no type is given, it prints all symbols whose
 name contains string."

(interactive 
"sstring : 
stype (nothing for any type, 'a as type parameter) :")
(proof-shell-invisible-command (concat "search \"" string "\" " type)))

(defvar phox-state-menu
  '("State"
["dependencies of a theorem" phox-depend-theorem t]
["show en extension list" phox-eshow-extlist t]
["value of a flag" phox-flag-name t]
["list of all paths" phox-path t]
["print expression" phox-print-expression  t]
["print expression with sorts" phox-print-sort-expression  t]
["priority of symbols (all if arg. empty)" phox-priority-symbols-list t]
["search for loaded symbols matching string" phox-search-string t]
)
"Phox menu for informations on state of the system."
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;--------------------------------------------------------------------------;;
;;
;;    Deleting symbols
;;
;;--------------------------------------------------------------------------;;
;; obsol�te probablement, sinon � modifier pour en �tendre la port�e.

(defun phox-delete-symbol(symbol)
  "Interactive function : 
ask for a symbol and send a delete command to PhoX for it."
  (interactive "ssymbol : ")
  (proof-shell-invisible-command (concat "del " symbol)))

(defun phox-delete-symbol-on-cursor()
"Interactive function : 
send a delete command to PhoX for the symbol whose name is under the cursor."
  (interactive)
  (let (start end)
    (save-excursion
      (forward-word -1)
      (setq start (point))
      (forward-word 1)
      (setq end (point)))
    (if (char-equal (char-after (- end 1)) ?\.)(setq end (- end 1)))
    (phox-delete-symbol (buffer-substring start end))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'phox-fun)


    