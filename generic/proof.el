;; proof.el	 Proof General loader.  All files require this one.
;;
;; Copyright (C) 1998-2002 LFCS Edinburgh. 
;; Authors:   David Aspinall, Yves Bertot, Healfdene Goguen,
;;            Thomas Kleymann and Dilip Sequeira
;; License:   GPL (GNU GENERAL PUBLIC LICENSE)
;;
;; $Id$
;;

(require 'proof-site)			; site config
(require 'proof-compat)			; Emacs and OS compatibility
(require 'proof-utils)			; utilities
(require 'proof-config)			; configuration variables


(proof-splash-message)                  ; welcome the user now.

;;;
;;; Extra autoloads that aren't automatic 
;;; (defined with define-derived-mode)
;;;

(autoload 'proof-mode "proof-script"
  "Proof General major mode class for proof scripts.")

(autoload 'proof-shell-mode "proof-shell"
  "Proof General shell mode class for proof assistant processes")


;;;
;;; Global variables
;;;

(deflocal proof-buffer-type nil
  "Symbol for the type of this buffer: 'script, 'shell, 'goals, or 'response.")

(defvar proof-shell-busy nil
  "A lock indicating that the proof shell is processing.
When this is non-nil, proof-shell-ready-prover will give
an error.")

(defvar proof-included-files-list nil 
  "List of files currently included in proof process.
This list contains files in canonical truename format
\(see `file-truename').

Whenever a new file is being processed, it gets added to this list
via the proof-shell-process-file configuration settings.
When the prover retracts a file, this list is resynchronised via the
proof-shell-retract-files-regexp and proof-shell-compute-new-files-list 
configuration settings.

Only files which have been *fully* processed should be included here.
Proof General itself will automatically add the filenames of a script
buffer which has been completely read when scripting is deactivated.
It will automatically remove the filename of a script buffer which
is completely unread when scripting is deactivated.

NB: Currently there is no generic provision for removing files which
are only partly read-in due to an error, so ideally the proof assistant
should only output a processed message when a file has been successfully
read.")


(defvar proof-script-buffer nil
  "The currently active scripting buffer or nil if none.")

;; FIXME: should fixup Coq's multiple file mode
(defvar proof-previous-script-buffer nil
  "Previous value of proof-script-buffer, recorded when scripting turned off.
At the moment, this is only used for Coq's ugly multiple file code,
to help guess the directory of files Coq says it's reinterning.")

(defvar proof-shell-buffer nil
  "Process buffer where the proof assistant is run.")

(defvar proof-goals-buffer nil
  "The goals buffer.")

(defvar proof-response-buffer nil
  "The response buffer.")

(defvar proof-trace-buffer nil
  "A tracing buffer for storing tracing output from the proof shell.
See `proof-shell-trace-output-regexp' for details.")

(defvar proof-thms-buffer nil
  "A buffer for displaying a list of theorems from the proof assistant.
See `proof-shell-thm-display-regexp' for details.")

(defvar proof-shell-error-or-interrupt-seen nil
  "Flag indicating that an error or interrupt has just occurred.
Set to 'error or 'interrupt if one was observed from the proof 
assistant during the last group of commands.")

(defvar proof-shell-proof-completed nil
  "Flag indicating that a completed proof has just been observed.
If non-nil, the value counts the commands from the last command
of the proof (starting from 1).")

;; FIXME da: remove proof-terminal-string.  At the moment some
;; commands need to have the terminal string, some don't.
;; It's used variously in proof-script and proof-shell, which
;; is another mess.  [Shell mode implicitly assumes script mode
;; has been configured].
;; We should assume commands are terminated at the specific level.

(defvar proof-terminal-string nil
  "End-of-line string for proof process.")

;;;
;;; Load other Proof General libraries
;;;

(require 'proof-system)


;;;
;;; Unload utility (not wholly successful)
;;;

(defun unload-pg ()
  (interactive)
  (mapcar 
   (lambda (feat) (condition-case nil
		    (unload-feature feat 'force)
		    (error nil)))
   '(proof-splash pg-assoc pg-xml proof-depends proof-indent proof-site
     proof-shell pg-metadata proof-menu pg-pbrpm pg-pgip proof-script
     proof-autoloads pg-response pg-goals pg-pgip-old proof-toolbar
     proof-easy-config proof-config proof-mmm proof pg-xhtml
     proof-utils proof-syntax proof-system _pkg pg-user proof-x-symbol
     pg-thymodes pg-autotest
     ;; 
     isar-syntax isar-find-theorems x-symbol-isabelle x-symbol-isar
     isar-autotest interface-setup isabelle-system isar isar-mmm
     isar-keywords
     ;;
     coq-abbrev coq-db x-symbol-coq coq-local-vars coq coq-syntax
     coq-indent coq-autotest)))

     


(provide 'proof)
;; proof.el ends here
