;; proof-config.el  Proof General configuration for proof assistant
;;
;; Copyright (C) 1994 - 1998 LFCS Edinburgh. 
;; Authors: David Aspinall, Yves Bertot, Healfdene Goguen,
;;          Thomas Kleymann and Dilip Sequeira
;;
;; Maintainer:  Proof General maintainer <proofgen@dcs.ed.ac.uk>
;;
;; $Id$
;;
;; 
;; This file declares all user options and prover-specific
;; configuration variables for Proof General.  The variables
;; are used variously by the proof script mode and the 
;; proof shell mode, menus, and toolbar.
;; 
;; To customize Proof General for a new proof assistant, you
;; should read this file carefully! 
;;
;;  1. User options 
;;  2. Major modes
;;  3. Menus, user-level commands.
;;  4. Script mode configuration 
;;  5. Shell mode configuration
;;     5a. commands
;;     5b. regexps
;;     5c. hooks
;;  6. Global constants 
;;
;; The user options don't need to be set on a per-prover basis,
;; and the global constants probably should not be touched.
;; The remaining variables in sections 2-5 must be set for
;; each proof assistant.
;;
;; Customization groups and structure
;;
;;  proof-general	: User options for Proof General    (1)
;;    <Prover name>     : User options for proof assistant
;;
;;  proof-general-internals  :  Internal settings of Proof General (6)
;;    prover-config	     :  Configuration for proof assistant (2,3)
;;      proof-script	     :     settings for proof script mode (4)
;;      proof-shell	     :     settings for proof shell mode (5)
;;    <Prover name>-config   :  Specific internal settings for a prover
;;
;; [Developers note: The customize library is a bit picky over the
;;  :type property and some correct types don't work properly.
;;  If the type is ill-formed, editing the whole group will be broken.
;;  Check after updates, by killing all customize buffers and
;;  invoking customize-group]
;;


;;
;; 1. User options for proof mode
;;
;; The following variables are user options for Proof General.
;; They appear in the 'proof' customize group and should
;; *not* normally be touched by prover specific code.
;;

(defcustom proof-prog-name-ask nil
  "*If non-nil, query user which program to run for the inferior process."
  :type 'boolean
  :group 'proof-general)

(and (featurep 'toolbar)
(defcustom proof-toolbar-wanted t
  "*Whether to use toolbar in proof mode."
  :type 'boolean
  :group 'proof-general))

(defcustom proof-toolbar-follow-mode 'locked
  "*Choice of how point moves with toolbar commands.
One of the symbols: locked, follow, ignore.
If locked, point sticks to the end of the locked region with toolbar commands.
If follow, point moves just when needed to display the locked region end.
If ignore, point is never moved after toolbar movement commands."
  :type '(choice
	  (const :tag "Follow locked region" locked)
	  (const :tag "Keep locked region displayed" follow)
	  (const :tag "Never move" ignore))
  :group 'proof-general)

(defcustom proof-window-dedicated t
  "*Whether response and goals buffers have dedicated windows.
If t, windows displaying responses from the prover will not
be switchable to display other windows.  This helps manage
your display, but can sometimes be inconvenient, especially
for experienced Emacs users."
  :type 'boolean
  :group 'proof-general)

(defcustom proof-strict-read-only 
  ;; For FSF Emacs, strict read only is buggy when used in
  ;; conjunction with font-lock.
  (string-match "XEmacs" emacs-version)
  "*Whether Proof General is strict about the read-only region in buffers.
If non-nil, an error is given when an attempt is made to edit the
read-only region.  If nil, Proof General is more relaxed (but may give
you a reprimand!)"
  :type 'boolean
  :group 'proof-general)

(defcustom proof-script-indent nil
  ;; Particular proof assistants can enable this if they feel
  ;; confident about it.  (I don't). -da
  "If non-nil, enable indentation code for proof scripts.
Currently the indentation code can be rather slow for large scripts,
and is critical on the setting of regular expressions for particular
provers.  Enable it if it works for you."  
  :type 'boolean 
  :group 'proof-general)

(defcustom proof-one-command-per-line nil
  "*If non-nil, format for newlines after each proof command in a script.
This option is not fully-functional at the moment."
  :type 'boolean
  :group 'proof-general)

(defcustom proof-rsh-command ""
  "Shell command prefix to run a command on a remote host.  
For example,

   ssh bigjobs

Would cause Proof General to issue the command 'ssh bigjobs isabelle'
to start Isabelle remotely on our large compute server called 'bigjobs'.

The protocol used should be configured so that no user interaction
(passwords, or whatever) is required to get going."
  :type 'string
  :group 'proof-general)

(defcustom proof-auto-delete-windows t
  "If non-nil, automatically remove windows when they are cleaned.
For example, at the end of a proof the goals buffer window will
be cleared; if this flag is set it will automatically be removed.
If you want to fix the sizes of your windows you may want to set this
variable to 'nil' to avoid windows being deleted automatically.
If you use multiple frames, only the windows in the currently
selected frame will be affected."
  :type 'boolean
  :group 'proof-general)

;;
;; Faces.
;; We ought to test that these work sensibly:
;;   a) with default colours
;;   b) with -rv
;;   c) on console 
;;   d) on win32
;;   e) all above with FSF Emacs and XEmacs.
;; But it's difficult to keep track of all that!  
;; Please report any bad/failing colour 
;; combinations to proofgen@dcs.ed.ac.uk
;;
;; Some of these faces aren't used by default in Proof General,
;; but you can use them in font lock patterns for specific
;; script languages.
;;

(defgroup proof-faces nil
  "Faces used by Proof General."
  :group 'proof
  :prefix "proof-")
  

(defface proof-queue-face
  '((((type x) (class color) (background light))
     (:background "mistyrose"))
    (((type x) (class color) (background dark))
     (:background "mediumvioletred"))
    (t				
     (:foreground "white" :background "black")))
  "*Face for commands in proof script waiting to be processed."
  :group 'proof-faces)

(defface proof-locked-face
  '((((type x) (class color) (background light))   
     (:background "lavender"))
    (((type x) (class color) (background dark))   
     (:background "navy"))
    (t				
     (:underline t)))
  "*Face for locked region of proof script (processed commands)."
  :group 'proof-faces)

(defface proof-declaration-name-face
  '((((type x) (class color) (background light))   
     (:foreground "chocolate" 
      :bold t))
    (((type x) (class color) (background dark))   
     (:foreground "orange"
      :bold t))
    (t				
     (:italic t :bold t)))
  "*Face for declaration names in proof scripts.
Exactly what uses this face depends on the proof assistant."
  :group 'proof-faces)

(defconst proof-declaration-name-face 'proof-declaration-name-face
  "Expression that evaluates to a face.
Required so that 'proof-declaration-name-face is a proper facename in
both XEmacs 20.4 and Emacs 20.2's version of font-lock.")

(defface proof-tacticals-name-face
  '((((type x) (class color) (background light))   
     (:foreground "MediumOrchid3"))
    (((type x) (class color) (background dark))   
     (:foreground "orchid"))
    (t				
     (bold t)))
  "*Face for names of tacticals in proof scripts.
Exactly what uses this face depends on the proof assistant."
  :group 'proof-faces)

(defconst proof-tacticals-name-face 'proof-tacticals-name-face
  "Expression that evaluates to a face.
Required so that 'proof-declaration-name-face is a proper facename in
both XEmacs 20.4 and Emacs 20.2's version of font-lock.")

(defface proof-error-face 
  '((((type x) (class color) (background light))   
     (:background "salmon1"  
      :bold t))
    (((type x) (class color) (background dark))   
     (:background "brown" 
      :bold t))
    (t				
     (:bold t)))
  "*Face for error messages from proof assistant."
  :group 'proof-faces)

(defface proof-warning-face
  '((((type x) (class color) (background light))   
     (:background "lemon chiffon"))
    (((type x) (class color) (background dark))   
     (:background "orange2"))
    (t				
     (:italic t)))
  "*Face for warning messages.
Could come either from proof assistant or Proof General itself."
  :group 'proof-faces)

(defface proof-eager-annotation-face
  '((((type x) (class color) (background light))   
     (:background "lightgoldenrod"))
    (((type x) (class color) (background dark))   
     (:background "darkgoldenrod"))
    (t				
     (:italic t)))
  "*Face for messages from proof assistant."
  :group 'proof-faces)





;;
;; CONFIGURATION VARIABLES
;;
;;

(defgroup prover-config nil
  "Configuration of Proof General for the prover in use."
  :group 'proof-general-internals
  :prefix "proof-")

;; The variables in the "prover-config" (NB: not "proof config"!!)
;; customize group are those which are intended to be set by the
;; prover specific elisp, i.e. constants set on a per-prover basis.

;; Putting these in a customize group is useful for documenting
;; this type of variable, and for developing a new instantiation
;; of Proof General.
;; But it is *not* useful for final user-level customization!
;; The reason is that saving these customizations across a session is
;; not liable to work, because the prover specific elisp usually
;; overrides with a series of setq's in <assistant>-mode-config type
;; functions.  This is why prover-config appears under the
;; proof-general-internal group.


(defcustom proof-assistant ""
  "Name of the proof assistant Proof General is using.
This is set automatically by the mode stub defined in proof-site,
from the name given in proof-assistant-table."
  :type 'string
  :group 'prover-config)




;;
;; 2. The major modes used by Proof General.
;;

;; FIXME: these functions could be set automatically to standard values,
;; i.e. <assistant>-mode, <assistant>-shell-mode, <assistant>-pbp-mode.
;; FIXME: mode-for-script is unused at the moment, added just for
;; uniformity.  The other two are used when a shell buffer is started.

(defcustom proof-mode-for-shell 'proof-shell-mode
  "Mode for proof shell buffers.
Usually customised for specific prover.
Suggestion: this can be set in proof-pre-shell-start-hook."
  :type 'function
  :group 'prover-config)

(defcustom proof-mode-for-response 'proof-response-mode
  "Mode for proof response buffer.
Usually customised for specific prover.
Suggestion: this can be set in proof-pre-shell-start-hook."
  :type 'function
  :group 'prover-config)

(defcustom proof-mode-for-pbp 'pbp-mode
  "Mode for proof state display buffers.
Usually customised for specific prover.
Suggestion: this can be set in proof-pre-shell-start-hook."
  :type 'function
  :group 'prover-config)

(defcustom proof-mode-for-script 'proof-mode
  "Mode for proof script buffers.
This is used by Proof General to find out which buffers 
contain proof scripts.
Suggestion: this can be set in the script mode configuration."
  :type 'function
  :group 'prover-config)




;;
;; 3. Configuration for menus, user-level commands, etc.
;;

(defcustom proof-assistant-home-page ""
  "Web address for information on proof assistant"
  :type 'string
  :group 'prover-config)

(defcustom proof-ctxt-string ""
  "*Command to display the context in proof assistant."
  :type 'string
  :group 'prover-config)

(defcustom proof-help-string ""
  "*Command to ask for help in proof assistant."
  :type 'string
  :group 'prover-config)

(defcustom proof-prf-string ""
  "Command to display proof state in proof assistant."
  :type 'string
  :group 'prover-config)

(defcustom proof-goal-command nil
  "Command to set a goal in the proof assistant.  String or fn.
If a string, the format character %s will be replaced by the
goal string. If a function, should return a command string to
insert when called interactively."
  :type '(choice string function)
  :group 'prover-config)

(defcustom proof-save-command nil
  "Command to save a proved theorem in the proof assistant.  String or fn.
If a string, the format character %s will be replaced by the
theorem name. If a function, should return a command string to
insert when called interactively."
  :type '(choice string function)
  :group 'prover-config)



;;
;; 4. Configuration for proof script mode
;;

;;
;; The following variables should be set before proof-config-done
;; is called.  These configure the mode for the script buffer,
;; including highlighting, etc.
;;

(defgroup proof-script nil
  "Proof General configuration of scripting buffer mode."
  :group 'prover-config
  :prefix "proof-")


(defcustom proof-terminal-char nil
  "Character which terminates a proof command in a script buffer."
  :type 'character
  :group 'proof-script)

(defcustom proof-comment-start ""
  "String which starts a comment in the proof assistant command language.
The script buffer's comment-start is set to this string plus a space."
  :type 'string
  :group 'proof-script)

(defcustom proof-comment-end ""
  "String which starts a comment in the proof assistant command language.
The script buffer's comment-end is set to this string plus a space."
  :type 'string
  :group 'proof-script)

(defcustom proof-save-command-regexp nil 
  "Matches a save command"
  :type 'regexp
  :group 'proof-script)

(defcustom proof-save-with-hole-regexp nil 
  "Regexp which matches a command to save a named theorem.
Match number 2 should be the name of the theorem saved.
Used for setting names of goal..save regions and for default
func-menu configuration in proof-script-find-next-goalsave."
  :type 'regexp
  :group 'proof-script)

(defcustom proof-goal-command-regexp nil
  "Matches a goal command."
  :type 'regexp
  :group 'proof-script)

(defcustom proof-goal-with-hole-regexp nil 
  "Regexp which matches a command used to issue and name a goal.
Match number 2 should be the name of the goal issued.
Used for setting names of goal..save regions and for default
func-menu configuration in proof-script-find-next-goalsave."
  :type 'regexp
  :group 'proof-script)

(defcustom proof-script-next-entity-regexps nil
  "Regular expressions to control finding definitions in script.
This is the list of the form

   (ANYENTITY-REGEXP
    DISCRIMINATOR-REGEXP  ... DISCRIMINATOR-REGEXP)

The idea is that ANYENTITY-REGEXP matches any named entity in the
proof script, on a line where the name appears.  This is assumed to be
the start or the end of the entity.  The discriminators then test
which kind of entity has been found, to get its name.  A
DISCRIMINATOR-REGEXP has one of the forms

  (REGEXP MATCHNO) 
  (REGEXP MATCHNO 'backward BACKREGEXP) 
  (REGEXP MATCHNO 'forward FORWARDREGEXP)

If REGEXP matches the string captured by ANYENTITY-REGEXP, then
MATCHNO is the match number for the substring which names the entity.

If 'backward BACKREGEXP is present, then the start of the entity
is found by searching backwards for BACKREGEXP.

Conversely, if 'forward FORWARDREGEXP is found, then the end of
the entity is found by searching forwards for FORWARDREGEXP.

Otherwise, the start and end of the entity will be the region matched
by ANYENTITY-REGEXP.

This mechanism allows fairly complex parsing of the buffer, in
particular, it allows for goal..save regions which are named only at
the end.  However, it does not parse strings, comments. or parentheses.

A default value which should work for goal..saves is calculated from
proof-goal-with-hole-regexp, proof-goal-command-regexp, and
proof-save-with-hole-regexp."
  :type 'sexp
    ;; Bit tricky.
    ;; (list (regexp :tag "Any entity matcher")
    ;;    (:inline t repeat (choice regexp (const 'backward) etc
  :group 'proof-script)


(defcustom proof-script-find-next-entity-fn
  'proof-script-find-next-entity
  "Name of function to find next interesting entity in a script buffer.
This is used to configure func-menu.  The default value is
proof-script-find-next-entity, which searches for the next entity
based on fume-function-name-regexp which by default is set from
proof-script-next-entity-regexps.  The function should move
point forward in a buffer, and return a cons cell of the name and the
beginning of the entity's region."
  :type 'function
  :group 'proof-script)

;; FIXME da: This next one is horrible.  We clearly would rather
;; have proof-goal-regexp instead.  I think this was born to
;; solve func-menu configuration for Coq (or a similar problem),
;; but now that can be configured in a better way.
;; This function variable needs removing.

(defcustom proof-goal-command-p nil 
  "Is this a goal"
  :type 'function
  :group 'proof-script)

(defcustom proof-lift-global nil
  "This function lifts local lemmas from inside goals out to top level.
This function takes the local goalsave span as an argument. Set this
to `nil' of the proof assistant does not support nested goals."
  :type 'function
  ;; FIXME customize broken on choices with function in them?
  ;; :type '(choice (const :tag "No local lemmas" nil) (function))
  :group 'proof-script)

(defcustom proof-count-undos-fn nil 
  "Compute number of undos in a target segment"
  :type 'function
  :group 'proof-script)

; Not yet implemented.
;
;(defcustom proof-atomic-sequence-lists nil
;  "list of instructions for setting up atomic sequences of commands (ACS).

;Each instruction is
;a list of the form `(END START &optional FORGET-COMMAND)'. END is a
;regular expression to recognise the last command in an ACS. START
;is a function. Its input is the last command of an ACS. Its output
;is a regular exression to recognise the first command of the ACS.
;It is evaluated once and the output is successively matched agains
;previously processed commands until a match occurs (or the
;beginning of the current buffer is reached). The region determined
;by (START,END) is locked as an ACS. Optionally, the ACS is
;annotated with the actual command to retract the ACS. This is
;computed by applying FORGET-COMMAND to the first and last command
;of the ACS."
;  ;; FIXME customize broken on choices with function in them?
;  ;;:type '(repeat (cons regexp function (choice (const nil) function)))
;  :type '(repeat (cons regexp function function))
;  :group 'proof-shell)


(defconst proof-no-command "COMMENT"
  "String used as a nullary action (send no command to the proof assistant).
Only relevant for proof-find-and-forget-fn.")

(defcustom proof-find-and-forget-fn nil
  "Function returning a command string to forget back to before its argument span.
The special string proof-no-command means there is nothing to do."
  :type 'function
  :group 'proof-script)

(defcustom proof-goal-hyp-fn nil
  "Function which returns cons cell if point is at a goal/hypothesis.
First element of cell is a symbol, 'goal' or 'hyp'.  The second
element is a string: the goal or hypothesis itself.  This is used
when parsing the proofstate output"
  :type 'function
  :group 'proof-script)

(defcustom proof-kill-goal-command ""
  "Command to kill a goal."
  :type 'string
  :group 'proof-script)

(defcustom proof-global-p nil
  "Whether a command is a global declaration.  Predicate on strings or nil.
This is used to handle nested goals allowed by some provers."
  :type 'function
  :group 'proof-script)

(defcustom proof-state-preserving-p nil
  "A predicate, non-nil if its argument preserves the proof state."
  :type 'function
  :group 'proof-script)








;;
;;  5. Configuration for proof shell	                            
;;
;; The variables in this section concern the proof shell mode.
;; The first group of variables are hooks invoked at various points.
;; The second group of variables are concerned with matching the output
;; from the proof assistant.
;;
;; Variables here are put into the customize group 'proof-shell'.
;;
;; These should be set in the shell mode configuration, again,
;; before proof-shell-config-done is called.
;;

(defgroup proof-shell nil
  "Settings for output from the proof assistant in the proof shell."
  :group 'prover-config
  :prefix "proof-shell-")


;; 
;; 5a. commands
;;

(defcustom proof-prog-name nil
  "System command to run program name in proof shell.
Suggestion: this can be set in proof-pre-shell-start-hook from
a variable which is in the proof assistant's customization
group.  This allows different proof assistants to coexist
(albeit in separate Emacs sessions)."
  :type 'string
  :group 'proof-shell)

(defcustom proof-shell-init-cmd ""
   "The command for initially configuring the proof process."
   :type '(choice string (const nil))
   :group 'proof-shell)

(defcustom proof-shell-cd nil
  "Command to the proof assistant to change the working directory."
  :type 'string
  :group 'proof-shell)


;;
;; 5b. Regexp variables for matching output from proof process.
;;

(defcustom proof-shell-prompt-pattern nil 
   "Proof shell's value for comint-prompt-pattern, which see."
   :type 'regexp
   :group 'proof-shell)

;; FIXME da: replace this with wakeup-regexp or prompt-regexp?
;; May not need next variable.
(defcustom proof-shell-wakeup-char nil
  "A special character which terminates an annotated prompt.
Set to nil if proof assistant does not support annotated prompts."
  :type '(choice character (const nil))
  :group 'proof-shell)

(defcustom proof-shell-annotated-prompt-regexp ""
  "Regexp matching a (possibly annotated) prompt pattern.
Output is grabbed between pairs of lines matching this regexp.
To help matching you may be able to annotate the proof assistant
prompt with a special character not appearing in ordinary output.
The special character should appear in this regexp, should
be the value of proof-shell-wakeup-char."
  :type 'regexp
  :group 'proof-shell)

(defcustom proof-shell-abort-goal-regexp nil
  "Regexp matching output from an aborted proof."
  :type 'regexp
  :group 'proof-shell)

(defcustom proof-shell-error-regexp nil
  "Regexp matching an error report from the proof assistant.
We assume that an error message corresponds to a failure
in the last proof command executed.  (So don't match
mere warning messages with this regexp)."
  :type 'regexp
  :group 'proof-shell) 

(defcustom proof-shell-interrupt-regexp nil
  "Regexp matching output indicating the assistant was interrupted."
  :type 'regexp
  :group 'proof-shell) 

(defcustom proof-shell-proof-completed-regexp nil
  "Regexp matching output indicating a finished proof."
  :type 'regexp
  :group 'proof-shell)

(defcustom proof-shell-clear-response-regexp nil
  "Regexp matching output telling Proof General to clear the response buffer.
This feature is useful to give the prover more control over what output
is shown to the user.  Set to nil to disable."
  :type 'regexp
  :group 'proof-shell)

(defcustom pbp-goal-command nil
  "Command informing the prover that `pbp-button-action' has been
  requested on a goal."
  :type 'regexp
  :group 'proof-shell)

(defcustom pbp-hyp-command nil
  "Command informing the prover that `pbp-button-action' has been
  requested on an assumption."
  :type 'regexp
  :group 'proof-shell)

(defcustom proof-shell-result-start ""
  "Regexp matching start of an output from the prover after pbp commands.
In particular, after a `pbp-goal-command' or a `pbp-hyp-command'."
  :type 'regexp
  :group 'proof-shell)

(defcustom proof-shell-result-end ""
  "Regexp matching end of output from the prover after pbp commands.
In particular, after a `pbp-goal-command' or a `pbp-hyp-command'."
  :type 'regexp
  :group 'proof-shell)

(defcustom proof-shell-start-goals-regexp ""
  "Regexp matching the start of the proof state output."
  :type 'regexp
  :group 'proof-shell)

(defcustom proof-shell-end-goals-regexp ""
  "Regexp matching the end of the proof state output."
  :type 'regexp
  :group 'proof-shell)

(defcustom pbp-error-regexp nil
  "Regexp indicating that the proof process has identified an error."
  :type 'regexp
  :group 'proof-shell)

(defcustom proof-shell-eager-annotation-start nil
  "Eager annotation field start.  A regular expression or nil.
An eager annotation indicates to Emacs that some following output
should be displayed immediately and not accumulated for parsing.
Set to nil to disable this feature.

The default value is \"\n\" to match up to the end of the line."
  :type '(choice regexp (const :tag "Disabled" nil))
  :group 'proof-shell)

(defcustom proof-shell-eager-annotation-end "\n"
  "Eager annotation field end.  A regular expression or nil.
An eager annotation indicates to Emacs that some following output
should be displayed immediately and not accumulated for parsing.
The default value is \"\\n\" to match up to the end of the line."
  :type '(choice regexp (const :tag "Unset" nil))
  :group 'proof-shell)

(defcustom proof-shell-assumption-regexp nil
  "A regular expression matching the name of assumptions."
  :type '(choice regexp (const :tag "Unset" nil))
  :group 'proof-shell)



;;
;; 5c. hooks 
;;

(defcustom proof-shell-insert-hook nil 
  "Hooks run by proof-shell-insert before inserting a command.
Can be used to configure the proof assistant to the interface in
various ways (for example, setting the line width of output).
Any text inserted by these hooks will be sent to the proof process
before every command issued by Proof General."
  :type '(repeat function)
  :group 'proof-shell)

(defcustom proof-pre-shell-start-hook nil
  "Hooks run before proof shell is started.
Usually this is set to a function which configures the proof shell
variables."
  :type '(repeat function)
  :group 'proof-shell)

(defcustom proof-shell-handle-delayed-output-hook
  '(proof-pbp-focus-on-first-goal)
  "Hooks run after new output has been displayed in the RESPONSE buffer."
  :type '(repeat function)
  :group 'proof-shell)

(defcustom proof-shell-handle-error-hook
  '(proof-goto-end-of-locked-if-pos-not-visible-in-window)
  "Hooks run after an error has been reported in the RESPONSE buffer."
  :type '(repeat function)
  :group 'proof-shell)

(defcustom proof-shell-process-file nil
  "A tuple of the form (regexp . function) to match a processed file name.

If REGEXP matches output, then the function FUNCTION is invoked on the
output string chunk. It must return a script file name (with complete
path) the system is currently processing. In practice, FUNCTION is
likely to inspect the match data.  If it returns the empty string,
the file name of the scripting buffer is used instead.  If it
returns nil, no action is taken.

Care has to be taken in case the prover only reports on compiled
versions of files it is processing. In this case, FUNCTION needs to
reconstruct the corresponding script file name. The new (true) file
name is added to the front of `proof-included-files-list'."
  :type '(choice (cons regexp function) (const nil))
  :group 'proof-shell)

(defcustom proof-shell-retract-files-regexp nil
  "A REGEXP to match that the prover has retracted across file boundaries.

At this stage, Proof General's view of the processed files is out of
date and needs to be updated with the help of the function
`proof-shell-compute-new-files-list'."
  :type '(choice regexp (const nil))
  :group 'proof-shell)

(defcustom proof-shell-compute-new-files-list nil
  "Function to update `proof-included-files list'.

It needs to return an up to date list of all processed files. Its
output is stored in `proof-included-files-list'. Its input is the
string of which `proof-shell-retract-files-regexp' matched a
substring. In practice, this function is likely to inspect the
previous (global) variable `proof-included-files-list' and the match
data triggered by `proof-shell-retract-files-regexp'."
  :type '(choice function (const nil))
  :group 'proof-shell)





;;
;; 6. Global constants
;; 
(defcustom proof-general-name "Proof-General"
  "Proof General name used internally and in menu titles."
  :type 'string
  :group 'proof-general-internals)

(defcustom proof-proof-general-home-page
  "http://www.dcs.ed.ac.uk/home/proofgen"
  "*Web address for Proof General"
  :type 'string
  :group 'proof-general-internals)


;; FIXME: da: could we put these into another keymap shared across the
;; various PG modes?
(defcustom proof-universal-keys
  '(([(control c) (control c)] . proof-interrupt-process)
    ([(control c) (control v)] . proof-execute-minibuffer-cmd))
"List of keybindings which for the script and response buffer. 
Elements of the list are tuples (k . f) 
where `k' is a keybinding (vector) and `f' the designated function."
  :type 'sexp
  :group 'proof-general-internals)

;;; 
;;; FIXME: unsorted below here
;;;

(defcustom pbp-change-goal nil
  "Command to change to the goal %s"
  :type 'string
  :group 'prover-config)

(defcustom proof-shell-process-output-system-specific nil
  "Set this variable to handle system specific output.
Errors, start of proofs, abortions of proofs and completions of
proofs are recognised in the function `proof-shell-process-output'.
All other output from the proof engine is simply reported to the
user in the RESPONSE buffer.

To catch further special cases, set this variable to a pair of
functions '(condf . actf). Both are given (cmd string) as arguments.
`cmd' is a string containing the currently processed command.
`string' is the response from the proof system. To change the
behaviour of `proof-shell-process-output', (condf cmd string) must
return a non-nil value. Then (actf cmd string) is invoked. See the
documentation of `proof-shell-process-output' for the required
output format."
  :type '(cons (function function))
  :group 'proof-shell)

(defcustom proof-activate-scripting-hook nil
  "Hook run when a buffer is switched into scripting mode.
The current buffer will be the newly active scripting buffer.

This hook may be useful for synchronizing with the proof
assistant, for example, to switch to a new theory."
  :type '(repeat function)
  :group 'proof-script)




;;
;; Proof script indentation
;;

;; FIXME: document this junk
(defcustom proof-stack-to-indent nil
  "Prover-specific code for indentation."
  :type 'sexp
  :group 'proof-script)

(defcustom proof-parse-indent nil
  "Proof-assistant specific function for parsing characters for
  indentation which is invoked in `proof-parse-to-point.'. Must be a
  function taking two arguments, a character (the current character)
  and a stack reflecting indentation, and must return a stack. The
  stack is a list of the form (c . p) where `c' is a character
  representing the type of indentation and `p' records the column for
  indentation. The generic `proof-parse-to-point.' function supports
  parentheses and commands. It represents these with the characters
  `?\(', `?\[' and `proof-terminal-char'. "
  :type 'sexp
  :group 'proof-script)


;;
;; More shell junk to sort out [maybe for pbp]
;;

(defcustom proof-shell-first-special-char nil
  "First special character.
Codes above this character can have special meaning to Proof General,
and are stripped from the prover's output strings."
  :type '(choice character (const nil))
  :group 'proof-shell)

(defcustom proof-shell-start-char nil
  "Starting special for a subterm markup.
Subsequent characters with values *below* proof-shell-first-special-char
are assumed to be subterm position indicators.  Subterm markups should
be finished with proof-shell-end-char."
  :type '(choice character (const nil))
  :group 'proof-shell)

(defcustom proof-shell-end-char nil
  "Finishing special for a subterm markup.
See documentation of proof-shell-start-char."
  :type '(choice character (const nil))
  :group 'proof-shell)

(defvar proof-shell-goal-char nil "goal mark")

(defvar proof-shell-field-char nil "annotated field end")


;; FIXME da: where is this variable used?  
;;	      dropped in favour of goal-char?
;; Answer: this is used in *specific* modes, see e.g.
;; lego-goal-hyp.  This stuff needs making more generic.

(defvar proof-shell-goal-regexp nil
  "A regular expression matching the identifier of a goal.
Used by proof mode to parse proofstate output, and also
to set outline heading regexp.")

(defvar proof-analyse-using-stack nil
  "Are annotations sent by proof assistant local or global")


;; End of proof-config.el
(provide 'proof-config)
