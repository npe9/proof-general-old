;;; proof-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(if (featurep 'proof-autoloads) (error "Already loaded"))
  
(provide 'proof-autoloads)


;;;### (autoloads (bufhist-exit bufhist-init bufhist-mode) "bufhist"
;;;;;;  "../lib/bufhist.el" (19127 26600))
;;; Generated autoloads from ../lib/bufhist.el

(autoload (quote bufhist-mode) "bufhist" "\
Minor mode retaining an in-memory history of the buffer contents.

Commands:\\<bufhist-minor-mode-map>
\\[bufhist-prev]    bufhist-prev    go back in history
\\[bufhist-next]    bufhist-next    go forward in history
\\[bufhist-first]   bufhist-first   go to first item in history
\\[bufhist-last]    bufhist-last    go to last (current) item in history.
\\[bufhist-clear]   bufhist-clear   clear history.
\\[bufhist-delete]  bufhist-clear   delete current item from history.

\(fn &optional ARG)" t nil)

(autoload (quote bufhist-init) "bufhist" "\
Initialise a ring history for the current buffer.
The history will be read-only unless READWRITE is non-nil.
For read-only histories, edits to the buffer switch to the latest version.
The size defaults to `bufhist-ring-size'.

\(fn &optional READWRITE RINGSIZE)" t nil)

(autoload (quote bufhist-exit) "bufhist" "\
Stop keeping ring history for current buffer.

\(fn)" t nil)

;;;***

;;;### (autoloads (holes-insert-and-expand holes-abbrev-complete
;;;;;;  holes-mode holes-set-make-active-hole) "holes" "../lib/holes.el"
;;;;;;  (19108 1539))
;;; Generated autoloads from ../lib/holes.el

(autoload (quote holes-set-make-active-hole) "holes" "\
Make a new hole between START and END or at point, and make it active.

\(fn &optional START END)" t nil)

(autoload (quote holes-mode) "holes" "\
Toggle Holes minor mode.
With arg, turn Outline minor mode on if arg is positive, off otherwise.

The mode `holes-mode' is meant to help program editing.  It is
useful to build complicated expressions by copy pasting several
peices of text from different parts of a buffer (or even from
different buffers).

HOLES

A hole is a piece of (highlighted) text that may be replaced by
another part of text later.  There is no information stored on the
file for holes, so you can save and modify files containing holes with
no harm... You can even insert or delete characters inside holes like
any other characters.

USE

At any time only one particular hole, called \"active\", can be
\"filled\".  Holes can be in several buffers but there is always one or
zero active hole globally.  It is highlighted with a different color.

Functions described below have default shortcuts when `holes-mode' is
on that you can customize.

TO DEFINE A HOLE, two methods:

 o Select a region with keyboard or mouse, then use
   \\[holes-set-make-active-hole].  If the selected region is empty,
   then a hole containing # is created at point.

 o Select text with mouse while pressing ctrl and meta (`C-M-select').
   If the selected region is empty (i.e. if you just click while
   pressing ctrl+meta), then a hole containing # is created.

TO ACTIVATE A HOLE, click on it with the button 1 of your mouse.  The
previous active hole will be deactivated.

TO FORGET A HOLE without deleting its text, click on it with the
button 2 (middle) of your mouse.

TO DESTROY A HOLE and delete its text, click on it with the button 3
of your mouse.

TO FILL A HOLE with a text selection, first make sure it is active,
then two methods:

 o Select text with keyboard or mouse and hit
   \\[holes-replace-update-active-hole]

 o Select text with mouse while pressing ctrl, meta and shift
   (`C-M-S-select').  This is a
   generalization of the `mouse-track-insert' feature of XEmacs.  This
   method allows you to fill different holes faster than with the usual
   copy-paste method.

After replacement the next hole is automatically made active so you
can fill it immediately by hitting again
\\[holes-replace-update-active-hole] or `C-M-S-select'.

TO JUMP TO THE ACTIVE HOLE, just hit
\\[holes-set-point-next-hole-destroy].  You must
be in the buffer containing the active hole.  the point will move to
the active hole, and the active hole will be destroyed so you can type
something to put at its place.  The following hole is automatically
made active, so you can hit \\[holes-set-point-next-hole-destroy]
again.

It is useful in combination with abbreviations.  For example in
`coq-mode' \"fix\" is an abbreviation for Fixpoint # (# : #) {struct #} :
# := #, where each # is a hole. Then hitting
\\[holes-set-point-next-hole-destroy] goes from one hole to the
following and you can fill-in each hole very quickly.

COMBINING HOLES AND SKELETONS

`holes' minor mode is made to work with minor mode `skeleton' minor
mode.

KNOWN BUGS

 o Don't try to make overlapping holes, it doesn't work. (what would
it mean anyway?)

 o Cutting or pasting a hole will not produce new holes, and
undoing on holes cannot make holes re-appear.

\(fn &optional ARG)" t nil)

(autoload (quote holes-abbrev-complete) "holes" "\
Complete abbrev by putting holes and indenting.
Moves point at beginning of expanded text.  Put this function as
call-back for your abbrevs, and just expanded \"#\" and \"@{..}\" will
become holes.

\(fn)" nil nil)

(autoload (quote holes-insert-and-expand) "holes" "\
Insert S, expand it and replace #s and @{]s by holes.

\(fn S)" nil nil)

;;;***

;;;### (autoloads (maths-menu-mode) "maths-menu" "../lib/maths-menu.el"
;;;;;;  (19107 62723))
;;; Generated autoloads from ../lib/maths-menu.el

(autoload (quote maths-menu-mode) "maths-menu" "\
Install a menu for entering mathematical characters.
Uses window system menus only when they can display multilingual text.
Otherwise the menu-bar item activates the text-mode menu system.
This mode is only useful with a font which can display the maths repertoire.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (proof-associated-windows proof-associated-buffers)
;;;;;;  "pg-assoc" "pg-assoc.el" (19121 59721))
;;; Generated autoloads from pg-assoc.el

(autoload 'proof-associated-buffers "pg-assoc" "\
Return a list of the associated buffers.
Some may be dead/nil.

\(fn)" nil nil)

(autoload 'proof-associated-windows "pg-assoc" "\
Return a list of the associated buffers windows.
Dead or nil buffers are not represented in the list.

\(fn)" nil nil)

;;;***

;;;### (autoloads (proof-goals-config-done) "pg-goals" "pg-goals.el"
;;;;;;  (19121 59721))
;;; Generated autoloads from pg-goals.el

(autoload 'proof-goals-config-done "pg-goals" "\
Initialise the goals buffer after the child has been configured.

\(fn)" nil nil)

;;;***

;;;### (autoloads (pg-pgip-askprefs pg-pgip-maybe-askpgip pg-pgip-process-packet)
;;;;;;  "pg-pgip" "pg-pgip.el" (19122 39720))
;;; Generated autoloads from pg-pgip.el

(autoload 'pg-pgip-process-packet "pg-pgip" "\
Process the command packet PGIP, which is parsed XML according to pg-xml-parse-*.
The list PGIPS may contain one or more PGIP packets, whose contents are processed.

\(fn PGIPS)" nil nil)

(autoload 'pg-pgip-maybe-askpgip "pg-pgip" "\
Send an <askpgip> message to the prover if PGIP is supported.

\(fn)" nil nil)

(autoload 'pg-pgip-askprefs "pg-pgip" "\
Send an <askprefs> message to the prover.

\(fn)" nil nil)

;;;***

;;;### (autoloads (pg-response-has-error-location proof-next-error
;;;;;;  pg-response-message pg-response-display-with-face pg-response-maybe-erase
;;;;;;  proof-response-config-done proof-response-mode) "pg-response"
;;;;;;  "pg-response.el" (19121 59721))
;;; Generated autoloads from pg-response.el

(autoload 'proof-response-mode "pg-response" "\
Responses from Proof Assistant

\(fn)" t nil)

(autoload 'proof-response-config-done "pg-response" "\
Complete initialisation of a response-mode derived buffer.

\(fn)" nil nil)

(autoload 'pg-response-maybe-erase "pg-response" "\
Erase the response buffer according to pg-response-erase-flag.
ERASE-NEXT-TIME is the new value for the flag.
If CLEAN-WINDOWS is set, use proof-clean-buffer to do the erasing.
If FORCE, override pg-response-erase-flag.

If the user option proof-tidy-response is nil, then
the buffer is only cleared when FORCE is set.

No effect if there is no response buffer currently.
Returns non-nil if response buffer was cleared.

\(fn &optional ERASE-NEXT-TIME CLEAN-WINDOWS FORCE)" nil nil)

(autoload 'pg-response-display-with-face "pg-response" "\
Display STR with FACE in response buffer.

\(fn STR &optional FACE)" nil nil)

(autoload 'pg-response-message "pg-response" "\
Issue the message ARGS in the response buffer and display it.

\(fn &rest ARGS)" nil nil)

(autoload 'proof-next-error "pg-response" "\
Jump to location of next error reported in the response buffer.

A prefix arg specifies how many error messages to move;
negative means move back to previous error messages.

Optional argument ARGP means reparse the error message buffer
and start at the first error.

\(fn &optional ARGP)" t nil)

(autoload 'pg-response-has-error-location "pg-response" "\
Return non-nil if the response buffer has an error location.
See `pg-next-error-regexp'.

\(fn)" nil nil)

;;;***

;;;### (autoloads (pg-defthymode) "pg-thymodes" "pg-thymodes.el"
;;;;;;  (19108 51621))
;;; Generated autoloads from pg-thymodes.el

(autoload (quote pg-defthymode) "pg-thymodes" "\
Define a Proof General mode for theory files.
Mode name is SYM-mode, named NAMED.  BODY is the body of a setq and
can define a number of variables for the mode, viz:

  SYM-<font-lock-keywords>      (value for font-lock-keywords)
  SYM-<syntax-table-entries>	(list of pairs: used for modify-syntax-entry calls)
  SYM-<menu>			(menu for the mode, arg of easy-menu-define)
  SYM-<parent-mode>		(defaults to fundamental-mode)
  SYM-<filename-regexp>	        (regexp matching filenames for auto-mode-alist)

All of these settings are optional.

\(fn SYM NAME &rest BODY)" nil (quote macro))

;;;***

;;;### (autoloads (pg-clear-input-ring pg-remove-from-input-history
;;;;;;  pg-add-to-input-history pg-next-matching-input-from-input
;;;;;;  pg-previous-matching-input-from-input proof-imenu-enable
;;;;;;  pg-hint pg-next-error-hint pg-processing-complete-hint pg-jump-to-end-hint
;;;;;;  pg-response-buffers-hint pg-slow-fontify-tracing-hint proof-electric-terminator-enable
;;;;;;  proof-define-assistant-command-witharg proof-define-assistant-command
;;;;;;  proof-goto-point proof-script-new-command-advance) "pg-user"
;;;;;;  "pg-user.el" (19127 24433))
;;; Generated autoloads from pg-user.el

(autoload (quote proof-script-new-command-advance) "pg-user" "\
Move point to a nice position for a new command.
Assumes that point is at the end of a command.

\(fn)" t nil)

(autoload (quote proof-goto-point) "pg-user" "\
Assert or retract to the command at current position.
Calls `proof-assert-until-point' or `proof-retract-until-point' as
appropriate.

\(fn)" t nil)

(autoload (quote proof-define-assistant-command) "pg-user" "\
Define FN (docstring DOC) to send BODY to prover, based on CMDVAR.
BODY defaults to CMDVAR, a variable.

\(fn FN DOC CMDVAR &optional BODY)" nil (quote macro))

(autoload (quote proof-define-assistant-command-witharg) "pg-user" "\
Define command FN to prompt for string CMDVAR to proof assistant.
CMDVAR is a variable holding a function or string.  Automatically has history.

\(fn FN DOC CMDVAR PROMPT &rest BODY)" nil (quote macro))

(autoload (quote proof-electric-terminator-enable) "pg-user" "\
Make sure the modeline is updated to display new value for electric terminator.

\(fn)" nil nil)

(autoload (quote pg-slow-fontify-tracing-hint) "pg-user" "\
Not documented

\(fn)" nil nil)

(autoload (quote pg-response-buffers-hint) "pg-user" "\
Not documented

\(fn &optional NEXTBUF)" nil nil)

(autoload (quote pg-jump-to-end-hint) "pg-user" "\
Not documented

\(fn)" nil nil)

(autoload (quote pg-processing-complete-hint) "pg-user" "\
Display hint for showing end of locked region or processing complete.

\(fn)" nil nil)

(autoload (quote pg-next-error-hint) "pg-user" "\
Display hint for locating error.

\(fn)" nil nil)

(autoload (quote pg-hint) "pg-user" "\
Display a hint HINTMSG in the minibuffer, if `pg-show-hints' is non-nil.
The function `substitute-command-keys' is called on the argument.

\(fn HINTMSG)" nil nil)

(autoload (quote proof-imenu-enable) "pg-user" "\
Add or remove index menu.

\(fn)" nil nil)

(autoload (quote pg-previous-matching-input-from-input) "pg-user" "\
Search backwards through input history for match for current input.
\(Previous history elements are earlier commands.)
With prefix argument N, search for Nth previous match.
If N is negative, search forwards for the -Nth following match.

\(fn N)" t nil)

(autoload (quote pg-next-matching-input-from-input) "pg-user" "\
Search forwards through input history for match for current input.
\(Following history elements are more recent commands.)
With prefix argument N, search for Nth following match.
If N is negative, search backwards for the -Nth previous match.

\(fn N)" t nil)

(autoload (quote pg-add-to-input-history) "pg-user" "\
Maybe add CMD to the input history.
CMD is only added to the input history if it is not a duplicate
of the last item added.

\(fn CMD)" nil nil)

(autoload (quote pg-remove-from-input-history) "pg-user" "\
Maybe remove CMD from the end of the input history.
This is called when the command is undone.  It's only
removed if it matches the last item in the ring.

\(fn CMD)" nil nil)

(autoload (quote pg-clear-input-ring) "pg-user" "\
Not documented

\(fn)" nil nil)

;;;***

;;;### (autoloads (pg-xml-parse-string) "pg-xml" "pg-xml.el" (19109
;;;;;;  19687))
;;; Generated autoloads from pg-xml.el

(autoload (quote pg-xml-parse-string) "pg-xml" "\
Parse string in ARG, same as pg-xml-parse-buffer.

\(fn ARG)" nil nil)

;;;***

;;;### (autoloads (proof-dependency-in-span-context-menu proof-depends-process-dependencies)
;;;;;;  "proof-depends" "proof-depends.el" (19109 19687))
;;; Generated autoloads from proof-depends.el

(autoload (quote proof-depends-process-dependencies) "proof-depends" "\
Process dependencies reported by prover, for NAME in span GSPAN.
Called from `proof-done-advancing' when a save is processed and
`proof-last-theorem-dependencies' is set.

\(fn NAME GSPAN)" nil nil)

(autoload (quote proof-dependency-in-span-context-menu) "proof-depends" "\
Make a portion of a context-sensitive menu showing proof dependencies.

\(fn SPAN)" nil nil)

;;;***

;;;### (autoloads (proof-easy-config) "proof-easy-config" "proof-easy-config.el"
;;;;;;  (19108 51621))
;;; Generated autoloads from proof-easy-config.el

(autoload (quote proof-easy-config) "proof-easy-config" "\
Configure Proof General for proof-assistant using BODY as a setq body.
The symbol SYM and string name NAME must match those given in
the `proof-assistant-table', which see.

\(fn SYM NAME &rest BODY)" nil (quote macro))

;;;***

;;;### (autoloads (proof-indent-line) "proof-indent" "proof-indent.el"
;;;;;;  (19108 51621))
;;; Generated autoloads from proof-indent.el

(autoload (quote proof-indent-line) "proof-indent" "\
Indent current line of proof script, if indentation enabled.

\(fn)" t nil)

;;;***

;;;### (autoloads (proof-maths-menu-enable proof-maths-menu-set-global)
;;;;;;  "proof-maths-menu" "proof-maths-menu.el" (19122 39720))
;;; Generated autoloads from proof-maths-menu.el

(autoload 'proof-maths-menu-set-global "proof-maths-menu" "\
Set global status of maths-menu mode for PG buffers to be FLAG.
Turn on/off menu in all script buffers and ensure new buffers follow suit.

\(fn FLAG)" nil nil)

(autoload 'proof-maths-menu-enable "proof-maths-menu" "\
Turn on or off maths-menu mode in Proof General script buffer.
This invokes `maths-menu-mode' to toggle the setting for the current
buffer, and then sets PG's option for default to match.
Also we arrange to have maths menu mode turn itself on automatically
in future if we have just activated it for this buffer.

\(fn)" t nil)

;;;***

;;;### (autoloads (proof-aux-menu proof-menu-define-specific proof-menu-define-main
;;;;;;  proof-menu-define-keys) "proof-menu" "proof-menu.el" (19127
;;;;;;  27638))
;;; Generated autoloads from proof-menu.el

(autoload (quote proof-menu-define-keys) "proof-menu" "\
Prover specific keymap under C-c C-a.

\(fn MAP)" nil nil)

(autoload (quote proof-menu-define-main) "proof-menu" "\
Not documented

\(fn)" nil nil)

(autoload (quote proof-menu-define-specific) "proof-menu" "\
Not documented

\(fn)" nil nil)

(autoload (quote proof-aux-menu) "proof-menu" "\
Construct and return PG auxiliary menu used in non-scripting buffers.

\(fn)" nil nil)

;;;***

;;;### (autoloads (proof-mmm-enable proof-mmm-set-global) "proof-mmm"
;;;;;;  "proof-mmm.el" (19121 59722))
;;; Generated autoloads from proof-mmm.el

(autoload 'proof-mmm-set-global "proof-mmm" "\
Set global status of MMM mode for PG buffers to be FLAG.

\(fn FLAG)" nil nil)

(autoload 'proof-mmm-enable "proof-mmm" "\
Turn on or off MMM mode in Proof General script buffer.
This invokes `mmm-mode' to toggle the setting for the current
buffer, and then sets PG's option for default to match.
Also we arrange to have MMM mode turn itself on automatically
in future if we have just activated it for this buffer.

\(fn)" t nil)

;;;***

;;;### (autoloads (proof-config-done proof-mode proof-insert-sendback-command
;;;;;;  proof-insert-pbp-command proof-register-possibly-new-processed-file
;;;;;;  pg-set-span-helphighlights proof-locked-region-empty-p proof-locked-region-full-p
;;;;;;  proof-unprocessed-begin proof-unprocessed-begin proof-colour-locked)
;;;;;;  "proof-script" "proof-script.el" (19127 27615))
;;; Generated autoloads from proof-script.el

(autoload (quote proof-colour-locked) "proof-script" "\
Alter the colour of the locked region according to variable `proof-colour-locked'.

\(fn)" t nil)

(autoload (quote proof-unprocessed-begin) "proof-script" "\
Return end of locked region in current buffer or (point-min) otherwise.
The position is actually one beyond the last locked character.

\(fn)" nil nil)

(autoload (quote proof-unprocessed-begin) "proof-script" "\
Return end of the locked region of the current buffer.
Only call this from a scripting buffer.

\(fn)" nil nil)

(autoload (quote proof-locked-region-full-p) "proof-script" "\
Non-nil if the locked region covers all the buffer's non-whitespace.
Works on any buffer.

\(fn)" nil nil)

(autoload (quote proof-locked-region-empty-p) "proof-script" "\
Non-nil if the locked region is empty.  Works on any buffer.

\(fn)" nil nil)

(autoload (quote pg-set-span-helphighlights) "proof-script" "\
Add a daughter help span for SPAN with help message, highlight, actions.
We add the last output (which should be non-empty) to the hover display here.
Optional argument MOUSEFACE means use the given face as a mouse highlight
face, if it is a face, otherwise, if it is non-nil but not a face,
do not add a mouse highlight.
which case no mouse hover face is added.
Argument FACE means add regular face property FACE to the span.

\(fn SPAN &optional MOUSEFACE FACE)" nil nil)

(autoload (quote proof-register-possibly-new-processed-file) "proof-script" "\
Register a possibly new FILE as having been processed by the prover.

If INFORMPROVER is non-nil, the proof assistant will be told about this,
to co-ordinate with its internal file-management.  (Otherwise we assume
that it is a message from the proof assistant which triggers this call).
In this case, the user will be queried to save some buffers, unless
NOQUESTIONS is non-nil.

No action is taken if the file is already registered.

A warning message is issued if the register request came from the
proof assistant and Emacs has a modified buffer visiting the file.

\(fn FILE &optional INFORMPROVER NOQUESTIONS)" nil nil)

(autoload (quote proof-insert-pbp-command) "proof-script" "\
Insert CMD into the proof queue.

\(fn CMD)" nil nil)

(autoload (quote proof-insert-sendback-command) "proof-script" "\
Insert CMD into the proof script, execute assert-until-point.

\(fn CMD)" nil nil)

(autoload (quote proof-mode) "proof-script" "\
Proof General major mode class for proof scripts.
\\{proof-mode-map}

\(fn)" t nil)

(autoload (quote proof-config-done) "proof-script" "\
Finish setup of Proof General scripting mode.
Call this function in the derived mode for the proof assistant to
finish setup which depends on specific proof assistant configuration.

\(fn)" nil nil)

;;;***

;;;### (autoloads (proof-shell-config-done proof-shell-mode proof-shell-invisible-command-invisible-result
;;;;;;  proof-shell-invisible-cmd-get-result proof-shell-invisible-command
;;;;;;  proof-shell-wait proof-extend-queue proof-start-queue proof-shell-insert
;;;;;;  proof-shell-available-p proof-shell-ready-prover) "proof-shell"
;;;;;;  "proof-shell.el" (19122 39720))
;;; Generated autoloads from proof-shell.el

(autoload 'proof-shell-ready-prover "proof-shell" "\
Make sure the proof assistant is ready for a command.
If QUEUEMODE is set, succeed if the proof shell is busy but
has mode QUEUEMODE, which is a symbol or list of symbols.
Otherwise, if the shell is busy, give an error.
No change to current buffer or point.

\(fn &optional QUEUEMODE)" nil nil)

(defsubst proof-shell-live-buffer nil "\
Return buffer of active proof assistant, or nil if none running." (and proof-shell-buffer (buffer-live-p proof-shell-buffer) (scomint-check-proc proof-shell-buffer)))

(autoload 'proof-shell-available-p "proof-shell" "\
Return non-nil if there is a proof shell active and available.
No error messages.  Useful as menu or toolbar enabler.

\(fn)" nil nil)

(autoload 'proof-shell-insert "proof-shell" "\
Insert STRINGS at the end of the proof shell, call `scomint-send-input'.

The ACTION argument is a symbol which is typically the name of a
callback for when STRING has been processed.

First we call `proof-shell-insert-hook'.  The arguments `action' and
`scriptspan' may be examined by the hook to determine how to modify
the `string' variable (exploiting dynamic scoping) which will be
the command actually sent to the shell.

Note that the hook is not called for the empty (null) string
or a carriage return.

Then we strip STRING of carriage returns before inserting it
and updating `proof-marker' to point to the end of the newly
inserted text.

Do not use this function directly, or output will be lost.  It is only
used in `proof-add-to-queue' when we start processing a queue, and in
`proof-shell-exec-loop', to process the next item.

\(fn STRINGS ACTION &optional SCRIPTSPAN)" nil nil)

(autoload 'proof-start-queue "proof-shell" "\
Begin processing a queue of commands in QUEUEITEMS.
If START is non-nil, START and END are buffer positions in the
active scripting buffer for the queue region.

This function calls `proof-add-to-queue'.

\(fn START END QUEUEITEMS)" nil nil)

(autoload 'proof-extend-queue "proof-shell" "\
Extend the current queue with QUEUEITEMS, queue end END.
To make sense, the commands should correspond to processing actions
for processing a region from (buffer-queue-or-locked-end) to END.
The queue mode is set to 'advancing

\(fn END QUEUEITEMS)" nil nil)

(autoload 'proof-shell-wait "proof-shell" "\
Busy wait for `proof-shell-busy' to become nil.
Needed between sequences of commands to maintain synchronization,
because Proof General does not allow for the action list to be extended
in some cases.   May be called by `proof-shell-invisible-command'.

\(fn)" nil nil)

(autoload 'proof-shell-invisible-command "proof-shell" "\
Send CMD to the proof process.
The CMD is `invisible' in the sense that it is not recorded in buffer.
CMD may be a string or a string-yielding expression.

Automatically add proof-terminal-char if necessary, examining
`proof-shell-no-auto-terminate-commands'.

By default, let the command be processed asynchronously.
But if optional WAIT command is non-nil, wait for processing to finish
before and after sending the command.

In case CMD is (or yields) nil, do nothing.

INVISIBLECALLBACK will be invoked after the command has finished,
if it is set.  It should probably run the hook variables
`proof-state-change-hook'.

If NOERROR is set, surpress usual error action.

\(fn CMD &optional WAIT INVISIBLECALLBACK &rest FLAGS)" nil nil)

(autoload 'proof-shell-invisible-cmd-get-result "proof-shell" "\
Execute CMD and return result as a string.
This expects CMD to result in some theorem prover output.
Ordinary output (and error handling) is disabled, and the result
\(contents of `proof-shell-last-output') is returned as a string.

\(fn CMD)" nil nil)

(autoload 'proof-shell-invisible-command-invisible-result "proof-shell" "\
Execute CMD for side effect in the theorem prover, waiting before and after.
Error messages are displayed as usual.

\(fn CMD)" nil nil)

(autoload 'proof-shell-mode "proof-shell" "\
Proof General shell mode class for proof assistant processes

\(fn)" t nil)

(autoload 'proof-shell-config-done "proof-shell" "\
Initialise the specific prover after the child has been configured.
Every derived shell mode should call this function at the end of
processing.

\(fn)" nil nil)

;;;***

;;;### (autoloads (proof-ready-for-assistant) "proof-site" "proof-site.el"
;;;;;;  (19121 59722))
;;; Generated autoloads from proof-site.el

(autoload 'proof-ready-for-assistant "proof-site" "\
Configure PG for symbol ASSISTANTSYM, name ASSISTANT-NAME.
If ASSISTANT-NAME is omitted, look up in `proof-assistant-table'.

\(fn ASSISTANTSYM &optional ASSISTANT-NAME)" nil nil)

;;;***

;;;### (autoloads (proof-splash-message proof-splash-display-screen)
;;;;;;  "proof-splash" "proof-splash.el" (19109 19687))
;;; Generated autoloads from proof-splash.el

(autoload (quote proof-splash-display-screen) "proof-splash" "\
Save window config and display Proof General splash screen.
If TIMEOUT is non-nil, arrange for a time-out to occur outside this function.

\(fn &optional TIMEOUT)" t nil)

(autoload (quote proof-splash-message) "proof-splash" "\
Make sure the user gets welcomed one way or another.

\(fn)" t nil)

;;;***

;;;### (autoloads (proof-splice-separator proof-format) "proof-syntax"
;;;;;;  "proof-syntax.el" (19113 19458))
;;; Generated autoloads from proof-syntax.el

(autoload 'proof-format "proof-syntax" "\
Format a string by matching regexps in ALIST against STRING.
ALIST contains (REGEXP . REPLACEMENT) pairs where REPLACEMENT
may be a string or sexp evaluated to get a string.

\(fn ALIST STRING)" nil nil)

(autoload 'proof-splice-separator "proof-syntax" "\
Splice SEP into list of STRINGS, ignoring nil entries.

\(fn SEP STRINGS)" nil nil)

;;;***

;;;### (autoloads (proof-toolbar-scripting-menu proof-toolbar-setup)
;;;;;;  "proof-toolbar" "proof-toolbar.el" (19121 59722))
;;; Generated autoloads from proof-toolbar.el

(autoload 'proof-toolbar-setup "proof-toolbar" "\
Initialize Proof General toolbar and enable it for all PG buffers.
If `proof-toolbar-enable' is nil, change the buffer toolbars
back the default toolbar.

\(fn)" t nil)
 (autoload 'proof-toolbar-toggle "proof-toolbar")

(autoload 'proof-toolbar-scripting-menu "proof-toolbar" "\
Menu made from the Proof General toolbar commands.

\(fn)" nil nil)

;;;***

;;;### (autoloads (proof-unicode-tokens-enable proof-unicode-tokens-set-global
;;;;;;  proof-unicode-tokens-mode-if-enabled) "proof-unicode-tokens"
;;;;;;  "proof-unicode-tokens.el" (19126 41475))
;;; Generated autoloads from proof-unicode-tokens.el

(autoload (quote proof-unicode-tokens-mode-if-enabled) "proof-unicode-tokens" "\
Turn on or off the Unicode Tokens minor mode in this buffer.

\(fn)" nil nil)

(autoload (quote proof-unicode-tokens-set-global) "proof-unicode-tokens" "\
Set global status of unicode tokens mode for PG buffers to be FLAG.
Turn on/off menu in all script buffers and ensure new buffers follow suit.

\(fn FLAG)" nil nil)

(autoload (quote proof-unicode-tokens-enable) "proof-unicode-tokens" "\
Turn on or off Unicode tokens mode in Proof General script buffer.
This invokes `unicode-tokens-mode' to toggle the setting for the current
buffer, and then sets PG's option for default to match.
Also we arrange to have unicode tokens mode turn itself on automatically
in future if we have just activated it for this buffer.
Note: this function is called when the customize setting for the prover
is changed.

\(fn)" t nil)

;;;***

;;;### (autoloads (defpacustom proof-defpacustom-fn) "proof-utils"
;;;;;;  "proof-utils.el" (19122 39720))
;;; Generated autoloads from proof-utils.el

(autoload 'proof-defpacustom-fn "proof-utils" "\
As for macro `defpacustom' but evaluating arguments.

\(fn NAME VAL ARGS)" nil nil)

(autoload 'defpacustom "proof-utils" "\
Define a setting NAME for the current proof assitant, default VAL.
NAME can correspond to some internal setting, flag, etc, for the
proof assistant, in which case a :setting and :type value should be provided.
The :type of NAME should be one of 'integer, 'boolean, 'string.
The customization variable is automatically in group `proof-assistant-setting'.
The function `proof-assistant-format' is used to format VAL.
If NAME corresponds instead to a PG internal setting, then a form :eval to
evaluate can be provided instead.

\(fn NAME VAL &rest ARGS)" nil (quote macro))

;;;***

;;;### (autoloads (scomint-make scomint-make-in-buffer) "scomint"
;;;;;;  "../lib/scomint.el" (19126 40592))
;;; Generated autoloads from ../lib/scomint.el

(autoload (quote scomint-make-in-buffer) "scomint" "\
Make a Comint process NAME in BUFFER, running PROGRAM.
If BUFFER is nil, it defaults to NAME surrounded by `*'s.
PROGRAM should be either a string denoting an executable program to create
via `start-file-process', or a cons pair of the form (HOST . SERVICE) denoting
a TCP connection to be opened via `open-network-stream'.  If there is already
a running process in that buffer, it is not restarted.  Optional fourth arg
STARTFILE is the name of a file to send the contents of to the process.

If PROGRAM is a string, any more args are arguments to PROGRAM.

\(fn NAME BUFFER PROGRAM &optional STARTFILE &rest SWITCHES)" nil nil)

(autoload (quote scomint-make) "scomint" "\
Make a Comint process NAME in a buffer, running PROGRAM.
The name of the buffer is made by surrounding NAME with `*'s.
PROGRAM should be either a string denoting an executable program to create
via `start-file-process', or a cons pair of the form (HOST . SERVICE) denoting
a TCP connection to be opened via `open-network-stream'.  If there is already
a running process in that buffer, it is not restarted.  Optional third arg
STARTFILE is the name of a file to send the contents of the process to.

If PROGRAM is a string, any more args are arguments to PROGRAM.

\(fn NAME PROGRAM &optional STARTFILE &rest SWITCHES)" nil nil)

;;;***

;;;### (autoloads (texi-docstring-magic) "texi-docstring-magic" "../lib/texi-docstring-magic.el"
;;;;;;  (19107 62790))
;;; Generated autoloads from ../lib/texi-docstring-magic.el

(autoload (quote texi-docstring-magic) "texi-docstring-magic" "\
Update all texi docstring magic annotations in buffer.
With prefix arg, no errors on unknown symbols.  (This results in
@def .. @end being deleted if not known).

\(fn &optional NOERROR)" t nil)

;;;***

;;;### (autoloads (unicode-chars-list-chars) "unicode-chars" "../lib/unicode-chars.el"
;;;;;;  (19107 62795))
;;; Generated autoloads from ../lib/unicode-chars.el

(autoload (quote unicode-chars-list-chars) "unicode-chars" "\
Insert each Unicode character into a buffer.
Lets you see which characters are available for literal display
in your emacs font.

\(fn)" t nil)

;;;***

;;;### (autoloads (unicode-tokens-encode-str) "unicode-tokens" "../lib/unicode-tokens.el"
;;;;;;  (19127 27485))
;;; Generated autoloads from ../lib/unicode-tokens.el

(autoload (quote unicode-tokens-encode-str) "unicode-tokens" "\
Return a unicode encoded version presentation of STR.

\(fn STR)" nil nil)

;;;***

;;;### (autoloads nil nil ("../lib/local-vars-list.el" "../lib/pg-dev.el"
;;;;;;  "../lib/pg-fontsets.el" "../lib/proof-compat.el" "../lib/ps-fix.el"
;;;;;;  "../lib/span.el" "pg-autotest.el" "pg-custom.el" "pg-pbrpm.el"
;;;;;;  "pg-vars.el" "proof-auxmodes.el" "proof-config.el" "proof-faces.el"
;;;;;;  "proof-useropts.el" "proof.el") (19127 27664 537600))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:

;;; proof-autoloads.el ends here
