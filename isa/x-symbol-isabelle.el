;;  ID:         $Id$
;;  Author:     David von Oheimb
;;  Copyright   1998 Technische Universitaet Muenchen
;;  token language "Isabelle Symbols" for package x-symbol
;;
;; NB: Part of Proof General distribution.
;;

(defvar x-symbol-isabelle-required-fonts nil)

;; FIXME da: these next two are also set in proof-x-symbol.el, but
;; it's handy to use this file away from PG.  In future could
;; fix things so just (require 'proof-x-symbol) would be enough
;; here.
(defvar x-symbol-isabelle-name "Isabelle Symbol")
(defvar x-symbol-isabelle-modeline-name "isa")

(defvar x-symbol-isabelle-header-groups-alist nil)
;'(("Operator" bigop operator)
;    ("Relation" relation)
;    ("Arrow, Punctuation" arrow triangle shape
;     white line dots punctuation quote parenthesis)
;    ("Symbol" symbol currency mathletter setsymbol)
;    ("Greek Letter" greek greek1)
;    ("Acute, Grave" acute grave))
;  "*If non-nil, used in isasym specific grid/menu."

(defvar x-symbol-isabelle-class-alist
  '((VALID "Isabelle Symbol" (x-symbol-info-face))
    (INVALID "no Isabelle Symbol" (red x-symbol-info-face))))
(defvar x-symbol-isabelle-class-face-alist nil)
(defvar x-symbol-isabelle-electric-ignore "[:'][A-Za-z]\\|<=")

(defvar x-symbol-isabelle-font-lock-keywords nil)
(defvar x-symbol-isabelle-master-directory  'ignore)
(defvar x-symbol-isabelle-image-searchpath '("./"))
(defvar x-symbol-isabelle-image-cached-dirs '("images/" "pictures/"))
(defvar x-symbol-isabelle-image-file-truename-alist nil)
(defvar x-symbol-isabelle-image-keywords nil)

(defvar x-symbol-isabelle-case-insensitive nil)
;(defvar x-symbol-isabelle-token-shape '(?\\ "\\\\\\<[A-Za-z][A-Za-z0-9_']*>\\a'" . "[A-Za-z]"))
(defvar x-symbol-isabelle-token-shape nil)

(defvar x-symbol-isabelle-exec-specs '(nil ("\\`\\\\<[A-Za-z][A-Za-z0-9_']*>\\'" .
                                          "\\\\<[A-Za-z][A-Za-z0-9_']*>")))

(defvar x-symbol-isabelle-input-token-ignore nil)
(defun x-symbol-isabelle-default-token-list (tokens) tokens)


(defvar x-symbol-isabelle-token-list 'x-symbol-isabelle-default-token-list)

(defvar x-symbol-isabelle-symbol-table      ; symbols (isabelle font)
  '((visiblespace "\\<spacespace>")
    (Gamma "\\<Gamma>")
    (Delta "\\<Delta>")
    (Theta "\\<Theta>")
    (Lambda "\\<Lambda>")
    (Pi "\\<Pi>")
    (Sigma "\\<Sigma>")
    (Phi "\\<Phi>")
    (Psi "\\<Psi>")
    (Omega "\\<Omega>")
    (alpha "\\<alpha>")
    (beta "\\<beta>")
    (gamma "\\<gamma>")
    (delta "\\<delta>")
    (epsilon1 "\\<epsilon>")
    (zeta "\\<zeta>")
    (eta "\\<eta>")
    (theta1 "\\<theta>")
    (kappa1 "\\<kappa>")
    (lambda "\\<lambda>")
    (mu "\\<mu>")
    (nu "\\<nu>")
    (xi "\\<xi>")
    (pi "\\<pi>")
    (rho "\\<rho>")
    (sigma "\\<sigma>")
    (tau "\\<tau>")
    (phi1 "\\<phi>")
    (chi "\\<chi>")
    (psi "\\<psi>")
    (omega "\\<omega>")
    (notsign "\\<not>")
    (logicaland "\\<and>")
    (logicalor "\\<or>")
    (universal1 "\\<forall>")
    (existential1 "\\<exists>")
    (biglogicaland "\\<And>")
    (ceilingleft "\\<lceil>")
    (ceilingright "\\<rceil>")
    (floorleft "\\<lfloor>")
    (floorright "\\<rfloor>")
    (bardash "\\<turnstile>")
    (bardashdbl "\\<Turnstile>")
    (semanticsleft "\\<lbrakk>")
    (semanticsright "\\<rbrakk>")
    (periodcentered "\\<cdot>")
    (element "\\<in>")
    (reflexsubset "\\<subseteq>")
    (intersection "\\<inter>")
    (union "\\<union>")
    (bigintersection "\\<Inter>")
    (bigunion "\\<Union>")
    (sqintersection "\\<sqinter>")
    (squnion "\\<squnion>")
    (bigsqintersection "\\<Sqinter>")
    (bigsqunion "\\<Squnion>")
    (perpendicular "\\<bottom>")
    (dotequal "\\<doteq>")
    (equivalence "\\<equiv>")
    (notequal "\\<noteq>")
    (propersqsubset "\\<sqsubset>")
    (reflexsqsubset "\\<sqsubseteq>")
    (properprec "\\<prec>")
    (reflexprec "\\<preceq>")
    (propersucc "\\<succ>")
    (approxequal "\\<approx>")
    (similar "\\<sim>")
    (simequal "\\<simeq>")
    (lessequal "\\<le>")
    (coloncolon "\\<Colon>")
    (arrowleft "\\<leftarrow>")
    (endash "\\<midarrow>")
    (arrowright "\\<rightarrow>")
    (arrowdblleft "\\<Leftarrow>")
;   (rightleftharpoons "\\<Midarrow>") ;missing symbol (but not necessary)
    (arrowdblright "\\<Rightarrow>")
    (frown "\\<bow>")
    (mapsto "\\<mapsto>")
    (leadsto "\\<leadsto>")
    (arrowup "\\<up>")
    (arrowdown "\\<down>")
    (notelement "\\<notin>")
    (multiply "\\<times>")
    (circleplus "\\<oplus>")
    (circleminus "\\<ominus>")
    (circlemultiply "\\<otimes>")
    (circleslash "\\<oslash>")
    (propersubset "\\<subset>")
    (infinity "\\<infinity>")
    (box "\\<box>")
    (lozenge1 "\\<diamond>")
    (circ "\\<circ>")
    (bullet "\\<bullet>")
    (bardbl "\\<parallel>")
    (radical "\\<surd>")
    (copyright "\\<copyright>")))

(defvar x-symbol-isabelle-xsymbol-table    ; xsymbols
  '((plusminus "\\<plusminus>")
    (division "\\<div>")
    (longarrowright "\\<longrightarrow>")
    (longarrowleft "\\<longleftarrow>")
    (longarrowboth "\\<longleftrightarrow>")
    (longarrowdblright "\\<Longrightarrow>")
    (longarrowdblleft "\\<Longleftarrow>")
    (longarrowdblboth "\\<Longleftrightarrow>")
    (brokenbar "\\<brokenbar>")
    (hyphen "\\<hyphen>")
    (macron "\\<macron>")
    (exclamdown "\\<exclamdown>")
    (questiondown "\\<questiondown>")
    (guillemotleft "\\<guillemotleft>")
    (guillemotright "\\<guillemotright>")
    (degree "\\<degree>")
    (onesuperior "\\<onesuperior>")
    (onequarter "\\<onequarter>")
    (twosuperior "\\<twosuperior>")
    (onehalf "\\<onehalf>")
    (threesuperior "\\<threesuperior>")
    (threequarters "\\<threequarters>")
    (paragraph "\\<paragraph>")
    (registered "\\<registered>")
    (ordfeminine "\\<ordfeminine>")
    (ordmasculine "\\<ordmasculine>")
    (section "\\<section>")
    (pounds "\\<pounds>")
    (yen "\\<yen>")
    (cent "\\<cent>")
    (currency "\\<currency>")
    (braceleft2 "\\<lbrace>")
    (braceright2 "\\<rbrace>")
    (top "\\<top>")))

(defvar x-symbol-isabelle-user-table nil)

(defun x-symbol-isabelle-prepare-table (table)
  (let*
      ((is-isar (eq proof-assistant-symbol 'isar))
       (prfx1 (if is-isar "" "\\"))
       (prfx2 (if is-isar "\\" "")))
    (mapcar (lambda (entry)
              (list (car entry) '() (concat prfx1 (cadr entry)) (concat prfx2 (cadr entry))))
            table)))

(defvar x-symbol-isabelle-table
  (x-symbol-isabelle-prepare-table
   (append
    x-symbol-isabelle-user-table
    x-symbol-isabelle-symbol-table
    x-symbol-isabelle-xsymbol-table)))

;;;===========================================================================
;;;  Internal
;;;===========================================================================

(defvar x-symbol-isabelle-menu-alist nil
  "Internal.  Alist used for Isasym specific menu.")
(defvar x-symbol-isabelle-grid-alist nil
  "Internal.  Alist used for Isasym specific grid.")

(defvar x-symbol-isabelle-decode-atree nil
  "Internal.  Atree used by `x-symbol-token-input'.")
(defvar x-symbol-isabelle-decode-alist nil
  "Internal.  Alist used for decoding of Isasym macros.")
(defvar x-symbol-isabelle-encode-alist nil
  "Internal.  Alist used for encoding to Isasym macros.")

(defvar x-symbol-isabelle-nomule-decode-exec nil
  "Internal.  File name of Isasym decode executable.")
(defvar x-symbol-isabelle-nomule-encode-exec nil
  "Internal.  File name of Isasym encode executable.")



;;;===========================================================================
;;;  useful key bindings
;;;===========================================================================


;; FIXME: these break some standard bindings, and should only be set
;; for proof shell, script (or minibuffer) modes.

;(global-set-key [(meta l)] 'x-symbol-INSERT-lambda)

;(global-set-key [(meta n)] 'x-symbol-INSERT-notsign)
;(global-set-key [(meta a)] 'x-symbol-INSERT-logicaland)
;(global-set-key [(meta o)] 'x-symbol-INSERT-logicalor)
;(global-set-key [(meta f)] 'x-symbol-INSERT-universal1)
;(global-set-key [(meta t)] 'x-symbol-INSERT-existential1)

;(global-set-key [(meta A)] 'x-symbol-INSERT-biglogicaland)
;(global-set-key [(meta e)] 'x-symbol-INSERT-equivalence)
;(global-set-key [(meta u)] 'x-symbol-INSERT-notequal)
;(global-set-key [(meta m)] 'x-symbol-INSERT-arrowdblright)

;(global-set-key [(meta i)] 'x-symbol-INSERT-longarrowright)

(provide 'x-symbol-isabelle)
