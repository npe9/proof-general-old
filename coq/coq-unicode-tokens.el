;;; -*- coding: utf-8; -*-
;; coq-unicode-tokens.el --- Tokens for Unicode Tokens package
;;
;; Copyright(C) 2008 David Aspinall / LFCS Edinburgh
;; Author:    David Aspinall <David.Aspinall@ed.ac.uk>
;;
;; This file is loaded by `proof-unicode-tokens.el'.
;;
;; It sets the variables defined at the top of unicode-tokens.el,
;; unicode-tokens-<foo> is set from coq-<foo>.  See the corresponding
;; variable for documentation.
;;
;; For Coq, there is no dedicated token syntax, you may use plain
;; sequences of characters or use UTF-8 directly with Notation
;; commands (see utf8.v), or even (at risk of confusion!) mix the two.
;; 

;;
;; Controls
;;

(defconst coq-control-region-format-regexp "\\(%s\\)\\(.*?\\)\\(%s\\)")

(defconst coq-control-regions
  '(("Subscript" ",{" "}" sub)
    ("Superscript" "\\^{" "}" sup)))

(defconst coq-control-char-format-regexp "\\(%s\\)\\(\\<.*?\\>\\)")

(defconst coq-control-characters
  '(("Subscript" "__" sub) 
    ("Superscript" "\\^\\^" sup)))

(defcustom coq-fontsymb-properties 
  '((sub      (display (raise -0.4) (display (height -1))))
    (sup      (display (raise 0.4) (height -1))))
  "Mapping from symbols to property lists used for markup scheme."
  :set 'proof-set-value)


;;
;; Symbols
;;

(defconst coq-token-format "%s")

(defconst coq-greek-letters-tokens
  '(("alpha" "α")
    ("beta" "β")
    ("gamma" "γ")
    ("delta" "δ")
    ("epsilon" "ε") ; actually varepsilon (some is epsilon)
    ("zeta" "ζ")
    ("eta" "η")
    ("theta" "θ")
    ("iota" "ι")
    ("kappa" "κ")
    ("lambda" "λ")
    ("mu" "μ")
    ("nu" "ν")
    ("xi" "ξ")
    ("pi" "π")
    ("rho" "ρ")
    ("sigma" "σ")
    ("tau" "τ")
    ("upsilon" "υ")
    ("phi" "ϕ")
    ("chi" "χ")
    ("psi" "ψ")
    ("omega" "ω")
    ("Gamma" "Γ")
    ("Delta" "Δ")
    ("Theta" "Θ")
    ("Lambda" "Λ")
    ("Xi" "Ξ")
    ("Pi" "Π")
    ("Sigma" "Σ")
    ("Upsilon" "Υ")
    ("Phi" "Φ")
    ("Psi" "Ψ")
    ("Omega" "Ω")))

(defconst coq-misc-letters-tokens
  '(("bool" "IB")
    ("complex" "ℂ")
    ("nat" "ℕ")
    ("rat" "ℚ")
    ("real" "ℝ")
    ("int" "ℤ")
    ;; these ones?
    ;; "H1"  "H2" "H3" ?
    ))

(defconst coq-symbol-tokens
  '(("~" "¬")
    ("/\\"  "∧")
    ("\\/"  "∨")
    ("forall" "∀")
    ("exists" "∃")
    (":=" "≔")
    ("<-" "←")
    ("->" "→")
    ("=>" "⇒")
    ("<=" "≤")
    (">=" "≥")
    ("<->" "≡")
    ("<>" "≠")
    ("fun" "λ")
    ("|-" )
    ("|=" )
    
    ))
;; Others
;;
;; ++>
;; ==>
;; -->

(defcustom coq-token-symbol-map
  (append
   coq-symbol-tokens
   coq-misc-letters-tokens
   coq-greek-letters-tokens)
  ;; an alist of token name, unicode char sequence
  "Table mapping Coq tokens to Unicode strings.

You can adjust this table to add entries, or to change entries for
glyphs that not are available in your Emacs or chosen font.

When a file is visited, tokens are replaced by the strings
in this table.  When the file is saved, the reverse is done.
The string mapping can be anything, but should be such that
tokens can be uniquely recovered from a decoded text; otherwise
results will be undefined when files are saved."
  :type '(repeat (cons (string :tag "Token name")
		       (string :tag "Unicode string")))
  :set 'proof-set-value
  :group 'coq
  :tag "Coq Unicode Token Mapping")


;; TODO: maybe give user a choice between storing Unicode in file and
;; not.  In first case, should not use tokens, but use Unicode.
;; In second case, should use Unicode but not not tokens.
;; At the moment, these shortcuts should *not* overlap with plain tokens!
(defcustom coq-shortcut-alist
  '(; short cut, unicode string
    ("<>" . "≠")
    ("|>" . "⊳")
    ("\\/" . "∨")
    ("/\\" . "∧")
    ("+O" . "⊕")
    ("-O" . "⊖")
    ("xO" . "⊗")
    ("/O" . "⊘")
    (".O" . "⊙")
    ("|+" . "†")
    ("|++" . "‡")
    ("<=" . "≤")
    ("|-" . "⊢")
    (">=" . "≥")
    ("-|" . "⊣")
    ("||" . "∥")
    ("==" . "≡")
    ("~=" . "≃")
    ("~~~" . "≍")
    ("~~" . "≈")
    ("~==" . "≅")
    ("|<>|" . "⋈")
    ("|=" . "⊨")
    ("=." . "≐")
    ("_|_" . "⊥")
    ("</" . "≮")
    (">=/" . "≱")
    ("=/" . "≠")
    ("==/" . "≢")
    ("~/" . "≁")
    ("~=/" . "≄")
    ("~~/" . "≉")
    ("~==/" . "≇")
    ("<-" . "←")
    ("->" . "→")
    ("=>" . "⇒")
    ("<->" . "↔")
    ("<=>" . "⇔")
    ("|->" . "↦")
    ("<--" . "⟵")
    ("<==" . "⟸")
    ("-->" . "⟶")
    ("==>" . "⟹")
    ("<==>" . "⟷")
    ("|-->" . "⟼")
    ("<--" . "←⎯")
    ("<-->" . "⟷")
    ("<<" . "⟪")
    ("[|" . "⟦")
    (">>" . "⟫")
    ("|]" . "⟧")
    ("``" . "”")
    ("''" . "“")
    ("--" . "–")
    ("---" . "—")
    ("''" . "″")
    ("'''" . "‴")
    ("''''" . "⁗")
    (":=" . "≔")
    ;; some word shortcuts, started with backslash otherwise
    ;; too annoying, perhaps.
    ("\\forall" . "∀")
    ("\\exists" . "∃")
    ("\\nat" . "ℕ")
    ("\\int" . "ℤ")
    ("\\rat" . "ℚ")
    ("\\real" . "ℝ")
    ("\\complex" . "ℂ")
    ("\\euro" . "€")
    ("\\yen" . "¥")
    ("\\cent" . "¢"))
  "Shortcut key sequence table for Unicode strings.

You can adjust this table to add more entries, or to change entries for
glyphs that not are available in your Emacs or chosen font.

These shortcuts are only used for input; no reverse conversion is
performed.  But if tokens exist for the target of shortcuts, they
will be used on saving the buffer."
  :type '(repeat (cons (string :tag "Shortcut sequence")
		       (string :tag "Unicode string")))
  :set 'proof-set-value
  :group 'isabelle
  :tag "Coq Unicode Token Mapping")

  




(provide 'coq-unicode-tokens)

;;; coq-unicode-tokens.el ends here
