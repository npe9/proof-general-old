;;; This file implements spans in terms of overlays, for emacs19.
;;; Copyright (C) 1998 LFCS Edinburgh
;;; Author: Healfdene Goguen

;; Maintainer: LEGO Team <lego@dcs.ed.ac.uk>

;; $Log$
;; Revision 2.0  1998/08/11 15:00:13  da
;; New branch
;;
;; Revision 1.9  1998/06/10 14:02:39  hhg
;; Wrote generic span functions for making spans read-only or read-write.
;; Fixed bug in add-span and remove-span concerning return value of
;; span-traverse.
;;
;; Revision 1.8  1998/06/10 12:41:47  hhg
;; Compare span-end first rather than span-start in span-lt, because
;; proof-lock-span is often changed and has starting point 1.
;; Factored out common code of add-span and remove-span into
;; span-traverse.
;;
;; Revision 1.7  1998/06/03 17:40:07  hhg
;; Changed last-span to before-list.
;; Added definitions of foldr and foldl if they aren't already loaded.
;; Changed definitions of add-span, remove-span and find-span-aux to be
;; non-recursive.
;; Removed detach-extent since this file isn't used by xemacs.
;; Added function append-unique to avoid repetitions in list generated by
;; spans-at-region.
;; Changed next-span so it uses member-if.
;;
;; Revision 1.6  1998/06/02 15:36:51  hhg
;; Corrected comment about this being for emacs19.
;;
;; Revision 1.5  1998/05/29 09:50:10  tms
;; o outsourced indentation to proof-indent
;; o support indentation of commands
;; o replaced test of Emacs version with availability test of specific
;;   features
;; o C-c C-c, C-c C-v and M-tab is now available in all buffers
;;
;; Revision 1.4  1998/05/21 17:27:41  hhg
;; Removed uninitialized os variable in spans-at-region.
;;
;; Revision 1.3  1998/05/21 08:28:52  hhg
;; Initialize 'before pointer in add-span-aux when last-span is nil.
;; Removed span-at-type.
;; Fixed bug in span-at-before, where (span-start span) may be nil.
;; Added spans-at-(point|region)[-prop], which fixes bug of C-c u at end
;; of buffer.
;;
;; Revision 1.2  1998/05/19 15:31:37  hhg
;; Added header and log message.
;; Fixed set-span-endpoints so it preserves invariant.
;; Changed add-span and remove-span so that they update last-span
;; correctly themselves, and don't take last-span as an argument.
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;               Bridging the emacs19/xemacs gulf                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; before-list represents a linked list of spans for each buffer.
;; It has the invariants of:
;; * being ordered wrt the starting point of the spans in the list,
;;   with detached spans at the end.
;; * not having overlapping overlays of the same type.

(defvar before-list nil
  "Start of backwards-linked list of spans")

(make-variable-buffer-local 'before-list)


(or (fboundp 'foldr)
(defun foldr (func a seq)
  "Return (func (func (func (... (func a Sn) ...) S2) S1) S0)
when func's argument is 2 and seq is a sequence whose
elements = S0 S1 S2 ... Sn. [tl-seq.el]"
  (let ((i (length seq)))
    (while (> i 0)
      (setq i (1- i))
      (setq a (funcall func a (elt seq i)))
      )
    a)))

(or (fboundp 'foldl)
(defun foldl (func a seq)
  "Return (... (func (func (func a S0) S1) S2) ...)
when func's argument is 2 and seq is a sequence whose
elements = S0 S1 S2 .... [tl-seq.el]"
  (let ((len (length seq))
        (i 0))
    (while (< i len)
      (setq a (funcall func a (elt seq i)))
      (setq i (1+ i))
      )
    a)))

(defsubst span-start (span)
  (overlay-start span))

(defsubst span-end (span)
  (overlay-end span))

(defun set-span-property (span name value)
  (overlay-put span name value))

(defsubst span-property (span name)
  (overlay-get span name))

(defun span-read-only-hook (overlay after start end &optional len)
  (error "Region is read-only"))

(defun span-read-only (span)
  (set-span-property span 'modification-hooks '(span-read-only-hook))
  (set-span-property span 'insert-in-front-hooks '(span-read-only-hook)))

(defun span-read-write (span)
  (set-span-property span 'modification-hooks nil)
  (set-span-property span 'insert-in-front-hooks nil))

(defun int-nil-lt (m n)
  (cond
   ((eq m n) nil)
   ((not n) t)
   ((not m) nil)
   (t (< m n))))

;; We use end first because proof-locked-queue is often changed, and
;; its starting point is always 1
(defun span-lt (s u)
  (or (int-nil-lt (span-end s) (span-end u))
      (and (eq (span-end s) (span-end u))
	   (int-nil-lt (span-start s) (span-start u)))))

(defun span-traverse (span prop)
  (cond
   ((not before-list)
    ;; before-list empty
    'empty)
   ((funcall prop before-list span)
    ;; property holds for before-list and span
    'hd)
   (t
    ;; traverse before-list for property
    (let ((l before-list) (before (span-property before-list 'before)))
      (while (and before (not (funcall prop before span)))
	(setq l before)
	(setq before (span-property before 'before)))
      l))))

(defun add-span (span)
  (let ((ans (span-traverse span 'span-lt)))
    (cond
     ((eq ans 'empty)
      (set-span-property span 'before nil)
      (setq before-list span))
     ((eq ans 'hd)
      (set-span-property span 'before before-list)
      (setq before-list span))
     (t
      (set-span-property span 'before
			 (span-property ans 'before))
      (set-span-property ans 'before span)))))

(defun make-span (start end)
  (add-span (make-overlay start end)))

(defun remove-span (span)
  (let ((ans (span-traverse span 'eq)))
    (cond
     ((eq ans 'empty)
      (error "Bug: empty span list"))
     ((eq ans 'hd)
      (setq before-list (span-property before-list 'before)))
     (ans
      (set-span-property ans 'before (span-property span 'before)))
     (t (error "Bug: span does not occur in span list")))))

;; extent-at gives "smallest" extent at pos
;; we're assuming right now that spans don't overlap
(defun spans-at-point (pt)
  (let ((overlays nil) (os nil))
    (setq os (overlays-at pt))
    (while os
      (if (not (memq (car os) overlays))
	  (setq overlays (cons (car os) overlays)))
      (setq os (cdr os)))
    overlays))

;; assumes that there are no repetitions in l or m
(defun append-unique (l m)
  (foldl (lambda (n a) (if (memq a m) n (cons a n))) m l))

(defun spans-at-region (start end)
  (let ((overlays nil) (pos start))
    (while (< pos end)
      (setq overlays (append-unique (spans-at-point pos) overlays))
      (setq pos (next-overlay-change pos)))
    overlays))

(defun spans-at-point-prop (pt prop)
  (let ((f (cond
	    (prop (lambda (spans span)
		    (if (span-property span prop) (cons span spans)
		      spans)))
	    (t (lambda (spans span) (cons span spans))))))
    (foldl f nil (spans-at-point pt))))

(defun spans-at-region-prop (start end prop)
  (let ((f (cond
	    (prop (lambda (spans span)
		    (if (span-property span prop) (cons span spans)
		      spans)))
	    (t (lambda (spans span) (cons span spans))))))
    (foldl f nil (spans-at-region start end))))

(defun span-at (pt prop)
  (car (spans-at-point-prop pt prop)))

(defsubst detach-span (span)
  (remove-span span)
  (delete-overlay span)
  (add-span span))

(defsubst delete-span (span)
  (remove-span span)
  (delete-overlay span))

;; The next two change ordering of list of spans:
(defsubst set-span-endpoints (span start end)
  (remove-span span)
  (move-overlay span start end)
  (add-span span))

(defsubst set-span-start (span value)
  (set-span-endpoints span value (span-end span)))

;; This doesn't affect invariant:
(defsubst set-span-end (span value)
  (set-span-endpoints span (span-start span) value))

(defsubst delete-spans (start end prop)
  (mapcar 'delete-span (spans-at-region-prop start end prop)))

(defun map-spans-aux (f l)
  (cond (l (cons (funcall f l) (map-spans-aux f (span-property l 'before))))
	(t ())))

(defsubst map-spans (f)
  (map-spans-aux f before-list))

(defun find-span-aux (prop-p l)
  (while (and l (not (funcall prop-p l)))
       (setq l (span-property l 'before)))
     l)

(defun find-span (prop-p)
  (find-span-aux prop-p before-list))

(defun span-at-before (pt prop)
  (let ((prop-pt-p
	 (cond (prop (lambda (span)
		       (let ((start (span-start span)))
			 (and start (> pt start)
			    (span-property span prop)))))
	       (t (lambda (span)
		    (let ((start (span-start span)))
		      (and start (> pt start))))))))
    (find-span prop-pt-p)))
  
(defun prev-span (span prop)
  (let ((prev-prop-p
	 (cond (prop (lambda (span) (span-property span prop)))
	       (t (lambda (span) t)))))
    (find-span-aux prev-prop-p (span-property span 'before))))

;; overlays are [start, end)
;; If there are two spans overlapping then this won't work.
(defun next-span (span prop)
  (let ((l (member-if (lambda (span) (span-property span prop))
		       (overlays-at
			(next-overlay-change (overlay-start span))))))
    (if l (car l) nil)))


(provide 'span-overlay)
