;;; screamer-slime.el --- SLIME-side support for Screamer's LOCAL-OUTPUT  -*- lexical-binding: t -*-

;; Copyright 2026 Paulo Henrique Raposo.
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to
;; deal in the Software without restriction, including without limitation the
;; rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
;; sell copies of the Software.

;; This file replaces the older iscream.el, which targeted the abandoned
;; ILisp + bridge.el stack. It provides the Emacs-side hooks needed by
;; Screamer's LOCAL-OUTPUT macro under SLIME.
;;
;; Setup:
;;   1. Add the directory containing this file to your `load-path'.
;;   2. (require 'screamer-slime) in your Emacs init.
;;   3. Connect to a Lisp with SLIME, load Screamer.
;;   4. On the Lisp side: (setf screamer:*iscream?* t)
;;
;; How it works:
;;
;; LOCAL-OUTPUT captures the body's *standard-output* and inserts the
;; captured text at the end of the *Screamer Output* buffer. A trail
;; closure registered at LOCAL-OUTPUT entry deletes that text on
;; backtrack. The result: while the search runs, the buffer shows
;; whatever path Screamer is currently exploring, and the text vanishes
;; the moment Screamer backtracks past that point. After the search
;; finishes (whether via ALL-VALUES exhausting or ONE-VALUE returning a
;; result), Screamer unwinds the entire trail to its starting point, so
;; ALL the text inserted during the search is removed. The buffer ends
;; up empty.
;;
;; This means LOCAL-OUTPUT is for LIVE OBSERVATION of the search, not
;; for persistent logging. Open *Screamer Output* in a visible window
;; before running the search and watch the text grow and shrink in real
;; time.
;;
;; With small ranges (e.g. an-integer-between 1 3) the search finishes
;; in fractions of a second; the buffer flickers too fast to see. Use
;; larger ranges to observe the live behaviour:
;;
;;   ;; Open the output buffer in another window first.
;;   (screamer::emacs-eval '(switch-to-buffer-other-window "*Screamer Output*"))
;;   (screamer-slime-clear-output)
;;
;;   ;; Run a search slow enough to watch.
;;   (all-values
;;     (let ((x (an-integer-between 0 100)))
;;       (local-output (format t "VALUE: ~A~%" x))
;;       x))
;;   ;; => (0 1 2 ... 100), and the *Screamer Output* buffer shows each
;;   ;; line appearing and disappearing as the search backtracks.
;;   ;; After the search completes, the buffer is empty again.

(require 'slime)

;; LOCAL-OUTPUT and EMACS-EVAL go through `swank:eval-in-emacs', which
;; SLIME guards behind `slime-enable-evaluate-in-emacs' (NIL by default).
;; Loading this file is itself an explicit opt-in to the integration, so
;; we enable the flag here. Users who do not want global eval-in-emacs
;; should not (require 'screamer-slime) -- the legacy iscream.el path
;; works without this flag (but is otherwise abandoned).
(setq slime-enable-evaluate-in-emacs t)

(defvar screamer-slime-output-buffer-name "*Screamer Output*"
  "Name of the buffer where LOCAL-OUTPUT writes.")

(defvar screamer-slime-backtrack-locations nil
  "Stack of buffer positions saved by `screamer-slime-push-end-marker',
popped (and the region from each marker to point-max deleted) by
`screamer-slime-pop-end-marker' on backtrack.")

(defun screamer-slime-output-buffer ()
  "Return (creating if needed) the *Screamer Output* buffer."
  (get-buffer-create screamer-slime-output-buffer-name))

(defun screamer-slime-push-end-marker ()
  "Record the current end-of-buffer point as a backtrack location.
Called by Screamer at the entry of a LOCAL-OUTPUT block via
`swank:eval-in-emacs'."
  (with-current-buffer (screamer-slime-output-buffer)
    (goto-char (point-max))
    (push (point) screamer-slime-backtrack-locations))
  nil)

(defun screamer-slime-pop-end-marker ()
  "Pop the most recent backtrack location and delete the region from
that location to point-max. Called by Screamer's trail closure on
backtrack."
  (with-current-buffer (screamer-slime-output-buffer)
    (when screamer-slime-backtrack-locations
      (let ((loc (pop screamer-slime-backtrack-locations)))
        (delete-region loc (point-max)))))
  nil)

(defun screamer-slime-insert-output (str)
  "Insert STR at the end of the *Screamer Output* buffer and display it.
Called by Screamer's LOCAL-OUTPUT to emit captured *standard-output* text.
The text inserted here is what gets deleted on backtrack by
`screamer-slime-pop-end-marker'."
  (with-current-buffer (screamer-slime-output-buffer)
    (goto-char (point-max))
    (insert str)
    (display-buffer (current-buffer)))
  nil)

(defun screamer-slime-clear-output ()
  "Clear the *Screamer Output* buffer and reset the backtrack stack.
Useful between independent Screamer searches."
  (interactive)
  (with-current-buffer (screamer-slime-output-buffer)
    (erase-buffer))
  (setq screamer-slime-backtrack-locations nil))

(provide 'screamer-slime)
;;; screamer-slime.el ends here
