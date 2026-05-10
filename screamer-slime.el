;;; screamer-slime.el --- SLIME-side support for Screamer's LOCAL-OUTPUT  -*- lexical-binding: t -*-

;; Copyright 2026 Paulo Henrique Raposo.
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to
;; deal in the Software without restriction, including without limitation the
;; rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
;; sell copies of the Software.

;; Replaces the classic iscream.el (ILisp/bridge.el-based). Provides the
;; Emacs-side hooks for Screamer's LOCAL-OUTPUT macro under SLIME.
;;
;; Setup:
;;   1. Add this directory to `load-path'.
;;   2. (require 'screamer-slime) in your Emacs init.
;;   3. Connect to a Lisp with SLIME, load Screamer.
;;   4. On the Lisp side: (setf screamer:*iscream?* t)
;;
;; The Lisp side creates a swank channel on the first LOCAL-OUTPUT call
;; and asks Emacs to register a mirror via `swank:ed-rpc-no-wait' -- a
;; permission gated by `defslimefun', NOT by `slime-enable-evaluate-in-
;; emacs'. After registration, LOCAL-OUTPUT sends `(:channel-send ID ...)'
;; events that are dispatched directly to the channel methods below; no
;; READ or EVAL runs on the Emacs side.
;;
;; LOCAL-OUTPUT semantics: the body's *standard-output* is captured in
;; Lisp and shipped here as a single string per call. The text appears
;; in the *Screamer Output* buffer and vanishes when Screamer backtracks
;; past the LOCAL-OUTPUT entry. After a search completes, the trail
;; unwinds entirely and the buffer ends empty -- LOCAL-OUTPUT is a live
;; observation tool, not a persistent log.

(require 'slime)

(defvar screamer-slime-output-buffer-name "*Screamer Output*"
  "Name of the buffer where LOCAL-OUTPUT writes.")

(defvar screamer-slime-backtrack-locations nil
  "Stack of buffer positions saved by `:insert' channel messages,
popped (with the region from each marker to point-max deleted) by
`:pop' messages on backtrack.")

(defvar screamer-slime--buffer-shown-p nil
  "Non-nil once the *Screamer Output* buffer has been displayed in
a window during the current session, so subsequent inserts skip the
`display-buffer' call.")

(defun screamer-slime-output-buffer ()
  "Return (creating if needed) the *Screamer Output* buffer."
  (get-buffer-create screamer-slime-output-buffer-name))

(defun screamer-slime--ensure-shown ()
  "Make the output buffer visible on the first insert of a session.
After the buffer is in a window, subsequent calls short-circuit."
  (unless screamer-slime--buffer-shown-p
    (display-buffer (screamer-slime-output-buffer))
    (setq screamer-slime--buffer-shown-p t)))

(defun screamer-slime-clear-output ()
  "Clear the *Screamer Output* buffer and reset the backtrack stack."
  (interactive)
  (with-current-buffer (screamer-slime-output-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)))
  (setq screamer-slime-backtrack-locations nil
        screamer-slime--buffer-shown-p nil))

(defsubst screamer-slime--insert (str)
  "Insert STR at point-max of the output buffer and push the
pre-insert position onto the backtrack stack. Pops the buffer into
a window on the first call of the session; subsequent calls skip
the `display-buffer' overhead."
  (screamer-slime--ensure-shown)
  (with-current-buffer (screamer-slime-output-buffer)
    (goto-char (point-max))
    (push (point) screamer-slime-backtrack-locations)
    (let ((inhibit-read-only t))
      (insert str))))

(defsubst screamer-slime--pop ()
  "Pop the most recent backtrack location and delete the region from
that location to point-max."
  (with-current-buffer (screamer-slime-output-buffer)
    (when screamer-slime-backtrack-locations
      (let ((loc (pop screamer-slime-backtrack-locations))
            (inhibit-read-only t))
        (delete-region loc (point-max))))))

(slime-define-channel-type screamer)

(slime-define-channel-method screamer :insert (str)
  (screamer-slime--insert str))

(slime-define-channel-method screamer :pop ()
  (screamer-slime--pop))

;; `defslimefun' marks the function as RPC-callable from Lisp via
;; `swank:ed-rpc-no-wait'. The permission check on the Emacs side is
;; `slime-rpc-allowed-p', which is independent of
;; `slime-enable-evaluate-in-emacs'.
(defslimefun screamer-slime-register-channel (id)
  "Create the Emacs-side mirror channel for the Lisp screamer-output
channel with ID. Called once per session by the Lisp side during the
first LOCAL-OUTPUT firing; subsequent traffic goes through
`(:channel-send ...)' events without any RPC. Refuses to overwrite an
existing channel binding -- a Lisp side that already registered would
otherwise shadow another channel and divert its messages."
  (when (slime-find-channel id)
    (error "screamer-slime: refusing to register; channel id %s already in use"
           id))
  (let ((ch (slime-make-channel% slime-screamer-channel-methods
                                 "screamer-output"
                                 id
                                 nil)))
    (push (cons id ch) (slime-channels))
    id))

(defun screamer-slime--on-connect ()
  "Reset both sides on a fresh SLIME connection (initial connect or
reconnect):
  - Emacs-side: forget the buffer-shown flag and clear the backtrack
    stack so the next insert pops the output buffer into a window
    again and starts with no leftover markers from the dead session.
  - Lisp-side: forget the cached channel ID, so the next LOCAL-OUTPUT
    re-handshakes against this connection. The form is a no-op when
    the Lisp image hasn't loaded Screamer."
  (setq screamer-slime--buffer-shown-p nil
        screamer-slime-backtrack-locations nil)
  (when (slime-connected-p)
    (slime-eval-async
     '(cl:when (cl:find-package "SCREAMER")
        (cl:let ((fn (cl:find-symbol "RESET-SCREAMER-OUTPUT-CHANNEL"
                                     "SCREAMER")))
          (cl:when fn (cl:funcall fn)))))))

(add-hook 'slime-connected-hook 'screamer-slime--on-connect)

(provide 'screamer-slime)
;;; screamer-slime.el ends here
