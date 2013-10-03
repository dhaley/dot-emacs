;;; my-geben --- even better than geben

;;; Code:

;; Breakpoints to always set when geben starts.
(defun my-geben-breakpoints (session)
  (message "Setting my geben breakpoints.")
                                        ; how often is this called?
                                        ; call debugger() in PHP, similar to
                                        ; debugger; in javascript.
                                        ; Would be nice to also set a watch on
                                        ; $GLOBALS['debugger'], but couldn't
                                        ; get that to work.
  (geben-set-breakpoint-call "debugger"))

; set breakpoints when geben starts
(add-hook 'geben-dbgp-init-hook #'my-geben-breakpoints t)

; Ideally, we would not break in debugger(), but instead in the place that
; called it.  For now we must step out to see who called us.

;; A poor man's variable watch, since dbgp doesn't seem to support watching
;; vars.

;; geben's eval command will show result in the minibuffer.

;; More ideal would be to put the watch in the context buffer (or it's own
;; watch buffer).  However this is the limit of my elisp.  I'm not sure how to
;; prevent geben from writing to minibuffer when geben-dbgp-command-eval is
;; called.
(defun my-geben-watch (session)
;(geben-dbgp-command-eval session "print_r($GLOBALS['watch'], 1)")) ; takes up
;several lines in minibuffer
;(geben-dbgp-command-eval session "$GLOBALS['watch']")) ; uses fewer lines,
;not always easy to read

; watch() keeps track of which variables to display.
  (geben-dbgp-command-eval
   session
   "function_exists(
'watch') ?
'watch: ' . print_r(watch(get_defined_vars()),
1): '
watch() not yet defined.'")
  )

; display watch vars, very often.
(add-hook 'geben-dbgp-continuous-command-hook #'my-geben-watch t)

;; This controls whether geben stops before running anything.
(setq geben-pause-at-entry-line nil)

(provide 'my-geben)

;;; my-geben.el ends here
