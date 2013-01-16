;;;_. Initialization


(defconst *is-a-mac*
  (eq system-type 'darwin)
  "Is this running on OS X?")
(defconst *is-carbon-emacs*
  (and *is-a-mac* (eq window-system 'mac))
  "Is this the Carbon port of Emacs?")
(defconst *is-cocoa-emacs*
  (and *is-a-mac* (eq window-system 'ns))
  "Is this the Cocoa version of Emacs?")
(defconst *is-linux*
  (eq system-type 'gnu/linux)
  "Is this running on Linux?")

(defconst user-cache-directory
  (file-truename "~/.cache/emacs-user-cache"))

;; These should always exist
(make-directory user-cache-directory t)


;; Set path to .emacs.d
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))


(defalias 'yes-or-no-p 'y-or-n-p)


;; If you find that Emacs on OSX fails to pick up the same $PATH setting
;; that you get in command line shells, instead defaulting to an
;; impoverished default that doesn’t include (for instance) anything
;; installed via MacPorts:

(add-hook 'after-init-hook
          #'(lambda ()
              (setenv "PATH"
                      (with-temp-buffer
                        (call-process "/bin/bash"
                                      nil (list (current-buffer) nil) nil
                                      "-l" "-c" "printf %s \"$PATH\"")
                        (buffer-string)))))

;; Fix ls problems

;; (when (eq system-type 'darwin)
;;   (require 'ls-lisp)
;;   (setq ls-lisp-use-insert-directory-program nil))


(setq message-log-max 16384)

(setenv "E" "~/.emacs.d")
(setenv "O" "~/git/dkh-org")
(setenv "S" "~/git/src")
(setenv "D" "~/data")

(defconst emacs-start-time (current-time))

(unless noninteractive
  (message "Loading %s..." load-file-name))

(load (expand-file-name "load-path" (file-name-directory load-file-name)))



;; '(mac-command-modifier (quote hyper))
;; '(mac-option-modifier (quote meta))
;; Keybonds

(global-set-key [(hyper a)] 'mark-whole-buffer)
(global-set-key [(hyper v)] 'yank)
;; (global-set-key [(hyper c)] 'kill-ring-save)
(global-set-key [(hyper s)] 'save-buffer)
(global-set-key [(hyper l)] 'goto-line)
(global-set-key [(hyper w)]
                (lambda () (interactive) (delete-window)))
;; (global-set-key [(hyper z)] 'undo)


;; mac switch meta key
(defun mac-switch-meta nil
  "switch meta between Option and Command"
  (interactive)
  (if (eq mac-option-modifier nil)
      (progn
        (setq mac-option-modifier 'meta)
        (setq mac-command-modifier 'hyper)
        )
    (progn
      (setq mac-option-modifier nil)
      (setq mac-command-modifier 'meta)
      )
    )
  )

(mac-switch-meta)

(require 'use-package)
(eval-when-compile
  (setq use-package-verbose (null byte-compile-current-file)))

;;;_ , Utility macros and functions

(defmacro hook-into-modes (func modes)
  `(dolist (mode-hook ,modes)
     (add-hook mode-hook ,func)))

(defun system-idle-time ()
  (with-temp-buffer
    (call-process "ioreg" nil (current-buffer) nil
                  "-c" "IOHIDSystem" "-d" "4" "-S")
    (goto-char (point-min))
    (and (re-search-forward "\"HIDIdleTime\" = \\([0-9]+\\)" nil t)
         (/ (float (string-to-number (match-string 1)))
            1000000000.0))))

(defun quickping (host)
  (= 0 (call-process "/sbin/ping" nil nil nil "-c1" "-W50" "-q" host)))


(defun cleanup-term-log ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (require 'ansi-color)
  (ansi-color-apply-on-region (point-min) (point-max))
  (goto-char (point-min))
  (while (re-search-forward "\\(.\\|$\\|P.+\\\\\n\\)" nil t)
    (overlay-put (make-overlay (match-beginning 0) (match-end 0))
                 'invisible t))
  (set-buffer-modified-p nil))

(add-hook 'find-file-hooks
          (function
           (lambda ()
             (if (string-match "/\\.iTerm/.*\\.log\\'"
                               (buffer-file-name))
                 (cleanup-term-log)))))

;;;_ , Read system environment

(defun read-system-environment ()
  (let ((plist (expand-file-name "~/.MacOSX/environment.plist")))
    (when (file-readable-p plist)
      (let ((dict (cdr (assq 'dict (cdar (xml-parse-file plist))))))
        (while dict
          (if (and (listp (car dict))
                   (eq 'key (caar dict)))
              (setenv (car (cddr (car dict)))
                      (car (cddr (car (cddr dict))))))
          (setq dict (cdr dict))))

      ;; Configure exec-path based on the new PATH
      (setq exec-path nil)
      (mapc (apply-partially #'add-to-list 'exec-path)
            (nreverse (split-string (getenv "PATH") ":"))))))

(read-system-environment)
(add-hook 'after-init-hook 'read-system-environment)

;;;_ , Load customization settings

(defvar running-alternate-emacs nil)
(if (string-match (concat "/Applications/\\(Misc/\\)?"
                          "Emacs\\([A-Za-z]+\\).app/Contents/MacOS/")
                  invocation-directory)


    (let ((settings (with-temp-buffer
                      (insert-file-contents
                       (expand-file-name "settings.el" user-emacs-directory))
                      (goto-char (point-min))
                      (read (current-buffer))))
          (suffix (downcase (match-string 2 invocation-directory))))

      (setq running-alternate-emacs t
            user-data-directory
            (replace-regexp-in-string "/data/" (format "/data-%s/" suffix)
                                      user-data-directory))

      (let* ((regexp "/\\.emacs\\.d/data/")
             (replace (format "/.emacs.d/data-%s/" suffix)))
        (dolist (setting settings)
          (let ((value (and (listp setting)
                            (nth 1 (nth 1 setting)))))
            (if (and (stringp value)
                     (string-match regexp value))
                (setcar (nthcdr 1 (nth 1 setting))
                        (replace-regexp-in-string regexp replace value)))))

        (eval settings)))

  (load (expand-file-name "settings" user-emacs-directory)))

(defvar running-alternate-emacs nil)

;;;_ , Enable disabled commands

(put 'downcase-region  'disabled nil)   ; Let downcasing work
(put 'erase-buffer     'disabled nil)
(put 'eval-expression  'disabled nil)   ; Let ESC-ESC work
(put 'narrow-to-page   'disabled nil)   ; Let narrowing work
(put 'narrow-to-region 'disabled nil)   ; Let narrowing work
(put 'set-goal-column  'disabled nil)
(put 'upcase-region    'disabled nil)   ; Let upcasing work

;;Hiding and replacing modeline strings with clean-mode-line

(defvar mode-line-cleaner-alist
  `((auto-complete-mode . " α")
    (yas/minor-mode . " υ")
    (paredit-mode . " π")
    (eldoc-mode . "")
    (abbrev-mode . "")
    ;; Major modes
    (lisp-interaction-mode . "λ")
    (hi-lock-mode . "")
    (python-mode . "Py")
    (emacs-lisp-mode . "EL")
    (nxhtml-mode . "nx"))
  "Alist for `clean-mode-line'.

When you add a new element to the alist, keep in mind that you
must pass the correct minor/major mode symbol and a string you
want to use in the modeline *in lieu of* the original.")


(defun clean-mode-line ()
  (interactive)
  (loop for cleaner in mode-line-cleaner-alist
        do (let* ((mode (car cleaner))
                  (mode-str (cdr cleaner))
                  (old-mode-str (cdr (assq mode minor-mode-alist))))
             (when old-mode-str
               (setcar old-mode-str mode-str))
             ;; major mode
             (when (eq mode major-mode)
               (setq mode-name mode-str)))))


(add-hook 'after-change-major-mode-hook 'clean-mode-line)


;; Modes and mode groupings
(defmacro hook-into-modes (func modes)
  `(dolist (mode-hook ,modes)
     (add-hook mode-hook ,func)))

(defvar my-lisp-modes
  '(emacs-lisp-mode
    inferior-emacs-lisp-mode
    ielm-mode
    lisp-mode
    inferior-lisp-mode
    lisp-interaction-mode
    slime-repl-mode))

(defvar my-lisp-mode-hooks
  (mapcar (function
           (lambda (mode)
             (intern
              (concat (symbol-name mode) "-hook"))))
          my-lisp-modes))

(defvar my-prog-mode-hooks
  '(prog-mode-hook
    emacs-lisp-mode-hook
    pyhon-mode-hook
    coffee-mode-hook
    js-mode-hook
    js2-mode-hook
    actionscript-mode-hook
    ruby-mode-hook
    haskell-mode-hook
    clojure-mode-hook
    go-mode-hook
    groovy-mode-hook
    qml-mode-hook))

(defvar my-significant-whitespace-mode-hooks
  '(coffee-mode-hook
    python-mode-hook
    haskell-mode-hook
    stylus-mode-hook
    haml-mode-hook))

(defvar my-markup-mode-hooks
  '(html-mode-hook
    markdown-mode-hook
    rst-mode-hook
    org-mode-hook))

(defvar my-html-like-mode-hooks
  '(html-mode-hook
    handlebars-mode-hook
    nxml-mode-hook
    web-mode-hook
    haml-mode-hook))

(defvar my-css-like-mode-hooks
  '(css-mode-hook
    stylus-mode-hook
    sass-mode-hook
    scss-mode))

;;; alias the new `flymake-report-status-slim' to
;;; `flymake-report-status'
(defalias 'flymake-report-status 'flymake-report-status-slim)
(defun flymake-report-status-slim (e-w &optional status)
  "Show \"slim\" flymake status in mode line."
  (when e-w
    (setq flymake-mode-line-e-w e-w))
  (when status
    (setq flymake-mode-line-status status))
  (let* ((mode-line " Φ"))
    (when (> (length flymake-mode-line-e-w) 0)
      (setq mode-line (concat mode-line ":" flymake-mode-line-e-w)))
    (setq mode-line (concat mode-line flymake-mode-line-status))
    (setq flymake-mode-line mode-line)
    (force-mode-line-update)))

;; Working with Coding Systems and Unicode in Emacs

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
;; backwards compatibility as default-buffer-file-coding-system
;; is deprecated in 23.2.
(if (boundp 'buffer-file-coding-system)
    (setq-default buffer-file-coding-system 'utf-8)
  (setq default-buffer-file-coding-system 'utf-8))

;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;;;_. Keybindings

;; Main keymaps for personal bindings are:
;;
;;   C-x <letter>  primary map (has many defaults too)
;;   C-c <letter>  secondary map (not just for mode-specific)
;;   C-. <letter>  tertiary map
;;
;;   M-g <letter>  goto map
;;   M-s <letter>  search map
;;   M-o <letter>  markup map (even if only temporarily)
;;
;;   C-<capital letter>
;;   M-<capital letter>
;;
;;   A-<anything>
;;   M-A-<anything>
;;
;; Single-letter bindings still available:
;;   C- ,'";:?<>|!#$%^&*`~ <tab>
;;   M- ?#

;;;_ , global-map

;;;_  . H-?


(defun sacha/search-word-backward ()
  "Find the previous occurrence of the current word."
  (interactive)
  (let ((cur (point)))
    (skip-syntax-backward "w_")
    (goto-char
     (if (re-search-backward (concat "\\_<" (current-word) "\\_>") nil t)
         (match-beginning 0)
       cur))))

(defun sacha/search-word-forward ()
  "Find the next occurrence of the current word."
  (interactive)
  (let ((cur (point)))
    (skip-syntax-forward "w_")
    (goto-char
     (if (re-search-forward (concat "\\_<" (current-word) "\\_>") nil t)
         (match-beginning 0)
       cur))))

(defadvice search-for-keyword (around sacha activate)
  "Match in a case-insensitive way."
  (let ((case-fold-search t))
    ad-do-it))

(bind-key "C-H-r" 'sacha/search-word-backward)
(bind-key "C-H-s" 'sacha/search-word-forward)



(defun double-quote ()
  (interactive)
  (if (use-region-p)
      (save-excursion
        (let ((beginning (region-beginning))
              (end (+ (region-end) 1)))
          (goto-char beginning)
          (insert "“")
          (goto-char end)
          (insert "”")))
    (insert "“”")
    (backward-char)))

(bind-key "C-c \"" 'double-quote)


                                        ; gets the name of the buffer.
(defun copy-buffer-name()
  (interactive)
  (let ((beg (point)))
    (set-mark (point))
    (insert buffer-file-name)
    (kill-region beg (point))))

                                        ; sets copy buffer name to Ctrl-c-p
(bind-key "H-p" 'copy-buffer-name)

                                        ; copies the region to a string (typically for parseing).
(defun region-to-string ()
  (interactive)
  (buffer-substring (mark) (point)))

(bind-key "H-i" 'ispell-word)
(bind-key "H-e" 'grab-email-my)
(bind-key "H-r" 'winner-redo) ;; C-c <right>
(bind-key "H-z" 'winner-undo) ;; C-c <left>
(bind-key "H-\\" 'my-toggle-window-split)
(bind-key "H-|" 'my-swap-windows)
(bind-key "<H-down>" 'shrink-window)
(bind-key "<H-left>" 'shrink-window-horizontally)
(bind-key "<H-right>" 'enlarge-window-horizontally)
(bind-key "<H-up>" 'enlarge-window)
(bind-key "<H-backspace>" 'scroll-down-command)
(bind-key "H-f" 'new-frame)

(defun grab-email-my ()
  "Grab the next email in the buffer
  First posted by François Fleuret <francois.fleuret@inria.fr>..
improved by many.."
  (interactive)
  (re-search-forward "[^ \t\n]+@[^ \t\n]+")
  (copy-region-as-kill (match-beginning 0) (match-end 0))
  )




;;;_  . C-?

(defvar ctl-period-map)
(define-prefix-command 'ctl-period-map)
(bind-key "C-." 'ctl-period-map)

(bind-key* "<C-return>" 'other-window)

(defun collapse-or-expand ()
  (interactive)
  (if (> (length (window-list)) 1)
      (delete-other-windows)
    (bury-buffer)))

(bind-key "C-z" 'collapse-or-expand)


;; http://www.masteringemacs.org/articles/2011/03/16/removing-blank-lines-buffer/
(defun flush-blank-lines (start end)
  (interactive "r")
  (flush-lines "^\\s-*$" start end nil))

(defun collapse-blank-lines (start end)
  (interactive "r")
  (replace-regexp "^\n\\{2,\\}" "\n" nil start end))

(bind-key "C-x C-m" 'execute-extended-command)
(bind-key "C-c C-m" 'execute-extended-command)
(bind-key "C-x C-r" 'rename-current-buffer-file)
(bind-key "C-x C-k" 'delete-current-buffer-file)

;;;_  . M-?

(defadvice async-shell-command (before uniqify-running-shell-command activate)
  (let ((buf (get-buffer "*Async Shell Command*")))
    (if buf
        (let ((proc (get-buffer-process buf)))
          (if (and proc (eq 'run (process-status proc)))
              (with-current-buffer buf
                (rename-uniquely)))))))

(bind-key "M-!" 'async-shell-command)
(bind-key "M-/" 'dabbrev-expand)
(bind-key "M-'" 'insert-pair)
(bind-key "M-\"" 'insert-pair)

(defun align-code (beg end &optional arg)
  (interactive "rP")
  (if (null arg)
      (align beg end)
    (let ((end-mark (copy-marker end)))
      (indent-region beg end-mark nil)
      (align beg end-mark))))

(bind-key "M-[" 'align-code)
(bind-key "M-`" 'other-frame)

(bind-key "M-j" 'delete-indentation-forward)
(bind-key "M-J" 'delete-indentation)

(bind-key "M-W" 'mark-word)

(defun mark-line (&optional arg)
  (interactive "p")
  (beginning-of-line)
  (let ((here (point)))
    (dotimes (i arg)
      (end-of-line))
    (set-mark (point))
    (goto-char here)))

(bind-key "M-L" 'mark-line)

(defun mark-sentence (&optional arg)
  (interactive "P")
  (backward-sentence)
  (mark-end-of-sentence arg))

(bind-key "M-S" 'mark-sentence)
(bind-key "M-X" 'mark-sexp)
(bind-key "M-H" 'mark-paragraph)
(bind-key "M-D" 'mark-defun)

(bind-key "M-g c" 'goto-char)
(bind-key "M-g l" 'goto-line)

(global-set-key (vector 'remap 'goto-line) 'goto-line-with-feedback)

(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (goto-line (read-number "Goto line: ")))
    (linum-mode -1)))


(defun delete-current-file (ξno-backup-p)
  "Delete the file associated with the current buffer.
Delete the current buffer too.

A backup file is created with filename appended “~”. Existing backup file are overwritten.

if ΞNO-BACKUP-P is non-nil (when called with `universal-argument'), don't create backup.

If no file is associated, just close buffer without prompt for save."
  (interactive "P")
  (let (fName)
    (when (buffer-file-name) ; buffer is associated with a file
      (setq fName (buffer-file-name))
      (save-buffer fName)
      (if ξno-backup-p
          (progn )
        (copy-file fName (concat fName "~" ) t)
        )
      (delete-file fName)
      (message "「%s」 deleted." fName)
      )
    (kill-buffer (current-buffer))
    ) )


(defun delete-duplicate-lines (beg end)
  (interactive "r")
  (let ((lines (split-string (buffer-substring beg end) "\n")))
    (delete-region beg end)
    (insert
     (mapconcat #'identity (delete-dups lines) "\n"))))

(defun delete-indentation-forward ()
  (interactive)
  (delete-indentation t))

(bind-key "M-s n" 'find-name-dired)
(bind-key "M-s o" 'occur)


;;;_  . M-C-?

(bind-key "<C-M-backspace>" 'backward-kill-sexp)

(defun isearch-backward-other-window ()
  (interactive)
  (split-window-vertically)
  (call-interactively 'isearch-backward))

(bind-key "C-M-r" 'isearch-backward-other-window)

(defun isearch-forward-other-window ()
  (interactive)
  (split-window-vertically)
  (call-interactively 'isearch-forward))

(bind-key "C-M-s" 'isearch-forward-other-window)

;; Some further isearch bindings
(bind-key "C-c" 'isearch-toggle-case-fold isearch-mode-map)
(bind-key "C-t" 'isearch-toggle-regexp isearch-mode-map)
(bind-key "C-^" 'isearch-edit-string isearch-mode-map)
(bind-key "C-i" 'isearch-complete isearch-mode-map)

;;;_  . A-?

(define-key key-translation-map (kbd "A-TAB") (kbd "C-TAB"))

;;;_ , ctl-x-map


(defun youngfrog/copy-rectangle-to-kill-ring (start end)
  "Saves a rectangle to the normal kill ring. Not suitable for yank-rectangle."
  (interactive "r")
  (let ((lines (extract-rectangle start end)))
    (with-temp-buffer
      (while lines ;; insert-rectangle, but without the unneeded stuff
        ;; (most importantly no push-mark)
        (insert-for-yank (car lines))
        (insert "\n")
        (setq lines (cdr lines)))
      (kill-ring-save (point-min) (point-max)))))

;;;_  . C-x ?

(bind-key "C-x B" 'ido-switch-buffer-other-window)
(bind-key "C-x d" 'delete-whitespace-rectangle)
(bind-key "C-x F" 'set-fill-column)
(bind-key "C-x t" 'toggle-truncate-lines)

(defun toggle-transparency ()
  (interactive)
  (if (/= (cadr (frame-parameter nil 'alpha)) 100)
      (set-frame-parameter nil 'alpha '(100 100))
    (set-frame-parameter nil 'alpha '(85 50))))

(bind-key "C-x T" 'toggle-transparency)

;;;_  . C-x C-?

(defun duplicate-line ()
  "Duplicate the line containing point."
  (interactive)
  (save-excursion
    (let (line-text)
      (goto-char (line-beginning-position))
      (let ((beg (point)))
        (goto-char (line-end-position))
        (setq line-text (buffer-substring beg (point))))
      (if (eobp)
          (insert ?\n)
        (forward-line))
      (open-line 1)
      (insert line-text))))

(bind-key "C-x C-d" 'duplicate-line)
(bind-key "C-x C-e" 'pp-eval-last-sexp)
(bind-key "C-x C-n" 'next-line)


(defun find-alternate-file-with-sudo (filename)
  (interactive
   (list (read-file-name "Find alternate file: " nil
                         nil nil (concat "/sudo::" (buffer-file-name)))))
  (find-alternate-file filename))

(bind-key "C-x C-v" 'find-alternate-file-with-sudo)


;; http://www.emacswiki.org/emacs/ElispCookbook
(defun qdot/filter (condp lst)
  (delq nil
        (mapcar (lambda (x) (and (funcall condp x) x)) lst)))


;;;_  . C-x M-?

(bind-key "C-x M-n" 'set-goal-column)

(defun refill-paragraph (arg)
  (interactive "*P")
  (let ((fun (if (memq major-mode '(c-mode c++-mode))
                 'c-fill-paragraph
               (or fill-paragraph-function
                   'fill-paragraph)))
        (width (if (numberp arg) arg))
        prefix beg end)
    (forward-paragraph 1)
    (setq end (copy-marker (- (point) 2)))
    (forward-line -1)
    (let ((b (point)))
      (skip-chars-forward "^A-Za-z0-9`'\"(")
      (setq prefix (buffer-substring-no-properties b (point))))
    (backward-paragraph 1)
    (if (eolp)
        (forward-char))
    (setq beg (point-marker))
    (delete-horizontal-space)
    (while (< (point) end)
      (delete-indentation 1)
      (end-of-line))
    (let ((fill-column (or width fill-column))
          (fill-prefix prefix))
      (if prefix
          (setq fill-column
                (- fill-column (* 2 (length prefix)))))
      (funcall fun nil)
      (goto-char beg)
      (insert prefix)
      (funcall fun nil))
    (goto-char (+ end 2))))

(bind-key "C-x M-q" 'refill-paragraph)

;;;_ , mode-specific-map

;;;_  . C-c ?

(bind-key "C-c <tab>" 'ff-find-other-file)
(bind-key "C-c SPC" 'just-one-space)

;; inspired by Erik Naggum's `recursive-edit-with-single-window'
(defmacro recursive-edit-preserving-window-config (body)
  "*Return a command that enters a recursive edit after executing BODY.
 Upon exiting the recursive edit (with\\[exit-recursive-edit] (exit)
 or \\[abort-recursive-edit] (abort)), restore window configuration
 in current frame."
  `(lambda ()
     "See the documentation for `recursive-edit-preserving-window-config'."
     (interactive)
     (save-window-excursion
       ,body
       (recursive-edit))))

(bind-key "C-c 0"
          (recursive-edit-preserving-window-config (delete-window)))
(bind-key "C-c 1"
          (recursive-edit-preserving-window-config
           (if (one-window-p 'ignore-minibuffer)
               (error "Current window is the only window in its frame")
             (delete-other-windows))))

(defun delete-current-line (&optional arg)
  (interactive "p")
  (let ((here (point)))
    (beginning-of-line)
    (kill-line arg)
    (goto-char here)))

(bind-key "C-c d" 'delete-current-line)

(bind-key "C-c e E" 'elint-current-buffer)

(defun do-eval-buffer ()
  (interactive)
  (call-interactively 'eval-buffer)
  (message "Buffer has been evaluated"))

(bind-key "C-c e b" 'do-eval-buffer)
(bind-key "C-c e c" 'cancel-debug-on-entry)
(bind-key "C-c e d" 'debug-on-entry)
(bind-key "C-c e e" 'toggle-debug-on-error)
(bind-key "C-c e f" 'emacs-lisp-byte-compile-and-load)
(bind-key "C-c e j" 'emacs-lisp-mode)
(bind-key "C-c e l" 'find-library)
(bind-key "C-c e r" 'eval-region)
(bind-key "C-c e s" 'scratch)
(bind-key "C-c e v" 'edit-variable)

(defun find-which (name)
  (interactive "sCommand name: ")
  (find-file-other-window
   (substring (shell-command-to-string (format "which %s" name)) 0 -1)))

(bind-key "C-c e w" 'find-which)
(bind-key "C-c e z" 'byte-recompile-directory)

(bind-key "C-c f" 'flush-lines)
(bind-key "C-c g" 'goto-line)

(bind-key "C-c k" 'keep-lines)

(eval-when-compile
  (defvar emacs-min-top)
  (defvar emacs-min-left)
  (defvar emacs-min-height)
  (defvar emacs-min-width))

(unless noninteractive
  (if running-alternate-emacs
      (progn
        (defvar emacs-min-top (if (= 1050 (x-display-pixel-height)) 574 722))
        (defvar emacs-min-left 5)
        (defvar emacs-min-height 38)
        (defvar emacs-min-width 158)
        )
    (defvar emacs-min-top 22)
    (defvar emacs-min-left (- (x-display-pixel-width) 918))
    (defvar emacs-min-height (if (= 1050 (x-display-pixel-height)) 55 64))
    (defvar emacs-min-width 158)))

(defun emacs-min ()
  (interactive)
  (set-frame-parameter (selected-frame) 'fullscreen nil)
  (set-frame-parameter (selected-frame) 'vertical-scroll-bars nil)
  (set-frame-parameter (selected-frame) 'horizontal-scroll-bars nil)
  (set-frame-parameter (selected-frame) 'top emacs-min-top)
  (set-frame-parameter (selected-frame) 'left emacs-min-left)
  (set-frame-parameter (selected-frame) 'height emacs-min-height)
  (set-frame-parameter (selected-frame) 'width emacs-min-width)

  (when running-alternate-emacs
    ;;     (set-background-color "grey85")
    ;;     (set-face-background 'fringe "gray80")
    )
  )

(if window-system
    (add-hook 'after-init-hook 'emacs-min))

(defun emacs-max ()
  (interactive)
  (if t
      (progn
        (set-frame-parameter (selected-frame) 'fullscreen 'fullboth)
        (set-frame-parameter (selected-frame) 'vertical-scroll-bars nil)
        (set-frame-parameter (selected-frame) 'horizontal-scroll-bars nil))
    (set-frame-parameter (selected-frame) 'top 26)
    (set-frame-parameter (selected-frame) 'left 2)
    (set-frame-parameter (selected-frame) 'width
                         (floor (/ (float (x-display-pixel-width)) 9.15)))
    (if (= 1050 (x-display-pixel-height))
        (set-frame-parameter (selected-frame) 'height
                             (if (>= emacs-major-version 24)
                                 66
                               55))
      (set-frame-parameter (selected-frame) 'height
                           (if (>= emacs-major-version 24)
                               75
                             64)))))

(defun emacs-toggle-size ()
  (interactive)
  (if (> (cdr (assq 'width (frame-parameters))) 100)
      (emacs-min)
    (emacs-max)))

(bind-key "C-c m" 'emacs-toggle-size)

(defun insert-date ()
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

(defun define-keys (mode-map keybindings)
  "Takes a mode map, and a list of (key function-designator)
lists. The functions are bound to the keys in the given mode-map.
Keys are in kbd format."
  (mapc (lambda (keybinding)
          (destructuring-bind (key function) keybinding
            (define-key mode-map (read-kbd-macro key) function)))
        keybindings))

(defun global-set-keys (keybindings)
  "Takes a list of (key function-designator) lists.
The functions are globally bound to the keys. Keys
are in kbd format."
  (mapc (lambda (keybinding)
          (destructuring-bind (key function) keybinding
            (global-set-key (read-kbd-macro key) function)))
        keybindings))



(defcustom user-initials nil
  "*Initials of this user."
  :set
  #'(lambda (symbol value)
      (if (fboundp 'font-lock-add-keywords)
          (mapc
           #'(lambda (mode)
               (font-lock-add-keywords
                mode (list (list (concat "\\<\\(" value " [^:\n]+\\):")
                                 1 font-lock-warning-face t))))
           '(c-mode c++-mode emacs-lisp-mode lisp-mode
                    python-mode perl-mode java-mode groovy-mode)))
      (set symbol value))
  :type 'string
  :group 'mail)




(defun insert-user-timestamp ()
  "Insert a quick timestamp using the value of `user-initials'."
  (interactive)
  (insert (format "%s (%s): " user-initials
                  (format-time-string "%Y-%m-%d" (current-time)))))

(bind-key "C-c n" 'insert-user-timestamp)
(bind-key "C-c o" 'customize-option)
(bind-key "C-c O" 'customize-group)

(bind-key "C-c q" 'fill-region)
(bind-key "C-c r" 'replace-regexp)
(bind-key "C-c s" 'replace-string)
(bind-key "C-c u" 'rename-uniquely)

(autoload 'auth-source-search "auth-source")

(defun tinify-url (url)
  (interactive "sURL to shorten: ")
  (let* ((api-login "jwiegley")
         (api-key
          (funcall
           (plist-get
            (car (auth-source-search :host "api.j.mp" :user api-login
                                     :type 'netrc :port 80))
            :secret))))
    (flet ((message (&rest ignore)))
      (with-current-buffer
          (let ((query
                 (format "format=txt&longUrl=%s&login=%s&apiKey=%s"
                         (url-hexify-string url) api-login api-key)))
            (url-retrieve-synchronously
             (concat "http://api.j.mp/v3/shorten?" query)))
        (goto-char (point-min))
        (re-search-forward "^$")
        (prog1
            (kill-new (buffer-substring (1+ (point)) (1- (point-max))))
          (kill-buffer (current-buffer)))))))

(bind-key "C-c U" 'tinify-url)
(bind-key "C-c v" 'ffap)

(defun view-clipboard ()
  (interactive)
  (delete-other-windows)
  (switch-to-buffer "*Clipboard*")
  (let ((inhibit-read-only t))
    (erase-buffer)
    (clipboard-yank)
    (goto-char (point-min))
    (html-mode)
    (view-mode)))

(bind-key "C-c V" 'view-clipboard)
(bind-key "C-c z" 'clean-buffer-list)

(bind-key "C-c [" 'align-regexp)
(bind-key "C-c =" 'count-matches)
(bind-key "C-c ;" 'comment-or-uncomment-region)


(defun wph-here()
  "Inserts the filename and line number at the current point"
  (interactive)
  (insert buffer-file-name)
  (insert ":")
  (insert (number-to-string (count-lines (point-min) (point))))
  )

(global-set-key "\C-c\C-f" 'wph-here)


;;;_  . C-c C-?

(defun delete-to-end-of-buffer ()
  (interactive)
  (kill-region (point) (point-max)))

(bind-key "C-c C-z" 'delete-to-end-of-buffer)

;;;_  . C-c M-?

(defun unfill-paragraph (arg)
  (interactive "*p")
  (let (beg end)
    (forward-paragraph arg)
    (setq end (copy-marker (- (point) 2)))
    (backward-paragraph arg)
    (if (eolp)
        (forward-char))
    (setq beg (point-marker))
    (when (> (count-lines beg end) 1)
      (while (< (point) end)
        (goto-char (line-end-position))
        (let ((sent-end (memq (char-before) '(?. ?\; ?! ??))))
          (delete-indentation 1)
          (if sent-end
              (insert ? )))
        (end-of-line))
      (save-excursion
        (goto-char beg)
        (while (re-search-forward "[^.;!?:]\\([ \t][ \t]+\\)" end t)
          (replace-match " " nil nil nil 1))))))

(bind-key "C-c M-q" 'unfill-paragraph)

(defun unfill-region (beg end)
  (interactive "r")
  (setq end (copy-marker end))
  (save-excursion
    (goto-char beg)
    (while (< (point) end)
      (unfill-paragraph 1)
      (forward-paragraph))))

(defun uniquify-region-lines (beg end)
  "Remove duplicate adjacent lines in region."
  (interactive "*r")
  (save-excursion
    (goto-char beg)
    (while (re-search-forward "^\\(.*\n\\)\\1+" end t)
      (replace-match "\\1"))))

(defun uniquify-buffer-lines ()
  "Remove duplicate adjacent lines in the current buffer."
  (interactive)
  (uniquify-region-lines (point-min) (point-max)))


;;;_, Toggle between split windows and a single window

(defun my-iswitchb-close()
  "Open iswitchb or, if in minibuffer go to next match. Handy way to cycle through the ring."
  (interactive)
  (if (window-minibuffer-p (selected-window))
      (keyboard-escape-quit)))

(defun toggle-windows-split()
  "Switch back and forth between one window and whatever split of windows we might have in the frame. The idea is to maximize the current buffer, while being able to go back to the previous split of windows in the frame simply by calling this command again."
  (interactive)
  (if (not (window-minibuffer-p (selected-window)))
      (progn
        (if (< 1 (count-windows))
            (progn
              (window-configuration-to-register ?u)
              (delete-other-windows))
          (jump-to-register ?u))))
  (my-iswitchb-close))

(define-key global-map (kbd "C-|") 'toggle-windows-split)


;; (defun shrink-window-to-region ()
;;   (interactive)
;;   (let
;;       ((window-lines (count-lines (window-start) (window-end)))
;;        (region-lines (count-lines (region-beginning) (region-end))))
;;     (shrink-window (- window-lines region-lines))))

(defun smart-copy ()
  "Copy word at point, or line if called twice, or region if transient-mark active."
  (interactive)
  (if (eq last-command 'smart-copy)
      (progn (kill-ring-save (line-beginning-position) (line-end-position))
             (message "Line pushed to kill ring"))
    (save-excursion
      (if (not mark-active)
          (progn
            (mark-word)
            (backward-word)
            (message "Word pushed to kill ring")))
      (kill-ring-save (region-beginning) (region-end)))))

(bind-key "M-w" 'smart-copy)

;; Functions (load all files in defuns-dir)
(setq defuns-dir (expand-file-name "defuns" dotfiles-dir))
(dolist (file (directory-files defuns-dir t "\\w+"))
  (when (file-regular-p file)
    (load file)))



;;;_ , ctl-period-map

;;;_  . C-. ?

(bind-key "C-. m" 'kmacro-keymap)

;;;_  . C-. C-i

(bind-key "C-. C-i" 'indent-rigidly)

;;;_ , help-map

(defvar lisp-find-map)
(define-prefix-command 'lisp-find-map)

(bind-key "C-h e" 'lisp-find-map)

;;;_  . C-h e ?

(bind-key "C-h e c" 'finder-commentary)
(bind-key "C-h e e" 'view-echo-area-messages)
(bind-key "C-h e f" 'find-function)
(bind-key "C-h e F" 'find-face-definition)

(defun my-describe-symbol  (symbol &optional mode)
  (interactive
   (info-lookup-interactive-arguments 'symbol current-prefix-arg))
  (let (info-buf find-buf desc-buf cust-buf)
    (save-window-excursion
      (ignore-errors
        (info-lookup-symbol symbol mode)
        (setq info-buf (get-buffer "*info*")))
      (let ((sym (intern-soft symbol)))
        (when sym
          (if (functionp sym)
              (progn
                (find-function sym)
                (setq find-buf (current-buffer))
                (describe-function sym)
                (setq desc-buf (get-buffer "*Help*")))
            (find-variable sym)
            (setq find-buf (current-buffer))
            (describe-variable sym)
            (setq desc-buf (get-buffer "*Help*"))
            ;;(customize-variable sym)
            ;;(setq cust-buf (current-buffer))
            ))))

    (delete-other-windows)

    (flet ((switch-in-other-buffer
            (buf)
            (when buf
              (split-window-vertically)
              (switch-to-buffer-other-window buf))))
      (switch-to-buffer find-buf)
      (switch-in-other-buffer desc-buf)
      (switch-in-other-buffer info-buf)
      ;;(switch-in-other-buffer cust-buf)
      (balance-windows))))

(bind-key "C-h e d" 'my-describe-symbol)
(bind-key "C-h e i" 'info-apropos)
(bind-key "C-h e k" 'find-function-on-key)
(bind-key "C-h e l" 'find-library)

(defvar lisp-modes  '(emacs-lisp-mode
                      inferior-emacs-lisp-mode
                      ielm-mode
                      lisp-mode
                      inferior-lisp-mode
                      lisp-interaction-mode
                      slime-repl-mode))

(defvar lisp-mode-hooks
  (mapcar (function
           (lambda (mode)
             (intern
              (concat (symbol-name mode) "-hook"))))
          lisp-modes))

(defun scratch ()
  (interactive)
  (let ((current-mode major-mode))
    (switch-to-buffer-other-window (get-buffer-create "*scratch*"))
    (goto-char (point-min))
    (when (looking-at ";")
      (forward-line 4)
      (delete-region (point-min) (point)))
    (goto-char (point-max))
    (if (memq current-mode lisp-modes)
        (funcall current-mode))))

(bind-key "C-h e s" 'scratch)
(bind-key "C-h e v" 'find-variable)
(bind-key "C-h e V" 'apropos-value)



(defun select-text-in-quote ()
  "Select text between the nearest left and right delimiters.
Delimiters are paired characters:
 () [] {} «» ‹› “” 〖〗 【】 「」 『』 （） 〈〉 《》 〔〕 ⦗⦘ 〘〙 ⦅⦆ 〚〛 ⦃⦄
 For practical purposes, also: \"\", but not single quotes."
  (interactive)
  (let (p1)
    (skip-chars-backward "^<>([{“「『‹«（〈《〔【〖⦗〘⦅〚⦃\"")
    (setq p1 (point))
    (skip-chars-forward "^<>)]}”」』›»）〉》〕】〗⦘〙⦆〛⦄\"")
    (set-mark p1)
    )
  )

(global-set-key (kbd "H-*") 'select-text-in-quote)


;; ***********
;; compilation
;; ***********
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (unless (fboundp 'ansi-color-apply-on-region)
    (require 'ansi-color))
  (when (fboundp 'ansi-color-apply-on-region)
    (ansi-color-apply-on-region (point-min) (point-max)))
  (toggle-read-only))


;; **********
;; re-buidler
;; **********
(defun reb-query-replace-this-regxp (replace)
  "Uses the regexp built with re-builder to query the target buffer.
This function must be run from within the re-builder buffer, not the target
buffer.

Argument REPLACE String used to replace the matched strings in the buffer.
Subexpression references can be used (\1, \2, etc)."
  (interactive "sReplace with: ")
  (if (eq major-mode 'reb-mode)
      (let ((reg (reb-read-regexp)))
        (select-window reb-target-window)
        (save-excursion
          (beginning-of-buffer)
          (query-replace-regexp reg replace)))
    (message "Not in a re-builder buffer!")))

;; *************
;; sudo commands
;; *************
(defun sudo-shell-command (command &optional output-buffer error-buffer)
  (interactive
   (list
    (read-shell-command "Shell command: " nil nil
                        (let ((filename
                               (cond
                                (buffer-file-name)
                                ((eq major-mode 'dired-mode)
                                 (dired-get-filename nil t)))))
                          (and filename (file-relative-name filename))))
    current-prefix-arg
    shell-command-default-error-buffer))
  (shell-command
   (concat "echo " (read-passwd "Password? ") " | sudo -S " command)
   output-buffer
   error-buffer))

(defun make-elisp-header ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((use-semicolons (looking-at ";"))
          (count (- (save-excursion (end-of-line) (point)) (point))))
      (open-line 1)
      (if use-semicolons
          (insert ";; " (make-string (- count 3) ?\*))
        (insert (make-string count ?\*)))
      (forward-line 2)
      (beginning-of-line)
      (open-line 1)
      (if use-semicolons
          (insert ";; " (make-string (- count 3) ?\*))
        (insert (make-string count ?\*))))))


(defun clean-up-buffer-or-region ()
  "Untabifies, indents and deletes trailing whitespace from buffer or region."
  (interactive)
  (save-excursion
    (unless (region-active-p)
      (mark-whole-buffer))
    (untabify (region-beginning) (region-end))
    (indent-region (region-beginning) (region-end))
    (save-restriction
      (narrow-to-region (region-beginning) (region-end))
      (delete-trailing-whitespace))))

;; Uneven application of white-space is bad, m'kay?
(bind-key "H-N" 'clean-up-buffer-or-region)
(bind-key "H-n" 'cleanup-buffer)

(setq package-archives
      '(("original"    . "http://tromey.com/elpa/")
        ("gnu"         . "http://elpa.gnu.org/packages/")
        ("marmalade"   . "http://marmalade-repo.org/packages/")))
(package-initialize)



;;;_. Packages

;;;_ , el-get

(use-package el-get
  ;;  :disabled t
  :commands (el-get
             el-get-install
             el-get-update
             el-get-list-packages)
  :init
  (defvar el-get-sources nil)

  :config
  (defun el-get-read-status-file ()
    (mapcar #'(lambda (entry)
                (cons (plist-get entry :symbol)
                      `(status "installed" recipe ,entry)))
            el-get-sources))

  (defalias 'el-get-init 'ignore
    "Don't use el-get for making packages available for use."))


;;;_ , abbrev

(use-package abbrev
  :commands abbrev-mode
  :diminish abbrev-mode
  :init
  (hook-into-modes #'abbrev-mode '(text-mode-hook))

  :config
  (progn
    (if (file-exists-p abbrev-file-name)
        (quietly-read-abbrev-file))

    (add-hook 'expand-load-hook
              (lambda ()
                (add-hook 'expand-expand-hook 'indent-according-to-mode)
                (add-hook 'expand-jump-hook 'indent-according-to-mode)))))

;;;_ , ace-jump-mode

(use-package ace-jump-mode
  :bind ("C-. C-s" . ace-jump-mode))

;;;_ , agda

(use-package agda2-mode
  :mode ("\\.agda\\'" . agda2-mode)
  :init
  (use-package agda-input))

;;;_ , allout

(use-package allout
  :diminish allout-mode
  :commands allout-mode
  :config
  (progn
    (defvar allout-unprefixed-keybindings nil)

    (defun my-allout-mode-hook ()
      (dolist (mapping '((?b . allout-hide-bodies)
                         (?c . allout-hide-current-entry)
                         (?l . allout-hide-current-leaves)
                         (?i . allout-show-current-branches)
                         (?e . allout-show-entry)
                         (?o . allout-show-to-offshoot)))
        (bind-key (concat (format-kbd-macro allout-command-prefix)
                          " " (char-to-string (car mapping)))
                  (cdr mapping)
                  allout-mode-map))

      (if (memq major-mode lisp-modes)
          (unbind-key "C-k" allout-mode-map)))

    (add-hook 'allout-mode-hook 'my-allout-mode-hook)))

;;;_ , archive-region

(use-package archive-region
  :commands kill-region-or-archive-region
  :bind ("C-w" . kill-region-or-archive-region))

(use-package apache-mode
  :mode ("\\(\\.htaccess$\\|\\.conf$\\)" . apache-mode)
  )


;;;_ , ansi-term

(use-package ansi-term
  :defer t
  :init
  (progn

    ;;     ;; ;; Use variable width font faces in current buffer
    ;;     (defun my-buffer-face-mode-variable ()
    ;;       ;;   "Set font to a variable width (proportional) fonts in current buffer"
    ;;       (interactive)
    ;;       (setq buffer-face-mode-face '(:family "Menlo For Powerline" :height 100))
    ;;       (text-scale-adjust 1)
    ;;       (buffer-face-mode))

    (setq system-uses-terminfo nil)
    ;; When you use this code, note that dabbrev-completion is C-x /, and yanking is C-y.


    (defun my-term-hook ()
  (goto-address-mode)
  (define-key term-raw-map "\C-y" 'my-term-paste)
  (let ((base03  "#002b36")
        (base02  "#073642")
        (base01  "#586e75")
        (base00  "#657b83")
        (base0   "#839496")
        (base1   "#93a1a1")
        (base2   "#eee8d5")
        (base3   "#fdf6e3")
        (yellow  "#b58900")
        (orange  "#cb4b16")
        (red     "#dc322f")
        (magenta "#d33682")
        (violet  "#6c71c4")
        (blue    "#268bd2")
        (cyan    "#2aa198")
        (green   "#859900"))
     (setq ansi-term-color-vector
           (vconcat `(ansi-term-color-vector [,base01 ,red ,green ,yellow ,blue ,magenta ,cyan ,base03])))
    ))

    ;; ["#586e75" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#002b36"]
    ;; ["#586e75" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#002b36"]
    ;; error in process filter: face-attribute: Wrong type argument: symbolp,
    ;; "#586e75"
    ;; error in process filter: Invalid face: ansi-term-color-vector
    ;; (vconcat `(unspecified ,base02 ,red ,green ,yellow ,blue
          ;;                        ,magenta ,cyan ,base2)))))

     ;; `(ansi-term-color-vector [,base01 ,red ,green ,yellow ,blue ,magenta ,cyan ,base03])

    ;; (add-hook 'term-mode-hook 'my-term-hook)


    (add-hook 'term-mode-hook
              '(lambda ()
                 (linum-mode 0)
                 (term-set-escape-char ?\C-z)
                 (term-set-escape-char ?\C-x)
                 (define-key term-raw-map "\C-c" 'term-interrupt-subjob)
                 (define-key term-raw-map (kbd "M-x") 'execute-extended-command)
                 (define-key term-raw-escape-map "/"
                   (lambda ()
                     (interactive)
                     (let ((beg (point)))
                       (dabbrev-expand nil)
                       (kill-region beg (point)))
                     (term-send-raw-string (substring-no-properties (current-kill 0)))))
                 (setq autopair-dont-activate t)
                 (setq ac-auto-start nil)
                 (visual-line-mode -1)
                 ;; (my-buffer-face-mode-variable)
                 ))


    (defun my-term-paste (&optional string)
      (interactive)
      (process-send-string
       (get-buffer-process (current-buffer))
       (if string string (current-kill 0))))

    (defun my-term-pasteboard-paste ()
      (interactive)
      (process-send-string
       (get-buffer-process (current-buffer))
       (ns-get-pasteboard)))

    (add-hook 'term-exec-hook '(lambda ()
                                 (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix)
                                 (goto-address-mode)
                                 (define-key term-raw-map (kbd "C-y") 'my-term-paste)
                                 (define-key term-raw-map (kbd "s-v") 'my-term-pasteboard-paste)
                                 ;;                                  (let ((base03 "#002b36")
                                 ;;                                        (base02 "#073642")
                                 ;;                                        (base01 "#586e75")
                                 ;;                                        (base00 "#657b83")
                                 ;;                                        (base0 "#839496")
                                 ;;                                        (base1 "#93a1a1")
                                 ;;                                        (base2 "#eee8d5")
                                 ;;                                        (base3 "#fdf6e3")
                                 ;;                                        (yellow "#b58900")
                                 ;;                                        (orange "#cb4b16")
                                 ;;                                        (red "#dc322f")
                                 ;;                                        (magenta "#d33682")
                                 ;;                                        (violet "#6c71c4")
                                 ;;                                        (blue "#268bd2")
                                 ;;                                        (cyan "#2aa198")
                                 ;;                                        (green "#859900"))
                                 ;;                                    (setq ansi-term-color-vector
                                 ;;                                          (vconcat `(unspecified ,base02 ,red ,green ,yellow ,blue
                                 ;;                                                                 ,magenta ,cyan ,base2))))
                                 ))

    (defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
      (if (memq (process-status proc) '(signal exit))
          (let ((buffer (process-buffer proc)))
            ad-do-it
            (kill-buffer buffer))
        ad-do-it))
    (ad-activate 'term-sentinel)

    (defvar my-term-shell "/usr/local/bin/zsh")
    (defadvice ansi-term (before force-bash)
      (interactive (list my-term-shell)))
    (ad-activate 'ansi-term)

    (setenv "PATH" (shell-command-to-string "echo $PATH"))

    (defun visit-ansi-term ()
      (interactive)
      "Creates an ansi-term and switches to it. If a buffer with name already exists, we simply switch to it."
      (let ((buffer-of-name (get-buffer (concat "*ansi-term-" (wg-name (wg-current-workgroup)))))
            ;;        (default-directory "/home/www")
            (term-cmd "/bin/zsh")
            )
        (cond ((bufferp buffer-of-name) ;If the buffer exists, switch to it (assume it is a shell)
               (switch-to-buffer buffer-of-name))
              ( t
                (progn
                  (ansi-term term-cmd)
                                        ;(process-send-string (get-buffer-process new-buff-name) (concat "cd " localdir "\n"))
                  (rename-buffer  (concat "*ansi-term-" (wg-name (wg-current-workgroup)))))))))))

(use-package ansi-color
  :config
  (progn
    (autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
    (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
    ))


;;;_ , ascii

(use-package ascii
  :commands (ascii-on ascii-toggle)
  :init
  (progn
    (defun ascii-toggle ()
      (interactive)
      (if ascii-display
          (ascii-off)
        (ascii-on)))

    (bind-key "C-c e A" 'ascii-toggle)))

;;;_ , auctex

(use-package tex-site
  :load-path "site-lisp/auctex/preview/"
  :defines (latex-help-cmd-alist latex-help-file)
  :mode ("\\.tex\\'" . latex-mode)
  :config
  (progn
    (defun latex-help-get-cmd-alist ()  ;corrected version:
      "Scoop up the commands in the index of the latex info manual.
   The values are saved in `latex-help-cmd-alist' for speed."
      ;; mm, does it contain any cached entries
      (if (not (assoc "\\begin" latex-help-cmd-alist))
          (save-window-excursion
            (setq latex-help-cmd-alist nil)
            (Info-goto-node (concat latex-help-file "Command Index"))
            (goto-char (point-max))
            (while (re-search-backward "^\\* \\(.+\\): *\\(.+\\)\\." nil t)
              (let ((key (buffer-substring (match-beginning 1) (match-end 1)))
                    (value (buffer-substring (match-beginning 2)
                                             (match-end 2))))
                (add-to-list 'latex-help-cmd-alist (cons key value))))))
      latex-help-cmd-alist)

    (use-package latex-mode
      :defer t
      :config
      (progn
        (use-package preview)
        (info-lookup-add-help :mode 'latex-mode
                              :regexp ".*"
                              :parse-rule "\\\\?[a-zA-Z]+\\|\\\\[^a-zA-Z]"
                              :doc-spec '(("(latex2e)Concept Index" )
                                          ("(latex2e)Command Index"))))))
  ;; (org-babel-load-file "~/git/foss/emacs-starter-kit-social-sciences/starter-kit-latex.org")
  )

;;;_ , auto-complete

(use-package auto-complete-config
  :load-path ("site-lisp/ac/auto-complete"
              "site-lisp/ac/ac-source-elisp"
              "site-lisp/ac/ac-source-semantic"
              "site-lisp/ac/ac-yasnippet"
              "site-lisp/ac/fuzzy-el"
              "site-lisp/ac/popup-el")
  :diminish auto-complete-mode
  :init
  (progn
    (use-package pos-tip)
    (use-package yasnippet)
    (ac-config-default))

  :config
  (progn
    (ac-set-trigger-key "TAB")
    (setq ac-use-menu-map t)

    (bind-key "A-M-?" 'ac-last-help)
    (unbind-key "C-s" ac-completing-map)))

;;;_ , autopair

(use-package autopair
  :disabled t
  :commands autopair-mode
  :diminish autopair-mode
  :init
  (hook-into-modes #'autopair-mode '(c-mode-common-hook
                                     text-mode-hook
                                     ruby-mode-hook
                                     python-mode-hook
                                     sh-mode-hook)))
;;;_ , autorevert

(use-package autorevert
  :commands auto-revert-mode
  :diminish auto-revert-mode
  :init
  (add-hook 'find-file-hook
            #'(lambda ()
                (auto-revert-mode 1))))

;;;_ , backup-each-save

(use-package backup-each-save
  :defer t
  :init
  (progn
    (autoload 'backup-each-save "backup-each-save")
    (add-hook 'after-save-hook 'backup-each-save)

    (defun my-make-backup-file-name (file)
      (make-backup-file-name-1 (file-truename file)))

    (defun show-backups ()
      (interactive)
      (require 'find-dired)
      (let* ((file (make-backup-file-name (buffer-file-name)))
             (dir (file-name-directory file))
             (args (concat "-iname '" (file-name-nondirectory file)
                           ".~*~'"))
             (dired-buffers dired-buffers)
             (find-ls-option '("-print0 | xargs -0 ls -lta" . "-lta")))
        ;; Check that it's really a directory.
        (or (file-directory-p dir)
            (error "Backup directory does not exist: %s" dir))
        (with-current-buffer (get-buffer-create "*Backups*")
          (let ((find (get-buffer-process (current-buffer))))
            (when find
              (if (or (not (eq (process-status find) 'run))
                      (yes-or-no-p "A `find' process is running; kill it? "))
                  (condition-case nil
                      (progn
                        (interrupt-process find)
                        (sit-for 1)
                        (delete-process find))
                    (error nil))
                (error "Cannot have two processes in `%s' at once"
                       (buffer-name)))))

          (widen)
          (kill-all-local-variables)
          (setq buffer-read-only nil)
          (erase-buffer)
          (setq default-directory dir
                args (concat find-program " . "
                             (if (string= args "")
                                 ""
                               (concat
                                (shell-quote-argument "(")
                                " " args " "
                                (shell-quote-argument ")")
                                " "))
                             (if (string-match "\\`\\(.*\\) {} \\(\\\\;\\|+\\)\\'"
                                               (car find-ls-option))
                                 (format "%s %s %s"
                                         (match-string 1 (car find-ls-option))
                                         (shell-quote-argument "{}")
                                         find-exec-terminator)
                               (car find-ls-option))))
          ;; Start the find process.
          (message "Looking for backup files...")
          (shell-command (concat args "&") (current-buffer))
          ;; The next statement will bomb in classic dired (no optional arg
          ;; allowed)
          (dired-mode dir (cdr find-ls-option))
          (let ((map (make-sparse-keymap)))
            (set-keymap-parent map (current-local-map))
            (define-key map "\C-c\C-k" 'kill-find)
            (use-local-map map))
          (make-local-variable 'dired-sort-inhibit)
          (setq dired-sort-inhibit t)
          (set (make-local-variable 'revert-buffer-function)
               `(lambda (ignore-auto noconfirm)
                  (find-dired ,dir ,find-args)))
          ;; Set subdir-alist so that Tree Dired will work:
          (if (fboundp 'dired-simple-subdir-alist)
              ;; will work even with nested dired format (dired-nstd.el,v 1.15
              ;; and later)
              (dired-simple-subdir-alist)
            ;; else we have an ancient tree dired (or classic dired, where
            ;; this does no harm)
            (set (make-local-variable 'dired-subdir-alist)
                 (list (cons default-directory (point-min-marker)))))
          (set (make-local-variable 'dired-subdir-switches) find-ls-subdir-switches)
          (setq buffer-read-only nil)
          ;; Subdir headlerline must come first because the first marker in
          ;; subdir-alist points there.
          (insert "  " dir ":\n")
          ;; Make second line a ``find'' line in analogy to the ``total'' or
          ;; ``wildcard'' line.
          (insert "  " args "\n")
          (setq buffer-read-only t)
          (let ((proc (get-buffer-process (current-buffer))))
            (set-process-filter proc (function find-dired-filter))
            (set-process-sentinel proc (function find-dired-sentinel))
            ;; Initialize the process marker; it is used by the filter.
            (move-marker (process-mark proc) 1 (current-buffer)))
          (setq mode-line-process '(":%s")))))

    (bind-key "C-x ~" 'show-backups))

  :config
  (progn
    (defun backup-each-save-filter (filename)
      (not (string-match
            (concat "\\(^/tmp\\|\\.emacs\\.d/data\\(-alt\\)?/"
                    "\\|\\.newsrc\\(\\.eld\\)?\\)")
            filename)))

    (setq backup-each-save-filter-function 'backup-each-save-filter)

    (defun my-dont-backup-files-p (filename)
      (unless (string-match filename "/\\(archive/sent/\\|recentf$\\)")
        (normal-backup-enable-predicate filename)))

    (setq backup-enable-predicate 'my-dont-backup-files-p)))

;;;_ , basecamp

(use-package basecamp
  :init
  (progn
    (defun syncbasecamp ()
      (interactive)
      (http-get "http://floatsolutions.com/docs/basecamp/index.php?accesskey=sdf6SDFwr88sdfASDdye76qw76876DFGDfgsdf" nil 'ignore nil "basecamp.org" nil)
      (org-mode)
      (save-buffer))

    (defun completebasecamp (todoid)
      (interactive)
      (http-get (concatenate 'string "http://floatsolutions.com/docs/basecamp/index.php?accesskey=sdf6SDFwr88sdfASDdye76qw76876DFGDfgsdf&complete=" todoid) nil 'ignore nil "basecamp.org" nil)
      (org-mode)
      (save-buffer))

    (defun basecamp-showlist ()
      (interactive)
      (find-file-other-window "~/org/basecamp.org")
      (syncbasecamp))

    (defun basecamp-showprojects ()
      (interactive)
      (find-file-other-window "~/org/projects.org"))
    ))

;;;_ , bbdb

(use-package bbdb-com
  :commands bbdb-create
  :bind ("M-B" . bbdb))


;;;_ , bbdb

(use-package bbdb
  :if (not running-alternate-emacs)
  :init
  (progn
    (bbdb-initialize 'gnus 'message)
    (setq bbdb-accept-name-mismatch                 t
          bbdb-completion-display-record            nil
          bbdb-message-all-addresses                t
          bbdb-mua-update-interactive-p             '(create . query))
    (setq rs-bbdb-ignored-from-list '(
                                      "-confirm"
                                      "-request@kgnu.org"
                                      "@public.gmane.org"
                                      "DAEMON"
                                      "MAILER-DAEMON"
                                      "bozo@dev.null.invalid"
                                      "confirm-nomail"
                                      "daemon"
                                      "damon.haley"
                                      "dhaley"
                                      "do-not-reply"
                                      "emacs-orgmode-confirm"
                                      "facebookmail"
                                      "gnulist"
                                      "lists.math.uh.edu"
                                      "mailman-owner"
                                      "no.?reply"
                                      "noreply"
                                      "post <at> gwene.org"
                                      "post@gwene.org"
                                      "privacy-noreply"
                                      "twitter"
                                      "vinylisl"
                                      "webappsec-return"))

    (setq bbdb-ignore-message-alist
          `(("From" . , (regexp-opt rs-bbdb-ignored-from-list))))


    ;; ; NOTE: there can be only one entry per header (such as To, From)
    ;;       ;; http://flex.ee.uec.ac.jp/texi/bbdb/bbdb_11.html
    ;;       bbdb-ignore-some-messages-alist
    ;;       `(("From" . ,(concat "no.?reply\\|DAEMON\\|daemon\\|facebookmail\\|"
    ;;                            "gmane\\|ebay\\|amazon\\|tfl\\|trenitalia"))))


    (defun message-read-from-minibuffer (prompt &optional initial-contents)
      "Read from the minibuffer while providing abbrev expansion."
      (bbdb-completing-read-mails prompt initial-contents))
    ))


                                        ;_ , bm

(use-package bm
  :pre-init
  (progn
    (defvar ctl-period-breadcrumb-map)
    (define-prefix-command 'ctl-period-breadcrumb-map)
    (bind-key "C-. c" 'ctl-period-breadcrumb-map))

  :bind (("C-. c b" . bm-last-in-previous-buffer)
         ("C-. c f" . bm-first-in-next-buffer)
         ("C-. c g" . bm-previous)
         ("C-. c l" . bm-show-all)
         ("C-. c c" . bm-toggle)
         ("C-. c m" . bm-toggle)
         ("C-. c n" . bm-next)
         ("C-. c p" . bm-previous)))

;;;_ , bookmark

(use-package bookmark
  :bind ("C-c j" . bookmark-jump)
  :defer t
  :config
  (progn
    (use-package bookmark+)

    (defun my-bookmark-set ()
      (interactive)
      (flet ((bmkp-completing-read-lax
              (prompt &optional default alist pred hist)
              (completing-read prompt alist pred nil nil hist default)))
        (call-interactively #'bookmark-set)))

    (bind-key "C-x r m" 'my-bookmark-set)))

;;;_ , browse-kill-ring+

(use-package browse-kill-ring+)

;; (use-package browse-kill-ring
;;   :bind ("M-y" . browse-kill-ring))
;;
;; make hippie expand behave itself
(setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-list
                                         try-expand-line
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))

;;;_ , cmake-mode

(use-package cmake-mode
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'"         . cmake-mode)))


;;;_ , make-mode

(use-package makefile-mode
  :mode ((".make\\'" . makefile-gmake-mode))
  :config
  (progn
    (require 'make-mode)

    (defconst makefile-nmake-statements
      `("!IF" "!ELSEIF" "!ELSE" "!ENDIF" "!MESSAGE" "!ERROR" "!INCLUDE" ,@makefile-statements)
      "List of keywords understood by nmake.")

    (defconst makefile-nmake-font-lock-keywords
      (makefile-make-font-lock-keywords
       makefile-var-use-regex
       makefile-nmake-statements
       t))

    (define-derived-mode makefile-nmake-mode makefile-mode "nMakefile"
      "An adapted `makefile-mode' that knows about nmake."
      (setq font-lock-defaults
            `(makefile-nmake-font-lock-keywords ,@(cdr font-lock-defaults))))

    )
  )


;;;_ , color-moccur

(let ((ad-redefinition-action 'accept))
  (use-package color-moccur
    :commands (isearch-moccur isearch-all)
    :bind ("M-s O" . moccur)
    :init
    (progn
      (bind-key "M-o" 'isearch-moccur isearch-mode-map)
      (bind-key "M-O" 'isearch-moccur-all isearch-mode-map))

    :config
    (use-package moccur-edit)))

;;;_ , copy-code

(use-package copy-code
  :bind ("A-M-W" . copy-code-as-rtf))

;;;_ , crosshairs

(use-package crosshairs
  :bind ("M-o c" . crosshairs-mode))


(use-package csv-mode
  :comamnds csv-mode
  :mode ("\\.[Cc][Ss][Vv]\\'" . csv-mode))

;;;_ , css-mode
(use-package css-mode
  :mode ("\\.css$" . css-mode)
  :config
  (progn
    (setq css-indent-offset 2
          cssm-indent-level '2)
    (define-keys css-mode-map
      '(("<return>" newline-and-indent)))
    (add-hook 'css-mode-hook
              (lambda ()
                ;; (paredit-mode 1)
                (rainbow-mode 1)))))

(use-package cursor-chg
  :init
  (progn
    (change-cursor-mode 1)
    (toggle-cursor-type-when-idle 1)))


;;;_ , ibuffer

(use-package ibuffer
  :defer t
  :bind ("C-x C-b" . ibuffer)
  :config
  (progn
    (use-package ibuffer-git)
    (define-ibuffer-column size-h
      (:name "Size" :inline t)
      (cond
       ((> (buffer-size) 1000)
        (format "%7.1fk" (/ (buffer-size) 1000.0)))
       ((> (buffer-size) 1000000)
        (format "%7.1fM" (/ (buffer-size) 1000000.0)))
       (t
        (format "%8d" (buffer-size)))))

    (setq
     ibuffer-default-sorting-mode 'filename/process
     ibuffer-eliding-string "…"
     ibuffer-compile-formats t
     ibuffer-formats '((mark modified read-only
                             " " (name 25 25 :left :elide)
                             " " (size-h 9 -1 :right)
                             " " (mode 7 7 :left :elide)
                             " " (git-status 8 8 :left)
                             " " filename-and-process)
                       (mark " " (name 16 -1) " " filename))
     ibuffer-show-empty-filter-groups nil
     ibuffer-saved-filter-groups
     (quote (("default"
              ("emacs lisp" (mode . emacs-lisp-mode))
              ("python" (mode . python-mode))
              ("ruby" (mode . ruby-mode))
              ("coffee-script" (mode . coffee-mode))
              ("java-script" (or
                              (mode . js-mode)
                              (mode . js2-mode)))
              ("action-script" (mode . actionscript-mode))
              ("java" (mode . java-mode))
              ("html" (or
                       (mode . html-mode)
                       (mode . web-mode)
                       (mode . haml-mode)))
              ("xml" (mode . nxml-mode))
              ("css preprocessor" (or
                                   (mode . scss-mode)
                                   (mode . sass-mode)
                                   (mode . stylus-mode)))
              ("css" (mode . css-mode))
              ("org agenda" (mode . org-agenda-mode))
              ("org" (or
                      (mode . org-mode)
                      (name . "^\\*Calendar\\*$")
                      (name . "^diary$")))
              ("text misc" (or
                            (mode . text-mode)
                            (mode . rst-mode)
                            (mode . markdown-mode)))
              ("w3m" (mode . w3m-mode))
              ("git" (or
                      (mode . magit-log-edit-mode)
                      (mode . magit-log)))
              ("dired" (mode . dired-mode))
              ("help" (or
                       (mode . Info-mode)
                       (mode . help-mode)
                       (mode . Man-mode)
                       (mode . apropos-mode)
                       (mode . woman-mode)))
              ("*kite*" (name . "^\\*kite.*\\*"))
              ("*helm*" (name . "^\\*helm.*\\*"))
              ("*buffer*" (name . "\\*.*\\*"))
              ("ssh"
               (or
                (name . "\\*tramp")
                (name . "^\\*debug tramp")
                ))
              ("emacs"
               (or
                (mode . occur-mode)
                (mode . bookmark-bmenu-mode)
                (mode . help-mode)
                (name . "^\\*scratch\\*$")
                (name . "^\\*Messages\\*$")

                (name . "^\\*Compile-Log\\*$")
                (name . "^\\*Backtrace\\*$")
                (name . "^\\*info\\*$")
                (name . "^\\*Occur\\*$")
                (name . "^\\*grep\\*$")
                (name . "^\\*Process List\\*$")
                (name . "^\\*gud\\*$")
                (name . "^\\*compilation\\*$")
                (name . "^\\*Kill Ring\\*$")
                ))
              ("latex" (or (mode . latex-mode)
                           (mode . LaTeX-mode)
                           (mode . bibtex-mode)
                           (mode . reftex-mode)))
              ("irc"
               (or
                (name . "^\\*Sauron\\*$")
                (mode . garak-mode)
                (name . "^\\*Garak\\*$")
                (mode . erc-mode)
                (mode . twittering-mode)
                (name . "^\\*scratch\\* (irc)$")
                ))
              ("gnus" (or
                       (mode . message-mode)
                       (mode . bbdb-mode)
                       (mode . mail-mode)
                       (mode . gnus-group-mode)
                       (mode . gnus-summary-mode)
                       (mode . gnus-article-mode)
                       (name . "^\\.bbdb$")
                       (name . "^\\*fetchmail\\*$")
                       (name . "^\\.newsrc-dribble")
                       (name . "^\\*gnus trace\\*$")
                       (name . "^\\*scratch\\* (gnus)$")
                       ))
              ("Magit" (name . "\*magit"))
              ))))

    (add-hook 'ibuffer-mode-hook
              #'(lambda ()
                  (ibuffer-switch-to-saved-filter-groups "default")))))



;;;_ , iflipb

(use-package iflipb
  :disabled t
  :commands (iflipb-next-buffer iflipb-previous-buffer)
  :bind (("S-<tab>" . my-iflipb-next-buffer)
         ("A-S-<tab>" . my-iflipb-previous-buffer))
  :init
  (progn
    (defvar my-iflipb-auto-off-timeout-sec 2)
    (defvar my-iflipb-auto-off-timer-canceler-internal nil)
    (defvar my-iflipb-ing-internal nil)

    (defun my-iflipb-auto-off ()
      (message nil)
      (setq my-iflipb-auto-off-timer-canceler-internal nil
            my-iflipb-ing-internal nil))

    (defun my-iflipb-next-buffer (arg)
      (interactive "P")
      (iflipb-next-buffer arg)
      (if my-iflipb-auto-off-timer-canceler-internal
          (cancel-timer my-iflipb-auto-off-timer-canceler-internal))
      (run-with-idle-timer my-iflipb-auto-off-timeout-sec 0 'my-iflipb-auto-off)
      (setq my-iflipb-ing-internal t))

    (defun my-iflipb-previous-buffer ()
      (interactive)
      (iflipb-previous-buffer)
      (if my-iflipb-auto-off-timer-canceler-internal
          (cancel-timer my-iflipb-auto-off-timer-canceler-internal))
      (run-with-idle-timer my-iflipb-auto-off-timeout-sec 0 'my-iflipb-auto-off)
      (setq my-iflipb-ing-internal t)))

  :config
  (progn
    (setq iflipb-always-ignore-buffers
          "\\`\\( \\|diary\\|ipa\\|\\.newsrc-dribble\\'\\)"
          iflipb-wrap-around t)

    (defun iflipb-first-iflipb-buffer-switch-command ()
      "Determines whether this is the first invocation of
iflipb-next-buffer or iflipb-previous-buffer this round."
      (not (and (or (eq last-command 'my-iflipb-next-buffer)
                    (eq last-command 'my-iflipb-previous-buffer))
                my-iflipb-ing-internal)))))



;;;;_ , dash.el

(use-package dash
  :load-path "dash.el")

;;;_ , debbugs

(use-package debbugs-gnu
  :commands (debbugs-gnu debbugs-gnu-search))

;;;_ , dedicated

(use-package dedicated
  :bind ("C-. d" . dedicated-mode))

;;;_ , diff-mode

(use-package diff-mode
  :commands diff-mode
  :config
  (use-package diff-mode-))

;;;_ , dired

(use-package dired
  :defer t
  :init
  (progn
    (defvar mark-files-cache (make-hash-table :test #'equal))

    (defun mark-similar-versions (name)
      (let ((pat name))
        (if (string-match "^\\(.+?\\)-[0-9._-]+$" pat)
            (setq pat (match-string 1 pat)))
        (or (gethash pat mark-files-cache)
            (ignore (puthash pat t mark-files-cache))))))
  :config
  (progn
    ;; Also auto refresh dired, but be quiet about it
    (setq global-auto-revert-non-file-buffers t)
    (setq auto-revert-verbose nil)
    (defun dired-package-initialize ()
      (unless (featurep 'runner)
        (use-package dired-x)
        ;; (use-package dired-async)
        (use-package dired-sort-map)
        (use-package runner)
        (use-package dired-details-hide
          :commands dired-details-toggle)

        (bind-key "l" 'dired-up-directory dired-mode-map)
        (bind-key "H" 'dired-details-toggle dired-mode-map)

        (defun my-dired-switch-window ()
          (interactive)
          (if (eq major-mode 'sr-mode)
              (call-interactively #'sr-change-window)
            (call-interactively #'other-window)))

        ;;http://puntoblogspot.blogspot.com/2010/06/kill-buffers-illustrated-emacs.html
        (defun kill-all-dired-buffers()
          "Kill all dired buffers."
          (interactive)
          (save-excursion
            (let((count 0))
              (dolist(buffer (buffer-list))
                (set-buffer buffer)
                (when (equal major-mode 'dired-mode)
                  (setq count (1+ count))
                  (kill-buffer buffer)))
              (message "Killed %i dired buffer(s)." count ))))

        (bind-key "<tab>" 'my-dired-switch-window dired-mode-map)

        (bind-key "M-!" 'async-shell-command dired-mode-map)
        (unbind-key "M-G" dired-mode-map)
        (unbind-key "M-s f" dired-mode-map)

        (defadvice dired-omit-startup (after diminish-dired-omit activate)
          "Make sure to remove \"Omit\" from the modeline."
          (diminish 'dired-omit-mode) dired-mode-map)

        (defadvice dired-next-line (around dired-next-line+ activate)
          "Replace current buffer if file is a directory."
          ad-do-it
          (while (and  (not  (eobp)) (not ad-return-value))
            (forward-line)
            (setq ad-return-value(dired-move-to-filename)))
          (when (eobp)
            (forward-line -1)
            (setq ad-return-value(dired-move-to-filename))))

        (defadvice dired-previous-line (around dired-previous-line+ activate)
          "Replace current buffer if file is a directory."
          ad-do-it
          (while (and  (not  (bobp)) (not ad-return-value))
            (forward-line -1)
            (setq ad-return-value(dired-move-to-filename)))
          (when (bobp)
            (call-interactively 'dired-next-line)))

        (defvar dired-omit-regexp-orig (symbol-function 'dired-omit-regexp))

        ;; Omit files that Git would ignore
        (defun dired-omit-regexp ()
          (let ((file (expand-file-name ".git"))
                parent-dir)
            (while (and (not (file-exists-p file))
                        (progn
                          (setq parent-dir
                                (file-name-directory
                                 (directory-file-name
                                  (file-name-directory file))))
                          ;; Give up if we are already at the root dir.
                          (not (string= (file-name-directory file)
                                        parent-dir))))
              ;; Move up to the parent dir and try again.
              (setq file (expand-file-name ".git" parent-dir)))
            ;; If we found a change log in a parent, use that.
            (if (file-exists-p file)
                (let ((regexp (funcall dired-omit-regexp-orig))
                      (omitted-files
                       (shell-command-to-string "git clean -d -x -n")))
                  (if (= 0 (length omitted-files))
                      regexp
                    (concat
                     regexp
                     (if (> (length regexp) 0)
                         "\\|" "")
                     "\\("
                     (mapconcat
                      #'(lambda (str)
                          (concat
                           "^"
                           (regexp-quote
                            (substring str 13
                                       (if (= ?/ (aref str (1- (length str))))
                                           (1- (length str))
                                         nil)))
                           "$"))
                      (split-string omitted-files "\n" t)
                      "\\|")
                     "\\)")))
              (funcall dired-omit-regexp-orig)

              (define-key dired-mode-map [?@] 'dired-up-directory)

              )))))

    (eval-after-load "dired-aux"
      '(defun dired-do-async-shell-command (command &optional arg file-list)
         "Run a shell command COMMAND on the marked files asynchronously.

Like `dired-do-shell-command' but if COMMAND doesn't end in ampersand,
adds `* &' surrounded by whitespace and executes the command asynchronously.
The output appears in the buffer `*Async Shell Command*'."
         (interactive
          (let ((files (dired-get-marked-files t current-prefix-arg)))
            (list
             ;; Want to give feedback whether this file or marked files are
             ;; used:
             (dired-read-shell-command "& on %s: " current-prefix-arg files)
             current-prefix-arg
             files)))
         (unless (string-match "[ \t][*?][ \t]" command)
           (setq command (concat command " *")))
         (unless (string-match "&[ \t]*\\'" command)
           (setq command (concat command " &")))
         (dired-do-shell-command command arg file-list)))

    (add-hook 'dired-mode-hook 'dired-package-initialize)

    (defun dired-double-jump (first-dir second-dir)
      (interactive
       (list (ido-read-directory-name "First directory: "
                                      (expand-file-name "~")
                                      nil nil "dl/")
             (ido-read-directory-name "Second directory: "
                                      (expand-file-name "~")
                                      nil nil "Archives/")))
      (dired first-dir)
      (dired-other-window second-dir))

    (bind-key "C-c J" 'dired-double-jump)))

;;;_ , doxymacs

(use-package doxymacs
  :disabled t
  :load-path "site-lisp/doxymacs/lisp/")

;;;_ , dvc

(use-package dvc-autoloads
  :load-path "site-lisp/dvc/lisp/")

;;;_ , ediff

(use-package ediff
  :pre-init
  (progn
    (defvar ctl-period-equals-map)
    (define-prefix-command 'ctl-period-equals-map)
    (bind-key "C-. =" 'ctl-period-equals-map)

    (bind-key "C-. = c" 'compare-windows)) ; not an ediff command, but it fits

  :bind (("C-. = b" . ediff-buffers)
         ("C-. = B" . ediff-buffers3)
         ("C-. = =" . ediff-files)
         ("C-. = f" . ediff-files)
         ("C-. = F" . ediff-files3)
         ("C-. = r" . ediff-revision)
         ("C-. = p" . ediff-patch-file)
         ("C-. = P" . ediff-patch-buffer)
         ("C-. = l" . ediff-regions-linewise)
         ("C-. = w" . ediff-regions-wordwise))
  :config
  (use-package ediff-keep))

;;;_ , edit-server

(use-package edit-server
  :if (and window-system (not running-alternate-emacs)
           (not noninteractive))
  :init
  (progn
    (add-hook 'after-init-hook 'server-start t)
    (add-hook 'after-init-hook 'edit-server-start t)))

;;;_ , emms

(use-package emms-setup
  :load-path "site-lisp/emms/lisp"
  :defines emms-info-functions
  :commands (emms-all emms-devel)
  :init
  (progn
    (defvar emms-initialized nil)

    (defun my-emms ()
      (interactive)
      (unless emms-initialized
        (emms-devel)
        (emms-default-players)
        (require 'emms-info-libtag)
        (setq emms-info-functions '(emms-info-libtag))
        (setq emms-initialized t))
      (call-interactively #'emms-smart-browse))

    (bind-key "C-. M" 'my-emms))

  :config
  (progn
    (bind-key "S-<f7>" 'emms-previous)
    (bind-key "S-<f8>" 'emms-pause)
    (bind-key "S-<f9>" 'emms-next)
    (bind-key "S-<f10>" 'emms-stop)

    (defun emms-player-mplayer-volume-up ()
      "Depends on mplayer’s -slave mode."
      (interactive)
      (process-send-string
       emms-player-simple-process-name "volume 1\n"))

    (defun emms-player-mplayer-volume-down ()
      "Depends on mplayer’s -slave mode."
      (interactive)
      (process-send-string
       emms-player-simple-process-name "volume -1\n"))

    (bind-key "C-. C--" 'emms-player-mplayer-volume-down)
    (bind-key "C-. C-=" 'emms-player-mplayer-volume-up)))


(use-package pcache
  :defer t
  :init
  (progn
    (setq
     pcache-directory
     (let ((dir (expand-file-name "pcache/" user-cache-directory)))
       (make-directory dir t)
       dir))))

(defun eproject-active ()
  "Check if eproject is available and it's minor mode enabled."
  (and (boundp 'eproject-mode) eproject-mode))

(use-package eproject
  :defer t
  :commands (eproject-root
             eproject-maybe-turn-on)
  :mode (("\\.eproject\\'" . dot-eproject-mode))
  :diminish ((eproject-mode . "prj"))
  :init
  (progn
    (setq eproject-completing-read-function 'eproject--ido-completing-read)
    (use-package eproject-extras
      :commands (eproject-open-all-project-files
                 eproject-ibuffer
                 eproject-find-file
                 eproject-grep)
      :config
      (progn
        (unbind-key "C-c C-f" eproject-mode-map)
        (unbind-key "C-c C-b" eproject-mode-map)))
    (use-package eproject-tasks
      :commands (helm-eproject-tasks))

    (defun my-eproject-maybe-turn-on ()
      "Ignores errors"
      (interactive)
      (condition-case msg
          (eproject-maybe-turn-on)
        (error (message "Ignored eproject-maybe-turn-on error: %s" msg))))

    (hook-into-modes #'my-eproject-maybe-turn-on
                     my-prog-mode-hooks)
    (hook-into-modes #'my-eproject-maybe-turn-on
                     my-css-like-mode-hooks)
    (hook-into-modes #'my-eproject-maybe-turn-on
                     my-html-like-mode-hooks)
    (hook-into-modes #'my-eproject-maybe-turn-on
                     '(dired-mode-hook)))
  :config
  (progn
    ;; Order of these project definitions are important
    ;; The latest defined project types are checked first.

    (define-project-type generic () nil
      :relevant-files (".*")
      :irrelevant-files ("^[.]" "^[#]" ".git/" ".hg/" ".bzr/" "_darcs/")
      :file-name-map (lambda (root) (lambda (root file) file))
      :local-variables (lambda (root) (lambda (root file) nil))
      :config-file ".eproject")

    (define-project-type generic-eproject (generic) (look-for ".eproject"))
    (define-project-type generic-git (generic) (look-for ".git/"))
    (define-project-type generic-hg (generic) (look-for ".hg"))
    (define-project-type generic-bzr (generic) (look-for ".bzr"))
    (define-project-type generic-darcs (generic) (look-for "_darcs"))

    (define-project-type nodejs (generic)
      (look-for "package.json")
      :relevant-files ("\\.js\\'" "\\.coffee\\'"
                       "\\.html\\'" "\\.hb\\'"
                       "\\.json\\'" "\\.y[a]?ml\\'" "\\.xml\\'"
                       "\\.txt\\'" "\\.markdown\\'" "\\.md\\'"
                       "\\.rst\\'" "\\.org\\'"
                       "\\.css\\'" "\\.sass\\'" "\\.scss\\'" "\\.styl\\'")
      :irrelevant-files ("^[.]" "^[#]"
                         "node_modules/.*"
                         "\\.sass-cache/.*"
                         "tmp/.*"
                         "log/.*"
                         "\\.min\\.js\\'"
                         "\\.min\\.css\\'")
      :main-file "package.json")

    (define-project-type web (generic) nil
      :relevant-files ("\\.py\\'""\\.rb\\'"
                       "\\.js\\'" "\\.coffee\\'"
                       "\\.html\\'" "\\.haml\\'" "\\.hb\\'"
                       "\\.json\\'" "\\.y[a]?ml\\'" "\\.xml\\'"
                       "\\.txt\\'" "\\.markdown\\'" "\\.md\\'"
                       "\\.rst\\'" "\\.org\\'"
                       "\\.css\\'" "\\.sass\\'" "\\.scss\\'" "\\.styl\\'")
      :irrelevant-files ("node_modules/.*"
                         "\\.sass-cache/.*"
                         "tmp/.*"
                         "log/.*"
                         "\\.min\\.js\\'"
                         "\\.min\\.css\\'"))

    (define-project-type ruby-on-rails (web)
      (and (look-for "Gemfile") (look-for "config/application.rb"))
      :irrelevant-files ("app/assets/images/.*"
                         "tmp/.*"
                         "log/.*"
                         "public/.*"
                         "vendor/.*"
                         "\\.min\\.js\\'"
                         "\\.min\\.css\\'")
      :main-file "Gemfile")

    (define-project-type django (web)
      (look-for "manage.py")
      :irrelevant-files ("media/.*"
                         "contrib/.*"
                         ".*\\.sqlite?"
                         "node_modules/.*"
                         "\\.sass-cache/.*"
                         "static/tinymce/.*"
                         "\\.min\\.js\\'"
                         "\\.min\\.css\\'"
                         "static/CACHE/.*")
      :main-file "manage.py")

    (define-project-type appcelerator (generic)
      (and (look-for ".project") (look-for "tiapp.xml"))
      :relevant-files ("\\.js\\'" "\\.coffee\\'"
                       "\\.html\\'" "\\.hb\\'"
                       "\\.json\\'" "\\.y[a]?ml\\'" "\\.xml\\'"
                       "\\.txt\\'" "\\.markdown\\'" "\\.md\\'"
                       "\\.rst\\'" "\\.org\\'"
                       "\\.css\\'" "\\.sass\\'" "\\.scss\\'" "\\.styl\\'")
      :irrelevant-files ("^[.]" "^[#]"
                         "build/.*"
                         "node_modules/.*"
                         "\\.sass-cache/.*"
                         "tmp/.*"
                         "log/.*"
                         "\\.min\\.js\\'"
                         "\\.min\\.css\\'")
      :main-file "tiapp.xml")))


(use-package truthy
  :commands (truthy
             truthy-s
             truthy-l))



(use-package conf-mode
  :mode ("\\.info\\|\\.gitmodules"  . conf-mode))

(use-package php-mode
  :interpreter ("php" . php-mode)
  init:
  (progn
    (require 'php-extras)
    (setq php-manual-path "~/git/.emacs.d/php/php-chunked-xhtml/")

    ;;(setq php-completion-file "~/git/ewax/misc/php-completion-file")

    (add-hook 'php-mode-hook '(lambda ()(c-subword-mode t)))
    (add-hook 'php-mode-hook '(lambda () (php-electric-mode)))
    ))


;; (use-package projectile
;;   :load-path "~/.emacs.d/site-lisp/projectile"
;;   :diminish projectile
;;   :config
;;   (progn
;;     (projectile-global-mode)
;;     ;; (bind-key "C-c p j" 'projectile-jump-to-project-file)
;;     ;; (bind-key "C-c p f" 'projectile-grep-in-project)
;;     ;; (bind-key "C-c p r" 'projectile-replace-in-project)
;;     ;; (bind-key "C-c p b" 'projectile-switch-to-buffer)
;;     ;; (bind-key "C-c p o" 'projectile-multi-occur)
;;     ;; (bind-key "C-c p t" 'projectile-regenerate-tags)
;;     ;; (bind-key "C-c p i" 'projectile-invalidate-project-cache)
;;     )
;;   )


(use-package key-chord
  ;; :if (and
  ;;      (not degrade-p-minimalism)
  ;;      (not degrade-p-noninteractive))
  :commands
  (key-chord-mode
   key-chord-define
   key-chord-define-global)
  :init
  (progn
    (setq
     key-chord-two-keys-delay 0.05
     key-chord-one-key-delay 0.15)
    ;; (when (not degrade-p-terminal)
    (key-chord-mode 1)
    )
  ;; )
  :config
  (progn
    (mapc
     (lambda (keyscommand)
       (key-chord-define-global
        (car keyscommand) (cdr keyscommand)))
     '(;; top row shifted keys (kind of)
       ("1j" . "!")
       ("2j" . "'") ("2k" . "\"") ("2l" . "`")
       ("2w" . "@") ;; note exception
       ("3j" . "#")
       ("4j" . "$")
       ("5j" . "%")
       ("6a" . "&")
       ("7a" . "\\") ("7s" . "|") ("7d" . "/") ;; standing slashes: \ | /
       ;; brackets: ( { [ <
       ("8a" . "(") ("9a" . ")")
       ("8s" . "{") ("9s" . "}")
       ("8d" . "[") ("9d" . "]")
       ("8f" . "<") ("9f" . ">")
       ("0a" . "=")
       ("+a" . "?")))
    (key-chord-define-global ";s" 'scratch)
    (key-chord-define-global ";d" 'delete-window)
    (key-chord-define-global ";g" 'magit-status)

    ))



;;;_ , drupal-mode

(use-package drupal-mode
  :mode ("\\.\\(php\\|inc\\|module\\|test\\|install\\|theme\\|\\profile\\|\\local\\.php\\)$" . drupal-mode)
  :interpreter ("drupal" . drupal-mode)

  (progn
    (require 'etags)
    (require 'smart-dash)
    (setq php-manual-path "~/git/.emacs.d/php/php-chunked-xhtml/")
    (setq flymake-phpcs-command "~/.emacs.d/site-lisp/flymake-phpcs/bin/flymake_phpcs")
    (setq flymake-phpcs-show-rule t)
    (defun my-insert-drupal-hook (tagname)
      "Clone the specified function as a new module hook implementation.

For Drupal <= 6, you will need to grab the developer documentation
before generating the TAGS file:

cvs -z6 -d:pserver:anonymous:anonymous@cvs.drupal.org:/cvs/drupal-contrib export -r DRUPAL-6--1 -d developer-docs contributions/docs/developer

Exuberant ctags:
$ ctags -eR --langmap=php:+.module.install.inc.engine --languages=php

Old etags:
$ find . -type f \\( -name '*.php' -o -name '*.module' -o -name '*.install' -o -name '*.inc' -o -name '*.engine' \\) | etags --language=php -
"
      (interactive (find-tag-interactive "Hook: "))
      (let ((module (file-name-sans-extension
                     (file-name-nondirectory (buffer-file-name)))))
        (find-tag (format "^function %s(" tagname) nil t)
        (let ((tmp-buffer (generate-new-buffer "*temp*")))
          (c-mark-function)
          (copy-to-buffer tmp-buffer (point) (mark))
          (kill-buffer) ;; the relevant API file
          (switch-to-buffer tmp-buffer))
        (newline)
        (forward-line -1)
        (insert "/**\n * Implements ")
        (forward-word)
        (forward-char) ;; to start of function name
        (let ((start (point)))
          (search-forward "(")
          (backward-char)
          (let ((funcname (filter-buffer-substring start (point))))
            (move-beginning-of-line nil)
            (backward-char)
            (insert funcname)))
        (insert "().\n */")
        (search-forward "_")
        (backward-char)
        (delete-region (point) (progn (forward-word -1) (point)))
        (insert module)
        (let ((function (filter-buffer-substring (point-min) (point-max))))
          (kill-buffer)
          (insert function))
        (backward-sexp)
        (forward-line)
        (back-to-indentation)))))



;;;_ , erc

(use-package erc
  ;; :commands erc
  :if running-alternate-emacs
  :init
  (progn

    (defun setup-irc-environment ()
      (interactive)
      (set-frame-font
       "-*-Lucida Grande-normal-normal-normal-*-*-*-*-*-p-0-iso10646-1" nil
       nil)
      (set-frame-parameter (selected-frame) 'width 90)
      (custom-set-faces
       '(erc-timestamp-face ((t (:foreground "dark violet")))))

      (setq erc-timestamp-only-if-changed-flag nil
            erc-timestamp-format "%H:%M "
            erc-fill-prefix "          "
            erc-fill-column 88
            erc-insert-timestamp-function 'erc-insert-timestamp-left)
      (set-input-method "Agda")

      (defun reset-erc-track-mode ()
        (interactive)
        (setq erc-modified-channels-alist nil)
        (erc-modified-channels-update))

      (bind-key "C-c r" 'reset-erc-track-mode))

    (add-hook 'erc-mode-hook 'setup-irc-environment)

    (defun irc ()
      (interactive)

      (erc-tls :server "asimov.freenode.net"
               :port 6697
               :nick "dkh"
               :password (funcall
                          (plist-get
                           (car (auth-source-search :host "asimov.freenode.net"
                                                    :user "dkh"
                                                    :type 'netrc
                                                    :port 6697))
                           :secret))
               ;;       (erc-tls :server "asimov.freenode.net"
               ;;                :port 6697
               ;;                :nick "dkh")
               ))
    (defun im ()
      (interactive)
      (erc :server "localhost"
           :port 6667
           :nick "dkh"
           :password (funcall
                      (plist-get
                       (car (auth-source-search :host "bitlbee"
                                                :user "dkh"
                                                :type 'netrc
                                                :port 6667))
                       :secret))))
    (add-hook 'after-init-hook 'im)
    (add-hook 'after-init-hook 'irc)
    )

  :config
  (progn
    (require 'sauron)
    ;; turn on abbrevs
    (abbrev-mode 1)

    (defun create-new-erc-frames ()
      (interactive)
      (switch-to-bitlbee)
      (sauron-toggle-hide-show)
      (switch-to-buffer-other-frame "#drupal-colorado")
      (switch-to-buffer-other-frame "#emacs"))
    (bind-key "H-E" 'create-new-erc-frames)

    ;; add abbrevs
    (abbrev-table-put erc-mode-abbrev-table :parents (list text-mode-abbrev-table))
    (add-hook 'erc-mode-hook (lambda () (abbrev-mode 1)))

    (erc-track-minor-mode 1)
    (erc-track-mode 1)

    (use-package erc-alert)
    (use-package erc-hl-nicks)

    ;; For bitlbee - not very useful unless jabber icons would populate
    (require 'erc-nicklist)

    (use-package erc-yank
      :init
      (bind-key "C-y" 'erc-yank erc-mode-map))

    (use-package wtf
      :commands wtf-is
      :init
      (defun erc-cmd-WTF (term &rest ignore)
        "Look up definition for TERM."
        (let ((def (wtf-is term)))
          (if def
              (let ((msg (concat "{Term} " (upcase term) " is " def)))
                (with-temp-buffer
                  (insert msg)
                  (kill-ring-save (point-min) (point-max)))
                (message msg))
            (message (concat "No definition found for " (upcase term)))))))


    (defun switch-to-bitlbee ()
      (interactive)
      (switch-to-buffer-other-window "&bitlbee")
      (call-interactively 'erc-channel-names)
      (goto-char (point-max)))

    (bind-key "C-c b" 'switch-to-bitlbee)

    (defun erc-cmd-SHOW (&rest form)
      "Eval FORM and send the result and the original form as:
FORM => (eval FORM)."
      (let* ((form-string (mapconcat 'identity form " "))
             (result
              (condition-case err
                  (eval (read-from-whole-string form-string))
                (error
                 (format "Error: %s" err)))))
        (erc-send-message (format "%s => %S" form-string result))))

    (defun erc-cmd-INFO (&rest ignore)
      "Send current info node."
      (unless (get-buffer "*info*")
        (error "No *info* buffer"))
      (let (output)
        (with-current-buffer "*info*"
          (let* ((file (file-name-nondirectory Info-current-file))
                 (node Info-current-node))
            (setq output (format "(info \"(%s)%s\") <-- hit C-x C-e to evaluate"
                                 file node))))
        (erc-send-message output)))

    (eval-when-compile
      (defvar erc-fools))

    (defun erc-cmd-FOOL (term &rest ignore)
      (add-to-list 'erc-fools term))

    (defun erc-cmd-UNFOOL (term &rest ignore)
      (setq erc-fools (delete term erc-fools)))

    (defun erc-cmd-OPME ()
      "Request chanserv to op me."
      (erc-message "PRIVMSG"
                   (format "chanserv op %s %s"
                           (erc-default-target)
                           (erc-current-nick)) nil))

    (defun erc-cmd-DEOPME ()
      "Deop myself from current channel."
      (erc-cmd-DEOP (format "%s" (erc-current-nick))))


    (defun erc-cmd-UNTRACK (&optional target)
      "Add TARGET to the list of target to be tracked."
      (if target
          (erc-with-server-buffer
           (let ((untracked
                  (car (erc-member-ignore-case target erc-track-exclude))))
             (if untracked
                 (erc-display-line
                  (erc-make-notice
                   (format "%s is not currently tracked!" target))
                  'active)
               (add-to-list 'erc-track-exclude target)
               (erc-display-line
                (erc-make-notice (format "Now not tracking %s" target))
                'active))))

        (if (null erc-track-exclude)
            (erc-display-line
             (erc-make-notice "Untracked targets list is empty") 'active)

          (erc-display-line (erc-make-notice "Untracked targets list:") 'active)
          (mapc #'(lambda (item)
                    (erc-display-line (erc-make-notice item) 'active))
                (erc-with-server-buffer erc-track-exclude))))
      t)


    (defun erc-cmd-TRACK (target)
      "Remove TARGET of the list of targets which they should not be tracked.
   If no TARGET argument is specified, list contents of `erc-track-exclude'."
      (when target
        (erc-with-server-buffer
         (let ((tracked
                (not (car (erc-member-ignore-case target erc-track-exclude)))))
           (if tracked
               (erc-display-line
                (erc-make-notice (format "%s is currently tracked!" target))
                'active)
             (setq erc-track-exclude (remove target erc-track-exclude))
             (erc-display-line
              (erc-make-notice (format "Now tracking %s" target))
              'active)))))
      t)
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (defun my-reformat-jabber-backlog ()
      "Fix \"unkown participant\" backlog messages from bitlbee."
      (save-excursion
        (goto-char (point-min))
        (if (looking-at
             "^<root> System message: Message from unknown participant \\([^:]+\\):")
            (replace-match "<\\1>"))))
    (add-hook 'erc-insert-modify-hook 'my-reformat-jabber-backlog)))


;;;_ , eshell

(use-package eshell
  :defer t
  :init
  (progn
    (defun eshell-initialize ()
      (defun eshell-spawn-external-command (beg end)
        "Parse and expand any history references in current input."
        (save-excursion
          (goto-char end)
          (when (looking-back "&!" beg)
            (delete-region (match-beginning 0) (match-end 0))
            (goto-char beg)
            (insert "spawn "))))

      (add-hook 'eshell-expand-input-functions 'eshell-spawn-external-command)

      (defun ss (server)
        (interactive "sServer: ")
        (call-process "spawn" nil nil nil "ss" server))

      (eval-after-load "em-unix"
        '(progn
           (unintern 'eshell/su)
           (unintern 'eshell/sudo)))

      (setq eshell-prompt-regexp "^[^#$]*[#$] ")

      (load "em-hist")           ; So the history vars are defined
      (if (boundp 'eshell-save-history-on-exit)
          (setq eshell-save-history-on-exit t)) ; Don't ask, just save
                                        ;(message "eshell-ask-to-save-history is %s" eshell-ask-to-save-history)
      (if (boundp 'eshell-ask-to-save-history)
          (setq eshell-ask-to-save-history 'always)) ; For older(?) version
                                        ;(message "eshell-ask-to-save-history is %s" eshell-ask-to-save-history)

      (defun eshell/ef (fname-regexp &rest dir) (ef fname-regexp default-directory))


;;; ---- path manipulation

      (defun pwd-repl-home (pwd)
        (interactive)
        (let* ((home (expand-file-name (getenv "HOME")))
               (home-len (length home)))
          (if (and
               (>= (length pwd) home-len)
               (equal home (substring pwd 0 home-len)))
              (concat "~" (substring pwd home-len))
            pwd)))

      (defun curr-dir-git-branch-string (pwd)
        "Returns current git branch as a string, or the empty string if
PWD is not in a git repo (or the git command is not found)."
        (interactive)
        (when (and (eshell-search-path "git")
                   (locate-dominating-file pwd ".git"))
          (let ((git-output (shell-command-to-string (concat "cd " pwd " && git branch | grep '\\*' | sed -e 's/^\\* //'"))))
            (propertize (concat "["
                                (if (> (length git-output) 0)
                                    (substring git-output 0 -1)
                                  "(no branch)")
                                "]") 'face `(:foreground "green"))
            )))

      (setq eshell-prompt-function
            (lambda ()
              (concat
               (propertize ((lambda (p-lst)
                              (if (> (length p-lst) 3)
                                  (concat
                                   (mapconcat (lambda (elm) (if (zerop (length elm)) ""
                                                         (substring elm 0 1)))
                                              (butlast p-lst 3)
                                              "/")
                                   "/"
                                   (mapconcat (lambda (elm) elm)
                                              (last p-lst 3)
                                              "/"))
                                (mapconcat (lambda (elm) elm)
                                           p-lst
                                           "/")))
                            (split-string (pwd-repl-home (eshell/pwd)) "/")) 'face `(:foreground "yellow"))
               (or (curr-dir-git-branch-string (eshell/pwd)))
               (propertize "# " 'face 'default))))

      (setq eshell-highlight-prompt nil)
      ;; https://github.com/anthracite/emacs-config/blob/master/init.el
      (defun eshell-maybe-bol ()
        "Moves point behind the eshell prompt, or
at the beginning of line, if already there."
        (interactive)
        (let ((p (point)))
          (eshell-bol)
          (when (= p (point))
            (beginning-of-line))))
      (defun eshell-clear ()
        "Clears the eshell buffer."
        (interactive)
        (let ((inhibit-read-only t))
          (erase-buffer)))

      (defun dkh-eshell-macs ()
        (interactive)
        "Creates a tool config shell and switches to it. If a buffer with name already exists, we simply switch to it."
        (let ((buffer-of-name (get-buffer (concat "*eshell-" (wg-name (wg-current-workgroup)) "-tool-config*"))))
          (cond ((bufferp buffer-of-name) ;If the buffer exists, switch to it (assume it is a shell)
                 (switch-to-buffer buffer-of-name))
                ( t
                  (progn
                    (eshell t)
                                        ;(process-send-string (get-buffer-process new-buff-name) (concat "cd " localdir "\n"))
                    (rename-buffer  (concat "*eshell-" (wg-name (wg-current-workgroup)) "-tool-config*")))))))


      (defun dkh-shell-with-name (name)
        (interactive "sName: ")
        "Creates a shell with name given by the first argument, and switches to it. If a buffer with name already exists, we simply switch to it."
        (let ((buffer-of-name (get-buffer (concat "*eshell-" (wg-name (wg-current-workgroup)) "-" name "*")))
              (localdir name))
          (cond ((bufferp buffer-of-name) ;If the buffer exists, switch to it (assume it is a shell)
                 (switch-to-buffer buffer-of-name))
                ( t
                  (progn
                    (eshell)
                                        ;(process-send-string (get-buffer-process new-buff-name) (concat "cd " localdir "\n"))
                    (rename-buffer  (concat "*eshell-" (wg-name (wg-current-workgroup)) "-" name "*")))))))


      (add-hook 'eshell-first-time-mode-hook 'eshell-initialize)

      ;; Make ls output be RET and mouse-2 clickable
      ;; (load-library "esh-clickable-ls.el")


      ;; http://www.emacswiki.org/emacs-ru/EshellEnhancedLS
      ;; ;;This makes Eshell’s ‘ls’ file names RET-able. Yay!
      (eval-after-load "em-ls"
        '(progn
           (defun ted-eshell-ls-find-file-at-point (point)
             "RET on Eshell's `ls' output to open files."
             (interactive "d")
             (find-file (buffer-substring-no-properties
                         (previous-single-property-change point 'help-echo)
                         (next-single-property-change point 'help-echo))))

           (defun pat-eshell-ls-find-file-at-mouse-click (event)
             "Middle click on Eshell's `ls' output to open files.
       From Patrick Anderson via the wiki."
             (interactive "e")
             (ted-eshell-ls-find-file-at-point (posn-point (event-end event))))

           (let ((map (make-sparse-keymap)))
             (define-key map (kbd "RET")      'ted-eshell-ls-find-file-at-point)
             (define-key map (kbd "<return>") 'ted-eshell-ls-find-file-at-point)
             (define-key map (kbd "<mouse-2>") 'pat-eshell-ls-find-file-at-mouse-click)
             (defvar ted-eshell-ls-keymap map))

           (defadvice eshell-ls-decorated-name (after ted-electrify-ls activate)
             "Eshell's `ls' now lets you click or RET on file names to open them."
             (add-text-properties 0 (length ad-return-value)
                                  (list 'help-echo "RET, mouse-2: visit this file"
                                        'mouse-face 'highlight
                                        'keymap ted-eshell-ls-keymap)
                                  ad-return-value)
             ad-return-value)))


      ;; Info Manual
      ;; With the following, you can type info cvs at the eshell prompt and it will work.
      (defun eshell/info (&optional subject)
        "Invoke `info', optionally opening the Info system to SUBJECT."
        (let ((buf (current-buffer)))
          (Info-directory)
          (if (not (null subject))
              (let ((node-exists (ignore-errors (Info-menu subject))))
                (if (not node-exists)
                    (format "No menu item `%s' in node `(dir)Top'." subject))))))

      ;; ;; aliases

      (defalias 'open 'find-file)
      (defalias 'openo 'find-file-other-window)

      (defun eshell/emacs (file)
        (find-file file))

      (defun eshell/cds ()
        "Change directory to the project's root."
        (eshell/cd (locate-dominating-file default-directory ".dir-locals.el")))

      (defun eshell/cdg ()
        "Change directory to the project's root."
        (eshell/cd (locate-dominating-file default-directory ".git")))

    ;;;###autoload
      (defun eshell/extract (file)
        (let ((command (some (lambda (x)
                               (if (string-match-p (car x) file)
                                   (cadr x)))
                             '((".*\.tar.bz2" "tar xjf")
                               (".*\.tar.gz" "tar xzf")
                               (".*\.bz2" "bunzip2")
                               (".*\.rar" "unrar x")
                               (".*\.gz" "gunzip")
                               (".*\.tar" "tar xf")
                               (".*\.tbz2" "tar xjf")
                               (".*\.tgz" "tar xzf")
                               (".*\.zip" "unzip")
                               (".*\.Z" "uncompress")
                               (".*" "echo 'Could not extract the file:'")))))
          (eshell-command-result (concat command " " file))))

      (defun esk-eshell-in-dir (&optional prompt)
        "Change the directory of an existing eshell to the directory of the file in
  the current buffer or launch a new eshell if one isn't running.  If the
  current buffer does not have a file (e.g., a *scratch* buffer) launch or raise
  eshell, as appropriate.  Given a prefix arg, prompt for the destination
  directory."
        (interactive "P")
        (let* ((name (buffer-file-name))
               (dir (cond (prompt (read-directory-name "Directory: " nil nil t))
                          (name (file-name-directory name))
                          (t nil)))
               (buffers (delq nil (mapcar (lambda (buf)
                                            (with-current-buffer buf
                                              (when (eq 'eshell-mode major-mode)
                                                (buffer-name))))
                                          (buffer-list))))
               (buffer (cond ((eq 1 (length buffers)) (first buffers))
                             ((< 1 (length buffers)) (ido-completing-read
                                                      "Eshell buffer: " buffers nil t
                                                      nil nil (first buffers)))
                             (t (eshell)))))
          (with-current-buffer buffer
            (when dir
              (eshell/cd (list dir))
              (eshell-send-input))
            (end-of-buffer)
            (pop-to-buffer buffer))))


      (defun curr-dir-svn-string (pwd)
        (interactive)
        (when (and (eshell-search-path "svn")
                   (locate-dominating-file pwd ".svn"))
          (concat "[s:"
                  (cond ((string-match-p "/trunk\\(/.*\\)?" pwd)
                         "trunk")
                        ((string-match "/branches/\\([^/]+\\)\\(/.*\\)?" pwd)
                         (match-string 1 pwd))
                        (t
                         "(no branch)"))
                  "] ")))

      ;; (eshell/export "EPOCROOT=\\Paragon\\")

      ;; http://permalink.gmane.org/gmane.emacs.devel/107713
      ;; (defadvice cd (around dir-locals-on-cd activate)
      ;;    "Apply the variables defined in .dir-locals.el when changing
      ;;  into and outof a directory in eshell."
      ;;    ;; clean up old variables
      ;;    (while file-local-variables-alist
      ;;      (let ((x (pop file-local-variables-alist)))
      ;;        (kill-local-variable x)))
      ;;    ;; cd
      ;;    ad-do-it
      ;;    ;; run hack-dir-local-variables w/o buffer-file-name
      ;;    (let ((variables-file (dir-locals-find-file default-directory))
      ;;         (class nil)
      ;;         (dir-name nil))
      ;;      (cond
      ;;       ((stringp variables-file)
      ;;        (setq dir-name (file-name-directory default-directory))
      ;;        (setq class (dir-locals-read-from-file variables-file)))
      ;;       ((consp variables-file)
      ;;        (setq dir-name (car variables-file))
      ;;        (setq class (cdr variables-file))))
      ;;      (when class
      ;;        (let ((variables
      ;;              (dir-locals-collect-variables
      ;;               (dir-locals-get-class-variables class) dir-name nil)))
      ;;         (when variables
      ;;           (hack-local-variables-filter variables dir-name)))))
      ;;    ;; apply file-local-variables to buffer
      ;;    (while file-local-variables-alist
      ;;      (let ((x (pop file-local-variables-alist)))
      ;;        (if (consp x)
      ;;            (set (car x) (cdr x))
      ;;          (kill-local-variable x)))))

      (defun curr-dir-project-string ()
        "Returns current project as a string, or the empty string if
PWD is not in a project"
        (interactive)
        (let ((project-root-dir (locate-dominating-file default-directory
                                                        ".dir-locals.el")))
          (let ((path (split-string project-root-dir "/")))     ; path as list
            (car (last (nbutlast path 1)))
            )
          ))



      (add-hook 'eshell-first-time-mode-hook 'eshell-initialize)
      (add-hook 'eshell-mode-hook
                '(lambda ()
                   ;; (or (getenv "CDPATH") (setenv "CDPATH" ".:~:~/.emacs.d:~/data:~/data/releases"))
                   (make-local-variable 'project-name)
                   (local-set-key "\C-c\C-q" 'eshell-kill-process)
                   (local-set-key "\C-c\C-k" 'compile)))))

  ;; eshell
  (eval-after-load 'esh-opt
    '(progn
       ;; we need this to override visual commands
       (require 'em-term)
       ;; If I try to SSH from an eshell, launch it in ansi-term instead
       (add-to-list 'eshell-visual-commands "ssh")))
  ;; fix ANSI colour issues from test runners etc.
  (add-hook 'eshell-preoutput-filter-functions
            'ansi-color-filter-apply)


  (use-package esh-toggle
    :requires eshell
    :bind ("C-x C-z" . eshell-toggle))

;;;_ , ess

  (use-package ess-site
    :disabled t
    :load-path "site-lisp/ess/lisp/"
    :commands R)


  (use-package nf-procmail-mode
    :mode (".procmailrc$" . nf-procmail-mode)
    )


;;;_ , ldap

  (use-package ldap
    :init
    (progn
      (setq ldap-ldapsearch-args (quote ("-tt" "-LLL" "-x")))
      (setq ldap-host-parameters-alist '(("directory.colorado.edu" base "dc=colorado,dc=edu")))))

;;;_ , eudc

  (defalias 'eu 'eudc-query-form)

  (use-package eudc
    :if (not running-alternate-emacs)
    :init
    (progn
      (use-package ldap)

      (eudc-protocol-set 'eudc-inline-query-format
                         '((firstname)
                           (lastname)
                           (firstname lastname)
                           (net))
                         'bbdb)

      (eudc-protocol-set 'eudc-inline-query-format
                         '(
                           (cn)
                           (cn cn)
                           (cn cn cn)
                           (Displayname)
                           (mail))
                         'ldap)

      ;; How to display results?
      (defalias 'bbdb-record-net 'bbdb-record-mail) ; Compatibility bbdbv3/v2

      (eudc-protocol-set 'eudc-inline-expansion-format
                         '("%s %s <%s>" firstname lastname net)
                         'bbdb)

      (eudc-protocol-set 'eudc-inline-expansion-format
                         '("%s <%s>"  displayName mail)
                         'ldap)


      (defun enz-eudc-expand-inline()
        (interactive)
        (move-end-of-line 1)
        (insert "*")
        (unless (condition-case nil
                    (eudc-expand-inline)
                  (error nil))
          (backward-delete-char-untabify 1)
          ;; Adds some hooks
          (eval-after-load "message"
            '(define-key message-mode-map (kbd "TAB") 'enz-eudc-expand-inline))
          (eval-after-load "sendmail"
            '(define-key mail-mode-map (kbd "TAB") 'enz-eudc-expand-inline))
          (eval-after-load "post"
            '(define-key post-mode-map (kbd "TAB") 'enz-eudc-expand-inline))))

      ))



  ;; (use-package linum-mode
  ;;   (progn
  ;; (global-linum-mode 1)
  ;; (setq linum-format
  ;;       (lambda (line)
  ;; (propertize
  ;; (format
  ;; (let
  ;; ((w (length (number-to-string (count-lines (point-min)
  ;; (point-max))))))
  ;; (concat " %" (number-to-string w) "d ")) line) 'face 'linum)))

  ;; (add-hook 'compilation-mode-hook '(lambda () (linum-mode 0)))

  ;; ;; Left click to jump to line
  ;; (defun line-at-click ()
  ;;   (cdr (cdr (mouse-position))))

  ;; (defun md-goto-linum ()
  ;;   (interactive)
  ;;   (setq *linum-mdown-line* nil)
  ;;   (goto-line (line-at-click)))

  ;; (setq linum-mode-inhibit-modes-list '(eshell-mode
  ;;                                       shell-mode
  ;;                                       erc-mode
  ;;                                       jabber-roster-mode
  ;;                                       jabber-chat-mode
  ;;                                       gnus-group-mode
  ;;                                       gnus-summary-mode
  ;;                                       gnus-article-mode))

  ;; (defadvice linum-on (around linum-on-inhibit-for-modes)
  ;;   "Stop the load of linum-mode for some major modes."
  ;;     (unless (member major-mode linum-mode-inhibit-modes-list)
  ;;       ad-do-it))

  ;; (ad-activate 'linum-on)

  ;;     ))

;;;_ , eval-expr

  (use-package eval-expr
    :bind ("M-:" . eval-expr)
    :config
    (progn
      (setq eval-expr-print-function 'pp
            eval-expr-print-level 20
            eval-expr-print-length 100)

      (defun eval-expr-minibuffer-setup ()
        (set-syntax-table emacs-lisp-mode-syntax-table)
        (paredit-mode))))

  ;; (add-to-list 'load-path "~/.emacs.d/site-lisp/expand-region.el")
  ;; expand-region
  (use-package expand-region
    :bind (("C-c =" . er/expand-region)
           ("H-M-SPC" . er/expand-region))
    :config
    (progn
      (defun er/add-text-mode-expansions ()
        (make-variable-buffer-local 'er/try-expand-list)
        (setq er/try-expand-list (append
                                  er/try-expand-list
                                  '(mark-paragraph
                                    mark-page))))

      (add-hook 'text-mode-hook 'er/add-text-mode-expansions)
      ))




;;;_ , fetchmail-mode

  (use-package fetchmail-mode
    :mode (".fetchmailrc$" . fetchmail-mode)
    :commands fetchmail-mode
    )

;;;_ , flyspell

  (use-package ispell
    :bind (("C-c i c" . ispell-comments-and-strings)
           ("C-c i d" . ispell-change-dictionary)
           ("C-c i k" . ispell-kill-ispell)
           ("C-c i m" . ispell-message)
           ("C-c i r" . ispell-region)))

  ;; load Flymake cursor

  (use-package flymake
    ;; :if (not degrade-p-flymake)
    :commands (flymake-mode
               flymake-mode-on)
    :init
    (progn
      (setq
       flymake-no-changes-timeout (* 60 5)
       flymake-start-syntax-check-on-find-file nil
       flymake-start-syntax-check-on-newline nil)

      (use-package js
        :if (executable-find "jshint")
        :defer t
        :config
        (progn
          (use-package flymake-jshint)))

      (use-package js2-mode
        :if (executable-find "jshint")
        :defer t
        :config
        (progn
          (use-package flymake-jshint)
          (add-hook 'js2-mode-hook 'flymake-mode-on)))

      (use-package flymake-jsonlint
        :if (executable-find "jsonlint")
        :commands flymake-jsonlint-load
        :init
        (progn
          (add-hook 'json-mode-hook 'flymake-jsonlint-load)
          (add-hook 'json-mode-hook 'flymake-mode-on)))

      (use-package flymake-ruby
        :commands flymake-ruby-load
        :init
        (progn
          (add-hook 'ruby-mode-hook 'flymake-ruby-load)
          (add-hook 'ruby-mode-hook 'flymake-mode-on)))

      (use-package flymake-coffee
        :commands flymake-coffee-load
        :init
        (progn
          (add-hook 'coffee-mode-hook 'flymake-coffee-load)
          (add-hook 'coffee-mode-hook 'flymake-mode-on)))

      (use-package flymake-php
        :commands flymake-php-load
        :init
        (progn
          (add-hook 'php-mode-hook 'flymake-php-load)
          (add-hook 'php-mode-hook 'flymake-mode-on)))

      (use-package flymake-python-pyflakes
        :commands flymake-python-pyflakes-load
        :init
        (progn
          (setq flymake-python-pyflakes-executable "flake8")
          (add-hook 'python-mode-hook 'flymake-python-pyflakes-load)
          (add-hook 'python-mode-hook 'flymake-mode-on)))

      (use-package flymake-python-pyflakes
        :commands flymake-python-pyflakes-load)

      (use-package flymake-scss
        :commands flymake-scss-load
        :init
        (progn
          (add-hook 'scss-mode-hook 'flymake-scss-load)
          (add-hook 'scss-mode-hook 'flymake-mode-on))))
    :config
    (progn
      (use-package flymake-cursor)))

  (use-package flycheck
    :disabled t
    :commands (flycheck-mode)
    :init
    (progn
      (add-hook 'python-mode-hook 'flycheck-mode))
    :config
    (progn
      (use-package flymake-cursor)))


  (use-package geben
    :commands (geben
               geben-mode))

  (use-package highlight-tail
    :commands highlight-tail-mode)


  (use-package nyan-mode
    ;; :if (and
    ;;      (not degrade-p-looks)
    ;;      (not degrade-p-terminal)
    ;;      (not degrade-p-noninteractive))
    :disabled t
    :commands nyan-mode
    :init
    (progn (nyan-mode 1)))



  (use-package flyspell
    :bind (("C-c i b" . flyspell-buffer)
           ("C-c i f" . flyspell-mode))
    :config
    (define-key flyspell-mode-map [(control ?.)] nil))

;;;_ , fold-dwim

  (use-package fold-dwim
    :bind (("<f13>" . fold-dwim-toggle)
           ("<f14>" . fold-dwim-hide-all)
           ("<f15>" . fold-dwim-show-all)))

;;;_ , gist

  (use-package gist
    :bind ("C-c G" . gist-region-or-buffer))

                                        ; (use-package git-commit-mode
                                        ;   :mode (("COMMIT_EDITMSG" . git-commit-mode)
                                        ;          ("NOTES_EDITMSG" . git-commit-mode)
                                        ;          ("MERGE_MSG" . git-commit-mode)
                                        ;          ("TAG_EDITMSG" . git-commit-mode)))

;;;_ , gnus
  (use-package dot-gnus
    :if (not running-alternate-emacs)
    :bind (("M-G"   . switch-to-gnus)
           ("C-x m" . compose-mail))
    :init
    (progn
      (setq gnus-init-file (expand-file-name "dot-gnus" user-emacs-directory)
            ;; (setq gnus-init-file "~/.emacs.d/dot-gnus.el"
            gnus-home-directory "~/git/gnus"
            message-directory "~/git/gnus/Mail")

      (abbrev-table-put gnus-article-edit-mode-abbrev-table :parents (list org-mode-abbrev-table))
      (use-package org-mime)
      (use-package eudc)
      (use-package rgr-web)

      ;; (setq message-mode-hook (quote (abbrev-mode footnote-mode turn-on-auto-fill turn-on-flyspell turn-on-orgstruct (lambda nil (set-fill-column 78)))))

      (add-hook 'message-mode-hook 'orgstruct++-mode 'append)
      (add-hook 'message-mode-hook 'turn-on-auto-fill 'append)
      (add-hook 'message-mode-hook 'orgtbl-mode 'append)
      (add-hook 'message-mode-hook 'turn-on-flyspell 'append)
      (add-hook 'message-mode-hook
                '(lambda () (setq fill-column 72))
                'append)
      (add-hook 'message-mode-hook
                '(lambda () (local-set-key (kbd "C-c M-o") 'org-mime-htmlize))
                'append)
      (add-hook 'message-mode-hook 'abbrev-mode 'footnote-mode 'turn-on-orgstruct)

      (add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)
      (add-hook 'message-setup-hook 'bbdb-mail-aliases) ; BBDB 3.x


      ))


;;;_ , grep

  (use-package grep
    :bind (("M-s d" . find-grep-dired)
           ("M-s f" . find-grep)
           ("M-s g" . grep))
    :init
    (progn
      (defun find-grep-in-project (command-args)
        (interactive
         (let ((default (thing-at-point 'symbol)))
           (list (read-shell-command "Run find (like this): "
                                     (cons (concat "git --no-pager grep -n "
                                                   default)
                                           (+ 24 (length default)))
                                     'grep-find-history))))
        (if command-args
            (let ((null-device nil))      ; see grep
              (grep command-args))))

      (bind-key "M-s p" 'find-grep-in-project))

    :config
    (progn
      (use-package grep-ed)

      (grep-apply-setting 'grep-command "egrep -nH -e ")
      (if t
          (grep-apply-setting 'grep-find-command '("gf -e " . 7))
        (grep-apply-setting
         'grep-find-command
         '("find . -type f -print0 | xargs -P4 -0 egrep -nH -e " . 52)))))

;;;_ , gtags

  (use-package gtags
    :commands gtags-mode
    :diminish gtags-mode
    :config
    (progn
      (defun my-gtags-or-semantic-find-tag ()
        (interactive)
        (if (and (fboundp 'semantic-active-p)
                 (funcall #'semantic-active-p))
            (call-interactively #'semantic-complete-jump)
          (call-interactively #'gtags-find-tag)))

      (bind-key "M-." 'my-gtags-or-semantic-find-tag gtags-mode-map)

      (bind-key "C-c t ." 'gtags-find-rtag)
      (bind-key "C-c t f" 'gtags-find-file)
      (bind-key "C-c t p" 'gtags-parse-file)
      (bind-key "C-c t g" 'gtags-find-with-grep)
      (bind-key "C-c t i" 'gtags-find-with-idutils)
      (bind-key "C-c t s" 'gtags-find-symbol)
      (bind-key "C-c t r" 'gtags-find-rtag)
      (bind-key "C-c t v" 'gtags-visit-rootdir)

      (bind-key "<mouse-2>" 'gtags-find-tag-from-here gtags-mode-map)

      (use-package helm-gtags
        :bind ("M-T" . helm-gtags-select)
        :config
        (bind-key "M-," 'helm-gtags-resume gtags-mode-map))))

;;;_ , gud

  (use-package gud
    :commands gud-gdb
    :init
    (progn
      (defun show-debugger ()
        (interactive)
        (let ((gud-buf
               (catch 'found
                 (dolist (buf (buffer-list))
                   (if (string-match "\\*gud-" (buffer-name buf))
                       (throw 'found buf))))))
          (if gud-buf
              (switch-to-buffer-other-window gud-buf)
            (call-interactively 'gud-gdb))))

      (bind-key "C-. g" 'show-debugger))

    :config
    (progn
      (bind-key "<f9>" 'gud-cont)
      (bind-key "<f10>" 'gud-next)
      (bind-key "<f11>" 'gud-step)
      (bind-key "S-<f11>" 'gud-finish)))

;;;_ , helm

  (use-package helm-config
    :if (not running-alternate-emacs)
    :init
    (progn
      (bind-key "C-c M-x" 'helm-M-x)
      (bind-key "C-h a" 'helm-c-apropos)
      (bind-key "M-s a" 'helm-do-grep)
      (bind-key "M-s b" 'helm-occur)
      (bind-key "M-s F" 'helm-for-files)

      (use-package helm-commands)

      (bind-key "C-h e a" 'my-helm-apropos)
      (bind-key "C-x M-!" 'helm-command-from-zsh)
      (bind-key "C-x f" 'helm-find-git-file)

      (use-package helm-descbinds
        :commands helm-descbinds
        :init
        (fset 'describe-bindings 'helm-descbinds))

      (bind-key "C-h b" 'helm-descbinds))

    :config
    (helm-match-plugin-mode t))

;;;_ , hi-lock

  (use-package hi-lock
    :bind (("M-o l" . highlight-lines-matching-regexp)
           ("M-o r" . highlight-regexp)
           ("M-o w" . highlight-phrase)))

;;;_ , hilit-chg

  (use-package hilit-chg
    :bind ("M-o C" . highlight-changes-mode))

;;;_ , hl-line

  (use-package hl-line
    :bind ("M-o h" . hl-line-mode)
    :config
    (use-package hl-line+))

;;;_ , identica

  (use-package identica-mode
    :bind (("\C-cip" . identica-update-status-interactive)
           ("\C-cid" . identica-direct-message-interactive)
           ))



;;;_ , ido

  (use-package ido
    :defines (ido-cur-item
              ido-require-match
              ido-selected
              ido-final-text
              ido-show-confirm-message)
    :init
    (ido-mode 'buffer)

    :config
    (progn
      (use-package ido-hacks
        :init
        (ido-hacks-mode 1))

      (use-package ido-springboard)

      (defun ido-smart-select-text ()
        "Select the current completed item.  Do NOT descend into directories."
        (interactive)
        (when (and (or (not ido-require-match)
                       (if (memq ido-require-match
                                 '(confirm confirm-after-completion))
                           (if (or (eq ido-cur-item 'dir)
                                   (eq last-command this-command))
                               t
                             (setq ido-show-confirm-message t)
                             nil))
                       (ido-existing-item-p))
                   (not ido-incomplete-regexp))
          (when ido-current-directory
            (setq ido-exit 'takeprompt)
            (unless (and ido-text (= 0 (length ido-text)))
              (let ((match (ido-name (car ido-matches))))
                (throw 'ido
                       (setq ido-selected
                             (if match
                                 (replace-regexp-in-string "/\\'" "" match)
                               ido-text)
                             ido-text ido-selected
                             ido-final-text ido-text)))))
          (exit-minibuffer)))

      (add-hook 'ido-minibuffer-setup-hook
                #'(lambda ()
                    (bind-key "<return>" 'ido-smart-select-text
                              ido-file-completion-map)))

      (defun ido-switch-buffer-tiny-frame (buffer)
        (interactive (list (ido-read-buffer "Buffer: " nil t)))
        (with-selected-frame
            (make-frame '((width                . 80)
                          (height               . 22)
                          (left-fringe          . 0)
                          (right-fringe         . 0)
                          (vertical-scroll-bars . nil)
                          (unsplittable         . t)
                          (has-modeline-p       . nil)
                          ;;(background-color     . "grey80")
                          (minibuffer           . nil)))
          (switch-to-buffer buffer)
          (set (make-local-variable 'mode-line-format) nil)))

      (bind-key "C-x 5 t" 'ido-switch-buffer-tiny-frame)


      ;; Use ido everywhere
      (require 'ido-ubiquitous)
      (ido-ubiquitous-mode 1)

      ;; Fix ido-ubiquitous for newer packages
      (defmacro ido-ubiquitous-use-new-completing-read (cmd package)
        `(eval-after-load ,package
           '(defadvice ,cmd (around ido-ubiquitous-new activate)
              (let ((ido-ubiquitous-enable-compatibility nil))
                ad-do-it))))

      (ido-ubiquitous-use-new-completing-read webjump 'webjump)
      (ido-ubiquitous-use-new-completing-read yas/expand 'yasnippet)
      (ido-ubiquitous-use-new-completing-read yas/visit-snippet-file 'yasnippet)))



  (use-package iedit
    :bind ("C-c ;" . iedit-mode)
    :config
    (progn
      (defun iedit-dwim (arg)
        "Starts iedit but uses \\[narrow-to-defun] to limit its scope."
        (interactive "P")
        (if arg
            (iedit-mode)
          (save-excursion
            (save-restriction
              (widen)
              ;; this function determines the scope of `iedit-start'.
              (narrow-to-defun)
              (if iedit-mode
                  (iedit-done)
                ;; `current-word' can of course be replaced by other
                ;; functions.
                (iedit-start (current-word)))))))

      )
    )

  (global-set-key (kbd "C-;") 'iedit-dwim)

;;;_ , ielm

  (use-package ielm
    :bind ("C-c :" . ielm)
    :config
    (progn
      (defun my-ielm-return ()
        (interactive)
        (let ((end-of-sexp (save-excursion
                             (goto-char (point-max))
                             (skip-chars-backward " \t\n\r")
                             (point))))
          (if (>= (point) end-of-sexp)
              (progn
                (goto-char (point-max))
                (skip-chars-backward " \t\n\r")
                (delete-region (point) (point-max))
                (call-interactively #'ielm-return))
            (call-interactively #'paredit-newline))))

      (add-hook 'ielm-mode-hook
                (function
                 (lambda ()
                   (bind-key "<return>" 'my-ielm-return ielm-map)))
                t)))

;;;_ , image-file

  (use-package image-file
    :disabled t
    :init
    (auto-image-file-mode 1))

;;;_ , info

  (use-package info
    :bind ("C-h C-i" . info-lookup-symbol)
    :init
    (remove-hook 'menu-bar-update-hook 'mac-setup-help-topics)

    :config
    (progn
      (defadvice info-setup (after load-info+ activate)
        (use-package info+))

      (defadvice Info-exit (after remove-info-window activate)
        "When info mode is quit, remove the window."
        (if (> (length (window-list)) 1)
            (delete-window)))))

  (use-package info-look
    :commadns info-lookup-add-help)

;;;_ , indirect

  (use-package indirect
    :bind ("C-c C" . indirect-region))

;;;_ , initsplit

  (eval-after-load "cus-edit"
    '(use-package initsplit))

;;;_ , ipa

  (use-package ipa
    :commands ipa-insert
    :init
    (progn
      (autoload 'ipa-load-annotations-into-buffer "ipa")
      (add-hook 'find-file-hook 'ipa-load-annotations-into-buffer)))

  ;; ;;;_ , js2-mode

;;;;_ , javascript

  (use-package js2-mode
    :load-path "js2-mode"
    :mode ("\\.js$" . js2-mode)
    :config
    (progn
      (setq js2-pretty-multiline-declarations t)
      (use-package js2-refactor ;; js2-refactor-mode
        :load-path "js2-refactor"
        :requires mark-multiple)

      (add-hook 'js2-mode-hook
                (lambda ()
                  ;; (paredit-mode 1)
                  (slime-js-minor-mode 1)
                  (local-set-key (kbd "<return>") 'newline-and-indent))
                t)))

  ;; js2-hightlight-vars-mode
  ;; see http://mihai.bazon.net/projects/editing-javascript-with-emacs-js2-mode/js2-highlight-vars-mode
  ;; M-n js2-highlight-vars-next
  ;; M-p js2-highlight-vars-prev
  ;; M-r js2-highlight-vars-rename
  (use-package js2-highlight-vars
    :requires js2-mode
    :commands (js2-highlight-vars-mode)
    :init (add-hook 'js2-mode-hook 'js2-highlight-vars-mode t))


;;;;_ , nodejs-mode

  (use-package nodejs-mode
    :disabled t
    :load-path "nodejs-mode"
    :commands (nodejs))



  ;; JSON files
  (use-package json-mode
    :commands json-mode
    :mode ("\\.json\\'" . json-mode)
    :config
    (progn
      (use-package json-pretty-print
        :commands json-pretty-print)
      (add-hook 'json-mode-hook
                #'(lambda ()
                    (set (make-local-variable 'js-indent-level) 2)))))

;;;_ , ledger

  (use-package "ldg-new"
    :commands ledger-mode
    :init
    (progn
      (defun my-ledger-start-entry (&optional arg)
        (interactive "p")
        (find-file-other-window "~/Documents/Accounts/ledger.dat")
        (goto-char (point-max))
        (skip-syntax-backward " ")
        (if (looking-at "\n\n")
            (goto-char (point-max))
          (delete-region (point) (point-max))
          (insert ?\n)
          (insert ?\n))
        (insert (format-time-string "%Y/%m/%d ")))

      (bind-key "C-c L" 'my-ledger-start-entry)

      (defun ledger-matchup ()
        (interactive)
        (while (re-search-forward "\\(\\S-+Unknown\\)\\s-+\\$\\([-,0-9.]+\\)"
                                  nil t)
          (let ((account-beg (match-beginning 1))
                (account-end (match-end 1))
                (amount (match-string 2))
                account answer)
            (goto-char account-beg)
            (set-window-point (get-buffer-window) (point))
            (recenter)
            (redraw-display)
            (with-current-buffer (get-buffer "nrl-mastercard-old.dat")
              (goto-char (point-min))
              (when (re-search-forward (concat "\\(\\S-+\\)\\s-+\\$" amount)
                                       nil t)
                (setq account (match-string 1))
                (goto-char (match-beginning 1))
                (set-window-point (get-buffer-window) (point))
                (recenter)
                (redraw-display)
                (setq answer
                      (read-char (format "Is this a match for %s (y/n)? "
                                         account)))))
            (when (eq answer ?y)
              (goto-char account-beg)
              (delete-region account-beg account-end)
              (insert account))
            (forward-line))))))

;;;_ , lisp-mode

  ;; Utilities every Emacs Lisp coders should master:
  ;;
  ;;   paredit          Let's you manipulate sexps with ease
  ;;   redshank         Think: Lisp refactoring
  ;;   edebug           Knowing the traditional debugger is good too
  ;;   eldoc
  ;;   cldoc
  ;;   elint
  ;;   elp
  ;;   ert


                                        ; elisp
  (defalias 'eb 'eval-buffer)
  (defalias 'er 'eval-region)
  (defalias 'ed 'eval-defun)
  (defalias 'ele 'eval-last-sexp)
  (defalias 'eis 'elisp-index-search)

  (use-package lisp-mode
    :mode ("\\.abbrev_defs" . lisp-mode)
    :interpreter ("lisp" . lisp-mode)
    ;; :load-path "site-lisp/slime/contrib/"
    :init
    (progn
      (defface esk-paren-face
        '((((class color) (background dark))
           (:foreground "grey50"))
          (((class color) (background light))
           (:foreground "grey55")))
        "Face used to dim parentheses."
        :group 'starter-kit-faces)

      ;; Change lambda to an actual lambda symbol
      (mapc (lambda (major-mode)
              (font-lock-add-keywords
               major-mode
               '(("(\\(lambda\\)\\>"
                  (0 (ignore
                      (compose-region (match-beginning 1)
                                      (match-end 1) ?λ))))
                 ("(\\|)" . 'esk-paren-face)
                 ("(\\(ert-deftest\\)\\>[         '(]*\\(setf[    ]+\\sw+\\|\\sw+\\)?"
                  (1 font-lock-keyword-face)
                  (2 font-lock-function-name-face
                     nil t)))))
            lisp-modes)

      (defvar slime-mode nil)
      (defvar lisp-mode-initialized nil)

      (defun initialize-lisp-mode ()
        (unless lisp-mode-initialized
          (setq lisp-mode-initialized t)

          (use-package redshank
            :diminish redshank-mode)

          (use-package elisp-slime-nav
            :diminish elisp-slime-nav-mode)

          (use-package edebug)

          (use-package eldoc
            :diminish eldoc-mode
            :defer t
            :init
            (use-package eldoc-extension
              :disabled t
              :defer t
              :init
              (add-hook 'emacs-lisp-mode-hook
                        #'(lambda () (require 'eldoc-extension)) t))

            :config
            (eldoc-add-command 'paredit-backward-delete
                               'paredit-close-round))

          (use-package cldoc
            :diminish cldoc-mode)

          (use-package ert
            :commands ert-run-tests-interactively
            :bind ("C-c e t" . ert-run-tests-interactively))

          (use-package elint
            :commands 'elint-initialize
            :init
            (defun elint-current-buffer ()
              (interactive)
              (elint-initialize)
              (elint-current-buffer))

            :config
            (progn
              (add-to-list 'elint-standard-variables 'current-prefix-arg)
              (add-to-list 'elint-standard-variables 'command-line-args-left)
              (add-to-list 'elint-standard-variables 'buffer-file-coding-system)
              (add-to-list 'elint-standard-variables 'emacs-major-version)
              (add-to-list 'elint-standard-variables 'window-system)))

          (use-package highlight-cl
            :init
            (mapc (function
                   (lambda (mode-hook)
                     (add-hook mode-hook
                               'highlight-cl-add-font-lock-keywords)))
                  lisp-mode-hooks))

          (defun my-elisp-indent-or-complete (&optional arg)
            (interactive "p")
            (call-interactively 'lisp-indent-line)
            (unless (or (looking-back "^\\s-*")
                        (bolp)
                        (not (looking-back "[-A-Za-z0-9_*+/=<>!?]+")))
              (call-interactively 'lisp-complete-symbol)))

          (defun my-lisp-indent-or-complete (&optional arg)
            (interactive "p")
            (if (or (looking-back "^\\s-*") (bolp))
                (call-interactively 'lisp-indent-line)
              (call-interactively 'slime-indent-and-complete-symbol)))

          (defun my-byte-recompile-file ()
            (save-excursion
              (byte-recompile-file buffer-file-name)))

          ;; Register Info manuals related to Lisp
          (use-package info-lookmore
            :init
            (progn
              (info-lookmore-elisp-cl)
              (info-lookmore-elisp-userlast)
              (info-lookmore-elisp-gnus)
              (info-lookmore-apropos-elisp)))

          (mapc (lambda (mode)
                  (info-lookup-add-help
                   :mode mode
                   :regexp "[^][()'\" \t\n]+"
                   :ignore-case t
                   :doc-spec '(("(ansicl)Symbol Index" nil nil nil))))
                lisp-modes)))

      (defun my-lisp-mode-hook ()
        (initialize-lisp-mode)

        (auto-fill-mode 1)
        (paredit-mode 1)

        (require 'redshank-loader
                 "~/.emacs.d/site-lisp/redshank/redshank-loader")

        (redshank-mode 1)
        (elisp-slime-nav-mode 1)

        (local-set-key (kbd "<return>") 'paredit-newline)
        (add-hook 'after-save-hook 'check-parens nil t)

        (if (memq major-mode
                  '(emacs-lisp-mode inferior-emacs-lisp-mode ielm-mode))
            (progn
              (bind-key "<M-return>" 'outline-insert-heading emacs-lisp-mode-map)
              (bind-key "<tab>" 'my-elisp-indent-or-complete emacs-lisp-mode-map))
          (turn-on-cldoc-mode)

          (bind-key "<tab>" 'my-lisp-indent-or-complete lisp-mode-map)
          (bind-key "M-q" 'slime-reindent-defun lisp-mode-map)
          (bind-key "M-l" 'slime-selector lisp-mode-map))

        (yas/minor-mode 1))


      (hook-into-modes #'my-lisp-mode-hook lisp-mode-hooks)))

;;;_ , log4j-mode

  (use-package log4j-mode
    :disabled t
    :mode ("\\.log\\'" . log4j-mode))


;;;_ , lorem-ipsum
  (use-package lorem-ipsum
    :commands (Lorem-ipsum-insert-paragraphs
               Lorem-ipsum-insert-sentences
               Lorem-ipsum-insert-list)
    )

;;;_ , lua-mode

  (use-package lua-mode
    :mode ("\\.lua\\'" . lua-mode)
    :interpreter ("lua" . lua-mode))

;;;_ , lusty-explorer

  (use-package lusty-explorer
    :bind ("C-x C-f" . lusty-file-explorer)
    :config
    (progn
      (add-hook 'lusty-setup-hook
                (lambda ()
                  (bind-key "SPC" 'lusty-select-match lusty-mode-map)
                  (bind-key "C-d" 'exit-minibuffer lusty-mode-map)))

      (defun lusty-open-this ()
        "Open the given file/directory/buffer, creating it if not already present."
        (interactive)
        (when lusty--active-mode
          (ecase lusty--active-mode
            (:file-explorer
             (let* ((path (minibuffer-contents-no-properties))
                    (last-char (aref path (1- (length path)))))
               (lusty-select-match)
               (lusty-select-current-name)))
            (:buffer-explorer (lusty-select-match)))))

      (defvar lusty-only-directories nil)

      (defun lusty-file-explorer-matches (path)
        (let* ((dir (lusty-normalize-dir (file-name-directory path)))
               (file-portion (file-name-nondirectory path))
               (files
                (and dir
                     ;; NOTE: directory-files is quicker but
                     ;;       doesn't append slash for directories.
                     ;;(directory-files dir nil nil t)
                     (file-name-all-completions "" dir)))
               (filtered (lusty-filter-files
                          file-portion
                          (if lusty-only-directories
                              (loop for f in files
                                    when (= ?/ (aref f (1- (length f))))
                                    collect f)
                            files))))
          (if (or (string= file-portion "")
                  (string= file-portion "."))
              (sort filtered 'string<)
            (lusty-sort-by-fuzzy-score filtered file-portion))))

      (defun lusty-read-directory ()
        "Launch the file/directory mode of LustyExplorer."
        (interactive)
        (let ((lusty--active-mode :file-explorer))
          (lusty--define-mode-map)
          (let* ((lusty--ignored-extensions-regex
                  (concat "\\(?:" (regexp-opt completion-ignored-extensions) "\\)$"))
                 (minibuffer-local-filename-completion-map lusty-mode-map)
                 (lusty-only-directories t))
            (lusty--run 'read-directory-name default-directory ""))))

      (defun lusty-read-file-name ()
        "Launch the file/directory mode of LustyExplorer."
        (interactive)
        (let ((lusty--active-mode :file-explorer))
          (lusty--define-mode-map)
          (let* ((lusty--ignored-extensions-regex
                  (concat "\\(?:" (regexp-opt completion-ignored-extensions) "\\)$"))
                 (minibuffer-local-filename-completion-map lusty-mode-map)
                 (lusty-only-directories nil))
            (lusty--run 'read-file-name default-directory ""))))

      (if (featurep 'icicles)
          (defadvice lusty-file-explorer (around lusty-file-explorer-without-icy
                                                 activate)
            (flet ((message (&rest ignore)))
              (let ((icy-was-on icicle-mode))
                (if icy-was-on (icy-mode 0))
                (unwind-protect
                    ad-do-it
                  (if icy-was-on (icy-mode 1)))))))))

;;;_ , macrostep

  (use-package macrostep
    :bind ("C-c e m" . macrostep-expand))

;;;_ , magit

  (use-package magit
    :bind (("C-x g" . magit-status)
           ("C-x G" . magit-status-with-prefix))
    :init
    (defun magit-status-with-prefix ()
      (interactive)
      (let ((current-prefix-arg '(4)))
        (call-interactively 'magit-status)))

    ;; full screen magit-status

    (defadvice magit-status (around magit-fullscreen activate)
      (window-configuration-to-register :magit-fullscreen)
      ad-do-it
      (delete-other-windows))

    (defun magit-quit-session ()
      "Restores the previous window configuration and kills the magit buffer"
      (interactive)
      (kill-buffer)
      (jump-to-register :magit-fullscreen))

    (define-key magit-status-mode-map (kbd "q") 'magit-quit-session)

    ;; Actual changes lost in a sea of whitespace diffs?

    (defun magit-toggle-whitespace ()
      (interactive)
      (if (member "-w" magit-diff-options)
          (magit-dont-ignore-whitespace)
        (magit-ignore-whitespace)))

    (defun magit-ignore-whitespace ()
      (interactive)
      (add-to-list 'magit-diff-options "-w")
      (magit-refresh))

    (defun magit-dont-ignore-whitespace ()
      (interactive)
      (setq magit-diff-options (remove "-w" magit-diff-options))
      (magit-refresh))

    (define-key magit-status-mode-map (kbd "W") 'magit-toggle-whitespace)

    :config
    (progn
      (setenv "GIT_PAGER" "")

      (add-hook 'magit-log-edit-mode-hook
                #'(lambda ()
                    (set-fill-column 72)
                    (flyspell-mode)))

      (require 'magit-topgit)
      (require 'rebase-mode)

      (defun start-git-monitor ()
        (interactive)
        (start-process "git-monitor" (current-buffer) "~/bin/git-monitor"))

      ;;(add-hook 'magit-status-mode-hook 'start-git-monitor)
      ))

;;;_ , markdown-mode

  (use-package markdown-mode
    :commands markdown-mode
    :mode (("\\.markdown\\'" . markdown-mode)
           ("\\.md\\'" . markdown-mode)
           ("\\.mdwn\\'" . markdown-mode)
           ("\\.mkd\\'" . markdown-mode)
           ("\\.mkdown\\'" . markdown-mode)
           ("\\.mdtext\\'" . markdown-mode))
    :init
    (progn
      (setq markdown-command "pandoc -f markdown -t html")
      (defun markdown-imenu-create-index ()
        (let* ((root '(nil . nil))
               cur-alist
               (cur-level 0)
               (pattern "^\\(\\(#+\\)[ \t]*\\(.+\\)\\|\\([^# \t\n=-].*\\)\n===+\\|\\([^# \t\n=-].*\\)\n---+\\)$")
               (empty-heading "-")
               (self-heading ".")
               hashes pos level heading)
          (save-excursion
            (goto-char (point-min))
            (while (re-search-forward pattern (point-max) t)
              (cond
               ((setq hashes (match-string-no-properties 2))
                (setq heading (match-string-no-properties 3)
                      pos (match-beginning 1)
                      level (length hashes)))
               ((setq heading (match-string-no-properties 4))
                (setq pos (match-beginning 4)
                      level 1))
               ((setq heading (match-string-no-properties 5))
                (setq pos (match-beginning 5)
                      level 2)))
              (let ((alist (list (cons heading pos))))
                (cond
                 ((= cur-level level) ; new sibling
                  (setcdr cur-alist alist)
                  (setq cur-alist alist))
                 ((< cur-level level) ; first child
                  (dotimes (i (- level cur-level 1))
                    (setq alist (list (cons empty-heading alist))))
                  (if cur-alist
                      (let* ((parent (car cur-alist))
                             (self-pos (cdr parent)))
                        (setcdr parent (cons (cons self-heading self-pos) alist)))
                    (setcdr root alist)) ; primogenitor
                  (setq cur-alist alist)
                  (setq cur-level level))
                 (t ; new sibling of an ancestor
                  (let ((sibling-alist (last (cdr root))))
                    (dotimes (i (1- level))
                      (setq sibling-alist (last (cdar sibling-alist))))
                    (setcdr sibling-alist alist)
                    (setq cur-alist alist))
                  (setq cur-level level)))))
            (cdr root))))

      (defun markdown-preview-file ()
        "run Marked on the current file and revert the buffer"
        (interactive)
        (shell-command
         (format "open -a /Applications/Marked.app %s"
                 (shell-quote-argument (buffer-file-name)))))

      (bind-key "C-x M" 'markdown-preview-file)
      (setq markdown-imenu-generic-expression
            '(("title"  "^\\(.*\\)[\n]=+$" 1)
              ("h2-"    "^\\(.*\\)[\n]-+$" 1)
              ("h1"   "^# \\(.*\\)$" 1)
              ("h2"   "^## \\(.*\\)$" 1)
              ("h3"   "^### \\(.*\\)$" 1)
              ("h4"   "^#### \\(.*\\)$" 1)
              ("h5"   "^##### \\(.*\\)$" 1)
              ("h6"   "^###### \\(.*\\)$" 1)
              ("fn"   "^\\[\\^\\(.*\\)\\]" 1)
              ))
      (add-hook 'markdown-mode-hook
                '(lambda ()
                   (setq imenu-create-index-function 'markdown-imenu-create-index)
                   (setq imenu-generic-expression markdown-imenu-generic-expression)))))

;;;;_ , mark-multiple

  (use-package mark-multiple
    :load-path "mark-multiple")

;;;_ , merlin

  (defun merlin-record-times ()
    (interactive)
    (require 'rx)
    (let* ((text (buffer-substring-no-properties (line-beginning-position)
                                                 (line-end-position)))
           (regex
            (rx (and string-start (0+ space)
                     (group (and (= 2 num) ?/ (= 2 num) ?/ (= 2 num)
                                 space (= 2 num) ?: (= 2 num) space
                                 (in "AP") ?M)) (1+ space)
                                 (group (and (= 2 num) ?/ (= 2 num) ?/ (= 2 num)
                                             space (= 2 num) ?: (= 2 num) space
                                             (in "AP") ?M)) (1+ space)
                                             (? (and (group ?*) (1+ space)))
                                             (group (1+ (or digit (in ".hms"))))
                                             (1+ space) (group (1+ nonl)) string-end))))
      (if (string-match regex text)
          (let ((start (match-string 1 text))
                (end (match-string 2 text))
                (cleared (match-string 3 text))
                (duration (match-string 4 text)) commodity
                (account (match-string 5 text)))
            (when (string-match "\\([0-9.]+\\)\\([mhs]\\)" duration)
              (setq commodity (match-string 2 duration)
                    duration (match-string 1 duration))
              (cond ((string= commodity "h")
                     (setq commodity "hours"))
                    ((string= commodity "m")
                     (setq commodity "minutes"))
                    ((string= commodity "s")
                     (setq commodity "seconds"))))
            (if (string-match "\\([0-9.][0-9.a-z]+\\)" account)
                (setq account (match-string 1 account)))
            (do-applescript
             (format
              "
tell application \"Merlin\"
  activate

  set act to 0

  set listActivity to every activity of first document
  repeat with oneActivity in listActivity
    if subtitle of oneActivity is \"%s\" then
      set act to oneActivity
      exit repeat
    end if
  end repeat

  if act is 0 then
    set myselection to selected object of main window of first document as list

    if (count of myselection) is 0 then
      display dialog \"Please select activity to set time for\" buttons {\"OK\"}
    else
      set act to beginning of myselection
    end if
  end if

  if act is 0 or (class of act is project) or (is milestone of act is true) then
    display dialog \"Cannot locate activity for %s\" buttons {\"OK\"}
  else
    tell act
      if ((class is not project) and (is milestone is not true)) then
        set actual start date to (date \"%s\")
        set given actual work to {amount:%s, unit:%s, floating:false, ¬
            relative error:0}
        if %s then
          set actual end date to (date \"%s\")
          delete last actuals reporting date

          set given remaining work to {amount:0, unit:hours, floating:false, ¬
              relative error:0}
        else
          delete actual end date
          set last actuals reporting date to (date \"%s\")

          -- set theReturnedItems to (display dialog \"Enter remaining hours for \" ¬
          --     with title \"Given Remaining Work\" with icon stop ¬
          --     default answer \"\" buttons {\"OK\", \"Cancel\"} default button 1)
          -- set theAnswer to the text returned of theReturnedItems
          -- set theButtonName to the button returned of theReturnedItems
          --
          -- set given remaining work to {amount:(theAnswer as number), unit:hours, ¬
          --        floating:false, relative error:0}
        end if
      end if
    end tell
  end if
end tell" account account start duration commodity (if cleared "true" "false")
end end))))))

;;;_ , mudel

  (use-package mudel
    :commands mudel
    :bind ("C-c M" . mud)
    :init
    (defun mud ()
      (interactive)
      (mudel "4dimensions" "4dimensions.org" 6000)))

;;;_ , mule

  (use-package mule
    :init
    (progn
      (prefer-coding-system 'utf-8)
      (set-terminal-coding-system 'utf-8)
      (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))))

;;;_ , multi-term

  (use-package multi-term
    :bind (("C-. t" . multi-term-next)
           ("C-. T" . multi-term))
    :init
    (defun screen ()
      (interactive)
      (let (term-buffer)
        ;; Set buffer.
        (setq term-buffer
              (let ((multi-term-program (executable-find "screen"))
                    (multi-term-program-switches "-DR"))
                (multi-term-get-buffer)))
        (set-buffer term-buffer)
        ;; Internal handle for `multi-term' buffer.
        (multi-term-internal)
        ;; Switch buffer
        (switch-to-buffer term-buffer)))

    :config
    (progn
      (if t
          (defalias 'my-term-send-raw-at-prompt 'term-send-raw)
        (defun my-term-send-raw-at-prompt ()
          (interactive)
          (if (save-excursion
                (search-backward " $ " (line-beginning-position) t))
              (progn
                (if (memq 'meta (event-modifiers last-command-event))
                    (progn
                      (term-send-raw-string
                       (format "\e%c"
                               (logand last-command-event (lognot #x8000000)))))
                  (call-interactively #'term-send-raw)))
            (call-interactively (lookup-key (current-global-map)
                                            (vector last-command-event))))))

      (defun my-term-end-of-buffer ()
        (interactive)
        (call-interactively #'end-of-buffer)
        (if (and (eobp) (bolp))
            (delete-char -1)))

      (require 'term)

      (defadvice term-process-pager (after term-process-rebind-keys activate)
        (define-key term-pager-break-map  "\177" 'term-pager-back-page))))


  ;; multiple cursors
  (use-package multiple-cursors
    :bind (("H-c ." . mc/mark-next-like-this)
           ("H-c ," . mc/mark-previous-like-this)
           ("H-c C-l". mc/mark-all-like-this)))

;;;_ , nf-procmail-mode

  (use-package nf-procmail-mode
    :commands nf-procmail-mode)

;;;_ , nroff-mode

  (use-package nroff-mode
    :commands nroff-mode
    :config
    (progn
      (defun update-nroff-timestamp ()
        (save-excursion
          (goto-char (point-min))
          (when (re-search-forward "^\\.Dd ")
            (let ((stamp (format-time-string "%B %e, %Y")))
              (unless (looking-at stamp)
                (delete-region (point) (line-end-position))
                (insert stamp)
                (let (after-save-hook)
                  (save-buffer)))))))

      (add-hook 'nroff-mode-hook
                #'(lambda ()
                    (add-hook 'after-save-hook 'update-nroff-timestamp nil t)))))

;;;_ , nxml-mode

  (use-package nxml-mode
    :commands nxml-mode
    :init
    (defalias 'xml-mode 'nxml-mode)
    :config
    (progn
      (defun my-nxml-mode-hook ()
        (bind-key "<return>" 'newline-and-indent nxml-mode-map))

      (add-hook 'nxml-mode-hook 'my-nxml-mode-hook)

      (defun tidy-xml-buffer ()
        (interactive)
        (save-excursion
          (call-process-region (point-min) (point-max) "tidy" t t nil
                               "-xml" "-i" "-wrap" "0" "-omit" "-q")))

      (bind-key "C-H" 'tidy-xml-buffer nxml-mode-map)))

  ;; ;;;_ , org-mode
  ;;

  (use-package dot-org
    :if (not running-alternate-emacs)
    :commands org-agenda-list
    :bind (
           ("C-c l" . org-store-link)
           ("<f12>" . org-agenda)
           ("<f5>"  . bh/org-todo)
           ("<S-f5>" . bh/widen)
           ("<f7>" . bh/set-truncate-lines)
           ("<f8>" . org-cycle-agenda-files)
           ("<f9> <f9>" . bh/show-org-agenda)
           ("<f9> b" . bbdb)
           ("<f9> c" . calendar)
           ("<f9> f" . boxquote-insert-file)
           ("<f9> g" . gnus)
           ("<f9> h" . bh/hide-other)
           ("<f9> n" . org-narrow-to-subtree)
           ("<f9> W" . widen)
           ("<f9> u" . bh/narrow-up-one-level)

           ("<f9> I" . bh/punch-in)
           ("<f9> O" . bh/punch-out)

           ("<f9> o" . bh/make-org-scratch)

           ("<f9> r" . boxquote-region)
           ("<f9> s" . bh/switch-to-scratch)

           ("<f9> t" . bh/insert-inactive-timestamp)
           ("<f9> T" . tabify)
           ;;         ("<f9> U" . untabify)

           ("<f9> v" . visible-mode)
           ("<f9> SPC" . bh/clock-in-last-task)
           ("C-<f9>" . previous-buffer)
           ("M-<f9>" . org-toggle-inline-images)
           ("C-x n r" . narrow-to-region)
           ("C-<f10>" . next-buffer)
           ("<f11>" . org-clock-goto)
           ("C-<f11>" . org-clock-in)
           ("C-s-<f12>" . bh/save-then-publish)
           ("C-M-r" . org-capture)
           ("C-c r" . org-capture)
           )
    :init
    (progn
      (require 'yasnippet)
      (require 'org)
      (require 'org-habit) ;; added by dkh
      (require 'org-drill)
      ;; (org-babel-tangle "~/.emacs.d/dkh-org.org")
      (org-babel-load-file "~/.emacs.d/dkh-org.org")
      (define-key org-mode-map (kbd "C-c k") 'org-cut-subtree)

      (setq org-export-with-section-numbers nil)
      (setq org-html-include-timestamps nil)

      (defun sacha/org-export-subtree-as-html-fragment ()
        (interactive)
        (org-export-region-as-html
         (org-back-to-heading)
         (org-end-of-subtree)
         t))

      ;; (setq org-link-abbrev-alist
      ;;   '(("google" . "http://www.google.com/search?q=")
      ;;     ("gmap" . "http://maps.google.com/maps?q=%s")
      ;;     ("blog" . "http://sachachua.com/blog/p/")))

      ;; (org-babel-do-load-languages
      ;;     'org-babel-load-languages '((python . t) (R . t) (perl . t)))

      ;; (bind-key "C-c 1"  (lambda () (interactive) (switch-to-buffer-other-window "*Org Agenda*")))
      (bind-key "C-c 1"  (lambda () (interactive) (switch-to-buffer "*Org Agenda*")))
      ))


  (use-package org-latex
    :config
    (progn
      (require 'org-latex)
      (unless (boundp 'org-export-latex-classes)
        (setq org-export-latex-classes nil))
      (add-to-list 'org-export-latex-classes
                   '("article"
                     "\\documentclass{article}"
                     ("\\section{%s}" . "\\section*{%s}")))

      (add-to-list 'org-export-latex-classes
                   '("article"
                     "\\documentclass{article}"
                     ("\\section{%s}" . "\\section*{%s}")
                     ("\\subsection{%s}" . "\\subsection*{%s}")
                     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                     ("\\paragraph{%s}" . "\\paragraph*{%s}")
                     ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

      (add-to-list 'org-export-latex-classes
                   `("book"
                     "\\documentclass{book}"
                     ("\\part{%s}" . "\\part*{%s}")
                     ("\\chapter{%s}" . "\\chapter*{%s}")
                     ("\\section{%s}" . "\\section*{%s}")
                     ("\\subsection{%s}" . "\\subsection*{%s}")
                     ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
                   )

      (add-to-list 'org-export-latex-classes
                   '("org-article"
                     "\\documentclass{org-article}
         [NO-DEFAULT-PACKAGES]
         [PACKAGES]
         [EXTRA]"
                     ("\\section{%s}" . "\\section*{%s}")
                     ("\\subsection{%s}" . "\\subsection*{%s}")
                     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                     ("\\paragraph{%s}" . "\\paragraph*{%s}")
                     ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

      (add-to-list 'org-export-latex-classes
                   '("koma-article"
                     "\\documentclass{scrartcl}
             [NO-DEFAULT-PACKAGES]
             [EXTRA]"
                     ("\\section{%s}" . "\\section*{%s}")
                     ("\\subsection{%s}" . "\\subsection*{%s}")
                     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                     ("\\paragraph{%s}" . "\\paragraph*{%s}")
                     ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

      (setq org-export-latex-listings 'minted)
      (setq org-export-latex-custom-lang-environments
            '(
              (emacs-lisp "common-lispcode")
              ))
      (setq org-export-latex-minted-options
            '(("frame" "lines")
              ("fontsize" "\\scriptsize")
              ("linenos" "")))
      (setq org-latex-to-pdf-process
            '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
              "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
              "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

      (setq org-export-latex-listings 'listings)
      (setq org-export-latex-custom-lang-environments
            '((emacs-lisp "common-lispcode")))
      (setq org-export-latex-listings-options
            '(("frame" "lines")
              ("basicstyle" "\\footnotesize")
              ("numbers" "left")
              ("numberstyle" "\\tiny")))
      (setq org-latex-to-pdf-process
            '("pdflatex -interaction nonstopmode -output-directory %o %f"
              "pdflatex -interaction nonstopmode -output-directory %o %f"
              "pdflatex -interaction nonstopmode -output-directory %o %f"))
      (org-add-link-type
       "latex" nil
       (lambda (path desc format)
         (cond
          ((eq format 'html)
           (format "<span class=\"%s\">%s</span>" path desc))
          ((eq format 'latex)
           (format "\\%s{%s}" path desc)))))
      ))

  (use-package org-jira
    :load-path ("~/.emacs.d/elpa/jira-0.3.3"
                "~/.emacs.d/site-lisp/org-jira")
    :commands (org-jira-create-issue org-jira-get-issue)
    :bind ("H-j" . org-jira-create-issue)
    :init
    (progn

      (require 'jira)
      (setq jira-url "https://cuboulder.atlassian.net//rpc/xmlrpc")
      (require 'org-jira)
      (setq jiralib-url "https://cuboulder.atlassian.net")
      ;; jiralib is not explicitly required, since org-jira will load it.
      (setq org-jira-working-dir "~/git/dkh-org/.org-jira")
      (setq org-jira-current-project "VCR")

      (setq jira-users
            (list
             (cons "Administrator" "webcom")
             (cons "Matt Tucker" "matt")
             (cons "Kevin Crafts" "kevin")
             (cons "Damon Haley" "daha1836")
             (cons "Alfredo Nevarez" "alfredo")
             (cons "Catherine Snider" "snider")
             (cons "Kosta Tovstiadi" "kosta")
             (cons "Will Kubie" "kubie")
             (cons "Joanna Bertrand" "joanna")
             (cons "Wendy Turnbull" "wendy")
             ))))



;;;_ , pabbrev

  (use-package pabbrev
    :commands pabbrev-mode
    :diminish pabbrev-mode)


  (use-package pandoc-mode
    :commands (turn-on-pandoc
               pandoc-load-default-settings))


;;;_ , paredit

  (use-package paredit
    :commands paredit-mode
    :diminish paredit-mode
    :config
    (progn
      (use-package paredit-ext)

      (bind-key "C-M-l" 'paredit-recentre-on-sexp paredit-mode-map)

      (bind-key ")" 'paredit-close-round-and-newline paredit-mode-map)
      (bind-key "M-)" 'paredit-close-round paredit-mode-map)

      (bind-key "M-k" 'paredit-raise-sexp paredit-mode-map)
      (bind-key "M-h" 'mark-containing-sexp paredit-mode-map)
      (bind-key "M-I" 'paredit-splice-sexp paredit-mode-map)

      (unbind-key "M-r" paredit-mode-map)
      (unbind-key "M-s" paredit-mode-map)

      (bind-key "C-. d" 'paredit-forward-down paredit-mode-map)
      (bind-key "C-. B" 'paredit-splice-sexp-killing-backward paredit-mode-map)
      (bind-key "C-. C" 'paredit-convolute-sexp paredit-mode-map)
      (bind-key "C-. F" 'paredit-splice-sexp-killing-forward paredit-mode-map)
      (bind-key "C-. a" 'paredit-add-to-next-list paredit-mode-map)
      (bind-key "C-. A" 'paredit-add-to-previous-list paredit-mode-map)
      (bind-key "C-. j" 'paredit-join-with-next-list paredit-mode-map)
      (bind-key "C-. J" 'paredit-join-with-previous-list paredit-mode-map)

      (add-hook 'allout-mode-hook
                #'(lambda ()
                    (bind-key "M-k" 'paredit-raise-sexp allout-mode-map)
                    (bind-key "M-h" 'mark-containing-sexp allout-mode-map)))))

;;;_ , paren

  (unless
      (use-package mic-paren
        :init
        (paren-activate))

    (use-package paren
      :init
      (show-paren-mode 1)))

;;;_ , per-window-point

  (use-package per-window-point
    :init
    (pwp-mode 1))

;;;_ , persistent-scratch

  (use-package persistent-scratch
    :if (and window-system (not running-alternate-emacs)
             (not noninteractive)))

;;;_ , popup-ruler

  (use-package popup-ruler
    :bind (("C-. r" . popup-ruler)
           ("C-. R" . popup-ruler-vertical)))

;;;_ , pp-c-l

  (use-package pp-c-l
    :init
    (hook-into-modes 'pretty-control-l-mode '(prog-mode-hook)))

  (use-package proof-site
    :load-path "site-lisp/proofgeneral/generic/"
    :config
    (progn
      (eval-after-load "coq"
        '(progn
           (add-hook 'coq-mode-hook
                     (lambda ()
                       (yas/minor-mode 1)
                       (whitespace-mode 1)
                       (unicode-tokens-use-shortcuts 0)))
           (bind-key "M-RET" 'proof-goto-point coq-mode-map)
           (bind-key "<tab>" 'yas/expand-from-trigger-key coq-mode-map)
           (bind-key "C-c C-p" (lambda ()
                                 (interactive)
                                 (proof-layout-windows)
                                 (proof-prf)) coq-mode-map)))))


;;;_ , ps-print

  (use-package ps-print
    :defer t
    :config
    (progn
      (defun ps-spool-to-pdf (beg end &rest ignore)
        (interactive "r")
        (let ((temp-file (concat (make-temp-name "ps2pdf") ".pdf")))
          (call-process-region beg end (executable-find "ps2pdf")
                               nil nil nil "-" temp-file)
          (call-process (executable-find "open") nil nil nil temp-file)))

      (setq ps-print-region-function 'ps-spool-to-pdf)))


;;;_ , puppet-mode

  (use-package puppet-mode
    :mode ("\\.pp\\'" . puppet-mode)
    :config
    (use-package puppet-ext))

;;;_ , python-mode

  (use-package python-mode
    :mode ("\\.py\\'" . python-mode)
    :interpreter ("python" . python-mode)
    :config
    (progn
      (defvar python-mode-initialized nil)

      (defun my-python-mode-hook ()
        (unless python-mode-initialized
          (setq python-mode-initialized t)

          (info-lookup-add-help
           :mode 'python-mode
           :regexp "[a-zA-Z_0-9.]+"
           :doc-spec
           '(("(python)Python Module Index" )
             ("(python)Index"
              (lambda
                (item)
                (cond
                 ((string-match
                   "\\([A-Za-z0-9_]+\\)() (in module \\([A-Za-z0-9_.]+\\))" item)
                  (format "%s.%s" (match-string 2 item)
                          (match-string 1 item)))))))))

        (setq indicate-empty-lines t)
        (set (make-local-variable 'parens-require-spaces) nil)
        (setq indent-tabs-mode nil)

        (bind-key "C-c C-z" 'python-shell python-mode-map)
        (unbind-key "C-c c" python-mode-map))

      (add-hook 'python-mode-hook 'my-python-mode-hook)))

;;;_ , quickrun

  (use-package quickrun
    :bind ("C-c C-r" . quickrun))


;;;;_ , rainbow-delimiters

  (use-package rainbow-delimiters
    :load-path "rainbow-delimiters"
    :commands (rainbow-delimiters-mode))


;;;;_ , rainbow-mode

  (use-package rainbow-mode
    ;; :if (and
    ;;      (not degrade-p-terminal)
    ;;      (not degrade-p-font-lock))
    :commands rainbow-mode
    :init
    (progn
      (hook-into-modes #'rainbow-mode
                       '(css-mode-hook
                         stylus-mode-hook
                         sass-mode-hook)))
    :diminish ((rainbow-mode . "rb")))

;;;_ , recentf

  (use-package recentf
    :if (not noninteractive)
    :init
    (progn
      (recentf-mode 1)

      (defun recentf-add-dired-directory ()
        (if (and dired-directory
                 (file-directory-p dired-directory)
                 (not (string= "/" dired-directory)))
            (let ((last-idx (1- (length dired-directory))))
              (recentf-add-file
               (if (= ?/ (aref dired-directory last-idx))
                   (substring dired-directory 0 last-idx)
                 dired-directory)))))

      (add-hook 'dired-mode-hook 'recentf-add-dired-directory)))

;;;_ , repeat-insert

  (use-package repeat-insert
    :disabled t
    :commands (insert-patterned
               insert-patterned-2
               insert-patterned-3
               insert-patterned-4))

;;;_ , ruby-mode

  (use-package ruby-mode
    :mode ("\\.rb\\'" . ruby-mode)
    :interpreter ("ruby" . ruby-mode)
    :config
    (progn
      (use-package yari
        :init
        (progn
          (defvar yari-helm-source-ri-pages
            '((name . "RI documentation")
              (candidates . (lambda () (yari-ruby-obarray)))
              (action  ("Show with Yari" . yari))
              (candidate-number-limit . 300)
              (requires-pattern . 2)
              "Source for completing RI documentation."))

          (defun helm-yari (&optional rehash)
            (interactive (list current-prefix-arg))
            (when current-prefix-arg (yari-ruby-obarray rehash))
            (helm 'yari-helm-source-ri-pages (yari-symbol-at-point)))))

      (defun my-ruby-smart-return ()
        (interactive)
        (when (memq (char-after) '(?\| ?\" ?\'))
          (forward-char))
        (call-interactively 'newline-and-indent))

      (defun my-ruby-mode-hook ()
        (require 'inf-ruby)
        (inf-ruby-keys)

        (bind-key "<return>" 'my-ruby-smart-return ruby-mode-map)
        (bind-key "C-h C-i" 'helm-yari ruby-mode-map)

        (set (make-local-variable 'yas/fallback-behavior)
             '(apply ruby-indent-command . nil))
        (bind-key "<tab>" 'yas/expand-from-trigger-key ruby-mode-map))

      (add-hook 'ruby-mode-hook 'my-ruby-mode-hook)))

;;;_ , sage-mode

  (use-package sage
    :load-path "/Applications/Misc/sage/local/share/emacs/"
    :init
    (progn
      (setq sage-command "/Applications/Misc/sage/sage")

      ;; If you want sage-view to typeset all your output and have plot()
      ;; commands inline, uncomment the following line and configure sage-view:
      (require 'sage-view "sage-view")
      (add-hook 'sage-startup-before-prompt-hook 'compilation-setup)
      (add-hook 'sage-startup-after-prompt-hook 'sage-view)
      ;; You can use commands like
      ;; (add-hook 'sage-startup-after-prompt-hook 'sage-view-disable-inline-output)
      (add-hook 'sage-startup-after-prompt-hook 'sage-view-disable-inline-plots t)
      ;; to enable some combination of features
      ))

  ;; Sauron

  (use-package sauron
    :load-path "~/.emacs.d/site-lisp/sauron"
    :if running-alternate-emacs
    :bind (("C-. s" . sauron-toggle-hide-show)
           ("C-. R" . sauron-clear))
    :init
    (progn

      ;; uncomment to show sauron in the current frame
      (setq sauron-separate-frame nil)

      ;; watch for some animals
      (setq sauron-watch-patterns 'erc-keywords)
      (setq sauron-watch-nicks 'erc-pals)


      ;; events to ignore
      (add-hook 'sauron-event-block-functions
                (lambda (origin prio msg &optional props)
                  (or
                   (string-match "^*** Users" msg)))) ;; filter out IRC spam

      )
    :config
    (progn
      ;; John Wiegley’s alert.el has a bit of overlap with sauron; however, I’ve
      ;; added some wrapper function to make it trivial to feed sauron events
      ;; into alert. Simply adding:
      (add-hook 'sauron-event-added-functions 'sauron-alert-el-adapter)))

  ;; Saveplace
  ;; - places cursor in the last place you edited file
  (use-package saveplace
    :config
    (progn
      (setq-default save-place t)
      ;; Keep places in the load path
      (setq save-place-file "~/Documents/places")))

;;;_ , selectkey

  (use-package selectkey
    :init
    (progn
      (bind-key "C-. b" 'selectkey-select-prefix-map)

      (selectkey-define-select-key compile "c" "\\*compilation")
      (selectkey-define-select-key shell-command "o" "Shell Command")
      (selectkey-define-select-key shell "s" "\\*shell" (shell))
      (selectkey-define-select-key multi-term "t" "\\*terminal" (multi-term-next))
      (selectkey-define-select-key eshell "z" "\\*eshell" (eshell))))



;;;_ , session

  (use-package session
    :if (not noninteractive)
    :load-path "site-lisp/session/lisp/"
    :init
    (progn
      (session-initialize)

      (defun remove-session-use-package-from-settings ()
        (when (string= (file-name-nondirectory (buffer-file-name)) "settings.el")
          (save-excursion
            (goto-char (point-min))
            (when (re-search-forward "^ '(session-use-package " nil t)
              (delete-region (line-beginning-position)
                             (1+ (line-end-position)))))))

      (add-hook 'before-save-hook 'remove-session-use-package-from-settings)

      ;; expanded folded secitons as required
      (defun le::maybe-reveal ()
        (when (and (or (memq major-mode  '(org-mode outline-mode))
                       (and (boundp 'outline-minor-mode)
                            outline-minor-mode))
                   (outline-invisible-p))
          (if (eq major-mode 'org-mode)
              (org-reveal)
            (show-subtree))))

      (add-hook 'session-after-jump-to-last-change-hook
                'le::maybe-reveal)

      (defun save-information ()
        (with-temp-message "Saving Emacs information..."
          (recentf-cleanup)

          (loop for func in kill-emacs-hook
                unless (memq func '(exit-gnus-on-exit server-force-stop))
                do (funcall func))

          (unless (or noninteractive
                      running-alternate-emacs
                      (eq 'listen (process-status server-process)))
            (server-start))))

      (run-with-idle-timer 300 t 'save-information)

      (if window-system
          (add-hook 'after-init-hook 'session-initialize t))))

;;;_ , sh-script

  (use-package sh-script
    :defer t
    :config
    (progn
      (defvar sh-script-initialized nil)
      (defun initialize-sh-script ()
        (unless sh-script-initialized
          (setq sh-script-initialized t)
          (info-lookup-add-help :mode 'shell-script-mode
                                :regexp ".*"
                                :doc-spec
                                '(("(bash)Index")))))

      (add-hook 'shell-mode-hook 'initialize-sh-script)))

;;;_ , sh-toggle

  (use-package sh-toggle
    :bind ("C-. C-z" . shell-toggle))

  (use-package shell-mode
    :defer t
    :config
    (progn

      ;; by Ellen Taylor, 2012-07-20
      ;; (defadvice shell (around always-new-shell)
      ;;   "Always start a new shell."
      ;;   (let ((buffer (generate-new-buffer-name "*shell*"))) ad-do-it))

      ;; (ad-activate 'shell)


      (defun comint-delchar-or-eof-or-kill-buffer (arg)
        (interactive "p")
        (if (null (get-buffer-process (current-buffer)))
            (kill-buffer)
          (comint-delchar-or-maybe-eof arg)))

      (add-hook 'shell-mode-hook
                (lambda ()
                  (define-key shell-mode-map
                    (kbd "C-d") 'comint-delchar-or-eof-or-kill-buffer)))))
;;;_ , smart-compile

  (use-package smart-compile
    :commands smart-compile
    :bind (
           ;;         ("C-c c" . smart-compile)
           ("A-n"   . next-error)
           ("A-p"   . previous-error))
    :init
    (progn
      (defun show-compilation ()
        (interactive)
        (let ((compile-buf
               (catch 'found
                 (dolist (buf (buffer-list))
                   (if (string-match "\\*compilation\\*" (buffer-name buf))
                       (throw 'found buf))))))
          (if compile-buf
              (switch-to-buffer-other-window compile-buf)
            (call-interactively 'compile))))

      (bind-key "M-O" 'show-compilation)))

;;;_ , smerge-mode

  (use-package smerge-mode
    :commands (smerge-mode smerge-command-prefix)
    :init
    (setq smerge-command-prefix (kbd "C-. C-.")))

;;;;_ , smex

  (use-package smex
    :load-path "smex"
    :bind ("M-X" . dhl-invoke-smex)
    :requires ido
    :config
    (progn
      (smex-initialize)
      (setq smex-save-file "~/.smex")
      (smex-auto-update)

      (defun dhl-invoke-smex (x)
        "Invokes smex, if called without a prefix argument,
smex-major-mode-commands otherwise. Note that this
prevents using commands with prefix arguments."
        (interactive "p")
        (if (= x 1)
            (smex)
          (smex-major-mode-commands)))))


;;;_ , stopwatch

  (use-package stopwatch
    :bind ("<f8>" . stopwatch))

;;;_ , sunrise-commander

  (use-package sunrise-commander
    :disabled t
    :commands (sunrise sunrise-cd)
    :init
    (progn
      (defun my-activate-sunrise ()
        (interactive)
        (let ((sunrise-exists
               (loop for buf in (buffer-list)
                     when (string-match " (Sunrise)$" (buffer-name buf))
                     return buf)))
          (if sunrise-exists
              (call-interactively 'sunrise)
            (sunrise "~/dl/" "~/Archives/"))))

      (bind-key "C-c j" 'my-activate-sunrise)
      (bind-key "C-c C-j" 'sunrise-cd))

    :config
    (progn
      (require 'sunrise-x-modeline)
      (require 'sunrise-x-tree)
      (require 'sunrise-x-tabs)

      (bind-key "/" 'sr-sticky-isearch-forward sr-mode-map)
      (bind-key "<backspace>" 'sr-scroll-quick-view-down sr-mode-map)
      (bind-key "C-x t" 'sr-toggle-truncate-lines sr-mode-map)

      (bind-key "q" 'sr-history-prev sr-mode-map)
      (bind-key "z" 'sr-quit sr-mode-map)

      (unbind-key "C-e" sr-mode-map)
      (unbind-key "C-p" sr-tabs-mode-map)
      (unbind-key "C-n" sr-tabs-mode-map)
      (unbind-key "M-<backspace>" sr-term-line-minor-mode-map)

      (bind-key "M-[" 'sr-tabs-prev sr-tabs-mode-map)
      (bind-key "M-]" 'sr-tabs-next sr-tabs-mode-map)

      (defun sr-browse-file (&optional file)
        "Display the selected file with the default appication."
        (interactive)
        (setq file (or file (dired-get-filename)))
        (save-selected-window
          (sr-select-viewer-window)
          (let ((buff (current-buffer))
                (fname (if (file-directory-p file)
                           file
                         (file-name-nondirectory file)))
                (app (cond
                      ((eq system-type 'darwin)       "open %s")
                      ((eq system-type 'windows-nt)   "open %s")
                      (t                              "xdg-open %s"))))
            (start-process-shell-command "open" nil (format app file))
            (unless (eq buff (current-buffer))
              (sr-scrollable-viewer (current-buffer)))
            (message "Opening \"%s\" ..." fname))))

      (defun sr-goto-dir (dir)
        "Change the current directory in the active pane to the given one."
        (interactive (list (progn
                             (require 'lusty-explorer)
                             (lusty-read-directory))))
        (if sr-goto-dir-function
            (funcall sr-goto-dir-function dir)
          (unless (and (eq major-mode 'sr-mode)
                       (sr-equal-dirs dir default-directory))
            (if (and sr-avfs-root
                     (null (posix-string-match "#" dir)))
                (setq dir (replace-regexp-in-string
                           (expand-file-name sr-avfs-root) "" dir)))
            (sr-save-aspect
             (sr-within dir (sr-alternate-buffer (dired dir))))
            (sr-history-push default-directory)
            (sr-beginning-of-buffer))))))

;;;_ , texinfo

  (use-package texinfo
    :defines texinfo-section-list
    :mode ("\\.texi\\'" . texinfo-mode)
    :config
    (progn
      (defun my-texinfo-mode-hook ()
        (dolist (mapping '((?b . "emph")
                           (?c . "code")
                           (?s . "samp")
                           (?d . "dfn")
                           (?o . "option")
                           (?x . "pxref")))
          (local-set-key (vector (list 'alt (car mapping)))
                         `(lambda () (interactive)
                            (TeX-insert-macro ,(cdr mapping))))))

      (add-hook 'texinfo-mode-hook 'my-texinfo-mode-hook)

      (defun texinfo-outline-level ()
        ;; Calculate level of current texinfo outline heading.
        (require 'texinfo)
        (save-excursion
          (if (bobp)
              0
            (forward-char 1)
            (let* ((word (buffer-substring-no-properties
                          (point) (progn (forward-word 1) (point))))
                   (entry (assoc word texinfo-section-list)))
              (if entry
                  (nth 1 entry)
                5)))))))

  ;; setup tramp mode
  ;; Tramp mode: allow me to SSH to hosts and edit as sudo like:
  ;; C-x C-f /sudo:example.com:/etc/something-owned-by-root
  ;; from: http://www.gnu.org/software/tramp/#Multi_002dhops
  (use-package tramp
    :config
    (progn
      (setq tramp-default-method "ssh")
      (add-to-list 'tramp-default-proxies-alist
                   '(nil "\\`root\\'" "/ssh:%h:"))
      (add-to-list 'tramp-default-proxies-alist
                   '((regexp-quote (system-name)) nil nil))))


  ;; https://github.com/anthracite/emacs-config/blob/master/init.el
;;;;_ , twittering-mode

  (use-package twittering-mode
    :load-path "twittering-mode"
    :commands twit
    :config
    (progn
      (setq twittering-icon-mode t
            twittering-timer-interval 150
            twittering-number-of-tweets-on-retrieval 100
            Twittering-use-ssl t
            twittering-use-master-password nil
            twittering-scroll-mode t
            twittering-initial-timeline-spec-string '(":home"
                                                      ":replies"
                                                      ":favorites"
                                                      ":direct_messages"))
      (twittering-enable-unread-status-notifier)

      (define-keys twittering-mode-map
        '(("n" twittering-goto-next-status)
          ("p" twittering-goto-previous-status)
          ("j" twittering-goto-next-status-of-user)
          ("k" twittering-goto-previous-status-of-user)))
      ))



;;;_ , vkill

  (use-package vkill
    :commands vkill
    :init
    (progn
      (defun vkill-and-helm-occur ()
        (interactive)
        (vkill)
        (call-interactively #'helm-occur))

      (bind-key "C-x L" 'vkill-and-helm-occur))

    :config
    (setq vkill-show-all-processes t))

  (use-package rgr-web
    ;;  :commands
    :load-path "~/.emacs.d/lisp/"
    :bind (("<f4>" . rgr/browse-url)
           ))




;;;_ , w3m

  (use-package w3m
    :commands (w3m-search w3m-find-file)
    :bind (("C-. u"   . w3m-browse-url)
           ("C-. U"   . w3m-browse-url-new-session)
           ("C-. A-u" . w3m-browse-chrome-url-new-session)
           )
    :init
    (progn
      (setq w3m-command "/opt/local/bin/w3m")

      (setq w3m-session-file "~/Documents/w3m-session")

      (setq w3m-coding-system 'utf-8
            w3m-file-coding-system 'utf-8
            w3m-file-name-coding-system 'utf-8
            w3m-input-coding-system 'utf-8
            w3m-output-coding-system 'utf-8
            w3m-terminal-coding-system 'utf-8)

      (add-hook 'w3m-mode-hook 'w3m-link-numbering-mode)

      (autoload 'w3m-session-crash-recovery-remove "w3m-session")

      (defvar ivan-w3m-facebook-user
        "user@facebook.com"
        "Facebook user name.")

      (defvar ivan-w3m-facebook-password
        "secret"
        "Facebook password.")

      (defalias 'fb 'ivan-w3m-open-facebook)

      (defun ivan-w3m-open-facebook ()
        "Open Facebook."
        (interactive)
        (setq w3m-async-exec nil)
        (w3m-goto-url "http://m.facebook.com?l=en_US")
        (goto-char (point-min))
        (ivan-w3m-fill-entry
         "Email" ivan-w3m-facebook-user 'ivan-w3m-fill-text)
        (ivan-w3m-fill-entry
         "Password" ivan-w3m-facebook-password 'ivan-w3m-fill-password)
        (search-forward "Log In")
        (left-char 1)
        (setq w3m-async-exec t)
        (widget-button-press (point)))


      (defun show-browser ()
        (interactive)
        (let ((w3m-buf
               (catch 'found
                 (dolist (buf (buffer-list))
                   (if (string-match "\\*w3m" (buffer-name buf))
                       (throw 'found buf))))))
          (if w3m-buf
              (switch-to-buffer-other-window w3m-buf)
            (call-interactively 'w3m-find-file))))

      (bind-key "C-. w" 'show-browser)

      (defun wikipedia-query (term)
        (interactive (list (read-string "Wikipedia search: " (word-at-point))))
        (require 'w3m-search)
        (w3m-search "en.wikipedia" term))

      (eval-when-compile
        (autoload 'w3m-search-escape-query-string "w3m-search"))

      (defun wolfram-alpha-query (term)
        (interactive (list (read-string "Ask Wolfram Alpha: " (word-at-point))))
        (require 'w3m-search)
        (w3m-browse-url (concat "http://m.wolframalpha.com/input/?i="
                                (w3m-search-escape-query-string term))))

      (defun goto-emacswiki ()
        (interactive)
        (w3m-browse-url "http://www.emacswiki.org"))

      (defun w3m-browse-url-new-session (url)
        (interactive (progn
                       (require 'browse-url)
                       (browse-url-interactive-arg "Emacs-w3m URL: ")))
        (w3m-browse-url url t))

      (defun w3m-browse-chrome-url-new-session ()
        (interactive)
        (let ((url (do-applescript
                    (string-to-multibyte "tell application \"Google Chrome\"
  URL of active tab of front window
  end tell"))))
          (w3m-browse-url (substring url 1 (1- (length url))) t)))

      (bind-key "A-M-e" 'goto-emacswiki)
      (bind-key "A-M-g" 'w3m-search)
      (bind-key "A-M-w" 'wikipedia-query))

    :config
    ;; (eval-after-load "w3m"
    ;;   '(progn
    ;;      (setq w3m-add-user-agent nil
    ;;            w3m-default-display-inline-images t
    ;;            w3m-default-save-directory "~/.emacs.d/.w3m"
    ;;            w3m-favicon-use-cache-file t
    ;;            w3m-key-binding (quote info)
    ;;            w3m-profile-directory "~/.emacs.d/.w3m"
    ;;            w3m-resize-images t
    ;;            w3m-cookie-accept-bad-cookies t
    ;;            w3m-use-cookies nil
    ;;            w3m-key-binding (quote info)
    ;;            w3m-display-inline-image t
    ;;             ;; added my DKH
    ;;             w3m-home-page "http://www.emacswiki.org/"
    ;;            w3m-command-arguments
    ;;            (nconc w3m-command-arguments

    ;;                   '("-o" "http_proxy=http://192.168.0.5:8118"))
    ;;            w3m-no-proxy-domains '("colorado.edu"
    ;;            "competitions.colorado.edu" "neighbor.com" "jobsatcu.com"
    ;;            "identi.ca" "vinylisland.org" "dhaley.org")
    ;;            )))


    (let (proxy-host proxy-port)
      (with-temp-buffer
        (shell-command "scutil --proxy" (current-buffer))

        (when (re-search-forward "HTTPPort : \\([0-9]+\\)" nil t)
          (setq proxy-port (match-string 1)))
        (when (re-search-forward "HTTPProxy : \\(\\S-+\\)" nil t)
          (setq proxy-host (match-string 1))))

      (if (and proxy-host proxy-port)
          (setq w3m-command-arguments
                (nconc w3m-command-arguments
                       (list "-o" (format "http_proxy=http://%s:%s/"
                                          proxy-host proxy-port)))))

      (use-package w3m-type-ahead
        :requires w3m
        :init
        (add-hook 'w3m-mode-hook 'w3m-type-ahead-mode))

      (add-hook 'w3m-display-hook
                (lambda (url)
                  (let ((buffer-read-only nil))
                    (delete-trailing-whitespace))))

      (defun my-w3m-linknum-follow ()
        (interactive)
        (w3m-linknum-follow))

      (bind-key "k" 'w3m-delete-buffer w3m-mode-map)
      (bind-key "i" 'w3m-view-previous-page w3m-mode-map)
      (bind-key "p" 'w3m-previous-anchor w3m-mode-map)
      (bind-key "n" 'w3m-next-anchor w3m-mode-map)

      (defun dka-w3m-textarea-hook()
        (save-excursion
          (while (re-search-forward "\r\n" nil t)
            (replace-match "\n" nil nil))
          (delete-other-windows)))

      (add-hook 'w3m-form-input-textarea-mode-hook 'dka-w3m-textarea-hook)

      (bind-key "<return>" 'w3m-view-url-with-external-browser
                w3m-minor-mode-map)
      (bind-key "S-<return>" 'w3m-safe-view-this-url w3m-minor-mode-map)))

;;;_ , wcount-mode

  (use-package wcount-mode
    :commands wcount)

  ;;_ , webjump

  (use-package webjump
    :commands webjump
    :config
    (progn
      (setq webjump-sites (append '(                    ("Java API" .
                                                         [simple-query "www.google.com" "http://www.google.com/search?hl=en&as_sitesearch=http://java.sun.com/javase/6/docs/api/&q=" ""])
                                                        ("Stack Overflow" . "www.stackoverlow.com")
                                                        ("Pop's Site"   . "www.joebob-and-son.com/")

                                                        )
                                  webjump-sample-sites))
      ;; Add Urban Dictionary to webjump
      (eval-after-load "webjump"
        '(add-to-list 'webjump-sites
                      '("Urban Dictionary" .
                        [simple-query
                         "www.urbandictionary.com"
                         "http://www.urbandictionary.com/define.php?term="
                         ""])))))

;;;_ , whitespace

  (use-package whitespace
    :diminish (global-whitespace-mode
               whitespace-mode
               whitespace-newline-mode)
    :commands (whitespace-buffer
               whitespace-cleanup
               whitespace-mode)
    :init
    (progn
      ;; display only tails of lines longer than 80 columns, tabs and
      ;; trailing whitespaces
      ;; style information is here: http://www.emacswiki.org/emacs/WhiteSpace
      (setq whitespace-line-column 80
            whitespace-style '(face tabs trailing lines-tail))

      (add-hook 'prog-mode-hook 'enable-whitespace-mode)

      (setq modes-where-I-want-whitespace-mode-to-be-enabled
            '(ruby-mode-hook
              ;; javascript-mode-hook
              ;; js-mode-hook
              ;; css-mode-hook
              sass-mode-hook
              yaml-mode-hook
              emacs-lisp-mode-hook
              ;; nxhtml-mode-hook
              ))

      (mapc (lambda (mode-hook)
              (add-hook mode-hook 'enable-whitespace-mode))
            modes-where-I-want-whitespace-mode-to-be-enabled)

      (defun enable-whitespace-mode ()
        (whitespace-mode 1)
        )

      ;; ;; (global-whitespace-mode t)
      ;; ;; (setq whitespace-global-modes '(not dired-mode tar-mode))
      ;; (setq whitespace-global-modes '(not erc-mode web-mode))

      ;; (hook-into-modes 'whitespace-mode
      ;;                  '(prog-mode-hook
      ;;                    c-mode-common-hook))

      (defun normalize-file ()
        (interactive)
        (save-excursion
          (goto-char (point-min))
          (whitespace-cleanup)
          (delete-trailing-whitespace)
          (goto-char (point-max))
          (delete-blank-lines)
          (set-buffer-file-coding-system 'unix)
          (goto-char (point-min))
          (while (re-search-forward "\r$" nil t)
            (replace-match ""))
          (set-buffer-file-coding-system 'utf-8)
          (let ((require-final-newline t))
            (save-buffer))))

      (defun maybe-turn-on-whitespace ()
        "Depending on the file, maybe clean up whitespace."
        (let ((file (expand-file-name ".clean"))
              parent-dir)
          (while (and (not (file-exists-p file))
                      (progn
                        (setq parent-dir
                              (file-name-directory
                               (directory-file-name
                                (file-name-directory file))))
                        ;; Give up if we are already at the root dir.
                        (not (string= (file-name-directory file)
                                      parent-dir))))
            ;; Move up to the parent dir and try again.
            (setq file (expand-file-name ".clean" parent-dir)))
          ;; If we found a change log in a parent, use that.
          (when (and (file-exists-p file)
                     (not (file-exists-p ".noclean"))
                     (not (and buffer-file-name
                               (string-match "\\.texi\\'" buffer-file-name))))
            (add-hook 'write-contents-hooks
                      #'(lambda ()
                          (ignore (whitespace-cleanup))) nil t)
            (whitespace-cleanup))))

      (add-hook 'find-file-hooks 'maybe-turn-on-whitespace t))

    :config
    (progn
      (remove-hook 'find-file-hooks 'whitespace-buffer)
      (remove-hook 'kill-buffer-hook 'whitespace-buffer)))


;;;_ , web-mode

  (use-package web-mode
    :mode ("\\.tpl\\.php$" . web-mode)
    :init
    (progn
      (defun web-mode-hook () "Hooks for Web mode."
        ;; (setq web-mode-markup-indent-offset 2)
        ;; (setq web-mode-css-indent-offset 2)
        ;; (setq web-mode-code-indent-offset 2)
        ;; (set-face-attribute 'web-mode-css-rule-face nil :foreground "Pink3")
        ;;       Available faces:
        ;; web-mode-doctype-face, web-mode-html-tag-face, web-mode-html-attr-name-face, web-mode-html-attr-value-face
        ;; web-mode-css-rule-face, web-mode-css-prop-face, web-mode-css-pseudo-class-face, web-mode-css-at-rule-face
        ;; web-mode-preprocessor-face, web-mode-string-face, web-mode-comment-face
        ;; web-mode-variable-name-face, web-mode-function-name-face, web-mode-constant-face, web-mode-type-face, web-mode-keyword-face
        ;; web-mode-folded-face
        ;; (define-key web-mode-map (kbd "C-n") 'web-mode-match-tag)
        ;; (add-to-list 'web-mode-snippets '("mydiv" "<div>" "</div>"))
        ;; (setq web-mode-disable-autocompletion t)
        ;; (setq web-mode-disable-css-colorization t)
        ;;       (setq web-mode-extra-php-constants '("constant1" "constant2")) Also available : web-mode-extra-php-keywords, web-mode-extra-js-keywords, web-mode-extra-jsp-keywords, web-mode-extra-asp-keywords
        ;; (Note: do not put this line in the hook)

        )
      ;; (add-hook 'web-mode-hook 'web-mode-hook)
      ;; (add-hook 'local-write-file-hooks (lambda () (delete-trailing-whitespace) nil))
      ;; (local-set-key (kbd "RET") 'newline-and-indent)
      ;; :mode ("\\.\\(php\\|tpl\\|\\.html\\.erb\\)$" . web-mode)
      ;; :interpreter ("web" . web-mode)

      (setq web-mode-engines-alist '(("\\.html\\.twig\\'" . "twig")))

      ))



;;;_ , windmove

  (setq windmove-wrap-around t)
  (windmove-default-keybindings)        ; Move between frames with Shift+arrow
                                        ; windmove shows a stacktrace when
                                        ; there is nothing to move to


  ;; If, like me, you’re a heavy org-mode user, you’ll find that these key
  ;; bindings won’t work in org-mode buffers because org-mode takes them
  ;; over. Happily you can solve this by adding the line
  (setq org-replace-disputed-keys t)

  (defmacro maser/swallow-errors (name f-with-error)
    `(defun ,name ()
       (interactive)
       (condition-case err
           (,f-with-error)
         (error
          (message "%s" (error-message-string err))))))

  (maser/swallow-errors windmove-down-without-errors windmove-down)
  (maser/swallow-errors windmove-up-without-errors windmove-up)
  (maser/swallow-errors windmove-right-without-errors windmove-right)
  (maser/swallow-errors windmove-left-without-errors windmove-left)


  (bind-key "H-'" 'windmove-right-without-errors)
  (bind-key "H-/" 'windmove-down-without-errors)
  (bind-key "H-;" 'windmove-left-without-errors)
  (bind-key "H-[" 'windmove-up-without-errors)

  (require 'framemove)
  ;;   (framemove-default-keybindings)
  ;;
  ;; If you want to integrate framemove and windmove
  ;; You can omit the call to 'framemove-default-keybindings
  ;; And instead do:
  ;;    (require 'framemove)
  ;;(windmove-default-keybindings)
  (setq framemove-hook-into-windmove t)

  (bind-key "M-<up>" 'fm-up-frame)
  (bind-key "M-<down>" 'fm-down-frame)
  (bind-key "M-<left>" 'fm-left-frame)
  (bind-key "M-<right>" 'fm-right-frame)

  (bind-key "M-S-<up>" 'fm-next-frame)


  (use-package wgrep
    :commands (wgrep-setup))

;;;_ , winner

  (use-package winner
    :diminish winner-mode
    :if (not noninteractive)
    :init
    (progn
      (winner-mode 1)

      (bind-key "M-N" 'winner-redo)
      (bind-key "M-P" 'winner-undo)))

;;;_ , dkh-web.el

;;;_ , workgroups

  (use-package workgroups
    :diminish workgroups-mode
    :commands wg-switch-to-index-1
    :if (not noninteractive)
    :init
    (progn
      (defvar workgroups-preload-map)
      (define-prefix-command 'workgroups-preload-map)

      (bind-key "C-8" 'workgroups-preload-map)
      (bind-key "C-8" 'wg-switch-to-index-1 workgroups-preload-map)
      (bind-key "0" 'wg-switch-to-index-0 workgroups-preload-map)
      (bind-key " 1" 'wg-switch-to-index-1 workgroups-preload-map)
      (bind-key " 2" 'wg-switch-to-index-2 workgroups-preload-map)
      (bind-key " 3" 'wg-switch-to-index-3 workgroups-preload-map)
      (bind-key " 4" 'wg-switch-to-index-4 workgroups-preload-map)
      (bind-key " 5" 'wg-switch-to-index-5 workgroups-preload-map)
      (bind-key " 6" 'wg-switch-to-index-6 workgroups-preload-map)
      (bind-key " 7" 'wg-switch-to-index-7 workgroups-preload-map)
      (bind-key " 8" 'wg-switch-to-index-8 workgroups-preload-map)
      (bind-key " 9" 'wg-switch-to-index-9 workgroups-preload-map)

      (bind-key "C-8 e" 'dkh-eshell-macs)

      (bind-key "C-8 a" 'visit-ansi-term)

      )

    :config
    (progn
      (workgroups-mode 1)
      (let ((workgroups-file (expand-file-name "workgroups" user-emacs-directory)))

        (if running-alternate-emacs
            (progn
              (setq wg-file "/Users/daha1836/.emacs.d/data-alt/workgroups")
              (wg-load "/Users/daha1836/.emacs.d/data-alt/workgroups"))
          (if (file-readable-p workgroups-file)
              (wg-load workgroups-file))))

      (require 'powerline)
      (powerline-default)
      (bind-key "C-\\" 'wg-switch-to-previous-workgroup wg-map)
      (bind-key "\\" 'toggle-input-method wg-map)
      )
    )


;;;_ , wrap-region

  (use-package wrap-region
    :commands wrap-region-mode
    :diminish wrap-region-mode
    :config
    (wrap-region-add-wrappers
     '(("$" "$")
       ("/" "/" nil ruby-mode)
       ("/* " " */" "#" (java-mode javascript-mode css-mode
                                   c-mode c++-mode))
       ("`" "`" nil (markdown-mode ruby-mode shell-script-mode)))))

;;;_ , write-room

  (defun write-room ()
    "Make a frame without any bling."
    (interactive)
    ;; to restore:
    ;; (setq mode-line-format (default-value 'mode-line-format))
    (let ((frame (make-frame
                  '((minibuffer . nil)
                    (vertical-scroll-bars . nil)
                    (left-fringe . 0); no fringe
                    (right-fringe . 0)
                    (background-mode . dark)
                    (background-color . "cornsilk")
                    (foreground-color . "black")
                    (cursor-color . "green")
                    (border-width . 0)
                    (border-color . "black"); should be unnecessary
                    (internal-border-width . 64); whitespace!
                    (cursor-type . box)
                    (menu-bar-lines . 0)
                    (tool-bar-lines . 0)
                    (fullscreen . fullboth)  ; this should work
                    (unsplittable . t)))))
      (select-frame frame)
      (find-file "~/Documents/Notes.txt")
      (setq mode-line-format nil
            fill-column 65)
      (set-window-margins (selected-window) 50 50)))


;;;_ , xmsi-mode

  (use-package xmsi-mode
    ;; (autoload 'xmsi-mode "xmsi-math-symbols-input" "Load xmsi minor mode for inputting math (Unicode) symbols." t)
    :commands (xmsi-mode xmsi-math-symbols-input)
    )

;;;_ , yaml-mode

  (use-package yaml-mode
    :mode ("\\.ya?ml\\'" . yaml-mode))

;;;_ , yasnippet

  (use-package yasnippet
    ;; :if (not noninteractive)
    :diminish yas-minor-mode
    :commands (yas-reload-all
               yas-global-mode
               yas-minor-mode
               snippet-mode
               yas-expand
               yas-expand-snippet
               yas-minor-mode-on
               dired-snippets-dir)
    :mode ("/\\.emacs\\.d/snippets/" . snippet-mode)
    :init
    (progn

      (hook-into-modes #'(lambda () (yas/minor-mode 1))
                       '(prog-mode-hook
                         org-mode-hook
                         ruby-mode-hook
                         message-mode-hook
                         erc-mode-hook
                         emacs-lisp-mode-hook
                         pyhon-mode-hook
                         coffee-mode-hook
                         js-mode-hook
                         js2-mode-hook
                         actionscript-mode-hook
                         ))
      (setq ;; Yasnippet
       ;; Dont print yasnippet messages
       yas-verbosity 0
       ;; Snippet directories
       yas-snippet-dirs (list (expand-file-name
                               "snippets" user-emacs-directory))
       ;; Disable yasnippet prompt by default
       ;; (using auto-complete to prompt)
       yas-prompt-functions '(yas-popup-isearch-prompt
                              yas-ido-prompt
                              yas-completing-prompt
                              yas-no-prompt))

      (defalias 'yas/reload-all 'yas-reload-all)
      (defalias 'yas/global-mode 'yas-global-mode)
      (defalias 'yas/minor-mode 'yas-minor-mode)
      (defalias 'yas/expand 'yas-expand)
      (defalias 'yas/expand-snippet 'yas-expand-snippet)

      )
    :config
    (progn
      ;; (yas/initialize)
      ;; (bind-key "C-x y" 'yas-insert-snippet yas-minor-mode-map)
      (use-package popup
        :init
        (progn
          (defun yas-popup-isearch-prompt (prompt choices &optional display-fn)
            (when (featurep 'popup)
              (popup-menu*
               (mapcar
                (lambda (choice)
                  (popup-make-item
                   (or (and display-fn (funcall display-fn choice))
                       choice)
                   :value choice))
                choices)
               :prompt prompt
               ;; start isearch mode immediately
               :isearch t)))))

      (defun dired-snippets-dir ()
        "Open dired in the yas snippets dir."
        (interactive)
        (dired (expand-file-name
                "snippets" user-emacs-directory)))

      (yas-reload-all)

      (yas/load-directory (expand-file-name "snippets/" user-emacs-directory))

      (bind-key "<tab>" 'yas/next-field-or-maybe-expand yas/keymap)

      (defun yas/new-snippet (&optional choose-instead-of-guess)
        (interactive "P")
        (let ((guessed-directories (yas/guess-snippet-directories)))
          (switch-to-buffer "*new snippet*")
          (erase-buffer)
          (kill-all-local-variables)
          (snippet-mode)
          (set (make-local-variable 'yas/guessed-modes)
               (mapcar #'(lambda (d)
                           (intern (yas/table-name (car d))))
                       guessed-directories))
          (unless (and choose-instead-of-guess
                       (not (y-or-n-p "Insert a snippet with useful headers? ")))
            (yas/expand-snippet "\
# -*- mode: snippet -*-
# name: $1
# --
$0"))))

      (bind-key "C-c y TAB" 'yas/expand)
      (bind-key "C-c y n" 'yas/new-snippet)
      (bind-key "C-c y f" 'yas/find-snippets)
      (bind-key "C-c y r" 'yas/reload-all)
      (bind-key "C-c y v" 'yas/visit-snippet-file)

      ))


;;;_ , yaoddmuse

  (use-package yaoddmuse
    :bind (("C-c w f" . yaoddmuse-browse-page-default)
           ("C-c w e" . yaoddmuse-edit-default)
           ("C-c w p" . yaoddmuse-post-library-default)))

;;;_ , zencoding-mode

  (use-package zencoding-mode
    :commands zencoding-mode
    :init
    (progn
      (add-hook 'nxml-mode-hook 'zencoding-mode)
      (add-hook 'html-mode-hook 'zencoding-mode)
      (add-hook 'html-mode-hook
                #'(lambda ()
                    (bind-key "<return>" 'newline-and-indent html-mode-map))))

    :config
    (progn
      (defvar zencoding-mode-keymap (make-sparse-keymap))
      (bind-key "C-c C-c" 'zencoding-expand-line zencoding-mode-keymap)))



;;;_. Post initialization

  (when window-system
    (let ((elapsed (float-time (time-subtract (current-time)
                                              emacs-start-time))))
      (message "Loading %s...done (%.3fs)" load-file-name elapsed))

    (add-hook 'after-init-hook
              `(lambda ()
                 (let ((elapsed (float-time (time-subtract (current-time)
                                                           emacs-start-time))))
                   (message "Loading %s...done (%.3fs) [after-init]"
                            ,load-file-name elapsed)))
              t))


  ;; fix ls probs with dired
  (when (eq system-type 'darwin)
    (require 'ls-lisp)
    (setq ls-lisp-use-insert-directory-program nil))


  (add-to-list 'custom-theme-load-path "~/.emacs.d/site-lisp/solarized-emacs")

  (defun bw-toggle-solarized ()
    "Toggles between solarized light and dark"
    (interactive)
    (cond
     ((custom-theme-enabled-p 'solarized-dark)
      (progn
        (disable-theme 'solarized-dark)
        (enable-theme 'solarized-light)))
     ((custom-theme-enabled-p 'solarized-light)
      (progn
        (disable-theme 'solarized-light)
        (enable-theme 'solarized-dark)))))

  (defadvice load-theme
    (before load-theme)
    (let ((theme-name (ad-get-arg 0)))
      (when (or (eq theme-name 'solarized-dark)
                (eq theme-name 'solarized-light)))))

  (ad-activate 'load-theme)

  (blink-cursor-mode 1)

  (defvar blink-cursor-colors (list  "#92c48f" "#6785c5" "#be369c" "#d9ca65")
    "On each blink the cursor will cycle to the next color in this list.")

  (setq blink-cursor-count 0)

  (defun blink-cursor-timer-function ()
    "Cyberpunk variant of timer `blink-cursor-timer'. OVERWRITES original version in `frame.el'.

This one changes the cursor color on each blink. Define colors in `blink-cursor-colors'."
    (when (not (internal-show-cursor-p))
      (when (>= blink-cursor-count (length blink-cursor-colors))
        (setq blink-cursor-count 0))
      (set-cursor-color (nth blink-cursor-count blink-cursor-colors))
      (setq blink-cursor-count (+ 1 blink-cursor-count))
      )
    (internal-show-cursor nil (not (internal-show-cursor-p)))
    )



  (unless running-alternate-emacs
    ;; (org-babel-load-file "~/.emacs.d/dkh-core.org")
  ;;;_. Load some private settings
    (org-babel-load-file "~/git/.emacs.d/dkh-private.org")
    )





  (defun byte-compile-current-buffer ()
    "`byte-compile' current buffer if it's emacs-lisp-mode and compiled file exists."
    (interactive)
    (when (and (eq major-mode 'emacs-lisp-mode)
               (file-exists-p (byte-compile-dest-file buffer-file-name)))
      (byte-compile-file buffer-file-name)))

  (add-hook 'after-save-hook 'byte-compile-current-buffer)


  ;; OS X Specific configuration

;;; Ignore .DS_Store files with ido mode
  (add-to-list 'ido-ignore-files "\\.DS_Store")
  ;; # FIXME: this is to ignore Dropbox "Icon" files that seem to be
  ;; "Icon", but I can't figure out how to ignore that.
  (add-to-list 'ido-ignore-files "Icon")

  ;; toggle-input-method
  (setq default-input-method "MacOSX")

  ;; fix hostname.local stuff
  (setq system-name (car (split-string system-name "\\.")))

;;; Use default Mac OS X browser
  ;;(setq browse-url-browser-function 'browse-url-default-macosx-browser)

  ;; ispell isn't available on OS X, but aspell is via Homebrew
  ;;(setq-default ispell-program-name "aspell")



  ;; Use Solarized-dark on OS X
  ;; we load the theme after init because we might have changed some
  ;; variables in customize

  (if running-alternate-emacs
      (progn
        (add-hook 'after-init-hook
                  (lambda ()
                    (load-theme 'solarized-dark t)
                    ;; (load-theme 'solarized-light t t)
                    )))
    (add-hook 'after-init-hook
              (lambda ()
                (load-theme 'deeper-blue) t)))




  ;; Even though we may have set the Mac OS X Terminal's Alt key as the
  ;; emacs Meta key, we want to be able to insert a '#' using Alt-3 in
  ;; emacs as we would in other programs.
  (fset 'insert-pound "#")
  (define-key global-map "\M-3" 'insert-pound)

  ;; OS X ls doesn't support --dired
  (setq dired-use-ls-dired nil)

  ;; (setenv "GPG_AGENT_INFO" "~/.gnupg/S.gpg-agent")

  (defun offlineimap-get-password (host port)
    (let* ((netrc (netrc-parse (expand-file-name "~/git/.emacs.d/.autinfo.gpg")))
           (hostentry (netrc-machine netrc host port port)))
      (when hostentry (netrc-get hostentry "password"))))

  (setq
   epa-file-cache-passphrase-for-symmetric-encryption t
   user-full-name "Damon Haley"
   user-mail-address "damon.haley@colorado.edu"
   )


  (defalias 'list-matching-lines 'occur)
  (defalias 'delete-matching-lines 'flush-lines)
  (defalias 'delete-non-matching-lines 'keep-lines)

  (defalias 'td 'toggle-debug-on-error)

  ;; Allow "y or n" instead of "yes or no"
  ;; (fset 'yes-or-no-p 'y-or-n-p)

  (require 'switch-window)

  (require 'window-number)
  (window-number-mode)
  (window-number-meta-mode)

  (require 'buffer-move)

  (bind-key "<C-S-up>"     'buf-move-up)
  (bind-key "<C-S-down>"   'buf-move-down)
  (bind-key "<C-S-left>"   'buf-move-left)
  (bind-key "<C-S-right>"  'buf-move-right)

  ;; Various superfluous white-space. Just say no.
  (add-hook 'before-save-hook 'cleanup-buffer-safe)

  (defun open-in-desktop ()
    "Open the current file in desktop.
Works in Microsoft Windows, Mac OS X, Linux."
    (interactive)
    (cond
     ((string-equal system-type "windows-nt")
      (w32-shell-execute "explore" (replace-regexp-in-string "/" "\\" default-directory t t)))
     ((string-equal system-type "darwin") (shell-command "open ."))
     ((string-equal system-type "gnu/linux") (shell-command "xdg-open ."))))

  (bind-key "C-S-o" 'open-in-desktop)

;;;; Emoji composition tests
;;; Regional indicators (#x1F1E6 - #x1F1FF)

  (defun emoji-insert_regions ()
    "Insert Regional indicators (#x1F1E6 - #x1F1FF)"
    (interactive)
    (insert (mapconcat (lambda (s) (mapcar (lambda (c) (+ c (- #x1F1FF ?Z))) s))
                       '("CN" "DE" "ES" "FR" "GB" "IT" "JP" "KR" "RU" "US")
                       " ")))

;;;
  (defun emoji-insert_comparision ()
    "Variation Selectors 15 (text-style) and 16 (emoji-style)"
    (interactive)
    (insert (mapconcat
             (lambda (c) (format "#x%04x:\t%c\uFE0E\u20E3 %c\uFE0F\u20E3" c c c))
             (cdr (assq 'keycap mac-emoji-variation-characters-alist)) "\n")
            ?\n
            (mapconcat
             (lambda (c) (format "#x%04x:\t%c\uFE0E %c\uFE0F" c c c))
             (mapconcat 'cdr mac-emoji-variation-characters-alist "") "\n")))


  (define-key global-map (kbd "C-+") 'text-scale-increase)
  (define-key global-map (kbd "C--") 'text-scale-decrease)

  ;; ### Project utilities

  ;; (defun project-directory ()
  ;;   (let ((root (ffip-project-root)))
  ;;     (if root (directory-file-name root))))

  ;; (defun project-make ()
  ;;   (interactive)
  ;;   (let ((root (locate-dominating-file (buffer-file-name) "Makefile")))
  ;;     (if root
  ;;         (shell-command (format "cd %s && make" root))
  ;;       (message "No Makefile found"))))

  ;; ;; Allow hooks to be defined in .dir-locals.el

  ;; (defun project-apply-directory-hooks ()
  ;;   "Sets directory-local hooks using the value of `directory-hooks-alist', which
  ;; is a list of (hook-name . hook-function) pairs."
  ;;   (when (boundp 'directory-hooks-alist)
  ;;     (dolist (hook-definition directory-hooks-alist)
  ;;       (add-hook (car hook-definition)
  ;;                 (cdr hook-definition)
  ;;                 nil t))))

  ;; (add-hook 'hack-local-variables-hook #'project-apply-directory-hooks)

  ;; (add-to-list 'load-path "~/.emacs.d/site-lisp/frame-tag")
  ;; (require 'frame-tag)
  ;; (frame-tag-mode 1)

  ;; Local Variables:
  ;;   mode: emacs-lisp
  ;;   mode: allout
  ;;   outline-regexp: "^;;;_\\([,. ]+\\)"
  ;; End:

;;; init.el ends here
