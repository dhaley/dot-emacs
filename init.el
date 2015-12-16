(defconst emacs-start-time (current-time))
(unless noninteractive
  (message "Loading %s..." load-file-name))

(setq message-log-max 16384)

(defconst user-site-lisp-directory
  (expand-file-name "site-lisp/" user-emacs-directory))

(defun add-to-load-path (path &optional dir)
  (setq load-path
        (cons (expand-file-name path (or dir user-emacs-directory)) load-path)))


;; Add top-level lisp directories, in case they were not setup by the
;; environment.

(dolist (dir (nreverse
              (list 
                    
                    user-site-lisp-directory)))
  (dolist (entry (nreverse (directory-files-and-attributes dir)))
    (if (cadr entry)
        (add-to-load-path (car entry) dir))))

(eval-and-compile
  (mapc
   #'(lambda (path)
       (push (expand-file-name path user-emacs-directory) load-path))
   '("site-lisp" "override" "lisp" "lisp/use-package" ""))

  (defun agda-site-lisp ()
    (let ((agda
           (nth 1 (split-string
                   (shell-command-to-string "load-env-agda which agda")
                   "\n"))))))) 

(eval-and-compile
  (defvar use-package-verbose t)
  ;; (defvar use-package-expand-minimally t)
  (eval-after-load 'advice
    `(setq ad-redefinition-action 'accept))
  (require 'cl)
  (require 'use-package))

(require 'bind-key)
(require 'diminish nil t)

;;; Utility macros and functions

;; support textexpander (dkh (2015-03-25): check if it's running)
;; (bind-key "M-v" #'scroll-up)
(bind-key "H-v" #'yank)

(defsubst hook-into-modes (func &rest modes)
  (dolist (mode-hook modes) (add-hook mode-hook func)))

;;; Load customization settings

(defvar running-alternate-emacs nil)
(defvar user-data-directory (expand-file-name "data" user-emacs-directory))

(if (not (string-match (concat "Emacs\\([A-Za-z]+\\).app/Contents/MacOS/")
                       invocation-directory))
    (load (expand-file-name "settings" user-emacs-directory))
  (let ((settings (with-temp-buffer
                    (insert-file-contents
                     (expand-file-name "settings.el" user-emacs-directory))
                    (goto-char (point-min))
                    (read (current-buffer))))
        (suffix (downcase (match-string 1 invocation-directory))))
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
      (eval settings))))

;;; Enable disabled commands

(put 'downcase-region  'disabled nil)   ; Let downcasing work
(put 'erase-buffer     'disabled nil)
(put 'eval-expression  'disabled nil)   ; Let ESC-ESC work
(put 'narrow-to-page   'disabled nil)   ; Let narrowing work
(put 'narrow-to-region 'disabled nil)   ; Let narrowing work
(put 'set-goal-column  'disabled nil)
(put 'upcase-region    'disabled nil)   ; Let upcasing work

;;; Configure libraries

(eval-and-compile
  (push (expand-file-name "lib" user-emacs-directory) load-path))

(use-package anaphora       :defer t :load-path "lib/anaphora")
(use-package button-lock    :defer t :load-path "lib/button-lock")
(use-package dash           :defer t :load-path "lib/dash-el")
(use-package ctable         :defer t :load-path "lib/emacs-ctable")
(use-package deferred       :defer t :load-path "lib/emacs-deferred")
(use-package epc            :defer t :load-path "lib/emacs-epc")
(use-package web            :defer t :load-path "lib/emacs-web")
(use-package epl            :defer t :load-path "lib/epl")
(use-package f              :defer t :load-path "lib/f-el")
(use-package fame           :defer t)
(use-package fuzzy          :defer t)
(use-package gh             :defer t :load-path "lib/gh-el")
(use-package ht             :defer t :load-path "lib/ht-el")
(use-package let-alist      :defer t)
(use-package logito         :defer t :load-path "lib/logito")
(use-package makey          :defer t :load-path "lib/makey")
(use-package pcache         :defer t :load-path "lib/pcache")
(use-package pkg-info       :defer t :load-path "lib/pkg-info")
(use-package popup          :defer t :load-path "lib/popup-el")
(use-package popwin         :defer t :load-path "lib/popwin-el")
(use-package pos-tip        :defer t :load-path "lib/pos-tip")
(use-package s              :defer t :load-path "lib/s-el")
(use-package working        :defer t)
(use-package xml-rpc        :defer t)

;;; Keybindings

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dkh (2015-03-25): Move all of these into declarations                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
;;   H-<anything>
;;   M-H-<anything>
;;
;; Single-letter bindings still available:
;;   C- ,'";:?<>|!#$%^&*`~ <tab>
;;   M- ?#

;;; global-map

(autoload 'org-cycle "org" nil t)
(autoload 'hippie-expand "hippie-exp" nil t)
(autoload 'indent-according-to-mode "indent" nil t)

(defun smart-tab (&optional arg)
  (interactive "P")
  (cond
   ((looking-back "^[-+* \t]*")
    (if (eq major-mode 'org-mode)
        (org-cycle arg)
      (indent-according-to-mode)))
   (t
    ;; Hippie also expands yasnippets, due to `yas-hippie-try-expand' in
    ;; `hippie-expand-try-functions-list'.
    (hippie-expand arg))))

(define-key key-translation-map (kbd "H-TAB") (kbd "C-TAB"))

(bind-key "<H-down>" #'shrink-window)
(bind-key "<H-left>" #'shrink-window-horizontally)
(bind-key "<H-right>" #'enlarge-window-horizontally)
(bind-key "<H-up>" #'enlarge-window)
(bind-key "H-`" #'make-frame)
;;; C-

(defvar ctl-period-map)
(define-prefix-command 'ctl-period-map)
(bind-key "C-." #'ctl-period-map)

(bind-key* "<C-return>" #'other-window)

(defun collapse-or-expand ()
  (interactive)
  (if (> (length (window-list)) 1)
      (delete-other-windows)
    (bury-buffer)))

(bind-key "C-z" #'delete-other-windows)

;;; M-

(defadvice async-shell-command (before uniqify-running-shell-command activate)
  (let ((buf (get-buffer "*Async Shell Command*")))
    (if buf
        (let ((proc (get-buffer-process buf)))
          (if (and proc (eq 'run (process-status proc)))
              (with-current-buffer buf
                (rename-uniquely)))))))

(bind-key "M-!" #'async-shell-command)
(bind-key "M-'" #'insert-pair)
(bind-key "M-\"" #'insert-pair)
(bind-key "M-`" #'other-frame)

(bind-key "M-j" #'delete-indentation-forward)
(bind-key "M-J" #'delete-indentation)

(bind-key "M-W" #'mark-word)

(defun mark-line (&optional arg)
  (interactive "p")
  (beginning-of-line)
  (let ((here (point)))
    (dotimes (i arg)
      (end-of-line))
    (set-mark (point))
    (goto-char here)))

(bind-key "M-L" #'mark-line)

(defun mark-sentence (&optional arg)
  (interactive "P")
  (backward-sentence)
  (mark-end-of-sentence arg))

(bind-key "M-S" #'mark-sentence)
(bind-key "M-X" #'mark-sexp)
(bind-key "M-D" #'mark-defun)

(bind-key "M-g c" #'goto-char)
(bind-key "M-g l" #'goto-line)

(defun delete-indentation-forward ()
  (interactive)
  (delete-indentation t))

;;; M-C-

(bind-key "<C-M-backspace>" #'backward-kill-sexp)

;;; ctl-x-map

;;; C-x

(bind-key "C-x d" #'delete-whitespace-rectangle)
(bind-key "C-x F" #'set-fill-column)
(bind-key "C-x t" #'toggle-truncate-lines)

(defun delete-current-buffer-file ()
  "Delete the current buffer and the file connected with it"
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (kill-buffer buffer)
      (when (yes-or-no-p "Are you sure, want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

(bind-key "C-x K" #'delete-current-buffer-file)

;;; C-x C-

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

(bind-key "C-x C-d" #'duplicate-line)
(bind-key "C-x C-e" #'pp-eval-last-sexp)
(bind-key "C-x C-n" #'next-line)

(defun find-alternate-file-with-sudo ()
  (interactive)
  (find-alternate-file (concat "/sudo::" (buffer-file-name))))

(bind-key "C-x C-v" #'find-alternate-file-with-sudo)

;;; C-x M-

(bind-key "C-x M-n" #'set-goal-column)

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

(bind-key "C-x M-q" #'refill-paragraph)

;;; mode-specific-map

;;; C-c

(bind-key "C-c <tab>" #'ff-find-other-file)
(bind-key "C-c SPC" #'just-one-space)

(defmacro when-feature-loaded (feature &rest body)
  "When FEATURE is loaded, evaluate and execute BODY."
  `(when (featurep ,feature) ,@body))

(defmacro recursive-edit-preserving-window-config (body)
  "*Return a command that enters a recursive edit after executing BODY.
Upon exiting the recursive edit (with\\[exit-recursive-edit] (exit)
or \\[abort-recursive-edit] (abort)), restore window configuration
in current frame.
Inspired by Erik Naggum's `recursive-edit-with-single-window'."
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

(bind-key "C-c d" #'delete-current-line)

(defun do-eval-buffer ()
  (interactive)
  (call-interactively 'eval-buffer)
  (message "Buffer has been evaluated"))

(defun do-eval-region ()
  (interactive)
  (call-interactively 'eval-region)
  (message "Region has been evaluated"))

(bind-keys :prefix-map my-lisp-devel-map
           :prefix "C-c e"
           ("E" . elint-current-buffer)
           ("b" . do-eval-buffer)
           ("c" . cancel-debug-on-entry)
           ("d" . debug-on-entry)
           ("e" . toggle-debug-on-error)
           ("f" . emacs-lisp-byte-compile-and-load)
           ("j" . emacs-lisp-mode)
           ("l" . find-library)
           ("r" . do-eval-region)
           ("s" . scratch)
           ("z" . byte-recompile-directory))

(bind-key "C-c f" #'flush-lines)
(bind-key "C-c k" #'keep-lines)

(eval-when-compile
  (defvar emacs-min-height)
  (defvar emacs-min-width))

(defvar display-name
  (let ((width (display-pixel-width)))
    (cond ((>= width 2560) 'retina-imac)
          ((= width 1440) 'retina-macbook-pro))))

(defvar emacs-min-top 23)
(defvar emacs-min-left
  (cond ((eq display-name 'retina-imac) 975)
        (t 521)))
(defvar emacs-min-height
  (cond ((eq display-name 'retina-imac) 57)
        (t 44)))
(defvar emacs-min-width 100)

(if running-alternate-emacs
    (setq emacs-min-top 22
          emacs-min-left 5
          emacs-min-height 57
          emacs-min-width 90))

(defvar emacs-min-font
  (cond
   ((eq display-name 'retina-imac)
    (if running-alternate-emacs
        "-*-Myriad Pro-normal-normal-normal-*-20-*-*-*-p-0-iso10646-1"
      ;; "-*-Source Code Pro-normal-normal-normal-*-20-*-*-*-m-0-iso10646-1"
      "-*-Hack-normal-normal-normal-*-18-*-*-*-m-0-iso10646-1"
      ))
   ((string= system-name "ubuntu")
    ;; "-*-Source Code Pro-normal-normal-normal-*-20-*-*-*-m-0-iso10646-1"
    "-*-Hack-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1"
    )
   (t
    (if running-alternate-emacs
        "-*-Myriad Pro-normal-normal-normal-*-17-*-*-*-p-0-iso10646-1"
      ;; "-*-Source Code Pro-normal-normal-normal-*-15-*-*-*-m-0-iso10646-1"
      "-*-Hack-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1"
      ))))

(let ((frame-alist
       (list (cons 'top    emacs-min-top)
             (cons 'left   emacs-min-left)
             (cons 'height emacs-min-height)
             (cons 'width  emacs-min-width)
             (cons 'font   emacs-min-font))))
  (setq initial-frame-alist frame-alist))

(defun emacs-min ()
  (interactive)

  (set-frame-parameter (selected-frame) 'fullscreen nil)
  (set-frame-parameter (selected-frame) 'vertical-scroll-bars nil)
  (set-frame-parameter (selected-frame) 'horizontal-scroll-bars nil)

  (set-frame-parameter (selected-frame) 'top emacs-min-top)
  (set-frame-parameter (selected-frame) 'left emacs-min-left)
  (set-frame-parameter (selected-frame) 'height emacs-min-height)
  (set-frame-parameter (selected-frame) 'width emacs-min-width)

  (set-frame-font emacs-min-font))

(if window-system
    (add-hook 'after-init-hook 'emacs-min))

(defun emacs-max ()
  (interactive)
  (set-frame-parameter (selected-frame) 'fullscreen 'fullboth)
  (set-frame-parameter (selected-frame) 'vertical-scroll-bars nil)
  (set-frame-parameter (selected-frame) 'horizontal-scroll-bars nil))

(defun emacs-toggle-size ()
  (interactive)
  (if (> (cdr (assq 'width (frame-parameters))) 100)
      (emacs-min)
    (emacs-max)))

(bind-key "C-c m" #'emacs-toggle-size)

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
                    python-mode perl-mode java-mode groovy-mode
                    haskell-mode literate-haskell-mode)))
      (set symbol value))
  :type 'string
  :group 'mail)

(defun insert-user-timestamp ()
  "Insert a quick timestamp using the value of `user-initials'."
  (interactive)
  (insert (format "%s (%s): " user-initials
                  (format-time-string "%Y-%m-%d" (current-time)))))

(bind-key "C-c n" #'insert-user-timestamp)
(bind-key "C-c o" #'customize-option)
(bind-key "C-c O" #'customize-group)
(bind-key "C-c F" #'customize-face)

(bind-key "C-c q" #'fill-region)
(bind-key "C-c r" #'replace-regexp)
(bind-key "C-c s" #'replace-string)
(bind-key "C-c u" #'rename-uniquely)

(bind-key "C-c v" #'ffap)

(defun view-clipboard ()
  (interactive)
  (delete-other-windows)
  (switch-to-buffer "*Clipboard*")
  (let ((inhibit-read-only t))
    (erase-buffer)
    (clipboard-yank)
    (goto-char (point-min))))

(bind-key "C-c V" #'view-clipboard)
(bind-key "C-c z" #'clean-buffer-list)

(bind-key "C-c =" #'count-matches)
(bind-key "C-c ;" #'comment-or-uncomment-region)

;;; C-c C-

(defun delete-to-end-of-buffer ()
  (interactive)
  (kill-region (point) (point-max)))

(bind-key "C-c C-z" #'delete-to-end-of-buffer)

(defun copy-current-buffer-name ()
  (interactive)
  (let ((name (buffer-file-name)))
    (kill-new name)
    (message name)))

(bind-key "C-c C-0" #'copy-current-buffer-name)

;;; C-c M-

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

(bind-key "C-c M-q" #'unfill-paragraph)

(defun unfill-region (beg end)
  (interactive "r")
  (setq end (copy-marker end))
  (save-excursion
    (goto-char beg)
    (while (< (point) end)
      (unfill-paragraph 1)
      (forward-paragraph))))

;;; ctl-period-map

;;; C-.

(bind-key "C-. m" #'kmacro-keymap)

(bind-key "C-. C-i" #'indent-rigidly)

;;; help-map

(defvar lisp-find-map)
(define-prefix-command 'lisp-find-map)

(bind-key "C-h e" #'lisp-find-map)

;;; C-h e

(bind-key "C-h e c" #'finder-commentary)
(bind-key "C-h e e" #'view-echo-area-messages)
(bind-key "C-h e f" #'find-function)
(bind-key "C-h e F" #'find-face-definition)

(defun my-switch-in-other-buffer (buf)
  (when buf
    (split-window-vertically)
    (switch-to-buffer-other-window buf)))

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

    (switch-to-buffer find-buf)
    (my-switch-in-other-buffer desc-buf)
    (my-switch-in-other-buffer info-buf)
    ;;(switch-in-other-buffer cust-buf)
    (balance-windows)))

(bind-key "C-h e d" #'my-describe-symbol)
(bind-key "C-h e i" #'info-apropos)
(bind-key "C-h e k" #'find-function-on-key)
(bind-key "C-h e l" #'find-library)

(defvar lisp-modes '(emacs-lisp-mode
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

(bind-key "C-h e s" #'scratch)
(bind-key "C-h e v" #'find-variable)
(bind-key "C-h e V" #'apropos-value)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dkh (2015-03-25): Move all of the above into use-package declarations    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Delayed configuration

(use-package flycheck
  :load-path "site-lisp/flycheck"
  :defer 5
  :config

  (add-to-list 'display-buffer-alist
               `(,(rx bos "*Flycheck errors*" eos)
                 (display-buffer-reuse-window
                  display-buffer-in-side-window)
                 (reusable-frames . visible)
                 (side            . bottom)
                 (window-height   . 0.4)))

  (defun lunaryorn-quit-bottom-side-windows ()
    "Quit side windows of the current frame."
    (interactive)
    (dolist (window (window-at-side-list))
      (quit-window nil window)))

  (bind-key "C-H-M-:" 'lunaryorn-quit-bottom-side-windows)

  (use-package helm-flycheck
    :load-path "site-lisp/helm-flycheck"
    :init
    (define-key flycheck-mode-map (kbd "C-c ! h") 'helm-flycheck))

  (use-package drupal-mode
    :load-path "site-lisp/drupal-mode"
    :commands drupal-mode-bootstrap
    :config
    (add-hook 'drupal-mode-hook
              '(lambda ()
                 (add-to-list 'Info-directory-list '"~/.emacs.d/site-lisp/drupal-mode")
                 (yas-activate-extra-mode 'drupal-mode)
                 (when (apply 'derived-mode-p drupal-php-modes)
                   (flycheck-mode t))))
    (use-package drupal-spell
      :load-path "site-lisp/drupal-spell")))

(use-package dot-org
  :load-path ("override/org-mode/contrib/lisp"
              "override/org-mode/lisp"
              "override/org-mode/contrib/lisp")
  :commands my-org-startup
  :bind (("M-C"   . jump-to-org-agenda)
         ("M-m"   . org-smart-capture)
         ("M-M"   . org-inline-note)
         ("C-c a" . org-agenda)
         ("C-c S" . org-store-link)
         ("C-c l" . org-insert-link)
         ("C-. n" . org-velocity-read))
  :defer 30
  :config
  (when (not running-alternate-emacs)
    (run-with-idle-timer 300 t 'jump-to-org-agenda)
    (my-org-startup)))

(use-package dot-gnus
  :load-path "override/gnus"
  :bind (("M-G"   . switch-to-gnus)
         ("C-x m" . compose-mail))
  :init
  (progn
    (setq gnus-init-file (expand-file-name "dot-gnus" user-emacs-directory)
          gnus-home-directory "~/Messages/Gnus/")))

(defun define-keys (mode-map keybindings)
  "Takes a mode map, and a list of (key function-designator)
lists.  The functions are bound to the keys in the given mode-map.
Keys are in kbd format."
  (mapc (lambda (keybinding)
          (destructuring-bind (key function) keybinding
            (define-key mode-map (read-kbd-macro key) function)))
        keybindings))

;;; Packages

(use-package ggtags
  :load-path "site-lisp/ggtags"
  :commands ggtags-mode
  :diminish ggtags-mode)

(use-package abbrev
  :disabled t
  :commands abbrev-mode
  :diminish abbrev-mode
  :init
  (hook-into-modes #'abbrev-mode 'text-mode-hook)

  :config
  (if (file-exists-p abbrev-file-name)
      (quietly-read-abbrev-file))

  (add-hook 'expand-load-hook
            (lambda ()
              (add-hook 'expand-expand-hook 'indent-according-to-mode)
              (add-hook 'expand-jump-hook 'indent-according-to-mode))))

(use-package ace-jump-mode
  :load-path "site-lisp/ace-jump-mode"
  :bind ("M-h" . ace-jump-mode)
  :config
  (setq ace-jump-mode-submode-list
        '(ace-jump-char-mode
          ace-jump-word-mode
          ace-jump-line-mode)))

(use-package ace-link
  :defer 1
  :config
  (ace-link-setup-default))

(use-package ace-isearch
  :disabled t
  :load-path "site-lisp/ace-isearch"
  :config
  (global-ace-isearch-mode 1))

(use-package ace-window
  :load-path "site-lisp/ace-window"
  :init

  (defun dkh-scroll-other-window()
    (interactive)
    (scroll-other-window 1))

  (defun dkh-scroll-other-window-down ()
    (interactive)
    (scroll-other-window-down 1))
  :config
  (setq aw-keys (quote (97 111 101 117 105 100 104 116 110))
        aw-dispatch-always t
        aw-dispatch-alist
        '((?x aw-delete-window     "Ace - Delete Window")
          (?c aw-swap-window       "Ace - Swap Window")
          (?n aw-flip-window)
          (?v aw-split-window-vert "Ace - Split Vert Window")
          (?h aw-split-window-horz "Ace - Split Horz Window")
          (?m delete-other-windows "Ace - Maximize Window")
          (?g delete-other-windows)
          (?b balance-windows)
          (?u winner-undo)
          (?r winner-redo)
          (?j dired-jump)
          (32 mode-line-other-buffer)))
  (when (when-feature-loaded 'hydra)
    (defhydra hydra-window-size (:color red)
      "Windows size"
      ("a" shrink-window-horizontally "shrink horizontal")
      ("." shrink-window "shrink vertical")
      ("j" enlarge-window "enlarge vertical")
      ("i" enlarge-window-horizontally "enlarge horizontal"))
    (defhydra hydra-window-frame (:color red)
      "Frame"
      ("f" make-frame "new frame")
      ("x" delete-frame "delete frame"))
    (defhydra hydra-window-scroll (:color red)
      "Scroll other window"
      ("<SPC>" dkh-scroll-other-window "scroll")
      ("<backspace>" dkh-scroll-other-window-down "scroll down"))
    (add-to-list 'aw-dispatch-alist '(?w hydra-window-size/body) t)
    (add-to-list 'aw-dispatch-alist '(32 hydra-window-scroll/body) t)
    (add-to-list 'aw-dispatch-alist '(?\; hydra-window-frame/body) t))
  
  (set-face-attribute 'aw-leading-char-face nil :foreground "deep sky blue" :weight 'bold :height 3.0)
  (set-face-attribute 'aw-mode-line-face nil :inherit 'mode-line-buffer-id :foreground "lawn green"))

(use-package avy
  :load-path "site-lisp/avy"
  :bind* (("H-l" . avy-goto-line))
  :init
  (defun toggle-window-split ()
    (interactive)
    (if (= (count-windows) 2)
        (let* ((this-win-buffer (window-buffer))
               (next-win-buffer (window-buffer (next-window)))
               (this-win-edges (window-edges (selected-window)))
               (next-win-edges (window-edges (next-window)))
               (this-win-2nd (not (and (<= (car this-win-edges)
                                           (car next-win-edges))
                                       (<= (cadr this-win-edges)
                                           (cadr next-win-edges)))))
               (splitter
                (if (= (car this-win-edges)
                       (car (window-edges (next-window))))
                    'split-window-horizontally
                  'split-window-vertically)))
          (delete-other-windows)
          (let ((first-win (selected-window)))
            (funcall splitter)
            (if this-win-2nd (other-window 1))
            (set-window-buffer (selected-window) this-win-buffer)
            (set-window-buffer (next-window) next-win-buffer)
            (select-window first-win)
            (if this-win-2nd (other-window 1))))))
  (define-key ctl-x-4-map "t" 'toggle-window-split)
  :config
  (setq avy-keys       '(?a ?s ?d ?e ?f ?g ?r ?v ?h ?j ?k ?l ?n ?m ?u)
        avy-background t
        avy-all-windows t
        avy-style 'at-full
        avy-case-fold-search nil))

(use-package hydra
  :load-path "site-lisp/hydra"
  :defer 0.1
  :preface
  (require 'hydra-examples)
  :init
  (bind-key* "\\" 'hydra-master/body)
  (bind-key* "C-x o" 'hydra-window/body)

  (bind-key "C-z" 'delete-other-windows)

  (global-set-key (kbd "M-o f") 'flash-active-buffer)
  
  (make-face 'flash-active-buffer-face)
  (set-face-attribute 'flash-active-buffer-face nil
                      :background "red"
                      :foreground "black")
  (defun flash-active-buffer ()
    (interactive)
    (run-at-time "100 millisec" nil
                 (lambda (remap-cookie)
                   (face-remap-remove-relative remap-cookie))
                 (face-remap-add-relative 'default 'flash-active-buffer-face)))

  (defun dkh-toggle-show-trailing-whitespace ()
    "Toggle show-trailing-whitespace between t and nil"
    (interactive)
    (setq show-trailing-whitespace (not show-trailing-whitespace)))

  ;; Make window splitting more useful
  ;; Copied from http://www.reddit.com/r/emacs/comments/25v0eo/you_emacs_tips_and_tricks/chldury

  (defun my/vsplit-last-buffer (prefix)
    "Split the window vertically and display the previous buffer."
    (interactive "p")
    (split-window-vertically)
    (other-window 1 nil)
    (if (= prefix 1)
        (switch-to-next-buffer)))

  (defun my/hsplit-last-buffer (prefix)
    "Split the window horizontally and display the previous buffer."
    (interactive "p")
    (split-window-horizontally)
    (other-window 1 nil)
    (if (= prefix 1) (switch-to-next-buffer)))

  (bind-key "C-x 2" 'my/vsplit-last-buffer)
  (bind-key "C-x 3" 'my/hsplit-last-buffer)

  :config
  (hydra-add-font-lock)

  (eval-and-compile
    (defhydra hydra-common (:color blue)
      ("<ESC>" nil "quit")))

  (global-set-key
   (kbd "C-M-j")
   (defhydra hydra-bmk ()
     ("i" (find-file "~/.emacs.d/init.el") "dot-emacs")
     ("b" (find-file "~/.bash_profile") "bash-profile")
     ("B" (find-file "~/.bashrc") "bashrc")
     ("d" (find-file "~/Documents/cde.drush/nrel.aliases.drushrc.php") "drush-aliases")
     ("e" (find-file "~/.emacs.d") "user-emacs-dir")
     ("t" (find-file "~/Documents/Tasks/todo.txt") "todo.txt")
     ("s" (find-file "~/.emacs.d/settings.el") "settings.el")
     ("o" (find-file "~/.emacs.d/dot-org.el") "dot-org")
     ("g" (find-file "~/.emacs.d/dot-gnus.el") "dot-gnus")
     ("O" (find-file "~/.emacs.d/org-settings.el") "org-settings")
     ("r" (find-file "~/src/drupal_scripts/release.sh") "release.sh")
     ("T" (find-file "~/Documents/Tasks") "Tasks dir")
     ("G" (find-file "~/.emacs.d/gnus-settings.el") "gnus-settings")
     ("u" (find-file "~/.emacs.d/site-lisp/xmsi-math-symbols-input.el") "math-symbols")))

  (global-set-key
   (kbd "C-H-M-S-SPC")
   (defhydra hydra-zoom ()
     "zoom"
     ("h" text-scale-increase "in")
     ("t" text-scale-decrease "out")
     ("0" (text-scale-set 0) "reset")
     ("1" (text-scale-set 0) :bind nil)
     ("2" (text-scale-set 0) :bind nil :color blue)))

  (defhydra hydra-error (global-map "C-H-M-S tab")
    "goto-error"
    ("h" first-error "first")
    ("j" next-error "next")
    ("k" previous-error "prev")
    ("v" recenter-top-bottom "recenter")
    ("q" nil "quit"))

  (defhydra hydra-master (:color blue :idle 0.4)
    "
                                                                       ╭───────┐
                                                                       │ Index │
╭──────────────────────────────────────────────────────────────────────┴───────╯
  [_a_] bookmarks    [^h^]               [_o_] organization  [_v_] games
  [_b_] buffers      [_i_] internet      [_p_] project       [_w_] window
  [_c_] flycheck     [_j_] jump          [_q_] exit          [_x_] shell
  [_d_] development  [_k_] spell         [_r_] register      [^y^]
  [_e_] emacs        [_l_] lisp          [_s_] search        [^z^]
  [_f_] file         [_m_] media         [_t_] text
  [_g_] git          [_n_] narrow        [^u^]
--------------------------------------------------------------------------------
    "
    ("<SPC>" dkh-alternate-buffers "alternate buffers")
    ("<ESC>" nil "quit")
    ("\\" (insert "\\") "\\")
    ("a"     hydra-bookmarks/body nil)
    ("b"     hydra-buffers/body nil)
    ("c"     hydra-flycheck/body nil)
    ("d"     hydra-development/body nil)
    ("e"     hydra-emacs/body nil)
    ("f"     hydra-file/body nil)
    ("g"     hydra-git/body nil)
    ("i"     hydra-internet/body nil)
    ("j"     hydra-jump/body nil)
    ("k"     hydra-spell/body nil)
    ("l"     hydra-lisp/body nil)
    ("m"     hydra-media/body nil)
    ("n"     hydra-narrow/body nil)
    ("o"     hydra-organization/body nil)
    ("p"     hydra-project/body nil)
    ("q"     hydra-exit/body nil)
    ("r"     hydra-register/body nil)
    ("s"     hydra-search/body nil)
    ("t"     hydra-text/body nil)
    ("v"     hydra-games/body nil)
    ("w"     ace-window nil)
    ("x"     hydra-system/body nil))
  
  (defhydra hydra-bookmarks (:color blue :hint nil :idle 0.4 :inherit (hydra-common/heads))
    "
                                                                   ╭───────────┐
       List                          Do                            │ Bookmarks │
╭──────────────────────────────────────────────────────────────────┴───────────╯
  [_h_] list bookmarks (helm)     [_j_] jump to a bookmark
  [_l_] list bookmarks            [_m_] set bookmark at point
  ^ ^                             [_s_] save bookmarks
--------------------------------------------------------------------------------
    "
    ("h" helm-bookmarks)
    ("j" bookmark-jump)
    ("l" list-bookmarks)
    ("m" bookmark-set)
    ("s" bookmark-save))

  (defhydra hydra-buffers (:color blue :hint nil :idle 0.4 :inherit (hydra-common/heads))
    "
                                                                     ╭─────────┐
  Switch                 Do                                          │ Buffers │
╭────────────────────────────────────────────────────────────────────┴─────────╯
  [_b_] switch (ido)       [_d_] kill the buffer
  [_i_] ibuffer            [_r_] toggle read-only mode
  [_a_] alternate          [_u_] revert buffer changes
  [_s_] switch (helm)      [_w_] save buffer
--------------------------------------------------------------------------------
    "
    ("a" dkh-alternate-buffers)
    ("b" ivy-switch-buffer)
    ("d" dkh-kill-this-buffer)
    ("i" ibuffer)
    ("m" ace-swap-window)
    ("r" read-only-mode)
    ("s" helm-buffers-list)
    ("u" dkh-revert-buffer)
    ("w" save-buffer))

  (defhydra hydra-flycheck (:color blue :hint nil :idle 0.4 :inherit (hydra-common/heads))
    "
                                                                    ╭──────────┐
   Navigate          Show Errors                  Do                │ Flycheck │
╭───────────────────────────────────────────────────────────────────┴──────────╯
   ^_p_^revious     [_l_] list errors           [_t_] toggle Flycheck
      ^^↑^^         [_e_] list errors (helm)    [_c_] select checker
    ^_f_^irst       [_d_] clear all errors      [_r_] run via compile
      ^^↓^^          ^ ^                        [_h_] describe checker
    ^_n_^ext
--------------------------------------------------------------------------------
      "
    ("c" flycheck-select-checker)
    ("h" flycheck-describe-checker)
    ("d" flycheck-clear)
    ("e" helm-flycheck)
    ("f" flycheck-first-error)
    ("l" flycheck-list-errors)
    ("n" flycheck-next-error :color red)
    ("p" flycheck-previous-error :color red)
    ("r" flycheck-compile)
    ("t" flycheck-mode))

  (defhydra hydra-development (:color blue :hint nil :idle 0.4 :inherit (hydra-common/heads))
    "
                                                                 ╭─────────────┐
     Zeal                   Web                 Quickrun         │ Development │
╭────────────────────────────────────────────────────────────────┴─────────────╯
  [_z_] search docs   [_c_] Web Colors          [_q_] buffer
  [_d_] set docset    [_h_] HTTP header         [_v_] region
   ^ ^                [_m_] HTTP method         [_x_] shell
   ^ ^                [_r_] HTTP relation       [_p_] with arg
   ^ ^                [_s_] HTTP status code    [_k_] buffer (helm)
   ^ ^                [_g_] RESTclient          [_o_] only compile
   ^ ^                [_f_] RFC doc             [_R_] replace
  [_l_] lines of code [_F_] RFC index           [_e_] eval/print
--------------------------------------------------------------------------------
      "
    ("z" zeal-at-point)
    ("d" zeal-at-pont-set-docset)
    ("c" helm-colors)
    ("g" restclient-mode)
    ("f" irfc-visit)
    ("F" irfc-index)
    ("q" quickrun)
    ("v" quickrun-region)
    ("x" quickrun-shell)
    ("p" quickrun-with-arg)
    ("o" quickrun-compile-only)
    ("R" quickrun-replace-region)
    ("e" quickrun-eval-print)
    ("k" helm-quickrun)
    ("h" http-header)
    ("m" http-method)
    ("r" http-relation)
    ("s" http-status-code)
    ("l" cloc))

  (defhydra hydra-emacs (:color blue :hint nil :idle 0.4 :inherit (hydra-common/heads))
    "
                                                                       ╭───────┐
   Execute       Packages         Help                     Misc        │ Emacs │
╭──────────────────────────────────────────────────────────────────────┴───────╯
  [_s_] smex       [_p_] list      [_a_] apropos (helm)    [_t_] change theme (helm)
  [_m_] smex mode  [_i_] install   [_f_] info manual       [_l_] list emacs process
  [_h_] helm M-x   [_u_] upgrade   [_k_] bindings (helm)   [_c_] init time
   ^ ^              ^ ^            [_b_] personal bindings [_o_] unbound commands
--------------------------------------------------------------------------------
      "
    ("C-h b" helm-descbinds "bindings")
    ("a" helm-apropos)
    ("b" describe-personal-keybindings)
    ("c" emacs-init-time)
    ("i" package-install)
    ("k" helm-descbinds)
    ("l" list-processes)
    ("f" info-display-manual)
    ("p" paradox-list-packages)
    ("t" helm-themes)
    ("u" paradox-upgrade-packages)
    ("m" smex-major-mode-commands)
    ("s" smex)
    ("h" helm-M-x)
    ("o" smex-show-unbound-commands))

  (defhydra hydra-file (:color blue :hint nil :idle 0.4 :inherit (hydra-common/heads))
    "
                                                                        ╭──────┐
     Ido               Helm                 Dired        Ztree          │ File │
╭───────────────────────────────────────────────────────────────────────┴──────╯
  [_o_] open file   [_f_] find file      [_d_] dired    [_z_] diff dirs
   ^ ^              [_m_] mini
--------------------------------------------------------------------------------
      "
    ("o" find-file)
    ("f" helm-find-files)
    ("m" helm-mini)
    ("z" ztree-diff)
    ("d" dired))

  (defhydra hydra-text (:color blue :hint nil :idle 0.4 :inherit (hydra-common/heads))
    "
                                                                        ╭──────┐
 Size  Toggle              Unicode                        Do            │ Text │
╭───────────────────────────────────────────────────────────────────────┴──────╯
  _k_  [_f_] fill column     [_d_] unicode character           [_a_] align with regex
  ^↑^  [_h_] hidden chars    [_e_] evil digraphs table         [_w_] remove trailing ' '
  ^ ^  [_l_] line numbers    [_s_] specific code block         [_n_] count words
  ^↓^  [_t_] trailing ' '    [_u_] unicode character (helm)    [_i_] lorem ipsum
  _j_  [_v_] font space      [_p_] character code              [_x_] comment box
  ^ ^  [_c_] comment          ^ ^                              [_q_] boxquote
  ^ ^  [_b_] multibyte chars  ^ ^                              [_m_] iedit (multiple)
  ^ ^   ^ ^                   ^ ^                              [_r_] expand region
--------------------------------------------------------------------------------
      "
    ("a" align-regexp)
    ("b" toggle-enable-multibyte-characters)
    ("c" evilnc-comment-or-uncomment-lines)
    ("d" insert-char)
    ("e" evil-ex-show-digraphs)
    ("f" fci-mode)
    ("h" whitespace-mode)
    ("i" lorem-ipsum-insert-paragraphs)
    ("k" text-scale-increase :color red)
    ("j" text-scale-decrease :color red)
    ("l" linum-mode)
    ("n" count-words)
    ("m" iedit)
    ("p" describe-char)
    ("r" er/expand-region)
    ("s" charmap)
    ("t" dkh-toggle-show-trailing-whitespace)
    ("u" helm-ucs)
    ("v" variable-pitch-mode)
    ("w" whitespace-cleanup)
    ("q" hydra-boxquote/body)
    ("x" comment-box))

  (defhydra hydra-git (:color blue :hint nil :idle 0.4 :inherit (hydra-common/heads))
    "
                                                                         ╭─────┐
   Magit                          VC                    Timemachine      │ Git │
╭────────────────────────────────────────────────────────────────────────┴─────╯
  [_s_] status              [_d_] diffs between revisions  [_t_] timemachine
  [_B_] blame mode          [_b_] edition history
  [_l_] file log
--------------------------------------------------------------------------------
      "
    ("B" magit-blame-mode)
    ("b" vc-annotate)
    ("d" vc-diff)
    ("l" magit-file-log)
    ("s" magit-status)
    ("t" git-timemachine))

  (defhydra hydra-internet (:color blue :hint nil :idle 0.4 :inherit (hydra-common/heads))
    "
                                                                    ╭──────────┐
    Browse       Search             Social               Post       │ Internet │
╭───────────────────────────────────────────────────────────────────┴──────────╯
  [_w_] eww      [_g_] google          [_f_] elfeed            [_i_] imgur
  [_u_] url      [_m_] google maps     [_t_] twitter
   ^ ^           [_s_] surfraw         [_x_] stack overflow
--------------------------------------------------------------------------------
      "
    ("f" elfeed)
    ("g" google-this)
    ("i" imgur-post)
    ("m" google-maps)
    ("s" helm-surfraw)
    ("t" twit)
    ("w" eww)
    ("u" browse-url-at-point)
    ("x" sx-tab-newest))

  (defhydra hydra-jump (:color blue :hint nil :idle 0.4 :inherit (hydra-common/heads))
    "
                                                                        ╭──────┐
  Window          Word/Char        Line         iSearch                 │ Jump │
╭───────────────────────────────────────────────────────────────────────┴──────╯
  [_w_] jump        [_j_] word         [_l_] jump     [_i_] jump
  [_d_] close       [_p_] all words    [_y_] copy
  [_z_] maximize    [_b_] subword      [_m_] move
  [_s_] swap        [_c_] char         [_v_] copy region
   ^ ^              [_a_] two chars
--------------------------------------------------------------------------------
      "
    ("w" ace-window)
    ("d" ace-delete-window)
    ("z" ace-maximize-window)
    ("s" ace-swap-window)
    ("j" avy-goto-word-1)
    ("p" avy-goto-word-0)
    ("b" avy-goto-subword-0)
    ("c" avy-goto-char)
    ("a" avy-goto-char-2)
    ("l" avy-goto-line)
    ("y" avy-copy-line)
    ("m" avy-move-line)
    ("v" avy-copy-region)
    ("i" avy-isearch))

  (defhydra hydra-spell (:color blue :hint nil :idle 0.4 :inherit (hydra-common/heads))
    "
                                                                       ╭───────┐
    Flyspell               Ispell                      Gtranslate      │ Spell │
╭──────────────────────────────────────────────────────────────────────┴───────╯
  [_k_] correct word       [_w_] check word            [_g_] en ⇆ es
  [_n_] next error         [_t_] toggle dictionary     [_G_] any lang
  [_f_] toggle flyspell    [_d_] change dictionary
  [_p_] toggle prog mode
--------------------------------------------------------------------------------
      "
    ("w" ispell-word)
    ("d" ispell-change-dictionary)
    ("t" dkh-switch-dictionary)
    ("g" google-translate-smooth-translate)
    ("G" google-translate-query-translate)
    ("f" flyspell-mode)
    ("p" flyspell-prog-mode)
    ("k" flyspell-auto-correct-word)
    ("n" flyspell-goto-next-error))

  (defhydra hydra-lisp (:color blue :hint nil :idle 0.4 :inherit (hydra-common/heads))
    "
                                                                        ╭──────┐
    Elisp              Bug hunter                                       │ Lisp │
╭───────────────────────────────────────────────────────────────────────┴──────╯
  [_r_] eval region    [_f_] file
  [_s_] eval sexp      [_i_] init-file
--------------------------------------------------------------------------------
      "
    ("f" bug-hunter-file)
    ("i" bug-hunter-init-file)
    ("r" eval-region)
    ("s" eval-last-sexp))

  (defhydra hydra-narrow (:color blue :hint nil :idle 0.4 :inherit (hydra-common/heads))
    "
                                                                      ╭────────┐
    Narrow                                                            │ Narrow │
╭─────────────────────────────────────────────────────────────────────┴────────╯
  [_f_] narrow to defun
  [_p_] narrow to page
  [_r_] narrow to region
  [_w_] widen
--------------------------------------------------------------------------------
      "
    ("f" narrow-to-defun)
    ("p" narrow-to-page)
    ("r" narrow-to-region)
    ("w" widen))

  (defhydra hydra-project (:color blue :hint nil :idle 0.4 :inherit (hydra-common/heads))
    "
                                                                  ╭────────────┐
  Files             Search          Buffer             Do         │ Projectile │
╭─────────────────────────────────────────────────────────────────┴────────────╯
  [_f_] file          [_a_] ag          [_b_] switch         [_g_] magit
  [_l_] file dwim     [_A_] grep        [_v_] show all       [_p_] commander
  [_r_] recent file   [_s_] occur       [_V_] ibuffer        [_i_] info
  [_d_] dir           [_S_] replace     [_K_] kill all
  [_o_] other         [_t_] find tag
  [_u_] test file     [_T_] make tags
  [_h_] root
                                                                      ╭────────┐
  Other Window      Run             Cache              Do             │ Fixmee │
╭──────────────────────────────────────────────────╯ ╭────────────────┴────────╯
  [_F_] file          [_U_] test        [_kc_] clear         [_x_] TODO & FIXME
  [_L_] dwim          [_m_] compile     [_kk_] add current   [_X_] toggle
  [_D_] dir           [_c_] shell       [_ks_] cleanup
  [_O_] other         [_C_] command     [_kd_] remove
  [_B_] buffer
--------------------------------------------------------------------------------
      "
    ("a"   projectile-ag)
    ("A"   projectile-grep)
    ("b"   projectile-switch-to-buffer)
    ("B"   projectile-switch-to-buffer-other-window)
    ("c"   projectile-run-async-shell-command-in-root)
    ("C"   projectile-run-command-in-root)
    ("d"   projectile-find-dir)
    ("D"   projectile-find-dir-other-window)
    ("f"   projectile-find-file)
    ("F"   projectile-find-file-other-window)
    ("g"   projectile-vc)
    ("h"   projectile-dired)
    ("i"   projectile-project-info)
    ("kc"  projectile-invalidate-cache)
    ("kd"  projectile-remove-known-project)
    ("kk"  projectile-cache-current-file)
    ("K"   projectile-kill-buffers)
    ("ks"  projectile-cleanup-known-projects)
    ("l"   projectile-find-file-dwim)
    ("L"   projectile-find-file-dwim-other-window)
    ("m"   projectile-compile-project)
    ("o"   projectile-find-other-file)
    ("O"   projectile-find-other-file-other-window)
    ("p"   projectile-commander)
    ("r"   projectile-recentf)
    ("s"   projectile-multi-occur)
    ("S"   projectile-replace)
    ("t"   projectile-find-tag)
    ("T"   projectile-regenerate-tags)
    ("u"   projectile-find-test-file)
    ("U"   projectile-test-project)
    ("v"   projectile-display-buffer)
    ("V"   projectile-ibuffer)
    ("X"   fixmee-mode)
    ("x"   fixmee-view-listing))

  (defhydra hydra-exit (:color blue :hint nil :idle 0.4 :inherit (hydra-common/heads))
    "
                                                                        ╭──────┐
   Quit                                                                 │ Exit │
╭───────────────────────────────────────────────────────────────────────┴──────╯
  [_c_] exit emacs (standalone or client)
  [_s_] shutdown the emacs daemon
--------------------------------------------------------------------------------
      "
    ("c" save-buffers-kill-terminal)
    ("s" save-buffers-kill-emacs))

  (defhydra hydra-register (:color blue :hint nil :idle 0.4 :inherit (hydra-common/heads))
    "
                                                                    ╭──────────┐
   Logs                        Registers                Undo        │ Register │
╭───────────────────────────────────────────────────────────────────┴──────────╯
  [_c_] commands history       [_e_] emacs registers    [_u_] undo tree
  [_o_] echo-area messages     [_r_] evil registers
  [_b_] minibuffer             [_m_] evil marks
  [_l_] messages               [_k_] kill ring
  [_d_] diff buffer with file
--------------------------------------------------------------------------------
      "
    ("c" helm-complex-command-history)
    ("d" dkh-diff-buffer-with-file)
    ("e" helm-register)
    ("k" helm-show-kill-ring)
    ("a" helm-all-mark-rings)
    ("l" popwin:messages)
    ("m" evil-show-marks)
    ("o" view-echo-area-messages)
    ("r" evil-show-registers)
    ("b" helm-minibuffer-history)
    ("u" undo-tree-visualize))

  (defhydra hydra-search (:color blue :hint nil :idle 0.4 :inherit (hydra-common/heads))
    "
                                                                      ╭────────┐
   Files                             Buffer                           │ Search │
╭─────────────────────────────────────────────────────────────────────┴────────╯
  [_a_] regex search (Ag)           [_b_] by word
  [_A_] regex by filetype (Ag)      [_o_] by word (occur)
  [_h_] regex search (grep & helm)  [_w_] by word (multi)
  [_g_] regex search (grep)         [_t_] tags & titles
  [_f_] find
  [_l_] locate
--------------------------------------------------------------------------------
      "
    ("A" ag-files)
    ("a" ag)
    ("b" helm-swoop)
    ("f" helm-find)
    ("g" rgrep)
    ("h" helm-do-grep)
    ("l" helm-locate)
    ("o" helm-occur)
    ("t" helm-semantic-or-imenu)
    ("w" helm-multi-swoop))

  (defhydra hydra-games (:color blue :hint nil :idle 0.4 :inherit (hydra-common/heads))
    "
                                                                       ╭───────┐
   Game                                                                │ Games │
╭──────────────────────────────────────────────────────────────────────┴───────╯
  [_p_] 2048-game      [_c_] chess (computer)
  [_b_] bubbles        [_a_] chess (internet)
  [_t_] tetris
  [_g_] gomoku
--------------------------------------------------------------------------------
      "
    ("p" 2048-game)
    ("b" bubbles-set-game-hard)
    ("c" chess)
    ("a" chess-ics)
    ("g" gomoku)
    ("t" tetris))

  (defhydra hydra-system (:color blue :hint nil :idle 0.4 :inherit (hydra-common/heads))
    "
                                                                      ╭────────┐
   Terminals                     System                               │ System │
╭─────────────────────────────────────────────────────────────────────┴────────╯
  [_s_] new multi-term           [_c_] shell command
  [_n_] next multi-term          [_a_] aync shell command
  [_p_] previous multi-term      [_m_] man page
  [_d_] dedicated multi-term     [_l_] list system process
  [_e_] eshell                   [_t_] top command
--------------------------------------------------------------------------------
      "
    ("a" async-shell-command)
    ("c" shell-command)
    ("e" eshell)
    ("m" helm-man-woman)
    ("l" proced)
    ("s" multi-term)
    ("n" multi-term-next)
    ("p" multi-term-previous)
    ("d" multi-term-dedicated-toggle)
    ("t" helm-top))

  (defhydra hydra-media (:color blue :hint nil :idle 0.4 :inherit (hydra-common/heads))
    "
                                                                       ╭───────┐
   Emms                Mpd                  Volume                     │ Media │
╭──────────────────────────────────────────────────────────────────────┴───────╯
 [_b_] browse         [_n_] next song          [_-_] volume down
 [_f_] play file      [_p_] previous song      [_+_] volume up
  ^ ^                 [_c_] clear playlist
  ^ ^                 [_o_] show song
  ^ ^                 [_P_] pause
  ^ ^                 [_s_] stop
  ^ ^                 [_y_] start & sync
--------------------------------------------------------------------------------
      "
    ("a" emms-start)
    ("x" emms-stop)
    ("b" emms-smart-browse)
    ("f" emms-play-file)
    ("m" emms-player-mpd-connect)
    ("c" emms-player-mpd-clear)
    ("n" emms-player-mpd-next)
    ("o" emms-player-mpd-show)
    ("P" emms-player-mpd-pause)
    ("p" emms-player-mpd-previous)
    ("s" emms-player-mpd-stop)
    ("y" emms-player-mpd-start)
    ("-" emms-volume-lower)
    ("\+" emms-volume-raise))

  (defhydra hydra-organization (:color blue :hint nil :idle 0.4 :inherit (hydra-common/heads))
    "
                                                                ╭──────────────┐
     Tasks            Org mode               Comms      Others  │ Organization │
╭───────────────────────────────────────────────────────────────┴──────────────╯
  [_a_] agenda      [_c_] capture             [_m_] mail      [_x_] speed type
  [_l_] agenda list [_p_] pomodoro            [_t_] contacts
  [_d_] calendar    [_s_] search headings     [_h_] add location
   ^ ^              [_g_] open location gmaps
   ^ ^              [_f_] archive subtree
--------------------------------------------------------------------------------
      "
    ("a" org-agenda)
    ("c" org-capture)
    ("d" cfw:open-org-calendar)
    ("g" org-location-google-maps)
    ("h" org-address-google-geocode-set)
    ("l" org-agenda-list)
    ("f" org-archive-subtree)
    ("m" mu4e)
    ("p" org-pomodoro)
    ("s" helm-org-agenda-files-headings)
    ("t" org-contacts)
    ("x" speed-type-text))

  (defhydra hydra-window (:color amaranth)
    "
Move Point^^^^   Move Splitter   ^Ace^                       ^Split^
--------------------------------------------------------------------------------
_._, _<up>_      Shift + Move    _C-a_: ace-window           _2_: split-window-below
_a_, _<left>_                    _C-s_: ace-window-swap      _3_: split-window-right
_j_, _<down>_                    _C-z_: ace-window-delete    ^ ^
_i_, _<right>_                   ^   ^                       ^ ^
You can use arrow-keys or WASD.
"
    ("f" flash-active-buffer nil)
    ("2" my/vsplit-last-buffer nil)
    ("3" my/hsplit-last-buffer nil)
    ("a" windmove-left nil)
    ("j" windmove-down nil)
    ("." windmove-up nil)
    ("i" windmove-right nil)
    ("A" hydra-move-splitter-left nil)
    ("J" hydra-move-splitter-down nil)
    (">" hydra-move-splitter-up nil)
    ("<left>" windmove-left nil)
    ("<down>" windmove-down nil)
    ("<up>" windmove-up nil)
    ("<right>" windmove-right nil)
    ("<S-left>" hydra-move-splitter-left nil)
    ("<S-down>" hydra-move-splitter-down nil)
    ("<S-up>" hydra-move-splitter-up nil)
    ("M-a"  buf-move-left)
    ("M-j"  buf-move-down)
    ("M-."  buf-move-up)
    ("M-i"  buf-move-right)
    ("C-w"  window-configuration-to-register)
    ("C-a"  ace-window nil)
    ("u" hydra--universal-argument nil)
    ("C-s" (lambda () (interactive) (ace-window 4)) nil)
    ("C-z" (lambda () (interactive) (ace-window 16)) nil)
    ("h" hl-line-mode nil)
    ("e" eyebrowse-mode nil)
    ("c" crosshairs-mode nil)
    ("g" golden-ratio-mode nil)
    ("q" nil "quit")))

(use-package ag
  :load-path "site-lisp/ag-el"
  :commands (ag ag-regexp)
  :init
  (use-package helm-ag
    :load-path "site-lisp/emacs-helm-ag"
    :commands helm-ag))

(use-package agda2-mode
  :disabled 1
  :mode "\\.agda\\'"
  :load-path (lambda () (list (agda-site-lisp)))
  :defines agda2-mode-map
  :preface
  (defun agda2-insert-helper-function (&optional prefix)
    (interactive "P")
    (let ((func-def (with-current-buffer "*Agda information*"
                      (buffer-string))))
      (save-excursion
        (forward-paragraph)
        (let ((name (car (split-string func-def " "))))
          (insert "  where\n    " func-def "    " name " x = ?\n")))))

  :config
  (use-package agda-input)
  (bind-key "C-c C-i" 'agda2-insert-helper-function agda2-mode-map)

  (defun char-mapping (key char)
    (bind-key key `(lambda () (interactive) (insert ,char))))

  (char-mapping "H-G" "Γ")
  (char-mapping "H-l" "λ x → ")
  (char-mapping "H-:" " ∷ ")
  (char-mapping "H-r" " → ")
  (char-mapping "H-~" " ≅ ")
  (char-mapping "H-=" " ≡ "))

(use-package alert
  :load-path "lisp/alert"
  :commands alert)

(use-package align
  :bind (("M-["   . align-code)
         ("C-c [" . align-regexp))
  :commands align
  :preface
  (defun align-code (beg end &optional arg)
    (interactive "rP")
    (if (null arg)
        (align beg end)
      (let ((end-mark (copy-marker end)))
        (indent-region beg end-mark nil)
        (align beg end-mark)))))

(use-package allout
  :disabled t
  :diminish allout-mode
  :commands allout-mode
  :config
  (defvar allout-unprefixed-keybindings nil)

  (defun my-allout-mode-hook ()
    (dolist (mapping '((?b . allout-hide-bodies)
                       (?c . allout-hide-current-entry)
                       (?l . allout-hide-current-leaves)
                       (?i . allout-show-current-branches)
                       (?e . allout-show-entry)
                       (?o . allout-show-to-offshoot)))
      (eval `(bind-key ,(concat (format-kbd-macro allout-command-prefix)
                                " " (char-to-string (car mapping)))
                       (quote ,(cdr mapping))
                       allout-mode-map)))

    (if (memq major-mode lisp-modes)
        (unbind-key "C-k" allout-mode-map)))

  (add-hook 'allout-mode-hook 'my-allout-mode-hook))

(use-package AppleScripts
  :bind (("H-s" . osx-say) ("H-f" . osx-pathfinder-or-iterm)))

(use-package archive-region
  :disabled t
  :bind ("C-w" . kill-region-or-archive-region))

(use-package ascii
  :bind ("C-c e A" . ascii-toggle)
  :commands ascii-on
  :functions ascii-off
  :preface
  (defun ascii-toggle ()
    (interactive)
    (if ascii-display
        (ascii-off)
      (ascii-on))))

(use-package tex-site
  :load-path "~/.emacs.d/elpa/auctex-11.88.7"
  :defines (latex-help-cmd-alist latex-help-file)
  :mode ("\\.tex\\'" . TeX-latex-mode)
  :init
  (setq reftex-plug-into-AUCTeX t)
  :config
  (defun latex-help-get-cmd-alist ()    ;corrected version:
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

  (use-package ebib
    :load-path "site-lisp/ebib"
    :preface
    (use-package parsebib :load-path "site-lisp/parsebib"))

  (use-package latex
    :defer t
    :config
    (use-package preview)
    (use-package ac-math
      :disabled t
      :config
      (defun ac-latex-mode-setup ()
        (nconc ac-sources
               '(ac-source-math-unicode ac-source-math-latex
                                        ac-source-latex-commands)))

      (add-to-list 'ac-modes 'latex-mode)
      (add-hook 'latex-mode-hook 'ac-latex-mode-setup))

    (add-hook 'LaTeX-mode-hook 'reftex-mode)

    (info-lookup-add-help :mode 'LaTeX-mode
                          :regexp ".*"
                          :parse-rule "\\\\?[a-zA-Z]+\\|\\\\[^a-zA-Z]"
                          :doc-spec '(("(latex2e)Concept Index" )
                                      ("(latex2e)Command Index")))))

(use-package auto-complete-config
  :disabled t
  :load-path "site-lisp/auto-complete"
  :diminish auto-complete-mode
  :init
  (use-package pos-tip)
  (ac-config-default)

  :config
  (ac-set-trigger-key "<backtab>")
  (setq ac-use-menu-map t)

  (bind-key "H-M-?" #'ac-last-help)
  (unbind-key "C-s" ac-completing-map))

;;;_ , auto-dim-other-buffers

(use-package auto-dim-other-buffers
  :defer 10
  :commands auto-dim-other-buffers-mode
  :init
  (auto-dim-other-buffers-mode 1))
(use-package autopair
  :disabled t
  :load-path "site-lisp/autopair"
  :commands autopair-mode
  :diminish autopair-mode
  :init
  (hook-into-modes #'autopair-mode
                   'c-mode-common-hook
                   'text-mode-hook
                   'ruby-mode-hook
                   'php-mode
                   'python-mode-hook
                   'sh-mode-hook))

(use-package autorevert
  :commands auto-revert-mode
  :diminish auto-revert-mode
  :init
  (add-hook 'find-file-hook #'(lambda () (auto-revert-mode 1))))

(use-package backup-each-save
  :commands backup-each-save
  :preface
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
              args (concat
                    find-program " . "
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
        (set (make-local-variable 'dired-subdir-switches)
             find-ls-subdir-switches)
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

  (bind-key "C-x ~" #'show-backups)

  :init
  (defun my-make-backup-file-name (file)
    (make-backup-file-name-1 (file-truename file)))

  (add-hook 'after-save-hook 'backup-each-save)

  :config
  (defun backup-each-save-filter (filename)
    (not (string-match
          (concat "\\(^/tmp\\|\\.emacs\\.d/data\\(-alt\\)?/"
                  "\\|\\.newsrc\\(\\.eld\\)?\\|"
                  "\\(archive/sent/\\|recentf\\`\\)\\)")
          filename)))

  (setq backup-each-save-filter-function 'backup-each-save-filter)

  (defun my-dont-backup-files-p (filename)
    (unless (string-match filename "\\(archive/sent/\\|recentf\\`\\)")
      (normal-backup-enable-predicate filename)))

  (setq backup-enable-predicate 'my-dont-backup-files-p))

(use-package bbdb-com
  :load-path "override/bbdb/lisp"
  :commands bbdb-create
  :bind ("M-B" . bbdb))

(use-package bm
  :disabled t
  :load-path "site-lisp/bm"
  :init
  (defvar ctl-period-breadcrumb-map)
  (define-prefix-command 'ctl-period-breadcrumb-map)
  (bind-key "C-. c" #'ctl-period-breadcrumb-map)

  :bind (("C-. c b" . bm-last-in-previous-buffer)
         ("C-. c f" . bm-first-in-next-buffer)
         ("C-. c g" . bm-previous)
         ("C-. c l" . bm-show-all)
         ("C-. c c" . bm-toggle)
         ("C-. c m" . bm-toggle)
         ("C-. c n" . bm-next)
         ("C-. c p" . bm-previous)))

(use-package bookmark
  :load-path "site-lisp/bookmark-plus"
  :defer 10
  :config
  (use-package bookmark+))

(use-package boxquote
  :load-path "site-lisp/boxquote"
  :defer t
  :config
  (setq-default  boxquote-bottom-corner "╰"      ; U+2570
                 boxquote-side          "│ "     ; U+2572 + space
                 boxquote-top-and-tail  "────"   ; U+2500 (×4)
                 boxquote-top-corner    "╭")     ; U+256F
  (when (when-feature-loaded 'hydra)
    (defhydra hydra-boxquote (:color blue :hint nil)
       "
                                                                    ╭──────────┐
  Text           External           Apropos         Do              │ Boxquote │
╭───────────────────────────────────────────────────────────────────┴──────────╯
  [_r_] region        [_f_] file      [_K_] describe-key        [_t_] title
  [_p_] paragraph     [_b_] buffer    [_F_] describe-function   [_u_] unbox
  [_a_] buffer        [_s_] shell     [_V_] describe-variable   [_w_] fill-paragraph
  [_e_] text           ^ ^            [_W_] where-is            [_n_] narrow
  [_d_] defun         [_y_] yank       ^ ^                      [_c_] narrow to content
  [_q_] boxquote      [_Y_] yanked     ^ ^                      [_x_] kill
--------------------------------------------------------------------------------
       "
      ("<esc>" nil "quit")
      ("x" boxquote-kill)
      ("Y" boxquote-yank)
      ("e" boxquote-text)
      ("u" boxquote-unbox)
      ("d" boxquote-defun)
      ("t" boxquote-title)
      ("r" boxquote-region)
      ("a" boxquote-buffer)
      ("q" boxquote-boxquote)
      ("W" boxquote-where-is)
      ("p" boxquote-paragraph)
      ("f" boxquote-insert-file)
      ("K" boxquote-describe-key)
      ("s" boxquote-shell-command)
      ("b" boxquote-insert-buffer)
      ("y" boxquote-kill-ring-save)
      ("w" boxquote-fill-paragraph)
      ("F" boxquote-describe-function)
      ("V" boxquote-describe-variable)
      ("n" boxquote-narrow-to-boxquote)
      ("c" boxquote-narrow-to-boxquote-content))))

(use-package browse-kill-ring+
  :defer 10
  :commands browse-kill-ring)

(use-package cmake-mode
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'"         . cmake-mode)))

(use-package color-moccur
  :commands (isearch-moccur isearch-all isearch-moccur-all)
  :bind ("M-s O" . moccur)
  :init
  (bind-key "M-o" #'isearch-moccur isearch-mode-map)
  (bind-key "M-O" #'isearch-moccur-all isearch-mode-map)
  :config
  (use-package moccur-edit))

(use-package company
  :load-path "site-lisp/company-mode"
  :diminish company-mode
  :commands company-mode
  :config
  ;; From https://github.com/company-mode/company-mode/issues/87
  ;; See also https://github.com/company-mode/company-mode/issues/123
  (defadvice company-pseudo-tooltip-unless-just-one-frontend
      (around only-show-tooltip-when-invoked activate)
    (when (company-explicit-action-p)
      ad-do-it))

  (use-package helm-company
    :disabled t
    :load-path "site-lisp/helm-company"))

(use-package compile
  :bind (("C-c c" . compile)
         ("M-O"   . show-compilation))
  :preface
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

  (defun compilation-ansi-color-process-output ()
    (ansi-color-process-output nil)
    (set (make-local-variable 'comint-last-output-start)
         (point-marker)))

  :config
  (add-hook 'compilation-filter-hook #'compilation-ansi-color-process-output))

(use-package conf-mode
  :mode "\\.info\\|\\.gitmodules")

(use-package copy-code
  :disabled t
  :bind ("H-M-W" . copy-code-as-rtf))

(use-package counsel
  :load-path "site-lisp/swiper"
  :bind (("C-h f" . counsel-describe-function)
         ("C-h v" . counsel-describe-variable)
         ("C-x C-f" . counsel-find-file)
         ;; ("C-c j" . counsel-git-grep)
         ("M-x" . counsel-M-x))
  :config
  (setq counsel-find-file-at-point t)
  (ivy-set-actions
   'counsel-find-file
   `((,(propertize "delete" 'face 'font-lock-warning-face)
      (lambda (x) (delete-file (expand-file-name x ivy--directory))))))
  (use-package smex))

(use-package crosshairs
  :bind ("M-o c" . crosshairs-mode))

(use-package cursor-chg
  :disabled t
  :defer 10
  :commands change-cursor-mode
  :config
  (change-cursor-mode 1)
  (toggle-cursor-type-when-idle 1))

(use-package debbugs-gnu
  :disabled t
  :load-path "site-lisp/debbugs"
  :commands (debbugs-gnu debbugs-gnu-search))

(use-package dedicated
  :bind ("C-. D" . dedicated-mode))

(use-package diff-mode
  :commands diff-mode
  :config
  (use-package diff-mode-))

(use-package dired
  :bind ("C-c J" . dired-double-jump)
  :preface
  (defvar mark-files-cache (make-hash-table :test #'equal))

  (defun mark-similar-versions (name)
    (let ((pat name))
      (if (string-match "^\\(.+?\\)-[0-9._-]+$" pat)
          (setq pat (match-string 1 pat)))
      (or (gethash pat mark-files-cache)
          (ignore (puthash pat t mark-files-cache)))))

  (defun dired-mark-similar-version ()
    (interactive)
    (setq mark-files-cache (make-hash-table :test #'equal))
    (dired-mark-sexp '(mark-similar-versions name)))

  (defun dired-double-jump (first-dir second-dir)
    (interactive
     (list (read-directory-name "First directory: "
                                (expand-file-name "~")
                                nil nil "dl/")
           (read-directory-name "Second directory: "
                                (expand-file-name "~")
                                nil nil "Archives/")))
    (dired first-dir)
    (dired-other-window second-dir))

  :config
  (use-package dired-x)
  (use-package dired+
    :config
    (unbind-key "M-s f" dired-mode-map))

  (bind-key "l" #'dired-up-directory dired-mode-map)
  (bind-key "e" #'dired-mark-files-containing-regexp dired-mode-map)
  (defun my-dired-switch-window ()
    (interactive)
    (if (eq major-mode 'sr-mode)
        (call-interactively #'sr-change-window)
      (call-interactively #'other-window)))

  (bind-key "<tab>" #'my-dired-switch-window dired-mode-map)

  (bind-key "M-!" #'async-shell-command dired-mode-map)
  (unbind-key "M-G" dired-mode-map)

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
        (funcall dired-omit-regexp-orig)))))

(use-package dired-toggle
  :load-path "site-lisp/dired-toggle"
  :bind ("C-. d" . dired-toggle)
  :preface
  (defun my-dired-toggle-mode-hook ()
    (interactive)
    (visual-line-mode 1)
    (setq-local visual-line-fringe-indicators '(nil right-curly-arrow))
    (setq-local word-wrap nil))
  :config
  (add-hook 'dired-toggle-mode-hook #'my-dired-toggle-mode-hook))

(use-package doxymacs
  :disabled t
  :load-path "site-lisp/doxymacs/lisp/")

(use-package eclimd
  :load-path "site-lisp/emacs-eclim"
  :commands start-eclimd
  :config
  (use-package eclim
    :defer t
    :config
    (global-eclim-mode)
    (use-package company-emacs-eclim
      :requires company
      :config
      (company-emacs-eclim-setup))))

(use-package edebug
  :defer t
  :preface
  (defvar modi/fns-in-edebug nil
    "List of functions for which `edebug' is instrumented.")

  (defconst modi/fns-regexp
    (concat "(\\s-*"
            "\\(defun\\|defmacro\\)\\s-+"
            "\\(?1:\\(\\w\\|\\s_\\)+\\)\\_>") ; word or symbol char
    "Regexp to find defun or defmacro definition.")

  (defun modi/toggle-edebug-defun ()
    (interactive)
    (let (fn)
      (save-excursion
        (search-backward-regexp modi/fns-regexp)
        (setq fn (match-string 1))
        (mark-sexp)
        (narrow-to-region (point) (mark))
        (if (member fn modi/fns-in-edebug)
            ;; If the function is already being edebugged, uninstrument it
            (progn
              (setq modi/fns-in-edebug (delete fn modi/fns-in-edebug))
              (eval-region (point) (mark))
              (setq-default eval-expression-print-length 12)
              (setq-default eval-expression-print-level  4)
              (message "Edebug disabled: %s" fn))
          ;; If the function is not being edebugged, instrument it
          (progn
            (add-to-list 'modi/fns-in-edebug fn)
            (setq-default eval-expression-print-length nil)
            (setq-default eval-expression-print-level  nil)
            (edebug-defun)
            (message "Edebug: %s" fn)))
        (widen)))))

(use-package ediff
  :init
  (defvar ctl-period-equals-map)
  (define-prefix-command 'ctl-period-equals-map)
  (bind-key "C-. =" #'ctl-period-equals-map)

  :bind (("C-. = b" . ediff-buffers)
         ("C-. = B" . ediff-buffers3)
         ("C-. = c" . compare-windows)
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

(use-package edit-server
  :disabled t
  :if (and window-system
           (not running-alternate-emacs)
           (not noninteractive))
  :load-path "site-lisp/emacs_chrome/servers/"
  :init
  (add-hook 'after-init-hook 'server-start t)
  (add-hook 'after-init-hook 'edit-server-start t))

(use-package edit-var
  :bind ("C-c e v" . edit-variable))

(use-package emmet-mode
  :disabled t
  :config
  ;; (add-hook 'web-mode-hook  'emmet-mode)
  (bind-keys :map emmet-mode-keymap
             ("C-n" . emmet-next-edit-point)
             ("C-p" . emmet-prev-edit-point))

  (use-package helm-emmet
    :load-path "site-lisp/helm-emmet"
    :commands helm-emmet)

  (use-package ac-emmet
    :config
    (add-hook 'web-mode-hook 'ac-emmet-html-setup)))

(use-package emms-setup
  :load-path "site-lisp/emms/lisp"
  :defines (emms-info-functions emms-player-simple-process-name)
  :commands (emms-standard emms-devel emms-stream-init)  ;; for helm-emms
  :bind ("C-. M" . my-emms)
  :preface
  (defvar emms-initialized nil)
  (declare-function emms-smart-browse "emms-browser")
  (require 'emms-player-simple)
  (defun my-emms ()
    (interactive)
    ; Stop set up stuff from runnig wit every function call.
    (unless emms-initialized
      (emms-standard)
      (emms-default-players)
      (require 'emms-info-libtag)
      (require 'emms-browser)
      (define-emms-simple-player flac123 '(file) 
        "\\.flac$" "/usr/local/bin/mplayer")
      (add-to-list 'emms-player-list emms-player-flac123)
      (setq emms-info-functions '(emms-info-libtag)
            emms-initialized t))
    (call-interactively #'emms-smart-browse))

  :config
  (setq emms-info-libtag-program-name "~/bin/emms-print-metadata")
  (bind-key "S-<f7>" #'emms-previous)
  (bind-key "S-<f8>" #'emms-pause)
  (bind-key "S-<f9>" #'emms-next)
  (bind-key "S-<f10>" #'emms-stop)
  ; Add music file or directory to EMMS playlist on ! in dired.
  (bind-key "!" 'emms-add-dired)
  
  (defun emms-player-mplayer-volume-up ()
    "Depends on mplayer’s -slave mode."
    (interactive)
    (process-send-string emms-player-simple-process-name "volume 1\n"))

  (defun emms-player-mplayer-volume-down ()
    "Depends on mplayer’s -slave mode."
    (interactive)
    (process-send-string emms-player-simple-process-name "volume -1\n"))

  (bind-key "C-. C--" #'emms-player-mplayer-volume-down)
  (bind-key "C-. C-=" #'emms-player-mplayer-volume-up))

(use-package eyebrowse
  :commands eyebrowse-mode
  :diminish eyebrowse-mode
  :init
  (setq eyebrowse-wrap-around t
        eyebrowse-switch-back-and-forth t)
  :config
  (eyebrowse-mode 1)
  (global-set-key (kbd "C-'") 'eyebrowse-next-window-config))

(use-package gist
  :load-path "site-lisp/gist"
  :bind ("C-c G" . gist-region-or-buffer))

(use-package erc
  :defer t
  :defines (erc-timestamp-only-if-changed-flag
            erc-timestamp-format
            erc-fill-prefix
            erc-fill-column
            erc-insert-timestamp-function
            erc-modified-channels-alist)
  :preface
  (defun lookup-password (host user port)
    (require 'auth-source)
    (funcall (plist-get
              (car (auth-source-search
                    :host host
                    :user user
                    :type 'netrc
                    :port port))
              :secret)))

  (defun slowping (host)
    (= 0 (call-process "ping" nil nil nil "-c1" "-W5000" "-q" host)))

  (defun irc ()
    (interactive)
    (require 'erc)
    (if (slowping "192.168.9.133")
        (progn
          (erc :server "192.168.9.133"
               :port 6697
               :nick "dkh"
               :password (lookup-password "192.168.9.133"
                                          "dkh/freenode" 6697))
          (erc :server "192.168.9.133"
               :port 6697
               :nick "dkh"
               :password (lookup-password "192.168.9.133"
                                          "dhk/bitlbee" 6697)))

      (erc-tls :server "irc.freenode.net"
               :port 6697
               :nick "dhk"
               :password (lookup-password "irc.freenode.net" "dhk" 6667))))

  (defun setup-irc-environment ()
    (setq erc-timestamp-only-if-changed-flag nil
          erc-timestamp-format "%H:%M "
          erc-fill-prefix "          "
          erc-fill-column 88
          erc-insert-timestamp-function 'erc-insert-timestamp-left)

    (use-package agda-input
      :config
      (set-input-method "Agda"))

    (defun reset-erc-track-mode ()
      (interactive)
      (setq erc-modified-channels-alist nil)
      (erc-modified-channels-update)
      (erc-modified-channels-display)
      (force-mode-line-update))

    (bind-key "C-c r" #'reset-erc-track-mode))

  (defcustom erc-foolish-content '()
    "Regular expressions to identify foolish content.
    Usually what happens is that you add the bots to
    `erc-ignore-list' and the bot commands to this list."
    :group 'erc
    :type '(repeat regexp))

  (defun erc-foolish-content (msg)
    "Check whether MSG is foolish."
    (erc-list-match erc-foolish-content msg))

  :init
  (add-hook 'erc-mode-hook 'setup-irc-environment)
  (add-to-list
   'erc-mode-hook
   #'(lambda () (set (make-local-variable 'scroll-conservatively) 100)))

  (if running-alternate-emacs
      (add-hook 'after-init-hook 'irc))

  :config
  (erc-track-minor-mode 1)
  (erc-track-mode 1)

  (use-package erc-alert)
  (use-package erc-highlight-nicknames)
  (use-package erc-patch)
  (use-package erc-macros)
  (use-package erc-image
    :disabled t
    :load-path "site-lisp/erc-image"
    :commands (erc-image-show-url-image)
    :init
    (define-erc-module image nil
      "Display inlined images in ERC buffer"
      ((add-hook 'erc-insert-modify-hook 'erc-image-show-url-image t)
       (add-hook 'erc-send-modify-hook 'erc-image-show-url-image t))
      ((remove-hook 'erc-insert-modify-hook 'erc-image-show-url-image)
       (remove-hook 'erc-send-modify-hook 'erc-image-show-url-image))
      t))

  (use-package erc-yank
    :load-path "lisp/erc-yank"
    :config
    (bind-key "C-y" #'erc-yank erc-mode-map))

  (use-package wtf
    :commands wtf-is)

  (add-hook 'erc-insert-pre-hook
            (lambda (s)
              (when (erc-foolish-content s)
                (setq erc-insert-this nil)))))

(use-package eshell
  :commands (eshell eshell-command)
  :preface
  (defvar eshell-isearch-map
    (let ((map (copy-keymap isearch-mode-map)))
      (define-key map [(control ?m)] 'eshell-isearch-return)
      (define-key map [return]       'eshell-isearch-return)
      (define-key map [(control ?r)] 'eshell-isearch-repeat-backward)
      (define-key map [(control ?s)] 'eshell-isearch-repeat-forward)
      (define-key map [(control ?g)] 'eshell-isearch-abort)
      (define-key map [backspace]    'eshell-isearch-delete-char)
      (define-key map [delete]       'eshell-isearch-delete-char)
      map)
    "Keymap used in isearch in Eshell.")

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

    (use-package em-unix
      :defer t
      :config
      (unintern 'eshell/su nil)
      (unintern 'eshell/sudo nil)))

  :init
  (add-hook 'eshell-first-time-mode-hook 'eshell-initialize)

  (use-package esh-toggle
    :bind ("C-x C-z" . eshell-toggle)))

(use-package ess-site
  :disabled t
  :load-path "site-lisp/ess/lisp/"
  :commands R)

(use-package etags
  :bind ("M-T" . tags-search))

(use-package eval-expr
  :load-path "site-lisp/eval-expr"
  :bind ("M-:" . eval-expr)
  :config
  (setq eval-expr-print-function 'pp
        eval-expr-print-level 20
        eval-expr-print-length 100)

  (defun eval-expr-minibuffer-setup ()
    (set-syntax-table emacs-lisp-mode-syntax-table)
    (paredit-mode)))

(use-package eww
  :bind ("H-M-g" . eww)
  :config
  (use-package eww-lnum
    :load-path "site-lisp/eww-lnum"
    :config
    (bind-key "f" #'eww-lnum-follow eww-mode-map)
    (bind-key "F" #'eww-lnum-universal eww-mode-map)))

(use-package feature-mode
  :load-path "site-lisp/cucumber"
  :mode "\\.feature$")

(use-package fetchmail-mode
  :mode ".fetchmailrc$"
  :commands fetchmail-mode)

(use-package fic-mode
  :init
  (hook-into-modes 'fic-mode 'prog-mode-hook))
(use-package flyspell
  :bind (("C-c i b" . flyspell-buffer)
         ("C-c i f" . flyspell-mode))
  :init
  (use-package ispell
    :bind (("C-c i c" . ispell-comments-and-strings)
           ("C-c i d" . ispell-change-dictionary)
           ("C-c i k" . ispell-kill-ispell)
           ("C-c i m" . ispell-message)
           ("C-c i r" . ispell-region)))
  :config
  (unbind-key "C-." flyspell-mode-map))

(use-package github-browse-file
  :bind ("H-o" . github-browse-file))

(use-package git-messenger
  :load-path "site-lisp/emacs-git-messenger"
  :bind ("C-x v m" . git-messenger:popup-message))

(use-package git-wip-mode
  :load-path "site-lisp/git-wip/emacs/"
  :diminish git-wip-mode
  :commands git-wip-mode
  :init (add-hook 'find-file-hook #'(lambda () (git-wip-mode 1))))

(use-package golden-ratio
  :diminish golden-ratio-mode
  :commands golden-ratio-mode
  :defer 5
  :config
  (progn
    (defun my/helm-alive-p ()
      (if (boundp 'helm-alive-p)
          (symbol-value 'helm-alive-p)))

    ;; Inhibit helm
    (add-to-list 'golden-ratio-inhibit-functions #'my/helm-alive-p)
    ;; Inhibit ERC and mu4e
    (setq golden-ratio-exclude-modes
          '(erc-mode mu4e-headers-mode mu4e-view-mode ediff-mode)))
    (setq golden-ratio-auto-scale t))

(use-package google-maps
  :load-path "site-lisp/google-maps"
  :defer 5
  :config
  (bind-keys :map google-maps-static-mode-map
             ("H" . google-maps-static-add-home-marker)
             ("k" . google-maps-static-move-north)
             ("j" . google-maps-static-move-south)
             ("h" . google-maps-static-move-west)
             ("l" . google-maps-static-move-east)
             ("y" . google-maps-static-copy-url)
             ("q" . quit-window))

  (when (when-feature-loaded 'hydra)
    (bind-keys :map google-maps-static-mode-map
               ("\\" . hydra-gmaps/body))
    (defhydra hydra-gmaps (:hint nil :color blue)
        "
                                                                   ╭─────────────┐
    Move       Zoom        Do                                      │ Google maps │
  ╭────────────────────────────────────────────────────────────────┴─────────────╯
   ^ ^   ^ _k_ ^    ^ ^   _<_/_+_/_._    [_t_] map type
   ^ ^   ^ ^↑^ ^    ^ ^   ^ ^ ^↑^ ^ ^    [_g_] refresh
   _h_ ← _c_|_C_ → _l_    ^ _z_|_Z_ ^    [_y_] yank url
   ^ ^   ^ ^↓^ ^    ^ ^   ^ ^ ^↓^ ^ ^    [_q_] quit
   ^ ^   ^ _j_ ^    ^ ^   _>_/_-_/_,_
  --------------------------------------------------------------------------------
        "
        ("\\" hydra-master/body "back")
        ("<ESC>" nil "quit")
        ("q"       google-maps-static-quit)
        ("+"       google-maps-static-zoom-in)
        (">"       google-maps-static-zoom-in)
        ("."       google-maps-static-zoom-in)
        ("-"       google-maps-static-zoom-out)
        ("<"       google-maps-static-zoom-out)
        (","       google-maps-static-zoom-out)
        ("z"       google-maps-static-zoom)
        ("Z"       google-maps-static-zoom-remove)
        ("y"       google-maps-static-copy-url)
        ("c"       google-maps-static-center)
        ("C"       google-maps-static-center-remove)
        ("t"       google-maps-static-set-maptype)
        ("g"       google-maps-static-refresh)
        ("k"       google-maps-static-move-north)
        ("j"       google-maps-static-move-south)
        ("h"       google-maps-static-move-west)
        ("l"       google-maps-static-move-east)))

  (use-package org-location-google-maps))

(use-package google-this
  :load-path "site-lisp/emacs-google-this"
  :defer t)

(use-package google-translate
    :load-path "site-lisp/google-translate"
  :bind ("\C-ct" . google-translate-smooth-translate)
  :init
  (progn
        (require 'google-translate-smooth-ui)
        (setq google-translate-translation-directions-alist '(("ja" . "en")))))

(use-package grep
  :bind (("M-s d" . find-grep-dired)
         ("M-s F" . find-grep)
         ("M-s G" . grep))
  :config
  (add-hook 'grep-mode-hook #'(lambda () (use-package grep-ed)))

  (grep-apply-setting 'grep-command "egrep -nH -e ")
  (grep-apply-setting
   'grep-find-command
   '("find . -type f -print0 | xargs -P4 -0 egrep -nH " . 49)))

(use-package gud
  :disabled t
  :commands gud-gdb
  :bind ("C-. g" . show-debugger)
  :init
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
  :config
  (progn
    (bind-key "<f9>" #'gud-cont)
    (bind-key "<f10>" #'gud-next)
    (bind-key "<f11>" #'gud-step)
    (bind-key "S-<f11>" #'gud-finish)))

(use-package guide-key
  :disabled t
  :load-path "site-lisp/guide-key"
  :diminish guide-key-mode
  :commands guide-key-mode
  :defer 10
  :config
  (setq guide-key/guide-key-sequence
        '("C-."
          "C-h e"
          "C-x 4"
          "C-x 5"
          "C-x r"
          "M-o"
          "M-s"))
  (guide-key-mode 1))

(use-package helm-grep
  :commands helm-do-grep-1
  :bind (("M-s f" . my-helm-do-grep-r)
         ("M-s g" . my-helm-do-grep))
  :preface
  (defun my-helm-do-grep ()
    (interactive)
    (helm-do-grep-1 (list default-directory)))

  (defun my-helm-do-grep-r ()
    (interactive)
    (helm-do-grep-1 (list default-directory) t)))

(use-package helm-swoop
  :load-path "site-lisp/helm-swoop"
  :bind (("M-s o" . helm-swoop)
         ("M-s /" . helm-multi-swoop))
  :config
  (use-package helm-match-plugin
    :config
    (helm-match-plugin-mode 1)))

(use-package helm-descbinds
  :load-path "site-lisp/helm-descbinds"
  :bind ("C-h b" . helm-descbinds)
  :init
  (fset 'describe-bindings 'helm-descbinds)
  :config
  (require 'helm-config))

(use-package helm-config
  :if (not running-alternate-emacs)
  :demand t
  :load-path "site-lisp/helm"
  :bind (("C-c h"   . helm-command-prefix)
         ("C-h a"   . helm-apropos)
         ("C-h e a" . my-helm-apropos)
         ("C-x f"   . helm-multi-files)
         ("M-s b"   . helm-occur)
         ("M-s n"   . my-helm-find)
         ("M-H"     . helm-resume))

  :preface
  (defun my-helm-find ()
    (interactive)
    (helm-find nil))

  :config
  (use-package helm-commands)
  (use-package helm-files)
  (use-package helm-buffers)
  (use-package helm-bm
    :load-path "site-lisp/helm-bm"
    :bind ("C-c b" . helm-bm))

  (use-package helm-open-github
    :load-path "site-lisp/helm-open-github"
    :init
    ;; (global-set-key (kbd "C-c o f") 'helm-open-github-from-file)
    ;; (global-set-key (kbd "C-c o c") 'helm-open-github-from-commit)
    ;; (global-set-key (kbd "C-c o i") 'helm-open-github-from-issues)
    ;; (global-set-key (kbd "C-c o p") 'helm-open-github-from-pull-requests)
    )

  (use-package helm-gtags
    :load-path "site-lisp/helm-gtags"
    :config
    (setq helm-gtags-path-style 'relative)
    (setq helm-gtags-ignore-case t)
    (setq helm-gtags-auto-update t)

    (ggtags-mode 1)
    (helm-gtags-mode 1)
    (define-key ggtags-mode-map (kbd "M-.") nil)
    (define-key ggtags-mode-map (kbd "M-/") nil)
    (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-find-tag)
    (define-key helm-gtags-mode-map (kbd "M-/") 'helm-gtags-pop-stack))
  
  (use-package helm-mode
    :diminish helm-mode
    :init
    (helm-mode 1))

  (use-package helm-ls-git
    :load-path "site-lisp/helm-ls-git")

  (use-package helm-match-plugin
    :config
    (helm-match-plugin-mode 1))

  (helm-autoresize-mode 1)

  (bind-key "<tab>" #'helm-execute-persistent-action helm-map)
  (bind-key "C-i" #'helm-execute-persistent-action helm-map)
  (bind-key "C-z" #'helm-select-action helm-map)
  (bind-key "H-v" #'helm-previous-page helm-map)

  (when (executable-find "curl")
    (setq helm-net-prefer-curl t))

  (when (when-feature-loaded 'hydra)
      (define-key helm-map (kbd "\\") 'hydra-helm/body)
      (defhydra hydra-helm (:hint nil :color pink)
        "
                                                                          ╭──────┐
   Navigation   Other  Sources     Mark             Do             Help   │ Helm │
  ╭───────────────────────────────────────────────────────────────────────┴──────╯
        ^_k_^         _K_       _p_   [_m_] mark         [_v_] view         [_H_] helm help
        ^^↑^^         ^↑^       ^↑^   [_t_] toggle all   [_d_] delete       [_s_] source help
    _h_ ←   → _l_     _c_       ^ ^   [_u_] unmark all   [_f_] follow: %(helm-attr 'follow)
        ^^↓^^         ^↓^       ^↓^    ^ ^               [_y_] yank selection
        ^_j_^         _J_       _n_    ^ ^               [_w_] toggle windows
  --------------------------------------------------------------------------------
        "
        ("<tab>" helm-keyboard-quit "back" :exit t)
        ("<escape>" nil "quit")
        ("\\" (insert "\\") "\\" :color blue)
        ("h" helm-beginning-of-buffer)
        ("j" helm-next-line)
        ("k" helm-previous-line)
        ("l" helm-end-of-buffer)
        ("g" helm-beginning-of-buffer)
        ("G" helm-end-of-buffer)
        ("n" helm-next-source)
        ("p" helm-previous-source)
        ("K" helm-scroll-other-window-down)
        ("J" helm-scroll-other-window)
        ("c" helm-recenter-top-bottom-other-window)
        ("m" helm-toggle-visible-mark)
        ("t" helm-toggle-all-marks)
        ("u" helm-unmark-all)
        ("H" helm-help)
        ("s" helm-buffer-help)
        ("v" helm-execute-persistent-action)
        ("d" helm-persistent-delete-marked)
        ("y" helm-yank-selection)
        ("w" helm-toggle-resplit-and-swap-windows)
        ("f" helm-follow-mode)))

  (use-package helm-themes
    :load-path "site-lisp/helm-themes"
    :commands helm-themes)
    (setq helm-google-suggest-use-curl-p t))

(use-package hi-lock
  :bind (("M-o l" . highlight-lines-matching-regexp)
         ("M-o r" . highlight-regexp)
         ("M-o w" . highlight-phrase)))

(use-package hilit-chg
  :bind ("M-o C" . highlight-changes-mode))

(use-package hippie-exp
  :bind (("M-/" . dabbrev-expand)
         ("M-?" . hippie-expand))
  :preface
  (autoload 'yas-expand "yasnippet" nil t)

  (defun my-yas-hippie-try-expand (first-time)
    (if (not first-time)
        (let ((yas-fallback-behavior 'return-nil))
          (yas-expand))
      (undo 1)
      nil))

  (defun my-hippie-expand-completions (&optional hippie-expand-function)
    "Return the full list of possible completions generated by `hippie-expand'.
   The optional argument can be generated with `make-hippie-expand-function'."
    (let ((this-command 'my-hippie-expand-completions)
          (last-command last-command)
          (buffer-modified (buffer-modified-p))
          (hippie-expand-function (or hippie-expand-function 'hippie-expand)))
      (flet ((ding))        ; avoid the (ding) when hippie-expand exhausts its
                                        ; options.
        (while (progn
                 (funcall hippie-expand-function nil)
                 (setq last-command 'my-hippie-expand-completions)
                 (not (equal he-num -1)))))
      ;; Evaluating the completions modifies the buffer, however we will finish
      ;; up in the same state that we began.
      (set-buffer-modified-p buffer-modified)
      ;; Provide the options in the order in which they are normally generated.
      (delete he-search-string (reverse he-tried-table))))

  (defmacro my-ido-hippie-expand-with (hippie-expand-function)
    "Generate an interactively-callable function that offers ido-based
  completion using the specified hippie-expand function."
    `(call-interactively
      (lambda (&optional selection)
        (interactive
         (let ((options (my-hippie-expand-completions ,hippie-expand-function)))
           (if options
               (list
                ;; (ido-completing-read "Completions: " options)
                (completing-read "Completions: " options)
                ))))
        (if selection
            (he-substitute-string selection t)
          (message "No expansion found")))))

  (defun my-ido-hippie-expand ()
    "Offer ido-based completion for the word at point."
    (interactive)
    (my-ido-hippie-expand-with 'hippie-expand))

  (defun my-try-expand-company (old)
    (require 'company)
    (unless company-candidates
      (company-auto-begin))
    (if (not old)
        (progn
          (he-init-string (he-lisp-symbol-beg) (point))
          (if (not (he-string-member he-search-string he-tried-table))
              (setq he-tried-table (cons he-search-string he-tried-table)))
          (setq he-expand-list
                (and (not (equal he-search-string ""))
                     company-candidates))))
    (while (and he-expand-list
                (he-string-member (car he-expand-list) he-tried-table))
      (setq he-expand-list (cdr he-expand-list)))
    (if (null he-expand-list)
        (progn
          (if old (he-reset-string))
          ())
      (progn
	(he-substitute-string (car he-expand-list))
	(setq he-expand-list (cdr he-expand-list))
	t)))

  (defun he-tag-beg ()
    (save-excursion
      (backward-word 1)
      (point)))

  (defun tags-complete-tag (string predicate what)
    (save-excursion
      ;; If we need to ask for the tag table, allow that.
      (if (eq what t)
          (all-completions string (tags-completion-table) predicate)
        (try-completion string (tags-completion-table) predicate))))

  (defun try-expand-tag (old)
    (when tags-table-list
      (unless old
        (he-init-string (he-tag-beg) (point))
        (setq he-expand-list
              (sort (all-completions he-search-string 'tags-complete-tag)
                    'string-lessp)))
      (while (and he-expand-list
                  (he-string-member (car he-expand-list) he-tried-table))
        (setq he-expand-list (cdr he-expand-list)))
      (if (null he-expand-list)
          (progn
            (when old (he-reset-string))
            ())
        (he-substitute-string (car he-expand-list))
        (setq he-expand-list (cdr he-expand-list))
        t)))

  (defun my-dabbrev-substring-search (pattern &optional reverse limit)
    (let ((result ())
          (regpat (cond ((not hippie-expand-dabbrev-as-symbol)
                         (concat (regexp-quote pattern) "\\sw+"))
                        ((eq (char-syntax (aref pattern 0)) ?_)
                         (concat (regexp-quote pattern) "\\(\\sw\\|\\s_\\)+"))
                        (t
                         (concat (regexp-quote pattern)
                                 "\\(\\sw\\|\\s_\\)+")))))
      (while (and (not result)
                  (if reverse
                      (re-search-backward regpat limit t)
                    (re-search-forward regpat limit t)))
        (setq result (buffer-substring-no-properties
                      (save-excursion
                        (goto-char (match-beginning 0))
                        (skip-syntax-backward "w_")
                        (point))
                      (match-end 0)))
        (if (he-string-member result he-tried-table t)
            (setq result nil)))     ; ignore if bad prefix or already in table
      result))

  (defun try-my-dabbrev-substring (old)
    (let ((old-fun (symbol-function 'he-dabbrev-search)))
      (fset 'he-dabbrev-search (symbol-function 'my-dabbrev-substring-search))
      (unwind-protect
          (try-expand-dabbrev old)
        (fset 'he-dabbrev-search old-fun))))

  (defun try-expand-flexible-abbrev (old)
    "Try to complete word using flexible matching.

  Flexible matching works by taking the search string and then
  interspersing it with a regexp for any character. So, if you try
  to do a flexible match for `foo' it will match the word
  `findOtherOtter' but also `fixTheBoringOrange' and
  `ifthisisboringstopreadingnow'.

  The argument OLD has to be nil the first call of this function, and t
  for subsequent calls (for further possible completions of the same
  string).  It returns t if a new completion is found, nil otherwise."
    (if (not old)
        (progn
          (he-init-string (he-lisp-symbol-beg) (point))
          (if (not (he-string-member he-search-string he-tried-table))
              (setq he-tried-table (cons he-search-string he-tried-table)))
          (setq he-expand-list
                (and (not (equal he-search-string ""))
                     (he-flexible-abbrev-collect he-search-string)))))
    (while (and he-expand-list
                (he-string-member (car he-expand-list) he-tried-table))
      (setq he-expand-list (cdr he-expand-list)))
    (if (null he-expand-list)
        (progn
          (if old (he-reset-string))
          ())
      (progn
	(he-substitute-string (car he-expand-list))
	(setq he-expand-list (cdr he-expand-list))
	t)))

  (defun he-flexible-abbrev-collect (str)
    "Find and collect all words that flex-matches STR.
  See docstring for `try-expand-flexible-abbrev' for information
  about what flexible matching means in this context."
    (let ((collection nil)
          (regexp (he-flexible-abbrev-create-regexp str)))
      (save-excursion
        (goto-char (point-min))
        (while (search-forward-regexp regexp nil t)
          ;; Is there a better or quicker way than using `thing-at-point'
          ;; here?
          (setq collection (cons (thing-at-point 'word) collection))))
      collection))

  (defun he-flexible-abbrev-create-regexp (str)
    "Generate regexp for flexible matching of STR.
  See docstring for `try-expand-flexible-abbrev' for information
  about what flexible matching means in this context."
    (concat "\\b" (mapconcat (lambda (x) (concat "\\w*" (list x))) str "")
            "\\w*" "\\b"))

  (defun my-try-expand-dabbrev-visible (old)
    (save-excursion (try-expand-dabbrev-visible old)))

  :config
  (setq hippie-expand-try-functions-list
        '(my-yas-hippie-try-expand
          my-try-expand-company
          try-my-dabbrev-substring
          my-try-expand-dabbrev-visible
          try-expand-dabbrev
          try-expand-dabbrev-all-buffers
          try-expand-dabbrev-from-kill
          try-expand-tag
          try-expand-flexible-abbrev
          try-complete-file-name-partially
          try-complete-file-name
          try-expand-all-abbrevs
          try-expand-list
          try-expand-line
          try-expand-line-all-buffers
          try-complete-lisp-symbol-partially
          try-complete-lisp-symbol))

  (bind-key "M-i" #'my-ido-hippie-expand)

  (defadvice he-substitute-string (after he-paredit-fix)
    "remove extra paren when expanding line in paredit"
    (if (and paredit-mode (equal (substring str -1) ")"))
        (progn (backward-delete-char 1) (forward-char)))))

(use-package hl-line
  :commands hl-line-mode
  :bind (("M-o h" . hl-line-mode))
  :config
  (use-package hl-line+))

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :init
  (add-hook 'ibuffer-mode-hook
            #'(lambda ()
                (ibuffer-switch-to-saved-filter-groups "default"))))

(use-package ido
  :demand t
  :defines (ido-cur-item
            ido-require-match
            ido-selected
            ido-final-text
            ido-show-confirm-message)
  :bind (("C-x b" . ido-switch-buffer)
         ("C-x B" . ido-switch-buffer-other-window))
  :preface
  (eval-when-compile
    (defvar ido-require-match)
    (defvar ido-cur-item)
    (defvar ido-show-confirm-message)
    (defvar ido-selected)
    (defvar ido-final-text))

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

  :config
  (ido-mode 'buffer)

  (use-package ido-hacks
    :demand t
    :load-path "site-lisp/ido-hacks"
    :bind ("M-x" . my-ido-hacks-execute-extended-command)
    :config
    (ido-hacks-mode 1)

    (defvar ido-hacks-completing-read (symbol-function 'completing-read))
    (fset 'completing-read ido-hacks-orgin-completing-read-function)
    (defun my-ido-hacks-execute-extended-command (&optional arg)
      (interactive "P")
      (flet ((completing-read
              (prompt collection &optional predicate require-match
                      initial-input hist def inherit-input-method)
              (funcall ido-hacks-completing-read
                       prompt collection predicate require-match
                       initial-input hist def inherit-input-method)))
        (ido-hacks-execute-extended-command arg))))

  (use-package flx-ido
    :disabled t
    :load-path "site-lisp/flx"
    :config
    (flx-ido-mode 1))

  (add-hook 'ido-minibuffer-setup-hook
            #'(lambda ()
                (bind-key "<return>" #'ido-smart-select-text
                          ido-file-completion-map))))

(use-package ielm
  :bind ("C-c :" . ielm)
  :config
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
               (bind-key "<return>" #'my-ielm-return ielm-map)))
            t))

(use-package iflipb
  :load-path "site-lisp/iflipb"
  :commands (iflipb-next-buffer iflipb-previous-buffer)
  :bind (("C-<tab>" . my-iflipb-next-buffer)
         ("C-S-<iso-lefttab>" . my-iflipb-previous-buffer))
  :preface
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
    (setq my-iflipb-ing-internal t))
  :init (bind-key "C-<escape>" 'mode-line-other-buffer)
  :config
  (setq iflipb-always-ignore-buffers
        "\\`\\( \\|diary\\|ipa\\|\\.newsrc-dribble\\'\\)"
        iflipb-wrap-around t)

  (defun iflipb-first-iflipb-buffer-switch-command ()
    (not (and (or (eq last-command 'my-iflipb-next-buffer)
                  (eq last-command 'my-iflipb-previous-buffer))
              my-iflipb-ing-internal))))

(use-package image-file
  :disabled t
  :config
  (auto-image-file-mode 1))

(use-package info
  :bind ("C-h C-i" . info-lookup-symbol)
  :init
  (remove-hook 'menu-bar-update-hook 'mac-setup-help-topics)
  :config
  (defadvice Info-exit (after remove-info-window activate)
    "When info mode is quit, remove the window."
    (if (> (length (window-list)) 1)
        (delete-window))))

(use-package info-look
  :commands info-lookup-add-help)

(use-package indirect
  :bind ("C-c C" . indirect-region))

(use-package cus-edit
  :load-path "lisp/initsplit"
  :defer 5
  :config
  (use-package initsplit))

(use-package ipa
  :load-path "site-lisp/ipa-el"
  :commands (ipa-insert ipa-load-annotations-into-buffer)
  :init
  (add-hook 'find-file-hook 'ipa-load-annotations-into-buffer))

(use-package irfc
  :init
  (setq-default irfc-directory (concat user-data-directory "RFC")
                irfc-assoc-mode t)
  (defun irfc-index ()
    (interactive)
    (defvar dkh-rfc-index-file (concat irfc-directory "/rfc0000.txt" ))
    (defvar dkh-rfc-index-url "https://www.ietf.org/download/rfc-index.txt")
    (unless (file-exists-p dkh-rfc-index-file)
      (url-copy-file dkh-rfc-index-url dkh-rfc-index-file))
    (find-file dkh-rfc-index-file))
  :config
  (bind-keys :map irfc-mode-map
             ("SPC" . scroll-up)
             ("S-SPC" . scroll-down)
             ("j" . next-line)
             ("k" . previous-line)
             ("h" . backward-char)
             ("l" . forward-char)
             ("J" . irfc-scroll-up-one-line)
             ("K" . irfc-scroll-down-one-line)
             ("G" . end-of-buffer)
             ("g" . beginning-of-buffer)
             ("T" . irfc-render-toggle)
             ("q" . irfc-quit)
             ("o" . irfc-follow)
             ("v" . irfc-visit)
             ("i" . irfc-index)
             ("r" . irfc-reference-goto)
             ("f" . irfc-head-goto)
             ("F" . irfc-head-number-goto)
             ("e" . irfc-page-goto)
             ("n" . irfc-page-next)
             ("p" . irfc-page-prev)
             (">" . irfc-page-last)
             ("<" . irfc-page-first)
             ("t" . irfc-page-table)
             ("H" . irfc-head-next)
             ("L" . irfc-head-prev)
             ("RET" . irfc-table-jump)
             ("<tab>" . irfc-rfc-link-next)
             ("<backtab>" . irfc-rfc-link-prev))
  (when (when-feature-loaded 'hydra)
    (bind-keys :map irfc-mode-map
             ("\\" . hydra-irfc/body))
    (defhydra hydra-irfc (:hint nil :color blue)
          "
                                                                            ╭──────┐
      Move     Scroll   Page  Heads    Links      TOC           Do          │ iRFC │
    ╭───────────────────────────────────────────────────────────────────────┴──────╯
          ^_g_^     _S-SPC_    _<_     ^ ^ ^ ^        ^ ^       [_t_] TOC       [_v_] visit RFC
          ^^↑^^       ^↑^      ^↑^     ^ ^ ^ ^        ^ ^       [_RET_] node    [_i_] index
          ^_k_^       _K_      _p_     ^ _L_ ^    _<backtab>_    ^ ^            [_r_] reference
          ^^↑^^       ^↑^      ^↑^     ^ ^↑^ ^        ^↑^        ^ ^            [_T_] toggle
      _h_ ←   → _l_   ^ ^      _e_     _f_/_F_        _o_                       [_q_] quit
          ^^↓^^       ^↓^      ^↓^     ^ ^↓^ ^        ^↓^
          ^_j_^       _J_      _n_     ^ _H_ ^      _<tab>_
          ^^↓^^       ^↓^      ^↓^     ^ ^ ^ ^        ^ ^
          ^_G_^      _SPC_     _>_     ^ ^ ^ ^        ^ ^
    --------------------------------------------------------------------------------
          "
          ("\\" hydra-master/body "back")
          ("<escape>" nil "quit")
             ("SPC" scroll-up)
             ("S-SPC" scroll-down)
             ("j" next-line)
             ("k" previous-line)
             ("h" backward-char)
             ("l" forward-char)
             ("J" irfc-scroll-up-one-line)
             ("K" irfc-scroll-down-one-line)
             ("G" end-of-buffer)
             ("g" beginning-of-buffer)
             ("T" irfc-render-toggle)
             ("q" irfc-quit)
             ("o" irfc-follow)
             ("v" irfc-visit)
             ("i" irfc-index)
             ("r" irfc-reference-goto)
             ("f" irfc-head-goto)
             ("F" irfc-head-number-goto)
             ("e" irfc-page-goto)
             ("n" irfc-page-next)
             ("p" irfc-page-prev)
             (">" irfc-page-last)
             ("<" irfc-page-first)
             ("t" irfc-page-table)
             ("H" irfc-head-next)
             ("L" irfc-head-prev)
             ("RET" irfc-table-jump)
             ("<tab>" irfc-rfc-link-next)
             ("<backtab>" irfc-rfc-link-prev))))

(use-package isearch
  :no-require t
  :bind (("C-M-r" . isearch-backward-other-window)
         ("C-M-s" . isearch-forward-other-window))
  :preface
  (defun isearch-backward-other-window ()
    (interactive)
    (split-window-vertically)
    (call-interactively 'isearch-backward))

  (defun isearch-forward-other-window ()
    (interactive)
    (split-window-vertically)
    (call-interactively 'isearch-forward))

  :config
  (bind-key "C-c" #'isearch-toggle-case-fold isearch-mode-map)
  (bind-key "C-t" #'isearch-toggle-regexp isearch-mode-map)
  (bind-key "C-^" #'isearch-edit-string isearch-mode-map)
  (bind-key "C-i" #'isearch-complete isearch-mode-map))

(use-package js2-mode
  :load-path "site-lisp/js2-mode"
  :mode "\\.js\\'")

(use-package json-mode
  :load-path ("site-lisp/json-mode"
              "site-lisp/json-reformat"
              "site-lisp/json-snatcher")
  :mode "\\.json\\'")

(use-package know-your-http-well
  :load-path "site-lisp/know-your-http-well/emacs")

(use-package ledger-mode
  :load-path "~/src/ledger/lisp"
  :commands ledger-mode
  :bind ("C-c L" . my-ledger-start-entry)
  :preface
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
        (forward-line)))))

(use-package lisp-mode
  :defer t
  :preface
  (defface esk-paren-face
    '((((class color) (background dark))
       (:foreground "grey50"))
      (((class color) (background light))
       (:foreground "grey55")))
    "Face used to dim parentheses."
    :group 'starter-kit-faces)

  (defvar slime-mode nil)
  (defvar lisp-mode-initialized nil)

  (defun my-lisp-mode-hook ()
    (unless lisp-mode-initialized
      (setq lisp-mode-initialized t)

      (use-package redshank
        :diminish redshank-mode)

      (use-package elisp-slime-nav
        :load-path "site-lisp/elisp-slime-nav"
        :diminish elisp-slime-nav-mode)

      (use-package edebug)

      (use-package eldoc
        :diminish eldoc-mode
        :commands eldoc-mode
        :config
        (use-package eldoc-extension
          :disabled t
          :defer t
          :init
          (add-hook 'emacs-lisp-mode-hook
                    #'(lambda () (require 'eldoc-extension)) t))
        (eldoc-add-command 'paredit-backward-delete
                           'paredit-close-round))

      (use-package cldoc
        :commands (cldoc-mode turn-on-cldoc-mode)
        :diminish cldoc-mode)

      (use-package ert
        :bind ("C-c e t" . ert-run-tests-interactively))

      (use-package elint
        :commands 'elint-initialize
        :preface
        (defun elint-current-buffer ()
          (interactive)
          (elint-initialize)
          (elint-current-buffer))

        :config
        (add-to-list 'elint-standard-variables 'current-prefix-arg)
        (add-to-list 'elint-standard-variables 'command-line-args-left)
        (add-to-list 'elint-standard-variables 'buffer-file-coding-system)
        (add-to-list 'elint-standard-variables 'emacs-major-version)
        (add-to-list 'elint-standard-variables 'window-system))

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

      (use-package info-lookmore
        :load-path "site-lisp/info-lookmore"
        :config
        (info-lookmore-elisp-cl)
        (info-lookmore-elisp-userlast)
        (info-lookmore-elisp-gnus)
        (info-lookmore-apropos-elisp))

      (use-package testcover
        :commands testcover-this-defun)

      (mapc (lambda (mode)
              (info-lookup-add-help
               :mode mode
               :regexp "[^][()'\" \t\n]+"
               :ignore-case t
               :doc-spec '(("(ansicl)Symbol Index" nil nil nil))))
            lisp-modes))

    (auto-fill-mode 1)
    (paredit-mode 1)
    (redshank-mode 1)
    (elisp-slime-nav-mode 1)

    (local-set-key (kbd "<return>") 'paredit-newline)

    (add-hook 'after-save-hook 'check-parens nil t)

    (unless (memq major-mode
                  '(emacs-lisp-mode inferior-emacs-lisp-mode ielm-mode))
      (turn-on-cldoc-mode)
      (bind-key "M-q" #'slime-reindent-defun lisp-mode-map)
      (bind-key "M-l" #'slime-selector lisp-mode-map)))

  ;; Change lambda to an actual lambda symbol
  :init
  (mapc
   (lambda (major-mode)
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

  (apply #'hook-into-modes 'my-lisp-mode-hook lisp-mode-hooks))

(use-package llvm-mode
  :mode "\\.ll\\'")

(use-package lua-mode
  :load-path "site-lisp/lua-mode"
  :mode "\\.lua\\'"
  :interpreter ("lua" . lua-mode))

(use-package lusty-explorer
  :demand t
  :load-path "site-lisp/lusty-emacs"
  :bind ("C-x C-f" . my-lusty-file-explorer)
  :preface
  (defun lusty-read-directory ()
    "Launch the file/directory mode of LustyExplorer."
    (interactive)
    (require 'lusty-explorer)
    (let ((lusty--active-mode :file-explorer))
      (lusty--define-mode-map)
      (let* ((lusty--ignored-extensions-regex
              (concat "\\(?:" (regexp-opt completion-ignored-extensions)
                      "\\)$"))
             (minibuffer-local-filename-completion-map lusty-mode-map)
             (lusty-only-directories t))
        (lusty--run 'read-directory-name default-directory ""))))

  (defun lusty-read-file-name ()
    "Launch the file/directory mode of LustyExplorer."
    (interactive)
    (require 'lusty-explorer)
    (let ((lusty--active-mode :file-explorer))
      (lusty--define-mode-map)
      (let* ((lusty--ignored-extensions-regex
              (concat "\\(?:" (regexp-opt completion-ignored-extensions)
                      "\\)$"))
             (minibuffer-local-filename-completion-map lusty-mode-map)
             (lusty-only-directories nil))
        (lusty--run 'read-file-name default-directory ""))))

  (defun my-lusty-file-explorer ()
    "Launch the file/directory mode of LustyExplorer."
    (interactive)
    (require 'lusty-explorer)
    (let ((lusty--active-mode :file-explorer)
          (helm-mode-prev (and (boundp 'helm-mode) helm-mode)))
      (if (fboundp 'helm-mode)
          (helm-mode -1))
      (unwind-protect
          (progn
            (lusty--define-mode-map)
            (let* ((lusty--ignored-extensions-regex
                    (concat "\\(?:" (regexp-opt
                                     completion-ignored-extensions) "\\)$"))
                   (minibuffer-local-filename-completion-map lusty-mode-map)
                   (file
                    ;; read-file-name is silly in that if the result is equal
                    ;; to the dir argument, it gets converted to the
                    ;; default-filename argument.  Set it explicitly to "" so
                    ;; if lusty-launch-dired is called in the directory we
                    ;; start at, the result is that directory instead of the
                    ;; name of the current buffer.
                    (lusty--run 'read-file-name default-directory "")))
              (when file
                (switch-to-buffer
                 (find-file-noselect
                  (expand-file-name file))))))
        (if (fboundp 'helm-mode)
            (helm-mode (if helm-mode-prev 1 -1))))))

  :config
  (defun my-lusty-setup-hook ()
    (bind-key "SPC" #'lusty-select-match lusty-mode-map)
    (bind-key "C-d" #'exit-minibuffer lusty-mode-map))

  (add-hook 'lusty-setup-hook 'my-lusty-setup-hook)

  (defun lusty-open-this ()
    "Open the given file/directory/buffer, creating it if not
    already present."
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
        (lusty-sort-by-fuzzy-score filtered file-portion)))))

(use-package macrostep
  :load-path "site-lisp/macrostep"
  :bind ("C-c e m" . macrostep-expand))

(use-package magit
  :load-path "site-lisp/magit/lisp"
  :bind (("C-x g" . magit-status)
         ("C-x G" . magit-status-with-prefix))
  :preface
  (defun magit-monitor (&optional no-display)
    "Start git-monitor in the current directory."
    (interactive)
    (when (string-match "\\*magit: \\(.+?\\)\\*" (buffer-name))
      (let ((name (format "*git-monitor: %s*"
                          (match-string 1 (buffer-name)))))
        (or (get-buffer name)
            (let ((buf (get-buffer-create name)))
              (ignore-errors
                (start-process "*git-monitor*" buf "git-monitor"
                               "-d" (expand-file-name default-directory)))
              buf)))))

  (defun magit-status-with-prefix ()
    (interactive)
    (let ((current-prefix-arg '(4)))
      (call-interactively 'magit-status)))

  (defun lusty-magit-status (dir &optional switch-function)
    (interactive (list (if current-prefix-arg
                           (lusty-read-directory)
                         (or (magit-get-top-dir)
                             (lusty-read-directory)))))
    (magit-status-internal dir switch-function))

  :init
  (add-hook 'magit-mode-hook 'hl-line-mode)

  :config
  (setenv "GIT_PAGER" "")

  (use-package magit-backup
    :disabled t
    :commands magit-backup-mode
    :config
    (magit-backup-mode -1))

  (use-package magit-commit
    :config
    (remove-hook 'server-switch-hook 'magit-commit-diff))

  (unbind-key "M-h" magit-mode-map)
  (unbind-key "M-s" magit-mode-map)
  (unbind-key "M-m" magit-mode-map)
  (unbind-key "M-w" magit-mode-map)

  ;; (bind-key "M-H" #'magit-show-level-2-all magit-mode-map)
  ;; (bind-key "M-S" #'magit-show-level-4-all magit-mode-map)
  (bind-key "U" #'magit-unstage-all magit-mode-map)

  (add-hook 'magit-log-edit-mode-hook
            #'(lambda ()
                (set-fill-column 72)
                (flyspell-mode)))

  (add-hook 'magit-status-mode-hook #'(lambda () (magit-monitor t))))

(use-package markdown-mode
  :load-path "site-lisp/markdown-mode"
  :mode (("\\`README\\.md\\'" . gfm-mode)
         ("\\.md\\'"          . markdown-mode)
         ("\\.markdown\\'"    . markdown-mode)))

(use-package midnight
  :defer 10)

(use-package mudel
  :disabled t
  :commands mudel
  :bind ("C-c M" . mud)
  :init
  (defun mud ()
    (interactive)
    (mudel "4dimensions" "4dimensions.org" 6000)))

(use-package mule
  :no-require t
  :defines x-select-request-type
  :config
  (prefer-coding-system 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

(use-package multi-term
  :disabled t
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
  (defalias 'my-term-send-raw-at-prompt 'term-send-raw)

  (defun my-term-end-of-buffer ()
    (interactive)
    (call-interactively #'end-of-buffer)
    (if (and (eobp) (bolp))
        (delete-char -1)))

  (require 'term)

  (defadvice term-process-pager (after term-process-rebind-keys activate)
    (define-key term-pager-break-map  "\177" 'term-pager-back-page)))

(use-package multiple-cursors
  :load-path "site-lisp/multiple-cursors-el"
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->"         . mc/mark-next-like-this)
         ("C-<"         . mc/mark-previous-like-this)
         ("C-c C-<"     . mc/mark-all-like-this))
  :config
  (setq mc/list-file (expand-file-name "mc-lists.el" user-data-directory)))

(use-package nf-procmail-mode
  :commands nf-procmail-mode)

(use-package nlinum
  :preface
  (defun goto-line-with-feedback ()
    "Show line numbers temporarily, while prompting for the line number input"
    (interactive)
    (unwind-protect
        (progn
          (nlinum-mode 1)
          (goto-char (point-min))
          (forward-line (read-number "Goto line: ")))
      (nlinum-mode -1)))

  :init
  (bind-key "C-c g" #'goto-line)
  (global-set-key [remap goto-line] 'goto-line-with-feedback))

(use-package nroff-mode
  :commands nroff-mode
  :config
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
                (add-hook 'after-save-hook 'update-nroff-timestamp nil t))))

(use-package nxml-mode
  :commands nxml-mode
  :init
  (defalias 'xml-mode 'nxml-mode)
  :config
  (defun my-nxml-mode-hook ()
    (bind-key "<return>" 'newline-and-indent nxml-mode-map))

  (add-hook 'nxml-mode-hook 'my-nxml-mode-hook)

  (defun tidy-xml-buffer ()
    (interactive)
    (save-excursion
      (call-process-region (point-min) (point-max) "tidy" t t nil
                           "-xml" "-i" "-wrap" "0" "-omit" "-q" "-utf8")))

  (bind-key "C-c M-h" #'tidy-xml-buffer nxml-mode-map))

(use-package on-screen
  :disabled t
  :load-path "site-lisp/on-screen"
  :defer 5
  :config
  (on-screen-global-mode 1))

(use-package osx-trash
  :load-path "site-lisp/osx-trash"
  :config
  (when (eq system-type 'darwin)
    (osx-trash-setup))
  (setq delete-by-moving-to-trash t))

(use-package ox-reveal
  :load-path "site-lisp/org-reveal")

(use-package pabbrev
  :load-path "site-lisp/pabbrev"
  :commands pabbrev-mode
  :diminish pabbrev-mode)

(use-package spinner
  :load-path "site-lisp/spinner"
  :defer 5
  :config
  (use-package paradox
    :load-path "site-lisp/paradox"
    :commands paradox-list-packages
    :config
    (setq paradox-github-token t
          paradox-automatically-star nil
          paradox-execute-asynchronously t)))

(use-package paredit
  :commands paredit-mode
  :diminish paredit-mode
  :config
  (use-package paredit-ext)

  (bind-key "C-M-l" #'paredit-recentre-on-sexp paredit-mode-map)

  (bind-key ")" #'paredit-close-round-and-newline paredit-mode-map)
  (bind-key "M-)" #'paredit-close-round paredit-mode-map)

  (bind-key "M-k" #'paredit-raise-sexp paredit-mode-map)
  (bind-key "M-I" #'paredit-splice-sexp paredit-mode-map)

  (unbind-key "M-r" paredit-mode-map)
  (unbind-key "M-s" paredit-mode-map)

  (bind-key "C-. D" #'paredit-forward-down paredit-mode-map)
  (bind-key "C-. B" #'paredit-splice-sexp-killing-backward paredit-mode-map)
  (bind-key "C-. C" #'paredit-convolute-sexp paredit-mode-map)
  (bind-key "C-. F" #'paredit-splice-sexp-killing-forward paredit-mode-map)
  (bind-key "C-. a" #'paredit-add-to-next-list paredit-mode-map)
  (bind-key "C-. A" #'paredit-add-to-previous-list paredit-mode-map)
  (bind-key "C-. j" #'paredit-join-with-next-list paredit-mode-map)
  (bind-key "C-. J" #'paredit-join-with-previous-list paredit-mode-map))

(or (use-package mic-paren
      :defer 5
      :config
      (paren-activate))
    (use-package paren
      :defer 5
      :config
      (show-paren-mode 1)))

(use-package per-window-point
  :commands pwp-mode
  :defer 5
  :config
  (pwp-mode 1))

(use-package pdf-tools
  :load-path "site-lisp/pdf-tools"
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-page
                pdf-view-use-imagemagick t)
  (bind-keys :map pdf-view-mode-map
      ("\\" . hydra-pdftools/body)
      ("<s-spc>" .  pdf-view-scroll-down-or-next-page)
      ("g"  . pdf-view-first-page)
      ("G"  . pdf-view-last-page)
      ("l"  . image-forward-hscroll)
      ("h"  . image-backward-hscroll)
      ("j"  . pdf-view-next-line-or-next-page)
      ("k"  . pdf-view-previous-line-or-previous-page)
      ("e"  . pdf-view-goto-page)
      ("t"  . pdf-view-goto-label)
      ("u"  . pdf-view-revert-buffer)
      ("al" . pdf-annot-list-annotations)
      ("ad" . pdf-annot-delete)
      ("aa" . pdf-annot-attachment-dired)
      ("am" . pdf-annot-add-markup-annotation)
      ("at" . pdf-annot-add-text-annotation)
      ("y"  . pdf-view-kill-ring-save)
      ("i"  . pdf-misc-display-metadata)
      ("s"  . pdf-occur)
      ("b"  . pdf-view-set-slice-from-bounding-box)
      ("r"  . pdf-view-reset-slice))

  (when (when-feature-loaded 'hydra)
    (bind-keys :map pdf-view-mode-map
               ("\\" . hydra-pdftools/body))
    (defhydra hydra-pdftools (:color blue :hint nil)
        "
                                                                      ╭───────────┐
       Move  History   Scale/Fit     Annotations  Search/Link    Do   │ PDF Tools │
   ╭──────────────────────────────────────────────────────────────────┴───────────╯
      ^^^_g_^^^       _B_    ^↧^    _+_    ^ ^     [_al_] list    [_s_] search    [_u_] revert buffer
      ^^^^↑^^^^       ^↑^    _H_    ^↑^  ↦ _W_ ↤   [_am_] markup  [_o_] outline   [_i_] info
      ^^^_p_^^^       ^ ^    ^↥^    _0_    ^ ^     [_at_] text    [_F_] link      [_d_] dark mode
      ^^^^↑^^^^       ^↓^  ╭─^─^─┐  ^↓^  ╭─^ ^─┐   [_ad_] delete  [_f_] search link
 _h_ ← _e_/_t_ → _l_  _N_  │ _P_ │  _-_    _b_     [_aa_] dired
      ^^^^↓^^^^       ^ ^  ╰─^─^─╯  ^ ^  ╰─^ ^─╯   [_y_]  yank
      ^^^_n_^^^       ^ ^  _r_eset slice box
      ^^^^↓^^^^
      ^^^_G_^^^
   --------------------------------------------------------------------------------
        "
        ("\\" hydra-master/body "back")
        ("<ESC>" nil "quit")
        ("al" pdf-annot-list-annotations)
        ("ad" pdf-annot-delete)
        ("aa" pdf-annot-attachment-dired)
        ("am" pdf-annot-add-markup-annotation)
        ("at" pdf-annot-add-text-annotation)
        ("y"  pdf-view-kill-ring-save)
        ("+" pdf-view-enlarge :color red)
        ("-" pdf-view-shrink :color red)
        ("0" pdf-view-scale-reset)
        ("H" pdf-view-fit-height-to-window)
        ("W" pdf-view-fit-width-to-window)
        ("P" pdf-view-fit-page-to-window)
        ("n" pdf-view-next-page-command :color red)
        ("p" pdf-view-previous-page-command :color red)
        ("d" pdf-view-dark-minor-mode)
        ("b" pdf-view-set-slice-from-bounding-box)
        ("r" pdf-view-reset-slice)
        ("g" pdf-view-first-page)
        ("G" pdf-view-last-page)
        ("e" pdf-view-goto-page)
        ("t" pdf-view-goto-label)
        ("o" pdf-outline)
        ("s" pdf-occur)
        ("i" pdf-misc-display-metadata)
        ("u" pdf-view-revert-buffer)
        ("F" pdf-links-action-perfom)
        ("f" pdf-links-isearch-link)
        ("B" pdf-history-backward :color red)
        ("N" pdf-history-forward :color red)
        ("l" image-forward-hscroll :color red)
        ("h" image-backward-hscroll :color red)))

   (use-package org-pdfview
     :load-path "site-lisp/org-pdfview"))

(use-package persistent-scratch
  :if (and window-system (not running-alternate-emacs)
           (not noninteractive)))

(use-package perspective
  :disabled t
  :config
  (use-package persp-projectile)
  (persp-mode))

(use-package web-mode
  :mode "\\.phtml\\|\\.tpl\\.php\\|\\.blade\\.php\\|\\.jsp\\|\\.as[cp]x\\|\\.erb\\|\\.html?\\|/\\(views\\|html\\|theme\\|templates\\)"
  :init
  (progn
    (add-hook 'web-mode-before-auto-complete-hooks
              '(lambda ()
                 (let ((web-mode-cur-language
                        (web-mode-language-at-pos)))
                   (if (string= web-mode-cur-language "php")
                       (yas-activate-extra-mode 'php-mode)
                     (yas-deactivate-extra-mode 'php-mode))
                   (if (string= web-mode-cur-language "css")
                       (setq emmet-use-css-transform t)
                     (setq emmet-use-css-transform nil)))))
    (defun indent-and-newline ()
      "Indent and newline"
      (interactive)
      (progn (web-mode-indent-line)
             (newline-and-indent)))
    (defun web-mode-hook ()
      "Hooks for Web mode."
      ;;indent
      (setq web-mode-attr-indent-offset 2)
      (setq web-mode-markup-indent-offset 2)
      (setq web-mode-css-indent-offset 2)
      (setq web-mode-code-indent-offset 2)

      (setq web-mode-enable-auto-pairing t)
      (setq web-mode-enable-css-colorization t)

      (setq web-mode-enable-comment-keywords t)
      (setq web-mode-enable-current-column-highlight t)
      (setq web-mode-enable-current-element-highlight t)
      (setq web-mode-enable-element-tag-fontification t)

      (local-set-key (kbd "RET") 'indent-and-newline))
    (add-hook 'web-mode-hook  'web-mode-hook)

    (setq web-mode-ac-sources-alist
          '(("php" . (ac-source-yasnippet ac-source-php-auto-yasnippets))
            ("html" . (ac-source-emmet-html-aliases ac-source-emmet-html-snippets))
            ("css" . (ac-source-css-property ac-source-emmet-css-snippets))))

    (add-hook 'web-mode-before-auto-complete-hooks
              '(lambda ()
                 (let ((web-mode-cur-language
                        (web-mode-language-at-pos)))
                   (if (string= web-mode-cur-language "php")
                       (yas-activate-extra-mode 'php-mode)
                     (yas-deactivate-extra-mode 'php-mode))
                   (if (string= web-mode-cur-language "css")
                       (setq emmet-use-css-transform t)
                     (setq emmet-use-css-transform nil)))))))

(use-package php-mode
  :load-path "site-lisp/php-mode"
  :commands php-mode
  :mode "\\.inc$\\|\\.\\(module\\|test\\|install\\|theme\\|\\profile\\)$"
  :interpreter "php"
  :init
  (use-package conf-mode
    :mode "\\.info")
  (use-package php-auto-yasnippets
    :load-path "site-lisp/php-auto-yasnippets")
  :config
  (defun my-php-return ()
    "Advanced C-m for PHP doc multiline comments.
Inserts `*' at the beggining of the new line if
unless return was pressed outside the comment"
    (interactive)
    (setq last (point))
    (setq is-inside
          (if (search-backward "*/" nil t)
              ;; there are some comment endings - search forward
              (if (search-forward "/*" last t)
                  't
                'nil)
            ;; it's the only comment - search backward
            (goto-char last)
            (if (search-backward "/*" nil t)
                't
              'nil)))
    ;; go to last char position
    (goto-char last)
    ;; the point is inside some comment, insert `*'
    (if is-inside
        (progn
          (insert "\n*")
          (indent-for-tab-command))
      ;; else insert only new-line
      (insert "\n")))

  (defun my-php-indent-or-complete ()
    (interactive)
    (let (
          (call-interactively 'indent-according-to-mode)
          (call-interactively 'php-complete-function))))

  (defun php-show-arglist ()
    (interactive)
    (let* ((tagname (php-get-pattern))
           (buf (find-tag-noselect tagname nil nil))
           arglist)
      (with-current-buffer buf
        (goto-char (point-min))
        (when (re-search-forward
               (format "function\\s-+%s\\s-*(\\([^{]*\\))" tagname)
               nil t)
          (setq arglist (buffer-substring-no-properties
                         (match-beginning 1) (match-end 1)))))
      (if arglist
          (message "Arglist for %s: %s" tagname arglist)
        (message "Unknown function: %s" tagname))))

  (defun my-php-mode-hook ()
    (set (make-local-variable 'yas-fallback-behavior)
         '(apply my-php-indent-or-complete . nil))
    (bind-key "<tab>" 'yas-expand-from-trigger-key php-mode-map)
    (unbind-key "C-." php-mode-map))

  (add-hook 'php-mode-hook
            '(lambda ()
               (drupal-mode-bootstrap)
               (define-abbrev php-mode-abbrev-table "ex" "extends")
               (hs-minor-mode 1)
               (turn-on-eldoc-mode)
               (diminish 'hs-minor-mode)
               (helm-gtags-mode 1)
               (setq indicate-empty-lines t)
               'my-php-mode-hook
               (local-set-key "\r" 'my-php-return)
               (local-unset-key (kbd "C-c ."))
               ;; (paren-toggle-open-paren-context 1)
               (which-function-mode 1)))

  (bind-key "C-c C-F" 'php-search-local-documentation)

  (use-package php-boris
    :load-path "site-lisp/php-boris"
    :init
    (use-package highlight
      :load-path "site-lisp/highlight")
    (use-package php-boris-minor-mode
      :load-path "site-lisp/php-boris-minor-mode"))

  (use-package php-align
    :load-path "site-lisp/emacs-php-align"
    :config
    (php-align-setup))

  (use-package php-eldoc
    :load-path "site-lisp/php-eldoc"
    :disabled t
    :config
    (let ((manual "~/Documents/php/php-chunked-xhtml"))
      (when (file-readable-p manual)
        (setq php-manual-path manual)))

    (defun my-php-completion-at-point ()
      "Provide php completions for completion-at-point.
Relies on functions of `php-mode'."
      (let ((pattern (php-get-pattern)))
        (when pattern
          (list (- (point) (length pattern))
                (point)
                (or php-completion-table
                    (php-completion-table))
                :exclusive 'no))))

    (add-hook 'completion-at-point-functions 'my-php-completion-at-point nil t)
    (set (make-local-variable 'company-backends)
         '((company-capf :with company-dabbrev-code)))
    (set (make-local-variable 'electric-indent-mode) nil)
    (php-eldoc-enable))

  (defun var_dump-die ()
    (interactive)
    (let ((expression (if (region-active-p)
                          (buffer-substring (region-beginning) (region-end))
                        (sexp-at-point)))
          (line (thing-at-point 'line))
          (pre "die(var_dump(")
          (post "));"))
      (if expression
          (progn
            (beginning-of-line)
            (if (string-match "return" line)
                (progn
                  (newline)
                  (previous-line))
              (next-line)
              (newline)
              (previous-line))
            (insert pre)
            (insert (format "%s" expression))
            (insert post))
        ()
        (insert pre)
        (insert post)
        (backward-char (length post)))))

  (defun var_dump ()
    (interactive)
    (if (region-active-p)
        (progn
          (goto-char (region-end))
          (insert ");")
          (goto-char (region-beginning))
          (insert "var_dump("))
      (insert "var_dump();")
      (backward-char 3)))
  ;; pman
  ;; sudo pear channel-update doc.php.net
  ;; sudo pear install doc.php.net/pman

  (defun describe-function-via-pman (function)
    "Display the full documentation of FUNCTION, using pman"
    (interactive
     (let (
           (fn (intern-soft (thing-at-point 'symbol)
                            obarray))
           (enable-recursive-minibuffers t)
           val)
       (setq val (completing-read (if fn
                                      (format "Describe function (default %s): " fn)
                                    "Describe function: ")
                                  obarray 'fboundp t nil nil
                                  (and fn (symbol-name fn))))
       (list (if (equal val "")
                 fn (intern val)))))
    (if (null function)
        (message "You didn't specify a function")
      (help-setup-xref (list #'describe-function function)
                       (called-interactively-p 'interactive))
      (save-excursion
        (let ((manual-program "pman"))
          (man (symbol-name function))))))

  (define-key php-mode-map "\C-hS" 'describe-function-via-pman)
  (define-key php-mode-map (kbd "C-c C-y") 'yas/create-php-snippet)

  (use-package geben
    :commands (geben my-php-debug)
    :config
    (progn

      ;; Debug a simple PHP script.
      (defun my-php-debug ()
        "Run current PHP script for debugging with geben"
        (interactive)
        (call-interactively 'geben)
        (shell-command
         (concat
          "XDEBUG_CONFIG='idekey=my-php-54'  php "
          (buffer-file-name) " status" " &")))

      ;; geben won't connect because its "Already in debugging"  This might help.
      (defun my-geben-release ()
        (interactive)
        (geben-stop)
        (dolist (session geben-sessions)
          (ignore-errors
            (geben-session-release session))))

      (use-package my-geben)))

  (add-hook 'prog-mode-hook (lambda () (interactive) (setq show-trailing-whitespace 1))))

(use-package popup-ruler
  :bind (("C-. r" . popup-ruler)
         ("C-. R" . popup-ruler-vertical)))

(use-package popwin
  :load-path "lib/popwin-el"
  :config
  (popwin-mode 1)
  (setq popwin:popup-window-height 35
        popwin:special-display-config
        '(("*Miniedit Help*" :noselect t)
          (help-mode :noselect nil)
          (completion-list-mode :noselect t)
          (compilation-mode :noselect nil)
          (grep-mode :noselect t)
          (occur-mode :noselect t)
          ("*Pp Macroexpand Output*" :noselect t)
          ("*Shell Command Output*")
          ("*Async Shell Command*")
          ("*vc-diff*")
          ("*vc-change-log*")
          (" *undo-tree*" :width 60 :position right)
          ("^\\*anything.*\\*$" :regexp t)
          ("*slime-apropos*")
          ("*slime-macroexpansion*")
          ("*slime-description*")
          ("*slime-compilation*" :noselect t)
          ("*slime-xref*")
          ("*Flycheck errors*")
          ("*Warnings*")
          ("*Process List*")
          ("*Smex: Unbound Commands*")
          ("*Paradox Report*" :noselect nil)
          ("*Diff*" :noselect nil)
          ("*Messages*" :noselect nil)
          ("*Google Maps*" :noselect nil)
          ("*ag search*" :noselect nil)
          ("*PDF-Occur*" :noselect nil)
          ("*PDF-Metadata*" :noselect nil)
          ("^\\*Outline .*\\.pdf\\*$" :regexp t :noselect nil)
          ("*MULTI-TERM-DEDICATED*" :noselect nil :stick t)
          (sldb-mode :stick t)
          (slime-repl-mode)
          (slime-connection-list-mode)))

  ;; (add-hook 'popwin:after-popup-hook 'turn-off-evil-mode)
  (bind-keys :map popwin:window-map
             ((kbd "<escape>") . popwin:close-popup-window)))

(use-package powerline
  :load-path "site-lisp/powerline/"
  :disabled t
  :config
  (powerline-default-theme)
  (setq powerline-default-separator 'utf-8))

(use-package pp-c-l
  :commands pretty-control-l-mode
  :init
  (add-hook 'prog-mode-hook 'pretty-control-l-mode))

(use-package projectile
  :load-path "site-lisp/projectile"
  :diminish projectile-mode
  :commands projectile-global-mode
  :defer 5
  :bind-keymap ("C-c p" . projectile-command-map)
  :init
  (require 'grizzl)
  (projectile-global-mode)

  (use-package projectile-drupal
    :load-path "lisp/projectile-drupal"
    :init
    (progn
      (defun dkh-get-site-name ()
        "Gets site name based on University WWNG standard or standalone."
        (if (locate-dominating-file default-directory
                                    "current")
            (let* ((project-root-dir (locate-dominating-file default-directory
                                                             "current"))
                   (path (split-string project-root-dir "/")))     ; path as list
              (car (last (nbutlast path 1))))
          (projectile-project-name)))

      (defun dkh-get-base-url ()
        "Gets the projectile-drupal-base-url based on University WWNG standard or standalone."
        (let* ((uri
                (if (equal projectile-drupal-site-name "admissions_undergraduate")
                    "ww/admissions/undergraduate"
                  (concat "colorado.dev/" projectile-drupal-site-name))))
          (concat "http://" uri)))

      (bind-key "C-H-M-<" 'projectile-switch-to-prev-buffer)
      (bind-key "C-H-M->" 'projectile-switch-to-next-buffer)
      (bind-key "C-H-M-p" 'revert-buffer)
      (bind-key "C-H-M-\"" 'kill-buffer)))

  (add-hook 'projectile-mode-hook 'projectile-drupal-on)

  (bind-key "<C-H-M-S-escape>" 'projectile-project-buffers-other-buffer)

  (defun projectile-switch-to-last-project ()
    (interactive)
    (funcall projectile-switch-project-action projectile-last-project-root))

  (defun projectile-switch-to-last-project-root-buffer ()
    (interactive)
    (if (boundp 'projectile-last-project-root-buffer)
        (if (buffer-live-p projectile-last-project-root-buffer)
            (switch-to-buffer projectile-last-project-root-buffer)))
    (if (boundp 'projectile-last-project-root)
        (projectile-switch-to-last-project))
    (message "projectile-last-project-root is not defined"))

  (global-set-key (kbd "C-c p B") 'projectile-switch-to-last-project-root-buffer)

  (defun projectile-switch-to-last-project-buffer ()
    (interactive)
    (projectile-switch-to-last-project)
    (projectile-project-buffers-other-buffer))

  (global-set-key (kbd "C-c p B") 'projectile-switch-to-last-project-buffer)

  (defun dkh-project-record ()
    (setq projectile-last-project-root (projectile-project-root))
    (setq projectile-last-project-root-buffer (current-buffer)))

  (global-set-key (kbd "C-c p S") 'projectile-switch-to-last-project)

  (defun dkh-projectile-dired (&optional arg)
    "Open `dired' at the root of the project."
    (interactive)
    (if arg
        (dired arg))
    (dired default-directory))

  (defun buffer-projectile (change-buffer-fun)
    (let ((current-project-root (projectile-project-p))
          (next-project-root nil))
      (while (not (string= next-project-root current-project-root))
        (funcall change-buffer-fun)
        (setq next-project-root (projectile-project-p)))))

  (defun projectile-switch-to-prev-buffer ()
    (interactive)
    (buffer-projectile #'previous-buffer))

  (defun projectile-switch-to-next-buffer ()
    (interactive)
    (buffer-projectile #'next-buffer))

  (global-set-key (kbd "C-c p <left>") 'projectile-switch-to-prev-buffer)
  (global-set-key (kbd "C-c p <right>") 'projectile-switch-to-next-buffer)

  (defun projectile-post-project ()
    "Which project am I actually in?"
    (interactive)
    (message (projectile-project-root)))

  (defun projectile-add-project ()
    "Add folder of current buffer's file to list of projectile projects"
    (interactive)
    (if (buffer-file-name (current-buffer))
        (projectile-add-known-project
         (file-name-directory (buffer-file-name (current-buffer))))))

  (global-set-key (kbd "C-c p w") 'projectile-post-project)
  (global-set-key (kbd "C-c p +") 'projectile-add-project)

  (use-package persp-projectile
    :bind ("C-\\" . projectile-persp-switch-project)
    )

  (use-package helm-projectile
    :config
    (setq projectile-completion-system 'helm)
    (helm-projectile-on)
    (bind-key "M-s P" 'helm-projectile)
    (defun projectile-helm-ag ()
      (interactive)
      (helm-ag (projectile-project-root)))))

(use-package ps-print
  :defer t
  :config
  (defun ps-spool-to-pdf (beg end &rest ignore)
    (interactive "r")
    (let ((temp-file (concat (make-temp-name "ps2pdf") ".pdf")))
      (call-process-region beg end (executable-find "ps2pdf")
                           nil nil nil "-" temp-file)
      (call-process (executable-find "open") nil nil nil temp-file)))

  (setq ps-print-region-function 'ps-spool-to-pdf))

(use-package puppet-mode
  :disabled t
  :mode ("\\.pp\\'" . puppet-mode)
  :config
  (use-package puppet-ext))

(use-package python-mode
  :load-path "site-lisp/python-mode"
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :config
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

    (bind-key "C-c C-z" #'python-shell python-mode-map)
    (unbind-key "C-c c" python-mode-map))

  (add-hook 'python-mode-hook 'my-python-mode-hook))

(use-package quickrun
  :disabled t
  :load-path "site-lisp/emacs-quickrun"
  :bind ("C-c C-r" . quickrun))

(use-package rainbow-delimiters
  :load-path "site-lisp/rainbow-delimiters"
  :commands (rainbow-delimiters-mode)
  :config
    (add-hook 'prog-mode-hook 'rainbow-mode))

(use-package rainbow-mode
  :diminish ((rainbow-mode . "rb"))
  :commands rainbow-mode
  :config
    (add-hook 'prog-mode-hook 'rainbow-mode))

(use-package recentf
  :defer 10
  :commands (recentf-mode
             recentf-add-file
             recentf-apply-filename-handlers)
  :preface
  (defun recentf-add-dired-directory ()
    (if (and dired-directory
             (file-directory-p dired-directory)
             (not (string= "/" dired-directory)))
        (let ((last-idx (1- (length dired-directory))))
          (recentf-add-file
           (if (= ?/ (aref dired-directory last-idx))
               (substring dired-directory 0 last-idx)
             dired-directory)))))
  :init
  (add-hook 'dired-mode-hook 'recentf-add-dired-directory)
  :config
  (recentf-mode 1))

(use-package repeat-insert
  :disabled t
  :commands (insert-patterned
             insert-patterned-2
             insert-patterned-3
             insert-patterned-4))

(use-package restclient
  :load-path "site-lisp/restclient")

(use-package reveal-in-finder
  :load-path "site-lisp/reveal-in-finder"
  :if (eq system-type 'darwin)
  :bind
  ("C-H-M-S-o" . reveal-in-finder))

(use-package ruby-mode
  :load-path "site-lisp/ruby-mode"
  :mode ("\\.rb\\'" . ruby-mode)
  :interpreter ("ruby" . ruby-mode)
  :functions inf-ruby-keys
  :config
  (use-package yari
    :load-path "site-lisp/yari-with-buttons"
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
    (bind-key "<return>" #'my-ruby-smart-return ruby-mode-map)
    (bind-key "C-h C-i" #'helm-yari ruby-mode-map))

(use-package saveplace
  :config
  (progn
    (setq-default save-place t)
    ;; Keep places in the load path
    (setq save-place-file "~/Documents/places")))
  (add-hook 'ruby-mode-hook 'my-ruby-mode-hook))


(use-package selectkey
  :disabled t
  :bind-keymap ("C-. b" . selectkey-select-prefix-map)
  :config
  (selectkey-define-select-key compile "c" "\\*compilation")
  (selectkey-define-select-key shell-command "o" "Shell Command")
  (selectkey-define-select-key shell "s" "\\*shell" (shell))
  (selectkey-define-select-key multi-term "t" "\\*terminal" (multi-term-next))
  (selectkey-define-select-key eshell "z" "\\*eshell" (eshell)))

(use-package session
  :if (not noninteractive)
  :load-path "site-lisp/session/lisp/"
  :preface
  (defun remove-session-use-package-from-settings ()
    (when (string= (file-name-nondirectory (buffer-file-name))
                   "settings.el")
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "^ '(session-use-package " nil t)
          (delete-region (line-beginning-position)
                         (1+ (line-end-position)))))))

  ;; expanded folded secitons as required
  (defun le::maybe-reveal ()
    (when (and (or (memq major-mode  '(org-mode outline-mode))
                   (and (boundp 'outline-minor-mode)
                        outline-minor-mode))
               (outline-invisible-p))
      (if (eq major-mode 'org-mode)
          (org-reveal)
        (show-subtree))))

  (defvar server-process nil)

  (defun save-information ()
    (with-temp-message "Saving Emacs information..."
      (recentf-cleanup)

      (loop for func in kill-emacs-hook
            unless (memq func '(exit-gnus-on-exit server-force-stop))
            do (funcall func))

      (unless (or noninteractive
                  running-alternate-emacs
                  (and server-process
                       (eq 'listen (process-status server-process))))
        (server-start))))

  :config
  (add-hook 'before-save-hook 'remove-session-use-package-from-settings)
  (add-hook 'session-after-jump-to-last-change-hook 'le::maybe-reveal)
  (run-with-idle-timer 60 t 'save-information)
  (add-hook 'after-init-hook 'session-initialize t))

(use-package shift-text
  :load-path "site-lisp/shift-text"
  :commands (shfit-text-right shfit-text-left shift-text-up shift-text-down)
  :bind (("<M-right>" . shift-text-right)
         ("<M-left>" .  shift-text-left)
         ("<M-up>" .  shift-text-up)
         ("<M-down>" .  shift-text-down)))

(use-package sh-script
  :defer t
  :init
  (defvar sh-script-initialized nil)
  (defun initialize-sh-script ()
    (unless sh-script-initialized
      (setq sh-script-initialized t)
      (info-lookup-add-help :mode 'shell-script-mode
                            :regexp ".*"
                            :doc-spec
                            '(("(bash)Index")))))

  (add-hook 'shell-mode-hook 'initialize-sh-script))

(use-package sh-toggle
  :bind ("C-. C-z" . shell-toggle))

(use-package slime
  :disabled t
  :load-path "site-lisp/slime"
  :commands (sbcl slime)
  :init
  (add-hook
   'slime-load-hook
   #'(lambda ()
       (slime-setup
        '(slime-asdf
          slime-autodoc
          slime-banner
          slime-c-p-c
          slime-editing-commands
          slime-fancy-inspector
          slime-fancy
          slime-fuzzy
          slime-highlight-edits
          slime-parse
          slime-presentation-streams
          slime-presentations
          slime-references
          slime-repl
          slime-sbcl-exts
          slime-package-fu
          slime-fontifying-fu
          slime-mdot-fu
          slime-scratch
          slime-tramp
          ;; slime-enclosing-context
          ;; slime-typeout-frame
          slime-xref-browser))

       (define-key slime-repl-mode-map [(control return)] 'other-window)

       (define-key slime-mode-map [return] 'paredit-newline)
       (define-key slime-mode-map [(control ?h) ?F] 'info-lookup-symbol)))

  :config
  (progn
    (eval-when-compile
      (defvar slime-repl-mode-map))

    (setq slime-net-coding-system 'utf-8-unix)

    (setq slime-lisp-implementations
          '((sbcl
             ("sbcl" "--core"
              "/Users/dhaley/Library/Lisp/sbcl.core-with-slime-X86-64")
             :init
             (lambda (port-file _)
               (format "(swank:start-server %S)\n" port-file)))
            (ecl ("ecl" "-load" "/Users/dhk/Library/Lisp/init.lisp"))
            (clisp ("clisp" "-i" "/Users/dhk/Library/Lisp/lwinit.lisp"))))

    (setq slime-default-lisp 'sbcl)
    (setq slime-complete-symbol*-fancy t)
    (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)

    (defun sbcl (&optional arg)
      (interactive "P")
      (let ((slime-default-lisp (if arg 'sbcl64 'sbcl))
            (current-prefix-arg nil))
        (slime)))
    (defun clisp () (interactive) (let ((slime-default-lisp 'clisp)) (slime)))
    (defun ecl () (interactive) (let ((slime-default-lisp 'ecl)) (slime)))

    (defun start-slime ()
      (interactive)
      (unless (slime-connected-p)
        (save-excursion (slime))))

    (add-hook 'slime-mode-hook 'start-slime)
    (add-hook 'slime-load-hook #'(lambda () (require 'slime-fancy)))
    (add-hook 'inferior-lisp-mode-hook #'(lambda () (inferior-slime-mode t)))

    (use-package hyperspec
      :config
      (setq common-lisp-hyperspec-root
            (expand-file-name "~/Library/Lisp/HyperSpec/")))))

(use-package smart-compile
  :disabled t
  :commands smart-compile
  :bind (("C-c c" . smart-compile)
         ("A-n"   . next-error)
         ("A-p"   . previous-error)))

(use-package smartparens
  :disabled t
  :load-path "site-lisp/smartparens"
  :commands (smartparens-mode show-smartparens-mode)
  :config
  (use-package smartparens-config))

(use-package smerge-mode
  :commands smerge-mode
  :config
  (setq smerge-command-prefix (kbd "C-. C-.")))

(use-package stopwatch
  :bind ("<f8>" . stopwatch))

(use-package sunrise-commander
  :load-path "site-lisp/sunrise-commander"
  :bind (("C-c j" . my-activate-sunrise)
         ("C-c C-j" . sunrise-cd))
  :commands sunrise
  :defines sr-tabs-mode-map
  :preface
  (defun my-activate-sunrise ()
    (interactive)
    (let ((sunrise-exists
           (loop for buf in (buffer-list)
                 when (string-match " (Sunrise)$" (buffer-name buf))
                 return buf)))
      (if sunrise-exists
          (call-interactively 'sunrise)
        (sunrise "~/dl/" "~/Archives/"))))

  :config
  (require 'sunrise-x-modeline)
  (require 'sunrise-x-tree)
  (require 'sunrise-x-tabs)

  (bind-key "/" #'sr-sticky-isearch-forward sr-mode-map)
  (bind-key "<backspace>" #'sr-scroll-quick-view-down sr-mode-map)
  (bind-key "C-x t" #'sr-toggle-truncate-lines sr-mode-map)

  (bind-key "q" #'sr-history-prev sr-mode-map)
  (bind-key "z" #'sr-quit sr-mode-map)

  (unbind-key "C-e" sr-mode-map)
  (unbind-key "C-p" sr-tabs-mode-map)
  (unbind-key "C-n" sr-tabs-mode-map)
  (unbind-key "M-<backspace>" sr-term-line-minor-mode-map)

  (bind-key "M-[" #'sr-tabs-prev sr-tabs-mode-map)
  (bind-key "M-]" #'sr-tabs-next sr-tabs-mode-map)

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
        (sr-beginning-of-buffer)))))

(use-package swiper
  :bind ("C-. r" . swiper)
  :load-path "site-lisp/swiper"
  :diminish ivy-mode
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-display-style 'fancy)
  (bind-keys :map swiper-map
             ("<escape>" . minibuffer-keyboard-quit))
  (bind-keys :map ivy-minibuffer-map
             ("<escape>" . minibuffer-keyboard-quit)
             ("C-k"      . delete-minibuffer-contents))
  (defun dkh-swiper ()
     (interactive)
     (swiper)
     (add-to-list 'regexp-search-ring (ivy--regex ivy-text)))
  (ivy-mode t))

(use-package tablegen-mode
  :mode ("\\.td\\'" . tablegen-mode))

(use-package texinfo
  :defines texinfo-section-list
  :mode ("\\.texi\\'" . texinfo-mode)
  :config
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
            5))))))

(use-package theme-changer
  :load-path "site-lisp/theme-changer"
  :init
  (use-package solar
    :init
    ;; (setq calendar-location-name "New York, NY"
    ;;       calendar-latitude 41.8
    ;;       calendar-longitude -73.59))
    )
  :config
  (use-package apropospriate-theme)
  (change-theme 'apropospriate-light 'apropospriate-dark))

(use-package tiny
  :load-path "site-lisp/tiny"
  :bind ("C-. N" . tiny-expand))

(use-package tramp-sh
  :defer t)

(use-package unbound)

(use-package vkill
  :commands vkill
  :bind ("C-x L" . vkill-and-helm-occur)
  :preface
  (defun vkill-and-helm-occur ()
    (interactive)
    (vkill)
    (call-interactively #'helm-occur))

  :config
  (setq vkill-show-all-processes t))

(use-package volatile-highlights
  :load-path "site-lisp/volatile-highlights"
  :diminish (volatile-highlights-mode . "")
  :config
  (volatile-highlights-mode t))

(use-package wand
    :load-path "site-lisp/wand"
  :bind (("C-. RET" . wand:execute)
         ("C-. l" . wand:execute-current-line)
         ("C-. SPC" . toolbox:execute-and-replace))
  :init
  (progn
    (require 'load-functions))
  :config
  (progn
    (setq wand:*rules*
          (list (wand:create-rule :match "\\$ "
                                  :capture :after
                                  :action ~popup-shell-command)
                (wand:create-rule :match "https?://"
                                  :capture :whole
                                  :action browse-url-at-point)
                (wand:create-rule :match "file:"
                                  :capture :after
                                  :action toolbox:open-file)
                (wand:create-rule :match "#> "
                                  :capture :after
                                  :action ~add-bracket-and-eval)))))

(use-package wcount
  :disabled t
  :commands wcount-mode)

(use-package whitespace
  :diminish (global-whitespace-mode
             whitespace-mode
             whitespace-newline-mode)
  :commands (whitespace-buffer
             whitespace-cleanup
             whitespace-mode)
  :defines (whitespace-auto-cleanup
            whitespace-rescan-timer-time
            whitespace-silent)
  :preface
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
                  #'(lambda () (ignore (whitespace-cleanup))) nil t)
        (whitespace-cleanup))))

  :init
  (add-hook 'find-file-hooks 'maybe-turn-on-whitespace t)

  :config
  (remove-hook 'find-file-hooks 'whitespace-buffer)
  (remove-hook 'kill-buffer-hook 'whitespace-buffer)

  ;; For some reason, having these in settings.el gets ignored if whitespace
  ;; loads lazily.
  (setq whitespace-auto-cleanup t
        whitespace-line-column 80
        whitespace-rescan-timer-time nil
        whitespace-silent t
        whitespace-style '(face trailing lines space-before-tab empty)))

(use-package winner
  :if (not noninteractive)
  :defer 5
  :bind (("M-N" . winner-redo)
         ("M-P" . winner-undo))
  :config
  (winner-mode 1))

(use-package workgroups
  :load-path "site-lisp/workgroups"
  :diminish workgroups-mode
  :bind-keymap ("C-\\" . wg-map)
  :config
  (workgroups-mode 1)

  (let ((workgroups-file (expand-file-name "workgroups" user-data-directory)))
    (if (file-readable-p workgroups-file)
        (wg-load workgroups-file)))

  (bind-key "C-\\" #'wg-switch-to-previous-workgroup wg-map)
  (bind-key "\\" #'toggle-input-method wg-map)
  
  (defun wg-create-workgroup-awesome ()
    "create workgroups using names from awesome button"
    (interactive)
    (wg-create-workgroup (get-awesome-button)))

  (use-package awesome-button
    :commands (get-awesome-button awesome-button-say)
    :init
    (progn
      (defun awesome-button-say ()
        "Say a word for awesome"
        (interactive)
        (let
            ((a-word (get-awesome-button)))
          (kill-new a-word 't)
          (osx-say a-word)
          (message a-word))))))

(use-package wrap-region
  :load-path "site-lisp/wrap-region"
  :commands wrap-region-mode
  :diminish wrap-region-mode
  :config
  (wrap-region-add-wrappers
   '(("$" "$")
     ("/" "/" nil ruby-mode)
     ("/* " " */" "#" (java-mode javascript-mode css-mode c-mode c++-mode))
     ("`" "`" nil (markdown-mode ruby-mode shell-script-mode)))))

(use-package yaml-mode
  :load-path "site-lisp/yaml-mode"
  :mode ("\\.ya?ml\\'" . yaml-mode))

(use-package yasnippet
  :load-path "site-lisp/yasnippet"
  :demand t
  :diminish yas-minor-mode
  :commands (yas-expand yas-minor-mode)
  :functions (yas-guess-snippet-directories yas-table-name)
  :defines (yas-guessed-modes)
  :mode ("/\\.emacs\\.d/snippets/" . snippet-mode)
  :bind (("C-c y TAB" . yas-expand)
         ("C-c y s"   . yas-insert-snippet)
         ("C-c y n"   . yas-new-snippet)
         ("C-c y v"   . yas-visit-snippet-file))
  :preface
  (defun yas-new-snippet (&optional choose-instead-of-guess)
    (interactive "P")
    (let ((guessed-directories (yas-guess-snippet-directories)))
      (switch-to-buffer "*new snippet*")
      (erase-buffer)
      (kill-all-local-variables)
      (snippet-mode)
      (set (make-local-variable 'yas-guessed-modes)
           (mapcar #'(lambda (d)
                       (intern (yas-table-name (car d))))
                   guessed-directories))
      (unless (and choose-instead-of-guess
                   (not (y-or-n-p "Insert a snippet with useful headers? ")))
        (yas-expand-snippet
         (concat "\n"
                 "# -*- mode: snippet -*-\n"
                 "# name: $1\n"
                 "# --\n"
                 "$0\n")))))

  :config
  (yas-load-directory "~/.emacs.d/snippets/")
  (yas-global-mode 1)

  (bind-key "C-i" #'yas-next-field-or-maybe-expand yas-keymap))

(use-package zoom-window
  :bind ("H-z" . zoom-window-zoom))

(use-package ztree-diff
  :load-path "site-lisp/ztree"
  :config
  (set-face-attribute 'ztreep-diff-model-add-face  nil :foreground "deep sky blue")
  (bind-keys :map ztreediff-mode-map
                 ("p" . previous-line)
                 ("k" . previous-line)
                 ("j" . next-line)
                 ("n" . next-line))

  (when (when-feature-loaded 'hydra)
      (bind-keys :map ztreediff-mode-map
                 ("\\" . hydra-ztree/body))
      (defhydra hydra-ztree (:color blue :hint nil)
          "
                                                                      ╭────────────┐
       Move      File                 Do                              │ Ztree diff │
    ╭─────────────────────────────────────────────────────────────────┴────────────╯
      _k_/_p_   [_C_] copy                  [_h_] toggle equal files
      ^ ^↑^ ^   [_D_] delete                [_x_] toggle subtree
      ^_TAB_^   [_v_] view                  [_r_] partial rescan
      ^ ^↓^ ^   [_d_] simple diff           [_R_] full rescan
      _j_/_n_   [_RET_] diff/expand         [_g_] refresh
      ^ ^ ^ ^   [_SPC_] simple diff/expand
    --------------------------------------------------------------------------------
          "
         ("\\" hydra-master/body "back")
         ("<ESC>" nil "quit")
         ("p" previous-line)
         ("k" previous-line)
         ("j" next-line)
         ("n" next-line)
         ("C" ztree-diff-copy)
         ("h" ztree-diff-toggle-show-equal-files)
         ("D" ztree-diff-delete-file)
         ("v" ztree-diff-view-file)
         ("d" ztree-diff-simple-diff-files)
         ("r" ztree-diff-partial-rescan)
         ("R" ztree-diff-full-rescan)
         ("RET" ztree-perform-action)
         ("SPC" ztree-perform-soft-action)
         ("TAB" ztree-jump-side)
         ("g" ztree-refresh-buffer)
         ("x" ztree-toggle-expand-subtree))))

;;; Post initialization

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

(defun choose-browser (url &rest args)
  (interactive "sURL: ")
  (if current-prefix-arg
      (eww url)
    (let ((browse-url-browser-function 'browse-url-default-macosx-browser))
      (browse-url url))))

;; Registers

(dolist
    (r `((?i (file . "~/.emacs.d/init.el"))
         (?a (file . "~/.emacs.d/.abbrev_defs"))
         (?b (file . "~/.profile"))
         (?B (file . "~/.bashrc"))
         (?d (file . "~/Documents/cde.drush/nrel.aliases.drushrc.php"))
         (?e (file . "~/.emacs.d"))
         (?t (file . "~/Documents/Tasks/todo.txt"))
         (?s (file . "~/.emacs.d/settings.el"))
         (?o (file . "~/.emacs.d/dot-org.el"))
         (?g (file . "~/.emacs.d/dot-gnus.el"))
         (?O (file . "~/.emacs.d/org-settings.el"))
         (?r (file . "~/src/drupal_scripts/release.sh"))
         (?T (file . "~/Documents/Tasks"))
         (?G (file . "~/.emacs.d/gnus-settings.el"))
         (?u (file . "~/.emacs.d/site-lisp/xmsi-math-symbols-input.el"))
         (?z (file . "~/.zshrc"))))
  (set-register (car r) (cadr r)))

;; unused bindings
;; (bind-key "C-`" 'rotate-windows)
;;   :bind ("C-c C-r" . quickrun))

;;; init.el ends here
