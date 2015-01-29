;;;_. Initialization

(setq message-log-max 16384)

(defconst emacs-start-time (current-time))

(unless noninteractive
  (message "Loading %s..." load-file-name))

(load (expand-file-name "load-path" (file-name-directory load-file-name)))

(require 'use-package)
;; (eval-when-compile
;;   (setq use-package-verbose (null byte-compile-current-file)))

;;;_ , Utility macros and functions

(defmacro hook-into-modes (func modes)
  `(dolist (mode-hook ,modes)
     (add-hook mode-hook ,func)))

(add-hook 'prog-mode-hook (lambda () (interactive) (setq show-trailing-whitespace 1)))

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

(defvar mac-fullscreen-on  nil
  "keep a track of mac-mouse-turn-o(n|ff)-fullscreen, assumes fullscreen is not on")

(defun mac-toggle-fullscreen ()
  "toggle fullscreen mode in Emacs mac (by Yamamoto Mitsuharu)"
  (interactive)
  ;; check we are in the emacs mac build
                                        ; (when (functionp 'mac-process-hi-command)
  (if (eq mac-fullscreen-on t)
      (progn
        (mac-mouse-turn-off-fullscreen t)
        (setq mac-fullscreen-on nil))
    (progn
      (mac-mouse-turn-on-fullscreen t)
      (setq mac-fullscreen-on t))))

(when (and (window-system) (fboundp 'mac-mouse-turn-on-fullscreen))
  (bind-key "C-H-f" 'mac-toggle-fullscreen))

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

;;;_ , Enable disabled commands

(put 'downcase-region  'disabled nil)   ; Let downcasing work
(put 'erase-buffer     'disabled nil)
(put 'eval-expression  'disabled nil)   ; Let ESC-ESC work
(put 'narrow-to-page   'disabled nil)   ; Let narrowing work
(put 'narrow-to-region 'disabled nil)   ; Let narrowing work
(put 'set-goal-column  'disabled nil)
(put 'upcase-region    'disabled nil)   ; Let upcasing work

;;;_.  Keybindings

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

;;;_ , global-map

;;;_  . H-?

;;;_ , Enable C-8 prefix

(defvar workgroups-preload-map)
(define-prefix-command 'workgroups-preload-map)

(bind-key "<H-down>" 'shrink-window)
(bind-key "<H-left>" 'shrink-window-horizontally)
(bind-key "<H-right>" 'enlarge-window-horizontally)
(bind-key "<H-up>" 'enlarge-window)
(bind-key "H-`" 'new-frame)

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

(bind-key "M-T" 'tags-search)

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
(defun delete-indentation-forward ()
  (interactive)
  (delete-indentation t))

(bind-key "M-s n" 'find-name-dired)
                                        ;(bind-key "M-s o" 'occur)
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
;; ;; Some further isearch bindings
;; these keys break eshell
;; (bind-key "C-c" 'isearch-toggle-case-fold isearch-mode-map)
;; (bind-key "C-t" 'isearch-toggle-regexp isearch-mode-map)
;; (bind-key "C-^" 'isearch-edit-string isearch-mode-map)
;; (bind-key "C-i" 'isearch-complete isearch-mode-map)

;;;_  . H-?

;; (define-key key-translation-map (kbd "H-TAB") (kbd "C-TAB"))

;;;_ , ctl-x-map

;;;_  . C-x ?

(bind-key "C-x B" 'ido-switch-buffer-other-window)
(bind-key "C-x d" 'delete-whitespace-rectangle)
(bind-key "C-x F" 'set-fill-column)
(bind-key "C-x t" 'toggle-truncate-lines)

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

(defun find-alternate-file-with-sudo ()
  (interactive)
  (find-alternate-file (concat "/sudo::" (buffer-file-name))))

(bind-key "C-x C-v" 'find-alternate-file-with-sudo)

(defun mlm/locate-make-command-line (search-string)
  (list "mdfind" "-interpret" search-string))

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

(bind-key "C-c c" 'compile)
(bind-key "C-c d" 'delete-current-line)

(defun reset-dns ()
  (interactive)
  (message "Resetting DNS...")
  (shell-command "cleardns")
  (shell-command "launchctl unload ~/Library/LaunchAgents/mac.pdnsd.plist")
  (shell-command "launchctl load ~/Library/LaunchAgents/mac.pdnsd.plist")
  (message "Resetting DNS...done"))

(bind-key "C-c D" 'reset-dns)

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
  (defvar emacs-min-height)
  (defvar emacs-min-width))

(setq display-name
      (let ((width (display-pixel-width)))
        (cond
         ((= width 2560)
          'retina-imac)
         ((= width 1440)
          'retina-macbook-pro))))

(unless noninteractive
  (if running-alternate-emacs
      (progn
        (defun emacs-min-top () 22)
        (defun emacs-min-left () 5)
        (defvar emacs-min-height 57)
        (defvar emacs-min-width 90))

    (defun emacs-min-top () 23)
    (defun emacs-min-left ()
      (cond
       ((eq display-name 'retina-imac) 975)
       (t 521)))
    (defvar emacs-min-height
      (cond
       ((eq display-name 'retina-imac) 55)
       (t 44)))
    (defvar emacs-min-width 100)))

(defun emacs-min ()
  (interactive)

  (set-frame-parameter (selected-frame) 'fullscreen nil)
  (set-frame-parameter (selected-frame) 'vertical-scroll-bars nil)
  (set-frame-parameter (selected-frame) 'horizontal-scroll-bars nil)

  (set-frame-parameter (selected-frame) 'top (emacs-min-top))
  (set-frame-parameter (selected-frame) 'left (emacs-min-left))
  (set-frame-parameter (selected-frame) 'height emacs-min-height)
  (set-frame-parameter (selected-frame) 'width emacs-min-width)

  (set-frame-font
   (cond
    ((eq display-name 'retina-imac)
     (if running-alternate-emacs
         "-*-Myriad Pro-normal-normal-normal-*-20-*-*-*-p-0-iso10646-1"
       "-*-Source Code Pro-normal-normal-normal-*-20-*-*-*-m-0-iso10646-1"))
    (t
     (if running-alternate-emacs
         "-*-Myriad Pro-normal-normal-normal-*-17-*-*-*-p-0-iso10646-1"
       "-*-Source Code Pro-normal-normal-normal-*-15-*-*-*-m-0-iso10646-1"))))

  (when running-alternate-emacs
    (set-background-color "grey85")
    (set-face-background 'fringe "gray80")))

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

(bind-key "C-c m" 'emacs-toggle-size)

(defun insert-date ()
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

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
           '(emacs-lisp-mode lisp-mode php-mode
                             python-mode perl-mode)))
      (set symbol value))
  :type 'string
  :group 'mail)

(defun insert-user-timestamp ()
  "Insert a quick timestamp using the value of `user-initials'."
  (interactive)
  (insert (format "%s (%s): " user-initials
                  (format-time-string "%Y-%m-%dT%T" (current-time)))))

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
    (cl-cl-flet ((message (&rest ignore)))
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
  (insert (number-to-string (count-lines (point-min) (point)))))

(global-set-key "\C-c\C-h" 'wph-here)

;;;_  . C-c C-?

(defun delete-to-end-of-buffer ()
  (interactive)
  (kill-region (point) (point-max)))

;; (bind-key "C-c C-z" 'delete-to-end-of-buffer)

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

    (cl-cl-flet ((switch-in-other-buffer
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

(defun rotate-windows ()
  "Rotate your windows"
  (interactive)
  (cond ((not (> (count-windows)1))
         (message "You can't rotate a single window!"))
        (t
         (setq i 1)
         (setq numWindows (count-windows))
         (while  (< i numWindows)
           (let* (
                  (w1 (elt (window-list) i))
                  (w2 (elt (window-list) (+ (% i numWindows) 1)))

                  (b1 (window-buffer w1))
                  (b2 (window-buffer w2))

                  (s1 (window-start w1))
                  (s2 (window-start w2))
                  )
             (set-window-buffer w1  b2)
             (set-window-buffer w2 b1)
             (set-window-start w1 s2)
             (set-window-start w2 s1)
             (setq i (1+ i)))))))

(bind-key "C-|" 'rotate-windows)

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

;;;_. Packages

;;;_ , abbrev

(use-package abbrev
  :disabled t
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


(use-package ace-link
  :commands ace-link-setup-default
  :config (ace-link-setup-default))

;;;_ , ace-jump-mode

(use-package ace-jump-mode
  :bind ("H-h" . ace-jump-mode))

;;;_ , ace-window
(use-package ace-window
  :bind ("C-x o" . ace-window))

;;;_ , agda

(defun agda-site-lisp ()
  (let ((agda (nth 1 (split-string
                      (shell-command-to-string "load-env-agda which agda")
                      "\n"))))
    (and agda
         (expand-file-name
          "../share/x86_64-osx-ghc-7.8.3/Agda-2.4.2.2/emacs-mode"
          (file-name-directory agda)))))

(use-package agda2-mode
  :mode ("\\.agda\\'" . agda2-mode)
  :pre-init (add-to-list 'load-path (agda-site-lisp))
  :init
  (use-package agda-input)
  :config
  (progn
    (defun agda2-insert-helper-function (&optional prefix)
      (interactive "P")
      (let ((func-def (with-current-buffer "*Agda information*"
                        (buffer-string))))
        (save-excursion
          (forward-paragraph)
          (let ((name (car (split-string func-def " "))))
            (insert "  where\n    " func-def "    " name " x = ?\n")))))

    (bind-key "C-c C-i" 'agda2-insert-helper-function agda2-mode-map)))

;;;_ , allout

(use-package allout
  :disabled t
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
        (eval `(bind-key ,(concat (format-kbd-macro allout-command-prefix)
                                  " " (char-to-string (car mapping)))
                         (quote ,(cdr mapping))
                         allout-mode-map)))

      (if (memq major-mode lisp-modes)
          (unbind-key "C-k" allout-mode-map)))

    (add-hook 'allout-mode-hook 'my-allout-mode-hook)))

;;;_ , archive-region

(use-package archive-region
  :disabled t
  :commands kill-region-or-archive-region
  :bind ("C-w" . kill-region-or-archive-region))

(use-package apache-mode
  :mode ("\\(\\.htaccess$\\|\\.conf$\\)" . apache-mode)
  )

;;;_ , Applescripts
(use-package AppleScripts
  :bind (("H-s" . osx-say) ("H-f" . osx-pathfinder-or-iterm)))

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

;;;_ , ascii-art-to-unicode

(use-package ascii-art-to-unicode)

;;;_ , auctex

(use-package tex-site
  ;; :load-path "site-lisp/auctex/preview/"
  :defines (latex-help-cmd-alist latex-help-file)
  :mode ("\\.tex\\'" . TeX-latex-mode)
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
        (use-package ac-math)

        (defun ac-latex-mode-setup ()
          (nconc ac-sources
                 '(ac-source-math-unicode ac-source-math-latex
                                          ac-source-latex-commands)))

        (add-to-list 'ac-modes 'latex-mode)
        (add-hook 'latex-mode-hook 'ac-latex-mode-setup)

        (info-lookup-add-help :mode 'latex-mode
                              :regexp ".*"
                              :parse-rule "\\\\?[a-zA-Z]+\\|\\\\[^a-zA-Z]"
                              :doc-spec '(("(latex2e)Concept Index" )
                                          ("(latex2e)Command Index")))))))

;;;_ , auto-complete

(use-package auto-complete-config
  :disabled t
  :load-path ("site-lisp/ac/auto-complete"
              "site-lisp/ac/popup-el"
              "site-lisp/ac/ac-source-emmet")
  :diminish auto-complete-mode
  :init
  (progn
    (use-package pos-tip)
    (ac-config-default))

  :config
  (progn
    ;;(ac-set-trigger-key "TAB")
    (ac-set-trigger-key "<backtab>")
    (setq ac-use-menu-map t)

    (bind-key "H-M-?" 'ac-last-help)
    (unbind-key "C-s" ac-completing-map)))

;;;_ , auto-dim-other-buffers

(use-package auto-dim-other-buffers
  :commands auto-dim-other-buffers-mode
  :diminish auto-dim-other-buffers-mode)

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
  :disabled t
  :commands (syncbasecamp completebasecamp basecamp-showlist basecamp-showprojects)
  :init
  (progn
    (defun syncbasecamp ()
      (interactive)
      (http-get
       "http://floatsolutions.com/docs/basecamp/index.php?accesskey=sdf6SDFwr88sdfASDdye76qw76876DFGDfgsdf"
       nil 'ignore nil "basecamp.org" nil)
      (org-mode)
      (save-buffer))

    (defun completebasecamp (todoid)
      (interactive)
      (http-get (concatenate 'string
                             "http://floatsolutions.com/docs/basecamp/index.php?accesskey=sdf6SDFwr88sdfASDdye76qw76876DFGDfgsdf&complete="
                             todoid) nil 'ignore nil "basecamp.org" nil)
      (org-mode)
      (save-buffer))

    (defun basecamp-showlist ()
      (interactive)
      (find-file-other-window "~/org/basecamp.org")
      (syncbasecamp))

    (defun basecamp-showprojects ()
      (interactive)
      (find-file-other-window "~/org/projects.org"))))

;;;_ , bbdb

(use-package bbdb-com
  :commands bbdb-create
  :bind ("M-B" . bbdb))

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
  :disabled t
  :defer t
  :config
  (progn
    (use-package bookmark+)

    (defun my-bookmark-set ()
      (interactive)
      (cl-cl-flet ((bmkp-completing-read-lax
              (prompt &optional default alist pred hist)
              (completing-read prompt alist pred nil nil hist default)))
        (call-interactively #'bookmark-set)))

    (bind-key "C-x r m" 'my-bookmark-set)))

;;;_ , browse-kill-ring+

(use-package browse-kill-ring+)

(use-package buffer-move
  :bind (
         ("<C-S-up>"    . buf-move-up)
         ("<C-S-down>"  . buf-move-down)
         ("<C-S-left>"  . buf-move-left)
         ("<C-S-right>" . buf-move-right)))

(use-package caffeine
  :commands (caffeine-toggle)
  :config
  (caffeine-mode))

;;;; calfw
(use-package calfw
  :commands cfw:open-calendar-buffer
  :init
  (progn
    (use-package calfw-org
      :commands cfw:open-org-calendar
      :config
      (progn
        (defun cfw:open-org-calendar ()
          "Open an org schedule calendar in the new buffer."
          (interactive)
          (let* ((source1 (cfw:org-create-source))
                 (cp (cfw:create-calendar-component-buffer
                      :view 'two-weeks
                      :contents-sources (list source1)
                      :custom-map cfw:org-schedule-map
                      :sorter 'cfw:org-schedule-sorter)))
            (switch-to-buffer (cfw:cp-get-buffer cp))))))))


;;;_ , cc-mode

(use-package cc-mode
  :mode (("\\.h\\(h?\\|xx\\|pp\\)\\'" . c++-mode)
         ("\\.m\\'"                   . c-mode)
         ("\\.mm\\'"                  . c++-mode))
  :init
  (progn
    (defun my-paste-as-check ()
      (interactive)
      (save-excursion
        (insert "/*\n")
        (let ((beg (point)) end)
          (yank)
          (setq end (point-marker))
          (goto-char beg)
          (while (< (point) end)
            (forward-char 2)
            (insert "CHECK: ")
            (forward-line 1)))
        (insert "*/\n")))

    (defun my-c-indent-or-complete ()
      (interactive)
      (let ((class (syntax-class (syntax-after (1- (point))))))
        (if (or (bolp) (and (/= 2 class)
                            (/= 3 class)))
            (call-interactively 'indent-according-to-mode)
          (call-interactively 'auto-complete))))

    (defvar printf-index 0)

    (defun insert-counting-printf (arg)
      (interactive "P")
      (if arg
          (setq printf-index 0))
      (if t
          (insert (format "std::cerr << \"step %d..\" << std::endl;\n"
                          (setq printf-index (1+ printf-index))))
        (insert (format "printf(\"step %d..\\n\");\n"
                        (setq printf-index (1+ printf-index)))))
      (forward-line -1)
      (indent-according-to-mode)
      (forward-line))

    (defun my-c-mode-common-hook ()
      ;; (abbrev-mode 1)
      (ggtags-mode 1)
      (hs-minor-mode 1)
      (hide-ifdef-mode 1)
      (whitespace-mode 1)
      (which-function-mode 1)
      ;; (auto-complete-mode 1)
      (yas-minor-mode 1)
      (bug-reference-prog-mode 1)

      (diminish 'ggtags-mode)
      (diminish 'hs-minor-mode)
      (diminish 'hide-ifdef-mode)

      (bind-key "C-c p" 'insert-counting-printf c-mode-base-map)

      ;; (auto-complete-mode 1)
      ;; (setq ac-sources (list (if (and (fboundp 'semantic-active-p)
      ;;                                 (funcall #'semantic-active-p))
      ;;                            'ac-source-semantic
      ;;                          'ac-source-gtags)))
      ;; (bind-key "<A-tab>" 'ac-complete c-mode-base-map)

      ;;(doxymacs-mode 1)
      ;;(doxymacs-font-lock)

      (bind-key "<return>" 'newline-and-indent c-mode-base-map)

      ;; (set (make-local-variable 'yas-fallback-behavior)
      ;;      '(apply my-c-indent-or-complete . nil))
      ;; (bind-key "<tab>" 'yas-expand-from-trigger-key c-mode-base-map)

      (unbind-key "M-j" c-mode-base-map)
      (bind-key "C-c C-i" 'c-includes-current-file c-mode-base-map)
      (bind-key "C-c C-y" 'my-paste-as-check c-mode-base-map)

      (set (make-local-variable 'parens-require-spaces) nil)
      (setq indicate-empty-lines t)
      (setq fill-column 72)

      (bind-key "M-q" 'c-fill-paragraph c-mode-base-map)

      (let ((bufname (buffer-file-name)))
        (when bufname
          (cond
           ((string-match "/ledger/" bufname)
            (c-set-style "ledger"))
           ((string-match "/ansi/" bufname)
            (c-set-style "ti")
            (substitute-key-definition 'fill-paragraph 'ti-refill-comment
                                       c-mode-base-map global-map)
            (bind-key "M-q" 'ti-refill-comment c-mode-base-map))
           ((string-match "/edg/" bufname)
            (c-set-style "edg"))
           (t
            (c-set-style "clang")))))

      (font-lock-add-keywords 'c++-mode '(("\\<\\(assert\\|DEBUG\\)("
                                           1 font-lock-warning-face t))))

    (add-hook 'c-mode-common-hook 'my-c-mode-common-hook))

  :config
  (progn
    (setq c-syntactic-indentation nil)

    (bind-key "#" 'self-insert-command c-mode-base-map)
    (bind-key "{" 'self-insert-command c-mode-base-map)
    (bind-key "}" 'self-insert-command c-mode-base-map)
    (bind-key "/" 'self-insert-command c-mode-base-map)
    (bind-key "*" 'self-insert-command c-mode-base-map)
    (bind-key ";" 'self-insert-command c-mode-base-map)
    (bind-key "," 'self-insert-command c-mode-base-map)
    (bind-key ":" 'self-insert-command c-mode-base-map)
    (bind-key "(" 'self-insert-command c-mode-base-map)
    (bind-key ")" 'self-insert-command c-mode-base-map)
    (bind-key "<" 'self-insert-command c++-mode-map)
    (bind-key ">" 'self-insert-command c++-mode-map)

    (use-package cedet
      :disabled t
      :init
      (progn
        ;; Add further minor-modes to be enabled by semantic-mode.  See
        ;; doc-string of `semantic-default-submodes' for other things you can
        ;; use here.
        (dolist (submode '(global-semantic-idle-summary-mode
                           global-semantic-mru-bookmark-mode
                           global-semantic-idle-local-symbol-highlight-mode
                           global-semantic-show-unmatched-syntax-mode))
          (add-to-list 'semantic-default-submodes submode t))

        ;; Enable Semantic
        (semantic-mode 1)

        (when nil              ; jww (2012-06-20): this kills buffers
          ;; if you want to enable support for gnu global
          (use-package semanticdb-global)

          (semanticdb-enable-gnu-global-databases 'c-mode)
          (semanticdb-enable-gnu-global-databases 'c++-mode))))

    (add-to-list 'c-style-alist
                 '("ti"
                   (indent-tabs-mode . nil)
                   (c-basic-offset . 3)
                   (c-comment-only-line-offset . (0 . 0))
                   (c-hanging-braces-alist
                    . ((substatement-open before after)
                       (arglist-cont-nonempty)))
                   (c-offsets-alist
                    . ((statement-block-intro . +)
                       (knr-argdecl-intro . 5)
                       (substatement-open . 0)
                       (substatement-label . 0)
                       (label . 0)
                       (case-label . +)
                       (statement-case-open . 0)
                       (statement-cont . +)
                       (arglist-intro . c-lineup-arglist-intro-after-paren)
                       (arglist-close . c-lineup-arglist)
                       (inline-open . 0)
                       (brace-list-open . 0)
                       (topmost-intro-cont
                        . (first c-lineup-topmost-intro-cont
                                 c-lineup-gnu-DEFUN-intro-cont))))
                   (c-special-indent-hook . c-gnu-impose-minimum)
                   (c-block-comment-prefix . "")))

    (add-to-list 'c-style-alist
                 '("edg"
                   (indent-tabs-mode . nil)
                   (c-basic-offset . 2)
                   (c-comment-only-line-offset . (0 . 0))
                   (c-hanging-braces-alist
                    . ((substatement-open before after)
                       (arglist-cont-nonempty)))
                   (c-offsets-alist
                    . ((statement-block-intro . +)
                       (knr-argdecl-intro . 5)
                       (substatement-open . 0)
                       (substatement-label . 0)
                       (label . 0)
                       (case-label . +)
                       (statement-case-open . 0)
                       (statement-cont . +)
                       (arglist-intro . +)
                       (arglist-close . +)
                       (inline-open . 0)
                       (brace-list-open . 0)
                       (topmost-intro-cont
                        . (first c-lineup-topmost-intro-cont
                                 c-lineup-gnu-DEFUN-intro-cont))))
                   (c-special-indent-hook . c-gnu-impose-minimum)
                   (c-block-comment-prefix . "")))

    (add-to-list 'c-style-alist
                 '("ledger"
                   (indent-tabs-mode . nil)
                   (c-basic-offset . 2)
                   (c-comment-only-line-offset . (0 . 0))
                   (c-hanging-braces-alist
                    . ((substatement-open before after)
                       (arglist-cont-nonempty)))
                   (c-offsets-alist
                    . ((statement-block-intro . +)
                       (knr-argdecl-intro . 5)
                       (substatement-open . 0)
                       (substatement-label . 0)
                       (label . 0)
                       (case-label . 0)
                       (statement-case-open . 0)
                       (statement-cont . +)
                       (arglist-intro . +)
                       (arglist-close . +)
                       (inline-open . 0)
                       (brace-list-open . 0)
                       (topmost-intro-cont
                        . (first c-lineup-topmost-intro-cont
                                 c-lineup-gnu-DEFUN-intro-cont))))
                   (c-special-indent-hook . c-gnu-impose-minimum)
                   (c-block-comment-prefix . "")))

    (add-to-list 'c-style-alist
                 '("clang"
                   (indent-tabs-mode . nil)
                   (c-basic-offset . 2)
                   (c-comment-only-line-offset . (0 . 0))
                   (c-hanging-braces-alist
                    . ((substatement-open before after)
                       (arglist-cont-nonempty)))
                   (c-offsets-alist
                    . ((statement-block-intro . +)
                       (knr-argdecl-intro . 5)
                       (substatement-open . 0)
                       (substatement-label . 0)
                       (label . 0)
                       (case-label . 0)
                       (statement-case-open . 0)
                       (statement-cont . +)
                       (arglist-intro . +)
                       (arglist-close . +)
                       (inline-open . 0)
                       (brace-list-open . 0)
                       (topmost-intro-cont
                        . (first c-lineup-topmost-intro-cont
                                 c-lineup-gnu-DEFUN-intro-cont))))
                   (c-special-indent-hook . c-gnu-impose-minimum)
                   (c-block-comment-prefix . "")))

    (defun opencl ()
      (interactive)
      (find-file "~/src/ansi/opencl.c")
      (find-file-noselect "~/Contracts/TI/bugslayer/cl_0603/cl_0603.c")
      (find-file-noselect "~/Contracts/TI/bugslayer")
      (magit-status "~/src/ansi")
      (gud-gdb "gdb --fullname ~/Contracts/TI/bin/c60/acpia6x"))

    (defun ti-refill-comment ()
      (interactive)
      (let ((here (point)))
        (goto-char (line-beginning-position))
        (let ((begin (point)) end
              (marker ?-) (marker-re "\\(-----\\|\\*\\*\\*\\*\\*\\)")
              (leader-width 0))
          (unless (looking-at "[ \t]*/\\*[-* ]")
            (search-backward "/*")
            (goto-char (line-beginning-position)))
          (unless (looking-at "[ \t]*/\\*[-* ]")
            (error "Not in a comment"))
          (while (and (looking-at "\\([ \t]*\\)/\\* ")
                      (setq leader-width (length (match-string 1)))
                      (not (looking-at (concat "[ \t]*/\\*" marker-re))))
            (forward-line -1)
            (setq begin (point)))
          (when (looking-at (concat "[^\n]+?" marker-re "\\*/[ \t]*$"))
            (setq marker (if (string= (match-string 1) "-----") ?- ?*))
            (forward-line))
          (while (and (looking-at "[^\n]+?\\*/[ \t]*$")
                      (not (looking-at (concat "[^\n]+?" marker-re
                                               "\\*/[ \t]*$"))))
            (forward-line))
          (when (looking-at (concat "[^\n]+?" marker-re "\\*/[ \t]*$"))
            (forward-line))
          (setq end (point))
          (let ((comment (buffer-substring-no-properties begin end)))
            (with-temp-buffer
              (insert comment)
              (goto-char (point-min))
              (flush-lines (concat "^[ \t]*/\\*" marker-re "[-*]+\\*/[ \t]*$"))
              (goto-char (point-min))
              (while (re-search-forward "^[ \t]*/\\* ?" nil t)
                (goto-char (match-beginning 0))
                (delete-region (match-beginning 0) (match-end 0)))
              (goto-char (point-min))
              (while (re-search-forward "[ \t]*\\*/[ \t]*$" nil t)
                (goto-char (match-beginning 0))
                (delete-region (match-beginning 0) (match-end 0)))
              (goto-char (point-min)) (delete-trailing-whitespace)
              (goto-char (point-min)) (flush-lines "^$")
              (set-fill-column (- 80    ; width of the text
                                  6     ; width of "/*  */"
                                  leader-width))
              (goto-char (point-min)) (fill-paragraph nil)
              (goto-char (point-min))
              (while (not (eobp))
                (insert (make-string leader-width ? ) "/* ")
                (goto-char (line-end-position))
                (insert (make-string (- 80 3 (current-column)) ? ) " */")
                (forward-line))
              (goto-char (point-min))
              (insert (make-string leader-width ? )
                      "/*" (make-string (- 80 4 leader-width) marker) "*/\n")
              (goto-char (point-max))
              (insert (make-string leader-width ? )
                      "/*" (make-string (- 80 4 leader-width) marker) "*/\n")
              (setq comment (buffer-string)))
            (goto-char begin)
            (delete-region begin end)
            (insert comment)))
        (goto-char here)))))

;;;_ , cmake-mode

(use-package cmake-mode
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'"         . cmake-mode)))

(use-package crontab-mode
  :mode ("\\.?cron\\(tab\\)?\\'" . crontab-mode))

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
            `(makefile-nmake-font-lock-keywords ,@(cdr font-lock-defaults))))))

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


;;;_ , company-mode

(use-package company
  :init
  (add-hook 'after-init-hook 'global-company-mode))

;;;_ , copy-code

(use-package copy-code
  :bind ("H-M-W" . copy-code-as-rtf))

;;;_ , crosshairs

(use-package crosshairs
  :init

  (bind-key "M-o c" 'crosshairs-mode)

  (progn
    (toggle-crosshairs-when-idle 1)
    (col-highlight-set-interval 120)
    (setq col-highlight-face hl-line-face)

    ;; (defadvice switch-to-buffer (after switch-to-buffer-flash-crosshairs activate)
    ;;   "Call `flash-crosshairs' after `switch-to-buffer'"
    ;;   (flash-crosshairs))

;;     (defadvice select-window (around select-window-flash-crosshairs activate)
;;       "Call `flash-crosshairs' after `select-window', if switching to another buffer.
;; The check is necessary to prevent issues with mini-buffer switching."
;;       (let (cons (cur-buffer-name (buffer-name (current-buffer)))
;;                  ad-arg-bindings)
;;         ad-do-it
;;         (unless (string= (buffer-name (window-buffer window))
;;                          cur-buffer-name)
;;           (flash-crosshairs))))
    ))

;;;_ , css-mode

(use-package css-mode
  :mode ("\\.css$" . css-mode)
  :config
  (progn
    (define-keys css-mode-map
      '(("<return>" newline-and-indent)))
    ;; (use-package ac-emmet)
    (add-hook 'css-mode-hook
              (lambda ()
                ;; (ac-emmet-css-setup)
                ))))

;;;_ , iflipb

(use-package iflipb
  ;; :disabled t
  :commands (iflipb-next-buffer iflipb-previous-buffer)
  :bind (("C-H-M-S-<tab>" . my-iflipb-next-buffer)
         ("H-<backspace>" . my-iflipb-previous-buffer))
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

;;;_ , dash-at-point

(use-package dash-at-point
  :commands (dash-at-point)
  :bind ("\C-c8" . dash-at-point)
  )

;;;_ , debbugs

(use-package debbugs-gnu
  :commands (debbugs-gnu debbugs-gnu-search))

;;;_ , dedicated

(use-package dedicated
  :bind ("C-. D" . dedicated-mode))

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
            (ignore (puthash pat t mark-files-cache)))))

    (defun dired-mark-similar-version ()
      (interactive)
      (setq mark-files-cache (make-hash-table :test #'equal))
      (dired-mark-sexp '(mark-similar-versions name))))

  :config
  (progn
    (defun dired-package-initialize ()
      (unless (featurep 'runner)
        (use-package dired-x)
        ;; (use-package dired-async)
        ;; (use-package dired-sort-map)
        (use-package runner)

        (bind-key "l" 'dired-up-directory dired-mode-map)

        (defun my-dired-switch-window ()
          (interactive)
          (if (eq major-mode 'sr-mode)
              (call-interactively #'sr-change-window)
            (call-interactively #'other-window)))

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
              (funcall dired-omit-regexp-orig))))))

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

;;;_ , direx
(use-package direx
  :bind ("C-H-M-S-j" . direx:jump-to-directory)
  :config
  (progn

    (require 'direx-project)
    (setq direx:leaf-icon "  "
          direx:open-icon "▾ "
          direx:closed-icon "▸ ")

    (defun e2wm:def-plugin-direx (frame wm winfo)
      (let* ((buf (e2wm:history-get-main-buffer))
             (dbuf (with-current-buffer buf
                     (or
                      (ignore-errors
                        (direx-project:jump-to-project-root-noselect))
                      (direx:find-directory-noselect
                       (or default-directory "."))))))
        (with-current-buffer dbuf
          (direx:item-expand direx:root-item))
        (wlf:set-buffer wm (wlf:window-name winfo) dbuf)))

    (e2wm:plugin-register 'direx
                          "DireX"
                          'e2wm:def-plugin-direx)))

;;;_ , discover

(use-package discover
 :disabled t
 :init
 (progn
 (global-discover-mode 1)
 (use-package makey)))

;;;_ , doxymacs

(use-package doxymacs
  :commands doxymacs-mode
  :diminish doxymacs-mode
  :load-path "site-lisp/doxymacs-1.8.0/lisp")

;;;_ , drupal-mode

(use-package drupal-mode
  :commands drupal-mode-bootstrap
  ;; :init (setq drupal-get-function-args t)
  :config
  ;; (add-hook 'find-file-hook 'auto-insert)
  (add-hook 'drupal-mode-hook
            '(lambda ()
               (add-to-list 'Info-directory-list '"~/.emacs.d/site-lisp/drupal-mode")
               (yas-activate-extra-mode 'drupal-mode)
               (when (apply 'derived-mode-p drupal-php-modes)
                 (flycheck-mode t)))))

;;;_ , e2wm

(use-package e2wm
  :bind ("M-+" . e2wm:start-management)
  :config
  (progn
    (use-package e2wm-direx
      :init
      (progn
        (setq e2wm:c-code-recipe
              '(| (:left-max-size 40)
                  (- (:upper-size-ratio 0.6)
                     tree history)
                  (- (:lower-max-size 150)
                     (| (:right-max-size 40)
                        main imenu)
                     sub)))

        (setq e2wm:c-code-winfo
              '((:name main)
                (:name tree    :plugin direx)
                (:name history :plugin history-list)
                (:name imenu   :plugin imenu :default-hide nil)
                (:name sub     :buffer "*info*" :default-hide t)))))))

;;;_ , sublimity

(use-package sublimity
  :commands (sublimity-mode)
  :config
  (progn
    (require 'sublimity-scroll)
    (require 'sublimity-map)
    (require 'sublimity-attractive)
    (sublimity-map-set-delay nil)))

;;;_ , minimap

(use-package minimap
  :disabled t)

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

;; If you’re already running an instance of Emacs in --daemon mode then you can wrap the code in something like:

;;  (when (and (daemonp) (locate-library "edit-server"))
;;    (require 'edit-server)
;;    (edit-server-start))

;;;_ , edit-emacs

(use-package edit-server
  :disabled t
  :load-path "site-lisp/emacs_chrome/servers"
  :if (and window-system (not running-alternate-emacs)
           (not noninteractive))
  :init
  (progn
    ;; (add-hook 'after-init-hook 'server-start t)
    ;; (add-hook 'after-init-hook 'edit-server-start t)
    ))

;;_ ,  el-feed

(use-package elfeed
  :bind ("C-x w" . elfeed))


;;_ ,  emoji-cheat-sheet

(use-package emoji-cheat-sheet
  :commands (emoji-cheat-sheet))

;;;_ , emms

(use-package emms-setup
  :disabled t
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
    (bind-key "C-. C-=" 'emms-player-mplayer-volume-up)
    (add-to-list 'Info-directory-list
                 (expand-file-name "~/.emacs.d/site-lisp/emms/doc") t)
     ;;;  Highlight current line in browser
    (add-hook 'emms-browser-show-display-hook '(lambda () (hl-line-mode 1)))))


;;; Engine Mode:

(use-package engine-mode
  :disabled t
  :load-path "lisp/engine-mode/"
  :commands (engine-mode defengine)
  :init (engine-mode t)
  :config
  (progn
    (defengine duckduckgo
      "https://duckduckgo.com/?q=%s"
      "C-c e d")
    (defengine github
      "https://github.com/search?ref=simplesearch&q=%s"
      "C-c e g")
    (defengine wikipedia
      "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s"
      "C-c e w")))

(use-package eyebrowse
  :commands eyebrowse-mode)

;;;_ , conf-mode
(use-package conf-mode
  :mode ("\\.info\\|\\.gitmodules"  . conf-mode))

;;;_ , cursor-chg

(use-package cursor-chg
  :init
  (progn
    (change-cursor-mode 1)
    (toggle-cursor-type-when-idle 1)))

;;;_ , erc

(use-package erc
  :if running-alternate-emacs
  :init
  (progn
    (defun setup-irc-environment ()
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

      (erc-tls :server "irc.freenode.net"
               :port 6697
               :nick "dkh"
               :password (funcall
                          (plist-get
                           (car (auth-source-search :host "irc.freenode.net"
                                                    :user "dkh"
                                                    :type 'netrc
                                                    :port 6697))
                           :secret))))
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
    (add-hook 'after-init-hook 'irc))

  :config
  (progn
    (erc-track-minor-mode 1)
    (erc-track-mode 1)

    (use-package erc-alert)
    (use-package erc-highlight-nicknames)
    (use-package erc-patch)

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
      (switch-to-buffer "&bitlbee")
      (call-interactively 'erc-channel-names)
      (goto-char (point-max)))

    (bind-key "C-c b" 'switch-to-bitlbee)

    (defcustom erc-foolish-content '()
      "Regular expressions to identify foolish content.
    Usually what happens is that you add the bots to
    `erc-ignore-list' and the bot commands to this list."
      :group 'erc
      :type '(repeat regexp))

    (defun erc-foolish-content (msg)
      "Check whether MSG is foolish."
      (erc-list-match erc-foolish-content msg))

    (add-hook 'erc-insert-pre-hook
              (lambda (s)
                (when (erc-foolish-content s)
                  (setq erc-insert-this nil))))

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

    (defun erc-cmd-BAN (nick &optional redirect whole-ip)
      (let* ((chan (erc-default-target))
             (who (erc-get-server-user nick))
             (host (erc-server-user-host who))
             (user (erc-server-user-login who)))
        (erc-send-command
         (format "MODE %s +b *!%s@%s%s"
                 chan (if whole-ip "*" user) host (or redirect "")))))

    (defun erc-cmd-KICKBAN (nick &rest reason)
      (setq reason (mapconcat #'identity reason " "))
      (and (string= reason "")
           (setq reason nil))
      (erc-cmd-OPME)
      (sleep-for 0 250)
      (erc-cmd-BAN nick)
      (erc-send-command (format "KICK %s %s %s"
                                (erc-default-target)
                                nick
                                (or reason
                                    "Kicked (kickban)")))
      (sleep-for 0 250)
      (erc-cmd-DEOPME))

    (defun erc-cmd-KICKBANIP (nick &rest reason)
      (setq reason (mapconcat #'identity reason " "))
      (and (string= reason "")
           (setq reason nil))
      (erc-cmd-OPME)
      (sleep-for 0 250)
      (erc-cmd-BAN nick nil t)
      (erc-send-command (format "KICK %s %s %s"
                                (erc-default-target)
                                nick
                                (or reason
                                    "Kicked (kickbanip)")))
      (sleep-for 0 250)
      (erc-cmd-DEOPME))

    (defun erc-cmd-KICKTROLL (nick &rest reason)
      (setq reason (mapconcat #'identity reason " "))
      (and (string= reason "")
           (setq reason nil))
      (erc-cmd-OPME)
      (sleep-for 0 250)
      (erc-cmd-BAN nick "$#haskell-ops")
      (erc-send-command (format "KICK %s %s %s"
                                (erc-default-target)
                                nick
                                (or reason
                                    "Kicked (kicktroll)")))
      (sleep-for 0 250)
      (erc-cmd-DEOPME))

    ;; this is essentially a refactored `erc-cmd-KICK'
    (defun erc-cmd-REMOVE (target &optional reason-or-nick &rest reasonwords)
      "Remove a user from the default or specified channel.
    LINE has the format: \"#CHANNEL NICK REASON\" or \"NICK REASON\"."
      (let* ((target-channel-p (erc-channel-p target))
             (channel (if target-channel-p target (erc-default-target)))
             (nick (if target-channel-p reason-or-nick target))
             (reason
              (mapconcat 'identity
                         (or (if target-channel-p reasonwords
                               (and reason-or-nick
                                    (cons reason-or-nick reasonwords)))
                             `("Requested by" ,(erc-current-nick)))
                         " "))
             (server-command (format "REMOVE %s %s :%s" channel nick reason)))
        (if (not channel)
            (erc-display-message nil 'error (current-buffer)
                                 'no-default-channel)
          (erc-log (format "cmd: REMOVE: %s/%s: %s" channel nick reason))
          (erc-server-send server-command))))

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
      t)))

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
        "Creates a tool config shell and switches to it.  If a buffer with name already exists, we simply switch to it."
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
        "Creates a shell with name given by the first argument, and switches
        to it.  If a buffer with name already exists, we simply switch to it."
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

      ;; Support for links to working directories in eshell
      (require 'org-eshell)



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

      (add-hook 'eshell-first-time-mode-hook 'eshell-initialize)
      (add-hook 'eshell-mode-hook
                '(lambda ()
                   (make-local-variable 'project-name)
                   (local-set-key "\C-c\C-q" 'eshell-kill-process)
                   (local-set-key "\C-c\C-k" 'compile))))

    ;; http://www.masteringemacs.org/articles/2012/01/16/pcomplete-context-sensitive-completion-emacs/
    ;;**** Git Completion

    (defun pcmpl-git-commands ()
      "Return the most common git commands by parsing the git output."
      (with-temp-buffer
        (call-process-shell-command "git" nil (current-buffer) nil "help" "--all")
        (goto-char 0)
        (search-forward "available git commands in")
        (let (commands)
          (while (re-search-forward
                  "^[[:blank:]]+\\([[:word:]-.]+\\)[[:blank:]]*\\([[:word:]-.]+\\)?"
                  nil t)
            (push (match-string 1) commands)
            (when (match-string 2)
              (push (match-string 2) commands)))
          (sort commands #'string<))))

    (defconst pcmpl-git-commands (pcmpl-git-commands)
      "List of `git' commands.")

    (defvar pcmpl-git-ref-list-cmd "git for-each-ref refs/ --format='%(refname)'"
      "The `git' command to run to get a list of refs.")

    (defun pcmpl-git-get-refs (type)
      "Return a list of `git' refs filtered by TYPE."
      (with-temp-buffer
        (insert (shell-command-to-string pcmpl-git-ref-list-cmd))
        (goto-char (point-min))
        (let ((ref-list))
          (while (re-search-forward (concat "^refs/" type "/\\(.+\\)$") nil t)
            (add-to-list 'ref-list (match-string 1)))
          ref-list)))

    (defun pcomplete/git ()
      "Completion for `git'."
      ;; Completion for the command argument.
      (pcomplete-here* pcmpl-git-commands)
      (cond
       ((pcomplete-match (regexp-opt '("add" "rm" "mv")) 1)
        (while (pcomplete-here (pcomplete-entries))))
       ((pcomplete-match "help" 1)
        (pcomplete-here* pcmpl-git-commands))
       ;; provide branch completion for the command `checkout'.
       ((pcomplete-match "checkout" 1)
        (pcomplete-here* (pcmpl-git-get-refs "heads")))))))

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

;; expand-region

(use-package expand-region
  :bind ("C-=" . er/expand-region)
  :config
  (progn
    (use-package change-inner
      :load-path "lisp/change-inner.el/"
      :bind (("M-i" . change-inner)
             ("M-o" . change-outer)))
    (defun er/add-text-mode-expansions ()
      (make-variable-buffer-local 'er/try-expand-list)
      (setq er/try-expand-list (append
                                er/try-expand-list
                                '(mark-paragraph
                                  mark-page))))

    (add-hook 'text-mode-hook 'er/add-text-mode-expansions)))


;;; I use these packages to navigate and edit text in semantic terms,
;;; with the Expand Region package being the foundation for the rest.

(use-package expand-region
  :load-path "lisp/expand-region.el/"
  :bind ("C-=" . er/expand-region)
  :config
  (progn
    (use-package change-inner
      :bind (("M-i" . change-inner)
             ("M-o" . change-outer)))))

;;;_ , fancy-narrow

(use-package fancy-narrow)

;;;_ , features-mode

(use-package feature-mode
  :mode ("\\.feature$" . feature-mode))

;;;_ , fetchmail-mode

(use-package fetchmail-mode
  :mode (".fetchmailrc$" . fetchmail-mode)
  :commands fetchmail-mode
  )

;;;_ , ipretty

(use-package ipretty
  :bind (("C-h C-j" . ipretty-last-sexp)
         ("C-h C-k" . ipretty-last-sexp-other-buffer)))

;;;_ , flyspell

(use-package ispell
  :bind (("C-c i c" . ispell-comments-and-strings)
         ("C-c i d" . ispell-change-dictionary)
         ("C-c i k" . ispell-kill-ispell)
         ("C-c i m" . ispell-message)
         ("C-c i r" . ispell-region)))

(use-package flyspell
  :bind (("C-c i b" . flyspell-buffer)
         ("C-c i f" . flyspell-mode))
  :config
  (define-key flyspell-mode-map [(control ?.)] nil))


;;;_ , flycheck

(use-package flycheck
  :init (global-flycheck-mode))
  ;; :init
  ;;   (hook-into-modes #'flycheck-mode '(prog-mode-hook)))

;;;_ , google-this

(use-package google-this
  :commands (google-region google-translate-query-or-region google-error google-forecast google-lucky-search google-lucky-and-insert-url google-line google-maps google-cpp-reference google-symbol google-this google-word google-search)
  :diminish google-this-mode
  :config
  (google-this-mode))

;;;_ , google-translate

;; (use-package google-translate
;;   :bind (
;;          ("\C-ct" . google-translate-at-point)
;;          ("\C-cT" . google-translate-query-translate))
;;   :init
;;   (progn
;;         (require 'google-translate-default-ui)))


(use-package google-translate
  :bind ("\C-ct" . google-translate-smooth-translate)
  :init
  (progn
        (require 'google-translate-smooth-ui)
        (setq google-translate-translation-directions-alist '(("ja" . "en")))))

;;;_ , guide-key

(use-package guide-key
  :init
  (progn
    (setq guide-key/guide-key-sequence '("C-x r" "C-x 4" "C-c"))
    (guide-key-mode 1))
  :config
  (use-package guide-key-tip
    :init
    (progn
      (require 'pos-tip)
      (setq guide-key-tip/enabled t))))

;;;_ , highlight-sexp

(use-package hl-sexp
  :commands hl-sexp-mode
  :config
  (progn
    (hook-into-modes 'hl-sexp-mode
                     '(prog-mode-hook))))

;;;_ , highlight-symbol
(use-package highlight-symbol
  :disabled t
  :commands (highlight-symbol-prev highlight-symbol-next highlight-symbol-at-point highlight-symbol-query-replace))

;;;_ , highlight-tail
(use-package highlight-tail
  :commands highlight-tail-mode)

;;;_ , highlight-indentation

(use-package highlight-indentation
  :commands highlight-indentation-mode
  :config
  (progn
    (set-face-background 'highlight-indentation-face "#e3e3d3")
    (set-face-background 'highlight-indentation-current-column-face "#c3b3b3")))

;;;_ , ggtags

(use-package ggtags
  :commands ggtags-mode
  :diminish ggtags-mode
  :config
  (progn
    (defun my-gtags-or-semantic-find-tag ()
      (interactive)
      (if (and (fboundp 'semantic-active-p)
               (funcall #'semantic-active-p))
          (call-interactively #'semantic-complete-jump)
        (call-interactively #'ggtags-find-tag-dwim)))

    (bind-key "M-." 'my-gtags-or-semantic-find-tag ggtags-mode-map)

    (use-package helm-gtags
      :bind ("M-T" . helm-gtags-select))))

;;;_ , gist

(use-package gist
  :bind ("C-c G" . gist-region-or-buffer))

;;;_ , gnus
(use-package dot-gnus
  :if (not running-alternate-emacs)
  :bind (("M-G"   . switch-to-gnus)
         ("C-x m" . compose-mail))
  :init
  (progn
    (setq gnus-init-file (expand-file-name "dot-gnus" user-emacs-directory)
          gnus-home-directory "~/Messages/Gnus/"))
  :config
  (progn
    (use-package org-mime)
    (setq org-mime-library 'mml)))

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
        (progn
          (setq-default grep-first-column 1)
          (grep-apply-setting
           'grep-find-command
           '("ag --noheading --nocolor --smart-case --nogroup --column -- "
             . 61)))
      (grep-apply-setting
       'grep-find-command
       '("find . -name '*.hs' -type f -print0 | xargs -P4 -0 egrep -nH "
         . 62)))))

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
  :init
  (progn
    (bind-key "C-c M-x" 'helm-M-x)
    (bind-key "C-h a" 'helm-apropos)
    (bind-key "M-s a" 'helm-do-grep)
    (bind-key "M-s s" 'helm-do-grep)
    (bind-key "M-s b" 'helm-occur)
    (bind-key "M-s F" 'helm-for-files)

    ;; (bind-key "M-s f" 'helm-find-files)

    (bind-key "M-s r" 'helm-resume)
    (bind-key "M-s B" 'helm-bookmarks)
    (bind-key "M-s l" 'helm-buffers-list)

    (require 'grizzl)
    (use-package helm-commands)

    (bind-key "C-h e a" 'my-helm-apropos)
    (bind-key "C-x M-!" 'helm-command-from-zsh)
    (bind-key "C-x C-b" 'helm-buffers-list)
    (bind-key "C-x f" 'helm-find-git-file)

    (use-package helm-descbinds
      :commands helm-descbinds
      :init
      (fset 'describe-bindings 'helm-descbinds))

    (bind-key "C-h b" 'helm-descbinds)

    (bind-key "M-s ." 'helm-command-from-bash)

    (defadvice helm-buffers-list
        (around expand-window-helm-buffers-list activate)
      (let ((c (current-window-configuration)))
        (condition-case err
            (progn
              (delete-other-windows)
              ad-do-it)
          (t
           (set-window-configuration c)))))

    (use-package helm-css-scss)

    (use-package helm-ag
      :disabled t
      :commands (helm-ag projectile-helm-ag)
      :bind (("M-s <escape>"  . projectile-helm-ag)
             ("M-s A"  . helm-ag)
             ("M-s A"  . helm-ag))
      :config
      (progn
        (defun projectile-helm-ag ()
          (interactive)
          (helm-ag (projectile-project-root)))
        (bind-key "M-s <escape>" 'projectile-helm-ag)))

    (bind-key "C-h b" 'helm-descbinds)

    (use-package helm-open-github
      :bind (("C-. o f" . helm-open-github-from-file)
             ("C-. o c" . helm-open-github-from-commit)
             ("C-. o i" . helm-open-github-from-issues)
             ("C-. o p" . helm-open-github-from-pull-requests)))

    (use-package helm-dash
      :load-path "site-lisp/esqlite/Emacs-pcsv"
      :init
      (helm-dash-activate-docset "Drupal"))

    (helm-match-plugin-mode t)

    (helm-autoresize-mode 1)
    (bind-key "<tab>" 'helm-execute-persistent-action helm-map)
    (bind-key "C-i" 'helm-execute-persistent-action helm-map)
    (bind-key "C-z" 'helm-select-action helm-map)))

;;;_ , helm-dash



;;;_ , helm-itunes

(use-package helm-itunes
  :commands (helm-itunes))

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
  :commands hl-line-mode
  :bind ("M-o h" . hl-line-mode)
  :config
  (progn
  (use-package hl-line+)))

(use-package ibuffer
  :disabled t
  :bind ("C-x C-b" . ibuffer)
  :init
  (add-hook 'ibuffer-mode-hook
            #'(lambda ()
                (ibuffer-switch-to-saved-filter-groups "default"))))

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
  ;; (ido-mode (quote both))

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

    (bind-key "C-x 5 t" 'ido-switch-buffer-tiny-frame)))

;;;_ , iedit

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

;;;_ , indent-guide

(use-package indent-guide
  :init
  (progn
    (set-face-background 'indent-guide-face "#e3e3d3")))

;;;_ , info

(use-package info
  :bind ("C-h C-i" . info-lookup-symbol)
  :init
  (remove-hook 'menu-bar-update-hook 'mac-setup-help-topics)

  :config
  (progn
    ;; (defadvice info-setup (after load-info+ activate)
    ;;   (use-package info+))

    (defadvice Info-exit (after remove-info-window activate)
      "When info mode is quit, remove the window."
      (if (> (length (window-list)) 1)
          (delete-window)))))

(use-package info-look
  :commands info-lookup-add-help)

;;;_ , indirect

(use-package narrow-indirect
  :init
  (progn
    (define-key ctl-x-4-map "nd" 'ni-narrow-to-defun-other-window)
    (define-key ctl-x-4-map "nn" 'ni-narrow-to-region-other-window)
    (define-key ctl-x-4-map "np" 'ni-narrow-to-page-other-window)))

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

(use-package js2-mode
  :mode ("\\.js\\'" . js2-mode))

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
(use-package "ledger-mode"
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

;;;_ , linked-buffer

(use-package linked-buffer
  :disabled t)

;;;_ , lisp-mode

;; Utilities every Emacs Lisp coder should master:
;;
;;   paredit          Lets you manipulate sexps with ease
;;   redshank         Think: Lisp refactoring
;;   edebug           Knowing the traditional debugger is good too
;;   eldoc
;;   cldoc
;;   elint
;;   elp
;;   ert
;;   ielm

(use-package lisp-mode
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

      (yas-minor-mode 1))

    (hook-into-modes #'my-lisp-mode-hook lisp-mode-hooks)))


;;;_ , lorem-ipsum
(use-package lorem-ipsum
  :commands (Lorem-ipsum-insert-paragraphs
             Lorem-ipsum-insert-sentences
             Lorem-ipsum-insert-list)
  )

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
  (progn
    (defun magit-status-with-prefix ()
      (interactive)
      (let ((current-prefix-arg '(4)))
        (call-interactively 'magit-status)))

    (defun eshell/git (&rest args)
      (cond
       ((or (null args)
            (and (string= (car args) "status") (null (cdr args))))
        (magit-status default-directory))
       ((and (string= (car args) "log") (null (cdr args)))
        (magit-log))
       (t (throw 'eshell-replace-command
                 (eshell-parse-command
                  (concat "*" command)
                  (eshell-stringify-list (eshell-flatten-list args)))))))

    (add-hook 'magit-mode-hook 'hl-line-mode)

    (use-package magit-blame
      :commands magit-blame-mode)

    (use-package git-messenger
      :bind (("C-x v p" . git-messenger:popup-message))
      :config (define-key git-messenger-map (kbd "m") 'git-messenger:copy-message)))
  :config
  (progn
    (setenv "GIT_PAGER" "")

    (use-package magit-review
      :commands magit-review
      :config (require 'json))

    (unbind-key "M-h" magit-mode-map)
    (unbind-key "M-s" magit-mode-map)

    (add-hook 'magit-log-edit-mode-hook
              #'(lambda ()
                  (set-fill-column 72)
                  (flyspell-mode)))

    (require 'magit-topgit)
    (require 'git-rebase-mode)
    (require 'magit-blame)

    (global-set-key (kbd "C-x v b") 'magit-blame-mode)
    (defun magit-blame-this-file()
      "visit file and call magit-blame-mode"
      (interactive)
      (magit-visit-item)
      (magit-blame-mode))

    (defvar magit-git-monitor-process nil)
    (make-variable-buffer-local 'magit-git-monitor-process)

    (defun start-git-monitor ()
      (interactive)
      (unless magit-git-monitor-process
        (setq magit-git-monitor-process
              (start-process "git-monitor" (current-buffer) "git-monitor"
                             "-d" (expand-file-name default-directory)))))

    ;; (add-hook 'magit-status-mode-hook 'start-git-monitor)
))

;;;_ , magit-annex

(use-package magit-annex
  :defer t)

(use-package github-browse-file
  :bind ("H-o" . github-browse-file))

;;;_ , manage-minor-mode

(use-package manage-minor-mode
  :commands (manage-minor-mode))

;;;_ , markdown-mode

(use-package markdown-mode
  :mode (("\\`README\\.md\\'" . gfm-mode)
         ("\\.md\\'"          . markdown-mode)
         ("\\.markdown\\'"    . markdown-mode)))

;;;_ , mouse+

(use-package mouse+
  :init
  (progn
  (global-set-key [mouse-5]        'mouse-flash-position-or-M-x)
   (global-set-key [S-down-mouse-2]      'mouse-scan-lines-or-M-:)
  (global-set-key [mode-line C-mouse-1] 'mouse-tear-off-window)
  ))

;;;_ , mudel
(use-package mudel
  :disabled t
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
    (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))
    ;; set proper language (fixes cyrillic letters in ansi-term)
    ;; (setenv "LANG" "ru_RU.UTF-8")
    (set-fontset-font t 'unicode "Symbola" nil 'prepend)
    ;; override font for cyrillic characters
    (set-fontset-font t 'cyrillic "Droid Sans Mono")))

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
      (define-key term-pager-break-map  "\177" 'term-pager-back-page))

    (add-hook 'term-mode-hook
              (lambda ()
                (goto-address-mode)
                (define-key term-raw-map (kbd "C-y") 'term-paste)))

    (setenv "PATH" (shell-command-to-string "echo $PATH"))))

(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)

         ("C->"     . mc/mark-next-like-this)
         ("C-<"     . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this))
  :init
  (setq mc/list-file (expand-file-name "mc-lists.el" user-data-directory)))

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
                             "-xml" "-i" "-wrap" "0" "-omit" "-q" "-utf8")))

    (bind-key "C-c M-h" 'tidy-xml-buffer nxml-mode-map)))


;; ;;;_ , o-blog

(use-package o-blog
  :disabled t
  :commands (o-blog-publish
             o-blog-tangle-publish-and-view)
  :config
  (progn
    (defvar o-blog-local-site "~/Sites/dev")
    (defvar o-blog-out-dir "out")
    (defvar o-blog-local-url-index "http://127.0.0.1/~username/dev/index.html")

    (defun o-blog-publish ()
      (interactive)
      "publish blog locally"
      (org-publish-blog buffer-file-name)
      (if (file-exists-p o-blog-local-site)
          (delete-directory o-blog-local-site t))
      (copy-directory
       (format "%s%s" default-directory o-blog-out-dir) o-blog-local-site))

    (defun o-blog-tangle-publish-and-view ()
      (interactive)
      "tangle template  and style files in current buffer, publish blog locally
and view local index.html url"
      (org-babel-tangle-file buffer-file-name)
      (o-blog-publish)
      (browse-url o-blog-local-url-index)))

  :bind (("C-c C-v ," . o-blog-publish)
         ("C-c C-v ." . o-blog-tangle-publish-and-view)))


;; ;;;_ , org-mode

(use-package dot-org
  :commands org-agenda-list
  :bind (("M-C"   . jump-to-org-agenda)
         ("M-m"   . org-smart-capture)
         ("M-M"   . org-inline-note)
         ("C-c a" . org-agenda)
         ("C-c S" . org-store-link)
         ("C-c l" . org-insert-link))
  :init
  (progn

    (unless (boundp 'Info-directory-list)
      (setq Info-directory-list Info-default-directory-list))
    (setq Info-directory-list
          (cons (expand-file-name
                 "doc"
                 (expand-file-name
                  "org-mode"
                  (expand-file-name "override" user-emacs-directory)))
                Info-directory-list))

    (unless running-alternate-emacs
      (run-with-idle-timer 3600 t 'jump-to-org-agenda))

    (unless running-alternate-emacs
      ;; (run-with-idle-timer 300 t 'jump-to-org-agenda)
      (add-hook 'after-init-hook
                #'(lambda ()
                    (org-agenda-list)
                    (org-fit-agenda-window)
                    (org-resolve-clocks))
                (when (fboundp 'auto-dim-other-buffers-mode)
                  (auto-dim-other-buffers-mode t))) t))
  :config
  (progn
    (defun org-cycle-current-entry ()
      "toggle visibility of current entry from within the entry."
      (interactive)
      (save-excursion)
      (outline-back-to-heading)
      (org-cycle))

    (define-key org-mode-map (kbd "C-c C-/") 'org-cycle-current-entry)


    (defun org-select-heading ()
      "Go to heading of current node, select heading."
      (interactive)
      (outline-previous-heading)
      (search-forward (plist-get (cadr (org-element-at-point)) :raw-value))
      (set-mark (point))
      (beginning-of-line)
      (search-forward " "))

    (define-key org-mode-map (kbd "C-c C-h") 'org-select-heading)

    (fset 'org-toggle-drawer
          (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([67108896 3 16 14 tab 24 24] 0 "%d")) arg)))

    (fset 'org-cut-subtree-append
          "\227\C-ck")

    (define-key org-mode-map (kbd "C-c M-d") 'org-toggle-drawer)

    (define-key org-mode-map (kbd "C-c M-w") 'org-cut-subtree-append)))

;;;_ , org-jira

(use-package org-jira
  :disabled t
  :load-path ("~/.emacs.d/lisp/org-jira")
  :init
  (progn

    (setq
     org-jira-working-dir "~/Documents/Tasks/.org-jira")))

;;;_ , org-link-minor-mode

(use-package org-link-minor-mode
  :commands org-link-minor-mode)

;;;_ , outline-mode

;; (use-package outline
;;   ;; :diminish outline-mode
;;   :init (add-hook 'prog-mode-hook 'outline-minor-mode)
;;   :config
;;   (progn
;;     (use-package outshine
;;       :commands outshine-hook-function
;;       :diminish ""
;;       :init
;;       (progn
;;         (add-hook  'outline-minor-mode-hook 'outshine-hook-function)))))

;; (use-package outorg
;;   :commands outorg-edit-as-org
;;   :config
;;   (defun outshine-use-outorg (fun &optional whole-buffer-p &rest funargs)
;;     "Use outorg to call FUN with FUNARGS on subtree.

;; FUN should be an Org-mode function that acts on the subtree at
;; point. Optionally, with WHOLE-BUFFER-P non-nil,
;; `outorg-edit-as-org' can be called on the whole buffer.

;; Sets the variable `outshine-use-outorg-last-headline-marker' so
;; that it always contains a point-marker to the last headline this
;; function was called upon.

;; The old marker is removed first. Then a new point-marker is
;; created before `outorg-edit-as-org' is called on the headline."
;;     (save-excursion
;;       (unless (outline-on-heading-p)
;;         (outline-previous-heading))
;;       (outshine--set-outorg-last-headline-marker)
;;       (if whole-buffer-p
;;           (outorg-edit-as-org '(4))
;;         (outorg-edit-as-org))
;;       (if funargs
;;           (funcall fun funargs)
;;         (funcall fun))
;;       (outorg-copy-edits-and-exit))))

;;;_ , owdriver
(use-package owdriver
  :init
  (progn
    (require 'smartrep)
    (require 'log4e)
    (require 'yaxception)

    (global-unset-key (kbd "M-o"))
    (setq owdriver-prefix-key "M-o")
    (owdriver-config-default)
    (owdriver-mode 1)))

;;;_ , pabbrev

(use-package pabbrev
  :commands pabbrev-mode
  :diminish pabbrev-mode)

;;;_ , pandoc-mode

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
    ;;    (bind-key "M-h" 'mark-containing-sexp paredit-mode-map)
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

    (defun paredit-wrap-round-from-behind ()
      (interactive)
      (forward-sexp -1)
      (paredit-wrap-round)
      (insert " ")
      (forward-char -1))

    (define-key paredit-mode-map (kbd "M-)")
      'paredit-wrap-round-from-behind)

    (defun paredit--is-at-start-of-sexp ()
      (and (looking-at "(\\|\\[")
           (not (nth 3 (syntax-ppss))) ;; inside string
           (not (nth 4 (syntax-ppss))))) ;; inside comment

    (defun paredit-duplicate-closest-sexp ()
      (interactive)
      ;; skips to start of current sexp
      (while (not (paredit--is-at-start-of-sexp))
        (paredit-backward))
      (set-mark-command nil)
      ;; while we find sexps we move forward on the line
      (while (and (bounds-of-thing-at-point 'sexp)
                  (<= (point) (car (bounds-of-thing-at-point 'sexp)))
                  (not (= (point) (line-end-position))))
        (forward-sexp)
        (while (looking-at " ")
          (forward-char)))
      (kill-ring-save (mark) (point))
      ;; go to the next line and copy the sexprs we encountered
      (paredit-newline)
      (yank)
      (exchange-point-and-mark))

    ;; making paredit work with delete-selection-mode
    (put 'paredit-forward-delete 'delete-selection 'supersede)
    (put 'paredit-backward-delete 'delete-selection 'supersede)
    (put 'paredit-open-round 'delete-selection t)
    (put 'paredit-open-square 'delete-selection t)
    (put 'paredit-doublequote 'delete-selection t)

    (add-hook 'allout-mode-hook
              #'(lambda ()
                  (bind-key "M-k" 'paredit-raise-sexp allout-mode-map)
                  ;;                  (bind-key "M-h" 'mark-containing-sexp allout-mode-map)
                  ))))

;;;_ , paren

(unless (use-package mic-paren
          :init
          (paren-activate))
  (use-package paren
    :init
    (show-paren-mode 1)))

;;;_ , parenface-plus

(use-package parenface-plus)

;;;_ , per-window-point

(use-package per-window-point
  :init
  (pwp-mode 1))

;;;_ , persistent-scratch

(use-package persistent-scratch
  :if (and window-system (not running-alternate-emacs)
           (not noninteractive)))

;;;_ , php-mode

(use-package php-mode
  :commands php-mode
  :mode (
         ("\\.inc$" . php-mode)
         ("\\.\\(module\\|test\\|install\\|theme\\|\\profile\\)$" . php-mode))
  :interpreter "php"
  :init
  (progn

    (use-package conf-mode
      :mode "\\.info")

    (use-package emmet-mode
      :commands emmet-mode
      :init
      (progn
        (add-hook 'nxml-mode-hook 'emmet-mode)
        (add-hook 'html-mode-hook 'emmet-mode)
        (add-hook 'html-mode-hook
                  #'(lambda ()
                      (bind-key "<return>" 'newline-and-indent html-mode-map)))
        ;; (add-hook 'web-mode-hook 'emmet-mode)
        )

      :config
      (progn
        (defvar emmet-mode-keymap (make-sparse-keymap))
        (bind-key "C-c C-c" 'emmet-expand-line emmet-mode-keymap))))
  :config
  (progn
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
                 (ggtags-mode 1)
                 (diminish 'ggtags-mode)
                 (define-abbrev php-mode-abbrev-table "ex" "extends")
                 (hs-minor-mode 1)
                 (turn-on-eldoc-mode)
                 (diminish 'hs-minor-mode)
                 (setq indicate-empty-lines t)
                 'my-php-mode-hook
                 (local-set-key "\r" 'my-php-return)
                 (local-unset-key (kbd "C-c ."))
                 (paren-toggle-open-paren-context 1)))

    (bind-key "C-c C-F" 'php-search-local-documentation)
    (use-package php-extras
      :init
      (progn
        (require 'php-extras-gen-eldoc)
        (php-extras-autocomplete-setup)
        (php-extras-eldoc-setup)))

    (use-package php-boris)

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
    (use-package php-auto-yasnippets)

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
        (backward-char 3)))))

;;;_ , pretty-mode

(use-package pretty-mode)

;;;_ , projectile

(use-package projectile
  :diminish projectile-mode
  :init
  (progn
    (projectile-global-mode)

    (defun define-keys (mode-map keybindings)
      "Takes a mode map, and a list of (key function-designator)
lists.  The functions are bound to the keys in the given mode-map.
Keys are in kbd format."
      (mapc (lambda (keybinding)
              (destructuring-bind (key function) keybinding
                (define-key mode-map (read-kbd-macro key) function)))
            keybindings))

     (use-package ag
       :init
       (progn
         (bind-key "C-. a" 'ag)
         (use-package wgrep-ag)))

    (use-package projectile-drupal
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

    (defun projectile-switch-to-last-project ()
      (interactive)
      (funcall projectile-switch-project-action projectile-last-project-root))

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

    (use-package perspective
      :init
      (progn
        (persp-mode)
        (use-package persp-projectile
          :bind ("C-\\" . projectile-persp-switch-project)
          )))

    (use-package helm-projectile
      :init
      (progn
        (helm-projectile-on)
        (bind-key "M-s P" 'helm-projectile)))))

;;;_ , popup-ruler

(use-package popup-ruler
  :bind (("C-. r" . popup-ruler)
         ("C-. R" . popup-ruler-vertical)))

;;;_ , pp-c-l

(use-package pp-c-l
  :init
  (hook-into-modes 'pretty-control-l-mode '(prog-mode-hook)))

;;;_ , fic-mode
(use-package fic-mode
  :init
  (hook-into-modes 'fic-mode '(prog-mode-hook)))

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
  :bind ("C-c C-r" . quickrun)
  :config
  (progn

    (defface phpunit-pass
      '((t (:foreground "white" :background "green" :weight bold))) nil)
    (defface phpunit-fail
      '((t (:foreground "white" :background "red" :weight bold))) nil)

    (defun quickrun/phpunit-outputter ()
      (save-excursion
        (goto-char (point-min))
        (while (replace-regexp "^M" "")
          nil))
      (highlight-phrase "^OK.*$" 'phpunit-pass)
      (highlight-phrase "^FAILURES.*$" 'phpunit-fail))

    (quickrun-add-command "phpunit" '((:command . "phpunit")
                                      (:exec . "%c %s")
                                      (:outputter .
                                      quickrun/phpunit-outputter)))

    (add-to-list 'quickrun-file-alist '("Test\\.php$" . "phpunit"))

    (quickrun-add-command "phing" '((:command . "phing")
                                      (:exec . "%c %s")
                                      (:outputter .
                                      quickrun/phpunit-outputter)))

    (add-to-list 'quickrun-file-alist '("Test\\.php$" . "phpunit"))))


;;;;_ , rainbow-delimiters

(use-package rainbow-delimiters
  :load-path "site-lisp/rainbow-delimiters"
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
                       scss-mode-hook
                       stylus-mode-hook)))
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

;;;_ , rotate-text

;; rotate-text allows you rotate to commonly interchanged text with a single keystroke. For example, you can toggle between frame-width and frame-height, between public, protected and private and between variable1, variable2 through variableN.

(use-package rotate-text
  :commands (rotate-text rotate-text-backward))

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

      (set (make-local-variable 'yas-fallback-behavior)
           '(apply ruby-indent-command . nil))
      (bind-key "<tab>" 'yas-expand-from-trigger-key ruby-mode-map))

    (add-hook 'ruby-mode-hook 'my-ruby-mode-hook)))

;; Saveplace

;; - places cursor in the last place you edited file
(use-package saveplace
  :config
  (progn
    (setq-default save-place t)
    ;; Keep places in the load path
    (setq save-place-file "~/Documents/places")))

;;;_ , sass-mode

(use-package sass-mode
  :disabled t
  :mode ("\\.scss\\|\\.sass$" . sass-mode)
  :init
  (
   (use-package haml-mode)
   (add-hook 'sass-mode-hook
             (lambda ()
               (defun compass-compile-hook ()
                 (if (and (buffer-file-name)
                          (file-name-extension (buffer-file-name)))
                     (let* ((filename (buffer-file-name))
                            (suffix (downcase (file-name-extension filename))))
                       (if (and filename (string= suffix "scss"))
                           (compass-compile-project)))))
               (add-hook 'after-save-hook 'compass-compile-hook)))

   (defun compass-compile-project ()
     "Search the file-tree up from the current file looking for config.rb
and run compass from that directory"
     (interactive)
     (let* ((sass-file (buffer-file-name (current-buffer)))
            (local-dir (file-name-directory sass-file)))
       (cl-cl-flet ((contains-config-rb (dir-name)
                                  (find "config.rb" (directory-files dir-name)
                                        :test 'equal))
              (parent-dir (dir)
                          (expand-file-name
                           (file-name-as-directory (concat
                                                    (file-name-as-directory dir)
                                                    ".."))))
              (get-root (d)
                        (cond ((contains-config-rb d) d)
                              ((equal (parent-dir d) d) nil)
                              ((get-root (parent-dir d)))))

              (run-compass (dir)
                           (let ((default-directory dir))
                             (start-file-process "compass"
                                                 "*compass-process*"
                                                 "compass"
                                                 "compile"))))
         (let (( dir (get-root local-dir)))
           (if dir
               (run-compass dir)
             (princ (format "no config from %s" local-dir)))))))))

;;;_ , scss-mode

(use-package scss-mode
  :mode ("\\.scss\\'" . scss-mode)
  :init
  (progn
    ;; auto-comple
    ;; (add-to-list 'ac-modes 'scss-mode)
    )
  :config
   ;; (ac-emmet-css-setup)
   )

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

;;;_ , shift-text
(use-package shift-text
  :commands (shfit-text-right shfit-text-left shift-text-up shift-text-down)
  :bind (("<M-right>" . shift-text-right)
         ("<M-left>" .  shift-text-left)
         ("<M-up>" .  shift-text-up)
         ("<M-down>" .  shift-text-down))
  )

;;;_ , sh-mode
(use-package sh-mode
  :mode ("\\.bashrc\\|\\.bash_alias\\|\\.sh\\|.bash_history\\|alias$" . sh-mode))


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

    (add-hook 'shell-mode-hook
              (lambda ()
                (initialize-sh-script)
                (ansi-color-for-comint-mode-on)))

    (defun comint-kill-output-to-kill-ring ()
      "Kills all output from last command and puts it in kill buffer
Does not delete the prompt."
      (interactive)
      (let ((proc (get-buffer-process (current-buffer)))
            (replacement nil)
            (inhibit-read-only t))
        (save-excursion
          (let ((pmark (progn (goto-char (process-mark proc))
                              (forward-line 0)
                              (point-marker))))
            ;; Add the text to the kill ring.
            (copy-region-as-kill comint-last-input-end pmark)
            (delete-region comint-last-input-end pmark)
            (goto-char (process-mark proc))
            (setq replacement (concat "*** output flushed to kill ring ***\n"
                                      (buffer-substring pmark (point))))
            (delete-region pmark (point))))
        ;; Output message and put back prompt
        (comint-output-filter proc replacement)))
    (bind-key "C-c `" 'comint-kill-output-to-kill-ring)))

;;;_ , sh-toggle

(use-package sh-toggle
  :bind ("C-. C-z" . shell-toggle))

;;;_ , slime

(use-package slime
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
              "~/Library/Lisp/sbcl.core-with-slime-X86-64")
             :init
             (lambda (port-file _)
               (format "(swank:start-server %S)\n" port-file)))
            (ecl ("ecl" "-load" "~/Library/Lisp/init.lisp"))
            (clisp ("clisp" "-i" "~/Library/Lisp/lwinit.lisp"))))

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
      :init
      (setq common-lisp-hyperspec-root
            (expand-file-name "~/Library/Lisp/HyperSpec/")))))

;;;; show-css
(use-package show-css
  :commands (showcss-mode)
  :init
  (progn
    (defun toggle-showcss()
      "Toggle showcss-mode"
      (interactive)
      (if (derived-mode-p
           'html-mode 'nxml-mode 'nxhtml-mode 'web-mode)
          (showcss-mode 'toggle)
        (message "Not in an html mode")))))

;;;_ , smart-compile

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

(bind-key "M-O" 'show-compilation)

;;;; skewer-mode

(use-package skewer-mode
  :init
  (progn
    (use-package skewer-repl)
    (use-package skewer-setup)
    (use-package skewer-html)
    (use-package skewer-css
      :init
    (hook-into-modes #'skewer-css-mode
                     '(css-mode-hook
                       scss-mode-hook)))

    (add-hook 'js2-mode-hook 'skewer-mode)
    (add-hook 'html-mode-hook 'skewer-html-mode)
    ;; (add-hook 'web-mode-hook
    ;;           (lambda ()
    ;;           (skewer-html-mode 1)
    ;;           (skewer-css-mode 1)))
    )
  :config
  (progn
        (defun skewer-reload-page ()
      "Reloads browser."
      (interactive)
       (silent-save-some-buffers)
      (skewer-eval "window.location = window.location"))
    (defun skewer-scroll-down ()
      "Scroll down"
      (interactive)
      (skewer-eval "window.scrollBy(0,200);"))
    (defun skewer-scroll-up ()
      "Scroll down"
      (interactive)
      (skewer-eval "window.scrollBy(0,-200);"))
    (defun skewer-scroll-left ()
      "Scroll down"
      (interactive)
      (skewer-eval "window.scrollBy(-100,0);"))
    (defun skewer-scroll-right ()
      "Scroll down"
      (interactive)
      (skewer-eval "window.scrollBy(100,0);"))))

;;;_ , smart-mode-line

(use-package smart-compile
  :disabled t
  :commands smart-compile
  :bind (("C-c c" . smart-compile)
         ("A-n"   . next-error)
         ("A-p"   . previous-error)))

;;;_ , smart-mode-line

(use-package smart-mode-line
  :disabled t
  :config
  (progn
    (add-to-list 'sml/replacer-regexp-list '("^~/data/web/custom" ":Custom:"))
    (add-to-list 'sml/replacer-regexp-list '("^~/.emacs.d" ":Yak:"))
    (add-to-list 'sml/replacer-regexp-list '("^~/data/releases" ":Releases:"))
    (add-to-list 'sml/replacer-regexp-list '("^~/Documents/Tasks/" ":Task:"))
    (sml/setup)))


;;;_ , smartparens

(use-package smartparens
  :commands (smartparens-mode show-smartparens-mode)
  :config (require 'smartparens-config))

(use-package smerge-mode
  :commands (smerge-mode smerge-command-prefix)
  :init
  (setq smerge-command-prefix (kbd "C-. C-.")))

;;;_ , isql

(use-package sql
  :init
  (progn
    (sql-set-product 'mysql)
    ;; (load-library "sql-indent")
    ))

;;;_ , stripe-buffer

(use-package stripe-buffer)

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


;;;_ , switch-window
(use-package switch-window
  :bind ("C-x o" . switch-window))

;;;_ , tablegen-mode

(use-package tablegen-mode
  :mode ("\\.td\\'" . tablegen-mode))

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

;;;_ , textexpander

;; (when (= 0 (call-process "using-textexpander"))
;; (bind-key "A-v" 'scroll-down)
(bind-key "H-v" 'yank)
;; (bind-key "M-v" 'scroll-down)
;; )

;; (bind-key "M-v" 'scroll-down)
;; (bind-key "A-v" 'yank)


;; https://github.com/anthracite/emacs-config/blob/master/init.el
;;;;_ , twittering-mode

(use-package twittering-mode
  :disabled t
  :commands twit
  :config
  (progn
    (setq twittering-icon-mode t
          twittering-timer-interval 150
          twittering-number-of-tweets-on-retrieval 100
          Twittering-use-ssl t
          twittering-use-master-password t
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

;;;_ , unbound

(use-package unbound)

;;;_ , undo-tree

(use-package undo-tree
  :disabled t
  :diminish (undo-tree-mode global-undo-tree-mode)
  :init
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t)))

;;;_ , VKILL

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

;;;_ , volatile-highlights

(use-package volatile-highlights
  :init (volatile-highlights-mode t)
  :diminish volatile-highlights-mode)

;;;_ , w3m

(use-package w3m
  :commands (w3m-search w3m-find-file)
  :bind (("C-. u"   . w3m-browse-url)
         ("C-. U"   . w3m-browse-url-new-session)
         ("C-. H-u" . w3m-browse-chrome-url-new-session)
         )
  :init
  (progn
    (setq w3m-command "/usr/local/bin/w3m")

    (setq w3m-coding-system 'utf-8
          w3m-file-coding-system 'utf-8
          w3m-file-name-coding-system 'utf-8
          w3m-input-coding-system 'utf-8
          w3m-output-coding-system 'utf-8
          w3m-terminal-coding-system 'utf-8)

    (add-hook 'w3m-mode-hook 'w3m-lnum-mode)

    (autoload 'w3m-session-crash-recovery-remove "w3m-session")

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
      (let ((url (mac-do-applescript
                  (string-to-multibyte "tell application \"Google Chrome\"
  URL of active tab of front window
  end tell"))))
        (w3m-browse-url (substring url 1 (1- (length url))) t)))

    (defun choose-browser (url &rest args)
      (interactive "sURL: ")
      (if current-prefix-arg
          (w3m-browse-url url)
        (let ((browse-url-browser-function 'browse-url-default-macosx-browser))
          (browse-url url))))

    (use-package popwin-w3m
      :load-path "site-lisp/popwin/misc/")

    (bind-key "H-M-e" 'goto-emacswiki)
    (bind-key "H-M-g" 'w3m-search)
    (bind-key "H-M-w" 'wikipedia-query))
  :config
  (progn

    (setq w3m-form-textarea-use-org-mode-p t)

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
      (bind-key "S-<return>" 'w3m-safe-view-this-url w3m-minor-mode-map))))

;;; wand:

(use-package wand
  :bind (("C-. RET" . wand:execute)
         ("C-. l" . wand:execute-current-line)
         ("C-. SPC" . toolbox:execute-and-replace))
  :init
  (progn
    (require 'load-functions)
    (use-package popwin
      :init
      (progn
        (popwin-mode 1)
        (add-to-list 'popwin:special-display-config `("*ag*" :noselect t)))))
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

;;_ , wgrep

(use-package wgrep)

;;_ , which-func

(use-package which-func
  :init
  (progn
    (hook-into-modes 'which-function-mode
                     '(prog-mode-hook))))

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
  :mode (("\\.phtml\\'" . web-mode)
         ("\\.tpl\\.php\\'" . web-mode)
         ("\\.blade\\.php\\'" . web-mode)
         ("\\.jsp\\'" . web-mode)
         ("\\.as[cp]x\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.html?\\'" . web-mode)
         ("/\\(views\\|html\\|theme\\|templates\\)/.*\\.php\\'" . web-mode))
  :init
  (progn
    ;; (require 'ac-emmet)
    (setq web-mode-engines-alist '(("\\.html\\.twig\\'" . "twig")))

    ;; (setq web-mode-ac-sources-alist
    ;;       '(("php" . (ac-source-yasnippet ac-source-php-auto-yasnippets))
    ;;         ("html" . (ac-source-emmet-html-aliases ac-source-emmet-html-snippets))
    ;;         ("css" . (ac-source-css-property ac-source-emmet-css-snippets))))

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
    (defun web-mode-hook ()
      "Hooks for Web mode."
      (setq web-mode-markup-indent-offset 2)
      (setq web-mode-css-indent-offset 2)
      (setq web-mode-code-indent-offset 2)
      (setq web-mode-disable-autocompletion t)
      ;; (whitespace-mode -1)
      (local-set-key (kbd "RET") 'indent-and-newline))
    (add-hook 'web-mode-hook  'web-mode-hook)))


;;;_ , winner

(use-package winner
  :diminish winner-mode
  :if (not noninteractive)
  :init
  (progn
    (winner-mode 1)

    (bind-key "M-N" 'winner-redo)
    (bind-key "M-P" 'winner-undo)))


;; When two windows view the same buffer at the same time, and one
;; window is switched to another buffer and back, point is now the
;; same as in the other window, not as it was before we switched away.
;; This mode tries to work around this problem by storing and
;; restoring per-window positions for each buffer.

(use-package winpoint
  :diminish winpoint-mode
  :init
  (winpoint-mode 1))

;;;_ , workgroups

(use-package workgroups
  :disabled t
  :diminish workgroups-mode
  :if (not noninteractive)
  :init
  (progn
    (workgroups-mode 1)

    (let ((workgroups-file (expand-file-name "workgroups" user-data-directory)))
      (if (file-readable-p workgroups-file)
          (wg-load workgroups-file)))

    (bind-key "C-\\" 'wg-switch-to-previous-workgroup wg-map)
    (bind-key "\\" 'toggle-input-method wg-map)

    (add-hook 'wg-switch-hook
              '(lambda ()
                 ;; (message "ho")
                 ))

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
            (message a-word)))))))

;;;_ , wrap-region

(use-package wrap-region
  :disabled t ; replaced by smartparens and/or autopair
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

(use-package xmsi-math-symbols-input
  :commands (xmsi-mode xmsi-math-symbols-input)
  :bind ("<S-space>" . xmsi-mode)
  )

;;;_ , yaml-mode

(use-package yaml-mode
  :mode ("\\.ya?ml\\'" . yaml-mode)
  :init
  (progn
    (add-hook 'yaml-mode-hook
              '(lambda ()
                 (whitespace-mode 1)))))


;;;_ , yasnippet


(use-package yasnippet
  :if (not noninteractive)
  :diminish yas-minor-mode
  :commands (yas-minor-mode yas-expand)
  :mode ("/\\.emacs\\.d/snippets/" . snippet-mode)
  :init
  (hook-into-modes #'(lambda () (yas-minor-mode 1))
                   '(prog-mode-hook
                     org-mode-hook
                     ruby-mode-hook
                     message-mode-hook
                     gud-mode-hook
                     erc-mode-hook))
  :config
  (progn
    (yas-load-directory (expand-file-name "snippets/" user-emacs-directory))

    (bind-key "C-i" 'yas-next-field-or-maybe-expand yas-keymap)

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
          (yas-expand-snippet "\
  # -*- mode: snippet -*-
  # name: $1
  # --
  $0"))))

    (bind-key "C-c y TAB" 'yas-expand)
    (bind-key "C-c y n" 'yas-new-snippet)
    (bind-key "C-c y f" 'yas-find-snippets)
    (bind-key "C-c y r" 'yas-reload-all)
    (bind-key "C-c y v" 'yas-visit-snippet-file)))

;;;_ , yaoddmuse

(use-package yaoddmuse
  :bind (("C-c w f" . yaoddmuse-browse-page-default)
         ("C-c w e" . yaoddmuse-edit-default)
         ("C-c w p" . yaoddmuse-post-library-default)))

;;;_ , zoom-window

(use-package zoom-window
  :bind ("H-z" . zoom-window-zoom))

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


(add-to-list 'custom-theme-load-path "~/.emacs.d/site-lisp/solarized")

;;;;; Theme ;;;;;
;; Cycle through this set of themes
(setq my-themes '(solarized-light solarized-dark zenburn wombat tango))

(setq my-cur-theme nil)
(defun cycle-my-theme ()
  "Cycle through a list of themes, my-themes"
  (interactive)
  (when my-cur-theme
    (disable-theme my-cur-theme)
    (setq my-themes (append my-themes (list my-cur-theme))))
  (setq my-cur-theme (pop my-themes))
  (load-theme my-cur-theme t)
  ;; (setq sml/theme 'respectful)
  ;; (sml/setup)
  )

;; Bind this to C-t
(bind-key "C-H-t" 'cycle-my-theme)

(if running-alternate-emacs
    (progn
      (add-hook 'after-init-hook
                (lambda ()
                  ;; Switch to the first theme in the list above
                  (cycle-my-theme))))
  (add-hook 'after-init-hook
            (lambda ()
              (cycle-my-theme) t)))

;; http://emacsredux.com/blog/2013/04/28/switch-to-previous-buffer/
;; (bind-key "C-<tab>" 'mode-line-other-buffer)
(bind-key "C-<escape>" 'mode-line-other-buffer)

;; Registers

(dolist
    (r `((?i (file . "~/.emacs.d/init.el"))
         (?a (file . "~/.emacs.d/.abbrev_defs"))
         (?b (file . "~/.profile"))
         (?B (file . "~/.bashrc"))
         (?e (file . "~/.emacs.d"))
         (?t (file . "~/Documents/Tasks/todo.txt"))
         (?s (file . "~/.emacs.d/settings.el"))
         (?o (file . "~/.emacs.d/dot-org.el"))
         (?g (file . "~/.emacs.d/dot-gnus.el"))
         (?O (file . "~/.emacs.d/org-settings.el"))
         (?G (file . "~/.emacs.d/gnus-settings.el"))
         (?u (file . "~/.emacs.d/site-lisp/xmsi-math-symbols-input.el"))
         (?z (file . "~/.zshrc"))))
  (set-register (car r) (cadr r)))


;; Local Variables:
;;   mode: emacs-lisp
;;   mode: allout
;;   outline-regexp: "^;;;_\\([,. ]+\\)"
;; End:

;;; init.el ends here
