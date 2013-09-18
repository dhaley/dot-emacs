;;;_. Initialization

(setq message-log-max 16384)

(defconst emacs-start-time (current-time))

(unless noninteractive
  (message "Loading %s..." load-file-name))

(load (expand-file-name "load-path" (file-name-directory load-file-name)))

(require 'use-package)
(eval-when-compile
  (setq use-package-verbose (null byte-compile-current-file)))

;;;_ , Utility macros and functions

(defmacro hook-into-modes (func modes)
  `(dolist (mode-hook ,modes)
     (add-hook mode-hook ,func)))


;; (defvar prog-mode-hook nil)

(defun prog-mode-setup ()
  (run-hooks 'prog-mode-hook))

(add-hook 'php-mode-hook 'prog-mode-setup)

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

;;;_ , Enable disabled commands

(put 'downcase-region  'disabled nil)   ; Let downcasing work
(put 'erase-buffer     'disabled nil)
(put 'eval-expression  'disabled nil)   ; Let ESC-ESC work
(put 'narrow-to-page   'disabled nil)   ; Let narrowing work
(put 'narrow-to-region 'disabled nil)   ; Let narrowing work
(put 'set-goal-column  'disabled nil)
(put 'upcase-region    'disabled nil)   ; Let upcasing work
(put 'scroll-left 'disabled nil)



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

;; you can also bind the Function-key to Hyper.  Without losing the ability to
;; change the volume or pause/play - those will still work.

                                        ;(setq mac-command-modifier 'meta)
                                        ;(setq mac-option-modifier 'super)
(setq mac-function-modifier 'hyper)

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
        (setq mac-fullscreen-on nil)
        )
    (progn
      (mac-mouse-turn-on-fullscreen t)
      (setq mac-fullscreen-on t))))

(when (and (window-system) (fboundp 'mac-mouse-turn-on-fullscreen))
  ;; (global-set-key (kbd "<M-s-return>") 'mac-toggle-fullscreen)
   (bind-key "C-H-f" 'mac-toggle-fullscreen)
)



;;;_ , Enable C-8 prefix

(defvar workgroups-preload-map)
(define-prefix-command 'workgroups-preload-map)

(bind-key "C-8" 'workgroups-preload-map)

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
(defun region-to-string ()
  (interactive)
  (buffer-substring (mark) (point)))

(bind-key "H-i" 'ispell-word)
(bind-key "H-e" 'grab-email-my)
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

(bind-key "M-RET" 'other-frame)

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


(defun delete-current-file (ξno-backup-p)
  "Delete the file associated with the current buffer.
Delete the current buffer too.

A backup file is created with filename appended “~”. Existing backup file are
overwritten.

if ΞNO-BACKUP-P is non-nil (when called with `universal-argument'), don't
create backup.

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

;;;_  . H-?

(define-key key-translation-map (kbd "H-TAB") (kbd "C-TAB"))

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


(defun find-alternate-file-with-sudo ()
  (interactive)
  (find-alternate-file (concat "/sudo::" (buffer-file-name))))

(bind-key "C-x C-v" 'find-alternate-file-with-sudo)


(defun mlm/locate-make-command-line (search-string)
  (list "mdfind" "-interpret" search-string))

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
        ;; (set-background-color "grey85")
        (set-face-background 'fringe "gray80")))

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
lists.  The functions are bound to the keys in the given mode-map.
Keys are in kbd format."
  (mapc (lambda (keybinding)
          (destructuring-bind (key function) keybinding
            (define-key mode-map (read-kbd-macro key) function)))
        keybindings))

(defun global-set-keys (keybindings)
  "Takes a list of (key function-designator) lists.
The functions are globally bound to the keys.  Keys
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


(defun toggle-windows-split()
  "Switch back and forth between one window and whatever split of windows we
  might have in the frame.  The idea is to maximize the current buffer,
while being able to go back to the previous split of windows in the frame
simply by calling this command again."
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
  "Copy word at point, or line if called twice, or region if transient-mark
active."
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

;; (bind-key "M-w" 'kill-ring-save)

(defun smart-open-line ()
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode."
  (interactive)
  (move-end-of-line nil)
  (open-line 1)
  (forward-line 1)
  (indent-according-to-mode))

(global-set-key [(shift return)] 'smart-open-line)

;; Set path to .emacs.d
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))

;; set the “forward button” (5th button) to close.
(cond
 ((string-equal system-type "windows-nt") ; Windows
  (global-set-key (kbd "<mouse-5>") 'ergoemacs-close-current-buffer)
  (global-set-key (kbd "<mouse-4>") 'describe-char)
  )
 ((string-equal system-type "gnu/linux")
  (global-set-key (kbd "<mouse-9>") 'ergoemacs-close-current-buffer)
  )
 ((string-equal system-type "darwin") ; Mac
  (global-set-key (kbd "<mouse-5>") 'ergoemacs-close-current-buffer) ) )

(global-set-key (kbd "<M-wheel-up>") 'ergoemacs-previous-user-buffer)
(global-set-key (kbd "<M-wheel-down>") 'ergoemacs-next-user-buffer)



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


(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

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

(defun cleanup-buffer-safe ()
  "Perform a bunch of safe operations on the whitespace content of a buffer.
Does not indent buffer, because it is used for a before-save-hook, and that
might be bad."
  (interactive)
  (untabify-buffer)
  (delete-trailing-whitespace)
  (set-buffer-file-coding-system 'utf-8))

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer.
Including indent-buffer, which should not be called automatically on save."
  (interactive)
  (cleanup-buffer-safe)
  (indent-buffer))

;; Various superfluous white-space. Just say no.
(add-hook 'before-save-hook 'cleanup-buffer-safe)


;; Uneven application of white-space is bad, m'kay?
(bind-key "H-N" 'clean-up-buffer-or-region)
(bind-key "H-n" 'cleanup-buffer)


(global-set-key (kbd "H-j")
                (lambda ()
                  (interactive)
                  (join-line -1)))


(setq package-archives
      '(("original"    . "http://tromey.com/elpa/")
        ("gnu"         . "http://elpa.gnu.org/packages/")
        ("marmalade"   . "http://marmalade-repo.org/packages/")))
(package-initialize)



;;;_.  Packages


;; Install the packages I need if not already installed

(defun rwd-require-package (name)
  (or (package-installed-p name) (package-install name)))

(setq dkh-required-packages
      (list
       'apache-mode
       'applescript-mode
       'buffer-move
       'rainbow-mode
       'smarter-compile
       'switch-window
       'window-number
       ))

;; (package-refresh-contents)
(dolist
    (package dkh-required-packages)
  (when (not (package-installed-p package)) (package-install package)))




;;;_ , el-get

(use-package el-get
  :disabled t
  :commands (el-get
             el-get-install
             el-get-update
             el-get-list-packages)
  :init
  (defvar el-get-sources nil)

  :config
    (progn
    (defun el-get-read-status-file ()
      (mapcar #'(lambda (entry)
                  (cons (plist-get entry :symbol)
                        `(status "installed" recipe ,entry)))
              el-get-sources))

  (defalias 'el-get-init 'ignore
    "Don't use el-get for making packages available for use.")))


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



    (defun reload-abbrevs () "\
reload abbrevs."
      (interactive "")
      (kill-all-abbrevs)
      (read-abbrev-file abbrev-file-name)
      )

    (add-hook 'expand-load-hook
              (lambda ()
                (add-hook 'expand-expand-hook 'indent-according-to-mode)
                (add-hook 'expand-jump-hook 'indent-according-to-mode)))))

;;;_ , ace-jump-mode

(use-package ace-jump-mode
  :bind ("M-h" . ace-jump-mode))

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
        (eval `(bind-key ,(concat (format-kbd-macro allout-command-prefix)
                                  " " (char-to-string (car mapping)))
                         (quote ,(cdr mapping))
                         allout-mode-map)))

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
                                          ("(latex2e)Command Index"))))))
  ;; (org-babel-load-file
  ;; "~/git/foss/emacs-starter-kit-social-sciences/starter-kit-latex.org")
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
    (ac-config-default))

  :config
  (progn
    (ac-set-trigger-key "TAB")
    (setq ac-use-menu-map t)

    (bind-key "H-M-?" 'ac-last-help)
    (unbind-key "C-s" ac-completing-map)))

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
      (find-file-other-window "~/org/projects.org"))
    ))

;;;_ , bbdbq

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
  :bind ("H-M-W" . copy-code-as-rtf))

;;;_ , crosshairs

(use-package crosshairs
  :bind ("M-o c" . crosshairs-mode))


(use-package csv-mode
  :commands csv-mode
  :mode ("\\.[Cc][Ss][Vv]\\'" . csv-mode)
  :init
  (progn
    (add-hook 'csv-mode-hook
              '(lambda ()
                 (whitespace-mode 1)
                 (orgtbl-mode 1)
                 (stripe-buffer-mode 1)
))))

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
                (rainbow-mode 1)))))

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
              ("emacs Lisp" (mode . emacs-lisp-mode))
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


    ;; (require 'ibuffer-ext)
    ;; (add-to-list 'ibuffer-never-show-predicates "^\\*")

    (add-hook 'ibuffer-mode-hook
              #'(lambda ()
                  (ibuffer-switch-to-saved-filter-groups "default")))))



;;;_ , iflipb

(use-package iflipb
  :disabled t
  :commands (iflipb-next-buffer iflipb-previous-buffer)
  :bind (("S-<tab>" . my-iflipb-next-buffer)
         ("H-S-<tab>" . my-iflipb-previous-buffer))
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
  ;; :defer t
  :bind ("C-x C-j" . dired-jump)
  :init
  (progn
     (defvar mark-files-cache (make-hash-table :test #'equal))

     (defun mark-similar-versions (name)
       (let ((pat name))
         (if (string-match "^\\(.+?\\)-[0-9._-]+$" pat)
             (setq pat (match-string 1 pat)))
         (or (gethash pat mark-files-cache)
             (ignore (puthash pat t mark-files-cache))))

       (defun dired-mark-similar-version ()
         (interactive)
         (setq mark-files-cache (make-hash-table :test #'equal))
         (dired-mark-sexp '(mark-similar-versions name)))))

     :config
     (progn
       ;; Also auto refresh dired, but be quiet about it
       (setq global-auto-revert-non-file-buffers t)
       (setq auto-revert-verbose nil)

       (defun dired-package-initialize ()
         (unless (featurep 'runner)
           (use-package dired-x)
           ;; (use-package dired-async)
           ;; (use-package dired-sort-map)
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

       ;; (eval-after-load "dired-aux"
       ;;   '(defun dired-do-async-shell-command (command &optional arg file-list)
       ;;      "Run a shell command COMMAND on the marked files asynchronously.

 ;; Like `dired-do-shell-command' but if COMMAND doesn't end in ampersand,
 ;; adds `* &' surrounded by whitespace and executes the command asynchronously.
 ;; The output appears in the buffer `*Async Shell Command*'."
 ;;            (interactive
 ;;             (let ((files (dired-get-marked-files t current-prefix-arg)))
 ;;               (list
 ;;                ;; Want to give feedback whether this file or marked files are
 ;;                ;; used:
 ;;                (dired-read-shell-command "& on %s: " current-prefix-arg files)
 ;;                current-prefix-arg
 ;;                files)))
 ;;            (unless (string-match "[ \t][*?][ \t]" command)
 ;;              (setq command (concat command " *")))
 ;;            (unless (string-match "&[ \t]*\\'" command)
 ;;              (setq command (concat command " &")))
 ;;            (dired-do-shell-command command arg file-list)))

       (require 'stripe-buffer)
       (add-hook 'dired-mode-hook '(lambda ()
                                     (dired-package-initialize)
                                     (stripe-listify-buffer)
                                     (setq cursor-type t)
                                     ))

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

       (bind-key "C-c J" 'dired-double-jump)

       (defun dired-back-to-top ()
         (interactive)
         (beginning-of-buffer)
         (next-line 2))

       (define-key dired-mode-map
         (vector 'remap 'beginning-of-buffer) 'dired-back-to-top)

       (defun dired-jump-to-bottom ()
         (interactive)
         (end-of-buffer)
         (next-line -1))

       (define-key dired-mode-map
         (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom)

       ;; http://ergoemacs.org/emacs/emacs_dired_convert_images.html

       (defun scale-image (fileList scalePercentage)
         "Create a scaled jpg version of images of marked files in dired.
The new names have “-s” appended before the file name extension.
Requires ImageMagick shell tool."
         (interactive
          (list (dired-get-marked-files) (read-from-minibuffer "scale percentage:")))
         (require 'dired)

         (mapc
          (lambda (ξf)
            (let ( newName cmdStr )
              (setq newName (concat (file-name-sans-extension ξf) "-s" ".jpg") )
              (while (file-exists-p newName)
                (setq newName (concat (file-name-sans-extension newName) "-s" (file-name-extension newName t))) )

              ;; relative paths used to get around Windows/Cygwin path remapping problem
              (setq cmdStr (concat "convert -scale " scalePercentage "% -quality 85% " (file-relative-name ξf) " " (file-relative-name newName)) )
              (shell-command cmdStr)
              ))
          fileList ))

       (defun 2jpg (fileList)
         "Create a jpg version of images of marked files in dired.
Requires ImageMagick shell tool.
"
         (interactive (list (dired-get-marked-files) ))
         (require 'dired)

         (mapc
          (lambda (ξf)
            (let ( newName cmdStr )
              (setq newName (concat (file-name-sans-extension ξf) ".jpg") )
              (while (file-exists-p newName)
                (setq newName (concat (file-name-sans-extension newName) "-2" (file-name-extension newName t))) )

              ;; relative paths used to get around Windows/Cygwin path remapping problem
              (setq cmdStr (concat "convert " (file-relative-name ξf) " " (file-relative-name newName)) )

              ;; (async-shell-command cmdStr)
              (shell-command cmdStr)
              ))
          fileList ))

       ;;Command to zip File/Dir

       (defun 2zip ()
         "Zip the current file/dir in `dired'.
If multiple files are marked, only zip the first one.
Require unix zip commandline tool."
         (interactive)
         (require 'dired)
         (let ( (fileName (elt (dired-get-marked-files) 0))  )
           (shell-command (format "zip -r '%s.zip' '%s'" (file-relative-name fileName) (file-relative-name fileName)))
           ))))

;;;_ , doxymacs

(use-package doxymacs
  :commands doxymacs-mode
  :diminish doxymacs-mode
  :load-path "~/.emacs.d/site-lisp/doxymacs-1.8.0/lisp")

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

;;;_ , edit-emacs

(use-package edit-server
  :if (and window-system (not running-alternate-emacs)
           (not noninteractive))
  :init
  (progn
    ;; (add-hook 'after-init-hook 'server-start t)
    ;; (add-hook 'after-init-hook 'edit-server-start t)
))

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


(use-package truthy
  :commands (truthy
             truthy-s
             truthy-l))


(use-package conf-mode
  :mode ("\\.info\\|\\.gitmodules"  . conf-mode))


(use-package php-mode
  :commands php-mode
  :diminish php-mode
  :config
  (progn
    (require 'php-extras)
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
                'nil
                )
              )
            )
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

    (defun my-php-mode-hook ()
      (set (make-local-variable 'yas-fallback-behavior)
           '(apply my-php-indent-or-complete . nil))
      (bind-key "<tab>" 'yas-expand-from-trigger-key php-mode-map))

    (add-hook 'php-mode-hook
              '(lambda ()
                 (abbrev-mode 1)
                 (hs-minor-mode 1)
                 (imenu-add-menubar-index)
                 (subword-mode t)
                 (turn-on-eldoc-mode)
                 (which-func-mode 1)
                 (diminish 'hs-minor-mode)
                 (setq indicate-empty-lines t)
                 'my-php-mode-hook
                 (local-set-key "\r" 'my-php-return)
                 ))))

;;;_ , projectile

(use-package projectile
  :commands projectile-mode
  :diminish projectile
  :config
  (progn

    (use-package dash
      :commands dash
      :load-path "dash.el")

    (bind-key "C-c h" 'helm-projectile)))

;;;_ , drupal-mode

(use-package drupal-mode
  :commands (initialize_cu_drupal drupal-mode)
  :diminish drupal-mode
  :init
  (progn
    (add-to-list 'auto-mode-alist '("\\.\\(inc\\)$" . php-mode))
    (add-to-list 'auto-mode-alist '("\\.\\(profile\\)$" . php-mode))
    )
  :config
  (progn
    (require 'etags)
    (require 'tags-view)
    (require 'smart-dash)
    (require 'projectile)
    (require 'drupal-spell)

    (add-hook 'drupal-mode-hook
              '(lambda ()
                 (setq yas-extra-modes 'drupal-mode)
                 (doxymacs-mode 1)
                 (doxymacs-font-lock)
                 (projectile-on)))
    (add-to-list 'Info-directory-list '"~/.emacs.d/site-lisp/drupal-mode")

    (defun initialize_cu_drupal ()
      "Sets up project variables with out having anything project specific in the
.dir-locals.el file. "
      (interactive)

      (setq site-directory (file-truename (locate-dominating-file default-directory ".dir-locals.el"))
            tags-file-name (concat site-directory "TAGS")
            readme-file-name (concat site-directory "README.md")
            profile-name (curr-dir-project-string)
            profile-directory (concat site-directory "profiles/" profile-name)
            module-directory (concat profile-directory "/modules")
            theme-directory (concat profile-directory "/themes")
            profile-theme-directory (concat profile-directory "/themes/" profile-name)
            feature-directory (concat module-directory "/features")
            contrib-directory (concat module-directory "/contrib")
            custom-directory (concat module-directory "/custom")
            uri (concat "ww/" profile-name)
            doxymacs-doxygen-dirs `((
                                     ,site-directory
                                     ,(concat site-directory "doxy_tag.xml")
                                     ,(concat "file://" site-directory "docs/html")))
            drupal-rootdir site-directory)


      (defun drush-uli-to-string ()
        " Provide dynamically derived uri for drush uli"
        (interactive)
        (cd site-directory)
        (kill-new (shell-command-to-string (concat "drush --uri=" uri " uli"))))
      (bind-key "C-8 d e" `drush-uli-to-string)

      (setenv "8dr" readme-file-name)
      (setenv "8ds" site-directory)
      (setenv "DRUPAL_ROOT" site-directory)
      (setenv "8dp" profile-directory)
      (setenv "8dT" theme-directory)
      (setenv "8dt" profile-theme-directory)
      (setenv "8dm" module-directory)
      (setenv "8dc" custom-directory)
      (setenv "8df" feature-directory)
      (setenv "8db" contrib-directory)

      (bind-key "C-8 d r" (lambda()(interactive)(find-file readme-file-name)))
      (bind-key "C-8 d s" (lambda()(interactive)(find-file site-directory)))
      (bind-key "C-8 d p" (lambda()(interactive)(find-file profile-directory)))
      (bind-key "C-8 d T" (lambda()(interactive)(find-file theme-directory)))
      (bind-key "C-8 d t" (lambda()(interactive)(find-file profile-theme-directory)))
      (bind-key "C-8 d m" (lambda()(interactive)(find-file module-directory)))
      (bind-key "C-8 d c" (lambda()(interactive)(find-file custom-directory)))
      (bind-key "C-8 d f" (lambda()(interactive)(find-file feature-directory)))
      (bind-key "C-8 d b" (lambda()(interactive)(find-file contrib-directory)))

      (require 'find-file-in-project)
      ;; Function to create new functions that look for a specific pattern
      (defun ffip-create-pattern-file-finder (&rest patterns)
        (lexical-let ((patterns patterns))
          (lambda ()
            (interactive)
            (let ((ffip-patterns patterns))
              (find-file-in-project)))))

      ;; Find file in project, with specific patterns
      ;; (global-unset-key (kbd "C-8 f"))
      (bind-key "C-8 f ph" (ffip-create-pattern-file-finder "*.php"))
      (bind-key "C-8 f if" (ffip-create-pattern-file-finder "*.info"))
      (bind-key "C-8 f md" (ffip-create-pattern-file-finder "*.md"))
      (bind-key "C-8 f mo" (ffip-create-pattern-file-finder "*.module"))
      (bind-key "C-8 f in" (ffip-create-pattern-file-finder "*.inc"))
      (bind-key "C-8 f cs" (ffip-create-pattern-file-finder "*.css"))

      ;; (projectile-on)


      (setq projectile-project-compilation-commands
            (format "phpcs --report=emacs --standard=Drupal %s" (buffer-file-name)))
      ;; use native indexing such as .gitignore
      (setq projectile-use-native-indexing 't)
      (setq projectile-tags-command "~/bin/etags_drupal.sh")

      (define-key projectile-mode-map (kbd "C-8 p") 'projectile-find-file)
      (define-key projectile-mode-map (kbd "C-8 F") 'projectile-grep)

      (defun create-drush-buffer (command &rest a)
        (setq opt1 (car a))
        (setq opt2 (cadr a))
        (setq opt3 (caddr a))
        (setq allopt (concat opt1 " " opt2 " " opt3))
        (setq d-buffer (get-buffer-create  (concat "*drush " command " " allopt "*")))
        (buffer-live-p d-buffer)
        (with-current-buffer d-buffer
          (goto-char (point-min))
          (view-mode 1)
          (stripe-buffer-mode 1)
          (hl-line-mode 1)
          (if opt3
              (start-process "drush" (current-buffer) "drush"
                             command
                             opt1
                             opt2
                             opt3)
            (if opt2
                (start-process "drush" (current-buffer) "drush"
                               command
                               opt1
                               opt2)
              (if opt1
                  (start-process "drush" (current-buffer) "drush"
                                 command
                                 opt1)
                (start-process "drush" (current-buffer) "drush"
                               command))))

            (shrink-window-if-larger-than-buffer))
          (switch-to-buffer d-buffer))

      (defun drush-features-enabled ()
        (interactive)
        (create-drush-buffer "features-list" "--status=enabled"))
      (bind-key "C-8 f e" 'drush-features-enabled)

      (defun drush-features-list ()
        (interactive)
        (create-drush-buffer "features-list"))
      (bind-key "C-8 f l" 'drush-features-list)

      (defun drush-modules-nocore ()
        (interactive)
        (create-drush-buffer "pm-list" "--status=enabled" "--no-core" "--type=module"))
      (bind-key "C-8 m n" 'drush-modules-nocore))))

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
    (add-hook 'after-init-hook 'irc)
    ;; (add-hook 'after-init-hook 'create-new-erc-frames)
    )

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
      (switch-to-buffer-other-window "&bitlbee")
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

    (defun erc-cmd-BAN (nick &optional redirect whole-ip)
      (let* ((chan (erc-default-target))
             (who (erc-get-server-user nick))
             (host (erc-server-user-host who))
             (user (erc-server-user-login who)))
        (erc-send-command
         (format "MODE %s +b *!%s@%s%s"
                 chan (if whole-ip "*" user) host redirect))))

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
                                    "Kicked (kickban)"))))

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
                                    "Kicked (kickbanip)"))))

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
                                    "Kicked (kicktroll)"))))

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
    (add-hook 'erc-insert-modify-hook 'my-reformat-jabber-backlog)

    ;; turn on abbrevs
    (abbrev-mode 1)

    (defun create-new-erc-frames ()
      (interactive)
      (switch-to-bitlbee)
      (switch-to-buffer-other-frame "#drupal-colorado")
      (switch-to-buffer-other-frame "#emacs")
      )

    ;; (frame-configuration-to-register REGISTER &optional ARG)
    ;; (set-register "E"  (list (current-frame-configuration) (point-marker)))
    (bind-key "H-E" 'create-new-erc-frames)

    ;; add abbrevs
    (abbrev-table-put erc-mode-abbrev-table :parents (list
                                                      text-mode-abbrev-table))
    (use-package window-number)
    (window-number-mode)
    (window-number-meta-mode)
    (add-hook 'erc-mode-hook (lambda () (abbrev-mode 1)))))

(defun curr-dir-project-string ()
  "Returns current project as a string, or the empty string if
PWD is not in a project"
  (interactive)
  (let ((project-root-dir (locate-dominating-file default-directory "current")))
    (let ((path (split-string project-root-dir "/")))     ; path as list
      (car (last (nbutlast path 1))))))


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

      (add-hook 'eshell-first-time-mode-hook 'eshell-initialize)
      (add-hook 'eshell-mode-hook
                '(lambda ()
                   ;; (or (getenv "CDPATH") (setenv "CDPATH" ".:~:~/.emacs.d:~/data:~/data/releases"))
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
        (pcomplete-here* (pcmpl-git-get-refs "heads"))



)))))


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


;;;_ , eudc

(defalias 'eu 'eudc-query-form)


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
  :bind (("C-c =" . er/expand-region))
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

;;;_ , features-mode

(use-package feature-mode
  :mode ("\\.feature$" . feature-mode))

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

(use-package flyspell
  :bind (("C-c i b" . flyspell-buffer)
         ("C-c i f" . flyspell-mode))
  :config
  (define-key flyspell-mode-map [(control ?.)] nil))


;;;_ , flycheck

(use-package flycheck
  ;; :load-path ("site-lisp/flycheck/deps/dash.el"
  ;;             "site-lisp/flycheck/deps/s.el")
  :init
  (progn
    (flycheck-declare-checker xmllint
      "xmllint checker"
      :command '("xmllint" "--noout" "--postvalid" source)
      :error-patterns
      '(("^\\(?1:.*\\):\\(?2:[0-9]+\\): parser error : \\(?4:.*\\)$" error))
      :modes 'nxml-mode)

    (push 'xmllint flycheck-checkers)

    (flycheck-declare-checker jslint
      "jslint checker"
      :command '("jsl" "-process" source)
      :error-patterns
      '(("^\\(?1:.*\\)(\\(?2:[0-9]+\\)): error: \\(?4:.*\\)$" error)
        ("^\\(?1:.*\\)(\\(?2:[0-9]+\\)): \\(\\(lint \\)?warning\\): \\(?4:.*\\)$"
         warning))
      :modes 'js2-mode)

    (push 'jslint flycheck-checkers)

    (hook-into-modes #'flycheck-mode '(prog-mode-hook)))
  :config
  (progn
    (defalias 'flycheck-show-error-at-point-soon 'flycheck-show-error-at-point)
    (defalias 's-collapse-whitespace 'identity)))


(use-package geben
  :commands (geben
             geben-mode)

  :config
    (progn
      ;; Breakpoints to always set when geben starts.
      (defun my-geben-breakpoints (session)
        (message "Setting my geben breakpoints.") ; how often is this called?
                                        ; call debugger() in PHP, similar to debugger; in javascript.
                                        ; Would be nice to also set a watch on $GLOBALS['debugger'], but couldn't get that to work.
        (geben-set-breakpoint-call "debugger"))
      (add-hook 'geben-dbgp-init-hook #'my-geben-breakpoints t) ; set breakpoints when geben starts

                                        ; Ideally, we would not break in debugger(), but instead in the place that called it.  For now we must step out to see who called us.


      ;; A poor man's variable watch, since dbgp doesn't seem to support watching vars.
      ;; geben's eval command will show result in the minibuffer.
      ;; More ideal would be to put the watch in the context buffer (or it's own watch buffer).  However this is the limit of my elisp.  I'm not sure how to prevent geben from writing to minibuffer when geben-dbgp-command-eval is called.
      (defun my-geben-watch (session)
                                        ;(geben-dbgp-command-eval session "print_r($GLOBALS['watch'], 1)")) ; takes up several lines in minibuffer
                                        ;(geben-dbgp-command-eval session "$GLOBALS['watch']")) ; uses fewer lines, not always easy to read
        (geben-dbgp-command-eval session "function_exists('watch') ? 'watch: ' . print_r(watch(get_defined_vars()), 1) : 'watch() not yet defined.'") ; watch() keeps track of which variables to display.
        )
      (add-hook 'geben-dbgp-continuous-command-hook #'my-geben-watch t) ; display watch vars, very often.

      ;; This controls whether geben stops before running anything.
      (setq geben-pause-at-entry-line nil)

      ;; geben won't connect because its "Already in debugging"  This might help.
      (defun my-geben-release ()
        (interactive)
        (geben-stop)
        (dolist (session geben-sessions)
          (ignore-errors
            (geben-session-release session))))

    ;; Debug a simple PHP script.
    ;; Change the session key my-php-53 to any session key text you like
    (defun my-php-debug ()
      "Run current PHP script for debugging with geben"
      (interactive)
      (call-interactively 'geben)
      (shell-command
       (concat "XDEBUG_CONFIG='idekey=my-php-53' /Applications/MAMP/bin/php/php5.3.20/bin/php "
               (buffer-file-name) " &")))))


;;;_ , highlight-symbol
(use-package highlight-symbol
  :commands (highlight-symbol-prev highlight-symbol-next highlight-symbol-at-point highlight-symbol-query-replace)
  :init
  (progn
    (bind-key "C-8 C-p" 'highlight-symbol-prev)
    (bind-key "C-8 C-n" 'highlight-symbol-next)
    (bind-key "C-8 C-h" 'highlight-symbol-at-point)
    (bind-key "C-8 C-q" 'highlight-symbol-query-replace)))

;;;_ , highlight-tail
(use-package highlight-tail
  :commands highlight-tail-mode)


;;;_ , fold-dwim

(use-package fold-dwim
  :bind (("<f13>" . fold-dwim-toggle)
         ("<f14>" . fold-dwim-hide-all)
         ("<f15>" . fold-dwim-show-all)))

;;;_ , gist

(use-package gist
  :bind ("C-c G" . gist-region-or-buffer))

;;;_ , git-gutter+

(use-package git-gutter+
  :commands git-gutter+-mode
  :diminish git-gutter+-mode
  :config
  (progn
    (use-package git-gutter-fringe+
      :config
      (git-gutter-fr+-minimal))
    (global-git-gutter+-mode 1)))


;;;_ , git-blame

(use-package git-blame
  :commands git-blame-mode)

;;;_ , gnus
(use-package dot-gnus
  :if (not running-alternate-emacs)
  :bind (("M-G"   . switch-to-gnus)
         ("C-x m" . compose-mail))
  :init
  (progn
    (setq gnus-init-file (expand-file-name "dot-gnus" user-emacs-directory)
          gnus-home-directory "~/Messages/Gnus/")

    ;; (abbrev-table-put gnus-article-edit-mode-abbrev-table :parents (list org-mode-abbrev-table))
    )
  :config
  (progn
    (use-package org-mime)
    ;; (use-package eudc)
    (use-package rgr-web)))

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
          (grep-apply-setting 'grep-find-command
                              '("ag --noheading --column " . 25)))
      (grep-apply-setting
       'grep-find-command
       '("find . -type f -print0 | xargs -P4 -0 egrep -nH -e " . 52)))))

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
  :commands hl-line-mode
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
    (ido-ubiquitous-use-new-completing-read yas-expand 'yasnippet)
    (ido-ubiquitous-use-new-completing-read yas-visit-snippet-file 'yasnippet)

    ;; http://emacsrocks.com/e10.html
    (defadvice ido-imenu (before push-mark activate)
    (push-mark))
                                                                              )
)



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
    ;; (defadvice info-setup (after load-info+ activate)
    ;;   (use-package info+))

    (defadvice Info-exit (after remove-info-window activate)
      "When info mode is quit, remove the window."
      (if (> (length (window-list)) 1)
          (delete-window)))))

(use-package info-look
  :commands info-lookup-add-help)

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

;;;_ , aliases

(defalias 'acm 'auto-complete-mode)
(defalias 'ai 'auto-install-from-emacswiki)
(defalias 'bb  'bbdb)
(defalias 'bc  'bbdb-create)
(defalias 'cc 'calc)
(defalias 'gc 'gnus-summary-catchup-to-here)
(defalias 'delete-matching-lines 'flush-lines)
(defalias 'delete-non-matching-lines 'keep-lines)
(defalias 'dml 'delete-matching-lines)
(defalias 'dnml 'delete-non-matching-lines)
(defalias 'dsm 'desktop-save-mode)
(defalias 'dtw 'delete-trailing-whitespace)
(defalias 'eb 'eval-buffer)
(defalias 'ed 'eval-defun)
(defalias 'egi 'el-get-install)
(defalias 'eis 'elisp-index-search)
(defalias 'ele 'eval-last-sexp)
(defalias 'elm 'emacs-lisp-mode)
(defalias 'er 'eval-region)
(defalias 'eu 'eudc-query-form)
(defalias 'ev 'eval-buffer)
(defalias 'fb 'flyspell-buffer)
(defalias 'fd 'find-dired)
(defalias 'g 'grep)
(defalias 'gf 'grep-find)
(defalias 'gwsm 'global-whitespace-mode)
(defalias 'hm 'html-mode)
(defalias 'iw 'ispell-word)
(defalias 'j 'jabber)
(defalias 'lcd 'list-colors-display)
(defalias 'lf 'load-file)
(defalias 'list-matching-lines 'occur)
(defalias 'll 'load-library)  ;; dynamic, instead of require
(defalias 'lml 'list-matching-lines)
(defalias 'man 'woman)
(defalias 'ntr 'narrow-to-region)
(defalias 'om 'org-mode)
(defalias 'pi 'package-install)
(defalias 'ps 'powershell)
(defalias 'qrr 'query-replace-regexp)
(defalias 'r 'list-registers)
(defalias 'rb 'revert-buffer)
(defalias 'rn 'wdired-change-to-wdired-mode) ; rename file in dired
(defalias 'rof 'recentf-open-files)
(defalias 'rr 'reverse-region)
(defalias 'rs 'replace-string)
(defalias 'sbc 'set-background-color)
(defalias 'sc 'sql-connect)
(defalias 'sh 'shell)
(defalias 'sl 'sort-lines)
(defalias 'ssm 'shell-script-mode)
(defalias 'td 'toggle-debug-on-error)
(defalias 'tm 'text-mode)
(defalias 'vbm 'visual-basic-mode)
(defalias 'vlm 'visual-line-mode)
(defalias 'wsm 'whitespace-mode)

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

    (add-hook 'magit-mode-hook 'hl-line-mode))

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
    (require 'rebase-mode)
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

(use-package github-browse-file
  :bind ("H-o" . github-browse-file))

;;;_ , markdown-mode

(use-package markdown-mode
  :commands markdown-mode
  :mode (("\\.markdown\\'" . markdown-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.mdwn\\'" . markdown-mode)
         ("\\.mkdn\\'" . markdown-mode)
         ("\\.mdown\\'" . markdown-mode)
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
                 (setq imenu-generic-expression markdown-imenu-generic-expression)
                 (turn-on-pandoc)
                 ))))

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
      (define-key term-pager-break-map  "\177" 'term-pager-back-page))

(defun my-term-paste (&optional string)
  (interactive)
  (process-send-string
   (get-buffer-process (current-buffer))
   (if string string (current-kill 0))))

(add-hook 'term-exec-hook '(lambda ()
                             (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix)
                             (goto-address-mode)
                             (define-key term-raw-map (kbd "C-y") 'my-term-paste)
                             ))
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
                             "-xml" "-i" "-wrap" "0" "-omit" "-q")))

    (bind-key "C-H" 'tidy-xml-buffer nxml-mode-map)))


;; ;;;_ , o-blog

(use-package o-blog
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
    (unless running-alternate-emacs
      (run-with-idle-timer 300 t 'jump-to-org-agenda))

    (unless running-alternate-emacs
      (add-hook 'after-init-hook
                #'(lambda ()
                    (org-agenda-list)
                    (org-fit-agenda-window)
                    (org-resolve-clocks))) t)))


;;;_ , org-jira

(use-package org-jira
  :load-path ("~/.emacs.d/site-lisp/org-jira")
  :commands (org-jira-create-issue org-jira-get-issue)
  :bind (
         ("C-8 j G" . org-jira-get-subtasks)
         ("C-8 j P" . org-jira-get-projects)
         ("C-8 j b" . org-jira-browse-issue)
         ("C-8 j c" . org-jira-create-issue)
         ("C-8 j C" . org-jira-update-comment)
         ("C-8 j g" . org-jira-get-issues)
         ("C-8 j p" . org-jira-progress-issue)
         ("C-8 j r" . org-jira-refresh-issue)
         ("C-8 j s" . org-jira-create-subtask)
         ("C-8 j t" . org-jira-todo-to-jira)
         ("C-8 j u" . org-jira-update-issue))
  :init
  (progn

    ;; (require 'jira)
    (setq jira-url "https://cuboulder.atlassian.net//rpc/xmlrpc")
    ;; (require 'org-jira)
    (setq jiralib-url "https://cuboulder.atlassian.net")
    ;; jiralib is not explicitly required, since org-jira will load it.
    (setq org-jira-working-dir "~/Documents/Tasks/.org-jira")
    (setq org-jira-current-project "VCR")

    (setq jira-users
          (list
           (cons "Unassigned" "")
           (cons "Erin Corsin" "erin")
           (cons "Matt Tucker" "matt")
           (cons "Kevin Crafts" "kevin")
           (cons "Damon Haley" "daha1836")
           (cons "Alfredo Nevarez" "alfredo")
           (cons "Catherine Snider" "snider")
           (cons "Kosta Tovstiadi" "kosta")
           (cons "Will Kubie" "kubie")
           (cons "Joanna Bertrand" "joanna")
           (cons "Wendy Turnbull" "wendy")))))


(use-package outline-mode
  :diminish outline-mode
  :commands outline-mode
  :config
  (eval-after-load "outline" '(require 'foldout))
  )


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

(unless
    (use-package mic-paren
      :init
      (paren-activate))

  (use-package paren
    :init
    (show-paren-mode 1)))

;;;_ , page-break-lines

;; (use-package page-break-lines
;;   :diminish page-break-lines)

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
  :mode ("\\.bashrc\\|\\.bash_alias\\|\\.sh\\|.bash_history\\|alias" . sh-mode))


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
                  (kbd "C-d") 'comint-delchar-or-eof-or-kill-buffer)
                (autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
                (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)))))


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

(use-package smart-compile
  :disabled t
  :commands smart-compile
  :bind (("C-c c" . smart-compile)
         ("A-n"   . next-error)
         ("A-p"   . previous-error)))

;;;_ , smartparens

(use-package smartparens
  :commands (smartparens-mode show-smartparens-mode)
  :config (require 'smartparens-config))

(use-package smerge-mode
  :commands (smerge-mode smerge-command-prefix)
  :init
  (setq smerge-command-prefix (kbd "C-. C-.")))


;;;_ , stripe-buffer

(use-package stripe-buffer
  :init
  (progn
    (add-hook 'org-mode-hook 'turn-on-stripe-table-mode)))

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

;; https://github.com/anthracite/emacs-config/blob/master/init.el
;;;;_ , twittering-mode

(use-package twittering-mode
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


;;;_ , textexpander

;; (when (= 0 (call-process "using-textexpander"))
  (bind-key "H-v" 'scroll-down)
  (bind-key "H-v" 'yank)
  ;; (bind-key "M-v" 'scroll-down)
;; )

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
  :if (not running-alternate-emacs)
  ;; :load-path "~/.emacs.d/lisp/"
  :bind (("<f4>" . rgr/browse-url)
         ))

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

    (bind-key "H-M-e" 'goto-emacswiki)
    (bind-key "H-M-g" 'w3m-search)
    (bind-key "H-M-w" 'wikipedia-query))

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
    (hook-into-modes 'whitespace-mode
                     '(prog-mode-hook))

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
  :mode ("\\.tpl\\.php\\.html$" . web-mode)
  :init
  (progn
    (setq web-mode-engines-alist '(("\\.html\\.twig\\'" . "twig")))
    ))


;;;_ , windmove

;; (setq windmove-wrap-around t)
;; (windmove-default-keybindings)        ; Move between frames with Shift+arrow
;;                                         ; windmove shows a stacktrace when
;;                                         ; there is nothing to move to


;; ;; If, like me, you’re a heavy org-mode user, you’ll find that these key
;; ;; bindings won’t work in org-mode buffers because org-mode takes them
;; ;; over. Happily you can solve this by adding the line
;; (setq org-replace-disputed-keys t)

;; (defmacro maser/swallow-errors (name f-with-error)
;;   `(defun ,name ()
;;      (interactive)
;;      (condition-case err
;;          (,f-with-error)
;;        (error
;;         (message "%s" (error-message-string err))))))

;; (maser/swallow-errors windmove-down-without-errors windmove-down)
;; (maser/swallow-errors windmove-up-without-errors windmove-up)
;; (maser/swallow-errors windmove-right-without-errors windmove-right)
;; (maser/swallow-errors windmove-left-without-errors windmove-left)


;; (bind-key "H-'" 'windmove-right-without-errors)
;; (bind-key "H-/" 'windmove-down-without-errors)
;; (bind-key "H-;" 'windmove-left-without-errors)
;; (bind-key "H-[" 'windmove-up-without-errors)

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

;;;_ , workgroups

(use-package workgroups
  :diminish workgroups-mode
  :if (not noninteractive)
  :init
  (progn
    (workgroups-mode 1)

    (let ((workgroups-file (expand-file-name "workgroups" user-data-directory)))
      (if (file-readable-p workgroups-file)
          (wg-load workgroups-file)))

    (bind-key "C-\\" 'wg-switch-to-previous-workgroup wg-map)
    (bind-key "\\" 'toggle-input-method wg-map)))

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

(use-package xmsi-mode
  ;; (autoload 'xmsi-mode "xmsi-math-symbols-input" "Load xmsi minor mode for inputting math (Unicode) symbols." t)
  :commands (xmsi-mode xmsi-math-symbols-input)
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

    (bind-key "<tab>" 'yas-next-field-or-maybe-expand yas-keymap)

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


(add-to-list 'custom-theme-load-path "~/.emacs.d/site-lisp/emacs-color-theme-solarized")

(defvar blink-cursor-colors (list  "#b58900" "#cb4b16" "#dc322f" "#d33682"
  "#6c71c4" "#268bd2" "#2aa198" "#859900")
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

;; OS X Specific configuration

;;; Ignore .DS_Store files with ido mode
(add-to-list 'ido-ignore-files "\\.DS_Store")
;; # FIXME: this is to ignore Dropbox "Icon" files that seem to be
;; "Icon", but I can't figure out how to ignore that.
(add-to-list 'ido-ignore-files "Icon")

;; toggle-input-method
(setq default-input-method "MacOSX")

;; fix hostname.local stuff
(setq system-name (car (split-string system-name "\\.")))

;;;;; Theme ;;;;;
;; Cycle through this set of themes
(setq my-themes '(solarized-dark solarized-light zenburn wombat tango))

(setq my-cur-theme nil)
(defun cycle-my-theme ()
  "Cycle through a list of themes, my-themes"
  (interactive)
  (when my-cur-theme
    (disable-theme my-cur-theme)
    (setq my-themes (append my-themes (list my-cur-theme))))
  (setq my-cur-theme (pop my-themes))
  (load-theme my-cur-theme t))

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


;; Even though we may have set the Mac OS X Terminal's Alt key as the
;; emacs Meta key, we want to be able to insert a '#' using Alt-3 in
;; emacs as we would in other programs.
(fset 'insert-pound "#")
(define-key global-map "\M-3" 'insert-pound)

;; OS X ls doesn't support --dired
(setq dired-use-ls-dired nil)

;; (setenv "GPG_AGENT_INFO" "~/.gnupg/S.gpg-agent")

;; (defun offlineimap-get-password (host port)
;;   (let* ((netrc (netrc-parse (expand-file-name "~/Documents/.autinfo.gpg")))
;;          (hostentry (netrc-machine netrc host port port)))
;;     (when hostentry (netrc-get hostentry "password"))))

;; (setq
;;  epa-file-cache-passphrase-for-symmetric-encryption t
;;  user-full-name "Damon Haley"
;;  user-mail-address "damon.haley@colorado.edu"
;;  )


(defalias 'list-matching-lines 'occur)
(defalias 'delete-matching-lines 'flush-lines)
(defalias 'delete-non-matching-lines 'keep-lines)

(defalias 'td 'toggle-debug-on-error)

;; Allow "y or n" instead of "yes or no"
(fset 'yes-or-no-p 'y-or-n-p)

(require 'switch-window)

;; this interferes with 7.10 Numeric Arguments
;; (require 'window-number)
;; (window-number-mode)
;; (window-number-meta-mode)

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

(defun ergoemacs-open-in-external-app ()
  "Open the current file or dired marked files in external app."
  (interactive)
  (let ( doIt
         (myFileList
          (cond
           ((string-equal major-mode "dired-mode") (dired-get-marked-files))
           (t (list (buffer-file-name))) ) ) )

    (setq doIt (if (<= (length myFileList) 5)
                   t
                 (y-or-n-p "Open more than 5 files?") ) )

    (when doIt
      (cond
       ((string-equal system-type "windows-nt")
        (mapc (lambda (fPath) (w32-shell-execute "open" (replace-regexp-in-string "/" "\\" fPath t t)) ) myFileList)
        )
       ((string-equal system-type "darwin")
        (mapc (lambda (fPath) (shell-command (format "open \"%s\"" fPath)) )  myFileList) )
       ((string-equal system-type "gnu/linux")
        (mapc (lambda (fPath) (let ((process-connection-type nil)) (start-process "" nil "xdg-open" fPath)) ) myFileList) ) ) ) ) )

(bind-key "C-S-o" 'open-in-desktop)


(defun prelude-open-with ()
  "Simple function that allows us to open the underlying
file of a buffer in an external program."
  (interactive)
  (when buffer-file-name
    (shell-command (concat
                    (if (eq system-type 'darwin)
                        "open"
                      (read-shell-command "Open current file with: "))
                    " "
                    buffer-file-name))))

(defun prelude-google ()
  "Googles a query or region if any."
  (interactive)
  (browse-url
   (concat
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (url-hexify-string (if mark-active
         (buffer-substring (region-beginning) (region-end))
       (read-string "Google: "))))))


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


;; (defun set-site-directroy ()
;;   "Sets up project variables with out having anything project specific in the
;; .dir-locals.el file. "
;;   (interactive)
;;   (setq site-directory (locate-dominating-file default-directory ".dir-locals.el")
;; ))

;; Registers

(dolist (r `(
             (?O (file . "~/.emacs.d/dot-org.org"))
             (?a (file . "~/.emacs.d/.abbrev_defs"))
             (?g (file . "~/.emacs.d/gnus-settings.el"))
             (?G (file . "~/.emacs.d/dot-gnus.el"))
             (?s (file . "~/.emacs.d/settings.el"))
             (?t (file . "~/Documents/Tasks/todo.txt"))
             (?u (file . "~/.emacs.d/site-lisp/xmsi-math-symbols-input.el"))
             (?v (file . "~/.emacs.d/dkh-core.org"))
             (?z (file . "~/.zshrc"))
             ))
  (set-register (car r) (cadr r)))



(defun replace-latin-alphabet-to-gothic (p1 p2 reverse-direction-p)
  "Replace English alphabets to Unicode gothic characters.
For example, A ⇒ 𝔄, a ⇒ 𝔞.

When called interactively, work on current text block or text selection. (a “text block” is text between empty lines)

If any `universal-argument' is given, reverse direction.

When called in elisp, the p1 and p2 are region begin/end positions to work on."
  (interactive
   (let ((bds (get-selection-or-unit 'block)) )
     (list (elt bds 1) (elt bds 2) current-prefix-arg )) )

  (let (
        (latin-to-gothic [ ["A" "𝔄"] ["B" "𝔅"] ["C" "ℭ"] ["D" "𝔇"] ["E" "𝔈"] ["F" "𝔉"] ["G" "𝔊"] ["H" "ℌ"] ["I" "ℑ"] ["J" "𝔍"] ["K" "𝔎"] ["L" "𝔏"] ["M" "𝔐"] ["N" "𝔑"] ["O" "𝔒"] ["P" "𝔓"] ["Q" "𝔔"] ["R" "ℜ"] ["S" "𝔖"] ["T" "𝔗"] ["U" "𝔘"] ["V" "𝔙"] ["W" "𝔚"] ["X" "𝔛"] ["Y" "𝔜"] ["Z" "ℨ"] ["a" "𝔞"] ["b" "𝔟"] ["c" "𝔠"] ["d" "𝔡"] ["e" "𝔢"] ["f" "𝔣"] ["g" "𝔤"] ["h" "𝔥"] ["i" "𝔦"] ["j" "𝔧"] ["k" "𝔨"] ["l" "𝔩"] ["m" "𝔪"] ["n" "𝔫"] ["o" "𝔬"] ["p" "𝔭"] ["q" "𝔮"] ["r" "𝔯"] ["s" "𝔰"] ["t" "𝔱"] ["u" "𝔲"] ["v" "𝔳"] ["w" "𝔴"] ["x" "𝔵"] ["y" "𝔶"] ["z" "𝔷"] ])

        (gothic-to-latin [ ["𝔄" "A"] ["𝔅" "B"] ["ℭ" "C"] ["𝔇" "D"] ["𝔈" "E"] ["𝔉" "F"] ["𝔊" "G"] ["ℌ" "H"] ["ℑ" "I"] ["𝔍" "J"] ["𝔎" "K"] ["𝔏" "L"] ["𝔐" "M"] ["𝔑" "N"] ["𝔒" "O"] ["𝔓" "P"] ["𝔔" "Q"] ["ℜ" "R"] ["𝔖" "S"] ["𝔗" "T"] ["𝔘" "U"] ["𝔙" "V"] ["𝔚" "W"] ["𝔛" "X"] ["𝔜" "Y"] ["ℨ" "Z"] ["𝔞" "a"] ["𝔟" "b"] ["𝔠" "c"] ["𝔡" "d"] ["𝔢" "e"] ["𝔣" "f"] ["𝔤" "g"] ["𝔥" "h"] ["𝔦" "i"] ["𝔧" "j"] ["𝔨" "k"] ["𝔩" "l"] ["𝔪" "m"] ["𝔫" "n"] ["𝔬" "o"] ["𝔭" "p"] ["𝔮" "q"] ["𝔯" "r"] ["𝔰" "s"] ["𝔱" "t"] ["𝔲" "u"] ["𝔳" "v"] ["𝔴" "w"] ["𝔵" "x"] ["𝔶" "y"] ["𝔷" "z"] ])

        useMap
        )

    (if reverse-direction-p
        (progn (setq useMap gothic-to-latin))
      (progn (setq useMap latin-to-gothic))
      )
    (save-excursion
      (let ((case-fold-search nil))
        (replace-pairs-region p1 p2 useMap ) ) ) ) )

(require 'random-quote)
;; http://blog.makezine.com/2011/04/08/the-awesome-button/
(defun awesome-button ()
  " Insert random string from .quotes file"
  (interactive)
  (let ((random-quote (pick-random-quote)))
    (kill-new random-quote 't)
    (message random-quote)
    ))
(bind-key "H-a" 'awesome-button)

;; no need to type a space after comma
(global-set-key (kbd ",") (lambda() (interactive) (insert ", ")))

(use-package my-modeline
  :if (not running-alternate-emacs)
  :defer t
)

;; http://emacsredux.com/blog/2013/04/28/switch-to-previous-buffer/
;; (bind-key "C-<tab>" 'mode-line-other-buffer)
(bind-key "C-<escape>" 'mode-line-other-buffer)

(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)

(defun listenv ()
  "List all environment variables in order."
  (interactive)
  (switch-to-buffer-other-window "*env*")
  (erase-buffer)
  (insert (mapconcat 'identity process-environment "\n"))
  (sort-lines nil (point-min) (point-max)))

;; Delete var without $
(global-set-key(kbd "C-c DEL")
               (lambda ()
                 (interactive)
                 (search-backward "$")
                 (forward-char)
                 (kill-word 1)))

(dolist
    (r `((?i (file . "~/.emacs.d/init.el"))
         (?e (file . "~/.emacs.d"))
         (?t (file . "~/Documents/Tasks/todo.txt"))
         (?s (file . "~/.emacs.d/settings.el"))
         (?o (file . "~/.emacs.d/dot-org.el"))
         (?g (file . "~/.emacs.d/dot-gnus.el"))
         (?O (file . "~/.emacs.d/org-settings.el"))
         (?G (file . "~/.emacs.d/gnus-settings.el"))
         ))
  (set-register (car r) (cadr r)))


;; Local Variables:
;;   mode: emacs-lisp
;;   mode: allout
;;   outline-regexp: "^;;;_\\([,. ]+\\)"
;; End:

;;; init.el ends here
