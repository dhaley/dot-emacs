;; Misc defuns go here
;; It wouldn't hurt to look for patterns and extract once in a while

(defmacro create-simple-keybinding-command (name key)
  `(defmacro ,name (&rest fns)
     (list 'global-set-key (kbd ,key) `(lambda ()
                                         (interactive)
                                         ,@fns))))

(create-simple-keybinding-command f2 "<f2>")
(create-simple-keybinding-command f5 "<f5>")
(create-simple-keybinding-command f6 "<f6>")
(create-simple-keybinding-command f7 "<f7>")
(create-simple-keybinding-command f8 "<f8>")
(create-simple-keybinding-command f9 "<f9>")
(create-simple-keybinding-command f10 "<f10>")
(create-simple-keybinding-command f11 "<f11>")
(create-simple-keybinding-command f12 "<f12>")

(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (goto-line (read-number "Goto line: ")))
    (linum-mode -1)))

;; Add spaces and proper formatting to linum-mode. It uses more room than
;; necessary, but that's not a problem since it's only in use when going to
;; lines.
(setq linum-format (lambda (line)
  (propertize
   (format (concat " %"
                   (number-to-string
                    (length (number-to-string
                             (line-number-at-pos (point-max)))))
                   "d ")
           line)
   'face 'linum)))

(defun isearch-yank-selection ()
  "Put selection from buffer into search string."
  (interactive)
  (when (region-active-p)
    (deactivate-mark))
  (isearch-yank-internal (lambda () (mark))))

(defun region-as-string ()
  (buffer-substring (region-beginning)
                    (region-end)))

(defun isearch-forward-use-region ()
  (interactive)
  (when (region-active-p)
    (add-to-history 'search-ring (region-as-string))
    (deactivate-mark))
  (call-interactively 'isearch-forward))

(defun isearch-backward-use-region ()
  (interactive)
  (when (region-active-p)
    (add-to-history 'search-ring (region-as-string))
    (deactivate-mark))
  (call-interactively 'isearch-backward))

(defun view-url ()
  "Open a new buffer containing the contents of URL."
  (interactive)
  (let* ((default (thing-at-point-url-at-point))
         (url (read-from-minibuffer "URL: " default)))
    (switch-to-buffer (url-retrieve-synchronously url))
    (rename-buffer url t)
    ;; TODO: switch to nxml/nxhtml mode
    (cond ((search-forward "<?xml" nil t) (xml-mode))
          ((search-forward "<html" nil t) (html-mode)))))

(defun linkify-region-from-kill-ring (start end)
  (interactive "r")
  (let ((text (buffer-substring start end)))
    (delete-region start end)
    (insert "<a href=\"")
    (yank)
    (insert (concat "\">" text "</a>"))))

(defun recompile-init ()
  "Byte-compile all your dotfiles again."
  (interactive)
  (byte-recompile-directory dotfiles-dir 0))

(defun sudo-edit (&optional arg)
  (interactive "p")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun add-file-find-hook-with-pattern (pattern fn &optional contents)
  "Add a find-file-hook that calls FN for files where PATTERN
matches the file name, and optionally, where CONTENT matches file contents.
Both PATTERN and CONTENTS are matched as regular expressions."
  (lexical-let ((re-pattern pattern)
                (fun fn)
                (re-content contents))
    (add-hook 'find-file-hook
              (lambda ()
                (if (and
                     (string-match re-pattern (buffer-file-name))
                     (or (null re-content)
                         (string-match re-content
                                       (buffer-substring (point-min) (point-max)))))
                    (apply fun ()))))))

;; Fix kmacro-edit-lossage, it's normal implementation
;; is bound tightly to C-h
(defun kmacro-edit-lossage ()
  "Edit most recent 300 keystrokes as a keyboard macro."
  (interactive)
  (kmacro-push-ring)
  (edit-kbd-macro 'view-lossage))

(defun turn-on-subword-mode ()
  "Turns on subword-mode"
  (subword-mode 1))

;; https://github.com/bradleywright/emacs-d/blob/master/elisp.el

;; gotten from:
;; http://stackoverflow.com/questions/12492/pretty-printing-xml-files-on-emacs
(defun bf-pretty-print-xml-region (begin end)
  "Pretty format XML markup in region. You need to have nxml-mode
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this. The function inserts linebreaks to separate tags that have
nothing but whitespace between them. It then indents the markup
by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
    (nxml-mode)
    (goto-char begin)
    (while (search-forward-regexp "\>[ \\t]*\<" nil t)
      (backward-char) (insert "\n"))
    (indent-region begin end)))

(defun bw-start-term (buffer-name)
  "Start a terminal and rename buffer."
  (interactive "sbuffer name: ")
  (ansi-term (getenv "SHELL"))
  (rename-buffer (concat "*term: " buffer-name "*") t))


(defun local-hl-line-mode-off ()
  "Turn hl-line-mode off locally to a buffer"
  (interactive)
  (hl-line-mode -1))

(defun local-hl-line-mode-on ()
  "Turn hl-line-mode off locally to a buffer"
  (interactive)
  (make-local-variable 'global-hl-line-mode)
  (hl-line-mode))

(defun turn-on-flymake-mode ()
  "Turns on flymake-mode locally"
  (interactive)
  (flymake-mode 1))

(defun turn-on-paredit ()
  "Turn paredit-mode on locally"
  (paredit-mode 1))

(defun chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'" str)
    (setq str (replace-match "" t t str))) str)

(defun get-keychain-password (account-name)
  "Gets `account` keychain password from OS X Keychain"
  (interactive "sAccount name: ")
  (chomp
   (shell-command-to-string
    (concat
     "security 2>&1 >/dev/null find-generic-password -ga "
     account-name
     "| sed 's/^password: \\\"\\(.*\\)\\\"/\\1/'"))))

(defun get-buffer-line-length ()
  "Counts the number of lines in the current buffer"
  (count-lines (point-min) (point-max)))

(defun change-mode-if-not-in-mode (mode)
  "Changes to a mode if we're not already in that mode"
  (when (not (eq mode major-mode))
    (funcall mode)))

;; hook related functions, since anonymous functions can't be
;; guaranteed to not be added multiple times
(defun bw-fill-column ()
  (setq fill-column 72))

(defun bw-turn-on-auto-fill ()
  (turn-on-auto-fill))

(defun bw-clojure-repl-program ()
  "Changes lisp function to use Leiningen repl"
  (setq inferior-lisp-program "lein repl"))

(defun bw-clojure-slime-repl-font-lock ()
  "Gives us Clojure font lock in the repl"
  (let (font-lock-mode)
    (clojure-mode-font-lock-setup)))

;; http://paste.lisp.org/display/129008
(defun quit-or-hide ()
  (interactive)
  (if (boundp 'server-name)
      (if (> (length server-clients) 1)
          (delete-frame)
        (make-frame-invisible nil t))
    (bw-kill-emacs)))

(defun bw-kill-emacs ()
  "Warn before exiting Emacs"
  (interactive)
  (cond ((y-or-n-p "Quit Emacs? ")
         (save-buffers-kill-emacs))))

(defun bw-turn-off-trailing-whitespace ()
  "Turns off trailing whitespace"
  (set (make-local-variable 'whitespace-line-column) nil)
  (whitespace-mode -1))

(defun my-insert-tab (&optional arg)
  "inserts just whitespace"
  (interactive "P")
  (insert-tab arg))

;; from: http://stackoverflow.com/a/7934783
(defun beautify-json ()
  "Indents and pretties JSON structures"
  (interactive)
  (let ((b (if mark-active (min (point) (mark)) (point-min)))
        (e (if mark-active (max (point) (mark)) (point-max))))
    (shell-command-on-region b e
                             "python -mjson.tool" (current-buffer) t)))

(defun bw-load-mode-files (&optional load-from)
  "Loads all files resident in the `modes` directory"
  (let ((modes-dir (or load-from (concat dotfiles-dir "modes"))))
    (mapc 'load (directory-files modes-dir t "^[^#].*el$"))))

(defun bw-ido-find-tag ()
  "Find a tag using ido"
  (interactive)
  (tags-completion-table)
  (let (tag-names)
    (mapc (lambda (x)
            (unless (integerp x)
              (push (prin1-to-string x t) tag-names)))
          tags-completion-table)
    (find-tag (ido-completing-read "Tag: " tag-names))))

(defun bw-locate-library-dir (library)
  "Locates the directory containing a loaded library"
  (file-name-directory (locate-library library)))

(defun bw-add-to-load-path (dir)
  (add-to-list 'load-path dir))

(defun bw-join-dirs (prefix suffix)
  (file-name-as-directory (concat prefix suffix)))


(defun bw-shorten-dir (dir)
  "Shortens a directory path to e.g ~/src"
  (replace-regexp-in-string (getenv "HOME") "~" dir))

(defun bw-iedit-defun ()
  "Calls iedit with a prefix of 0"
  (interactive)
  (let ((current-prefix-arg '(0)))
    (call-interactively 'iedit-mode)))

(defun bw-occur-invalid-chars ()
  "Finds characters that aren't in the displayable range for ASCII"
  (interactive)
  (occur "[^\000-\177]"))

; https://github.com/robru/.emacs.d/blob/master/defuns.el
(defun better-mouse-buffer-menu ()
  "Trigger buffer-menu in the clicked-in window.

This is better than mouse-buffer-menu because it's colorized and
sorted in a more useful way."
  (interactive "@")
  (buffer-menu))

(defun kill-this-buffer-properly ()
  "Close this file and end any emacsclient sessions associated with it."
  (interactive)
  (if server-buffer-clients
      (server-edit)
    (kill-this-buffer)
    (buffer-menu)))

(defun back-to-indentation-or-home ()
  "Toggle point between beginning of line, or first non-whitespace character."
  (interactive "^") ;; Set mark if shift key used.
  (if (looking-at "^") ;; Regex meaning "beginning of line"
      (back-to-indentation)
    (move-beginning-of-line nil)))

(defun region-or-line-beginning ()
  "Identifies either the beginning of the line or the region, as appropriate."
  (if (use-region-p)
      (region-beginning)
    (line-beginning-position)))

(defun region-or-line-end (&optional offset)
  "Identifies either the end of the line or the region, as appropriate."
  (if (use-region-p)
      (region-end)
    (+ (or offset 0) (line-end-position))))

(defun duplicate-region-or-line ()
  "Duplicates the region, or the whole line at point."
  (interactive)
  (save-excursion
    (kill-region (region-or-line-beginning) (region-or-line-end 1))
    (yank) (yank)))

(defun comment-or-uncomment-whole-lines (beg end)
  "Comment or uncomment only whole lines."
  (interactive "r")
  (comment-or-uncomment-region
   (save-excursion (goto-char beg) (line-beginning-position))
   (save-excursion (goto-char end) (line-end-position))))

(defun comment-or-uncomment-smartly ()
  "Do What I Mean: Comment either the current line, or the region."
  (interactive)
  (comment-or-uncomment-whole-lines
   (region-or-line-beginning) (region-or-line-end)))

(defun cut-region-or-current-line ()
  "If no region is present, cut current line."
  (interactive)
  (if cua--rectangle
      (cua-cut-rectangle -1)
    (kill-region (region-or-line-beginning) (region-or-line-end 1))))

(defun copy-region-or-current-line ()
  "If no region is present, copy current line."
  (interactive)
  (if cua--rectangle
      (cua-copy-rectangle -1)
    (copy-region-as-kill (region-or-line-beginning) (region-or-line-end 1))))

(defun untabify-buffer ()
  "Strip tabs out, unless the current buffer is a Makefile."
  (interactive)
  (unless (string-match "makefile" mode-name)
    (untabify (point-min) (point-max))))

(defun cleanup-buffer ()
  "Strip all kinds of bad whitespace out."
  (interactive)
  (delete-trailing-whitespace) ; Strip spaces from EOL
  (set-buffer-file-coding-system 'utf-8)
  (save-excursion
    (save-restriction
      (widen)
      (untabify-buffer) ; Strip tabs everywhere
      (goto-char (point-max))
      (delete-blank-lines)))) ; Strip newlines from EOF

(defun aggressive-just-one-space ()
  "Like just-one-space, but across newlines, too."
  (interactive)
  (just-one-space)
  (while (looking-at "$")
    (delete-forward-char 1)
    (just-one-space)))

