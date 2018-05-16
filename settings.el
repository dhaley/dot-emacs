(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Info-fit-frame-flag nil)
 '(TeX-PDF-mode t)
 '(TeX-electric-escape t)
 '(TeX-engine (quote xetex))
 '(TeX-view-program-list
   (quote
    (("Skim"
      ("osascript" " ~/bin/skim-gotopage.script" " %O"
       (mode-io-correlate " %(outpage)"))))))
 '(TeX-view-program-selection
   (quote
    (((output-dvi style-pstricks)
      "dvips and gv")
     (output-dvi "xdvi")
     (output-pdf "Skim")
     (output-html "xdg-open"))))
 '(abbrev-file-name "~/.emacs.d/abbrevs")
 '(ace-isearch-submode (quote ace-jump-char-mode))
 '(ace-window-display-mode t)
 '(ad-redefinition-action (quote accept))
 '(after-save-hook
   (quote
    (executable-make-buffer-file-executable-if-script-p)))
 '(alert-default-style (quote fringe))
 '(alert-notifier-command
   "/Applications/terminal-notifier.app/Contents/MacOS/terminal-notifier")
 '(align-c++-modes (quote (csharp-mode c++-mode c-mode java-mode groovy-mode)))
 '(align-to-tab-stop nil)
 '(allout-command-prefix ".")
 '(appt-display-interval 30)
 '(appt-message-warning-time 60)
 '(async-bytecomp-allowed-packages nil)
 '(auth-source-debug (quote trivia))
 '(auth-sources (quote ("~/Documents/.authinfo.gpg")))
 '(auto-compression-mode t)
 '(auto-save-file-name-transforms (quote (("\\`/[^/]*:.*" "/tmp" t))))
 '(auto-save-interval 64)
 '(auto-save-list-file-prefix "~/.emacs.d/data/auto-save-list/.saves-")
 '(auto-save-timeout 2)
 '(avy-case-fold-search nil)
 '(avy-keys (quote (97 111 101 117 105 100 104 116 110 115)))
 '(aw-background nil)
 '(aw-keys (quote (97 111 101 117 105 100 104 116 110)))
 '(backup-directory-alist
   (quote
    (("/Volumes/Files/" . "/Volumes/Files/.backups")
     ("\\(recentf\\|archive/sent\\)" . "/tmp")
     (".*" . "~/.backups"))))
 '(backward-delete-char-untabify-method (quote untabify))
 '(bbdb-default-country "")
 '(bbdb-file "~/Documents/bbdb")
 '(bbdb-message-caching-enabled nil)
 '(bbdb-no-duplicates t)
 '(bbdb-offer-save (quote savenoprompt))
 '(bbdb-silent-running t)
 '(bbdb-use-pop-up nil)
 '(bbdb-vcard-import-translation-table
   (quote
    (("CELL\\|CAR" . "Mobile")
     ("WORK" . "Work")
     ("HOME" . "Home")
     ("^$" . "Work"))))
 '(bbdb/mail-auto-create-p nil)
 '(bc-bookmark-file "~/.emacs.d/data/breadcrumb")
 '(bind-key-segregation-regexp "\\`\\(\\(C-[chx.] \\|M-[gso] \\)\\([CM]-\\)?\\|.+-\\)")
 '(bm-highlight-style (quote bm-highlight-only-fringe))
 '(bmkp-bmenu-commands-file "~/.emacs.d/data/bmk-bmenu-commands.el")
 '(bmkp-bmenu-state-file "~/.emacs.d/data/bmk-bmenu-state.el")
 '(bmkp-crosshairs-flag nil)
 '(bmkp-last-as-first-bookmark-file "~/Documents/bookmarks")
 '(bookmark-default-file "~/Documents/bookmarks")
 '(browse-url-browser-function (quote choose-browser))
 '(byte-compile-verbose nil)
 '(calendar-daylight-time-zone-name "MST")
 '(calendar-latitude 40.73471)
 '(calendar-longitude -89.554659)
 '(calendar-mark-holidays-flag t)
 '(calendar-standard-time-zone-name "MST")
 '(calendar-time-zone -420)
 '(cfw:read-date-command
   (lambda nil
     (interactive)
     (let
         ((xs
           (decode-time
            (org-time-string-to-time
             (org-read-date)))))
       (list
        (nth 4 xs)
        (nth 3 xs)
        (nth 5 xs)))))
 '(clean-buffer-list-kill-never-buffer-names
   (quote
    ("*scratch*" "*Messages*" "*server*" "*Group*" "*Org Agenda*" "todo.txt" "&bitlbee")))
 '(clean-buffer-list-kill-never-regexps
   (quote
    ("^ \\*Minibuf-.*\\*$" "^\\*Summary" "^\\*Article" "^#")))
 '(clean-buffer-list-kill-regexps (quote (".*")))
 '(column-number-mode t)
 '(company-coq-disabled-features
   (quote
    (hello unicode-math-backend refman-tactic-abbrevs-backend refman-vernac-abbrevs-backend dynamic-symbols-backend)))
 '(company-coq-prettify-symbols-alist
   (quote
    (("|-" . 8866)
     ("True" . 8868)
     ("False" . 8869)
     ("->" . 8594)
     ("-->" . 10230)
     ("<-" . 8592)
     ("<--" . 10229)
     ("<->" . 8596)
     ("<-->" . 10231)
     ("==>" . 10233)
     ("<==" . 10232)
     ("++>" . 10239)
     ("<++" . 11059)
     ("fun" . 955)
     ("forall" . 8704)
     ("exists" . 8707)
     ("/\\" . 8743)
     ("\\/" . 8744)
     ("~" . 172)
     ("+-" . 177)
     ("<=" . 8804)
     (">=" . 8805)
     ("<>" . 8800)
     ("*" . 215)
     ("++" . 10746)
     ("nat" . 120029)
     ("Z" . 8484)
     ("N" . 8469)
     ("Q" . 8474)
     ("Real" . 8477)
     ("bool" . 120121)
     ("Prop" . 120031))))
 '(compilation-always-kill t)
 '(compilation-ask-about-save nil)
 '(compilation-context-lines 10)
 '(compilation-scroll-output (quote first-error))
 '(compilation-skip-threshold 2)
 '(compilation-window-height 100)
 '(compile-command "tail /var/log/drupal.log")
 '(css-indent-offset 2)
 '(current-language-environment "UTF-8")
 '(cursor-in-non-selected-windows nil)
 '(custom-buffer-done-function (quote kill-buffer))
 '(custom-file "~/.emacs.d/settings.el")
 '(custom-raised-buttons nil)
 '(debug-on-error t)
 '(default-frame-alist
    (quote
     ((font . "-*-Source Code Pro-normal-normal-normal-*-15-*-*-*-m-0-iso10646-1")
      (cursor-color . "#b247ee"))))
 '(default-input-method "latin-1-prefix")
 '(default-major-mode (quote text-mode) t)
 '(deft-auto-save-interval 0.0)
 '(deft-directory "~/Documents/Notes")
 '(deft-text-mode (quote org-mode))
 '(delete-by-moving-to-trash t)
 '(delete-old-versions (quote none))
 '(diary-file "~/Documents/diary")
 '(diff-mode-hook
   (quote
    (diff-delete-empty-files diff-make-unified smerge-mode)))
 '(directory-free-space-args "-kh")
 '(dired-clean-up-buffers-too nil)
 '(dired-dwim-target t)
 '(dired-listing-switches "-lah")
 '(dired-no-confirm
   (quote
    (byte-compile chgrp chmod chown copy hardlink symlink touch)))
 '(dired-omit-files
   "^\\.?#\\|^\\.\\(DS_Store\\|localized\\|AppleDouble\\)$\\|^\\.\\.$")
 '(dired-omit-mode nil t)
 '(dired-recursive-copies (quote always))
 '(dired-recursive-deletes (quote always))
 '(dired-use-ls-dired (quote unspecified))
 '(diredful-init-file "~/.emacs.d/data/diredful-conf.el")
 '(display-time-use-mail-icon t)
 '(doc-view-resolution 300)
 '(drupal-convert-line-ending t)
 '(drupal-css-modes (quote (css-mode scss-mode)))
 '(drupal-drush-program "~/drush8/vendor/bin/drush")
 '(drupal-ignore-paths-regexp "\\(vendor\\|node_modules\\|libraries\\)")
 '(drupal-php-modes (quote (php-mode web-mode)))
 '(ediff-combination-pattern
   (quote
    ("<<<<<<< A: HEAD" A "||||||| Ancestor" Ancestor "=======" B ">>>>>>> B: Incoming")))
 '(ediff-diff-options "-w")
 '(ediff-highlight-all-diffs nil)
 '(ediff-show-clashes-only t)
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(edit-server-new-frame nil)
 '(el-get-auto-update-cached-recipes nil)
 '(el-get-dir "~/.emacs.d/site-lisp/")
 '(el-get-generate-autoloads nil)
 '(electric-indent-mode nil)
 '(elfeed-db-directory "~/Messages/.elfeed")
 '(elfeed-feeds
   (quote
    ("https://rss.myinterfase.com/rss/cuboulder_rssfeedcareerevents.xml")))
 '(emacs-lisp-mode-hook
   (quote
    (turn-on-auto-fill
     (lambda nil
       (ignore-errors
         (diminish
          (quote auto-fill-function))))
     eldoc-mode
     (lambda nil
       (local-set-key
        [(meta 46)]
        (quote find-function))
       (local-set-key
        [(control 109)]
        (quote newline-and-indent))))))
 '(emms-source-file-default-directory "/Volumes/Multimedia")
 '(enable-recursive-minibuffers t)
 '(erc-auto-query (quote window-noselect))
 '(erc-autoaway-message "I'm away (after %i seconds of idle-time)")
 '(erc-autojoin-channels-alist
   (quote
    (("freenode.net" "#drupal-colorado" "#emacs" "#thoughtbot")
     ("localhost"))))
 '(erc-autojoin-timing (quote ident))
 '(erc-fill-function (quote erc-fill-variable))
 '(erc-fill-static-center 12)
 '(erc-format-nick-function (quote erc-format-@nick))
 '(erc-generate-log-file-name-function (quote erc-generate-log-file-name-short))
 '(erc-header-line-format nil)
 '(erc-hide-list (quote ("JOIN" "NICK" "PART" "QUIT" "MODE")))
 '(erc-keywords
   (quote
    (" cu " " tor " "Damon" "dkh" "dhaley" "eshell" "matt$" "gmane" "gwene" "locate-dominating-file" "usenet" "proxy" "privacy" " socks " "gnus" "workgroups" "wireshark" "hushmail" "page manager" "super cookie" "[^.]php" "nxhtml" "nxml" "erc-keywords" "libnotify" "[^#]orgW?mode" "rate module" "[^#]conkeror" "noscript" "https-everywhere" "references module" "org2blog" "Johan Falk" "Lanier" "solarized" "bbdb" "magit" "w3m" "erc-hl" "erc-highlight" "web-mode" "Rasa" "laima" "prelude" "flycheck" "eproject" "locate-dominating-file" "projectile" "dirlocal")))
 '(erc-log-channels-directory "~/Messages/ERC")
 '(erc-log-write-after-send t)
 '(erc-modules
   (quote
    (autojoin button completion dcc fill identd image irccontrols list match menu move-to-prompt netsplit networks noncommands readonly replace ring scrolltobottom services smiley stamp spelling track truncate highlight-nicknames)))
 '(erc-nick "dkh")
 '(erc-pals
   (quote
    ("laimagaigalas" "matu4921" "^johnw!" "sachac" "matt" "kevin" "jo" "cathy" "wiegley" "qdot" "crafts" "miles" "Karin" "BerntH" "technomancy" "ultimateboy" "a00001" "bozhidar" "magnars" "erin" "sellout")))
 '(erc-port 6667)
 '(erc-priority-people-regexp "\\`[^#].+")
 '(erc-prompt-for-nickserv-password nil)
 '(erc-rename-buffers t)
 '(erc-replace-alist (quote (("</?FONT>" . ""))))
 '(erc-server "asimov.freenode.net")
 '(erc-server-reconnect-timeout 60)
 '(erc-server-send-ping-interval 45)
 '(erc-server-send-ping-timeout 180)
 '(erc-services-mode t)
 '(erc-text-matched-hook (quote (erc-hide-fools my-erc-hook)))
 '(erc-track-enable-keybindings t)
 '(erc-track-exclude (quote ("#emacs" "#git")))
 '(erc-track-exclude-types
   (quote
    ("JOIN" "KICK" "NICK" "PART" "QUIT" "MODE" "333" "353")))
 '(erc-track-faces-priority-list
   (quote
    (erc-error-face
     (erc-nick-default-face erc-current-nick-face)
     erc-current-nick-face erc-keyword-face
     (erc-nick-default-face erc-pal-face)
     erc-pal-face erc-nick-msg-face erc-direct-msg-face)))
 '(erc-track-score-mode t)
 '(erc-track-showcount t)
 '(erc-user-full-name (quote user-full-name))
 '(erc-yank-query-before-gisting nil)
 '(eshell-directory-name "~/.emacs.d/eshell/")
 '(eshell-history-size 1000)
 '(eshell-ls-dired-initial-args (quote ("-h")))
 '(eshell-ls-exclude-regexp "~\\'")
 '(eshell-ls-initial-args "-h")
 '(eshell-modules-list
   (quote
    (eshell-alias eshell-basic eshell-cmpl eshell-dirs eshell-glob eshell-hist eshell-ls eshell-pred eshell-prompt eshell-rebind eshell-script eshell-smart eshell-term eshell-unix eshell-xtra)))
 '(eshell-prompt-function
   (lambda nil
     (concat
      (abbreviate-file-name
       (eshell/pwd))
      (if
          (=
           (user-uid)
           0)
          " # " " $ "))))
 '(eshell-save-history-on-exit t)
 '(eshell-stringify-t nil)
 '(eshell-term-name "ansi")
 '(eshell-visual-commands
   (quote
    ("vi" "top" "screen" "less" "lynx" "rlogin" "telnet")))
 '(eudc-inline-expansion-format (quote ("%s <%s>" name email)))
 '(eval-expr-print-function (quote pp))
 '(eww-download-directory "~/dl")
 '(eww-lnum-actions-link-alist
   (quote
    ("----  Link   ----"
     (102 eww-lnum-visit "Visit")
     (101
      (lambda
        (info)
        (eww-lnum-visit info nil t))
      "Edit URL and visit")
     (70
      (lambda
        (info)
        (eww-lnum-visit info t))
      "Visit in new buffer")
     (69
      (lambda
        (info)
        (eww-lnum-visit info t t))
      "Edit URL and visit in new buffer")
     (98
      (lambda
        (info)
        (eww-lnum-visit info :background))
      "Open in background")
     (66
      (lambda
        (info)
        (eww-lnum-visit info :background t))
      "Edit URL and open in background")
     (100
      (lambda
        (info)
        (save-excursion
          (goto-char
           (cadr info))
          (eww-download)))
      "Download")
     (119
      (lambda
        (info)
        (let
            ((url
              (car info)))
          (kill-new url)
          (message url)))
      "Copy")
     (38
      (lambda
        (info)
        (eww-browse-with-external-browser
         (car info)))
      "Open in external browser")
     (68
      (lambda
        (info)
        (shell-command
         (concat "aria2c -d ~/Downloads -x5 '"
                 (car info)
                 "' &")
         "*Aria*"))
      "Download with Aria"))))
 '(eww-search-prefix "https://startpage.com/do/m/mobilesearch?query=")
 '(fill-column 78)
 '(find-ls-option (quote ("-print0 | xargs -0 ls -ld" . "-ld")))
 '(find-ls-subdir-switches "-alh")
 '(flx-ido-use-faces nil)
 '(flycheck-display-errors-delay 0.0)
 '(flycheck-phpmd-rulesets (quote ("drupal")))
 '(flycheck-standard-error-navigation t)
 '(flyspell-abbrev-p nil)
 '(flyspell-incorrect-hook (quote (flyspell-maybe-correct-transposition)))
 '(flyspell-use-meta-tab nil)
 '(font-lock-support-mode (quote jit-lock-mode))
 '(font-lock-verbose nil)
 '(frame-title-format
   (quote
    (:eval
     (concat
      (if buffer-file-name default-directory "%b")
      "    "
      (number-to-string
       (cdr
        (assq
         (quote width)
         (frame-parameters))))
      "x"
      (number-to-string
       (cdr
        (assq
         (quote height)
         (frame-parameters))))))) t)
 '(gc-cons-threshold 3500000)
 '(gdb-find-source-frame t)
 '(gdb-same-frame nil)
 '(ggtags-oversize-limit 1048576)
 '(ggtags-use-sqlite3 t)
 '(git-commit-mode-hook
   (quote
    (turn-on-auto-fill flyspell-mode git-commit-save-message)) t)
 '(glasses-separator "-")
 '(glasses-uncapitalize-p t)
 '(global-auto-complete-mode t)
 '(global-font-lock-mode t)
 '(global-undo-tree-mode t)
 '(gnutls-algorithm-priority "NORMAL:%COMPAT")
 '(grep-find-command (quote ("ag --noheading --column --ignore branches " . 43)))
 '(helm-adaptive-history-file "~/.emacs.d/data/helm-adaptive-history")
 '(helm-buffers-fuzzy-matching t)
 '(helm-completing-read-handlers-alist
   (quote
    ((describe-function . helm-completing-read-symbols)
     (describe-variable . helm-completing-read-symbols)
     (debug-on-entry . helm-completing-read-symbols)
     (find-function . helm-completing-read-symbols)
     (find-tag . helm-completing-read-with-cands-in-buffer)
     (ffap-alternate-file)
     (ffap)
     (tmm-menubar)
     (find-file)
     (magit-status . ido))))
 '(helm-delete-minibuffer-contents-from-point t)
 '(helm-descbinds-window-style (quote split-window))
 '(helm-ff-file-name-history-use-recentf t)
 '(helm-ff-search-library-in-sexp t)
 '(helm-ff-skip-boring-files t)
 '(helm-for-files-preferred-list
   (quote
    (helm-source-files-in-current-dir helm-source-recentf helm-source-bookmarks helm-source-file-cache helm-source-buffers-list helm-source-locate helm-source-ls-git)))
 '(helm-grep-default-recurse-command "rg --no-heading --color=always -j4 -n%cH -e %p %f")
 '(helm-gtags-auto-update t)
 '(helm-gtags-ignore-case t)
 '(helm-gtags-path-style (quote relative))
 '(helm-ls-git-show-abs-or-relative (quote relative))
 '(helm-quick-update t)
 '(helm-recentf-fuzzy-match t)
 '(hi2-ifte-offset 4)
 '(hi2-layout-offset 4)
 '(hi2-left-offset 4)
 '(hi2-show-indentations nil)
 '(hippie-expand-try-functions-list
   (quote
    (yas-hippie-try-expand try-complete-file-name-partially try-complete-file-name try-expand-all-abbrevs try-expand-list try-expand-line try-expand-dabbrev try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill try-complete-lisp-symbol-partially try-complete-lisp-symbol)))
 '(history-delete-duplicates t)
 '(history-length 200)
 '(hoogle-binary-path "hoogle")
 '(hpaste-announce (quote always))
 '(hpaste-blank-title nil)
 '(hpaste-channel "#drupal")
 '(hpaste-default-nick "dkh")
 '(hpaste-lang (quote always))
 '(ibuffer-default-display-maybe-show-predicates t)
 '(ibuffer-expert t)
 '(ibuffer-formats
   (quote
    ((mark modified read-only " "
           (name 16 -1)
           " "
           (size 6 -1 :right)
           " "
           (mode 16 16)
           " " filename)
     (mark " "
           (name 16 -1)
           " " filename))))
 '(ibuffer-maybe-show-regexps nil)
 '(ibuffer-saved-filter-groups
   (quote
    (("default"
      ("Commands"
       (or
        (mode . shell-mode)
        (mode . eshell-mode)
        (mode . term-mode)
        (mode . compilation-mode)))
      ("Helm"
       (mode . helm-mode))
      ("Magit"
       (or
        (mode . magit-status-mode)
        (mode . magit-log-mode)))
      ("Coq"
       (mode . coq-mode))
      ("C++"
       (or
        (mode . c-mode)
        (mode . c++-mode)))
      ("Lisp"
       (mode . emacs-lisp-mode))
      ("Dired"
       (mode . dired-mode))
      ("Gnus"
       (or
        (mode . message-mode)
        (mode . mail-mode)
        (mode . gnus-group-mode)
        (mode . gnus-summary-mode)
        (mode . gnus-article-mode)
        (name . "^\\.newsrc-dribble")))
      ("Org"
       (or
        (name . "^\\*Calendar\\*$")
        (name . "^diary$")
        (mode . org-mode)))
      ("Emacs"
       (or
        (name . "^\\*scratch\\*$")
        (name . "^\\*Messages\\*$")))))))
 '(ibuffer-show-empty-filter-groups nil)
 '(ibuffer-shrink-to-minimum-size t t)
 '(ibuffer-use-other-window t)
 '(icicle-Completions-text-scale-decrease 0)
 '(icicle-apropos-cycle-next-keys (quote ([next] [(control 110)])))
 '(icicle-apropos-cycle-previous-keys (quote ([prior] [(control 112)])))
 '(icicle-incremental-completion nil)
 '(icicle-max-candidates 100)
 '(ido-auto-merge-work-directories-length 0)
 '(ido-cannot-complete-command (quote ido-exit-minibuffer))
 '(ido-decorations
   (quote
    ("{" "}" "," ",..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))
 '(ido-enable-flex-matching t)
 '(ido-enable-last-directory-history nil)
 '(ido-enable-tramp-completion nil)
 '(ido-enter-matching-directory (quote first))
 '(ido-ignore-files
   (quote
    ("\\`CVS/" "\\`#" "\\`.#" "\\`\\.\\./" "\\`\\./" "\\`\\.DS_Store" "\\`\\.localized" "\\.sparsebundle/" "\\.dmg\\'")))
 '(ido-save-directory-list-file "~/.emacs.d/data/ido.last")
 '(ido-use-virtual-buffers t)
 '(ido-use-virtual-buffers-automatically t)
 '(idris-interpreter-flags (quote ("-p" "effects")))
 '(image-dired-dir "~/.emacs.d/data/image-dired/")
 '(indent-tabs-mode nil)
 '(inhibit-startup-echo-area-message "dkh")
 '(inhibit-startup-screen t)
 '(initial-major-mode (quote org-mode))
 '(initsplit-customizations-alist
   (quote
    (("\\`\\(gnus\\|nn\\|message\\|mail\\|mm-\\|smtp\\|send-mail\\|check-mail\\|spam\\|sc-\\)" "~/.emacs.d/gnus-settings.el" nil nil)
     ("\\`\\(org-\\)" "~/.emacs.d/org-settings.el" nil nil))))
 '(ipa-file "~/Documents/ipa")
 '(ipa-overlay-position "above")
 '(irfc-directory "~/Archives/Admin/RFC/")
 '(ispell-extra-args (quote ("--sug-mode=fast" "--keyboard=dvorak")))
 '(ispell-personal-dictionary "~/Documents/.aspell.en.pws")
 '(ispell-program-name "/usr/local/bin/aspell")
 '(jka-compr-compression-info-list
   (quote
    (["\\.Z\\(~\\|\\.~[0-9]+~\\)?\\'" "compressing" "compress"
      ("-c")
      "uncompressing" "uncompress"
      ("-c")
      nil t "^_\235"]
     ["\\.bz2\\(~\\|\\.~[0-9]+~\\)?\\'" "bzip2ing" "bzip2" nil "bunzip2ing" "bzip2"
      ("-d")
      nil t "BZh"]
     ["\\.tbz\\'" "bzip2ing" "bzip2" nil "bunzip2ing" "bzip2"
      ("-d")
      nil nil\
      "BZh"]
     ["\\.tgz\\'" "compressing" "gzip"
      ("-c")
      "uncompressing" "gzip"
      ("-c" "-q" "-d")
      t nil "^_\213"]
     ["\\.g?z\\(~\\|\\.~[0-9]+~\\)?\\'" "compressing" "gzip"
      ("-c")
      "uncompressing" "gzip"
      ("-c" "-d")
      t t "^_\213"]
     ["\\
      .dz\\'" nil nil nil "uncompressing" "gzip"
      ("-c" "-d")
      nil t "^_\213"])))
 '(kill-do-not-save-duplicates t)
 '(kill-whole-line t)
 '(large-file-warning-threshold nil)
 '(ledger-file "~/Documents/Accounts/ledger.dat")
 '(ledger-post-use-ido t)
 '(line-number-mode t)
 '(load-prefer-newer t)
 '(mac-command-modifier (quote hyper))
 '(mac-function-modifier (quote hyper))
 '(mac-option-modifier (quote meta))
 '(mac-pass-command-to-system nil)
 '(mac-pass-control-to-system nil)
 '(mac-wheel-button-is-mouse-2 nil)
 '(magit-auto-revert-mode nil)
 '(magit-completing-read-function (quote helm--completing-read-default))
 '(magit-diff-options nil)
 '(magit-fetch-arguments nil)
 '(magit-highlight-trailing-whitespace nil)
 '(magit-highlight-whitespace nil)
 '(magit-process-popup-time 15)
 '(magit-stage-all-confirm nil)
 '(magit-unstage-all-confirm nil)
 '(magit-use-overlays nil)
 '(make-backup-file-name-function (quote my-make-backup-file-name))
 '(markdown-command "pandoc -f markdown_mmd -S")
 '(markdown-command-needs-filename t)
 '(markdown-enable-math t)
 '(markdown-open-command "open-markdown")
 '(menu-bar-mode nil)
 '(mml2015-use (quote epg) t)
 '(moccur-following-mode-toggle nil)
 '(modelinepos-column-limit 80)
 '(mu4e-attachment-dir "~/dl")
 '(mudel-mode-hook (quote (mudel-add-scroll-to-bottom)))
 '(mudel-output-filter-functions (quote (ansi-color-process-output)))
 '(multi-term-program "/usr/bin/screen")
 '(multi-term-program-switches "-DR")
 '(multi-term-scroll-show-maximum-output t)
 '(next-line-add-newlines nil)
 '(ns-alternate-modifier (quote alt))
 '(ns-command-modifier (quote meta))
 '(nxml-sexp-element-flag t)
 '(nxml-slash-auto-complete-flag t)
 '(olivetti-hide-mode-line t)
 '(pabbrev-idle-timer-verbose nil)
 '(package-archives
   (quote
    (("gnu" . "https://elpa.gnu.org/packages/")
     ("MELPA" . "https://melpa.org/packages/")
     ("Marmalade" . "https://marmalade-repo.org/packages/"))))
 '(package-selected-packages
   (quote
    (drupal-mode heap auto-overlays zenburn-theme twilight-theme tronesque-theme tron-theme theme-changer ssh soothe-theme solarized-theme purple-haze-theme pony-mode pastels-on-dark-theme let-alist late-night-theme jujube-theme irfc inf-ruby gratuitous-dark-theme gotham-theme gandalf-theme django-theme deep-thought-theme csv-mode color-theme-ir-black clues-theme bubbleberry-theme auctex assemblage-theme ascii ample-zen-theme ample-theme)))
 '(page-break-lines-modes
   (quote
    (emacs-lisp-mode compilation-mode outline-mode prog-mode)))
 '(parens-require-spaces t)
 '(pcomplete-compare-entries-function (quote file-newer-than-file-p))
 '(persistent-scratch-file-name "~/.emacs.d/data/persistent-scratch")
 '(php-boris-command "~/src/boris/bin/boris")
 '(php-completion-file "~/Messages/php/php-completion-file")
 '(php-executable "/Applications/MAMP/bin/php/php5.5.18/bin/php")
 '(php-extras-eldoc-functions-file "~/Messages/php/php-extras-eldoc-functions")
 '(php-mode-coding-style (quote drupal))
 '(php-template-compatibility nil)
 '(pp^L-^L-string
   "                                                                              ")
 '(projectile-cache-file "~/.emacs.d/data/projectile.cache")
 '(projectile-completion-system (quote grizzl))
 '(projectile-drupal-base-url-function (quote dkh-get-base-url))
 '(projectile-drupal-site-base-url-dev "http://www-dev.colorado.edu")
 '(projectile-drupal-site-base-url-prod "http://www.colorado.edu")
 '(projectile-drupal-site-base-url-stage "http://www-stage.colorado.edu")
 '(projectile-drupal-site-base-url-test "http://www-test.colorado.edu")
 '(projectile-drupal-site-name-function (quote dkh-get-site-name))
 '(projectile-enable-caching t)
 '(projectile-globally-ignored-files (quote ("TAGS" "GPATH" "GRTAGS" "GTAGS" "ID")))
 '(projectile-known-projects-file "~/.emacs.d/data/projectile-bookmarks.eld")
 '(projectile-project-root-files-bottom-up
   (quote
    ("includes/common.inc" "includes/bootstrap.inc" ".projectile" ".git" ".hg" ".fslckout" ".bzr" "_darcs")))
 '(projectile-switch-project-action (quote dkh-projectile-dired))
 '(projectile-switch-project-hook (quote dkh-project-record))
 '(proof-auto-action-when-deactivating-scripting (quote retract))
 '(proof-autosend-enable nil)
 '(proof-electric-terminator-enable t)
 '(proof-shell-fiddle-frames nil)
 '(proof-splash-enable nil)
 '(proof-sticky-errors t)
 '(ps-font-size (quote (8 . 10)))
 '(ps-footer-font-size (quote (12 . 14)))
 '(ps-header-font-size (quote (12 . 14)))
 '(ps-header-title-font-size (quote (14 . 16)))
 '(ps-line-number-font-size 10)
 '(ps-print-color-p nil)
 '(rdebug-many-windows nil)
 '(read-buffer-function (quote ido-read-buffer))
 '(recentf-auto-cleanup (quote never))
 '(recentf-exclude
   (quote
    ("~\\'" "\\`out\\'" "\\.log\\'" "^/[^/]*:" "\\.el\\.gz\\'")))
 '(recentf-max-saved-items 2000)
 '(recentf-save-file "~/.emacs.d/data/recentf")
 '(redisplay-dont-pause t t)
 '(regex-tool-backend (quote perl))
 '(runner-init-file "~/.emacs.d/data/runner-conf.el")
 '(sage-view-anti-aliasing-level 4)
 '(sage-view-margin (quote (20 . 20)))
 '(sage-view-scale 2.0)
 '(same-window-buffer-names
   (quote
    ("*eshell*" "*shell*" "*mail*" "*inferior-lisp*" "*ielm*" "*scheme*")))
 '(save-abbrevs (quote silently))
 '(save-interprogram-paste-before-kill t)
 '(save-kill-file-name "~/.emacs.d/data/kill-ring-saved.el")
 '(scroll-bar-mode nil)
 '(select-enable-clipboard t)
 '(semanticdb-default-save-directory "~/.emacs.d/data/semanticdb")
 '(server-mode t)
 '(session-globals-exclude (quote (load-history flyspell-auto-correct-ring)))
 '(session-globals-include
   (quote
    ((kill-ring 10 nil)
     (session-file-alist 200 t)
     (file-name-history 200 nil)
     search-ring regexp-search-ring sr-history-registry)))
 '(session-initialize (quote (session places keys)))
 '(session-name-disable-regexp "\\(\\`/tmp\\|COMMIT_EDITMSG\\)")
 '(session-registers (quote (t (0 . 127))))
 '(session-save-file "~/.emacs.d/data/session")
 '(shm-auto-insert-bangs nil)
 '(shm-indent-spaces 4)
 '(shm-use-hdevtools t)
 '(shm-use-presentation-mode t)
 '(show-paren-delay 0)
 '(slime-kill-without-query-p t)
 '(slime-repl-history-file "~/.emacs.d/data/slime-history.eld")
 '(slime-startup-animation nil)
 '(solarized-distinct-fringe-background t)
 '(solarized-high-contrast-mode-line nil)
 '(sp-highlight-pair-overlay nil)
 '(sql-sqlite-program "sqlite3")
 '(sr-attributes-display-mask (quote (nil nil t nil nil nil)))
 '(sr-autoload-extensions nil)
 '(sr-kill-unused-buffers nil)
 '(sr-listing-switches "--time-style=locale --group-directories-first -alDhgG")
 '(sr-loop-use-popups nil)
 '(sr-popviewer-style (quote single-frame))
 '(sr-show-file-attributes nil)
 '(sr-show-hidden-files t)
 '(sr-use-commander-keys nil)
 '(sr-windows-default-ratio 80)
 '(ssl-certificate-verification-policy 1)
 '(starttls-extra-arguments nil)
 '(starttls-gnutls-program (executable-find "gnutls-cli"))
 '(starttls-use-gnutls t)
 '(svn-status-hide-unmodified t)
 '(switch-to-buffer-preserve-window-point (quote already-displayed))
 '(tags-apropos-verbose t)
 '(tags-case-fold-search nil)
 '(tail-max-size 25)
 '(tail-volatile nil)
 '(temp-buffer-resize-mode t)
 '(term-bind-key-alist
   (quote
    (("C-c C-c" . term-interrupt-subjob)
     ("C-b" . my-term-send-raw-at-prompt)
     ("C-f" . my-term-send-raw-at-prompt)
     ("C-a" . my-term-send-raw-at-prompt)
     ("C-e" . my-term-send-raw-at-prompt)
     ("C-p" . previous-line)
     ("C-n" . next-line)
     ("C-s" . isearch-forward)
     ("C-r" . isearch-backward)
     ("C-m" . term-send-raw)
     ("M-f" . term-send-forward-word)
     ("M-b" . term-send-backward-word)
     ("M->" . my-term-end-of-buffer)
     ("M-o" . term-send-backspace)
     ("M-p" . term-send-up)
     ("M-n" . term-send-down)
     ("M-d" . term-send-forward-kill-word)
     ("M-DEL" . term-send-backward-kill-word)
     ("M-r" . term-send-reverse-search-history)
     ("M-," . term-send-input)
     ("M-." . comint-dynamic-complete)
     ("C-y" . term-paste))))
 '(term-buffer-maximum-size 0)
 '(term-scroll-show-maximum-output t)
 '(text-mode-hook
   (quote
    (turn-on-auto-fill
     (lambda nil
       (ignore-errors
         (diminish
          (quote auto-fill-function)))))))
 '(tls-program
   (quote
    ("openssl s_client -connect %h:%p -no_ssl2 -ign_eof -CApath /etc/postfix/certs -cert ~/Messages/me.pem")))
 '(tool-bar-mode nil)
 '(tramp-auto-save-directory "~/.backups" nil (tramp))
 '(tramp-default-method "ssh" nil (tramp))
 '(tramp-default-method-alist
   (quote
    (("\\`\\(127\\.0\\.0\\.1\\|::1\\|localhost6?\\)\\'" "\\`root\\'" "sudo"))) nil (tramp))
 '(tramp-persistency-file-name "~/.emacs.d/data/tramp")
 '(trash-directory "~/.Trash")
 '(undo-limit 800000)
 '(undo-tree-auto-save-history t)
 '(undo-tree-history-directory-alist (quote ((".*" . "~/.backups"))))
 '(undo-tree-mode-lighter "")
 '(undo-tree-visualizer-timestamps t)
 '(uniquify-buffer-name-style (quote post-forward-angle-brackets) nil (uniquify))
 '(url-cache-directory "~/.emacs.d/data/url/cache")
 '(url-configuration-directory "~/.emacs.d/data/url/")
 '(url-irc-function (quote url-irc-erc))
 '(user-full-name "Damon Haley")
 '(user-initials "dkh")
 '(user-mail-address "damon.k.haley@gmail.com")
 '(vc-command-messages t)
 '(vc-follow-symlinks t)
 '(vc-git-diff-switches (quote ("-w" "-U3")))
 '(vc-handled-backends (quote (GIT SVN CVS Bzr Hg)))
 '(vc-make-backup-files t)
 '(version-control t)
 '(visible-bell t)
 '(w3m-cookie-accept-bad-cookies (quote ask))
 '(w3m-default-display-inline-images t)
 '(w3m-fill-column 80)
 '(w3m-use-cookies t)
 '(warning-minimum-log-level :error)
 '(wdired-use-dired-vertical-movement (quote sometimes))
 '(web-mode-enable-comment-keywords t t)
 '(wg-mode-line-on nil)
 '(wg-morph-on nil)
 '(wg-prefix-key "")
 '(wg-query-for-save-on-emacs-exit nil)
 '(wg-query-for-save-on-workgroups-mode-exit nil)
 '(whitespace-auto-cleanup t t)
 '(whitespace-line-column 110)
 '(whitespace-rescan-timer-time nil t)
 '(whitespace-silent t t)
 '(whitespace-style (quote (face trailing lines space-before-tab empty)))
 '(workgroups-mode nil)
 '(x-stretch-cursor t)
 '(yaoddmuse-directory "~/.emacs.d/doc")
 '(yas-prompt-functions
   (quote
    (yas-ido-prompt yas-completing-prompt yas-no-prompt)))
 '(yas-snippet-dirs
   (quote
    ("~/.emacs.d/snippets" "~/.emacs.d/site-lisp/emacs-drupal-snippets/snippets" "~/.emacs.d/site-lisp/css-scss")))
 '(yas-triggers-in-field t)
 '(yas-wrap-around-region t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(diff-added ((((background dark)) (:foreground "#FFFF9B9BFFFF")) (t (:foreground "DarkGreen"))))
 '(diff-changed ((((background dark)) (:foreground "Yellow")) (t (:foreground "MediumBlue"))))
 '(diff-context ((((background dark)) (:foreground "White")) (t (:foreground "Black"))))
 '(diff-file-header ((((background dark)) (:foreground "Cyan" :background "Black")) (t (:foreground "Red" :background "White"))))
 '(diff-header ((((background dark)) (:foreground "Cyan")) (t (:foreground "Red"))))
 '(diff-index ((((background dark)) (:foreground "Magenta")) (t (:foreground "Green"))))
 '(diff-nonexistent ((((background dark)) (:foreground "#FFFFFFFF7474")) (t (:foreground "DarkBlue"))))
 '(flyparse-error-face ((t (:background "LightPink"))))
 '(flyparse-warning-face ((t (:background "DeepSkyBlue"))))
 '(ghc-face-error ((t (:inherit default))))
 '(ghc-face-warn ((t (:inherit default))))
 '(idris-loaded-region-face ((t (:background "#eaf8ff"))))
 '(wg-current-workgroup-face ((t (:foreground "purple")))))
