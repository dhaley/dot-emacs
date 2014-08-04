(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Info-fit-frame-flag nil)
 '(TeX-PDF-mode t)
 '(TeX-electric-escape t)
 '(TeX-engine (quote xetex))
 '(TeX-expand-list (quote (("%p" TeX-printer-query) ("%q" (lambda nil (TeX-printer-query t))) ("%V" (lambda nil (TeX-source-correlate-start-server-maybe) (TeX-view-command-raw))) ("%vv" (lambda nil (TeX-source-correlate-start-server-maybe) (TeX-output-style-check TeX-output-view-style))) ("%v" (lambda nil (TeX-source-correlate-start-server-maybe) (TeX-style-check TeX-view-style))) ("%r" (lambda nil (TeX-style-check TeX-print-style))) ("%l" (lambda nil (TeX-style-check LaTeX-command-style))) ("%(PDF)" (lambda nil (if (and (eq TeX-engine (quote default)) (or TeX-PDF-mode TeX-DVI-via-PDFTeX)) "pdf" ""))) ("%(PDFout)" (lambda nil (cond ((and (eq TeX-engine (quote xetex)) (not TeX-PDF-mode)) " -no-pdf") ((and (eq TeX-engine (quote luatex)) (not TeX-PDF-mode)) " --output-format=dvi") ((and (eq TeX-engine (quote default)) (not TeX-PDF-mode) TeX-DVI-via-PDFTeX) " \"\\pdfoutput=0 \"") (t "")))) ("%(mode)" (lambda nil (if TeX-interactive-mode "" " -interaction=nonstopmode"))) ("%(o?)" (lambda nil (if (eq TeX-engine (quote omega)) "o" ""))) ("%(tex)" (lambda nil (eval (nth 2 (assq TeX-engine (TeX-engine-alist)))))) ("%(latex)" (lambda nil (eval (nth 3 (assq TeX-engine (TeX-engine-alist)))))) ("%(execopts)" ConTeXt-expand-options) ("%S" TeX-source-correlate-expand-options) ("%dS" TeX-source-specials-view-expand-options) ("%cS" TeX-source-specials-view-expand-client) ("%(outpage)" (lambda nil (if TeX-source-correlate-output-page-function (funcall TeX-source-correlate-output-page-function) "1"))) ("%s" file nil t) ("%t" file t t) ("%`" (lambda nil (setq TeX-command-pos t TeX-command-text ""))) (" \"\\" (lambda nil (if (eq TeX-command-pos t) (setq TeX-command-pos pos pos (+ 3 pos)) (setq pos (1+ pos))))) ("\"" (lambda nil (if (numberp TeX-command-pos) (setq TeX-command-text (concat TeX-command-text (substring command TeX-command-pos (1+ pos))) command (concat (substring command 0 TeX-command-pos) (substring command (1+ pos))) pos TeX-command-pos TeX-command-pos t) (setq pos (1+ pos))))) ("%'" (lambda nil (prog1 (if (stringp TeX-command-text) (progn (setq pos (+ (length TeX-command-text) 9) TeX-command-pos (and (string-match " " (funcall file t t)) "\"")) (concat TeX-command-text " \"\\input\"")) (setq TeX-command-pos nil) "") (setq TeX-command-text nil)))) ("%n" TeX-current-line) ("%d" file "dvi" t) ("%f" file "ps" t) ("%o" (lambda nil (funcall file (TeX-output-extension) t))) ("%b" TeX-current-file-name-master-relative) ("%m" preview-create-subdirectory) ("%O" (lambda nil (expand-file-name (funcall file (TeX-output-extension) t)))))))
 '(TeX-view-program-list (quote (("Skim" ("osascript" " ~/bin/skim-gotopage.script" " %O" (mode-io-correlate " %(outpage)"))))))
 '(TeX-view-program-selection (quote (((output-dvi style-pstricks) "dvips and gv") (output-dvi "xdvi") (output-pdf "Skim") (output-html "xdg-open"))))
 '(abbrev-file-name "~/.emacs.d/.abbrev_defs")
 '(abbrev-mode t t)
 '(ac-auto-show-menu 1.0)
 '(ac-auto-start 3)
 '(ac-comphist-file "~/.emacs.d/data/ac-comphist.dat")
 '(ac-dwim nil)
 '(ac-ignore-case nil)
 '(ac-trigger-key "<tab>")
 '(ac-use-fuzzy nil)
 '(after-save-hook (quote (executable-make-buffer-file-executable-if-script-p)))
 '(ag-highlight-search t)
 '(alert-default-style (quote notifier))
 '(alert-notifier-command "/usr/local/bin/terminal-notifier")
 '(align-c++-modes (quote (csharp-mode c++-mode c-mode java-mode groovy-mode)))
 '(align-to-tab-stop nil)
 '(allout-command-prefix ".")
 '(auth-sources (quote ((:source "~/Documents/.authinfo.gpg" :host t :protocol t))))
 '(auto-compression-mode t nil (jka-compr))
 '(auto-save-file-name-transforms (quote (("\\`/[^/]*:.*" "/tmp" t))))
 '(auto-save-interval 64)
 '(auto-save-timeout 2)
 '(backup-directory-alist (quote (("/Volumes/Files/" . "/Volumes/Files/.backups") ("\\(recentf\\|archive/sent\\)" . "/tmp") (".*" . "~/.backups"))))
 '(backward-delete-char-untabify-method (quote untabify))
 '(battery-load-critical 7)
 '(battery-load-low 25)
 '(battery-mode-line-format "#%b %p %t")
 '(bbdb-default-country "")
 '(bbdb-file "~/Documents/.bbdb")
 '(bbdb-message-caching-enabled nil)
 '(bbdb-mua-update-interactive-p (quote (query . create)))
 '(bbdb-no-duplicates t)
 '(bbdb-offer-save (quote savenoprompt))
 '(bbdb-silent-running t)
 '(bbdb-use-pop-up nil)
 '(bbdb-vcard-import-translation-table (quote (("CELL\\|CAR" . "Mobile") ("WORK" . "Work") ("HOME" . "Home") ("^$" . "Work"))))
 '(bbdb/mail-auto-create-p nil)
 '(bind-key-segregation-regexp "\\`\\(\\(C-[chx.] \\|M-[gso] \\)\\([CM]-\\)?\\|.+-\\)")
 '(bm-highlight-style (quote bm-highlight-only-fringe))
 '(bmkp-bmenu-commands-file "~/.emacs.d/data/bmk-bmenu-commands.el")
 '(bmkp-bmenu-state-file "~/.emacs.d/data/bmk-bmenu-state.el")
 '(bmkp-last-as-first-bookmark-file nil)
 '(bookmark-default-file "~/Documents/bookmarks")
 '(browse-url-browser-function (quote choose-browser))
 '(byte-compile-verbose nil)
 '(calendar-daylight-time-zone-name "MST")
 '(calendar-latitude 40.73471)
 '(calendar-longitude -89.554659)
 '(calendar-mark-holidays-flag t)
 '(calendar-standard-time-zone-name "MST")
 '(calendar-time-zone -420)
 '(clean-buffer-list-kill-never-buffer-names (quote ("*scratch*" "*Messages*" "*server*" "*Group*" "*Org Agenda*" "todo.txt" "&bitlbee")))
 '(clean-buffer-list-kill-never-regexps (quote ("^ \\*Minibuf-.*\\*$" "^\\*Summary" "^\\*Article" "^#")))
 '(clean-buffer-list-kill-regexps (quote (".*")))
 '(column-number-mode t)
 '(compilation-context-lines 10)
 '(compilation-scroll-output t)
 '(compilation-skip-threshold 2)
 '(compile-command "tail /var/log/drupal.log")
 '(cpu-usage-format "%A %C0 %C1")
 '(current-language-environment "UTF-8")
 '(custom-buffer-done-function (quote kill-buffer))
 '(custom-file "~/.emacs.d/settings.el")
 '(custom-raised-buttons nil)
 '(custom-safe-themes (quote ("6fe6ab4abe97a4f13533e47ae59fbba7f2919583f9162b440dd06707b01f7794" "025354235e98db5e7fd9c1a74622ff53ad31b7bde537d290ff68d85665213d85" "3a727bdc09a7a141e58925258b6e873c65ccf393b2240c51553098ca93957723" "f5e56ac232ff858afb08294fc3a519652ce8a165272e3c65165c42d6fe0262a0" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "71b172ea4aad108801421cc5251edb6c792f3adbaecfa1c52e94e3d99634dee7" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(default-frame-alist (quote ((font . "-apple-Courier-medium-normal-normal-*-15-*-*-*-m-0-iso10646-1") (cursor-color . "#d33682"))))
 '(default-input-method "latin-1-prefix")
 '(default-major-mode (quote text-mode) t)
 '(deft-auto-save-interval 0.0)
 '(deft-directory "~/Documents/Notes")
 '(deft-text-mode (quote org-mode))
 '(delete-by-moving-to-trash t)
 '(delete-old-versions (quote none))
 '(diary-file "~/Documents/Tasks/diary")
 '(diff-mode-hook (quote (diff-delete-empty-files diff-make-unified smerge-mode)))
 '(directory-free-space-args "-kh")
 '(dired-clean-up-buffers-too nil)
 '(dired-dwim-target t)
 '(dired-listing-switches "-lah")
 '(dired-no-confirm (quote (byte-compile chgrp chmod chown copy hardlink symlink touch)))
 '(dired-omit-files "^\\.?#\\|^\\.\\(DS_Store\\|localized\\|AppleDouble\\)$\\|^\\.\\.$")
 '(dired-omit-mode nil t)
 '(dired-recursive-copies (quote always))
 '(dired-recursive-deletes (quote always))
 '(diredful-init-file "~/.emacs.d/data/diredful-conf.el")
 '(display-battery-mode t)
 '(display-time-mail-function (quote (lambda nil (file-exists-p "/tmp/unread"))))
 '(display-time-use-mail-icon t)
 '(doc-view-resolution 300)
 '(drupal-convert-line-ending nil)
 '(drupal-drush-program "~/src/drush/drush")
 '(drupal-php-modes (quote (php-mode web-mode)))
 '(ediff-combination-pattern (quote ("<<<<<<< A: HEAD" A "||||||| Ancestor" Ancestor "=======" B ">>>>>>> B: Incoming")))
 '(ediff-diff-options "-w")
 '(ediff-highlight-all-diffs nil)
 '(ediff-show-clashes-only t)
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(edit-server-new-frame nil)
 '(el-get-auto-update-cached-recipes nil)
 '(el-get-dir "~/.emacs.d/site-lisp/")
 '(el-get-generate-autoloads nil)
 '(elfeed-db-directory "~/Messages/.elfeed")
 '(elfeed-feeds (quote ("https://rss.myinterfase.com/rss/cuboulder_rssfeedcareerevents.xml")))
 '(emacs-lisp-mode-hook (quote (turn-on-auto-fill (lambda nil (ignore-errors (diminish (quote auto-fill-function)))) eldoc-mode (lambda nil (local-set-key [(meta 46)] (quote find-function)) (local-set-key [(control 109)] (quote newline-and-indent))) outline-minor-mode)))
 '(emms-cache-file "~/Documents/emms/cache")
 '(emms-directory "~/Documents/emms")
 '(emms-player-mplayer-parameters (quote ("-slave" "-quiet" "-really-quiet" "-volume" "15")))
 '(emms-score-file "~/Documents/emms/scores")
 '(emms-source-file-default-directory "~/Music/iTunes/iTunes Media")
 '(emms-stream-bookmarks-file "~/Documents/emms/streams")
 '(enable-recursive-minibuffers t)
 '(erc-autoawayo-message "I'm at the cappuccino bar(after %i seconds of idle-time)")
 '(erc-autojoin-channels-alist (quote (("freenode.net" "#drupal-colorado" "#emacs" "#thoughtbot") ("localhost"))))
 '(erc-autojoin-timing (quote ident))
 '(erc-fill-function (quote erc-fill-variable))
 '(erc-fill-static-center 12)
 '(erc-generate-log-file-name-function (quote erc-generate-log-file-name-short))
 '(erc-header-line-format "%t: %o")
 '(erc-hide-list (quote ("JOIN" "NICK" "PART" "QUIT" "MODE")))
 '(erc-keywords (quote (" cu " " tor " "Damon" "dkh" "dhaley" "eshell" "matt$" "gmane" "gwene" "locate-dominating-file" "usenet" "proxy" "privacy" " socks " "gnus" "workgroups" "wireshark" "hushmail" "page manager" "super cookie" "[^.]php" "nxhtml" "nxml" "erc-keywords" "libnotify" "[^#]orgW?mode" "rate module" "[^#]conkeror" "noscript" "https-everywhere" "references module" "org2blog" "Johan Falk" "Lanier" "solarized" "bbdb" "magit" "w3m" "erc-hl" "erc-highlight" "web-mode" "Rasa" "laima" "prelude" "flycheck" "eproject" "locate-dominating-file" "projectile" "dirlocal")))
 '(erc-log-channels-directory "~/Messages/ERC")
 '(erc-log-write-after-send t)
 '(erc-modules (quote (autojoin button completion dcc fill identd image irccontrols list match menu move-to-prompt netsplit networks noncommands readonly replace ring scrolltobottom services smiley stamp spelling track truncate highlight-nicknames)))
 '(erc-nick "dkh")
 '(erc-pals (quote ("laimagaigalas" "matu4921" "^johnw!" "sachac" "matt" "kevin" "jo" "cathy" "wiegley" "qdot" "crafts" "miles" "Karin" "BerntH" "technomancy" "ultimateboy" "a00001" "bozhidar" "magnars" "erin" "sellout")))
 '(erc-port 6667)
 '(erc-priority-people-regexp "\\`[^#].+")
 '(erc-prompt-for-nickserv-password nil)
 '(erc-replace-alist (quote (("</?FONT>" . ""))))
 '(erc-server "asimov.freenode.net")
 '(erc-server-reconnect-timeout 60)
 '(erc-server-send-ping-interval 45)
 '(erc-server-send-ping-timeout 180)
 '(erc-services-mode t)
 '(erc-text-matched-hook (quote (erc-hide-fools my-erc-hook)))
 '(erc-track-enable-keybindings t)
 '(erc-track-exclude (quote ("#emacs" "#git")))
 '(erc-track-exclude-types (quote ("NICK" "JOIN" "PART" "QUIT" "MODE" "301" "305" "306" "332" "333" "353" "324" "329")))
 '(erc-track-faces-priority-list (quote (erc-error-face (erc-nick-default-face erc-current-nick-face) erc-current-nick-face erc-keyword-face (erc-nick-default-face erc-pal-face) erc-pal-face erc-nick-msg-face erc-direct-msg-face)))
 '(erc-track-score-mode t)
 '(erc-track-showcount t)
 '(erc-user-full-name (quote user-full-name))
 '(eshell-aliases-file "~/Messages/eshell/alias")
 '(eshell-directory-name "~/Messages/eshell/")
 '(eshell-history-size 1000)
 '(eshell-ls-dired-initial-args (quote ("-h")))
 '(eshell-ls-exclude-regexp "~\\'")
 '(eshell-ls-initial-args "-h")
 '(eshell-modules-list (quote (eshell-alias eshell-basic eshell-cmpl eshell-dirs eshell-glob eshell-hist eshell-ls eshell-pred eshell-prompt eshell-rebind eshell-script eshell-smart eshell-term eshell-unix eshell-xtra)))
 '(eshell-prefer-to-shell t nil (eshell))
 '(eshell-prompt-function (lambda nil (concat (abbreviate-file-name (eshell/pwd)) (if (= (user-uid) 0) " # " " $ "))))
 '(eshell-save-history-on-exit t)
 '(eshell-stringify-t nil)
 '(eshell-term-name "ansi")
 '(eshell-visual-commands (quote ("vi" "top" "screen" "less" "lynx" "rlogin" "telnet")))
 '(eudc-inline-expansion-format (quote ("%s <%s>" name email)))
 '(eudc-options-file "~/.emacs.d/.eudc-options")
 '(eval-expr-print-function (quote pp))
 '(fci-rule-color "#383838")
 '(fill-column 78)
 '(find-ls-subdir-switches "-alh")
 '(flycheck-sass-compass t)
 '(flycheck-scss-compass t)
 '(flyspell-abbrev-p nil)
 '(flyspell-incorrect-hook (quote (flyspell-maybe-correct-transposition)))
 '(flyspell-use-meta-tab nil)
 '(font-lock-support-mode (quote jit-lock-mode))
 '(font-lock-verbose nil)
 '(frame-title-format (quote (:eval (concat (if buffer-file-name default-directory "%b") "    " (number-to-string (cdr (assq (quote width) (frame-parameters)))) "x" (number-to-string (cdr (assq (quote height) (frame-parameters))))))) t)
 '(gc-cons-threshold 3500000)
 '(gdb-find-source-frame t)
 '(gdb-same-frame nil)
 '(git-commit-mode-hook (quote (turn-on-auto-fill flyspell-mode git-commit-save-message)))
 '(global-auto-complete-mode t)
 '(global-auto-revert-mode t)
 '(global-font-lock-mode t nil (font-lock))
 '(gnutls-min-prime-bits 1024)
 '(google-translate-default-target-language "en")
 '(google-translate-enable-ido-completion t)
 '(grep-find-command (quote ("ag --noheading --column --ignore branches " . 43)))
 '(helm-buffers-fuzzy-matching t)
 '(hippie-expand-try-functions-list (quote (yas/hippie-try-expand try-complete-file-name-partially try-complete-file-name try-expand-all-abbrevs try-expand-list try-expand-line try-expand-dabbrev try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill try-complete-lisp-symbol-partially try-complete-lisp-symbol)))
 '(history-delete-duplicates t)
 '(history-length 200)
 '(hl-sexp-background-color "#586e75")
 '(ibuffer-default-display-maybe-show-predicates t)
 '(ibuffer-expert t)
 '(ibuffer-formats (quote ((mark modified read-only " " (name 16 -1) " " (size 6 -1 :right) " " (mode 16 16) " " filename) (mark " " (name 16 -1) " " filename))))
 '(ibuffer-maybe-show-predicate (list "^\\*Completions\\*$" "^\\*nnimap" "^\\*gnus trace" "^\\*imap log" "^\\*elim" "^\\*Completions\\*$" "^\\*BBDB\\*$" "^\\.bbdb$" "^\\.newsrc-dribble$" "^\\*fsm-debug" "\\.org_archive$" "^\\*jekyll-aa\\*$" "\\.diary$" "\\*vc\\*" "^\\*w3m cache\\*$" "^\\*w3m-cookie-parse-temp\\*$" "^\\*w3m[-]work\\*$" "^\\*w3m[-]work\\*"))
 '(ibuffer-maybe-show-regexps nil)
 '(ibuffer-saved-filter-groups (quote (("default" ("emacs Lisp" (mode . emacs-lisp-mode)) ("python" (mode . python-mode)) ("ruby" (mode . ruby-mode)) ("coffee-script" (mode . coffee-mode)) ("java-script" (or (mode . js-mode) (mode . js2-mode))) ("action-script" (mode . actionscript-mode)) ("java" (mode . java-mode)) ("html" (or (mode . html-mode) (mode . web-mode) (mode . haml-mode))) ("xml" (mode . nxml-mode)) ("css preprocessor" (or (mode . scss-mode) (mode . sass-mode) (mode . stylus-mode))) ("css" (mode . css-mode)) ("org agenda" (mode . org-agenda-mode)) ("org" (or (mode . org-mode) (name . "^\\*Calendar\\*$") (name . "^diary$"))) ("text misc" (or (mode . text-mode) (mode . rst-mode) (mode . markdown-mode))) ("w3m" (mode . w3m-mode)) ("git" (or (mode . magit-log-edit-mode) (mode . magit-log))) ("dired" (mode . dired-mode)) ("help" (or (mode . Info-mode) (mode . help-mode) (mode . Man-mode) (mode . apropos-mode) (mode . woman-mode))) ("*kite*" (name . "^\\*kite.*\\*")) ("*helm*" (name . "^\\*helm.*\\*")) ("*buffer*" (name . "\\*.*\\*")) ("ssh" (or (name . "\\*tramp") (name . "^\\*debug tramp"))) ("emacs" (or (mode . occur-mode) (mode . bookmark-bmenu-mode) (mode . help-mode) (name . "^\\*scratch\\*$") (name . "^\\*Messages\\*$") (name . "^\\*Compile-Log\\*$") (name . "^\\*Backtrace\\*$") (name . "^\\*info\\*$") (name . "^\\*Occur\\*$") (name . "^\\*grep\\*$") (name . "^\\*Process List\\*$") (name . "^\\*gud\\*$") (name . "^\\*compilation\\*$") (name . "^\\*Kill Ring\\*$"))) ("latex" (or (mode . latex-mode) (mode . LaTeX-mode) (mode . bibtex-mode) (mode . reftex-mode))) ("irc" (or (mode . garak-mode) (name . "^\\*Garak\\*$") (mode . erc-mode) (mode . twittering-mode) (name . "^\\*scratch\\* (irc)$"))) ("gnus" (or (mode . message-mode) (mode . bbdb-mode) (mode . mail-mode) (mode . gnus-group-mode) (mode . gnus-summary-mode) (mode . gnus-article-mode) (name . "^\\.bbdb$") (name . "^\\*fetchmail\\*$") (name . "^\\.newsrc-dribble") (name . "^\\*gnus trace\\*$") (name . "^\\*scratch\\* (gnus)$"))) ("Magit" (name . "*magit"))))))
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
 '(ido-decorations (quote ("{" "}" "," ",..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))
 '(ido-default-buffer-method (quote selected-window))
 '(ido-default-file-method (quote selected-window))
 '(ido-enable-flex-matching t)
 '(ido-enable-last-directory-history nil)
 '(ido-enable-tramp-completion nil)
 '(ido-enter-matching-directory (quote first))
 '(ido-everywhere t)
 '(ido-file-extensions-order (quote (".py" ".rb" ".java" ".coffee" ".js" ".c" ".cpp" ".el" ".html" ".htm" ".xhtml" ".styl" ".scss" ".sass" ".css" ".org" ".md" ".markdowm" ".doc" ".txt" ".rst" ".yml" ".yaml" ".xml" ".ini" ".cfg" ".cnf")))
 '(ido-ignore-buffers (quote ("\\` " "*Ido Completions*" "*Completions*" "*helm*" "type-break")))
 '(ido-ignore-files (quote ("\\`CVS/" "\\`#" "\\`.#" "\\`\\.\\./" "\\`\\./" "\\`\\.DS_Store" "\\`\\.localized" "\\.sparsebundle/" "\\.dmg\\'")))
 '(ido-max-directory-size 100000)
 '(ido-save-directory-list-file "~/.emacs.d/data/ido.last")
 '(ido-use-virtual-buffers t)
 '(ido-use-virtual-buffers-automatically t)
 '(image-dired-dir "~/.emacs.d/data/image-dired/")
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(initial-major-mode (quote org-mode))
 '(initsplit-customizations-alist (quote (("\\`\\(gnus\\|nn\\|message\\|mail\\|mm-\\|smtp\\|send-mail\\|check-mail\\|spam\\|sc-\\)" "~/.emacs.d/gnus-settings.el" nil nil) ("\\`\\(org-\\)" "~/.emacs.d/org-settings.el" nil nil))))
 '(ipa-file "~/Documents/ipa")
 '(ipa-overlay-position "above")
 '(irfc-directory "~/Archives/Admin/RFC/")
 '(ispell-extra-args (quote ("--sug-mode=fast")))
 '(ispell-personal-dictionary "~/Documents/.aspell.en.pws")
 '(ispell-program-name "/usr/local/bin/aspell")
 '(jiralib-url "https://cuboulder.atlassian.net")
 '(kill-do-not-save-duplicates t)
 '(kill-whole-line t)
 '(large-file-warning-threshold nil)
 '(ldap-host-parameters-alist (quote (("directory.colorado.edu" base "dc=colorado,dc=edu"))))
 '(ldap-ldapsearch-args (quote ("-LLL" "-x")))
 '(ldap-ldapsearch-prog "/usr/bin/ldapsearch")
 '(ledger-file "~/Documents/Accounts/ledger.dat")
 '(ledger-post-use-ido t)
 '(line-number-mode t)
 '(list-colors-sort (quote hsv))
 '(locate-command "mdfind")
 '(locate-make-command-line (quote mlm/locate-make-command-line))
 '(mac-pass-command-to-system nil)
 '(mac-pass-control-to-system nil)
 '(mac-wheel-button-is-mouse-2 nil)
 '(magit-auto-revert-mode nil)
 '(magit-completing-read-function (quote magit-ido-completing-read))
 '(magit-process-popup-time 15)
 '(magit-repo-dirs (quote ("~/.emacs.d/" "~/Documents/Tasks")))
 '(mail-sources (quote ((file :path "/var/mail/dadu"))))
 '(make-backup-file-name-function (quote my-make-backup-file-name))
 '(memory-usage-format "%R %F %S")
 '(moccur-following-mode-toggle nil)
 '(modelinepos-column-limit 80)
 '(mouse-autoselect-window -0.1)
 '(mouse-wheel-mode -1)
 '(mouse-wheel-scroll-amount (quote (1 ((shift) . 5) ((control)))))
 '(mudel-mode-hook (quote (mudel-add-scroll-to-bottom)))
 '(mudel-output-filter-functions (quote (ansi-color-process-output)))
 '(multi-term-program "/usr/bin/screen")
 '(multi-term-program-switches "-DR")
 '(multi-term-scroll-show-maximum-output t)
 '(network-speed-format-string "%NI#%RB#%TB#%RX#%TX#%AX")
 '(network-speed-precision 1)
 '(network-speed-update-interval 5)
 '(next-line-add-newlines nil)
 '(ns-alternate-modifier (quote alt))
 '(ns-command-modifier (quote meta))
 '(nxml-sexp-element-flag t)
 '(nxml-slash-auto-complete-flag t)
 '(offlineimap-command "offlineimap -u machineui")
 '(pabbrev-idle-timer-verbose nil)
 '(package-archives (quote (("gnu" . "http://elpa.gnu.org/packages/") ("ELPA" . "http://tromey.com/elpa/") ("Marmalade" . "http://marmalade-repo.org/packages/"))))
 '(page-break-lines-modes (quote (emacs-lisp-mode compilation-mode outline-mode prog-mode)))
 '(parens-require-spaces t)
 '(pcomplete-compare-entries-function (quote file-newer-than-file-p))
 '(persistent-scratch-file-name "~/.emacs.d/data/persistent-scratch")
 '(php-boris-command "~/src/boris/bin/boris")
 '(php-completion-file "~/Messages/php/php-completion-file")
 '(php-extras-eldoc-functions-file "~/Messages/php/php-extras-eldoc-functions")
 '(php-manual-path "~/Documents/php/php-chunked-xhtml")
 '(ping-program-options (quote ("-c" "4")))
 '(popwin:reuse-window nil)
 '(pp^L-^L-string "                                                                              ")
 '(projectile-completion-system 'grizzl)
 '(projectile-drupal-base-url-function (quote dkh-get-base-url))
 '(projectile-drupal-site-base-url-dev "http://www-dev.colorado.edu")
 '(projectile-drupal-site-base-url-prod "http://www.colorado.edu")
 '(projectile-drupal-site-base-url-stage "http://www-stage.colorado.edu")
 '(projectile-drupal-site-base-url-test "http://www-test.colorado.edu")
 '(projectile-drupal-site-name-function (quote dkh-get-site-name))
 '(projectile-enable-caching t)
 '(projectile-generic-command "find -L . -type f -print0")
 '(projectile-project-root-files-bottom-up (quote ("includes/common.inc" "includes/bootstrap.inc" ".projectile" ".git" ".hg" ".fslckout" ".bzr" "_darcs")))
 '(projectile-switch-project-action (quote dkh-projectile-dired))
 '(projectile-switch-project-hook (quote dkh-project-record))
 '(ps-font-size (quote (8 . 10)))
 '(ps-footer-font-size (quote (12 . 14)))
 '(ps-header-font-size (quote (12 . 14)))
 '(ps-header-title-font-size (quote (14 . 16)))
 '(ps-line-number-font-size 10)
 '(ps-print-color-p nil)
 '(rdebug-many-windows nil)
 '(read-buffer-function (quote ido-read-buffer))
 '(read-mail-command (quote gnus))
 '(recentf-auto-cleanup (quote never))
 '(recentf-exclude (quote ("~\\'" "\\`out\\'" "\\.log\\'" "^/[^/]*:" "\\.el\\.gz\\'")))
 '(recentf-max-saved-items 2000)
 '(recentf-save-file "~/.emacs.d/data/recentf")
 '(redisplay-dont-pause t t)
 '(regex-tool-backend (quote perl))
 '(require-final-newline t)
 '(runner-init-file "~/.emacs.d/runner-conf.el")
 '(same-window-buffer-names (quote ("*eshell*" "*shell*" "*mail*" "*inferior-lisp*" "*ielm*" "*scheme*")))
 '(same-window-regexps (quote ("\\*vc\\-.+\\*" "\\*magit.+\\*" "*grep*" "\\*compilation\\*\\(\\|<[0-9]+>\\)" "\\*Help\\*\\(\\|<[0-9]+>\\)" "\\*Shell Command Output\\*\\(\\|<[0-9]+>\\)" "\\*dictem.*")))
 '(save-abbrevs (quote silently))
 '(save-interprogram-paste-before-kill t)
 '(save-kill-file-name "~/.emacs.d/data/kill-ring-saved.el")
 '(scroll-bar-mode nil)
 '(scss-compile-at-save t)
 '(scss-output-directory "../css")
 '(scss-sass-options (quote ("--cache-location" "'/tmp/.sass-cache'" "--unix-newlines" "--compass" "--load-path '/Library/Ruby/Gems/2.0.0/gems/bourbon-4.0.2/app/assets/stylesheets'")))
 '(semanticdb-default-save-directory "~/.emacs.d/data/semanticdb")
 '(sentence-end-double-space nil)
 '(session-globals-exclude (quote (load-history flyspell-auto-correct-ring)))
 '(session-globals-include (quote ((kill-ring 10 nil) (session-file-alist 200 t) (file-name-history 200 nil) search-ring regexp-search-ring sr-history-registry)))
 '(session-initialize (quote (session places keys)))
 '(session-name-disable-regexp "\\(\\`/tmp\\|COMMIT_EDITMSG\\)")
 '(session-registers (quote (t (0 . 127))))
 '(session-save-file "~/.emacs.d/data/session")
 '(show-paren-delay 0)
 '(slime-kill-without-query-p t)
 '(slime-repl-history-file "~/.emacs.d/data/slime-history.eld")
 '(slime-startup-animation nil)
 '(sml/theme (quote respectful))
 '(sp-base-key-bindings (quote sp))
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
 '(sublimity-attractive-centering-width nil)
 '(svn-status-hide-unmodified t)
 '(tags-apropos-verbose t)
 '(tags-case-fold-search nil)
 '(tail-max-size 25)
 '(tail-volatile nil)
 '(temp-buffer-resize-mode t nil (help))
 '(term-bind-key-alist (quote (("C-c C-c" . term-interrupt-subjob) ("C-b" . my-term-send-raw-at-prompt) ("C-f" . my-term-send-raw-at-prompt) ("C-a" . my-term-send-raw-at-prompt) ("C-e" . my-term-send-raw-at-prompt) ("C-p" . previous-line) ("C-n" . next-line) ("C-s" . isearch-forward) ("C-r" . isearch-backward) ("C-m" . term-send-raw) ("M-f" . term-send-forward-word) ("M-b" . term-send-backward-word) ("M->" . my-term-end-of-buffer) ("M-o" . term-send-backspace) ("M-p" . term-send-up) ("M-n" . term-send-down) ("M-d" . term-send-forward-kill-word) ("M-DEL" . term-send-backward-kill-word) ("M-r" . term-send-reverse-search-history) ("M-," . term-send-input) ("M-." . comint-dynamic-complete) ("C-y" . term-paste))))
 '(term-buffer-maximum-size 0)
 '(term-scroll-show-maximum-output t)
 '(term-scroll-to-bottom-on-output t)
 '(text-mode-hook (quote (turn-on-auto-fill (lambda nil (ignore-errors (diminish (quote auto-fill-function)))))))
 '(tls-program (quote ("openssl s_client -connect %h:%p -no_ssl2 -ign_eof")))
 '(tool-bar-mode nil)
 '(tramp-auto-save-directory "~/.backups")
 '(tramp-default-method "rsync")
 '(tramp-default-method-alist (quote (("\\`\\(127\\.0\\.0\\.1\\|::1\\|localhost6?\\)\\'" "\\`root\\'" "sudo"))))
 '(tramp-persistency-file-name "~/.emacs.d/data/tramp")
 '(trash-directory "~/.Trash")
 '(undo-limit 800000)
 '(uniquify-buffer-name-style (quote post-forward-angle-brackets) nil (uniquify))
 '(url-cache-directory "~/.emacs.d/data/url/cache")
 '(url-configuration-directory "~/.emacs.d/data/url/")
 '(url-irc-function (quote url-irc-erc))
 '(user-full-name "Damon K. Haley")
 '(user-initials "dkh")
 '(user-mail-address "dkh@member.fsf.org")
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map (quote ((20 . "#BC8383") (40 . "#CC9393") (60 . "#DFAF8F") (80 . "#D0BF8F") (100 . "#E0CF9F") (120 . "#F0DFAF") (140 . "#5F7F5F") (160 . "#7F9F7F") (180 . "#8FB28F") (200 . "#9FC59F") (220 . "#AFD8AF") (240 . "#BFEBBF") (260 . "#93E0E3") (280 . "#6CA0A3") (300 . "#7CB8BB") (320 . "#8CD0D3") (340 . "#94BFF3") (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3")
 '(vc-command-messages t)
 '(vc-follow-symlinks t)
 '(vc-git-diff-switches (quote ("-w" "-U3")))
 '(vc-handled-backends (quote (GIT SVN CVS Bzr Hg)))
 '(vc-make-backup-files t)
 '(version-control t)
 '(visible-bell t)
 '(w3m-cookie-accept-bad-cookies nil)
 '(w3m-default-display-inline-images t)
 '(w3m-default-save-directory "~/Downloads")
 '(w3m-fill-column 80)
 '(w3m-session-file "~/Documents/w3m-session")
 '(w3m-use-cookies t)
 '(wdired-use-dired-vertical-movement (quote sometimes))
 '(wg-mode-line-on t)
 '(wg-morph-on nil)
 '(wg-prefix-key "")
 '(wg-query-for-save-on-emacs-exit nil)
 '(wg-query-for-save-on-workgroups-mode-exit nil)
 '(whitespace-auto-cleanup t)
 '(whitespace-line-column 90)
 '(whitespace-rescan-timer-time nil)
 '(whitespace-silent t)
 '(whitespace-style (quote (face trailing lines space-before-tab empty)))
 '(workgroups-mode t)
 '(x-select-enable-clipboard t)
 '(x-stretch-cursor t)
 '(yaoddmuse-browse-function (quote w3m-browse-url))
 '(yaoddmuse-directory "~/.emacs.d/doc")
 '(yas-prompt-functions (quote (yas-ido-prompt yas-completing-prompt yas-no-prompt)))
 '(yas-snippet-dirs (quote ("~/.emacs.d/snippets" "~/.emacs.d/site-lisp/emacs-drupal-snippets/snippets" "~/.emacs.d/site-lisp/css-scss")) nil (yasnippet))
 '(yas-triggers-in-field t)
 '(yas-wrap-around-region t)
 '(yas/triggers-in-field t)
 '(yas/wrap-around-region t)
 '(zencoding-preview-default nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-dim-other-buffers-face ((t (:inherit fringe))))
 '(diary ((t (:inherit eshell-ls-backup))))
 '(diff-added ((((background dark)) (:foreground "#FFFF9B9BFFFF")) (t (:foreground "DarkGreen"))))
 '(diff-changed ((((background dark)) (:foreground "Yellow")) (t (:foreground "MediumBlue"))))
 '(diff-context ((((background dark)) (:foreground "White")) (t (:foreground "Black"))))
 '(diff-file-header ((((background dark)) (:foreground "Cyan" :background "Black")) (t (:foreground "Red" :background "White"))))
 '(diff-header ((((background dark)) (:foreground "Cyan")) (t (:foreground "Red"))))
 '(diff-index ((((background dark)) (:foreground "Magenta")) (t (:foreground "Green"))))
 '(diff-nonexistent ((((background dark)) (:foreground "#FFFFFFFF7474")) (t (:foreground "DarkBlue"))))
 '(dired-subtree-depth-1-face ((t (:inherit org-todo-kwd-face))))
 '(dired-subtree-depth-2-face ((t (:inherit org-done-kwd-face))))
 '(dired-subtree-depth-3-face ((t (:inherit org-project-kwd-face))))
 '(dired-subtree-depth-4-face ((t (:inherit org-waiting-kwd-face))))
 '(dired-subtree-depth-5-face ((t (:inherit org-someday-kwd-face))))
 '(dired-subtree-depth-6-face ((t (:inherit org-started-kwd-face))))
 '(erc-timestamp-face ((t (:foreground "dark violet"))))
 '(holiday ((t (:inherit fringe))))
 '(wg-brace-face ((t (:inherit comint-highlight-prompt))))
 '(wg-command-face ((t (:inherit custom-state))))
 '(wg-current-workgroup-face ((t (:inherit eshell-ls-special))))
 '(wg-filename-face ((t (:inherit eshell-ls-archive))))
 '(wg-frame-face ((t (:inherit eshell-ls-backup))))
 '(wg-message-face ((t (:inherit eshell-ls-clutter))))
 '(wg-mode-line-face ((t (:inherit eshell-ls-readonly))))
 '(wg-other-workgroup-face ((t (:inherit eshell-ls-readonly))))
 '(wg-previous-workgroup-face ((t (:inherit eshell-ls-symlink))))
 '(which-func ((t (:inherit comint-highlight-prompt)))))
