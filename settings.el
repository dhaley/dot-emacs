(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Info-fit-frame-flag nil)
 '(TeX-PDF-mode t)
 '(TeX-engine (quote xetex))
 '(TeX-expand-list (quote (("%p" TeX-printer-query) ("%q" (lambda nil (TeX-printer-query t))) ("%V" (lambda nil (TeX-source-correlate-start-server-maybe) (TeX-view-command-raw))) ("%vv" (lambda nil (TeX-source-correlate-start-server-maybe) (TeX-output-style-check TeX-output-view-style))) ("%v" (lambda nil (TeX-source-correlate-start-server-maybe) (TeX-style-check TeX-view-style))) ("%r" (lambda nil (TeX-style-check TeX-print-style))) ("%l" (lambda nil (TeX-style-check LaTeX-command-style))) ("%(PDF)" (lambda nil (if (and (eq TeX-engine (quote default)) (or TeX-PDF-mode TeX-DVI-via-PDFTeX)) "pdf" ""))) ("%(PDFout)" (lambda nil (cond ((and (eq TeX-engine (quote xetex)) (not TeX-PDF-mode)) " -no-pdf") ((and (eq TeX-engine (quote luatex)) (not TeX-PDF-mode)) " --output-format=dvi") ((and (eq TeX-engine (quote default)) (not TeX-PDF-mode) TeX-DVI-via-PDFTeX) " \"\\pdfoutput=0 \"") (t "")))) ("%(mode)" (lambda nil (if TeX-interactive-mode "" " -interaction=nonstopmode"))) ("%(o?)" (lambda nil (if (eq TeX-engine (quote omega)) "o" ""))) ("%(tex)" (lambda nil (eval (nth 2 (assq TeX-engine (TeX-engine-alist)))))) ("%(latex)" (lambda nil (eval (nth 3 (assq TeX-engine (TeX-engine-alist)))))) ("%(execopts)" ConTeXt-expand-options) ("%S" TeX-source-correlate-expand-options) ("%dS" TeX-source-specials-view-expand-options) ("%cS" TeX-source-specials-view-expand-client) ("%(outpage)" (lambda nil (if TeX-source-correlate-output-page-function (funcall TeX-source-correlate-output-page-function) "1"))) ("%s" file nil t) ("%t" file t t) ("%`" (lambda nil (setq TeX-command-pos t TeX-command-text ""))) (" \"\\" (lambda nil (if (eq TeX-command-pos t) (setq TeX-command-pos pos pos (+ 3 pos)) (setq pos (1+ pos))))) ("\"" (lambda nil (if (numberp TeX-command-pos) (setq TeX-command-text (concat TeX-command-text (substring command TeX-command-pos (1+ pos))) command (concat (substring command 0 TeX-command-pos) (substring command (1+ pos))) pos TeX-command-pos TeX-command-pos t) (setq pos (1+ pos))))) ("%'" (lambda nil (prog1 (if (stringp TeX-command-text) (progn (setq pos (+ (length TeX-command-text) 9) TeX-command-pos (and (string-match " " (funcall file t t)) "\"")) (concat TeX-command-text " \"\\input\"")) (setq TeX-command-pos nil) "") (setq TeX-command-text nil)))) ("%n" TeX-current-line) ("%d" file "dvi" t) ("%f" file "ps" t) ("%o" (lambda nil (funcall file (TeX-output-extension) t))) ("%b" TeX-current-file-name-master-relative) ("%m" preview-create-subdirectory) ("%O" (lambda nil (expand-file-name (funcall file (TeX-output-extension) t)))))))
 '(TeX-view-program-list (quote (("Skim" ("osascript" " ~/bin/skim-gotopage.script" " %O" (mode-io-correlate " %(outpage)"))))))
 '(TeX-view-program-selection (quote (((output-dvi style-pstricks) "dvips and gv") (output-dvi "xdvi") (output-pdf "Skim") (output-html "xdg-open"))))
 '(abbrev-file-name "~/git/.emacs.d/.abbrev_defs")
 '(abbrev-mode t t)
 '(ac-auto-show-menu 1.0)
 '(ac-auto-start 3)
 '(ac-comphist-file "/Users/daha1836/.emacs.d/data/ac-comphist.dat")
 '(ac-dwim nil)
 '(ac-ignore-case nil)
 '(ac-use-fuzzy nil)
 '(after-save-hook (quote (executable-make-buffer-file-executable-if-script-p)))
 '(align-c++-modes (quote (csharp-mode c++-mode c-mode java-mode groovy-mode)))
 '(align-to-tab-stop nil)
 '(allout-command-prefix ".")
 '(ansi-color-names-vector ["black" "red" "green" "brown" "blue" "magenta" "blue" "white"])
 '(auth-sources (quote ((:source "~/git/.emacs.d/.authinfo.gpg" :host t :protocol t))))
 '(auto-compression-mode t nil (jka-compr))
 '(auto-save-interval 1024)
 '(backup-directory-alist (quote (("/Volumes/Files/" . "/Volumes/Files/.backups") ("\\(recentf\\|archive/sent\\)" . "/tmp") (".*" . "~/.emacs.d/backups"))))
 '(backward-delete-char-untabify-method (quote untabify))
 '(bbdb-complete-mail-allow-cycling t)
 '(bbdb-default-country "")
 '(bbdb-file "~/git/.emacs.d/.bbdb")
 '(bbdb-mail-user-agent (quote gnus))
 '(bbdb-message-caching-enabled nil)
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
 '(bookmark-file "~/.emacs.d/data/breadcrumb" t)
 '(browse-url-default-browser "conkeror")
 '(browse-url-generic-program "conkeror")
 '(byte-compile-verbose nil)
 '(c-default-style (quote ((java-mode . "gnu") (awk-mode . "awk") (other . "gnu"))))
 '(calendar-daylight-time-zone-name "CDT")
 '(calendar-latitude 40.73471)
 '(calendar-longitude -89.554659)
 '(calendar-mark-holidays-flag t)
 '(calendar-standard-time-zone-name "CST")
 '(calendar-time-zone -420)
 '(canlock-password "8d2ee9a7e4658c4ff6d863f91a3dd5340b3918ec")
 '(check-mail-summary-function (quote check-mail-box-summary))
 '(clean-buffer-list-kill-never-buffer-names (quote ("*scratch*" "*Messages*" "*server*" "*Group*" "*Org Agenda*" "todo.txt" "&bitlbee")))
 '(clean-buffer-list-kill-never-regexps (quote ("^ \\*Minibuf-.*\\*$" "^\\*Summary" "^\\*Article" "^#")))
 '(clean-buffer-list-kill-regexps (quote (".*")))
 '(column-number-mode t)
 '(company-backends (quote (company-elisp company-nxml company-css (company-gtags company-dabbrev-code company-keywords) company-oddmuse company-files company-dabbrev)))
 '(company-idle-delay nil)
 '(company-require-match nil)
 '(compilation-context-lines 10)
 '(compilation-scroll-output t)
 '(compilation-skip-threshold 2)
 '(coq-unicode-tokens-enable t)
 '(current-language-environment "UTF-8")
 '(custom-buffer-done-function (quote kill-buffer))
 '(custom-file "/Users/daha1836/.emacs.d/settings.el")
 '(custom-raised-buttons nil)
 '(custom-safe-themes (quote ("71b172ea4aad108801421cc5251edb6c792f3adbaecfa1c52e94e3d99634dee7" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(default-frame-alist (quote ((font . "-apple-Courier-medium-normal-normal-*-15-*-*-*-m-0-iso10646-1") (cursor-color . "#b247ee"))))
 '(default-input-method "latin-1-prefix")
 '(default-major-mode (quote text-mode) t)
 '(deft-auto-save-interval 0.0)
 '(deft-directory "/Users/daha1836/Documents/Notes")
 '(deft-text-mode (quote org-mode))
 '(delete-by-moving-to-trash t)
 '(delete-old-versions (quote none))
 '(diary-file "~/Documents/Tasks/diary")
 '(diff-mode-hook (quote (diff-delete-empty-files diff-make-unified smerge-mode)))
 '(directory-free-space-args "-kh")
 '(dired-async-use-native-commands t)
 '(dired-clean-up-buffers-too nil)
 '(dired-dwim-target t)
 '(dired-listing-switches "-lah")
 '(dired-no-confirm (quote (byte-compile chgrp chmod chown copy hardlink symlink touch)))
 '(dired-omit-files "^\\.?#\\|^\\.\\(DS_Store\\|localized\\|AppleDouble\\)$\\|^\\.\\.$")
 '(dired-omit-mode nil t)
 '(dired-recursive-copies (quote always))
 '(dired-recursive-deletes (quote always))
 '(diredful-init-file "~/.emacs.d/data/diredful-conf.el")
 '(display-time-mail-function (quote (lambda nil (file-exists-p "/tmp/unread"))))
 '(display-time-use-mail-icon t)
 '(doc-view-resolution 300)
 '(ediff-combination-pattern (quote ("<<<<<<< A: HEAD" A "||||||| Ancestor" Ancestor "=======" B ">>>>>>> B: Incoming")))
 '(ediff-diff-options "-w")
 '(ediff-highlight-all-diffs nil)
 '(ediff-show-clashes-only t)
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(edit-server-new-frame nil)
 '(egocentric-additional-keywords (quote ("Damon" "vinylis" "dkh")))
 '(el-get-auto-update-cached-recipes nil)
 '(el-get-dir "~/.emacs.d/site-lisp/")
 '(el-get-generate-autoloads nil)
 '(emacs-lisp-mode-hook (quote (turn-on-auto-fill (lambda nil (ignore-errors (diminish (quote auto-fill-function)))) eldoc-mode (lambda nil (local-set-key [(meta 46)] (quote find-function)) (local-set-key [(control 109)] (quote newline-and-indent))))))
 '(emms-player-mplayer-parameters (quote ("-slave" "-quiet" "-really-quiet" "-volume" "15")))
 '(emms-source-file-default-directory "/Volumes/Data/Music/iTunes/iTunes Media/")
 '(enable-recursive-minibuffers t)
 '(erc-autoawayo-message "I'm at the cappuccino bar(after %i seconds of idle-time)")
 '(erc-autojoin-channels-alist (quote ((".*\\.freenode.net" "#drupal-colorado" "#emacs" "#fsf-members" "#erc" "#conkeror" "#org-mode" "#freedombox"))))
 '(erc-autojoin-mode t)
 '(erc-autojoin-timing :ident)
 '(erc-fools (quote ("anxt" "JordiGH" "nyc")))
 '(erc-generate-log-file-name-function (quote erc-generate-log-file-name-short))
 '(erc-header-line-format "%t: %o")
 '(erc-hide-list (quote ("JOIN" "NICK" "PART" "QUIT" "MODE")))
 '(erc-ignore-list (quote ("rudybot!.*" "anxt")))
 '(erc-join-buffer (quote bury))
 '(erc-keywords (quote (" cu " " tor " "Damon" "dkh" "dhaley" "matt" "relation module" "gmane" "gwene" "usenet" "proxy" "privacy" " socks " "gnus" "workgroups" "wireshark" "hushmail" "page manager" "super cookie" "[^.]php" "nxhtml" "nxml" "erc-keywords" "libnotify" "orgW?mode" "rate module" "conkeror" "noscript" "https-everywhere" "references
module" "org2blog" "Johan Falk" "Lanier" "solarized" "bbdb" "magit" "w3m" "erc-hl" "erc-highlight" "web-mode" "flymake" "Rasa" "dkh")))
 '(erc-log-channels-directory "~/Messages/.erc/logs")
 '(erc-log-write-after-send t)
 '(erc-modules (quote (button notifications autojoin completion dcc fill identd irccontrols list log match menu move-to-prompt netsplit networks noncommands readonly replace ring scrolltobottom services smiley stamp spelling track truncate track-score hl-nicks)))
 '(erc-nick "dkh")
 '(erc-nick-notify-urgency "normal")
 '(erc-notifications-icon "~/.emacs.d/icons/irc.png")
 '(erc-pals (quote ("laimagaigalas" "matu4921" "^johnw!" "sachac" "matt" "kevin" "jo" "cathy" "wiegley" "qdot" "crafts" "miles" "matt" "alfredo" "Karin" "BerntH")))
 '(erc-port 6667)
 '(erc-priority-people-regexp "\\`[^#].+")
 '(erc-prompt (lambda nil (if erc-network (concat "[" (symbol-name erc-network) "]") (concat "[" (car erc-default-recipients) "]"))))
 '(erc-prompt-for-nickserv-password nil)
 '(erc-replace-alist (quote (("</?FONT>" . ""))))
 '(erc-server "asimov.freenode.net")
 '(erc-services-mode t)
 '(erc-track-enable-keybindings t)
 '(erc-track-exclude (quote ("*stickychan" "*status")))
 '(erc-track-exclude-server-buffer t)
 '(erc-track-exclude-types (quote ("NICK" "JOIN" "PART" "QUIT" "MODE" "301" "305" "306" "332" "333" "353" "324" "329")))
 '(erc-track-faces-priority-list (quote (erc-error-face (erc-nick-default-face erc-current-nick-face) erc-current-nick-face erc-keyword-face (erc-nick-default-face erc-pal-face) erc-pal-face erc-nick-msg-face erc-direct-msg-face)))
 '(erc-track-priority-faces-only t)
 '(erc-track-shorten-start 1)
 '(erc-track-showcount t)
 '(erc-track-switch-direction (quote importance))
 '(erc-track-visibility (quote selected-visible))
 '(erc-user-full-name (quote user-full-name))
 '(eshell-aliases-file "~/git/.emacs.d/eshell/alias")
 '(eshell-directory-name "~/git/.emacs.d/eshell/")
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
 '(eudc-default-return-attributes nil)
 '(eudc-ldap-bbdb-conversion-alist (quote ((name . displayName) (net . mail) (address eudc-bbdbify-address Postaladdress "Office") (phone (eudc-bbdbify-phone telephoneNumber "Office")) (notes . title))))
 '(eudc-options-file "~/git/.emacs.d/.eudc-options")
 '(eudc-protocol (quote ldap))
 '(eudc-query-form-attributes (quote (name firstname email phone cuedupersonuuid Postaladdress)))
 '(eudc-server "directory.colorado.edu")
 '(eudc-strict-return-matches nil)
 '(eval-expr-print-function (quote pp))
 '(exec-path (quote ("/Users/daha1836/bin" "/usr/local/bin" "/opt/local/libexec/git-core" "/opt/local/bin" "/usr/bin" "/bin" "/usr/local/sbin" "/opt/local/sbin" "/usr/sbin" "/sbin" "/usr/X11R6/bin")))
 '(fill-column 78)
 '(find-ls-subdir-switches "-alh")
 '(flyspell-abbrev-p nil)
 '(flyspell-incorrect-hook (quote (flyspell-maybe-correct-transposition)))
 '(flyspell-use-meta-tab nil)
 '(font-lock-support-mode (quote jit-lock-mode))
 '(font-lock-verbose nil)
 '(frame-title-format (quote (:eval (concat (if buffer-file-name default-directory "%b") "    " (number-to-string (cdr (assq (quote width) (frame-parameters)))) "x" (number-to-string (cdr (assq (quote height) (frame-parameters))))))) t)
 '(gc-cons-threshold 3500000)
 '(gdb-find-source-frame t)
 '(gdb-same-frame nil)
 '(global-auto-complete-mode t)
 '(global-font-lock-mode t nil (font-lock))
 '(global-rainbow-delimiters-mode nil)
 '(gnus-activate-level 2)
 '(gnus-after-getting-new-news-hook (quote (gnus-group-list-groups gnus-group-save-newsrc gnus-display-time-event-handler)))
 '(gnus-agent-expire-all t)
 '(gnus-agent-expire-days 14)
 '(gnus-agent-go-online t)
 '(gnus-agent-mark-unread-after-downloaded nil)
 '(gnus-agent-synchronize-flags t)
 '(gnus-alias-default-identity "CU")
 '(gnus-alias-identity-alist (quote (("CU" "" "\"Damon Haley\"
<damon.haley@colorado.edu>" "University of Colorado" (("X-URL" . "http://dashboard.colorado.edu")) "" "~/git/.emacs.d/sig/cu.sig") ("VI" "" "\"Damon Haley\" <dkh@member.fsf.org>" "Vinyl Island University" nil "" "~/git/.emacs.d/sig/vi.sig") ("Haley" "" "\"Damon K. Haley\" <dhaley@warpmail.net>" "Haley's Comet" nil "" "~/git/.emacs.d/sig/haley.sig") ("LSD" "" "vinylisl@sdf.lonestar.org" "Peace Love & Understanding" (("X-URL" . "http://vinylisl.netlsd.com")) "" "~/git/.emacs.d/sig/lsd.sig"))))
 '(gnus-alias-identity-rules (quote (("Emacs Mailing Lists" ("To" "emacs" current) "VI") ("Emacs Newsgroups" ("Newsgroups" "emacs" current) "VI"))))
 '(gnus-alias-override-user-mail-address t)
 '(gnus-alias-unknown-identity-rule (quote error))
 '(gnus-always-read-dribble-file t)
 '(gnus-article-date-lapsed-new-header t)
 '(gnus-article-update-date-headers nil)
 '(gnus-asynchronous t)
 '(gnus-blocked-images nil)
 '(gnus-check-new-newsgroups nil)
 '(gnus-completing-read-function (quote gnus-ido-completing-read))
 '(gnus-default-adaptive-score-alist (quote ((gnus-dormant-mark (from 20) (subject 100)) (gnus-ticked-mark (subject 30)) (gnus-read-mark (subject 30)) (gnus-del-mark (subject -150)) (gnus-catchup-mark (subject -150)) (gnus-killed-mark (subject -1000)) (gnus-expirable-mark (from -1000) (subject -1000)))))
 '(gnus-default-article-saver (quote gnus-summary-save-in-mail))
 '(gnus-gcc-mark-as-read t)
 '(gnus-generate-tree-function (quote gnus-generate-horizontal-tree))
 '(gnus-gravatar-properties (quote (:ascent center)))
 '(gnus-gravatar-too-ugly gnus-ignored-from-addresses)
 '(gnus-group-default-list-level 2)
 '(gnus-group-line-format "%S%p%P%M%5y: %(%B%G%B%)
")
 '(gnus-group-mode-hook (quote (gnus-topic-mode gnus-agent-mode hl-line-mode)))
 '(gnus-group-use-permanent-levels t)
 '(gnus-harvest-sender-alist (quote ((".*@\\(cu\\|colorado\\|ti\\)\\.edu" . damon\.haley@colorado\.edu) (".*@fsf\\.org" . dkh@member\.fsf\.org))))
 '(gnus-home-directory "~/git/gnus/")
 '(gnus-ignored-from-addresses "\\(damon\\.haley\\|dhaley\\|dkh\\)\\(-[^@]+\\)?@\\(colorado\\.edu\\|\\(hushmail\\|ssl\\-mail\\)\\.com\\|member\\.fsf\\.org\\)")
 '(gnus-ignored-mime-types (quote ("application/x-pkcs7-signature" "application/ms-tnef" "text/x-vcard")))
 '(gnus-interactive-exit (quote quiet))
 '(gnus-large-newsgroup 4000)
 '(gnus-mailing-list-groups "\\`\\(list\\|wg21\\)\\.")
 '(gnus-mark-unpicked-articles-as-read t)
 '(gnus-message-archive-group (quote ((format-time-string "sent.%Y"))))
 '(gnus-message-replyencrypt nil)
 '(gnus-no-groups-message "No Gnus for Daemon.")
 '(gnus-novice-user nil)
 '(gnus-permanently-visible-groups "INBOX")
 '(gnus-picon-style (quote right))
 '(gnus-read-active-file nil)
 '(gnus-read-newsrc-file nil)
 '(gnus-refer-article-method (quote (current (nnir "nnimap:Local") (nntp "Gmane" (nntp-address "news.gmane.org")))))
 '(gnus-refer-thread-use-nnir t)
 '(gnus-registry-ignored-groups (quote (("nntp" t) ("^INBOX" t))))
 '(gnus-save-killed-list nil)
 '(gnus-save-newsrc-file nil)
 '(gnus-score-default-duration (quote p))
 '(gnus-score-expiry-days 30)
 '(gnus-select-group-hook (quote (gnus-group-set-timestamp)))
 '(gnus-select-method (quote (nnimap "Local" (nnimap-stream shell) (nnimap-shell-program "/opt/local/libexec/dovecot/imap"))))
 '(gnus-sieve-file "~/dovecot.sieve")
 '(gnus-sieve-select-method "nnimap:Local")
 '(gnus-signature-separator (quote ("^-- $" "^-- *$" "^_____+$")))
 '(gnus-simplify-subject-functions (quote (gnus-simplify-subject-fuzzy)))
 '(gnus-sort-gathered-threads-function (quote gnus-thread-sort-by-date) t)
 '(gnus-split-methods (quote ((gnus-save-site-lisp-file) (gnus-article-archive-name) (gnus-article-nndoc-name))))
 '(gnus-started-hook (quote ((lambda nil (run-hooks (quote gnus-after-getting-new-news-hook))))))
 '(gnus-subscribe-newsgroup-method (quote gnus-subscribe-topics))
 '(gnus-sum-thread-tree-single-indent "  ")
 '(gnus-summary-expunge-below -100)
 '(gnus-summary-line-format "%«%U%R %uS %ur %»%(%*%-14,14f   %1«%B%s%»%)
")
 '(gnus-summary-mark-below -100)
 '(gnus-summary-pick-line-format "%U%R %uS %ur %(%*%-14,14f  %B%s%)
")
 '(gnus-summary-prepared-hook (quote (gnus-summary-hide-all-threads)))
 '(gnus-summary-save-parts-default-mime ".*")
 '(gnus-suspend-gnus-hook (quote (gnus-group-save-newsrc)))
 '(gnus-thread-expunge-below -1000)
 '(gnus-thread-hide-subtree t)
 '(gnus-thread-sort-functions (quote (gnus-thread-sort-by-most-recent-number gnus-thread-sort-by-score)))
 '(gnus-topic-display-empty-topics nil)
 '(gnus-topic-line-format "%i[ %A: %(%{%n%}%) ]%v
")
 '(gnus-treat-body-boundary nil)
 '(gnus-treat-date-lapsed (quote head))
 '(gnus-treat-display-smileys t)
 '(gnus-treat-display-x-face (quote head))
 '(gnus-treat-from-gravatar (quote head))
 '(gnus-treat-from-picon (quote head))
 '(gnus-treat-hide-citation-maybe t)
 '(gnus-treat-mail-gravatar (quote head))
 '(gnus-treat-mail-picon nil)
 '(gnus-treat-newsgroups-picon nil)
 '(gnus-treat-strip-cr t)
 '(gnus-treat-strip-leading-blank-lines t)
 '(gnus-treat-strip-multiple-blank-lines t)
 '(gnus-treat-strip-trailing-blank-lines t)
 '(gnus-treat-unsplit-urls t)
 '(gnus-tree-minimize-window nil)
 '(gnus-uncacheable-groups "^nnml")
 '(gnus-use-adaptive-scoring (quote (line)))
 '(gnus-use-cache t)
 '(gnus-verbose 4)
 '(hippie-expand-try-functions-list (quote (yas/hippie-try-expand try-complete-file-name-partially try-complete-file-name try-expand-all-abbrevs try-expand-list try-expand-line try-expand-dabbrev try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill try-complete-lisp-symbol-partially try-complete-lisp-symbol)))
 '(history-delete-duplicates t)
 '(history-length 200)
 '(ibuffer-default-display-maybe-show-predicates t)
 '(ibuffer-expert t)
 '(ibuffer-formats (quote ((mark modified read-only " " (name 16 -1) " " (size 6 -1 :right) " " (mode 16 16) " " filename) (mark " " (name 16 -1) " " filename))))
 '(ibuffer-maybe-show-regexps nil)
 '(ibuffer-saved-filter-groups (quote (("default" ("Commands" (or (mode . shell-mode) (mode . eshell-mode) (mode . term-mode) (mode . compilation-mode))) ("Helm" (mode . helm-mode)) ("Magit" (or (mode . magit-status-mode) (mode . magit-log-mode))) ("C++" (or (mode . c-mode) (mode . c++-mode))) ("Lisp" (mode . emacs-lisp-mode)) ("Dired" (mode . dired-mode)) ("Gnus" (or (mode . message-mode) (mode . mail-mode) (mode . gnus-group-mode) (mode . gnus-summary-mode) (mode . gnus-article-mode) (name . "^\\.newsrc-dribble"))) ("Org" (or (name . "^\\*Calendar\\*$") (name . "^diary$") (mode . org-mode))) ("erc" (mode . erc-mode)) ("twitter" (mode . twittering-mode)) ("Emacs" (or (name . "^\\*scratch\\*$") (name . "^\\*Messages\\*$")))))))
 '(ibuffer-show-empty-filter-groups nil)
 '(ibuffer-shrink-to-minimum-size t t)
 '(ibuffer-use-other-window t)
 '(icicle-Completions-text-scale-decrease 0)
 '(icicle-apropos-cycle-next-keys (quote ([next] [(control 110)])))
 '(icicle-apropos-cycle-previous-keys (quote ([prior] [(control 112)])))
 '(icicle-incremental-completion nil)
 '(icicle-max-candidates 100)
 '(icomplete-mode 1)
 '(identica-username "vinylisl")
 '(ido-auto-merge-work-directories-length 0)
 '(ido-cannot-complete-command (quote ido-exit-minibuffer))
 '(ido-decorations (quote ("{" "}" "," ",..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))
 '(ido-enable-flex-matching t)
 '(ido-enable-last-directory-history nil)
 '(ido-enable-tramp-completion nil)
 '(ido-enter-matching-directory (quote first))
 '(ido-ignore-files (quote ("\\`CVS/" "\\`#" "\\`.#" "\\`\\.\\./" "\\`\\./" "\\`\\.DS_Store" "\\`\\.localized" "\\.sparsebundle/" "\\.dmg\\'")))
 '(ido-save-directory-list-file "~/.emacs.d/data/ido.last")
 '(ido-use-virtual-buffers t)
 '(image-dired-dir "~/.emacs.d/data/image-dired/")
 '(indent-tabs-mode nil)
 '(inhibit-startup-echo-area-message "daha1836")
 '(inhibit-startup-screen t)
 '(initial-major-mode (quote fundamental-mode))
 '(ipa-file "~/Documents/ipa")
 '(ipa-overlay-position "above")
 '(irfc-directory "~/Archives/Admin/RFC/")
 '(ispell-personal-dictionary "~/git/.emacs.d/.aspell.US.pws")
 '(kill-do-not-save-duplicates t)
 '(kill-whole-line t)
 '(large-file-warning-threshold nil)
 '(ledger-file "~/Documents/Accounts/ledger.dat")
 '(ledger-post-use-ido t)
 '(line-number-mode t)
 '(linum-format (lambda (line) (propertize (format (concat "%" (number-to-string (length (number-to-string (line-number-at-pos (point-max))))) "d ") line) (quote face) (quote linum))))
 '(mac-pass-command-to-system nil)
 '(mac-pass-control-to-system nil)
 '(mac-wheel-button-is-mouse-2 nil)
 '(magit-completing-read-function (quote magit-ido-completing-read))
 '(magit-process-popup-time 15)
 '(mail-envelope-from (quote header))
 '(mail-host-address "UCOMM-WEB-L-01")
 '(mail-setup-with-from nil)
 '(mail-source-delete-incoming t)
 '(mail-source-delete-old-incoming-confirm nil)
 '(mail-source-report-new-mail-interval 15)
 '(mail-sources (quote ((file :path "/var/mail/daha1836"))))
 '(mail-specify-envelope-from t)
 '(mail-user-agent (quote gnus-user-agent))
 '(make-backup-file-name-function (quote my-make-backup-file-name))
 '(mediawiki-debug t)
 '(message-alternative-emails "\\(dhaley\\|vinylisl\\)@\\(hushmail\\|ssl\\-mail\\).com\\|\\(dkh@member\\.fsf\\|news@vinylisland\\)\\.org\\|damon\\.haley@colorado\\.edu")
 '(message-default-headers (concat "X-Face: \"'PJ-yb+fYF0]%?,#==_(s>`~Hw_iwG![Cc+Sq$k>S|QbU)>?}Y51$4)\\9OEt:NL.@kZIqy <UnVZ*!XnGGV:iDO$YDhK7i~$.fs%r^0LJdztkb\\6=DI6by:GdO>.L<,Nd[nsMwrN3b]os1UqBw
" "X-Accept-Language: en-us
" "X-Operating-System: Debian GNU/Linux
"))
 '(message-directory "~/git/gnus/Mail/")
 '(message-fill-column 78)
 '(message-interactive t)
 '(message-mail-alias-type nil)
 '(message-mode-hook (quote (abbrev-mode footnote-mode turn-on-auto-fill turn-on-flyspell turn-on-orgstruct (lambda nil (set-fill-column 78)))))
 '(message-send-mail-function (quote message-send-mail-with-sendmail))
 '(message-send-mail-partially-limit nil)
 '(message-sendmail-envelope-from (quote header))
 '(message-sent-hook (quote (my-gnus-score-followup)))
 '(message-setup-hook (quote (gnus-alias-determine-identity gnus-harvest-set-from message-check-recipients (lambda nil (bbdb-mail-aliases t)))))
 '(message-signature-separator "^-- *$")
 '(message-subscribed-address-functions (quote (gnus-find-subscribed-addresses)))
 '(message-x-completion-alist (quote (("\\([rR]esent-\\|[rR]eply-\\)?[tT]o:\\|[bB]?[cC][cC]:" . gnus-harvest-find-address) ((if (boundp (quote message-newgroups-header-regexp)) message-newgroups-header-regexp message-newsgroups-header-regexp) . message-expand-group))))
 '(mm-attachment-override-types (quote ("text/x-vcard" "application/pkcs7-mime" "application/x-pkcs7-mime" "application/pkcs7-signature" "application/x-pkcs7-signature" "image/.*")))
 '(mm-decrypt-option (quote always))
 '(mm-discouraged-alternatives (quote ("application/msword" "text/richtext")))
 '(mm-inline-text-html-with-images t)
 '(mm-text-html-renderer (quote w3m))
 '(mm-verify-option (quote always))
 '(mm-w3m-safe-url-regexp nil)
 '(moccur-following-mode-toggle nil)
 '(modelinepos-column-limit 80)
 '(mouse-wheel-mode -1)
 '(mudel-mode-hook (quote (mudel-add-scroll-to-bottom)))
 '(mudel-output-filter-functions (quote (ansi-color-process-output)))
 '(multi-term-program "/opt/local/bin/screen")
 '(multi-term-program-switches "-DR")
 '(multi-term-scroll-show-maximum-output t)
 '(next-line-add-newlines nil)
 '(nnir-imap-default-search-key "imap")
 '(nnmail-crosspost nil)
 '(nnmail-expiry-wait 30)
 '(nnmail-extra-headers (quote (To Cc Newsgroups)))
 '(nnmail-scan-directory-mail-source-once t)
 '(ns-alternate-modifier (quote alt))
 '(ns-command-modifier (quote meta))
 '(nxml-sexp-element-flag t)
 '(nxml-slash-auto-complete-flag t)
 '(offlineimap-command "offlineimap -u machineui")
 '(org-src-tab-acts-natively t)
 '(pabbrev-idle-timer-verbose nil)
 '(package-archives (quote (("gnu" . "http://elpa.gnu.org/packages/") ("ELPA" . "http://tromey.com/elpa/") ("Marmalade" . "http://marmalade-repo.org/packages/"))))
 '(parens-require-spaces t)
 '(pcomplete-compare-entries-function (quote file-newer-than-file-p))
 '(persistent-scratch-file-name "~/.emacs.d/data/persistent-scratch")
 '(php+-flymake-enable t)
 '(php+-flymake-tests (quote (lint phpcs phpmd)))
 '(php+-mode-css-compile-on-save t)
 '(php+-mode-delete-trailing-whitespace t)
 '(php+-mode-js-compile-on-save t)
 '(php+-mode-php-compile-on-save nil)
 '(php+-mode-show-project-in-modeline t)
 '(php+-mode-show-trailing-whitespace t)
 '(php-auto-fill t)
 '(php-blank-line-at-end-of-class t)
 '(php-doc-default-author (quote ("Damon Haley" . "damon.haley@colorado.edu")))
 '(php-doc-default-category "CU-FIT")
 '(php-doc-default-copyright "2012 University of Colorado")
 '(php-doc-default-license "http://www.gnu.org/licenses/gpl-3.0.html GPLv3")
 '(php-doc-default-link "http://www.colorado.edu")
 '(php-doc-default-php-version "5.3")
 '(php-doc-default-version "SVN: $Id: $")
 '(php-file-patterns (quote ("\\.php[s34]?\\'" "\\.inc\\'")))
 '(php-format-align-array-double-arrows nil)
 '(php-format-break-all-method-call-arguments nil)
 '(php-format-break-all-method-chain-links t)
 '(php-hide-show-hide-doc-blocks t)
 '(php-hide-show-ignore-extensions (quote (".phtml")))
 '(php-manual-path "/usr/local/php-manual/php-chunked-xhtml")
 '(php-parse-send-to-front (quote (("__construct" nil method public) ("init" nil method public) ("setUp" nil method public) ("tearDown" nil property public))))
 '(php-project-list (quote (("rcweb" "/Users/bzwahr/projects/rcweb" "/Users/bzwahr/.ctags/rcweb" ("/usr/local/pear/share/pear/Zend/" "/usr/local/pear/share/pear/PHPUnit/") "/Users/bzwahr/projects/rcweb/tests/phpunit.xml" ("/Users/bzwahr/projects/rcweb/tests/application/" "/Users/bzwahr/projects/rcweb/library/Biosafety/" "/Users/bzwahr/projects/rcweb/application/") (("Brian Zwahr" . "bzwahr@tamu.edu") "" "" "" "" "" "" "Biosafety" "") "" "Default") ("scheduler" "/Users/bzwahr/projects/webscheduler" "/Users/bzwahr/.ctags/webscheduler" ("/usr/local/pear/share/pear/Zend/" "/usr/local/pear/share/pear/PHPUnit/") "/Users/bzwahr/projects/webscheduler/tests/phpunit.xml" ("/Users/bzwahr/projects/webscheduler/library/Scheduler/" "/Users/bzwahr/projects/webscheduler/application/" "/Users/bzwahr/projects/webscheduler/tests/application/") (("" . "") "" "" "" "" "" "" "Scheduler" "") "Default" ""))))
 '(php-tag-arguments (quote ("--PHP-kinds=+cf" "--regex-PHP='/abstract class ([^ ]*)//c/'" "--regex-PHP='/interface ([^ ]*)//c/'" "--regex-PHP='/(public |static |abstract |protected |private )+function ([^ (]*)//f/'")))
 '(php-tag-shell-command "/opt/local/bin/ctags")
 '(php-tags-relative t)
 '(php-test-ask-save nil)
 '(php-test-compile-tests (quote (lint phpcs phpmd)))
 '(php-test-file-extensions (quote ("php" "inc" "phtml")))
 '(php-test-show-command nil)
 '(phpcs-shell-command "/usr/local/pear/bin/phpcs")
 '(phpcs-standard "PSR2")
 '(phpmd-rulesets (quote (codesize design naming unusedcode)))
 '(phpmd-shell-command "/usr/local/pear/bin/phpmd")
 '(phpunit-shell-command "/usr/local/pear/bin/phpunit")
 '(ping-program-options (quote ("-c" "4")))
 '(pp^L-^L-string "
")
 '(proof-electric-terminator-enable t)
 '(proof-splash-enable nil)
 '(proof-three-window-enable nil)
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
 '(runner-init-file "~/.emacs.d/runner-conf.el")
 '(safe-local-variable-values (quote ((eval require (quote edg)) (eval ignore-errors (require (quote edg))) (after-save-hook my-byte-recompile-file) (after-save-hook git-commit-changes) (org-refer-by-number-id . "1C72AB51-33C2-4FF8-9367-2B494D125027"))))
 '(same-window-buffer-names (quote ("*eshell*" "*shell*" "*mail*" "*inferior-lisp*" "*ielm*" "*scheme*" "*Apropos*" "*Summary*" "*Backtrace*" "*Buffer List*" "*Colors*" "*Command History*" "*Diff*" "*Proced*" "*vc-dir*" "*SQL*" "scratch.org" "*Compile-Log*" "*Ibuffer*")))
 '(same-window-regexps (quote ("\\*vc\\-.+\\*" "\\*magit.+\\*" "*grep*" "\\*compilation\\*\\(\\|<[0-9]+>\\)" "\\*Help\\*\\(\\|<[0-9]+>\\)" "\\*Shell Command Output\\*\\(\\|<[0-9]+>\\)" "\\*dictem.*")))
 '(sauron-hide-mode-line t)
 '(sauron-watch-nicks erc-pals)
 '(sauron-watch-patterns erc-keywords)
 '(save-abbrevs (quote silently))
 '(save-interprogram-paste-before-kill t)
 '(save-kill-file-name "~/.emacs.d/data/kill-ring-saved.el")
 '(sc-citation-leader "")
 '(sc-confirm-always-p nil)
 '(sc-default-attribution "")
 '(sc-default-cite-frame (quote ((begin (progn (sc-fill-if-different) (setq sc-tmp-nested-regexp (sc-cite-regexp "") sc-tmp-nonnested-regexp (sc-cite-regexp) sc-tmp-dumb-regexp (concat "\\(" (sc-cite-regexp "") "\\)" (sc-cite-regexp sc-citation-nonnested-root-regexp))))) ("^[    ]*$" (if sc-cite-blank-lines-p (sc-cite-line) (sc-fill-if-different ""))) ((and (looking-at "^-- ?$") (not (save-excursion (goto-char (match-end 0)) (re-search-forward "^-- ?$" nil t)))) (sc-fill-if-different "")) (sc-reference-tag-string (if (string= sc-reference-tag-string "") (list (quote continue)) nil)) (sc-tmp-dumb-regexp (sc-cite-coerce-dumb-citer)) (sc-tmp-nested-regexp (sc-add-citation-level)) (sc-tmp-nonnested-regexp (sc-cite-coerce-cited-line)) (sc-nested-citation-p (sc-add-citation-level)) (t (sc-cite-line)) (end (sc-fill-if-different "")))))
 '(sc-preferred-attribution-list nil)
 '(sc-use-only-preference-p t)
 '(scroll-bar-mode nil)
 '(semanticdb-default-save-directory "~/.emacs.d/data/semanticdb")
 '(send-mail-function (quote sendmail-send-it))
 '(session-globals-exclude (quote (load-history flyspell-auto-correct-ring)))
 '(session-globals-include (quote ((kill-ring 10 nil) (session-file-alist 200 t) (file-name-history 200 nil) search-ring regexp-search-ring sr-history-registry)))
 '(session-initialize (quote (session places keys)))
 '(session-registers (quote (t (0 . 127))))
 '(session-save-file "~/.emacs.d/data/session")
 '(show-paren-delay 0)
 '(slime-kill-without-query-p t)
 '(slime-repl-history-file "~/.emacs.d/data/slime-history.eld")
 '(slime-startup-animation nil)
 '(smtpmail-default-smtp-server "smtp.colorado.edu")
 '(smtpmail-queue-dir "~/Messages/Gnus/Mail/queue/")
 '(smtpmail-smtp-server "smtp.colorado.edu")
 '(smtpmail-smtp-service 587)
 '(solarized-italic t)
 '(solarized-termcolors 256)
 '(spam-assassin-program "/opt/local/bin/spamc-5.12")
 '(spam-report-gmane-use-article-number nil)
 '(spam-sa-learn-program "/opt/local/bin/sa-learn-5.12")
 '(spam-use-regex-headers t)
 '(spam-use-spamassassin t)
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
 '(todochiku-icons (quote ((alarm . "alarm.png") (mail . "mail.png") (irc . "irc.png"))))
 '(todochiku-icons-directory (expand-file-name (concat dotfiles-dir "todochiku-icons/")))
 '(tool-bar-mode nil)
 '(tramp-auto-save-directory "~/.emacs.d/backups")
 '(tramp-default-method "rsyncc")
 '(tramp-default-method-alist (quote (("\\`\\(127\\.0\\.0\\.1\\|::1\\|localhost6?\\)\\'" "\\`root\\'" "sudo"))))
 '(tramp-persistency-file-name "~/.emacs.d/data/tramp")
 '(trash-directory "~/.Trash")
 '(undo-limit 800000)
 '(uniquify-buffer-name-style (quote post-forward-angle-brackets) nil (uniquify))
 '(url-cache-directory "/Users/daha1836/.emacs.d/data/url/cache")
 '(url-configuration-directory "~/.emacs.d/data/url/")
 '(url-irc-function (quote url-irc-erc))
 '(user-full-name "Damon K. Haley")
 '(user-initials "dkh")
 '(user-mail-address "damon.haley@coloraod.edu")
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
 '(wdired-use-dired-vertical-movement (quote sometimes))
 '(weblogger-config-alist (quote (("newartisans" "http://www.newartisans.com/xmlrpc.php" "daha1836" "" "5"))))
 '(wg-mode-line-on nil)
 '(wg-morph-on nil)
 '(wg-prefix-key "")
 '(wg-query-for-save-on-emacs-exit nil)
 '(wg-query-for-save-on-workgroups-mode-exit nil)
 '(whitespace-auto-cleanup t)
 '(whitespace-line-column 80)
 '(whitespace-rescan-timer-time nil)
 '(whitespace-silent t)
 '(whitespace-style (quote (face trailing lines space-before-tab empty)))
 '(workgroups-mode t)
 '(x-select-enable-clipboard t)
 '(x-stretch-cursor t)
 '(yaoddmuse-browse-function (quote w3m-browse-url))
 '(yaoddmuse-directory "~/.emacs.d/doc")
 '(yas/triggers-in-field t)
 '(yas/wrap-around-region t)
 '(zencoding-preview-default nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(completions-common-part ((t (:inherit default :foreground "red"))))
 '(diff-added ((((background dark)) (:foreground "#FFFF9B9BFFFF")) (t (:foreground "DarkGreen"))))
 '(diff-changed ((((background dark)) (:foreground "Yellow")) (t (:foreground "MediumBlue"))))
 '(diff-context ((((background dark)) (:foreground "White")) (t (:foreground "Black"))))
 '(diff-file-header ((((background dark)) (:foreground "Cyan" :background "Black")) (t (:foreground "Red" :background "White"))))
 '(diff-header ((((background dark)) (:foreground "Cyan")) (t (:foreground "Red"))))
 '(diff-index ((((background dark)) (:foreground "Magenta")) (t (:foreground "Green"))))
 '(diff-nonexistent ((((background dark)) (:foreground "#FFFFFFFF7474")) (t (:foreground "DarkBlue"))))
 '(diff-refine-change ((t (:inherit diff-refine-change :background nil))))
 '(diredp-compressed-file-suffix ((t (:foreground "#7b68ee"))) t)
 '(diredp-ignored-file-name ((t (:foreground "#aaaaaa"))) t)
 '(erb-face ((t (:background nil))) t)
 '(erb-out-delim-face ((t (:inherit erb-exec-delim-face :foreground "#b58900"))) t)
 '(iedit-occurrence ((t (:inherit lazy-highlight))) t)
 '(magit-diff-add ((t (:inherit diff-added :weight normal))))
 '(magit-diff-del ((t (:inherit diff-removed :weight normal))))
 '(match ((t (:inherit lazy-highlight :reverse t))))
 '(org-mode-line-clock ((t (:foreground "red" :box (:line-width -1 :style released-button)))) t)
 '(rainbow-delimiters-depth-1-face ((t (:foreground "blue"))) t)
 '(rainbow-delimiters-depth-2-face ((t (:foreground "blue"))) t)
 '(rainbow-delimiters-depth-3-face ((t (:foreground "#8b7500"))) t)
 '(rainbow-delimiters-depth-4-face ((t (:foreground "#8b7500"))) t)
 '(rainbow-delimiters-depth-5-face ((t (:foreground "#8b7500"))) t)
 '(rainbow-delimiters-depth-6-face ((t (:foreground "#8b7500"))) t)
 '(rainbow-delimiters-depth-7-face ((t (:foreground "#8b7500"))) t)
 '(rainbow-delimiters-depth-8-face ((t (:foreground "#8b7500"))) t)
 '(rainbow-delimiters-depth-9-face ((t (:foreground "#8b7500"))) t)
 '(rainbow-delimiters-unmatched-face ((t (:foreground "red"))) t)
 '(show-paren-match ((((class color) (background light)) (:background "azure2"))) t))
