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
 '(ac-auto-show-menu nil)
 '(ac-auto-start 3)
 '(ac-comphist-file "/Users/daha1836/.emacs.d/data/ac-comphist.dat")
 '(ac-dwim nil)
 '(ac-ignore-case nil)
 '(ac-use-fuzzy nil)
 '(abbrev-mode t)
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
 '(bbdb-default-country "")
 '(bbdb-file "~/git/.emacs.d/.bbdb")
 '(bbdb-message-caching-enabled nil)
 '(bbdb-no-duplicates t)
 '(bbdb-offer-save (quote savenoprompt))
 '(bbdb-silent-running t)
 '(bbdb-use-pop-up nil)
 '(bbdb-vcard-import-translation-table (quote (("CELL\\|CAR" . "Mobile") ("WORK" . "Work") ("HOME" . "Home") ("^$" . "Work"))))
 '(bbdb/mail-auto-create-p nil)
 '(bbdb-complete-mail-allow-cycling t)
 '(bbdb-mail-user-agent 'gnus)
 '(bookmark-file "~/.emacs.d/data/breadcrumb")
 '(bind-key-segregation-regexp "\\`\\(\\(C-[chx.] \\|M-[gso] \\)\\([CM]-\\)?\\|.+-\\)")
 '(bm-highlight-style (quote bm-highlight-only-fringe))
 '(bmkp-bmenu-commands-file "~/.emacs.d/data/bmk-bmenu-commands.el")
 '(bmkp-bmenu-state-file "~/.emacs.d/data/bmk-bmenu-state.el")
 '(bmkp-last-as-first-bookmark-file nil)
 '(bookmark-default-file "~/Documents/bookmarks")
 '(browse-url-generic-program "conkeror")
 '(browse-url-default-browser "conkeror")
 '(byte-compile-verbose nil)
 '(c-default-style (quote ((java-mode . "gnu") (awk-mode . "awk") (other . "gnu"))))
 '(calendar-daylight-time-zone-name "CDT")
 '(calendar-latitude 40.73471)
 '(calendar-longitude -89.554659)
 '(calendar-mark-holidays-flag t)
 '(calendar-standard-time-zone-name "CST")
 '(calendar-time-zone -420)
 '(canlock-password "8d2ee9a7e4658c4ff6d863f91a3dd5340b3918ec")
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
 '(current-language-environment "UTF-8")
 '(custom-buffer-done-function (quote kill-buffer))
 '(custom-file "/Users/daha1836/.emacs.d/settings.el")
 '(custom-raised-buttons nil)
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
 '(egocentric-additional-keywords '("Damon" "vinylis" "dkh"))
 '(el-get-auto-update-cached-recipes nil)
 '(el-get-dir "~/.emacs.d/site-lisp/")
 '(el-get-generate-autoloads nil)
 '(emacs-lisp-mode-hook (quote (turn-on-auto-fill (lambda nil (ignore-errors (diminish (quote auto-fill-function)))) eldoc-mode (lambda nil (local-set-key [(meta 46)] (quote find-function)) (local-set-key [(control 109)] (quote newline-and-indent))))))
 '(emms-player-mplayer-parameters (quote ("-slave" "-quiet" "-really-quiet" "-volume" "15")))
 '(emms-source-file-default-directory "/Volumes/Data/Music/iTunes/iTunes Media/")
 '(enable-recursive-minibuffers t)
  '(erc-autoawayo-message "I'm at the cappuccino bar(after %i seconds of idle-time)")
 '(erc-autojoin-channels-alist '((".*\\.freenode.net" "#drupal-colorado" "#emacs" "#fsf-members" "#erc" "#conkeror" "#org-mode" "#freedombox")))
 '(erc-autojoin-mode t)
 '(erc-autojoin-timing :ident)
 '(erc-fools (quote ("anxt")))
 '(erc-generate-log-file-name-function (quote erc-generate-log-file-name-short))
 '(erc-header-line-format "%t: %o")
 '(erc-join-buffer 'bury)
 '(erc-hide-list (quote ("JOIN" "NICK" "PART" "QUIT" "MODE")))
 '(erc-ignore-list (quote ("rudybot!.*" "anxt")))
 '(erc-keywords '("\scu\s" "\stor\s" "Damon" "dkh" "dhaley"
   "relation module" "contextual filter" "gmane" "gwene"
   "usenet" "proxy" "privacy" "\ssocks\s"
   "gnus" "workgroups" "wireshark" "chaos tools" "hushmail" 
 "Stallman" "\sog\s" "compiz" "page manager" "super cookie" "[^\.]php"
   "nxhtml" "nxml" "erc\-keywords" "libnotify" "emacs 24" "org\W?mode"
   "\sunity\s" "starter\W?kit" "rate module" "conkeror" "jabber"
   "noscript" "https\-everywhere" "\wfsf\w" "\seff\s" "Log4j" "\sperl\s"
   "python" "generic-mode" "rainbow-mode" "SQLi" "sql-mode"
   "references module" "flag module" "org2blog" "nodeone" "Johan Falk"
   "singularity" "Lanier" "\serc\s" "mdb2" "\spear\s" "html5" "jquery"
   "naquadah" "mumamo" "el\-get" "zenburn" "solarized" "gunnars"
   "speedbar" "cedet" "ecb" "hook_menu_alter" "theme engine" "auto complete"
   "auto completion" "bbdb" "hippy" "\svariant\s" "json" "magit" "w3m"
   "dired" "bitcoin" "matu4291" "wwng" "ucomm" "crafts" "matt" "jo" "kevin" "cathy"))
 '(erc-log-channels-directory "~/git/.emacs.d/.erc/logs")
 '(erc-log-write-after-send t)
 '(erc-modules (quote (notify notifications autojoin completion dcc fill identd irccontrols list log match menu move-to-prompt netsplit networks noncommands readonly replace ring scrolltobottom services smiley stamp spelling track track-score)))
 '(erc-nick "dkh")
 '(erc-notify-list (quote ("ultimateboy" "matu4291" "matt" "kevin" "jo" "laimagaigalas" "crafts" "technomancy" "_schulte_" "linebarg" "sniderc" "alne1147" "kubie" "goldhamm" "kniffin" "kowalews" "orrie" "schaper" "johnw" "sachac" "rgr" "qDot")))
 '(erc-notifications-icon "~/.emacs.d/icons/irc.png")
 '(erc-nick-notify-urgency "normal")
 '(erc-pals (quote ("laimagaigalas" "matu4921" "^johnw!" "sachac" "matt"
                    "kevin" "jo")))
 '(erc-port 6667)
 '(erc-prompt (lambda ()
                   (if erc-network
                       (concat "[" (symbol-name erc-network) "]")
                     (concat "[" (car erc-default-recipients) "]"))))
 '(erc-priority-people-regexp "\\`[^#].+")
 '(erc-prompt-for-nickserv-password nil)
 '(erc-replace-alist (quote (("</?FONT>" . ""))))
 '(erc-server "asimov.freenode.net")
 '(erc-services-mode t)
 '(erc-track-enable-keybindings t)
 '(erc-track-exclude-types '("NICK" "JOIN" "PART" "QUIT" "MODE"
                                "301" ; away notice
                                "305" ; return from awayness
                                "306" ; set awayness
                                "332" ; topic notice
                                "333" ; who set the topic
                                "353" ; Names notice
                                "324" ; modes
                                "329" ; channel creation date
                                ))
 '(erc-track-exclude-server-buffer t)
 '(erc-track-exclude '("*stickychan" "*status"))
 '(erc-track-showcount t)
 '(erc-track-shorten-start 1)
 '(erc-track-switch-direction 'importance)
 '(erc-track-visibility 'selected-visible)
 '(erc-track-faces-priority-list (quote (erc-error-face (erc-nick-default-face erc-current-nick-face) erc-current-nick-face erc-keyword-face (erc-nick-default-face erc-pal-face) erc-pal-face erc-nick-msg-face erc-direct-msg-face)))
 '(erc-track-priority-faces-only t)
 '(erc-user-full-name (quote user-full-name))
 '(eshell-directory-name "~/git/.emacs.d/eshell/")
 '(eshell-aliases-file "~/git/.emacs.d/eshell/alias")
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
 '(eudc-protocol (quote ldap))
 '(eudc-query-form-attributes (quote (name firstname email phone cuedupersonuuid Postaladdress)))
 '(eudc-options-file "~/git/.emacs.d/.eudc-options")
 '(eudc-server "directory.colorado.edu")
 '(eudc-default-return-attributes nil)
 '(eudc-strict-return-matches nil)
 '(eudc-ldap-bbdb-conversion-alist (quote ((name . displayName) (net . mail) (address eudc-bbdbify-address Postaladdress "Office") (phone (eudc-bbdbify-phone telephoneNumber "Office")) (notes . title))))
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
 '(global-font-lock-mode t nil (font-lock))
 '(read-mail-command 'gnus)
 '(hippie-expand-try-functions-list (quote (yas/hippie-try-expand try-complete-file-name-partially try-complete-file-name try-expand-all-abbrevs try-expand-list try-expand-line try-expand-dabbrev try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill try-complete-lisp-symbol-partially try-complete-lisp-symbol)))
 '(history-delete-duplicates t)
 '(history-length 200)
 '(ibuffer-default-display-maybe-show-predicates t)
 '(ibuffer-expert t)
 '(ibuffer-formats (quote ((mark modified read-only " " (name 16 -1) " " (size 6 -1 :right) " " (mode 16 16) " " filename) (mark " " (name 16 -1) " " filename))))
 '(ibuffer-maybe-show-regexps nil)
 '(ibuffer-saved-filter-groups (quote (("default" ("Commands" (or (mode . shell-mode) (mode . eshell-mode) (mode . term-mode) (mode . compilation-mode))) ("Helm" (mode . helm-mode)) ("Magit" (or (mode . magit-status-mode) (mode . magit-log-mode))) ("C++" (or (mode . c-mode) (mode . c++-mode))) ("Lisp" (mode . emacs-lisp-mode)) ("Dired" (mode . dired-mode)) ("Gnus" (or (mode . message-mode) (mode . mail-mode) (mode . gnus-group-mode) (mode . gnus-summary-mode) (mode . gnus-article-mode) (name . "^\\.newsrc-dribble"))) ("Org" (or (name . "^\\*Calendar\\*$") (name . "^diary$") (mode . org-mode))) ("Emacs" (or (name . "^\\*scratch\\*$") (name . "^\\*Messages\\*$")))))))
 '(ibuffer-show-empty-filter-groups nil)
 '(ibuffer-shrink-to-minimum-size t t)
 '(ibuffer-use-other-window t)
 '(icicle-Completions-text-scale-decrease 0)
 '(icicle-apropos-cycle-next-keys (quote ([next] [(control 110)])))
 '(icicle-apropos-cycle-previous-keys (quote ([prior] [(control 112)])))
 '(icicle-incremental-completion nil)
 '(icicle-max-candidates 100)
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
 '(mediawiki-debug t)
 '(mac-command-modifier (quote hyper))
 '(mac-option-modifier (quote meta))
 '(mac-pass-command-to-system nil)
 '(mac-pass-control-to-system nil)
 '(mac-wheel-button-is-mouse-2 nil)
 '(magit-completing-read-function (quote magit-ido-completing-read))
 '(magit-process-popup-time 15)
 '(make-backup-file-name-function (quote my-make-backup-file-name))
 '(moccur-following-mode-toggle nil)
 '(modelinepos-column-limit 80)
 '(mouse-wheel-mode -1)
 '(mudel-mode-hook (quote (mudel-add-scroll-to-bottom)))
 '(mudel-output-filter-functions (quote (ansi-color-process-output)))
 '(multi-term-program "/opt/local/bin/screen")
 '(multi-term-program-switches "-DR")
 '(multi-term-scroll-show-maximum-output t)
 '(next-line-add-newlines nil)
 '(ns-alternate-modifier (quote alt))
 '(ns-command-modifier (quote meta))
 '(nxml-sexp-element-flag t)
 '(nxml-slash-auto-complete-flag t)
 '(offlineimap-command "offlineimap -u machineui")
 '(org-src-tab-acts-natively t)
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
 '(ping-program-options '("-c" "4"))
 '(pp^L-^L-string "                                                                              ")
 '(ps-font-size (quote (8 . 10)))
 '(ps-footer-font-size (quote (12 . 14)))
 '(ps-header-font-size (quote (12 . 14)))
 '(ps-header-title-font-size (quote (14 . 16)))
 '(ps-line-number-font-size 10)
 '(ps-print-color-p nil)
 '(rdebug-many-windows nil)
 '(read-buffer-function (quote ido-read-buffer))
 '(recentf-auto-cleanup (quote never))
 '(recentf-exclude (quote ("~\\'" "\\`out\\'" "\\.log\\'" "^/[^/]*:" "\\.el\\.gz\\'")))
 '(recentf-max-saved-items 2000)
 '(recentf-save-file "~/.emacs.d/data/recentf")
 '(redisplay-dont-pause t t)
 '(regex-tool-backend (quote perl))
 '(runner-init-file "~/.emacs.d/runner-conf.el")
  '(safe-local-variable-values (quote ((eval require (quote edg)) (eval ignore-errors (require (quote edg))) (after-save-hook my-byte-recompile-file) (after-save-hook git-commit-changes) (org-refer-by-number-id . "1C72AB51-33C2-4FF8-9367-2B494D125027"))))
 '(same-window-buffer-names (quote ("*eshell*" "*shell*" "*mail*" "*inferior-lisp*" "*ielm*" "*scheme*")))
 '(sauron-hide-mode-line t)
 '(sauron-watch-patterns erc-keywords)
 '(sauron-watch-nicks erc-notify-list)
 '(save-abbrevs (quote silently))
 '(save-interprogram-paste-before-kill t)
 '(save-kill-file-name "~/.emacs.d/data/kill-ring-saved.el")
 '(scroll-bar-mode nil)
 '(semanticdb-default-save-directory "~/.emacs.d/data/semanticdb")
 '(session-globals-exclude (quote (load-history flyspell-auto-correct-ring)))
 '(session-globals-include (quote ((kill-ring 10 nil) (session-file-alist 200 t) (file-name-history 200 nil) search-ring regexp-search-ring sr-history-registry)))
 '(session-initialize (quote (session places keys)))
 '(session-registers (quote (t (0 . 127))))
 '(session-save-file "~/.emacs.d/data/session")
 '(show-paren-delay 0)
 '(slime-kill-without-query-p t)
 '(slime-repl-history-file "~/.emacs.d/data/slime-history.eld")
 '(slime-startup-animation nil)
 '(solarized-italic t)
 '(solarized-termcolors 256)
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
 '( todochiku-icons-directory (expand-file-name (concat dotfiles-dir "todochiku-icons/")))
 '(todochiku-icons (quote ((alarm . "alarm.png") (mail . "mail.png") (irc . "irc.png"))))
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
