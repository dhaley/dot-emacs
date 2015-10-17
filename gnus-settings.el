(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(check-mail-summary-function (quote check-mail-box-summary))
 '(gnus-activate-level 2)
 '(gnus-after-getting-new-news-hook
   (quote
    (gnus-group-list-groups gnus-group-save-newsrc gnus-display-time-event-handler)))
 '(gnus-agent-expire-all t)
 '(gnus-agent-expire-days 14)
 '(gnus-agent-go-online t)
 '(gnus-agent-mark-unread-after-downloaded nil)
 '(gnus-agent-synchronize-flags t)
 '(gnus-alias-default-identity "Haley")
 '(gnus-alias-identity-alist
   (quote
    (("CU" "" "\"Damon Haley\" <damon.haley@colorado.edu>" "University of Colorado" nil "" "~/Messages/sig/cu.sig")
     ("VI" "" "\"Damon Haley\" <dkh@member.fsf.org>" "Vinyl Island University" nil "" "~/Messages/sig/vi.sig")
     ("Haley" "" "\"Damon K. Haley\" <dhaley@warpmail.net>" "Haley's Comet" nil "" "")
     ("LSD" "" "vinylisl@sdf.lonestar.org" "Peace Love & Understanding" nil "" "~/Messages/sig/lsd.sig"))))
 '(gnus-alias-identity-rules
   (quote
    (("Emacs Mailing Lists"
      ("To" "emacs" current)
      "VI")
     ("Emacs Newsgroups"
      ("Newsgroups" "emacs" current)
      "VI")
     ("Ledger Mailing List"
      ("To" "ledger-cli@googlegroups\\\\.com" current)
      "VI")
     ("Colorado" cu-identify "CU"))))
 '(gnus-alias-override-user-mail-address t)
 '(gnus-alias-unknown-identity-rule (quote error))
 '(gnus-always-read-dribble-file t)
 '(gnus-article-date-lapsed-new-header t)
 '(gnus-article-update-date-headers nil)
 '(gnus-asynchronous t)
 '(gnus-auto-center-summary (quote vertical))
 '(gnus-auto-subscribed-groups "^nntp+news.gmane.org")
 '(gnus-blocked-images nil)
 '(gnus-check-new-newsgroups nil)
 '(gnus-completing-read-function (quote gnus-ido-completing-read))
 '(gnus-default-adaptive-score-alist
   (quote
    ((gnus-dormant-mark
      (from 20)
      (subject 100))
     (gnus-ticked-mark
      (subject 30))
     (gnus-read-mark
      (subject 30))
     (gnus-del-mark
      (subject -150))
     (gnus-catchup-mark
      (subject -150))
     (gnus-killed-mark
      (subject -1000))
     (gnus-expirable-mark
      (from -1000)
      (subject -1000)))))
 '(gnus-default-article-saver (quote gnus-summary-save-in-mail))
 '(gnus-directory "~/Messages/Gnus/News")
 '(gnus-gcc-mark-as-read t)
 '(gnus-generate-tree-function (quote gnus-generate-horizontal-tree))
 '(gnus-gravatar-properties (quote (:ascent center)))
 '(gnus-gravatar-too-ugly gnus-ignored-from-addresses)
 '(gnus-group-default-list-level 2)
 '(gnus-group-line-format "%S%p%P%M%5y: %(%B%G%B%)
")
 '(gnus-group-mode-hook (quote (gnus-topic-mode gnus-agent-mode hl-line-mode)))
 '(gnus-group-use-permanent-levels t)
 '(gnus-harvest-sender-alist
   (quote
    ((".*@\\(cu\\|colorado\\)\\.edu" . damon\.haley@colorado\.edu)
     (".*@fsf\\.org" . dkh@member\.fsf\.org))))
 '(gnus-home-directory "~/Messages/Gnus/")
 '(gnus-ignored-from-addresses
   "\\(damon\\.haley\\|dhaley\\|dkh\\)\\(-[^@]+\\)?@\\(colorado\\.edu\\|\\(hushmail\\|ssl\\-mail\\)\\.com\\|member\\.fsf\\.org\\)")
 '(gnus-ignored-mime-types
   (quote
    ("application/x-pkcs7-signature" "application/ms-tnef" "text/x-vcard")))
 '(gnus-interactive-exit (quote quiet))
 '(gnus-large-newsgroup 4000)
 '(gnus-mailing-list-groups "\\`\\(list\\|wg21\\)\\.")
 '(gnus-mark-unpicked-articles-as-read t)
 '(gnus-message-archive-group (quote ((format-time-string "sent.%Y"))))
 '(gnus-message-replyencrypt nil)
 '(gnus-no-groups-message "No Gnus for Daemon.")
 '(gnus-novice-user nil)
 '(gnus-parameters
   (quote
    (("list\\."
      (subscribed . t)
      (gcc-self . t))
     ("INBOX"
      (total-expire . t)
      (expiry-wait . 14)
      (expiry-target . "mail.archive"))
     ("\\(mail\\.\\|INBOX\\)"
      (gnus-use-scoring nil))
     ("mail\\.archive"
      (gnus-summary-line-format "%«%U%R %uS %ur %»%(%*%-14,14f %4u&size;
%1«%B%s%»%) "))
     ("list\\.emacs\\.devel"
      (to-address . "emacs-devel@gnu.org")
      (to-list . "emacs-devel@gnu.org")
      (total-expire . t)
      (expiry-wait . 90)
      (sieve anyof
             ((header :contains
                      ("To" "From" "Cc" "Sender")
                      "emacs-devel@gnu.org")
              (header :contains "list-id" "<emacs-devel.gnu.org>"))))
     ("list\\.emacs\\.sources"
      (to-address . "gnu-emacs-sources.gnu.org")
      (to-list . "gnu-emacs-sources.gnu.org")
      (total-expire . t)
      (expiry-wait . 90)
      (sieve anyof
             ((header :contains
                      ("To" "From" "Cc" "Sender")
                      "gnu-emacs-sources.gnu.org")
              (header :contains "list-id" "<gnu-emacs-sources.gnu.org>"))))
     ("list\\.emacs\\.help"
      (to-address . "help-gnu-emacs@gnu.org")
      (to-list . "help-gnu-emacs@gnu.org")
      (sieve anyof
             ((header :contains
                      ("To" "From" "Cc" "Sender")
                      "help-gnu-emacs@gnu.org")
              (header :contains "list-id" "<help-gnu-emacs.gnu.org>"))))
     ("list\\.emms\\.help"
      (to-address . "emms-help@gnu.org")
      (to-list . "emms-help@gnu.org")
      (sieve anyof
             ((header :contains
                      ("To" "From" "Cc" "Sender")
                      "emms-help@gnu.org")
              (header :contains "list-id" "<emms-help.gnu.org>"))))
     ("list\\.emacs\\.bugs"
      (to-list . "bug-gnu-emacs@gnu.org")
      (sieve anyof
             ((header :matches
                      ("To" "From" "Cc" "Sender")
                      "*@debbugs.gnu.org")
              (header :contains
                      ("To" "From" "Cc" "Sender")
                      "bug-gnu-emacs@gnu.org")
              (header :contains "list-id" "<bug-gnu-emacs.gnu.org>"))))
     ("list\\.emacs\\.diffs"
      (to-address . "emacs-diffs@gnu.org")
      (to-list . "emacs-diffs@gnu.org")
      (sieve anyof
             ((header :contains
                      ("To" "From" "Cc" "Sender")
                      "emacs-diffs@gnu.org")
              (header :contains "list-id" "<emacs-diffs.gnu.org>"))))
     ("list\\.emacs\\.elpa\\.diffs"
      (to-address . "emacs-elpa-diffs@gnu.org")
      (to-list . "emacs-diffs@gnu.org")
      (sieve anyof
             ((header :contains
                      ("To" "From" "Cc" "Sender")
                      "emacs-elpa-diffs@gnu.org")
              (header :contains "list-id" "<emacs-elpa-diffs.gnu.org>"))))
     ("list\\.emacs\\.buildstatus"
      (to-address . "emacs-buildstatus@gnu.org")
      (to-list . "emacs-buildstatus@gnu.org")
      (sieve anyof
             ((header :contains
                      ("To" "From" "Cc" "Sender")
                      "emacs-buildstatus@gnu.org")
              (header :contains "list-id" "<emacs-buildstatus.gnu.org>"))))
     ("list\\.emacs\\.sources"
      (to-address . "gnu-emacs-sources@gnu.org")
      (to-list . "gnu-emacs-sources@gnu.org")
      (sieve anyof
             ((header :contains
                      ("To" "From" "Cc" "Sender")
                      "gnu-emacs-sources@gnu.org")
              (header :contains "list-id" "<gnu-emacs-sources.gnu.org>"))))
     ("list\\.emacs\\.orgmode"
      (to-address . "emacs-orgmode@gnu.org")
      (to-list . "emacs-orgmode@gnu.org")
      (list-identifier . "\\[O\\]")
      (sieve anyof
             ((header :contains
                      ("To" "From" "Cc" "Sender")
                      "emacs-orgmode@gnu.org")
              (header :contains "list-id" "<emacs-orgmode.gnu.org>"))))
     ("^list\\.ding"
      (to-address . "ding@gnus.org")
      (to-list . "ding@gnus.org")
      (sieve anyof
             ((header :contains
                      ("To" "From" "Cc" "Sender")
                      "ding@gnus.org")
              (header :contains "list-id" "<ding.gnus.org>"))))
     ("^list\\.bbdb-info"
      (to-address . "bbdb-info@lists.sourceforge.net")
      (to-list . "bbdb-info.lists.sourceforge.net")
      (sieve anyof
             ((header :contains
                      ("To" "From" "Cc" "Sender")
                      "bbdb-info@lists.sourceforge.net")
              (header :contains "list-id" "<bbdb-info.lists.sourceforge.net>"))))
     ("^list\\.info-gnus"
      (to-address . "info-gnus-english@gnu.org")
      (to-list . "info-gnus-english@gnu.org")
      (sieve anyof
             ((header :contains
                      ("To" "From" "Cc" "Sender")
                      "info-gnus-english@gnu.org")
              (header :contains "list-id" "<info-gnus-english.gnu.org>"))))
     ("list\\.conkeror"
      (to-address . "conkeror@mozdev.org")
      (to-list . "conkeror@mozdev.org")
      (sieve anyof
             ((header :contains
                      ("To" "From" "Cc" "Sender")
                      "conkeror@mozdev.org")
              (header :contains "list-id" "<conkeror.mozdev.org>"))))
     ("list\\.emacs\\.macosx"
      (to-address . "macosx-emacs@email.esm.psu.edu")
      (sieve anyof
             ((header :contains
                      ("To" "From" "Cc" "Sender")
                      "macosx-emacs-request@email.esm.psu.edu")
              (header :contains "list-id" "<macosx-emacs.email.esm.psu.edu>"))))
     ("list\\.lug"
      (to-address . "lug@lug.boulder.co.us"))
     ("mail\\.cu\\.managed_services"
      (to-address . "trouble@colorado.edu"))
     ("mail\\.cu\\.its"
      (to-address . "help@colorado.edu"))
     ("mail.sc_kgnu_infotech"
      (to-address . "infotech@kgnu.org"))
     ("list\\\\.ledger\\\\.devel"
      (to-address . "ledger-cli@googlegroups.com")
      (to-list . "ledger-cli@googlegroups.com")
      (gcc-self . t)
      (sieve anyof
             ((header :contains
                      ("To" "From" "Cc" "Sender")
                      "ledger-cli@googlegroups.com")
              (header :contains "list-id" "<ledger-cli.googlegroups.com>"))))
     ("nnvirtual:micro-blog"
      (gnus-summary-line-format "%U%z %(%[%15&user-date;%]  %-15,15f  %B%s%)
")))))
 '(gnus-permanently-visible-groups "INBOX")
 '(gnus-picon-style (quote right))
 '(gnus-read-active-file nil)
 '(gnus-read-newsrc-file nil)
 '(gnus-refer-article-method
   (quote
    (current
     (nnir "nnimap:Local")
     (nntp "Gmane"
           (nntp-address "news.gmane.org")))))
 '(gnus-refer-thread-use-nnir t)
 '(gnus-registry-ignored-groups (quote (("nntp" t) ("^INBOX" t))))
 '(gnus-save-killed-list nil)
 '(gnus-save-newsrc-file nil)
 '(gnus-score-default-duration (quote p))
 '(gnus-score-expiry-days 30)
 '(gnus-select-group-hook (quote (gnus-group-set-timestamp)))
 '(gnus-select-method
   (quote
    (nnimap "Local"
            (nnimap-stream shell)
            (nnimap-shell-program "/usr/local/opt/dovecot/libexec/dovecot/imap"))))
 '(gnus-sieve-file "~/Messages/dovecot.sieve")
 '(gnus-sieve-select-method "nnimap:Local")
 '(gnus-signature-separator (quote ("^-- $" "^-- *$" "^_____+$")))
 '(gnus-simplify-subject-functions (quote (gnus-simplify-subject-fuzzy)))
 '(gnus-sort-gathered-threads-function (quote gnus-thread-sort-by-date) t)
 '(gnus-split-methods
   (quote
    ((gnus-save-site-lisp-file)
     (gnus-article-archive-name)
     (gnus-article-nndoc-name))))
 '(gnus-started-hook
   (quote
    ((lambda nil
       (run-hooks
        (quote gnus-after-getting-new-news-hook))))))
 '(gnus-startup-file "~/Messages/Gnus/.newsrc")
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
 '(gnus-suppress-duplicates t)
 '(gnus-suspend-gnus-hook (quote (gnus-group-save-newsrc)))
 '(gnus-thread-expunge-below -1000)
 '(gnus-thread-hide-subtree t)
 '(gnus-thread-sort-functions
   (quote
    (gnus-thread-sort-by-most-recent-number gnus-thread-sort-by-score)))
 '(gnus-topic-display-empty-topics nil)
 '(gnus-topic-line-format "%i[ %A: %(%{%n%}%) ]%v
")
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
 '(mail-envelope-from (quote header))
 '(mail-host-address "UCOMM-WEB-L-01")
 '(mail-setup-with-from nil)
 '(mail-source-delete-incoming t)
 '(mail-source-delete-old-incoming-confirm nil)
 '(mail-source-report-new-mail-interval 15)
 '(mail-sources (quote ((file :path "/var/mail/dadu"))))
 '(mail-specify-envelope-from t)
 '(mail-user-agent (quote gnus-user-agent))
 '(message-alternative-emails
   "\\(dhaley\\|vinylisl\\)@\\(hushmail\\|ssl\\-mail\\).com\\|\\(dkh@member\\.fsf\\|news@vinylisland\\)\\.org\\|damon\\.haley@colorado\\.edu")
 '(message-default-headers
   "X-Face: \"'PJ-yb+fYF0]%?,#==_(s>`~Hw_iwG![Cc+Sq$k>S|QbU)>?}Y51$4)\\9OEt:NL.@kZIqy <UnVZ*!XnGGV:iDO$YDhK7i~$.fs%r^0LJdztkb\\6=DI6by:GdO>.L<,Nd[nsMwrN3b]os1UqBw
X-Accept-Language: en-us
")
 '(message-default-mail-headers "Cc:
Bcc:
")
 '(message-directory "~/Messages/Gnus/Mail/")
 '(message-fill-column 78)
 '(message-interactive t)
 '(message-mail-alias-type nil)
 '(message-mode-hook
   (quote
    (abbrev-mode footnote-mode turn-on-auto-fill turn-on-flyspell
                 (lambda nil
                   (set-fill-column 78))
                 turn-on-orgstruct++ turn-on-orgtbl)))
 '(message-send-mail-function (quote message-send-mail-with-sendmail))
 '(message-send-mail-partially-limit nil)
 '(message-sendmail-envelope-from (quote header))
 '(message-sent-hook (quote (my-gnus-score-followup)))
 '(message-setup-hook (quote (gnus-harvest-set-from message-check-recipients)))
 '(message-signature-separator "^-- *$")
 '(message-subscribed-address-functions (quote (gnus-find-subscribed-addresses)))
 '(message-x-completion-alist
   (quote
    (("\\([rR]esent-\\|[rR]eply-\\)?[tT]o:\\|[bB]?[cC][cC]:" . gnus-harvest-find-address)
     ((if
          (boundp
           (quote message-newgroups-header-regexp))
          message-newgroups-header-regexp message-newsgroups-header-regexp)
      . message-expand-group))))
 '(mm-attachment-override-types
   (quote
    ("text/x-vcard" "application/pkcs7-mime" "application/x-pkcs7-mime" "application/pkcs7-signature" "application/x-pkcs7-signature" "image/.*")))
 '(mm-decrypt-option (quote always))
 '(mm-discouraged-alternatives (quote ("application/msword" "text/richtext")))
 '(mm-inline-text-html-with-images nil)
 '(mm-text-html-renderer (quote shr))
 '(mm-verify-option (quote always))
 '(mm-w3m-safe-url-regexp nil)
 '(nnir-imap-default-search-key "imap")
 '(nnmail-crosspost nil)
 '(nnmail-expiry-wait 30)
 '(nnmail-extra-headers (quote (To Cc Newsgroups)))
 '(nnmail-scan-directory-mail-source-once t)
 '(sc-citation-leader "")
 '(sc-confirm-always-p nil)
 '(sc-default-attribution "")
 '(sc-default-cite-frame
   (quote
    ((begin
      (progn
        (sc-fill-if-different)
        (setq sc-tmp-nested-regexp
              (sc-cite-regexp "")
              sc-tmp-nonnested-regexp
              (sc-cite-regexp)
              sc-tmp-dumb-regexp
              (concat "\\("
                      (sc-cite-regexp "")
                      "\\)"
                      (sc-cite-regexp sc-citation-nonnested-root-regexp)))))
     ("^[ 	]*$"
      (if sc-cite-blank-lines-p
          (sc-cite-line)
        (sc-fill-if-different "")))
     ((and
       (looking-at "^-- ?$")
       (not
        (save-excursion
          (goto-char
           (match-end 0))
          (re-search-forward "^-- ?$" nil t))))
      (sc-fill-if-different ""))
     (sc-reference-tag-string
      (if
          (string= sc-reference-tag-string "")
          (list
           (quote continue))
        nil))
     (sc-tmp-dumb-regexp
      (sc-cite-coerce-dumb-citer))
     (sc-tmp-nested-regexp
      (sc-add-citation-level))
     (sc-tmp-nonnested-regexp
      (sc-cite-coerce-cited-line))
     (sc-nested-citation-p
      (sc-add-citation-level))
     (t
      (sc-cite-line))
     (end
      (sc-fill-if-different "")))))
 '(sc-preferred-attribution-list nil)
 '(sc-use-only-preference-p t)
 '(send-mail-function (quote sendmail-send-it))
 '(smtpmail-default-smtp-server "mail.messagingengine.com")
 '(smtpmail-queue-dir "~/Messages/Gnus/Mail/queue/")
 '(smtpmail-smtp-server "smtp.colorado.edu")
 '(smtpmail-smtp-service 587))
