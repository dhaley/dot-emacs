;;;_ , Gnus
(load "gnus-settings")

(require 'gnus)
(require 'starttls)
(require 'nnmairix)
(require 'message)
(require 'bbdb)
(require 'bbdb-gnus)
(require 'bbdb-message)
(require 'async)

;;(gnus-compile)
(gnus-delay-initialize)

(bbdb-initialize 'gnus 'message)

(defvar switch-to-gnus-unplugged nil)
(defvar switch-to-gnus-run nil)

(defun switch-to-gnus (&optional arg)
  (interactive "P")
  (let* ((alist '("\\`\\*unsent" "\\`\\*Article"
                  "\\`\\*Summary" "\\`\\*Group"))
         (candidate
          (catch 'found
            (dolist (regexp alist)
              (dolist (buf (buffer-list))
                (if (string-match regexp (buffer-name buf))
                    (throw 'found buf)))))))
    (if (and switch-to-gnus-run candidate)
        (progn
          (if (featurep 'ido)
              (ido-visit-buffer candidate ido-default-buffer-method)
            (switch-to-buffer candidate))
          (if (string-match "Group" (buffer-name candidate))
              (gnus-group-get-new-news)))
      (let ((switch-to-gnus-unplugged arg))
        (gnus)
        (gnus-group-list-groups gnus-activate-level)
        (setq switch-to-gnus-run t)))))

(use-package fetchmail-ctl
  :init
  (progn
    (defun maybe-start-fetchmail-and-news ()
      (interactive)
      (when (and (not switch-to-gnus-unplugged)
                 (quickping "mail.messagingengine.com"))
        (do-applescript "tell application \"Notify\" to run")
        (switch-to-fetchmail)))

    (add-hook 'gnus-startup-hook 'maybe-start-fetchmail-and-news)

    (defadvice shutdown-fetchmail (after stop-mail-after-fetchmail activate)
      (async-start
       (lambda ()
         (call-process (expand-file-name "~/Messages/manage-mail/stop-mail")))
       (lambda (ret)
         (do-applescript "tell application \"Notify\" to quit"))))))

(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
(add-hook 'gnus-group-mode-hook 'hl-line-mode)

;; (require 'auto-show)
(use-package centered-cursor-mode)
(add-hook 'gnus-summary-mode-hook
          '(lambda ()
             (hl-line-mode 1)
             ;; (stripe-listify-buffer)
             ;; (centered-cursor-mode 1)
             ;; (visual-line-mode 1)
             (setq cursor-type t)))


(defun my-message-header-setup-hook ()
  (message-remove-header "From")
  (let ((gcc (message-field-value "Gcc")))
    (when (or (null gcc)
              (string-match "nnfolder\\+archive:" gcc))
      (message-remove-header "Gcc")
      (message-add-header
       (format "Gcc: %s"
               (if (string-match "\\`list\\." (or gnus-newsgroup-name ""))
                   "mail.sent"
                 "INBOX"))))))

(add-hook 'message-header-setup-hook 'my-message-header-setup-hook)

(defadvice gnus-summary-resend-message-edit (after call-my-mhs-hook activate)
  (my-message-header-setup-hook))

(defun my-gnus-summary-save-parts (&optional arg)
  (interactive "P")
  (let ((directory "~/Downloads"))
    (message "Saving all MIME parts to %s..." directory)
    (gnus-summary-save-parts ".*" directory arg)
    (message "Saving all MIME parts to %s...done" directory)))

(bind-key "X m" 'my-gnus-summary-save-parts gnus-summary-mode-map)

(defun queue-message-if-not-connected ()
  (set (make-local-variable 'gnus-agent-queue-mail)
       (if (quickping "smtp.colorado.edu") t 'always)))

(add-hook 'message-send-hook 'queue-message-if-not-connected)

(defun kick-postfix-if-needed ()
  (if (and (quickping "mail.messagingengine.com")
           (= 0 (call-process "/usr/bin/sudo" nil nil nil
                              "/usr/libexec/postfix/master" "-t")))
      (start-process "postfix" nil "/usr/bin/sudo"
                     "/usr/libexec/postfix/master" "-e" "60")))

(add-hook 'message-sent-hook 'kick-postfix-if-needed)

(defun exit-gnus-on-exit ()
  (if (and (fboundp 'gnus-group-exit)
           (gnus-alive-p))
      (with-current-buffer (get-buffer "*Group*")
        (let (gnus-interactive-exit)
          (gnus-group-exit)))))

(add-hook 'kill-emacs-hook 'exit-gnus-on-exit)

(defun open-mail-logs ()
  (interactive)
  (flet ((switch-in-other-buffer
          (buf)
          (when buf
            (split-window-vertically)
            (balance-windows)
            (switch-to-buffer-other-window buf))))
    (loop initially (delete-other-windows)
          with first = t
          for log in (directory-files "~/Messages/" t "\\.log\\'")
          for buf = (find-file-noselect log)
          do (if first
                 (progn
                   (switch-to-buffer buf)
                   (setf first nil))
               (switch-in-other-buffer buf))
          (with-current-buffer buf
            (goto-char (point-max))))))

(defun my-gnus-trash-article (arg)
  (interactive "P")
  (if (string-match "\\(drafts\\|queue\\|delayed\\)" gnus-newsgroup-name)
      (gnus-summary-delete-article arg)
    (gnus-summary-move-article arg "mail.trash")))

(define-key gnus-summary-mode-map [(meta ?q)] 'gnus-article-fill-long-lines)
(define-key gnus-summary-mode-map [?B delete] 'gnus-summary-delete-article)
(define-key gnus-summary-mode-map [?B backspace] 'my-gnus-trash-article)

(define-key gnus-article-mode-map [(meta ?q)] 'gnus-article-fill-long-lines)
(define-key gnus-article-mode-map (kbd "C-c C-x M-w") 'org-w3m-copy-for-org-mode)
(define-key gnus-article-mode-map (kbd "M-H") 'gnus-summary-catchup-to-here)
(define-key gnus-article-mode-map (kbd "Z-h") 'gnus-summary-catchup-to-here)
(bind-key "z" 'w3m-toggle-inline-images gnus-article-mode-map)

(defface gnus-summary-expirable-face
  '((((class color) (background dark))
     (:foreground "grey50" :italic t :strike-through t))
    (((class color) (background light))
     (:foreground "grey55" :italic t :strike-through t)))
  "Face used to highlight articles marked as expirable."
  :group 'gnus-summary-visual)

(push '((eq mark gnus-expirable-mark) . gnus-summary-expirable-face)
      gnus-summary-highlight)

(if window-system
    (setq
     gnus-summary-to-prefix "→"
     gnus-summary-newsgroup-prefix "⇶"
     gnus-ticked-mark ?⚑
     gnus-dormant-mark ?⚐
     gnus-expirable-mark ?♻
     gnus-read-mark ?✓
     gnus-del-mark ?✗
     gnus-killed-mark ?☠
     gnus-replied-mark ?⟲
     gnus-forwarded-mark ?⤳
     gnus-cached-mark ?☍
     gnus-recent-mark ?★
     gnus-unseen-mark ?✩
     gnus-unread-mark ?✉
     gnus-score-over-mark ?↑           ; ↑ ☀
     gnus-score-below-mark ?↓         ; ↓ ☂
     gnus-sum-thread-tree-false-root " ◌ "
     gnus-sum-thread-tree-single-indent "◎ "
     gnus-sum-thread-tree-indent "   "
     gnus-sum-thread-tree-root "● "
     gnus-sum-thread-tree-leaf-with-other "├─▶ "
     gnus-sum-thread-tree-single-leaf     "└─▶ " ; "╰─►"
     gnus-sum-thread-tree-vertical        "│ "
     ))

(defsubst dot-gnus-tos (time)
  "Convert TIME to a floating point number."
  (+ (* (car time) 65536.0)
     (cadr time)
     (/ (or (car (cdr (cdr time))) 0) 1000000.0)))

(defun gnus-user-format-function-S (header)
  "Return how much time it's been since something was sent."
  (condition-case err
      (let ((date (mail-header-date header)))
        (if (> (length date) 0)
            (let*
                ((then (dot-gnus-tos
                        (apply 'encode-time (parse-time-string date))))
                 (now (dot-gnus-tos (current-time)))
                 (diff (- now then))
                 (str
                  (cond
                   ((>= diff (* 86400.0 7.0 52.0))
                    (if (>= diff (* 86400.0 7.0 52.0 10.0))
                        (format "%3dY" (floor (/ diff (* 86400.0 7.0 52.0))))
                      (format "%3.1fY" (/ diff (* 86400.0 7.0 52.0)))))
                   ((>= diff (* 86400.0 30.0))
                    (if (>= diff (* 86400.0 30.0 10.0))
                        (format "%3dM" (floor (/ diff (* 86400.0 30.0))))
                      (format "%3.1fM" (/ diff (* 86400.0 30.0)))))
                   ((>= diff (* 86400.0 7.0))
                    (if (>= diff (* 86400.0 7.0 10.0))
                        (format "%3dw" (floor (/ diff (* 86400.0 7.0))))
                      (format "%3.1fw" (/ diff (* 86400.0 7.0)))))
                   ((>= diff 86400.0)
                    (if (>= diff (* 86400.0 10.0))
                        (format "%3dd" (floor (/ diff 86400.0)))
                      (format "%3.1fd" (/ diff 86400.0))))
                   ((>= diff 3600.0)
                    (if (>= diff (* 3600.0 10.0))
                        (format "%3dh" (floor (/ diff 3600.0)))
                      (format "%3.1fh" (/ diff 3600.0))))
                   ((>= diff 60.0)
                    (if (>= diff (* 60.0 10.0))
                        (format "%3dm" (floor (/ diff 60.0)))
                      (format "%3.1fm" (/ diff 60.0))))
                   (t
                    (format "%3ds" (floor diff)))))
                 (stripped
                  (replace-regexp-in-string "\\.0" "" str)))
              (concat (cond
                       ((= 2 (length stripped)) "  ")
                       ((= 3 (length stripped)) " ")
                       (t ""))
                      stripped))))
    (error "    ")))

(defvar gnus-count-recipients-threshold 5
  "*Number of recipients to consider as large.")

(defun gnus-user-format-function-r (header)
  "Given a Gnus message header, returns priority mark.
Here are the meanings:

The first column represent my relationship to the To: field.  It can be:

         I didn't appear (and the letter had one recipient)
   :     I didn't appear (and the letter had more than one recipient)
   <     I was the sole recipient
   +     I was among a few recipients
   *     There were many recipients

The second column represents the Cc: field:

    .    I wasn't mentioned, but one other was
    :    I wasn't mentioned, but others were
    ^    I was the only Cc mentioned
    &    I was among a few Cc recipients
    %    I was among many Cc recipients

These can combine in some ways to tell you at a glance how visible the message
is:

   >.    Someone wrote to me and one other
    &    I was copied along with several other people
   *:    Mail to lots of people in both the To and Cc!"
  (let* ((to (or (cdr (assoc 'To (mail-header-extra header))) ""))
         (cc (or (cdr (assoc 'Cc (mail-header-extra header))) ""))
         (to-len (length (split-string to "\\s-*,\\s-*")))
         (cc-len (length (split-string cc "\\s-*,\\s-*")))
         (to-char (cond )))
    (cond ((string-match gnus-ignored-from-addresses to)
           (cond ((= to-len 1)
                  (cond ((string= cc "") "< ")
                        ((= cc-len 1) "<.")
                        (t "<:")))
                 ((< to-len gnus-count-recipients-threshold)
                  (cond ((string= cc "") "+ ")
                        ((= cc-len 1) "+.")
                        (t "+:")))
                 (t
                  (cond ((string= cc "") "* ")
                        ((= cc-len 1) "*.")
                        (t "*:")))))

          ((string-match gnus-ignored-from-addresses cc)
           (cond ((= cc-len 1)
                  (cond ((= to-len 1) " ^")
                        (t ":^")))
                 ((< cc-len gnus-count-recipients-threshold)
                  (cond ((= to-len 1) " &")
                        (t ":&")))
                 (t
                  (cond ((= to-len 1) " %")
                        (t ":%")))))
          (t "  "))))

(use-package message-x)

(use-package gnus-dired
  :commands gnus-dired-mode
  :init
  (add-hook 'dired-mode-hook 'gnus-dired-mode))

(use-package my-gnus-score
  :init
  (progn
    (defun gnus-group-get-all-new-news ()
      (interactive)
      (gnus-group-get-new-news 5)
      (gnus-group-list-groups 4)
      (my-gnus-score-groups)
      (gnus-group-list-groups 4))

    (define-key gnus-group-mode-map [?v ?g] 'gnus-group-get-all-new-news)))

(use-package gnus-demon
  :init
  (progn
    (defun gnus-demon-scan-news-2 ()
      (when gnus-plugged
        (let ((win (current-window-configuration))
              (gnus-read-active-file nil)
              (gnus-check-new-newsgroups nil)
              (gnus-verbose 2)
              (gnus-verbose-backends 5))
          (unwind-protect
              (save-window-excursion
                (when (gnus-alive-p)
                  (with-current-buffer gnus-group-buffer
                    (gnus-group-get-new-news gnus-activate-level))))
            (set-window-configuration win)))))

    ;; (gnus-demon-add-handler 'gnus-demon-scan-news-2 5 2)

    (defun save-gnus-newsrc ()
      (if (and (fboundp 'gnus-group-exit)
               (gnus-alive-p))
          (with-current-buffer (get-buffer "*Group*")
            (gnus-save-newsrc-file))))

    (gnus-demon-add-handler 'save-gnus-newsrc nil 1)
    (gnus-demon-add-handler 'gnus-demon-close-connections nil 3)))

(use-package nnir
  :init
  (progn
    (defun activate-gnus ()
      (unless (get-buffer "*Group*") (gnus)))

    (defun gnus-goto-article (message-id)
      (activate-gnus)

      (defun gnus-goto-article (message-id)
        (activate-gnus)
        (gnus-summary-read-group "INBOX" 15 t)
        (let ((nnir-imap-default-search-key "imap")
              (nnir-ignored-newsgroups "mail\\.\\(spam\\|save\\|trash\\|sent\\)\\)")))
        (gnus-summary-refer-article message-id)))

    (defvar gnus-query-history nil)

    (defun gnus-query (query &optional arg)
      (interactive
       (list (read-string (format "IMAP Query %s: "
                                  (if current-prefix-arg "All" "Mail"))
                          (format-time-string "SENTSINCE %d-%b-%Y "
                                              (time-subtract (current-time)
                                                             (days-to-time 90)))
                          'gnus-query-history)
             current-prefix-arg))
      (activate-gnus)
      (let ((nnir-imap-default-search-key "imap")
            (nnir-ignored-newsgroups
             (if arg
                 (concat (regexp-opt
                          '("archive"
                            "archive.emacs"
                            "list"
                            "list.emacs"
                            "list.ledger"
                            "mail"
                            "mail.save"
                            "Drafts"
                            "Sent Messages"))
                         "\\'")
               (concat "\\(\\(list\\|archive\\)\\.\\|"
                       "mail\\.\\(spam\\|save\\|trash\\|sent\\)\\)"))))
        (gnus-group-make-nnir-group
         nil `((query    . ,query)
               (criteria . "")
               (server   . "nnimap:Local")))))

    (define-key global-map [(hyper meta ?f)] 'gnus-query)


    ;; You can now even Japan language folder search of so far plus the
    ;; following settings.

    (defadvice nnir-run-imap (before decode-group activate)
      (ad-set-arg 2 (mapcar 'gnus-group-decoded-name (ad-get-arg 2))))))

(use-package gnus-harvest
  :init
  (if (featurep 'message-x)
      (gnus-harvest-install 'message-x)
    (gnus-harvest-install)))

(use-package gnus-alias
  :commands (gnus-alias-determine-identity
             gnus-alias-message-x-completion
             gnus-alias-select-identity)
  :init
  (progn
    (add-hook 'message-setup-hook 'gnus-alias-determine-identity)

    (if (featurep 'message-x)
        (add-hook 'message-x-after-completion-functions
                  'gnus-alias-message-x-completion))

    (define-key message-mode-map "\C-c\C-f\C-p" 'gnus-alias-select-identity)

    (defun cu-identify (&optional arg)
      "Idenfify group at point."
      (interactive "P")
      ;; returns either nil or non-nil.  Non-nil indicates that a match of
      ;; whatever kind was achieved, and the specified Identity should be
      ;; used.
      (let ((group (save-excursion
                     (set-buffer gnus-group-buffer)
                     (gnus-group-group-name))))
        (if (string-match  "^mail\\.cu\\." group) group nil)))))

(use-package rs-gnus-summary
  :init
  (progn
    (defalias 'gnus-user-format-function-size
      'rs-gnus-summary-line-message-size)

    (setq gnus-balloon-face-0 'rs-gnus-balloon-0)
    (setq gnus-balloon-face-1 'rs-gnus-balloon-1)))

(use-package supercite
  :commands sc-cite-original
  :init
  (progn
    (add-hook 'mail-citation-hook 'sc-cite-original)

    (defun sc-remove-existing-signature ()
      (save-excursion
        (goto-char (region-beginning))
        (when (re-search-forward message-signature-separator (region-end) t)
          (delete-region (match-beginning 0) (region-end)))))

    (add-hook 'mail-citation-hook 'sc-remove-existing-signature))

  :config
  (defun sc-fill-if-different (&optional prefix)
    "Fill the region bounded by `sc-fill-begin' and point.
Only fill if optional PREFIX is different than
`sc-fill-line-prefix'.  If `sc-auto-fill-region-p' is nil, do not
fill region.  If PREFIX is not supplied, initialize fill
variables.  This is useful for a regi `begin' frame-entry."
    (if (not prefix)
        (setq sc-fill-line-prefix ""
              sc-fill-begin (line-beginning-position))
      (if (and sc-auto-fill-region-p
               (not (string= prefix sc-fill-line-prefix)))
          (let ((fill-prefix sc-fill-line-prefix))
            (unless (or (string= fill-prefix "")
                        (save-excursion
                          (goto-char sc-fill-begin)
                          (or (looking-at ">+  +")
                              (< (length
                                  (buffer-substring (point)
                                                    (line-end-position)))
                                 65))))
              (fill-region sc-fill-begin (line-beginning-position)))
            (setq sc-fill-line-prefix prefix
                  sc-fill-begin (line-beginning-position)))))
    nil))

(use-package browse-url
  :commands browse-url
  :init
  (progn
    (defun gnus-article-get-urls-region (min max)
      "Return a list of urls found in the region between MIN and MAX"
      (let (url-list)
        (save-excursion
          (save-restriction
            (narrow-to-region min max)
            (goto-char (point-min))
            (while (re-search-forward gnus-button-url-regexp nil t)
              (let ((match-string (match-string-no-properties 0)))
                (if (and (not (equal (substring match-string 0 4) "file"))
                         (not (member match-string url-list)))
                    (setq url-list (cons match-string url-list)))))))
        url-list))

    (defun gnus-article-get-current-urls ()
      "Return a list of the urls found in the current `gnus-article-buffer'"
      (let (url-list)
        (with-current-buffer gnus-article-buffer
          (setq url-list
                (gnus-article-get-urls-region (point-min) (point-max))))
        url-list))

    (defun gnus-article-browse-urls ()
      "Visit a URL from the `gnus-article-buffer' by showing a
buffer with the list of URLs found with the `gnus-button-url-regexp'."
      (interactive)
      (gnus-configure-windows 'article)
      (gnus-summary-select-article nil nil 'pseudo)
      (let ((temp-buffer (generate-new-buffer " *Article URLS*"))
            (urls (gnus-article-get-current-urls))
            (this-window (selected-window))
            (browse-window (get-buffer-window gnus-article-buffer))
            (count 0))
        (save-excursion
          (save-window-excursion
            (with-current-buffer temp-buffer
              (mapc (lambda (string)
                      (insert (format "\t%d: %s\n" count string))
                      (setq count (1+ count))) urls)
              (not-modified)
              (pop-to-buffer temp-buffer)
              (setq count
                    (string-to-number
                     (char-to-string (if (fboundp
                                          'read-char-exclusive)
                                         (read-char-exclusive)
                                       (read-char)))))
              (kill-buffer temp-buffer)))
          (if browse-window
              (progn (select-window browse-window)
                     (browse-url (nth count urls)))))
        (select-window this-window)))

    (define-key gnus-summary-mode-map [(control ?c) (control ?o)]
      'gnus-article-browse-urls)
    (define-key gnus-article-mode-map [(control ?c) (control ?o)]
      'gnus-article-browse-urls)

    (define-key gnus-article-mode-map [(control ?c) (control ?g)]
      'w3m-lnum-goto)))

(define-key gnus-summary-mode-map (kbd ">") 'gnus-summary-show-thread)
(define-key gnus-summary-mode-map (kbd "<") 'gnus-summary-hide-thread)


;; some trickery to show the newsread people are using and colour code depending on type
;; in this case highlight users of any outlook type dross :-;

(add-to-list
 'gnus-header-face-alist
 (list (concat
        "^"
        (regexp-opt '("User-Agent" "X-Mailer" "Newsreader" "X-Newsreader") t)
        ":.*") ;; other
       nil font-lock-comment-face))

(add-to-list
 'gnus-header-face-alist
 (list (concat
        "^"
        (regexp-opt '("User-Agent" "X-Mailer" "Newsreader" "X-Newsreader") t)
        ":.*Outlook.*")
       nil 'gnus-emphasis-highlight-words))

;; ;; And show any real men who use Gnus!
(add-to-list
 'gnus-header-face-alist
 (list (concat
        "^"
        (regexp-opt '("User-Agent" "X-Mailer" "Newsreader" "X-Newsreader") t)
        ":.*Gnus.*")
       nil 'gnus-server-opened))

(eval-after-load "gnus-art"
  '(progn
     (setq gnus-visible-headers
           (concat (format
                    "^\\(%s\\):"
                    (regexp-opt '("User-Agent" "X-Mailer" "X-Newsreader"
                                  "X-Spam-Level" "List-Id" "X-Report-Spam"
                                  "Archived-At" "Comments")))
                   "\\|" gnus-visible-headers))
     (add-to-list 'gnus-picon-databases "/usr/share/picons")
     ))


(defun message-check-news-syntax ()
  "Check the syntax of the message and prompt the user to be sure he wants to send."
  (and
   (save-excursion
     (save-restriction
       (widen)
       (and
        ;; We narrow to the headers and check them first.
        (save-excursion
          (save-restriction
            (message-narrow-to-headers)
            (message-check-news-header-syntax)))
        ;; Check the body.
        (message-check-news-body-syntax))))
                                        ; sm: this last line is my addition
   (y-or-n-p "Post the message? ")
   ))

(defvar my-message-attachment-regexp
  "attach\\|\Wfiles?\W\\|enclose\\|\Wdraft\\|\Wversion")
(defun check-mail ()
  "ask for confirmation before sending a mail. Scan for possible attachment"
  (save-excursion
    (message-goto-body)
    (let ((warning ""))
      (when (and (search-forward-regexp my-message-attachment-regexp nil t nil)
                 (not (search-forward "<#part" nil t nil)))
        (setq warning "No attachment.\n"))
      (goto-char (point-min))
      (unless (message-y-or-n-p (concat warning "Send the message ? ") nil nil)
        (error "Message not sent")))))
(add-hook 'message-send-hook 'check-mail)

(define-key gnus-article-mode-map (kbd "<deletechar>") 'gnus-article-goto-prev-page)


(defun gnus-goto-last-link ()
  "A lot of articles just have one link to goto"
  (interactive)
  (gnus-summary-show-article)
  (gnus-summary-select-article-buffer)
  (goto-char (point-max))
  (forward-line -1)
  (rgr/browse-url)
  ;; (let ((cur (wg-name (wg-current-workgroup)))
  ;;       (prev (wg-name (wg-previous-workgroup))))
  ;; (if (string-equal prev "gnus")
  ;;   (wg-switch-to-index-1)))
  (gnus-article-show-summary)
  (gnus-summary-mark-as-read-forward 1)
  )

(defun my-gnus-browse-gwene ()
  "Start a browser for current gwene article"
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (eq major-mode 'gnus-article-mode)
        (gnus-summary-show-article '(4))
        (switch-to-buffer buf)
        (goto-char (point-min))
        (re-search-forward "^Archived-at: <\\(.*\\)>$")
        (browse-url (match-string 1))
        (gnus-summary-show-article)))))

;; from Peter Munster


(defun my-alter-summary-map ()
  (local-set-key "i" '(lambda () (interactive)
                        (gnus-goto-last-link)
                        ))
  ;; (local-set-key "z" 'gnus-summary-mark-as-read-forward)
  (local-set-key ":" 'bbdb-mua-display-records)
  (local-set-key "d" [?M ?M ?e ?e down]))

(defun my-alter-article-map ()
  (local-set-key "1" 'delete-other-windows)
  (local-set-key "q" '(lambda () (interactive)
                        (gnus-article-show-summary)
                        (delete-other-windows)))
  (local-set-key "n" "hnh1")
  (local-set-key "N" "hNh1")
  (local-set-key "d" "hMMeenh1")
  )

(add-hook 'gnus-summary-mode-hook 'my-alter-summary-map)
(add-hook 'gnus-article-mode-hook 'my-alter-article-map)


(fset 'blow_up_article
      [S-down ?\C-x ?\C-+ ?\C-x ?\C-+ ?\C-x ?\C-+ ?\C-x ?\C-+ S-up])

(define-key gnus-summary-mode-map (kbd "C-c 5") 'blow_up_article)
;; (define-key gnus-summary-mode-map (kbd "C-c 5") 'gnus-summary-select-article-buffer)


;; text/calendar attachments

(add-to-list 'mm-inlined-types "text/calendar")
(add-to-list 'mm-automatic-display "text/calendar")
(add-to-list 'mm-inline-media-tests '("text/calendar" mm-inline-text-calendar identity))

(require 'icalendar)
(require 'boxquote)

(defun mm-inline-text-calendar (handle)
  (let ((text ""))
    (with-temp-buffer
      (mm-insert-part handle)
      (save-window-excursion
        (setq info (mapcar (lambda (s) (mm-decode-string s "utf-8"))
                           (format-text-calendar-for-display (icalendar--read-element nil nil))))))
    (let ((start (point)))
      (mm-insert-inline handle "\n")
      (mm-insert-inline handle (car info))
      (mm-insert-inline handle "\n")
      (goto-char start)
      (when (search-forward "DESCRIPTION " nil t)
        (replace-match "" nil t)
        (beginning-of-line)
        (fill-region (point) (point-max)))
      (boxquote-region start (point-max)))
    (boxquote-title (car (cdr info)))
    (mm-insert-inline handle "\n")))

(defun format-text-calendar-for-display (element)
  "Format a text/calendar element parsed by icalendar--read-element into text"
  (let ((fields '(LOCATION ORGANIZER DESCRIPTION))
        (fieldformat "%-11s %s\n")
        (content "")
        (first-date "")
        (last-date "")
        (title ""))
    (dolist (event (icalendar--all-events element))
      (let* ((zone-map (icalendar--convert-all-timezones element))
             (dtstart-zone (icalendar--find-time-zone (icalendar--get-event-property-attributes event 'DTSTART) zone-map))
             (dtstart (icalendar--decode-isodatetime (icalendar--get-event-property event 'DTSTART) nil dtstart-zone))
             (dtend-zone (icalendar--find-time-zone (icalendar--get-event-property-attributes event 'DTEND) zone-map))
             (dtend   (icalendar--decode-isodatetime (icalendar--get-event-property event 'DTEND) nil dtend-zone))
             (datestring (concat (icalendar--datetime-to-iso-date dtstart "-") " "
                                 (icalendar--datetime-to-colontime dtstart) "-"
                                 (icalendar--datetime-to-colontime dtend))))
        (when (string-equal first-date "")
          (setq first-date datestring))
        (setq last-date datestring)
        (dolist (field fields)
          (let ((propertyvalue (mapconcat (lambda (property)
                                            (icalendar--convert-string-for-import property)
                                            (replace-regexp-in-string "\\\\n" "\n"
                                                                      (replace-regexp-in-string "^MAILTO:" "" property)))
                                          (icalendar--get-event-properties event field) " ")))
            (when (not (string-equal propertyvalue ""))
              (setq content (concat content (format fieldformat field (replace-regexp-in-string "\\\\ " "" propertyvalue)))))))))
    (setq content (replace-regexp-in-string "\n\n$" "" content))
    (setq title (if (string-equal first-date last-date) first-date (concat first-date " ... " last-date)))
    (list content title)))

(use-package gnus-calendar
  :init
  (progn
    (gnus-calendar-setup)
    (gnus-calendar-org-setup)))


(provide 'dot-gnus)

;; Local Variables:
;;   mode: emacs-lisp
;;   mode: allout
;;   outline-regexp: "^;;;_\\([,. ]+\\)"
;; End:

;;; dot-gnus.el ends here
