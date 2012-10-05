
(require 'gnus)
(require 'starttls)
(require 'nnmairix)
(require 'message)
(require 'bbdb-gnus)

;;(gnus-compile)
(gnus-delay-initialize)
;;(bbdb-insinuate-gnus)

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

(add-hook 'gnus-summary-mode-hook 'hl-line-mode)

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

(defun exit-gnus-on-exit ()
  (if (and (fboundp 'gnus-group-exit)
           (gnus-alive-p))
      (with-current-buffer (get-buffer "*Group*")
        (let (gnus-interactive-exit)
          (gnus-group-exit)))))

(add-hook 'kill-emacs-hook 'exit-gnus-on-exit)

(define-key gnus-summary-mode-map [(meta ?q)] 'gnus-article-fill-long-lines)
;;(define-key gnus-summary-mode-map [?$] 'gmail-report-spam)
(define-key gnus-summary-mode-map [?B delete] 'gnus-summary-delete-article)
;;(define-key gnus-summary-mode-map [?B backspace] 'my-gnus-trash-article)

(define-key gnus-article-mode-map [(meta ?q)] 'gnus-article-fill-long-lines)

(defface gnus-summary-expirable-face
  '((((class color) (background dark))
     (:foreground "grey50" :italic t :strike-through t))
    (((class color) (background light))
     (:foreground "grey55" :italic t :strike-through t)))
  "Face used to highlight articles marked as expirable."
  :group 'gnus-summary-visual)

 (push '((eq mark gnus-expirable-mark) . gnus-summary-expirable-face)
      gnus-summary-highlight
)

;;(if window-system
;;    (setq
;;     gnus-sum-thread-tree-false-root      ""
;;     gnus-sum-thread-tree-single-indent   ""
;;     gnus-sum-thread-tree-root            ""
;;     gnus-sum-thread-tree-vertical        "|"
;;     gnus-sum-thread-tree-leaf-with-other "+-> "
;;     gnus-sum-thread-tree-single-leaf     "\\-> "
;;     gnus-sum-thread-tree-indent          " "
;;     ))

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
  (add-hook 'mail-citation-hook 'sc-cite-original)
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
      'gnus-article-browse-urls)))


(global-set-key (kbd "C-x m") 'compose-mail)

(add-hook 'message-sent-hook 'gnus-score-followup-article)
(add-hook 'message-sent-hook 'gnus-score-followup-thread)



(defun my-gnus ()
  "Start a new Gnus, or locate the existing buffer *Group*."
  (interactive)
  (if (buffer-live-p    (get-buffer "*Group*"))
      (switch-to-buffer (get-buffer "*Group*"))
    (gnus)))


(copy-face 'default 'my-gnus-face)
(copy-face 'my-gnus-face 'my-subject-face)

(copy-face 'my-gnus-face 'my-group-face)
(set-face-attribute 'my-group-face nil :inherit 'my-gnus-face)

(copy-face 'my-group-face 'my-group-face-unread)
(set-face-attribute 'my-group-face-unread nil :inherit 'my-group-face)

(copy-face 'my-group-face 'my-group-server-face)
(copy-face 'my-group-server-face 'my-group-server-face-unread)
(set-face-attribute 'my-group-server-face-unread nil :inherit 'my-group-server-face)

(copy-face 'my-group-face 'my-unread-count-face)
(copy-face 'my-unread-count-face 'my-unread-count-face-unread)
(set-face-attribute 'my-unread-count-face-unread nil :inherit 'my-unread-count-face)

(copy-face 'my-group-face 'my-inbox-icon-face)
(copy-face 'my-inbox-icon-face 'my-inbox-icon-face-unread)
(set-face-attribute 'my-inbox-icon-face-unread nil :inherit 'my-inbox-icon-face)

(copy-face 'my-gnus-face 'my-topic-empty-face)
(copy-face 'my-gnus-face 'my-topic-face)


;; (require 'gnushush)

;;           (require 'miniedit)

(defun dkh/unread-face (f)
  (intern (if (> (string-to-number gnus-tmp-number-of-unread) 0) (concat f "-unread") f)))

;; this corresponds to a topic line format of "%n %A"
(defun gnus-user-format-function-topic-line (dummy)
  (let ((topic-face (if (zerop total-number-of-articles)
                        'my-topic-empty-face
                      'my-topic-face)))
    (propertize
     (format "%s %d" name total-number-of-articles)
     'face topic-face)))

(defun gnus-user-format-function-s (header)
  (propertize (mail-header-subject header) 'face 'my-subject-face 'gnus-face t))

;; dkh commented out all this stuff
(defun gnus-user-format-function-g (headers) ;; gnus-group-line-format use %ug to call this func! e.g  "%M%S%p%P%(%-40,40ug%)%-5uy %ud\n"
  split full group protocol-server:group into three parts.
  (message "format function g for group %s" gnus-tmp-group)
  (string-match "\\(^.*\\)\\+\\(.*\\):\\(.*\\)" gnus-tmp-group)
  map the first two letters of the server name to a more friendly and cuddly display name
  (let*  ((match-ok (match-string 2 gnus-tmp-group))
          (server-key (if (null match-ok) nil (upcase(substring match-ok 0 2)))))
    (if (zerop (length server-key))
        gnus-tmp-group
      ;; construct new group format line with a small envelope taking the place of any INBOX
      (concat
       (propertize
        (format "%-8s" (cdr (assoc server-key dkh/server-name-maps)))
        'face (dkh/unread-face "my-group-server-face") 'face (dkh/unread-face (concat "my-group-server-face-" server-key)) 'gnus-face t)
       " - "
       (if (or (string-match "mail.misc" (match-string 3 gnus-tmp-group) )(string-match "INBOX" (match-string 3 gnus-tmp-group) ))
           (propertize "\x2709" 'face (dkh/unread-face "my-inbox-icon-face") 'gnus-face t)
         (propertize (match-string 3 gnus-tmp-group) 'face (dkh/unread-face "my-group-face") 'gnus-face t) )))))


(defun gnus-user-format-function-j (headers)
  ;; prefix each post depending on whether to, cc or Bcc to
  (let ((to (gnus-extra-header 'To headers)))
    (if (string-match dkh-mails to)
        (if (string-match "," to) "~" "»")
      (if (or (string-match dkh-mails
                            (gnus-extra-header 'Cc headers))
              (string-match dkh-mails                                        
                            (gnus-extra-header 'BCc headers)))
          "~"
        " "))))

(defun gnus-user-format-function-y (headers)
  "return string representation for unread articles"
  (concat
   (propertize  (if (= (string-to-number  gnus-tmp-number-of-unread) 0) "" "\x2709") 'face (dkh/unread-face "my-inbox-icon-face") 'gnus-face t)
   (propertize  (if (= (string-to-number  gnus-tmp-number-of-unread) 0) ""
                  (concat "   (" gnus-tmp-number-of-unread ")")) 'face (dkh/unread-face "my-unread-count-face") 'gnus-face t)))



(setq  gnus-user-date-format-alist
       ;; Format the date so we can see today/tomorrow quickly.
       ;; See http://emacs.wordpress.com/category/gnus/ for the original.
       '(
         ((gnus-seconds-today) . "Today, %H:%M")
         ((+ 86400 (gnus-seconds-today)) . "Yesterday, %H:%M")
         (604800 . "%A %H:%M") ;;that's one week
         ((gnus-seconds-month) . "%A %d")
         ((gnus-seconds-year) . "%B %d")
         (t . "%B %d '%y"))) ;;this one is used when no other does match


(defun gnus-group-read-group-no-prompt ()
  "Read news in this newsgroup and don't prompt.
                                Use the value of `gnus-large-newsgroup'."
  (interactive)
  (gnus-group-read-group gnus-large-newsgroup))

(defun gnus-article-sort-by-chars (h1 h2)
  "Sort articles by size."
  (< (mail-header-chars h1)
     (mail-header-chars h2)))

;;             (add-to-list 'message-syntax-checks '(existing-newsgroups . disabled))







;;F6 killfiles a poster, F7 ignores a thread
;;   (define-key gnus-summary-mode-map (kbd "<f6>") "LA")
;;   (define-key gnus-summary-mode-map (kbd "<f7>") 'gnus-summary-kill-thread)
(define-key gnus-summary-mode-map (kbd "<deletechar>") (lambda ()(interactive)(gnus-summary-delete-article)(next-line)))

;; some comfort keys to scroll article in other window when in summary window
(define-key gnus-summary-mode-map [(meta up)] (lambda() (interactive) (scroll-other-window -1)))
(define-key gnus-summary-mode-map [(meta down)] (lambda() (interactive) (scroll-other-window 1)))
;; thread navigation
(define-key gnus-summary-mode-map [(control down)] 'gnus-summary-next-thread)
(define-key gnus-summary-mode-map [(control up)] 'gnus-summary-prev-thread)


(define-key gnus-summary-mode-map (kbd ">") 'gnus-summary-show-thread)
(define-key gnus-summary-mode-map (kbd "<") 'gnus-summary-hide-thread)


;; some trickery to show the newsread people are using and colour code depending on type
;; in this case highlight users of any outlook type dross :-;
(setq  gnus-header-face-alist nil)
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

;; And show any real men who use Gnus!
(add-to-list
 'gnus-header-face-alist
 (list (concat
        "^"
        (regexp-opt '("User-Agent" "X-Mailer" "Newsreader" "X-Newsreader") t)
        ":.*Gnus.*")
       nil 'gnus-server-opened))

;; Format RSS feed titles nicely
(add-hook 'gnus-summary-mode-hook
          (lambda ()
            (if (string-match "^nnrss:.*" gnus-newsgroup-name)
                (progn
                  (make-local-variable 'gnus-show-threads)
                  (make-local-variable 'gnus-article-sort-functions)
                  (make-local-variable 'gnus-use-adaptive-scoring)
                  (make-local-variable 'gnus-use-scoring)
                  (make-local-variable 'gnus-score-find-score-files-function)
                  (setq gnus-show-threads nil)
                  (setq gnus-article-sort-functions 'gnus-article-sort-by-date)
                  (setq gnus-use-adaptive-scoring nil)
                  (setq gnus-use-scoring t)
                  ;;                  (setq gnus-score-find-score-files-function 'gnus-score-find-single)
                  ))))


(add-hook 'gnus-select-group-hook 'gnus-group-set-timestamp)

(defun gnus-user-format-function-d (headers)
  (let ((time (gnus-group-timestamp gnus-tmp-group)))
    (if time
        (format-time-string "%b %d  %H:%M" time)
      ""
      )
    )
  )

(define-key mode-specific-map [?m] (lambda()(interactive) (gnus-agent-toggle-plugged t)(gnus 1)))





(setq gnus-suppress-duplicates t
      gnus-save-duplicate-list t
      gnus-duplicate-list-length 100000)

(remove-hook 'gnus-article-prepare-hook 'bbdb-mua-display-sender)




; (gnus-add-configuration
;  '(article
;    (horizontal 1.0
;                (group 0.25)
;                (vertical 1.0
;                          (summary 0.16 point)
;                          (article 1.0)
;                          ("*BBDB*" 6))
;                )))
; 
; (add-hook 'gnus-summary-exit-hook
;           (lambda ()
;             (when (every (lambda (buffer) (member buffer (gnus-buffers)))
;                          (mapcar 'window-buffer (window-list)))
;               (delete-other-windows)))
;           t nil)

;; Set the window title
                                        ;(modify-frame-parameters nil '((title . "Gnus")))

;; indexing in mail groups supported by dovecot on the server side.
(require 'nnir)

(define-key gnus-group-mode-map (kbd "<H-f1>") 'gnus-group-make-nnir-group)
;; (define-key gnus-group-mode-map (kbd "<C-f3>") 'gnus-group-make-nnir-group)
;;  (define-key gnus-summary-mode-map (kbd "G G") 'command gnus-group-make-nnir-group)

                                        ; (setq nnir-search-engine 'imap)

(defcustom dkh/authinfo-file (expand-file-name(concat user-emacs-directory ".authinfo.gpg"))
  "regexp for searching blogger"
  :group 'dkh/gnus
  :type 'string)

(global-set-key (kbd "C-c x") '(lambda()(interactive)(save-buffers-kill-emacs)))
;; Mark gcc'ed (archive) as read:

;; put everything in ~/.emacs.d
(setq
;; gnus-init-file "~/git/.emacs.d/dkh-gnus.el"
 message-signatrue-directory "~/git/.emacs.d/sig/"
 )

;; (require 'info)
  (if (featurep 'xemacs)
      (add-to-list 'Info-directory-list "~/.emacs.d/el-get/nognus/texi/")
    (add-to-list 'Info-default-directory-list "~/.emacs.d/el-get/nognus/texi/"))

(defcustom gnus-summary-save-parts-exclude-article nil                                                                                                                                                                     
          "If non-nil don't save article along with attachments."                                                                                                                                                                  
          :group 'gnus-article-mime                                                                                                                                                                                                
          :type 'boolean)                                                                                                                                                                                                          
                                                                                                                                                                                                                                   
        (defun gnus-summary-save-parts-1 (type dir handle reverse)                                                                                                                                                                 
          (if (stringp (car handle))                                                                                                                                                                                               
(mapcar (lambda (h) (gnus-summary-save-parts-1 type dir h reverse))                                                                                                                                                  
    (cdr handle))                                                                                                                                                                                                    
            (when (if reverse                                                                                                                                                                                                      
                  (not (string-match type (mm-handle-media-type handle)))                                                                                                                                                          
                (string-match type (mm-handle-media-type handle)))                                                                                                                                                                 
              (let* ((name (or                                                                                                                                                                                                     
                            (mm-handle-filename handle)                                                                                                                                                                            
                            (unless gnus-summary-save-parts-exclude-article                                                                                                                                                        
                              (format "%s.%d.%d" gnus-newsgroup-name                                                                                                                                                               
                                      (cdr gnus-article-current)                                                                                                                                                                   
                                      gnus-summary-save-parts-counter))))                                                                                                                                                          
                     (file (when name                                                                                                                                                                                              
                             (expand-file-name                                                                                                                                                                                     
                              (gnus-map-function                                                                                                                                                                                   
                               mm-file-name-rewrite-functions                                                                                                                                                                      
                               (file-name-nondirectory                                                                                                                                                                             
                                name))                                                                                                                                                                                             
                              dir))))                                                                                                                                                                                              
                (when file                                                                                                                                                                                                         
                  (incf gnus-summary-save-parts-counter)                                                                                                                                                                           
                  (unless (file-exists-p file)                                                                                                                                                                                     
                    (mm-save-part-to-file handle file)))))))

(setq message-cite-prefix-regexp
"\\([ ]*[-_.#[:word:]]+>+\\|[ ]*[]>|}]\\)+")

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
)

(setq gnus-summary-line-format "%«%U%R %uS %ur %»%(%*%-14,14f   %1«%B%s%»%)
 ")

(setq message-kill-buffer-on-exit t)
(setq gnus-fetch-old-headers 'some)



;; Unbind this key; it's annoying!
(define-key gnus-summary-mode-map "o" (lambda () (interactive)))
(setq gnus-article-banner-alist '((iphone . "\\(^Sent from my iPhone$\\)")))

(setq gnus-novice-user nil)




(setq
 epa-file-cache-passphrase-for-symmetric-encryption t
 user-full-name "Damon Haley"
 user-mail-address "damon.haley@colorado.edu"
 )


 (setq
;;  display-time-mail-file "/var/mail/vinylisland"
 ;;
 ;; Personal headers
 ;;
 message-default-headers (concat
                          "X-Face: \"'PJ-yb+fYF0]%?,#==_(s>`~Hw_iwG![Cc+Sq$k>S|QbU)>?}Y51$4)\\9OEt:NL.@kZIqy <UnVZ*!XnGGV:iDO$YDhK7i~$.fs%r^0LJdztkb\\6=DI6by:GdO>.L<,Nd[nsMwrN3b]os1UqBw\n"
                          "X-Accept-Language: en-us\n"
                          "X-Operating-System: Debian GNU/Linux\n"
                          )
 gnus-treat-display-smileys t
 ;; How gnus gets address from the from header.
 gnus-extract-address-components 'mail-extract-address-components
 ;; Buttonize the different parts, please
 gnus-buttonized-mime-types  '("multipart/alternative" "multipart/signed"  "multipart/encrypted")
 ;; But keep buttons for multiple parts
 gnus-inhibit-mime-unbuttonizing t
 gnus-article-skip-boring t
 gnus-expert-user t

 gnus-indent-thread-level 1
 gnus-build-sparse-threads nil

 gnus-use-nocem t
 gnus-use-full-window nil     ;; do not destroy other windows

 )



(setq
 gnus-no-groups-message "No Gnus for Daemon."
 
 mail-imenu-generic-expression '(("Subject"  "^Subject: *\\(.*\\)" 1)
                                 ("Cc"     "^C[Cc]: *\\(.*\\)" 1)
                                 ("To"     "^To: *\\(.*\\)" 1)
                                 ("From"  "^From: *\\(.*\\)" 1))
 ;; whether gnus bothers with faces. It should!
  
 message-forward-ignored-headers "^Content-Transfer-Encoding:\\|^X-Gnus:\\|^To:\\|^Cc:\\|^From"
 message-make-forward-subject-function (quote message-forward-subject-fwd)
 gnus-visual t
)

;;       gnus-permanently-visible-groups "\\.*"
;; all mails should be always displayed in the mailbox

(setq 
 gnus-visible-headers (quote ("^From:" "^Newsgroups:" "^Subject:" "^Date:" "^Followup-To:" "^Reply-To:" "^Organization:" "^Summary:" "^Keywords:" "^To:" "^[BGF]?Cc:" "^Posted-To:" "^Mail-Copies-To:" "^Mail-Followup-To:" "^Apparently-To:" "^Gnus-Warning:" "^Resent-From:" "^X-Sent:" "^User-Agent:"))

 ;; from http://nijino.homelinux.net/emacs/.gnus.el
 gnus-signature-limit 8.0

 
 )

(setq
 gnushush-sender-header (quote none)
 gnushush-user-agent-header (quote none)

 
 
 )

;; From pop sources
(setq nnmail-split-methods 'nnmail-split-fancy)

;; (info "gnus")
;; See (info "(gnus) Splitting in IMAP"), and search for `fancy'
;; there.  The variable is nnimap-split-rule for me.

(setq nnimap-split-methods 'nnmail-split-fancy)

(setq nnmail-split-abbrev-alist
      '((any . "from\\|to\\|cc\\|sender\\|apparently-to\\|resent-from\\|resent-to\\|resent-cc")
        (mail . "mailer-daemon\\|postmaster\\|uucp")
        (to . "to\\|cc\\|apparently-to\\|resent-to\\|resent-cc")
        (from . "from\\|sender\\|resent-from")
        (daemon-errors . "Cron daemon\\|mailer-daemon")
        (list . "list-id\\|x-mailing-list\\|to\\|cc\\|sender")))

(setq

                                        ;      when replying we dont want to include peoples signatures.
 message-cite-function 'message-cite-original-without-signature
 ;;       message-required-news-headers (remove' Message-ID message-required-news-headers)
 ;;       message-required-mail-headers (remove' Message-ID message-required-mail-headers)
 ;; Use emacs-w3m to render html mails and display images
 gnus-mime-display-multipart-related-as-mixed t

 mm-inline-large-images t
 

 mm-external-terminal-program (quote urxvt)

 w3m-w3mkey-binding 'info
 w3m-safe-url-regexp nil
 
 )





;; ---------------------------------------------------------------------

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




(eval-after-load 'gnus-art
  '(progn
     (add-to-list 'gnus-picon-databases "/usr/share/picons")
     ))

(setq gnus-picon-style 'right)



(setq gnus-treat-from-picon 'head)
(setq gnus-treat-display-x-face 'head)



(defun gnus-goto-last-link ()
  "A lot of articles just have one link to goto"
  (interactive)
  (gnus-summary-show-article)
  (gnus-summary-select-article-buffer)
  (goto-char (point-max))
  (forward-line -2)
  (rgr/browse-url)
  (let ((cur (wg-name (wg-current-workgroup)))
        (prev (wg-name (wg-previous-workgroup))))
  (if (string-equal prev "gnus")
    (wg-switch-to-index-1)))
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
  (local-set-key (kbd "<f4>") '(lambda () (interactive)
                                 (gnus-goto-last-link)
                                 ))
    (local-set-key (kbd "z") '(lambda () (interactive)
                                   (gnus-summary-mark-as-read-forward)))
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


(add-to-list 'message-syntax-checks '(existing-newsgroups . disabled))


(provide 'dkh-gnus)

;; Local Variables:
;;   mode: emacs-lisp
;;   mode: allout
;;   outline-regexp: "^;;;_\\([,. ]+\\)"
;; End:

;;; dkh-gnus.el ends here
