;;(autoload 'w3m-browse-url "w3m" nil t)
;;(autoload 'browse-url-interactive-arg "browse-url")

(require 'browse-apropos-url)
(require 'browse-url)
(require 'thingatpt+)
(require 'w3m)
;; 
;; (defun rgr/browse (url)
;;   "If prefix is specified use the system default browser else use the configured emacs one"
;;   (if current-prefix-arg
;;       (if  url 
;;           (w3m-browse-url url) 
;;         (call-interactively 'browse-url))
;;     (when url (browse-url-generic url))
;;     ))
;; 
;; ;; (defun rgr/browse-url (&optional url)
;; ;;   "browse the url passed in"
;; ;;   (interactive)
;; ;;   (setq url (or url (w3m-url-valid (w3m-anchor)) (browse-url-url-at-point) (region-or-word-at-point)))
;; ;;   (setq url (read-string (format "Url \"%s\" :" url) url nil url))
;; ;;   (rgr/browse url))
;; 
;; 
;; 
;; ;; (defun rgr/browse-apropos-url (apropos-prefix prompt)
;; ;;   (interactive)
;; ;;   (let* ((default (region-or-word-at-point))
;; ;;          )
;; ;;     (setq default (read-string (format prompt default) default nil default))
;; ;;     (browse-apropos-url  (concat apropos-prefix " " default) nil current-prefix-arg)))
;; 
;;

;;browse-url-generic-program
(setq browse-url-default-browser "conkeror")
;; w3m-view-this-url
;; w3m-view-url-with-external-browser

;; browse-url-browser-function
;; browse-url-default-browser

(defun rgr/browse (url)
  "If prefix is specified use the system default browser else use the configured emacs one"
  (if current-prefix-arg
      (if  url
          (w3m-browse-url url)
        (call-interactively 'browse-url))
;;    (when url (browse-url-generic url))
    (when url (browse-url-default-browser url))
    (message (concat "foo" url))
    ))

(defun rgr/browse-url (&optional url)
  "browse the url passed in"
  (interactive)
  (setq url (or url (w3m-url-valid (w3m-anchor)) (browse-url-url-at-point) (region-or-word-at-point)))
  (setq url (read-string (format "Url \"%s\" :" url) url nil url))
  (rgr/browse url))

(global-set-key (kbd "<f4>") 'rgr/browse-url)




(defun rgr/browse (url)
  "If prefix is specified use the system default browser else use the configured emacs one"
  (if current-prefix-arg
      (when url (browse-url-generic url))
    (if  url (browse-url url) (call-interactively 'browse-url))))

(defun rgr/browse-url (&optional url)
  "browse the url passed in"
  (interactive)
  (setq url (or url (w3m-url-valid (w3m-anchor)) (browse-url-url-at-point) (region-or-word-at-point)))
  (setq url (read-string (format "Url \"%s\" :" url) url nil url))
  (rgr/browse url))
;; 
;; 
;; 
;; (defun rgr/google-search-prompt (&optional default)
;;   (interactive)
;;   (let* ((default (or default (region-or-word-at-point)))
;;          (term (read-string (format "Google.com the web for the following phrase (%s): "
;;                                     default) nil  nil default)))
;;     (rgr/browse (concat "http://www.google.com/search?q=" ; borrowed from dim
;;                         (replace-regexp-in-string 
;;                          "[[:space:]]+"
;;                          "+"
;;                          term)))))
;; 
;; ; google keys and url keys
;; ; prefix (ctl-u) to use external browser.
;; (global-set-key (kbd "<f4>") 'rgr/browse-url)
;; (global-set-key (kbd "<f3>") 'rgr/google-search-prompt)

(provide 'rgr-web)


;; Local Variables:
;;   mode: emacs-lisp
;;   mode: allout
;;   outline-regexp: "^;;;_\\([,. ]+\\)"
;; End:

;;; dkh-gnus.el ends here
