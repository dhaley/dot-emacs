;;(autoload 'w3m-browse-url "w3m" nil t)
;;(autoload 'browse-url-interactive-arg "browse-url")

;;(use-package browse-apropos-url)
(use-package browse-url)
(require 'thingatpt+)
(use-package w3m)

(defun rgr/browse (url)
  "If prefix is specified use the system default browser else use the configured emacs one"
  (if current-prefix-arg
      (if  url
          (w3m-browse-url url)
        (call-interactively 'browse-url))
    (when url (browse-url-default-browser url))
    (message (concat "foo" url))
    ))

(defun rgr/browse-url (&optional url)
  "browse the url passed in"
  (interactive)
  (setq url (or url (w3m-url-valid (w3m-anchor)) (browse-url-url-at-point) (region-or-word-at-point)))
  (setq url (read-string (format "Url \"%s\" :" url) url nil url))
  (rgr/browse url))


(provide 'rgr-web)


;; Local Variables:
;;   mode: emacs-lisp
;;   mode: allout
;;   outline-regexp: "^;;;_\\([,. ]+\\)"
;; End:

;;; dkh-gnus.el ends here
