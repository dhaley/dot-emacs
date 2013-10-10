;;; awesome-button --- toggle to and from the *shell* buffer

;; http://blog.makezine.com/2011/04/08/the-awesome-button/

(require 'random-quote)

(defun get-awesome-button ()
  " Insert random string from .quotes file"
  (interactive)
  (let ((random-quote (pick-random-quote)))
    random-quote))

(provide 'awesome-button)

;;; awesome-button.el ends here
