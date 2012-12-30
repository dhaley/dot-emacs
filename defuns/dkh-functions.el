
(defun git-fit ()
(interactive)
(magit-status 
(getenv "FIT_REPO")))

(defun dkh-eshell-macs ()
  (interactive)
  "Creates a tool config shell and switches to it. If a buffer with name already exists, we simply switch to it."
  (let ((buffer-of-name (get-buffer (concat "*eshell-" (wg-name (wg-current-workgroup)) "-tool-config*"))))
    (cond ((bufferp buffer-of-name) ;If the buffer exists, switch to it (assume it is a shell)
           (switch-to-buffer buffer-of-name))
          ( t 
            (progn
              (eshell t)
              ;(process-send-string (get-buffer-process new-buff-name) (concat "cd " localdir "\n"))
              (rename-buffer  (concat "*eshell-" (wg-name (wg-current-workgroup)) "-tool-config*")))))))

(defun visit-ansi-term ()
  (interactive)
  "Creates an ansi-term and switches to it. If a buffer with name already exists, we simply switch to it."
  (let ((buffer-of-name (get-buffer (concat "*ansi-term-" (wg-name (wg-current-workgroup)))))
;;        (default-directory "/home/www")
        (term-cmd "/bin/zsh")
)
    (cond ((bufferp buffer-of-name) ;If the buffer exists, switch to it (assume it is a shell)
           (switch-to-buffer buffer-of-name))
          ( t 
            (progn
              (ansi-term term-cmd)
              ;(process-send-string (get-buffer-process new-buff-name) (concat "cd " localdir "\n"))
              (rename-buffer  (concat "*ansi-term-" (wg-name (wg-current-workgroup)))))))))

(defun grab-email-my ()
  "Grab the next email in the buffer
  First posted by François Fleuret <francois.fleuret@inria.fr>..
improved by many.."
  (interactive)
  (re-search-forward "[^ \t\n]+@[^ \t\n]+")
  (copy-region-as-kill (match-beginning 0) (match-end 0))
  )