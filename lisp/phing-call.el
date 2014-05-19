;;; phing-call.el --- Interact with phing

(defun phing-call (target)
  "Call Phing's build.xml"

  ;; Ask for target to execute
  (interactive "MPhing Target: ")

  ;; Open a new *phing-compilation* buffer & kill the old one
  (let ((oldbuf (get-buffer "*phing-compilation*")))
    (if (not (null oldbuf))
        (kill-buffer "*phing-compilation*")))

  ;; Execute thing
  (let* ((buildfile (phing-file-search-upward (file-name-directory (buffer-file-name)) "build.xml"))
         (outbuf (get-buffer-create "*phing-compilation*"))
         (curbuf (current-buffer)))

    ;; Switch to new compilation window
    (switch-to-buffer-other-window outbuf)

    ;; Display command line
    (insert "#> phing -f " buildfile " " target "\n")

    ;; Switch back to called window
    (switch-to-buffer-other-window curbuf)

    ;; Call phing with no ANSI formatting
    (call-process "phing" nil outbuf t "-logger" "phing.listener.DefaultLogger" "-f" buildfile target)))

(defun phing-file-search-upward (directory file)
  "Search for a file in a directory. If not found, will search in the directories above"
  (let* ((updir (file-truename (concat (file-name-directory directory) "../")))
         (curfd (if (not (string= (substring directory (- (length directory) 1)) "/"))
                    (concat directory "/" file)
                  (concat directory file))))
    (if (file-exists-p curfd)
        curfd
      (if (and (not (string= (file-truename directory) updir))
               (< (length updir) (length (file-truename directory))))
          (phing-file-search-upward updir file)
        nil))))


(provide 'phing-call)
