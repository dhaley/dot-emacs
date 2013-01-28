;;; focused-buffer-mode.el --- Large text, less distractions


;;; Commentary:
;;

;;; Code:

(defun fullscreen (&optional arg)
  "Toggle fullscreen.
Currently only works under X."
  (interactive)
  (when (and (featurep 'x) (window-system))
    (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                           (list (cond ((or (not arg) (eq arg 0)) 2)
                                       ((> arg 0) 1)
                                       ((< arg 0) 0 )) "_NET_WM_STATE_FULLSCREEN" 0))))

(defun focused-buffer-mode-adjust-centering ()
  "."
  (interactive)
  ;; do re-centering
  (set-window-fringes nil 16 16 nil)
  (set-window-margins nil 0 0)

  (let* ((max-width
          (if (bound-and-true-p focused-buffer-mode-max-width)
              focused-buffer-mode-max-width 80))
         (w (window-width))
         (m (/ (- w  max-width) 2)))
    (set-window-margins nil m m)
    (setq fill-column max-width)))

;; TODO: minor mode
;; Is not done yet, has bugs.
(defun focused-buffer-mode (&optional arg max-width)
  "Auto set margins."
  (interactive)
  (unless (boundp 'focused-buffer-mode-enabled)
    (set (make-local-variable 'focused-buffer-mode-enabled) nil))
  (set (make-local-variable 'focused-buffer-mode-max-width) max-width)
  (cond ((or (eq arg 1)
            (and (or (not arg) (eq arg 0)) (not focused-buffer-mode-enabled)))
         ;; setup
         (set-face-attribute 'default (selected-frame)  :height 135)
         (set-face-attribute 'fringe (selected-frame)
                             :background (frame-parameter nil 'background-color))
         (with-no-warnings
           (when (and
                  (boundp 'rainbow-delimiters-mode)
                  rainbow-delimiters-mode)
             (rainbow-delimiters-mode -1))
           (when  (and
                   (boundp 'yascroll-bar-mode)
                   yascroll-bar-mode)
             (yascroll-bar-mode -1)))
         (setq mode-line-format nil
               header-line-format nil
               line-spacing 5)
         (add-hook 'window-configuration-change-hook
                   'focused-buffer-mode-adjust-centering t t)
         (fullscreen 1)
         (delete-other-windows)
         (setq focused-buffer-mode-enabled t))
        (t
         (remove-hook 'window-configuration-change-hook
                      'focused-buffer-mode-adjust-centering t)
         (and (boundp 'yascroll:enabled-window-systems)
            (yascroll-bar-mode 1))
         (set-window-fringes nil 8 8 nil)
         (set-window-margins nil 0 0)
         (set-face-attribute 'default (selected-frame)  :height 100)
         (setq line-spacing 0
               focused-buffer-mode-enabled nil)
         (fullscreen -1)
         (message "save-buffer +  revert-buffer + reload theme to restore looks."))))

(provide 'focused-buffer-mode)

;;; focused-buffer-mode.el ends here
