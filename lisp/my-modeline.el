;;; my-modeline.el --- Personalization of mode-line-stats

;;; Commentary:
;;

(require 'battery)
(require 'network-speed)
(require 'cpu-stats)
(require 'memory-stats)
(require 'misc-stats)

;;; Code:

(setq network-speed-interface-list (if (string= (shell-command-to-string "ifconfig | grep en1") "")
                                           '("en0")
                                         '("en1")))

(display-battery-mode t)
(network-speed-start)
(cpu-usage-start)
(memory-usage-start)

(defvar my-stuff-dir "~/.emacs.d/icons/xpm/")

;; FACES:

(defmacro alias-face (name face)
  "Creates an alias `name' to face `face'."
  `(progn (defface ,name '((default :inherit ,face :slant r))
            "A macro-defined alias face."
            :group 'default)
          (defvar ,name ',name)))

(alias-face my-green-face  font-lock-constant-face)
(alias-face my-yellow-face font-lock-function-name-face)
(alias-face my-red-face    font-lock-warning-face)

(alias-face my-important-face   font-lock-keyword-face)
(alias-face my-unimportant-face font-lock-comment-face)
(alias-face my-note-face        font-lock-doc-face)

(defvar my-battery-status-alist '(("#" "discharging")
                                  ("#+" "charging" my-green-face)
                                  ("#-" "low" my-yellow-face)
                                  ("#!" "critically low" my-red-face)))

(defvar my-vc-alist '((ignored "Ignored" my-unimportant-face)
                      ;; Everything is ok:
                      (up-to-date "Up to date" my-green-face)
                      ;; Kinda important:
                      (unregistered "Unknown" my-yellow-face)
                      (edited "Edited" my-yellow-face)
                      (added "Added" my-yellow-face)
                      ;; Most important:
                      (removed "Scheduled for removal" my-red-face)
                      (conflict "Has conflicts" my-red-face)
                      (missing "Missing" my-red-face)
                      (needs-update "Needs update" my-red-face)
                      (needs-merge "Needs merge" my-red-face)
                      (unlocked-changes "Has unlocked changes" my-red-face)))

(defvar my-load-average-threshold 5.0)

;; IMAGES

(defvar my-images-alist
  '((my-network-load-image . "77-ekg20.xpm")
    (my-cpu-usage-image . "106-sliders20.xpm")
    (my-load-average-image . "17-bar-chart20.xpm")
    (my-battery-status-image . "64-zap20.xpm")
    (my-ram-usage-image . "249-piechart20.xpm")
    (my-uptime-image . "221-recordplayer20.xpm")
    ))


(defvar display-mode-line-images t)

(defun my-create-image (filename)
  (create-image (concat my-stuff-dir filename)
                'xpm nil
                :ascent 'center))

(defun* toggle-mode-line-images (&optional (arg (not display-mode-line-images)))
  "Display various icons in the mode-line."
  (interactive)
  (setq display-mode-line-images arg)
  (if display-mode-line-images
      (mapc (lambda (nf)
              (set (car nf)
                   (my-create-image (cdr nf))))
            my-images-alist)
    (mapc (lambda (i)
            (set (car i) nil))
          my-images-alist)))

(toggle-mode-line-images t)


(provide 'my-modeline)

;;; my-modeline.el ends here
