
(require 'php+-mode)
(php+-mode-setup)

(setq php-manual-path "~/git/.emacs.d/php/php-chunked-xhtml/")

(setq php-completion-file "~/git/ewax/misc/php-completion-file")

(define-derived-mode drupal-mode php+-mode "Drupal"
  "Major mode for Drupal source code.
\\{drupal-mode-map}"
  (setq case-fold-search t) 
  (setq indent-tabs-mode nil)
  (setq c-basic-offset 2)
  (setq indent-tabs-mode nil)
  (setq tab-width 2)
  (setq fill-column 78)
  (c-set-offset 'arglist-cont 0)
  (c-set-offset 'arglist-intro '+)
  (c-set-offset 'case-label 2)
  (c-set-offset 'arglist-close 0)
  (setq yas/buffer-local-condition 
  '(cond
   ((looking-at "\\w") nil)
   ((and
     (not (bobp))
     (or (equal "font-lock-comment-face"
                (get-char-property (1- (point)) 'face))
         (equal "font-lock-string-face"
                (get-char-property (1- (point)) 'face))))
    '(require-snippet-condition . force-in-comment))
   (t t))))
(define-key drupal-mode-map (kbd "TAB") 'indent-according-to-mode)
(add-hook 'drupal-mode-hook (lambda () (flymake-mode 1)))
(add-hook 'drupal-mode-hook (lambda () (yas/minor-mode 1)))
(add-to-list 'auto-mode-alist '("\\.\\(php\\|test\\|module\\|inc\\|install\\|engine\\|profile\\|.theme\\)$" . drupal-mode))
(add-to-list 'auto-mode-alist '("\\.tpl.php$" . html-helper-mode))
(define-key drupal-mode-map '[M-S-up] 'flymake-goto-prev-error)
(define-key drupal-mode-map '[M-S-down] 'flymake-goto-next-error)
(define-key drupal-mode-map (kbd "C-c C-c") 'comment-dwim)

(defun sacha/drupal-module-name ()
  "Return the Drupal module name for .module and .install files."
  (file-name-sans-extension (file-name-nondirectory
                             (buffer-file-name))))
(add-to-list 'hs-special-modes-alist '(drupal-mode "{" "}" "/[*/]" nil hs-c-like-adjust-block-beginning))

(add-to-list 'load-path "~/.emacs.d/src/drupal-mode/")
(require 'drupal-drush-commands)

(message "0 dkh-php... Done")
