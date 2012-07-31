
(require 'php+-mode)
(php+-mode-setup)

(setq php-manual-path "~/git/.emacs.d/php/php-chunked-xhtml/")

(setq php-completion-file "~/git/ewax/misc/php-completion-file")

;; Start Drupal IDE with `drupal-ide' command:
(setq drupal-ide-load-path (concat user-emacs-directory "drupal/drupal-init.el"))
(autoload 'drupal-ide drupal-ide-load-path "Start IDE for PHP & Drupal development" t)

(message "0 dkh-php... Done")
