;;; drupal-spell-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "drupal-spell" "drupal-spell.el" (21752 54485
;;;;;;  0 0))
;;; Generated autoloads from drupal-spell.el

(autoload 'drupal-spell-enable "drupal-spell" "\
Enable Drupal dictionary as aspell extra dictionary in current buffer.

\(fn)" t nil)

(add-hook 'drupal-mode-hook 'drupal-spell-enable)

;;;***

;;;### (autoloads nil nil ("drupal-spell-pkg.el") (21752 54485 397683
;;;;;;  0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; drupal-spell-autoloads.el ends here
