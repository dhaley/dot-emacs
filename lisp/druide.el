;;; druide --- Commands to help manipulate diff3 regions





(defun initialize_cu_drupal ()
  "Sets up local and global project variables "
  (interactive)
  (let* ((project-root-dir (locate-dominating-file default-directory
                                                   "current"))
         (path (split-string project-root-dir "/")))     ; path as list
    (setq cu-drupal-site-name          (car (last (nbutlast path 1)))))

  (setq
   cu-drupal-site-directory (file-truename (locate-dominating-file
                                            default-directory
                                            "includes/bootstrap.inc"))
   cu-drupal-readme-file-name (concat cu-drupal-site-directory "README.md")
   cu-drupal-profile-name (curr-dir-project-string
                           cu-drupal-site-directory
                           cu-drupal-site-name)
   cu-drupal-sites-all-directory (concat
                                  cu-drupal-site-directory
                                  "sites/all"))
  ;; if there's a profile dir
  (if (and cu-drupal-profile-name (file-exists-p (concat
                                                  cu-drupal-site-directory
                                                  "profiles/"
                                                  cu-drupal-profile-name)))
      (progn
        (setq
         cu-drupal-profile-directory (concat
                                      cu-drupal-site-directory
                                      "profiles/"
                                      cu-drupal-profile-name)
         cu-drupal-module-directory (concat
                                     cu-drupal-profile-directory
                                     "/modules")
         cu-drupal-theme-directory (concat
                                    cu-drupal-profile-directory
                                    "/themes")
         cu-drupal-profile-theme-directory (concat
                                            cu-drupal-profile-directory
                                            "/themes/"
                                            cu-drupal-profile-name))
        (setenv "8dt" cu-drupal-profile-theme-directory)
        (bind-key "C-8 d t" (lambda()
                              (interactive)
                              (find-file
                               cu-drupal-profile-theme-directory)))

        (setenv "8dp" cu-drupal-profile-directory)
        (bind-key "C-8 d p" (lambda()
                              (interactive)
                              (find-file
                               cu-drupal-profile-directory))))
    (setq
     cu-drupal-module-directory (concat
                                 cu-drupal-sites-all-directory
                                 "/modules")
     cu-drupal-theme-directory (concat
                                cu-drupal-sites-all-directory
                                "/themes")))

  ;; set up the rest of global vars after determining profile parameters
  (setq
   cu-drupal-feature-directory (concat
                                cu-drupal-module-directory
                                "/features")
   cu-drupal-contrib-directory (concat
                                cu-drupal-module-directory
                                "/contrib")
   cu-drupal-custom-directory (concat
                               cu-drupal-module-directory
                               "/custom")
   cu-drupal-default-directory (concat
                                cu-drupal-site-directory "sites/default")
   cu-drupal-settings-file-name (concat
                                 cu-drupal-default-directory
                                 "/settings.php")
   cu-drupal-settings-local-file-name (concat
                                       cu-drupal-default-directory
                                       "/settings.local.php")
   ;; this is for drupal-mode's sake
   drupal-rootdir cu-drupal-site-directory
                                        ; let's set up drush aliases
   cu-drupal-local-alias (concat "@cu.local-" cu-drupal-site-name)
   cu-drupal-dev-alias (concat "@cu.wcustdev1-" cu-drupal-site-name)
   cu-drupal-stage-alias (concat "@cu.wstage1-" cu-drupal-site-name)
   cu-drupal-test-alias (concat "@cu.wcusttest1-" cu-drupal-site-name)
   cu-drupal-prod-alias (concat "@cu.wcust1-" cu-drupal-site-name))



  (setenv "8dr" cu-drupal-readme-file-name)
  (setenv "8ds" cu-drupal-site-directory)
  (setenv "DRUPAL_ROOT" cu-drupal-site-directory)
  (setenv "8dT" cu-drupal-theme-directory)
  (setenv "8dm" cu-drupal-module-directory)
  (setenv "8dc" cu-drupal-custom-directory)
  (setenv "8df" cu-drupal-feature-directory)
  (setenv "8db" cu-drupal-contrib-directory)
  (setenv "8dd" cu-drupal-default-directory)
  (setenv "8da" cu-drupal-sites-all-directory)
  (setenv "8dS" cu-drupal-settings-file-name)
  (setenv "8dl" cu-drupal-settings-local-file-name)

  (bind-key "C-8 d r" (lambda()
                        (interactive)
                        (find-file
                         cu-drupal-readme-file-name)))
  (bind-key "C-8 d s" (lambda()
                        (interactive)(find-file cu-drupal-site-directory)))
  (bind-key "C-8 d T" (lambda()
                        (interactive)(find-file cu-drupal-theme-directory)))

  (bind-key "C-8 d m" (lambda()
                        (interactive)
                        (find-file cu-drupal-module-directory)))
  (bind-key "C-8 d c" (lambda()
                        (interactive)
                        (find-file cu-drupal-custom-directory)))
  (bind-key "C-8 d f" (lambda()
                        (interactive)
                        (find-file cu-drupal-feature-directory)))
  (bind-key "C-8 d d" (lambda()
                        (interactive)
                        (find-file cu-drupal-default-directory)))
  (bind-key "C-8 d a" (lambda()
                        (interactive)
                        (find-file cu-drupal-sites-all-directory)))
  (bind-key "C-8 d S" (lambda()
                        (interactive)
                        (find-file cu-drupal-settings-file-name)))
  (bind-key "C-8 d l" (lambda()
                        (interactive)
                        (find-file
                         cu-drupal-settings-local-file-name)))
  (bind-key "C-8 d b" (lambda()
                        (interactive)
                        (find-file cu-drupal-contrib-directory))))


(provide 'druide)

;;; druide.el ends here
