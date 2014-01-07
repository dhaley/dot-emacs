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
   cu-drupal-dev-alias (concat "@cu.wwebdev1-" cu-drupal-site-name)
   cu-drupal-stage-alias (concat "@cu.wstage1-" cu-drupal-site-name)
   cu-drupal-test-alias (concat "@cu.wwebtest1-" cu-drupal-site-name)
   cu-drupal-prod-alias (concat "@cu.wweb1-" cu-drupal-site-name))

  (add-to-list 'projectile-globally-ignored-directories
               (concat
                "profiles/"
                cu-drupal-profile-name
                "/modules/contrib"))

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
                        (find-file cu-drupal-contrib-directory)))
    ;; browse drupal menus from emacs
    (setq
     base_url (concat "http://ww/" cu-drupal-site-name)
     d7-menus '("admin" "batch" "gsearch" "home" "navigation404" "node"
    "rss.xml" "shortcodes" "user" "block/%" "degree/%" "media/%" "node/%"
    "overlay-ajax/%" "user/%" "wysiwyg/%" "about-us/fast-facts"
    "about-us/people" "admin/appearance" "admin/compact" "admin/config"
    "admin/content" "admin/help" "admin/index" "admin/modules" "admin/people"
    "admin/reports" "admin/settings" "admin/structure" "admin/tasks"
    "admin/version" "admin/workbench" "block/add" "context-ui/activate"
    "context-ui/deactivate" "devel/arguments" "devel/elements" "devel/explain"
    "devel/php" "devel/phpinfo" "devel/reference" "devel/reinstall"
    "devel/run-cron" "devel/session" "devel/settings" "devel/switch"
    "devel/variable" "faculty-research/academic-program-areas"
    "faculty-research/areas-of-expertise" "faculty-research/faculty-profiles"
    "field_collection/ajax" "file/ajax" "file/progress" "filter/tips"
    "imagecrop/generate_image" "media/browser" "node/add"
    "overlay/dismiss-message" "quicktabs/ajax" "system/ajax" "system/files"
    "system/temporary" "system/timezone" "taxonomy/autocomplete"
    "token/flush-cache" "token/tree" "toolbar/toggle" "user/autocomplete"
    "user/login" "user/logout" "user/password" "user/register" "views/ajax"
    "workbench_access/taxonomy_autocomplete" "block/%/delete" "block/%/devel"
    "block/%/edit" "block/%/revisions" "block/%/view" "media/%/delete"
    "media/%/edit" "media/%/format-form" "media/%/multiedit" "media/%/view"
    "node/%/attachment" "node/%/delete" "node/%/devel" "node/%/display"
    "node/%/done" "node/%/edit" "node/%/revisions" "node/%/submissions"
    "node/%/view" "node/%/webform" "node/%/webform-results" "user/%/cancel"
    "user/%/devel" "user/%/display" "user/%/edit" "user/%/follow"
    "user/%/roles" "user/%/sections" "user/%/shortcuts" "user/%/view"
    "ctools/autocomplete/%" "features/ajaxcallback/%"
    "field-collection/field-callouts/%"
    "field-collection/field-person-program-area/%"
    "field-collection/field-profile-program-area/%"
    "field-collection/field-qa-collection/%" "field-collection/field-qa/%"
    "field-collection/field-slider-slide/%"
    "field-collection/field-social-links-collection/%" "taxonomy/term/%"
    "token/autocomplete/%" "vef/load/%" "video_filter/dashboard/%"
    "about-us/calendar/past-events" "about-us/news/videos"
    "about/equity-diversity/profiles" "admin/appearance/default"
    "admin/appearance/disable" "admin/appearance/enable"
    "admin/appearance/list" "admin/appearance/settings" "admin/config/content"
    "admin/config/date" "admin/config/development" "admin/config/media"
    "admin/config/people" "admin/config/regional" "admin/config/search"
    "admin/config/services" "admin/config/system"
    "admin/config/user-interface" "admin/config/workbench"
    "admin/config/workflow" "admin/content/blocks" "admin/content/files"
    "admin/content/media" "admin/content/node" "admin/content/webform"
    "admin/help/block" "admin/help/blocktheme" "admin/help/captcha"
    "admin/help/color" "admin/help/context" "admin/help/contextual"
    "admin/help/context_ui" "admin/help/date" "admin/help/devel"
    "admin/help/diff" "admin/help/environment_indicator" "admin/help/features"
    "admin/help/field" "admin/help/field_collection"
    "admin/help/field_sql_storage" "admin/help/field_ui" "admin/help/file"
    "admin/help/file_entity" "admin/help/filter" "admin/help/fitvids"
    "admin/help/flexslider" "admin/help/flexslider_views_slideshow"
    "admin/help/follow" "admin/help/globalredirect"
    "admin/help/googleanalytics" "admin/help/google_appliance"
    "admin/help/help" "admin/help/image" "admin/help/ldap_authentication"
    "admin/help/ldap_servers" "admin/help/list" "admin/help/manualcrop"
    "admin/help/maxlength" "admin/help/media" "admin/help/menu"
    "admin/help/menu_block" "admin/help/menu_breadcrumb" "admin/help/node"
    "admin/help/number" "admin/help/options" "admin/help/overlay"
    "admin/help/path" "admin/help/pathauto" "admin/help/quicktabs"
    "admin/help/recaptcha" "admin/help/redirect" "admin/help/role_delegation"
    "admin/help/shortcut" "admin/help/stringoverrides" "admin/help/strongarm"
    "admin/help/syslog" "admin/help/system" "admin/help/taxonomy"
    "admin/help/text" "admin/help/token" "admin/help/toolbar"
    "admin/help/user" "admin/help/views_accordion"
    "admin/help/views_slideshow" "admin/help/views_slideshow_cycle"
    "admin/help/webform" "admin/help/wysiwyg" "admin/modules/list"
    "admin/modules/uninstall" "admin/people/create" "admin/people/people"
    "admin/people/permissions" "admin/people/roles" "admin/reports/fields"
    "admin/reports/status" "admin/reports/varnish"
    "admin/reports/views-plugins" "admin/structure/block"
    "admin/structure/block-types" "admin/structure/context"
    "admin/structure/dependencies" "admin/structure/ds"
    "admin/structure/features" "admin/structure/field-collections"
    "admin/structure/menu" "admin/structure/quicktabs"
    "admin/structure/taxonomy" "admin/structure/types" "admin/structure/views"
    "admin/workbench/content" "admin/workbench/create"
    "admin/workbench/sections" "alumni/alumni-recognition/alumni-profiles"
    "block/add/articles" "block/add/block"
    "block/add/cu-events-calendar-block" "block/add/events-calendar-grid"
    "block/add/facebook-activity" "block/add/facebook-like-button"
    "block/add/feature-callout" "block/add/quicktab" "block/add/slider"
    "block/add/social-links" "block/add/twitter-block"
    "comment/%comment/devel" "devel/cache/clear" "devel/entity/info"
    "devel/field/info" "devel/menu/item" "devel/menu/reset"
    "devel/theme/registry" "features/autocomplete/packages"
    "media/browser/library" "media/browser/list" "media/browser/testbed"
    "node/add/article" "node/add/class-note" "node/add/faqs"
    "node/add/fast-fact" "node/add/file" "node/add/graduate-program"
    "node/add/page" "node/add/past-event" "node/add/person"
    "node/add/photo-gallery" "node/add/profile" "node/add/video"
    "node/add/webform" "email/%/%/%" "block/%/revisions/%" "node/%/publish/%"
    "node/%/submission/%" "node/%/unpublish/%" "block/%/devel/load"
    "block/%/devel/render" "node/%/attachment/newest" "node/%/devel/load"
    "node/%/devel/render" "node/%/devel/token" "node/%/revisions/list"
    "node/%/revisions/view" "node/%/webform-results/analysis"
    "node/%/webform-results/clear" "node/%/webform-results/download"
    "node/%/webform-results/submissions" "node/%/webform-results/table"
    "node/%/webform/components" "node/%/webform/configure"
    "node/%/webform/emails" "user/%/devel/load" "user/%/devel/render"
    "user/%/devel/token" "user/%/edit/account" "imagecrop/overview/%/%"
    "workbench_access/autocomplete/%/%"
    "field-collection/field-callouts/%/delete"
    "field-collection/field-callouts/%/edit"
    "field-collection/field-callouts/%/view"
    "field-collection/field-person-program-area/%/delete"
    "field-collection/field-person-program-area/%/edit"
    "field-collection/field-person-program-area/%/view"
    "field-collection/field-profile-program-area/%/delete"
    "field-collection/field-profile-program-area/%/edit"
    "field-collection/field-profile-program-area/%/view"
    "field-collection/field-qa-collection/%/delete"
    "field-collection/field-qa-collection/%/edit"
    "field-collection/field-qa-collection/%/view"
    "field-collection/field-qa/%/delete" "field-collection/field-qa/%/edit"
    "field-collection/field-qa/%/view"
    "field-collection/field-slider-slide/%/delete"
    "field-collection/field-slider-slide/%/edit"
    "field-collection/field-slider-slide/%/view"
    "field-collection/field-social-links-collection/%/delete"
    "field-collection/field-social-links-collection/%/edit"
    "field-collection/field-social-links-collection/%/view"
    "taxonomy/term/%/devel" "taxonomy/term/%/display" "taxonomy/term/%/edit"
    "taxonomy/term/%/feed" "taxonomy/term/%/view" "admin/content/book/%"
    "admin/structure/features/%" "admin/structure/field-collections/%"
    "admin/structure/taxonomy/%" "devel/variable/edit/%"
    "system/files/styles/%" "webform/ajax/options/%"
    "admin/appearance/settings/α" "admin/appearance/settings/bartik"
    "admin/appearance/settings/boxy" "admin/appearance/settings/canvas"
    "admin/appearance/settings/color_palette"
    "admin/appearance/settings/cu_backstretch"
    "admin/appearance/settings/cu_bootstrap"
    "admin/appearance/settings/cu_omega"
    "admin/appearance/settings/elegant_black"
    "admin/appearance/settings/garland" "admin/appearance/settings/global"
    "admin/appearance/settings/minimal_white"
    "admin/appearance/settings/ninesixty" "admin/appearance/settings/omega"
    "admin/appearance/settings/rubik" "admin/appearance/settings/seven"
    "admin/appearance/settings/sleek" "admin/appearance/settings/soe"
    "admin/appearance/settings/stark"
    "admin/appearance/settings/starterkit_alpha_xhtml"
    "admin/appearance/settings/starterkit_omega_html5"
    "admin/appearance/settings/starterkit_omega_xhtml"
    "admin/appearance/settings/tao" "admin/config/content/diff"
    "admin/config/content/email" "admin/config/content/fences"
    "admin/config/content/formats" "admin/config/content/publishcontent"
    "admin/config/content/webform" "admin/config/content/wysiwyg"
    "admin/config/date/date_popup" "admin/config/development/devel"
    "admin/config/development/environment_indicator"
    "admin/config/development/logging" "admin/config/development/maintenance"
    "admin/config/development/performance"
    "admin/config/development/strongarm" "admin/config/development/varnish"
    "admin/config/media/browser" "admin/config/media/colorbox"
    "admin/config/media/file-system" "admin/config/media/file-types"
    "admin/config/media/fitvids" "admin/config/media/flexslider"
    "admin/config/media/image-styles" "admin/config/media/image-toolkit"
    "admin/config/media/imagecrop" "admin/config/media/rebuild_types"
    "admin/config/media/vef_video_styles" "admin/config/people/accounts"
    "admin/config/people/captcha" "admin/config/people/ip-blocking"
    "admin/config/people/ldap" "admin/config/people/secure_permissions"
    "admin/config/regional/date-time" "admin/config/regional/settings"
    "admin/config/regional/stringoverrides" "admin/config/search/clean-urls"
    "admin/config/search/path" "admin/config/search/redirect"
    "admin/config/services/follow" "admin/config/services/rss-publishing"
    "admin/config/system/actions" "admin/config/system/cron"
    "admin/config/system/cu_alerts" "admin/config/system/globalredirect"
    "admin/config/system/googleanalytics" "admin/config/system/securepages"
    "admin/config/system/site-information"
    "admin/config/user-interface/blocktheme"
    "admin/config/user-interface/boxes"
    "admin/config/user-interface/client_ui_control"
    "admin/config/user-interface/menu-block"
    "admin/config/user-interface/menu-breadcrumb"
    "admin/config/user-interface/modulefilter"
    "admin/config/user-interface/shortcut" "admin/config/workbench/access"
    "admin/content/media/import" "admin/modules/list/confirm"
    "admin/modules/uninstall/confirm" "admin/people/permissions/list"
    "admin/people/permissions/roles" "admin/people/roles/edit"
    "admin/reports/fields/list" "admin/reports/fields/views-fields"
    "admin/reports/status/php" "admin/reports/status/rebuild"
    "admin/reports/status/run-cron" "admin/structure/block-types/add"
    "admin/structure/block-types/list" "admin/structure/block/add"
    "admin/structure/block/add-menu-block"
    "admin/structure/block/delete-menu-block" "admin/structure/context/add"
    "admin/structure/context/import" "admin/structure/context/list"
    "admin/structure/context/settings" "admin/structure/dependencies/bean"
    "admin/structure/dependencies/field_collection_item"
    "admin/structure/dependencies/file" "admin/structure/dependencies/node"
    "admin/structure/dependencies/overview"
    "admin/structure/dependencies/taxonomy_term"
    "admin/structure/dependencies/user" "admin/structure/ds/change-layout"
    "admin/structure/ds/list" "admin/structure/ds/revert-layout"
    "admin/structure/features/cleanup" "admin/structure/features/create"
    "admin/structure/features/features_override"
    "admin/structure/features/manage" "admin/structure/features/settings"
    "admin/structure/menu/add" "admin/structure/menu/list"
    "admin/structure/menu/parents" "admin/structure/menu/settings"
    "admin/structure/quicktabs/add" "admin/structure/quicktabs/list"
    "admin/structure/quicktabs/styles" "admin/structure/taxonomy/add"
    "admin/structure/taxonomy/list" "admin/structure/types/add"
    "admin/structure/types/list" "admin/structure/views/add"
    "admin/structure/views/add-template" "admin/structure/views/import"
    "admin/structure/views/list" "admin/structure/views/settings"
    "admin/workbench/content/all" "admin/workbench/content/edited"
    "comment/%comment/devel/load" "comment/%comment/devel/render"
    "comment/%comment/devel/token" "ctools/context/ajax/add"
    "ctools/context/ajax/configure" "ctools/context/ajax/delete"
    "block/%/revisions/%/delete" "block/%/revisions/%/edit"
    "block/%/revisions/%/set-active" "block/%/revisions/%/view"
    "node/%/revisions/%/delete" "node/%/revisions/%/revert"
    "node/%/revisions/%/view" "node/%/submission/%/delete"
    "node/%/submission/%/edit" "node/%/submission/%/resend"
    "node/%/submission/%/view" "node/%/webform-results/analysis/%"
    "node/%/webform/components/%" "node/%/webform/emails/%"
    "node/%/revisions/view/latest" "imagecrop/crop/%/%/%"
    "node_reference/autocomplete/%/%/%" "user/reset/%/%/%"
    "user_reference/autocomplete/%/%/%" "taxonomy/term/%/%/feed"
    "field-collection/field-callouts/%/revisions/%"
    "field-collection/field-person-program-area/%/revisions/%"
    "field-collection/field-profile-program-area/%/revisions/%"
    "field-collection/field-qa-collection/%/revisions/%"
    "field-collection/field-qa/%/revisions/%"
    "field-collection/field-slider-slide/%/revisions/%"
    "field-collection/field-social-links-collection/%/revisions/%"
    "taxonomy/term/%/devel/load" "taxonomy/term/%/devel/render"
    "taxonomy/term/%/devel/token" "field-collection/field-callouts/add/%/%"
    "field-collection/field-person-program-area/add/%/%"
    "field-collection/field-profile-program-area/add/%/%"
    "field-collection/field-qa-collection/add/%/%"
    "field-collection/field-qa/add/%/%"
    "field-collection/field-slider-slide/add/%/%"
    "field-collection/field-social-links-collection/add/%/%"
    "admin/structure/features/%/diff" "admin/structure/features/%/recreate"
    "admin/structure/features/%/status" "admin/structure/features/%/view"
    "admin/structure/field-collections/%/dependencies"
    "admin/structure/field-collections/%/display"
    "admin/structure/field-collections/%/fields"
    "admin/structure/taxonomy/%/add" "admin/structure/taxonomy/%/auto_label"
    "admin/structure/taxonomy/%/dependencies"
    "admin/structure/taxonomy/%/display" "admin/structure/taxonomy/%/edit"
    "admin/structure/taxonomy/%/fields" "admin/structure/taxonomy/%/list"
    "admin/config/content/formats/%" "admin/config/user-interface/shortcut/%"
    "admin/structure/block-types/manage/%" "admin/structure/context/list/%"
    "admin/structure/dependencies/delete/%"
    "admin/structure/dependencies/edit/%" "admin/structure/menu/manage/%"
    "admin/structure/quicktabs/manage/%" "admin/structure/types/manage/%"
    "admin/structure/views/view/%" "sites/default/files/styles/%"
    "admin/config/content/diff/entities" "admin/config/content/diff/fields"
    "admin/config/content/diff/settings" "admin/config/content/formats/add"
    "admin/config/content/formats/list" "admin/config/content/wysiwyg/list"
    "admin/config/development/varnish/general"
    "admin/config/media/flexslider/add"
    "admin/config/media/flexslider/advanced"
    "admin/config/media/flexslider/export"
    "admin/config/media/flexslider/import"
    "admin/config/media/flexslider/list" "admin/config/media/image-styles/add"
    "admin/config/media/image-styles/list"
    "admin/config/media/vef_video_styles/add"
    "admin/config/media/vef_video_styles/import"
    "admin/config/media/vef_video_styles/list"
    "admin/config/people/accounts/dependencies"
    "admin/config/people/accounts/display"
    "admin/config/people/accounts/fields"
    "admin/config/people/accounts/settings"
    "admin/config/people/captcha/captcha"
    "admin/config/people/captcha/recaptcha"
    "admin/config/people/ldap/authentication"
    "admin/config/people/ldap/servers" "admin/config/people/ldap/settings"
    "admin/config/people/secure_permissions/export"
    "admin/config/people/secure_permissions/view"
    "admin/config/regional/date-time/formats"
    "admin/config/regional/date-time/types"
    "admin/config/search/clean-urls/check"
    "admin/config/search/google_appliance/settings"
    "admin/config/search/path/add" "admin/config/search/path/delete_bulk"
    "admin/config/search/path/list" "admin/config/search/path/patterns"
    "admin/config/search/path/settings" "admin/config/search/path/update_bulk"
    "admin/config/search/redirect/add" "admin/config/search/redirect/list"
    "admin/config/search/redirect/settings"
    "admin/config/system/actions/configure"
    "admin/config/system/actions/manage" "admin/config/system/actions/orphan"
    "admin/config/user-interface/client_ui_control/settings"
    "admin/config/user-interface/shortcut/add-set"
    "admin/config/workbench/access/editors"
    "admin/config/workbench/access/install"
    "admin/config/workbench/access/roles"
    "admin/config/workbench/access/sections"
    "admin/config/workbench/access/settings"
    "admin/structure/block/box-add/simple" "admin/structure/block/demo/α"
    "admin/structure/block/demo/bartik" "admin/structure/block/demo/boxy"
    "admin/structure/block/demo/canvas"
    "admin/structure/block/demo/color_palette"
    "admin/structure/block/demo/cu_backstretch"
    "admin/structure/block/demo/cu_bootstrap"
    "admin/structure/block/demo/cu_omega"
    "admin/structure/block/demo/elegant_black"
    "admin/structure/block/demo/garland"
    "admin/structure/block/demo/minimal_white"
    "admin/structure/block/demo/ninesixty" "admin/structure/block/demo/omega"
    "admin/structure/block/demo/rubik" "admin/structure/block/demo/seven"
    "admin/structure/block/demo/sleek" "admin/structure/block/demo/soe"
    "admin/structure/block/demo/stark"
    "admin/structure/block/demo/starterkit_alpha_xhtml"
    "admin/structure/block/demo/starterkit_omega_html5"
    "admin/structure/block/demo/starterkit_omega_xhtml"
    "admin/structure/block/demo/tao" "admin/structure/block/list/α"
    "admin/structure/block/list/bartik" "admin/structure/block/list/boxy"
    "admin/structure/block/list/canvas"
    "admin/structure/block/list/color_palette"
    "admin/structure/block/list/cu_backstretch"
    "admin/structure/block/list/cu_bootstrap"
    "admin/structure/block/list/cu_omega"
    "admin/structure/block/list/elegant_black"
    "admin/structure/block/list/garland"
    "admin/structure/block/list/minimal_white"
    "admin/structure/block/list/ninesixty" "admin/structure/block/list/omega"
    "admin/structure/block/list/rubik" "admin/structure/block/list/seven"
    "admin/structure/block/list/sleek" "admin/structure/block/list/soe"
    "admin/structure/block/list/stark"
    "admin/structure/block/list/starterkit_alpha_xhtml"
    "admin/structure/block/list/starterkit_omega_html5"
    "admin/structure/block/list/starterkit_omega_xhtml"
    "admin/structure/block/list/tao" "admin/structure/ds/list/emergency"
    "admin/structure/ds/list/list" "admin/structure/views/settings/advanced"
    "admin/structure/views/settings/basic" "admin/views/ajax/autocomplete/tag"
    "admin/views/ajax/autocomplete/taxonomy"
    "admin/views/ajax/autocomplete/user" "ctools/context/ajax/access/add"
    "ctools/context/ajax/access/configure" "ctools/context/ajax/access/delete"
    "user/%/cancel/confirm/%/%" "node/%/webform/components/%/clone"
    "node/%/webform/components/%/delete" "node/%/webform/emails/%/delete"
    "entityreference/autocomplete/single/%/%/%"
    "entityreference/autocomplete/tags/%/%/%"
    "admin/structure/field-collections/%/fields/%"
    "admin/structure/taxonomy/%/fields/%"
    "admin/structure/field-collections/%/display/default"
    "admin/structure/field-collections/%/display/diff_standard"
    "admin/structure/field-collections/%/display/full"
    "admin/structure/field-collections/%/display/job_title"
    "admin/structure/field-collections/%/display/token"
    "admin/structure/taxonomy/%/display/default"
    "admin/structure/taxonomy/%/display/diff_standard"
    "admin/structure/taxonomy/%/display/full"
    "admin/structure/taxonomy/%/display/token"
    "admin/structure/block/manage/%/%" "admin/structure/views/ajax/%/%"
    "admin/structure/views/nojs/%/%" "admin/config/content/formats/%/disable"
    "admin/config/user-interface/shortcut/%/add-link"
    "admin/config/user-interface/shortcut/%/add-link-inline"
    "admin/config/user-interface/shortcut/%/delete"
    "admin/config/user-interface/shortcut/%/edit"
    "admin/config/user-interface/shortcut/%/links"
    "admin/structure/block-types/manage/%/auto_label"
    "admin/structure/block-types/manage/%/delete"
    "admin/structure/block-types/manage/%/dependencies"
    "admin/structure/block-types/manage/%/display"
    "admin/structure/block-types/manage/%/edit"
    "admin/structure/block-types/manage/%/fields"
    "admin/structure/block-types/manage/%/revert"
    "admin/structure/context/list/%/clone"
    "admin/structure/context/list/%/delete"
    "admin/structure/context/list/%/disable"
    "admin/structure/context/list/%/edit"
    "admin/structure/context/list/%/enable"
    "admin/structure/context/list/%/export"
    "admin/structure/context/list/%/revert"
    "admin/structure/menu/item/%/delete" "admin/structure/menu/item/%/edit"
    "admin/structure/menu/item/%/reset" "admin/structure/menu/manage/%/add"
    "admin/structure/menu/manage/%/delete"
    "admin/structure/menu/manage/%/edit" "admin/structure/menu/manage/%/list"
    "admin/structure/quicktabs/manage/%/clone"
    "admin/structure/quicktabs/manage/%/delete"
    "admin/structure/quicktabs/manage/%/edit"
    "admin/structure/quicktabs/manage/%/export"
    "admin/structure/types/manage/%/auto_label"
    "admin/structure/types/manage/%/delete"
    "admin/structure/types/manage/%/dependencies"
    "admin/structure/types/manage/%/display"
    "admin/structure/types/manage/%/edit"
    "admin/structure/types/manage/%/fields"
    "admin/structure/types/manage/%/form"
    "admin/structure/views/template/%/add"
    "admin/structure/views/view/%/break-lock"
    "admin/structure/views/view/%/clone" "admin/structure/views/view/%/delete"
    "admin/structure/views/view/%/disable" "admin/structure/views/view/%/edit"
    "admin/structure/views/view/%/enable"
    "admin/structure/views/view/%/export"
    "admin/structure/views/view/%/revert" "admin/config/content/diff/fields/%"
    "admin/config/content/wysiwyg/profile/%"
    "admin/config/media/file-types/manage/%"
    "admin/config/media/flexslider/delete/%"
    "admin/config/media/flexslider/edit/%"
    "admin/config/media/image-styles/delete/%"
    "admin/config/media/image-styles/edit/%"
    "admin/config/media/image-styles/revert/%"
    "admin/config/media/vef_video_styles/list/%"
    "admin/config/people/accounts/fields/%"
    "admin/config/people/ip-blocking/delete/%"
    "admin/config/search/path/delete/%" "admin/config/search/path/edit/%"
    "admin/config/search/redirect/delete/%"
    "admin/config/search/redirect/edit/%"
    "admin/config/system/actions/delete/%"
    "admin/config/user-interface/shortcut/link/%"
    "admin/people/permissions/roles/delete/%"
    "admin/people/permissions/roles/edit/%"
    "admin/config/content/diff/entities/node"
    "admin/config/content/shortcode/generator/button"
    "admin/config/content/shortcode/generator/icon"
    "admin/config/people/accounts/display/default"
    "admin/config/people/accounts/display/diff_standard"
    "admin/config/people/accounts/display/full"
    "admin/config/people/accounts/display/token"
    "admin/config/people/captcha/captcha/captcha_point"
    "admin/config/people/captcha/captcha/examples"
    "admin/config/people/captcha/captcha/settings"
    "admin/config/people/ldap/servers/add"
    "admin/config/people/ldap/servers/list"
    "admin/config/regional/date-time/formats/add"
    "admin/config/regional/date-time/formats/look up"
    "admin/config/regional/date-time/types/add"
    "admin/config/regional/stringoverrides/manage/en"
    "admin/structure/block/list/alpha/add"
    "admin/structure/block/list/alpha/add-menu-block"
    "admin/structure/block/list/bartik/add"
    "admin/structure/block/list/bartik/add-menu-block"
    "admin/structure/block/list/boxy/add"
    "admin/structure/block/list/boxy/add-menu-block"
    "admin/structure/block/list/canvas/add"
    "admin/structure/block/list/canvas/add-menu-block"
    "admin/structure/block/list/color_palette/add"
    "admin/structure/block/list/color_palette/add-menu-block"
    "admin/structure/block/list/cu_backstretch/add"
    "admin/structure/block/list/cu_backstretch/add-menu-block"
    "admin/structure/block/list/cu_bootstrap/add"
    "admin/structure/block/list/cu_bootstrap/add-menu-block"
    "admin/structure/block/list/cu_omega/add"
    "admin/structure/block/list/cu_omega/add-menu-block"
    "admin/structure/block/list/elegant_black/add"
    "admin/structure/block/list/elegant_black/add-menu-block"
    "admin/structure/block/list/garland/add"
    "admin/structure/block/list/garland/add-menu-block"
    "admin/structure/block/list/minimal_white/add"
    "admin/structure/block/list/minimal_white/add-menu-block"
    "admin/structure/block/list/ninesixty/add"
    "admin/structure/block/list/ninesixty/add-menu-block"
    "admin/structure/block/list/omega/add"
    "admin/structure/block/list/omega/add-menu-block"
    "admin/structure/block/list/rubik/add"
    "admin/structure/block/list/rubik/add-menu-block"
    "admin/structure/block/list/seven/add"
    "admin/structure/block/list/seven/add-menu-block"
    "admin/structure/block/list/sleek/add"
    "admin/structure/block/list/sleek/add-menu-block"
    "admin/structure/block/list/stark/add"
    "admin/structure/block/list/stark/add-menu-block"
    "admin/structure/block/list/starterkit_alpha_xhtml/add"
    "admin/structure/block/list/starterkit_alpha_xhtml/add-menu-block"
    "admin/structure/block/list/starterkit_omega_html5/add"
    "admin/structure/block/list/starterkit_omega_html5/add-menu-block"
    "admin/structure/block/list/starterkit_omega_xhtml/add"
    "admin/structure/block/list/starterkit_omega_xhtml/add-menu-block"
    "admin/structure/block/list/tao/add"
    "admin/structure/block/list/tao/add-menu-block"
    "admin/structure/ds/fields/manage_ctools/content"
    "admin/workbench/create/node/add/article"
    "admin/workbench/create/node/add/class-note"
    "admin/workbench/create/node/add/faqs"
    "admin/workbench/create/node/add/fast-fact"
    "admin/workbench/create/node/add/file"
    "admin/workbench/create/node/add/graduate-program"
    "admin/workbench/create/node/add/page"
    "admin/workbench/create/node/add/past-event"
    "admin/workbench/create/node/add/person"
    "admin/workbench/create/node/add/photo-gallery"
    "admin/workbench/create/node/add/profile"
    "admin/workbench/create/node/add/video"
    "admin/workbench/create/node/add/webform"
    "admin/structure/field-collections/%/fields/%/delete"
    "admin/structure/field-collections/%/fields/%/edit"
    "admin/structure/field-collections/%/fields/%/field-settings"
    "admin/structure/field-collections/%/fields/%/widget-type"
    "admin/structure/field-collections/%/groups/%/delete"
    "admin/structure/taxonomy/%/fields/%/delete"
    "admin/structure/taxonomy/%/fields/%/edit"
    "admin/structure/taxonomy/%/fields/%/field-settings"
    "admin/structure/taxonomy/%/fields/%/widget-type"
    "admin/structure/taxonomy/%/groups/%/delete"
    "admin/structure/block/manage/%/%/configure"
    "admin/structure/block/manage/%/%/delete"
    "admin/structure/block-types/manage/%/fields/%"
    "admin/structure/types/manage/%/fields/%"
    "admin/structure/views/view/%/preview/%"
    "admin/structure/block-types/manage/%/display/default"
    "admin/structure/block-types/manage/%/display/diff_standard"
    "admin/structure/block-types/manage/%/display/token"
    "admin/structure/types/manage/%/display/default"
    "admin/structure/types/manage/%/display/diff_standard"
    "admin/structure/types/manage/%/display/full"
    "admin/structure/types/manage/%/display/revision"
    "admin/structure/types/manage/%/display/rss"
    "admin/structure/types/manage/%/display/sidebar"
    "admin/structure/types/manage/%/display/teaser"
    "admin/structure/types/manage/%/display/title"
    "admin/structure/types/manage/%/display/token"
    "admin/structure/views/ajax/preview/%/%"
    "admin/structure/views/nojs/preview/%/%"
    "admin/config/content/wysiwyg/profile/%/delete"
    "admin/config/content/wysiwyg/profile/%/edit"
    "admin/config/media/file-types/manage/%/auto_label"
    "admin/config/media/file-types/manage/%/dependencies"
    "admin/config/media/file-types/manage/%/display"
    "admin/config/media/file-types/manage/%/fields"
    "admin/config/media/file-types/manage/%/file-display"
    "admin/config/media/vef_video_styles/list/%/clone"
    "admin/config/media/vef_video_styles/list/%/delete"
    "admin/config/media/vef_video_styles/list/%/disable"
    "admin/config/media/vef_video_styles/list/%/edit"
    "admin/config/media/vef_video_styles/list/%/enable"
    "admin/config/media/vef_video_styles/list/%/export"
    "admin/config/media/vef_video_styles/list/%/revert"
    "admin/config/people/accounts/fields/%/delete"
    "admin/config/people/accounts/fields/%/edit"
    "admin/config/people/accounts/fields/%/field-settings"
    "admin/config/people/accounts/fields/%/widget-type"
    "admin/config/people/accounts/groups/%/delete"
    "admin/config/regional/date-time/formats/%/delete"
    "admin/config/regional/date-time/formats/%/edit"
    "admin/config/regional/date-time/types/%/delete"
    "admin/config/user-interface/shortcut/link/%/delete"
    "admin/structure/block/manage/boxes/%/delete"
    "admin/config/people/ldap/servers/delete/%"
    "admin/config/people/ldap/servers/disable/%"
    "admin/config/people/ldap/servers/edit/%"
    "admin/config/people/ldap/servers/enable/%"
    "admin/config/people/ldap/servers/test/%"
    "admin/structure/block-types/manage/%/fields/%/delete"
    "admin/structure/block-types/manage/%/fields/%/edit"
    "admin/structure/block-types/manage/%/fields/%/field-settings"
    "admin/structure/block-types/manage/%/fields/%/widget-type"
    "admin/structure/block-types/manage/%/groups/%/delete"
    "admin/structure/types/manage/%/fields/%/delete"
    "admin/structure/types/manage/%/fields/%/edit"
    "admin/structure/types/manage/%/fields/%/field-settings"
    "admin/structure/types/manage/%/fields/%/widget-type"
    "admin/structure/types/manage/%/groups/%/delete"
    "admin/structure/views/view/%/edit/%/ajax"
    "admin/structure/views/view/%/preview/%/ajax"
    "admin/config/media/file-types/manage/%/fields/%"
    "admin/config/media/image-styles/edit/%/add/%"
    "admin/config/media/image-styles/edit/%/effects/%"
    "admin/config/media/file-types/manage/%/display/default"
    "admin/config/media/file-types/manage/%/display/media_large"
    "admin/config/media/file-types/manage/%/display/media_link"
    "admin/config/media/file-types/manage/%/display/media_original"
    "admin/config/media/file-types/manage/%/display/media_preview"
    "admin/config/media/file-types/manage/%/display/media_small"
    "admin/config/media/file-types/manage/%/display/token"
    "admin/config/media/file-types/manage/%/file-display/default"
    "admin/config/media/file-types/manage/%/file-display/media_large"
    "admin/config/media/file-types/manage/%/file-display/media_link"
    "admin/config/media/file-types/manage/%/file-display/media_original"
    "admin/config/media/file-types/manage/%/file-display/media_preview"
    "admin/config/media/file-types/manage/%/file-display/media_small"
    "admin/config/media/file-types/manage/%/file-display/token"
    "admin/config/media/file-types/manage/%/fields/%/delete"
    "admin/config/media/file-types/manage/%/fields/%/edit"
    "admin/config/media/file-types/manage/%/fields/%/field-settings"
    "admin/config/media/file-types/manage/%/fields/%/widget-type"
    "admin/config/media/file-types/manage/%/groups/%/delete"
    "admin/config/media/image-styles/edit/%/effects/%/delete"))

    (defun cu-drupal-menu-browse ()
      "browse specific menu path on drupal site"
      (interactive)
      (let ((menu (completing-read "Browse: " d7-menus)))
        (browse-url (concat base_url "/" menu))))
    (bind-key "C-8 m b" 'cu-drupal-menu-browse)
)


(provide 'druide)

;;; druide.el ends here
