;;; Compiled snippets and support files for `drupal-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'drupal-mode
                     '(("cli" "<?php\n\n/**\n * @file\n * A drupal cli execution script\n *\n * Run drupal API hooks from the command line.\n * This is useful before putting functionality in a site install.\n */\n\nerror_reporting(E_ERROR | E_WARNING | E_PARSE | E_NOTICE);\n\ndefine('DRUPAL_ROOT', getcwd());\n// Necessary if running from command line!\nip_address() = \"localhost\";\nrequire_once DRUPAL_ROOT . '/includes/bootstrap.inc';\nrequire_once \"includes/install.inc\";\ndrupal_bootstrap(DRUPAL_BOOTSTRAP_FULL);\n" "cli execution script template" nil nil nil nil nil nil)
                       ("doc-file" "/**\n * @file\n * Programmatically create VCR content. Oh Yeah!\n *\n * Will create pages, menus and menu items, beans and more.\n * More power!\n */\n" "Documenting files" nil nil nil nil nil nil)
                       ("doc-func" "/**\n * Verifies the syntax of the given e-mail address.\n *\n * Empty e-mail addresses are allowed. See RFC 2822 for details.\n *\n * @param string $mail\n *   A string containing an email address.\n *\n * @return bool\n *   TRUE if the address is in a valid format, and FALSE if it isn't.\n */\n" "Documenting functions and methods" nil nil nil nil nil nil)
                       ("var-get"
                        (progn variable_get
                               ('${1:varname}
                                '(\, ${2:)
                                "default value" })
                               $0)
                        "core function variable_get(name, default)" nil nil nil nil nil nil)
                       ("var-set"
                        (progn variable_set
                               ('${1:varname}
                                '(\, ${2:)
                                "value" })
                               $0)
                        "core function variable_set(name, val)" nil nil nil nil nil nil)))


;;; Do not edit! File generated at Fri Jan 25 15:38:24 2013
