;;; Compiled snippets and support files for `drupal-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'drupal-mode
                     '(("var-get"
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


;;; Snippet definitions:
;;;
(yas-define-snippets 'drupal-mode
                     '(("h-block" "\n/**\n * Implements hook_block().\n */\nfunction ${1:`(drupal-module-name)`}_block_info() {\n  $blocks['syndicate'] = array(\n    'info' => t('Syndicate'),\n    'cache' => DRUPAL_NO_CACHE,\n  );\n$0\n  $blocks['recent'] = array(\n    'info' => t('Recent content'),\n    // DRUPAL_CACHE_PER_ROLE will be assumed.\n  );\n\n  return $blocks;\n}\n\n/**\n * Implements hook_block_configure().\n */\nfunction $1_block_configure($delta = '') {\n  // This example comes from node.module.\n  $form = array();\n  if ($delta == 'recent') {\n    $form['node_recent_block_count'] = array(\n      '#type' => 'select',\n      '#title' => t('Number of recent content items to display'),\n      '#default_value' => variable_get('node_recent_block_count', 10),\n      '#options' => drupal_map_assoc(array(2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 15, 20, 30, 50)),\n    );\n  }\n  return $form;\n}\n\n/**\n * Implements hook_block_view().\n */\nfunction $1_block_view($delta = '') {\n  // This example is adapted from node.module.\n  $block = array();\n\n  switch ($delta) {\n    case 'syndicate':\n      $block['subject'] = t('Syndicate');\n      $block['content'] = array(\n        '#theme' => 'feed_icon',\n        '#url' => 'rss.xml',\n        '#title' => t('Syndicate'),\n      );\n      break;\n\n    case 'recent':\n      if (user_access('access content')) {\n        $block['subject'] = t('Recent content');\n        if ($nodes = node_get_recent(variable_get('node_recent_block_count', 10))) {\n          $block['content'] = array(\n            '#theme' => 'node_recent_block',\n            '#nodes' => $nodes,\n          );\n        }\n        else {\n          $block['content'] = t('No content available.');\n        }\n      }\n      break;\n  }\n  return $block;\n}\n" "hook block info/configure/view" nil
                        ("hooks")
                        nil nil nil nil)
                       ("h-node-delete" "\n/**\n * Implements hook_node_delete().\n */\nfunction ${1:`(drupal-module-name)`}_node_delete($node) {\n  $0\n}\n" "hook node delete" nil
                        ("hooks")
                        nil nil nil nil)
                       ("h-node-insert" "\n/**\n * Implements hook_node_insert().\n */\nfunction ${1:`(drupal-module-name)`}_node_insert($node) {\n  $0\n}\n" "hook node insert" nil
                        ("hooks")
                        nil nil nil nil)
                       ("h-node-presave" "\n/**\n * Implements hook_node_presave().\n */\nfunction ${1:`(drupal-module-name)`}_node_presave($node) {\n  $0\n}\n" "hook node presave" nil
                        ("hooks")
                        nil nil nil nil)
                       ("h-node-update" "\n/**\n * Implements hook_node_update().\n */\nfunction ${1:`(drupal-module-name)`}_node_update($node) {\n  $0\n}\n" "hook node update" nil
                        ("hooks")
                        nil nil nil nil)
                       ("h-node-view" "\n/**\n * Implements hook_node_view().\n */\nfunction ${1:`(drupal-module-name)`}_node_view($node, $view_mode) {\n  $0\n}\n" "hook node view" nil
                        ("hooks")
                        nil nil nil nil)
                       ("h-perm" "\n/**\n * Implements hook_permission().\n */\nfunction ${1:`(drupal-module-name)`}_permission(){\n  return array(\n    '${2:example restriction}' => array(\n      'title' => t('${3:Title of permission}'),\n      'description' => t('${4:Description of the permission.}'),\n			'restrict access' => ${5:$$(yas/choose-value '(\"FALSE\" \"TRUE\"))},\n    ),\n  );\n}\n$0" "hook perm" nil
                        ("hooks")
                        nil nil nil nil)
                       ("h-user-login" "\n/**\n * Implements hook_user_login().\n */\nfunction ${1:`(drupal-module-name)`}_user_login(&$edit, $account, $category) {\n  $0\n}\n" "hook user login" nil
                        ("hooks")
                        nil nil nil nil)
                       ("h-user-logout" "\n/**\n * Implements hook_user_logout().\n */\nfunction ${1:`(drupal-module-name)`}_user_logout($account) {\n  $0\n}\n" "hook user logout" nil
                        ("hooks")
                        nil nil nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'drupal-mode
                     '(("h-block-ru" "\n/**\n * Реализует hook_block_info(). Объявляет блоки модуля.\n *\n * @return array Ассоциативный массив с определением блока.\n */\nfunction ${1:`(drupal-module-name)`}_block_info() {\n  $blocks['syndicate'] = array(\n    'info' => t('Syndicate'),\n    'cache' => DRUPAL_NO_CACHE,\n  );\n$0\n  $blocks['recent'] = array(\n    'info' => t('Recent content'),\n    // DRUPAL_CACHE_PER_ROLE will be assumed.\n  );\n\n  return $blocks;\n}\n\n/**\n * Реализует hook_block_configure(). Определяет форму конфигурации.\n *\n * @return array Ассоциативный массив определяющий форму администрирования блока.\n */\nfunction $1_block_configure($delta = '') {\n  // This example comes from node.module.\n  $form = array();\n  if ($delta == 'recent') {\n    $form['node_recent_block_count'] = array(\n      '#type' => 'select',\n      '#title' => t('Number of recent content items to display'),\n      '#default_value' => variable_get('node_recent_block_count', 10),\n      '#options' => drupal_map_assoc(array(2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 15, 20, 30, 50)),\n    );\n  }\n  return $form;\n}\n\n/**\n * Реализует hook_block_view(). Отображение контента блоков модуля.\n *\n * @return string Содержимое блока.\n */\nfunction $1_block_view($delta = '') {\n  // This example is adapted from node.module.\n  $block = array();\n\n  switch ($delta) {\n    case 'syndicate':\n      $block['subject'] = t('Syndicate');\n      $block['content'] = array(\n        '#theme' => 'feed_icon',\n        '#url' => 'rss.xml',\n        '#title' => t('Syndicate'),\n      );\n      break;\n\n    case 'recent':\n      if (user_access('access content')) {\n        $block['subject'] = t('Recent content');\n        if ($nodes = node_get_recent(variable_get('node_recent_block_count', 10))) {\n          $block['content'] = array(\n            '#theme' => 'node_recent_block',\n            '#nodes' => $nodes,\n          );\n        }\n        else {\n          $block['content'] = t('No content available.');\n        }\n      }\n      break;\n  }\n  return $block;\n}\n" "hook block info/configure/view (ru)" nil
                        ("ru")
                        nil nil nil nil)
                       ("h-node-delete-ru" "\n/**\n * Реализует hook_node_delete().\n */\nfunction ${1:`(drupal-module-name)`}_node_delete($node) {\n  $0\n}\n" "hook node delete (ru)" nil
                        ("ru")
                        nil nil nil nil)
                       ("h-node-insert-ru" "\n/**\n * Реализует hook_node_insert().\n */\nfunction ${1:`(drupal-module-name)`}_node_insert($node) {\n  $0\n}\n" "hook node insert (ru)" nil
                        ("ru")
                        nil nil nil nil)
                       ("h-node-presave-ru" "\n/**\n * Реализует hook_node_presave().\n */\nfunction ${1:`(drupal-module-name)`}_node_presave($node) {\n  $0\n}\n" "hook node presave (ru)" nil
                        ("ru")
                        nil nil nil nil)
                       ("h-node-update-ru" "\n/**\n * Реализует hook_node_update().\n */\nfunction ${1:`(drupal-module-name)`}_node_update($node) {\n  $0\n}\n" "hook node update (ru)" nil
                        ("ru")
                        nil nil nil nil)
                       ("h-node-view-ru" "\n/**\n * Реализует hook_node_view().\n */\nfunction ${1:`(drupal-module-name)`}_node_view($node, $view_mode) {\n  $0\n}\n" "hook node view (ru)" nil
                        ("ru")
                        nil nil nil nil)
                       ("h-perm-ru" "\n/**\n * Реализует hook_permission().\n *\n * @return array Массив строковых значений прав доступа.\n * @ingroup hooks\n */\nfunction ${1:`(drupal-module-name)`}_permission(){\n  return array(\n    '${2:example restriction}' => array(\n      'title' => t('${3:Title of permission}'),\n      'description' => t('${4:Description of the permission.}'),\n			'restrict access' => ${5:$$(yas/choose-value '(\"FALSE\" \"TRUE\"))},\n    ),\n  );\n}\n$0" "hook perm" nil
                        ("ru")
                        nil nil nil nil)
                       ("h-user-login-ru" "\n/**\n * Реализует hook_user_login().\n */\nfunction ${1:`(drupal-module-name)`}_user_login(&$edit, $account, $category) {\n  $0\n}\n" "hook user login (ru)" nil
                        ("ru")
                        nil nil nil nil)
                       ("h-user-logout-ru" "\n/**\n * Реализует hook_user_logout().\n */\nfunction ${1:`(drupal-module-name)`}_user_logout($account) {\n  $0\n}\n" "hook user logout (ru)" nil
                        ("ru")
                        nil nil nil nil)))


;;; Do not edit! File generated at Tue Jan 15 10:55:25 2013
