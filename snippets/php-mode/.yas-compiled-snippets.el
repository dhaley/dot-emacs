;;; Compiled snippets and support files for `php-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'php-mode
                     '(("<?php" "<?php\n/**\n * ${1:file_title}\n *\n * @package   ${2:package}\n * @author    ${3:author} <${4:email}>\n * @copyright Copyright (c) ${5:`(format-time-string \"%Y\" (current-time))`} ${6:copyright}\n */\n\n/**\n * $1\n *\n * ${7:description}\n *\n * @package $2\n * @author  $3 <$4>\n * @copyright Copyright (c) $5 $6\n */\nclass ${8:`(replace-regexp-in-string\n \"^_+\" \"\"\n (mapconcat\n  #'identity\n  (split-string\n   ((lambda (a)\n      (substring a (let ((case-fold-search nil))\n                     (string-match \"\\\\\\\\(/[A-Z][a-zA-Z0-9]+\\\\\\\\)+$\" a )\n                     ))\n      )\n    (file-name-sans-extension (buffer-file-name))\n    ) \"/\") \"_\"))`}\n{\n   public function ${9:__construct}()\n   {\n      $0\n   }\n}\n" "<?php /* ... */ class ...{...} " nil
                        ("definitions")
                        nil nil nil nil)
                       ("README" "Minimal php-mode scripts for yasnippet by Nishimura (http://d.hatena.ne.jp/n314/).\nTaken from git://github.com/nishimura/minimal-yasnippet-php-mode.git" "README" nil nil nil nil nil nil)
                       ("/*" "/*\n * $0\n */" "/* ... */" nil
                        ("definitions")
                        nil nil nil nil)
                       ("def" "define('${1:name$(upcase yas/text)}', ${2:'value'});" "define('...', ...)" nil
                        ("definitions")
                        nil nil nil nil)
                       ("/**" "/**\n * $0\n */" "/** ... */" nil
                        ("definitions")
                        nil nil nil nil)
                       ("pri" "private function ${1:name}($2) {\n        $0\n}" "private-function" nil nil nil nil nil nil)
                       ("f" "public function ${1:name}($2) {\n       $0\n}" "public function" nil nil nil nil nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'php-mode
                     '(("class" "class ${1:`(replace-regexp-in-string\n \"^_+\" \"\"\n (mapconcat\n  #'identity\n  (split-string\n   ((lambda (a)\n      (substring a (let ((case-fold-search nil))\n                     (string-match \"\\\\\\\\(/[A-Z][a-zA-Z0-9]+\\\\\\\\)+$\" a )\n                     ))\n      )\n    (file-name-sans-extension (buffer-file-name))\n    ) \"/\") \"_\"))`}\n{\n   public function __construct()\n   {\n      $0\n   }\n}\n" "class ... " nil
                        ("definitions")
                        nil nil nil nil)
                       ("el" "else {\n  $0\n}" "else { ... }" nil
                        ("controls")
                        nil nil nil nil)
                       ("for" "for (${1:$i = 0}; ${2:$i < $condition}; ${3:$i++}) {\n  $0\n}\n" "for (...) {...}" nil
                        ("control structure")
                        nil nil nil nil)
                       ("fore" "foreach (${1:$iterator} as ${2:$key => $value}) {\n  $0\n}\n" "fore (...) {...}" nil
                        ("control structure")
                        nil nil nil nil)
                       ("fun" "function ${1:name}(${2:$args}) {\n  $0\n}" "fun ...(...)" nil
                        ("definitions")
                        nil nil nil nil)
                       ("function" "function ${1:name}(${2:$args}) {\n  $0\n}\n" "function ...(...)" nil
                        ("definitions")
                        nil nil nil nil)
                       ("if" "if (${1:condition}) {\n  $0\n}\n" "if (...) {...}" nil
                        ("control structure")
                        nil nil nil nil)
                       ("ife" "if (${1:condition}) {\n  $2\n}\nelse {\n  $0\n}\n" "if (...) {...} else { ... }" nil
                        ("control structure")
                        nil nil nil nil)
                       ("pri" "private function ${1:name}(${2:$args}) {\n  $0\n}\n" "pri ...(...)" nil
                        ("definitions")
                        nil nil nil nil)
                       ("pro" "protected function ${1:name}(${2:$args}) {\n  $0\n}\n" "pro ...(...)" nil
                        ("definitions")
                        nil nil nil nil)
                       ("pub" "/**\n * ${1:function_title}\n *\n * ${2:description}\n */\npublic function ${4:name}($5) {\n  $0\n}\n" "pub ...(...)" nil
                        ("definitions")
                        nil nil nil nil)
                       ("switch" "switch (${1:true}) {\ncase $2:\n  $0\ndefault:\n}\n" "switch (...) {...}" nil
                        ("control structure")
                        nil nil nil nil)
                       ("try" "try {\n  $1\n}\ncatch (${2:Exception} ${3:$e}) {\n  $0\n}\n" "try { ... } catch ( ... ){ ... }" nil
                        ("control structure")
                        nil nil nil nil)
                       ("while" "while (${1:condition}) {\n  $0\n}\n" "while (...) {...}" nil
                        ("control structure")
                        nil nil nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'php-mode
                     '(("ar" "array($0)" "array" nil
                        ("functions")
                        nil nil nil nil)))


;;; Do not edit! File generated at Fri Jan 18 13:56:41 2013
