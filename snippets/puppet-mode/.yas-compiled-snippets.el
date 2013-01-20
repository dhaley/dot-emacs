;;; Compiled snippets and support files for `puppet-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'puppet-mode
                     '(("dir" "file { \"/home/$1\":\n  owner   => $1,\n  group   => $1,\n  mode    => 0700,\n  type    => directory,\n  ensure  => directory;\n}" "dir" nil nil nil nil nil nil)))


;;; Do not edit! File generated at Fri Jan 18 13:56:41 2013
