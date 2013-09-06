;;; Compiled snippets and support files for `texinfo-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'texinfo-mode
                     '(("data" "@c data: $1\n@smallexample\n$0@end smallexample\n\n@c smallexample: $LEDGER -f $1 \n" "@c data: .. ..." nil nil nil nil nil nil)
                       ("enum" "@enumerate\n@item $0\n@end enumerate" "enum" nil nil nil nil nil nil)
                       ("ex" "@example\n$0\n@end example" "ex" nil nil nil nil nil nil)
                       ("item" "@itemize @bullet\n@item $0\n@end itemize" "item" nil nil nil nil nil nil)
                       ("smex" "@smallexample\n$0\n@end smallexample" "smex" nil nil nil nil nil nil)))


;;; Do not edit! File generated at Wed Sep  4 12:29:35 2013
