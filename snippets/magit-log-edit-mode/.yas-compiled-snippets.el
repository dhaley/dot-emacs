;;; Compiled snippets and support files for `magit-log-edit-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'magit-log-edit-mode
                     '(("misra" "Summary: MISRA-C:2004 - Rule $1 ($2)\n\nType                 : new\nISA                  : TSM470\nFeature              : MISRA-C:2004 Project\nImpacts Compatability: no\nImpacts Documentation: no\nImpacts Performance  : no\nImpacts readme       : no\nImpacts Tool Speed   : no\n\nTechnical Description:\n  ANSI/error_msg.txt:\n  ANSI/misra.c:\n\n  Implemented MISRA-C:2004 $2 rule $1 which states:\n\n    $3.\n\n  ANSI/$0.c:\n\n  :\n    .\n" "MISRA-C:2004 rule commit description" nil nil
                        ((yas/indent-line 'fixed)
                         (yas/wrap-around-region 'nil))
                        nil nil nil)))


;;; Do not edit! File generated at Fri Jan 25 15:38:25 2013
