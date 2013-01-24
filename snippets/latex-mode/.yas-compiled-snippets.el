;;; Compiled snippets and support files for `latex-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'latex-mode
                     '(("begin" "\n\\begin{${1:environment}}\n$0\n\\end{$1}" "\\begin{environment} ... \\end{environment}" nil nil nil nil nil nil)
                       ("layout-article" "\\documentclass[twoside, 10pt, a4paper, titlepage]{article}\n\n\\usepackage[latin1]{inputenc}\n\\usepackage[T1]{fontenc}\n\\usepackage[dvips]{graphicx}\n\n\n\\title{}\n\\author{}\n\n\\begin{document}\n\n\\maketitle\n\n\n\\section{}\n\n\n\\end{document}\n" "layout-article" nil nil nil nil nil nil)))


;;; Do not edit! File generated at Thu Jan 24 12:21:34 2013
