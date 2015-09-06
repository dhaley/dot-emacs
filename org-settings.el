(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-M-RET-may-split-line (quote ((headline) (default . t))))
 '(org-adapt-indentation nil)
 '(org-agenda-auto-exclude-function (quote org-my-auto-exclude-function))
 '(org-agenda-clock-consistency-checks
   (quote
    (:max-duration "4:00" :min-duration 0 :max-gap 0 :gap-ok-around
                   ("4:00"))))
 '(org-agenda-clockreport-parameter-plist
   (quote
    (:link t :maxlevel 5 :fileskip0 t :compact t :narrow 80)))
 '(org-agenda-cmp-user-defined (quote bh/agenda-sort))
 '(org-agenda-compact-blocks t)
 '(org-agenda-custom-commands
   (quote
    (("N" "Notes" tags "NOTE"
      ((org-agenda-overriding-header "Notes")
       (org-tags-match-list-sublevels t)))
     ("h" "Habits" tags-todo "STYLE=\"habit\""
      ((org-agenda-overriding-header "Habits")
       (org-agenda-sorting-strategy
        (quote
         (todo-state-down effort-up category-keep)))))
     (" " "Agenda"
      ((agenda "" nil)
       (tags "REFILE"
             ((org-agenda-overriding-header "Tasks to Refile")
              (org-tags-match-list-sublevels nil)))
       (tags-todo "-CANCELLED/!"
                  ((org-agenda-overriding-header "Stuck Projects")
                   (org-agenda-skip-function
                    (quote bh/skip-non-stuck-projects))
                   (org-agenda-sorting-strategy
                    (quote
                     (category-keep)))))
       (tags-todo "-HOLD-CANCELLED/!"
                  ((org-agenda-overriding-header "Projects")
                   (org-agenda-skip-function
                    (quote bh/skip-non-projects))
                   (org-tags-match-list-sublevels
                    (quote indented))
                   (org-agenda-sorting-strategy
                    (quote
                     (category-keep)))))
       (tags-todo "-CANCELLED/!NEXT"
                  ((org-agenda-overriding-header
                    (concat "Project Next Tasks"
                            (if bh/hide-scheduled-and-waiting-next-tasks "" " (including WAITING and SCHEDULED tasks)")))
                   (org-agenda-skip-function
                    (quote bh/skip-projects-and-habits-and-single-tasks))
                   (org-tags-match-list-sublevels t)
                   (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                   (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                   (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                   (org-agenda-sorting-strategy
                    (quote
                     (todo-state-down effort-up category-keep)))))
       (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
                  ((org-agenda-overriding-header
                    (concat "Project Subtasks"
                            (if bh/hide-scheduled-and-waiting-next-tasks "" " (including WAITING and SCHEDULED tasks)")))
                   (org-agenda-skip-function
                    (quote bh/skip-non-project-tasks))
                   (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                   (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                   (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                   (org-agenda-sorting-strategy
                    (quote
                     (category-keep)))))
       (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
                  ((org-agenda-overriding-header
                    (concat "Standalone Tasks"
                            (if bh/hide-scheduled-and-waiting-next-tasks "" " (including WAITING and SCHEDULED tasks)")))
                   (org-agenda-skip-function
                    (quote bh/skip-project-tasks))
                   (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                   (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                   (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                   (org-agenda-sorting-strategy
                    (quote
                     (category-keep)))))
       (tags-todo "-CANCELLED+WAITING|HOLD/!"
                  ((org-agenda-overriding-header
                    (concat "Waiting and Postponed Tasks"
                            (if bh/hide-scheduled-and-waiting-next-tasks "" " (including WAITING and SCHEDULED tasks)")))
                   (org-agenda-skip-function
                    (quote bh/skip-non-tasks))
                   (org-tags-match-list-sublevels nil)
                   (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                   (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)))
       (tags "-REFILE/"
             ((org-agenda-overriding-header "Tasks to Archive")
              (org-agenda-skip-function
               (quote bh/skip-non-archivable-tasks))
              (org-tags-match-list-sublevels nil))))
      nil))))
 '(org-agenda-deadline-leaders (quote ("!D!: " "D%02d: ")))
 '(org-agenda-default-appointment-duration 60)
 '(org-agenda-diary-file "~/Documents/diary.org")
 '(org-agenda-dim-blocked-tasks nil)
 '(org-agenda-exporter-settings
   (quote
    ((org-agenda-write-buffer-name "Damon's VC-Rsrch/Dean-Grad Agenda"))))
 '(org-agenda-files
   (quote
    ("~/Documents/todo.txt" "~/Documents/from-mobile.org")))
 '(org-agenda-fontify-priorities t)
 '(org-agenda-include-diary t)
 '(org-agenda-inhibit-startup t)
 '(org-agenda-insert-diary-extract-time t)
 '(org-agenda-log-mode-items (quote (closed clock state)))
 '(org-agenda-ndays 1)
 '(org-agenda-persistent-filter t)
 '(org-agenda-prefix-format
   (quote
    ((agenda . "  %-11c%?-12t% s")
     (timeline . "  % s")
     (todo . "  %-11c")
     (tags . "  %-11c"))))
 '(org-agenda-repeating-timestamp-show-all t)
 '(org-agenda-restriction-lock-highlight-subtree nil)
 '(org-agenda-scheduled-leaders (quote ("" "S%d: ")))
 '(org-agenda-scheduled-relative-text "S%d: ")
 '(org-agenda-scheduled-text "")
 '(org-agenda-show-all-dates t)
 '(org-agenda-skip-additional-timestamps-same-entry t)
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-scheduled-if-deadline-is-shown t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-skip-timestamp-if-done t)
 '(org-agenda-skip-unavailable-files t)
 '(org-agenda-sorting-strategy
   (quote
    ((agenda habit-down time-up user-defined-up effort-up category-keep)
     (todo category-up effort-up)
     (tags category-up effort-up)
     (search category-up))))
 '(org-agenda-span (quote day))
 '(org-agenda-start-on-weekday nil)
 '(org-agenda-start-with-log-mode nil)
 '(org-agenda-sticky t)
 '(org-agenda-tags-column -100)
 '(org-agenda-tags-todo-honor-ignore-options t)
 '(org-agenda-text-search-extra-files (quote (agenda-archives)))
 '(org-agenda-time-grid
   (quote
    ((daily today remove-match)
     #("----------------" 0 16
       (org-heading t))
     (900 1100 1300 1500 1700))))
 '(org-agenda-todo-ignore-with-date nil)
 '(org-agenda-use-time-grid nil)
 '(org-agenda-window-setup (quote current-window))
 '(org-archive-location "%s_archive::* Archived Tasks")
 '(org-archive-save-context-info (quote (time category itags)))
 '(org-attach-method (quote mv))
 '(org-babel-load-languages
   (quote
    ((php . t)
     (emacs-lisp . t)
     (css . t)
     (js . t)
     (makefile . t)
     (perl . t)
     (ruby . t)
     (gnuplot . t)
     (sql . t)
     (sh . t)
     (screen . t)
     (org . t)
     (plantuml . t)
     (latex . t))))
 '(org-babel-results-keyword "results")
 '(org-beamer-frame-default-options "fragile")
 '(org-blank-before-new-entry (quote ((heading) (plain-list-item . auto))))
 '(org-capture-templates
   (quote
    (("t" "Task" entry
      (file+headline "~/Documents/todo.txt" "Inbox")
      "* TODO %?
SCHEDULED: %t
:PROPERTIES:
:ID:       %(shell-command-to-string \"uuidgen\"):CREATED:  %U
:END:" :prepend t))))
 '(org-catch-invisible-edits (quote error))
 '(org-clock-auto-clock-resolution (quote when-no-clock-is-running))
 '(org-clock-clocked-in-display nil)
 '(org-clock-history-length 23)
 '(org-clock-idle-time 10)
 '(org-clock-in-resume t)
 '(org-clock-in-switch-to-state (quote bh/clock-in-to-next))
 '(org-clock-into-drawer t)
 '(org-clock-mode-line-total (quote current))
 '(org-clock-modeline-total (quote current))
 '(org-clock-out-remove-zero-time-clocks t)
 '(org-clock-out-switch-to-state nil)
 '(org-clock-out-when-done t)
 '(org-clock-persist t)
 '(org-clock-persist-file "~/.emacs.d/data/org-clock-save.el")
 '(org-clock-persist-query-resume nil)
 '(org-clock-report-include-clocking-task t)
 '(org-clock-resolve-expert t)
 '(org-clock-sound "/usr/local/lib/tngchime.wav")
 '(org-clone-delete-id t)
 '(org-columns-default-format
   "%80ITEM(Task) %10Effort(Effort){:} %10Confidence(Confidence) %10CLOCKSUM")
 '(org-completion-use-ido t)
 '(org-confirm-babel-evaluate nil)
 '(org-confirm-elisp-link-function nil)
 '(org-confirm-shell-link-function nil)
 '(org-crypt-disable-auto-save nil)
 '(org-crypt-key "F0B66B40")
 '(org-cycle-global-at-bob t)
 '(org-cycle-include-plain-lists t)
 '(org-cycle-separator-lines 0)
 '(org-deadline-warning-days 14)
 '(org-default-notes-file "~/Documents/todo.txt")
 '(org-default-priority 69)
 '(org-directory "~/Documents")
 '(org-ditaa-jar-path "~/bin/DitaaEps/DitaaEps.jar")
 '(org-edit-src-content-indentation 0)
 '(org-emphasis-alist
   (quote
    (("*" bold "<b>" "</b>")
     ("/" italic "<i>" "</i>")
     ("_" underline "<span style=\"text-decoration:underline;\">" "</span>")
     ("=" org-code "<code>" "</code>" verbatim)
     ("~" org-verbatim "<code>" "</code>" verbatim))))
 '(org-enable-priority-commands t)
 '(org-enforce-todo-dependencies t)
 '(org-export-allow-BIND t)
 '(org-export-babel-evaluate nil)
 '(org-export-html-inline-images t)
 '(org-export-html-style-extra
   "<link rel=\"stylesheet\" href=\"http://doc.norang.ca/org.css\" type=\"text/css\" />")
 '(org-export-html-style-include-default nil)
 '(org-export-html-xml-declaration
   (quote
    (("html" . "")
     ("was-html" . "<?xml version=\"1.0\" encoding=\"%s\"?>")
     ("php" . "<?php echo \"<?xml version=\\\"1.0\\\" encoding=\\\"%s\\\" ?>\"; ?>"))))
 '(org-export-htmlize-output-type (quote css))
 '(org-export-latex-classes
   (quote
    (("article" "\\documentclass[11pt]{article}"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
      ("\\paragraph{%s}" . "\\paragraph*{%s}")
      ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
     ("linalg" "\\documentclass{article}
\\usepackage{linalgjh}
[DEFAULT-PACKAGES]
[EXTRA]
`[PACKAGES]"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
      ("\\paragraph{%s}" . "\\paragraph*{%s}")
      ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
     ("report" "\\documentclass[11pt]{report}"
      ("\\part{%s}" . "\\part*{%s}")
      ("\\chapter{%s}" . "\\chapter*{%s}")
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
     ("book" "\\documentclass[11pt]{book}"
      ("\\part{%s}" . "\\part*{%s}")
      ("\\chapter{%s}" . "\\chapter*{%s}")
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
     ("beamer" "\\documentclass{beamer}" org-beamer-sectioning))))
 '(org-export-latex-listings t)
 '(org-export-with-section-numbers nil)
 '(org-export-with-sub-superscripts (quote {}))
 '(org-export-with-timestamps nil)
 '(org-extend-today-until 7)
 '(org-fast-tag-selection-single-key (quote expert))
 '(org-file-apps
(quote
 ((auto-mode . emacs)
  ("\\.mm\\'" . system)
  ("\\.x?html?\\'" . system)
  ("\\.pdf\\'" . system))))
 '(org-fontify-done-headline t)
 '(org-footnote-section nil)
 '(org-global-properties
(quote
 (("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
  ("Confidence_ALL" . "low medium high")
  ("STYLE_ALL" . "habit"))))
 '(org-habit-graph-column 50)
 '(org-habit-preceding-days 42)
 '(org-habit-today-glyph 45)
 '(org-hide-leading-stars t)
 '(org-html-checkbox-type "unicode")
 '(org-id-link-to-org-use-id (quote create-if-interactive-and-no-custom-id))
 '(org-id-locations-file "~/.emacs.d/data/org-id-locations")
 '(org-id-method (quote uuidgen))
 '(org-image-actual-width (quote (800)))
 '(org-indirect-buffer-display (quote current-window))
 '(org-insert-heading-respect-content t)
 '(org-irc-link-to-logs t t)
 '(org-latex-default-packages-alist
(quote
 (("T1" "fontenc" t)
  ("" "fixltx2e" nil)
  ("" "graphicx" t)
  ("" "longtable" nil)
  ("" "float" nil)
  ("" "wrapfig" nil)
  ("" "rotating" nil)
  ("normalem" "ulem" t)
  ("" "amsmath" t)
  ("" "textcomp" t)
  ("" "marvosym" t)
  ("" "wasysym" t)
  ("" "amssymb" t)
  ("" "hyperref" nil)
  "\\tolerance=1000")))
 '(org-link-abbrev-alist
(quote
 (("gmail" . "https://mail.google.com/mail/u/0/#all/%s")
  ("google" . "http://www.google.com/search?q=%s")
  ("map" . "http://maps.google.com/maps?q=%s"))))
 '(org-link-frame-setup
(quote
 ((vm . vm-visit-folder)
  (gnus . org-gnus-no-new-news)
  (file . find-file))))
 '(org-link-mailto-program (quote (compose-mail "%a" "%s")))
 '(org-list-allow-alphabetical t)
 '(org-list-demote-modify-bullet
(quote
 (("+" . "-")
  ("*" . "-")
  ("1." . "-")
  ("1)" . "-")
  ("A)" . "-")
  ("B)" . "-")
  ("a)" . "-")
  ("b)" . "-")
  ("A." . "-")
  ("B." . "-")
  ("a." . "-")
  ("b." . "-"))))
 '(org-log-done (quote time))
 '(org-log-into-drawer t)
 '(org-log-state-notes-insert-after-drawers nil)
 '(org-lowest-priority 69)
 '(org-mobile-agendas (quote ("Z")))
 '(org-mobile-directory "~/Dropbox/Apps/MobileOrg")
 '(org-mobile-files (quote ("~/Documents/todo.txt")))
 '(org-mobile-files-exclude-regexp "\\(TODO\\(-.*\\)?\\)\\'")
 '(org-mobile-inbox-for-pull "~/Documents/from-mobile.org")
 '(org-modules (quote (org-gnus org-id org-info org-habit org-depend)))
 '(org-odd-levels-only nil)
 '(org-outline-path-complete-in-steps nil)
 '(org-plantuml-jar-path nil)
 '(org-priority-faces
(quote
 ((65 :foreground "ForestGreen" :weight bold)
  (67 :foreground "dark gray" :slant italic))))
 '(org-refile-allow-creating-parent-nodes (quote confirm))
 '(org-refile-target-verify-function (quote bh/verify-refile-target))
 '(org-refile-targets
(quote
 ((nil :maxlevel . 9)
  (org-agenda-files :maxlevel . 9))))
 '(org-refile-use-outline-path t)
 '(org-remove-highlights-with-change t)
 '(org-return-follows-link t)
 '(org-reveal-root "/Users/dhaley/src/reveal.js")
 '(org-reverse-note-order t)
 '(org-special-ctrl-a/e (quote reversed))
 '(org-special-ctrl-k t)
 '(org-speed-commands-user
(quote
 (("0" . ignore)
  ("1" . ignore)
  ("2" . ignore)
  ("3" . ignore)
  ("4" . ignore)
  ("5" . ignore)
  ("6" . ignore)
  ("7" . ignore)
  ("8" . ignore)
  ("9" . ignore)
  ("a" . ignore)
  ("d" . ignore)
  ("h" . bh/hide-other)
  ("i" progn
   (forward-char 1)
   (call-interactively
    (quote org-insert-heading-respect-content)))
  ("k" . org-kill-note-or-show-branches)
  ("l" . ignore)
  ("m" . ignore)
  ("q" . bh/show-org-agenda)
  ("r" . ignore)
  ("s" . org-save-all-org-buffers)
  ("w" . org-refile)
  ("x" . ignore)
  ("y" . ignore)
  ("z" . org-add-note)
  ("A" . ignore)
  ("B" . ignore)
  ("E" . ignore)
  ("F" . bh/restrict-to-file-or-follow)
  ("G" . ignore)
  ("H" . ignore)
  ("J" . org-clock-goto)
  ("K" . ignore)
  ("L" . ignore)
  ("M" . ignore)
  ("N" . bh/narrow-to-org-subtree)
  ("P" . bh/narrow-to-org-project)
  ("Q" . ignore)
  ("R" . ignore)
  ("S" . ignore)
  ("T" . bh/org-todo)
  ("U" . bh/narrow-up-one-org-level)
  ("V" . ignore)
  ("W" . bh/widen)
  ("X" . ignore)
  ("Y" . ignore)
  ("Z" . ignore))))
 '(org-src-fontify-natively t)
 '(org-src-preserve-indentation nil)
 '(org-src-window-setup (quote current-window))
 '(org-startup-folded t)
 '(org-startup-indented t)
 '(org-startup-with-inline-images nil)
 '(org-structure-template-alist
(quote
 (("s" "#+begin_src ?

#+end_src" "<src lang=\"?\">

</src>")
  ("e" "#+begin_example
?
#+end_example" "<example>
?
</example>")
  ("m" "#+begin_src message

#+end_src" "<src lang=message>

</src>")
  ("q" "#+begin_quote
?
#+end_quote" "<quote>
?
</quote>")
  ("v" "#+begin_verse
?
#+end_verse" "<verse>
?
</verse>")
  ("c" "#+begin_center
?
#+end_center" "<center>
?
</center>")
  ("l" "#+begin_latex
?
#+end_latex" "<literal style=\"latex\">
?
</literal>")
  ("L" "#+latex: " "<literal style=\"latex\">?</literal>")
  ("h" "#+begin_html
?
#+end_html" "<literal style=\"html\">
?
</literal>")
  ("H" "#+html: " "<literal style=\"html\">?</literal>")
  ("a" "#+begin_ascii
?
#+end_ascii")
  ("A" "#+ascii: ")
  ("i" "#+index: ?" "#+index: ?")
  ("I" "#+include %file ?" "<include file=%file markup=\"?\">"))))
 '(org-stuck-projects (quote ("TODO=\"PROJECT\"" nil nil "SCHEDULED:")))
 '(org-tag-alist
(quote
 ((:startgroup)
  ("@errand" . 101)
  ("@net" . 110)
  ("@home" . 72)
  (:endgroup)
  ("WAITING" . 119)
  ("HOLD" . 104)
  ("PERSONAL" . 80)
  ("WORK" . 87)
  ("ORG" . 79)
  ("NOTE" . 78)
  ("CANCELLED" . 99)
  ("FLAGGED" . 63))))
 '(org-tags-column -97)
 '(org-tags-exclude-from-inheritance (quote ("crypt")))
 '(org-tags-match-list-sublevels t)
 '(org-time-clocksum-format
(quote
 (:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t)))
 '(org-time-clocksum-use-fractional t)
 '(org-todo-keyword-faces
(quote
 (("TODO" :inherit org-todo)
  ("PHONE" :foreground "forest green" :weight bold))))
 '(org-todo-keywords
(quote
 ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
  (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))
 '(org-todo-repeat-to-state "TODO")
 '(org-todo-state-tags-triggers
(quote
 (("CANCELLED"
   ("CANCELLED" . t))
  ("WAITING"
   ("WAITING" . t))
  ("HOLD"
   ("WAITING")
   ("HOLD" . t))
  (done
   ("WAITING")
   ("HOLD"))
  ("TODO"
   ("WAITING")
   ("CANCELLED")
   ("HOLD"))
  ("NEXT"
   ("WAITING")
   ("CANCELLED")
   ("HOLD"))
  ("DONE"
   ("WAITING")
   ("CANCELLED")
   ("HOLD")))))
 '(org-treat-S-cursor-todo-selection-as-state-change nil)
 '(org-use-fast-todo-selection t)
 '(org-use-property-inheritance (quote ("AREA")))
 '(org-use-speed-commands t)
 '(org-use-sub-superscripts (quote {}))
 '(org-use-tag-inheritance nil)
 '(org-velocity-always-use-bucket t)
 '(org-velocity-bucket "~/Documents/notes.txt")
 '(org-velocity-capture-templates
(quote
 (("v" "Velocity" entry
   (file "~/Documents/notes.txt")
   "* NOTE %:search
%i%?
:PROPERTIES:
:ID:       %(shell-command-to-string \\\"uuidgen\\\"):CREATED:  %U
:END:" :prepend t))))
 '(org-velocity-exit-on-match t)
 '(org-velocity-force-new t)
 '(org-velocity-search-method (quote regexp))
 '(org-x-backends (quote (ox-org ox-redmine)))
 '(org-x-redmine-title-prefix-function (quote org-x-redmine-title-prefix))
 '(org-x-redmine-title-prefix-match-function (quote org-x-redmine-title-prefix-match))
 '(org-yank-adjusted-subtrees t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-mode-line-clock ((t (:foreground "red" :box (:line-width -1 :style released-button))))))
