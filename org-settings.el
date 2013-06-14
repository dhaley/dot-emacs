(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-auto-exclude-function (quote bh/org-auto-exclude-function))
 '(org-agenda-clock-consistency-checks (quote (:max-duration "4:00" :min-duration 0 :max-gap 0 :gap-ok-around ("4:00"))))
 '(org-agenda-clockreport-parameter-plist (quote (:link t :maxlevel 5 :fileskip0 t :compact t :narrow 80)))
 '(org-agenda-cmp-user-defined (quote bh/agenda-sort))
 '(org-agenda-compact-blocks t)
 '(org-agenda-custom-commands (quote (("N" "Notes" tags "NOTE" ((org-agenda-overriding-header "Notes") (org-tags-match-list-sublevels t))) ("h" "Habits" tags-todo "STYLE=\"habit\"" ((org-agenda-overriding-header "Habits") (org-agenda-sorting-strategy (quote (todo-state-down effort-up category-keep))))) (" " "Agenda" ((agenda "" nil) (tags "REFILE" ((org-agenda-overriding-header "Tasks to Refile") (org-tags-match-list-sublevels nil))) (tags-todo "-CANCELLED/!" ((org-agenda-overriding-header "Stuck Projects") (org-agenda-skip-function (quote bh/skip-non-stuck-projects)) (org-agenda-sorting-strategy (quote (priority-down category-keep))))) (tags-todo "-WAITING-CANCELLED/!NEXT" ((org-agenda-overriding-header "Next Tasks") (org-agenda-skip-function (quote bh/skip-projects-and-habits-and-single-tasks)) (org-agenda-todo-ignore-scheduled t) (org-agenda-todo-ignore-deadlines t) (org-agenda-todo-ignore-with-date t) (org-tags-match-list-sublevels t) (org-agenda-sorting-strategy (quote (priority-down todo-state-down effort-up category-keep))))) (tags-todo "-REFILE-CANCELLED/!-HOLD-WAITING" ((org-agenda-overriding-header "Tasks") (org-agenda-skip-function (quote bh/skip-project-tasks-maybe)) (org-agenda-todo-ignore-scheduled t) (org-agenda-todo-ignore-deadlines t) (org-agenda-todo-ignore-with-date t) (org-agenda-sorting-strategy (quote (category-keep))))) (tags-todo "-HOLD-CANCELLED/!" ((org-agenda-overriding-header "Projects") (org-agenda-skip-function (quote bh/skip-non-projects)) (org-agenda-sorting-strategy (quote (priority-down category-keep))))) (tags-todo "-CANCELLED+WAITING/!" ((org-agenda-overriding-header "Waiting and Postponed Tasks") (org-agenda-skip-function (quote bh/skip-stuck-projects)) (org-tags-match-list-sublevels nil) (org-agenda-todo-ignore-scheduled (quote future)) (org-agenda-todo-ignore-deadlines (quote future)))) (tags "-REFILE/" ((org-agenda-overriding-header "Tasks to Archive") (org-agenda-skip-function (quote bh/skip-non-archivable-tasks)) (org-tags-match-list-sublevels nil)))) nil) ("r" "Tasks to Refile" tags "REFILE" ((org-agenda-overriding-header "Tasks to Refile") (org-tags-match-list-sublevels nil))) ("#" "Stuck Projects" tags-todo "-CANCELLED/!" ((org-agenda-overriding-header "Stuck Projects") (org-agenda-skip-function (quote bh/skip-non-stuck-projects)))) ("n" "Next Tasks" tags-todo "-WAITING-CANCELLED/!NEXT" ((org-agenda-overriding-header "Next Tasks") (org-agenda-skip-function (quote bh/skip-projects-and-habits-and-single-tasks)) (org-agenda-todo-ignore-scheduled t) (org-agenda-todo-ignore-deadlines t) (org-agenda-todo-ignore-with-date t) (org-tags-match-list-sublevels t) (org-agenda-sorting-strategy (quote (todo-state-down effort-up category-keep))))) ("R" "Tasks" tags-todo "-REFILE-CANCELLED/!-HOLD-WAITING" ((org-agenda-overriding-header "Tasks") (org-agenda-skip-function (quote bh/skip-project-tasks-maybe)) (org-agenda-sorting-strategy (quote (category-keep))))) ("p" "Projects" tags-todo "-HOLD-CANCELLED/!" ((org-agenda-overriding-header "Projects") (org-agenda-skip-function (quote bh/skip-non-projects)) (org-agenda-sorting-strategy (quote (category-keep))))) ("w" "Waiting Tasks" tags-todo "-CANCELLED+WAITING/!" ((org-agenda-overriding-header "Waiting and Postponed tasks")) (org-tags-match-list-sublevels nil)) ("A" "Tasks to Archive" tags "-REFILE/" ((org-agenda-overriding-header "Tasks to Archive") (org-agenda-skip-function (quote bh/skip-non-archivable-tasks)) (org-tags-match-list-sublevels nil))))))
 '(org-agenda-diary-file "~/Documents/Tasks/diary.org")
 '(org-agenda-dim-blocked-tasks nil)
 '(org-agenda-exporter-settings (quote ((org-agenda-write-buffer-name "Damon's VC-Rsrch/Dean-Grad Agenda"))))
 '(org-agenda-files (quote ("~/Documents/Tasks/todo.txt" "~/Documents/Tasks/refile.org" "~/Documents/Tasks/.org-jira/CYE.org" "~/Documents/Tasks/.org-jira/ENVD.org" "~/Documents/Tasks/from-mobile.org")))
 '(org-agenda-include-diary nil)
 '(org-agenda-insert-diary-extract-time t)
 '(org-agenda-log-mode-items (quote (closed state)))
 '(org-agenda-persistent-filter t)
 '(org-agenda-repeating-timestamp-show-all t)
 '(org-agenda-show-all-dates t)
 '(org-agenda-skip-additional-timestamps-same-entry t)
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-skip-timestamp-if-done t)
 '(org-agenda-sorting-strategy (quote ((agenda habit-down time-up user-defined-up priority-down effort-up category-keep) (todo category-up priority-down effort-up) (tags category-up priority-down effort-up) (search category-up))))
 '(org-agenda-span (quote day))
 '(org-agenda-start-on-weekday 1)
 '(org-agenda-tags-column -102)
 '(org-agenda-tags-todo-honor-ignore-options t)
 '(org-agenda-text-search-extra-files (quote (agenda-archives)))
 '(org-agenda-time-grid (quote ((daily today remove-match) #("----------------" 0 16 (org-heading t)) (900 1100 1300 1500 1700))))
 '(org-agenda-todo-ignore-with-date nil)
 '(org-agenda-window-setup (quote current-window))
 '(org-archive-location "%s_archive::* Archived Tasks")
 '(org-babel-results-keyword "results")
 '(org-blank-before-new-entry (quote ((heading) (plain-list-item . auto))))
 '(org-capture-templates (quote (("#" "used by gnus-calendar-org" entry (file+olp "~/Documents/Tasks/todo.txt" "Appointments") "%i" :immediate-finish t) ("j" "Journal" entry (file+datetree "~/Documents/Tasks/diary.org") "* %?%^G
 Entered on %T
   %i
" :clock-in t :clock-resume t) ("W" "Capture web snippet" entry (file+headline "~/owncloud/learn-emacs.txt" "Emacs mastery") "* Fact: '%:description'        :drill:
:PROPERTIES:
:DATE_ADDED: %u
:SOURCE_URL: %c
:END:

%i
%?
" :immediate-finish t :empty-lines 1) ("t" "todo" entry (file "~/Documents/Tasks/refile.org") "* TODO %?
%U
%a
" :clock-in t :clock-resume t) ("r" "respond" entry (file "~/Documents/Tasks/refile.org") "* NEXT Respond to %:from on %:subject
SCHEDULED: %t
%U
%a
" :immediate-finish t :clock-in t :clock-resume t) ("n" "note" entry (file "~/Documents/Tasks/refile.org") "* %? :NOTE:
%U
%a
" :clock-in t :clock-resume t) ("w" "org-protocol" entry (file "~/Documents/Tasks/refile.org") "* TODO Review %c
%U
" :immediate-finish t) ("p" "Phone call" entry (file "~/Documents/Tasks/refile.org") "* PHONE %? :PHONE:
%U" :clock-in t :clock-resume t) ("h" "Habit" entry (file "~/Documents/Tasks/refile.org") "* NEXT %?
%U
%a
SCHEDULED: %(format-time-string \"<%Y-%m-%d %a .+1d/3d>\")
:PROPERTIES:
:STYLE: habit
:REPEAT_TO_STATE: NEXT
:END:
"))))
 '(org-catch-invisible-edits (quote error))
 '(org-clock-auto-clock-resolution (quote when-no-clock-is-running))
 '(org-clock-history-length 36)
 '(org-clock-in-resume t)
 '(org-clock-in-switch-to-state (quote bh/clock-in-to-next))
 '(org-clock-into-drawer t)
 '(org-clock-out-remove-zero-time-clocks t)
 '(org-clock-out-when-done t)
 '(org-clock-persist t)
 '(org-clock-persist-query-resume nil)
 '(org-clock-report-include-clocking-task t)
 '(org-clock-sound "/usr/local/lib/tngchime.wav")
 '(org-clone-delete-id t)
 '(org-columns-default-format "%80ITEM(Task) %10Effort(Effort){:} %10Confidence(Confidence) %10CLOCKSUM")
 '(org-completion-use-ido t)
 '(org-confirm-babel-evaluate nil)
 '(org-crypt-disable-auto-save nil)
 '(org-crypt-key "F0B66B40")
 '(org-cycle-include-plain-lists t)
 '(org-cycle-separator-lines 0)
 '(org-deadline-warning-days 30)
 '(org-default-notes-file "~/Documents/Tasks/refile.org")
 '(org-default-priority 69)
 '(org-directory "~/Documents/Tasks")
 '(org-ditaa-jar-path "~/git/foss/org-mode/contrib/scripts/ditaa.jar")
 '(org-drawers (quote ("PROPERTIES" "LOGBOOK")))
 '(org-edit-src-content-indentation 0)
 '(org-emphasis-alist (quote (("*" bold "<b>" "</b>") ("/" italic "<i>" "</i>") ("_" underline "<span style=\"text-decoration:underline;\">" "</span>") ("=" org-code "<code>" "</code>" verbatim) ("~" org-verbatim "<code>" "</code>" verbatim))))
 '(org-enable-priority-commands t)
 '(org-enforce-todo-dependencies t)
 '(org-export-allow-BIND t)
 '(org-export-html-inline-images t)
 '(org-export-html-style-extra "<link rel=\"stylesheet\" href=\"http://doc.norang.ca/org.css\" type=\"text/css\" />")
 '(org-export-html-style-include-default nil)
 '(org-export-html-xml-declaration (quote (("html" . "") ("was-html" . "<?xml version=\"1.0\" encoding=\"%s\"?>") ("php" . "<?php echo \"<?xml version=\\\"1.0\\\" encoding=\\\"%s\\\" ?>\"; ?>"))))
 '(org-export-htmlize-output-type (quote css))
 '(org-export-latex-listings t)
 '(org-export-with-section-numbers nil)
 '(org-export-with-timestamps nil)
 '(org-fast-tag-selection-single-key (quote expert))
 '(org-file-apps (quote ((auto-mode . emacs) ("\\.mm\\'" . system) ("\\.x?html?\\'" . system) ("\\.pdf\\'" . system))))
 '(org-global-properties (quote (("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00") ("Confidence_ALL" . "low medium high") ("STYLE_ALL" . "habit"))))
 '(org-habit-graph-column 50)
 '(org-hide-leading-stars nil)
 '(org-id-method (quote uuidgen))
 '(org-insert-heading-respect-content nil)
 '(org-link-abbrev-alist (quote (("gmail" . "https://mail.google.com/mail/u/0/#all/%s") ("google" . "http://www.google.com/search?q=%s") ("map" . "http://maps.google.com/maps?q=%s"))))
 '(org-link-frame-setup (quote ((vm . vm-visit-folder) (gnus . org-gnus-no-new-news) (file . find-file))))
 '(org-link-mailto-program (quote (compose-mail "%a" "%s")))
 '(org-list-allow-alphabetical t)
 '(org-list-demote-modify-bullet (quote (("+" . "-") ("*" . "-") ("1." . "-") ("1)" . "-"))))
 '(org-log-done (quote time))
 '(org-log-into-drawer t)
 '(org-log-state-notes-insert-after-drawers nil)
 '(org-lowest-priority 69)
 '(org-mobile-agendas (quote ("Z")))
 '(org-mobile-directory "~/Dropbox/Apps/MobileOrg")
 '(org-mobile-files (quote ("~/Documents/Tasks/todo.txt")))
 '(org-mobile-files-exclude-regexp "\\(TODO\\(-.*\\)?\\)\\'")
 '(org-mobile-inbox-for-pull "~/Documents/Tasks/from-mobile.org")
 '(org-modules (quote (org-bbdb org-bibtex org-crypt org-gnus org-id org-info org-jsinfo org-habit org-inlinetask org-irc org-mew org-mhe org-protocol org-mac-link-grabber org-rmail org-vm org-wl org-w3m)))
 '(org-odd-levels-only nil)
 '(org-outline-path-complete-in-steps nil)
 '(org-plantuml-jar-path nil)
 '(org-refile-allow-creating-parent-nodes (quote confirm))
 '(org-refile-target-verify-function (quote bh/verify-refile-target))
 '(org-refile-targets (quote ((nil :maxlevel . 9) (org-agenda-files :maxlevel . 9))))
 '(org-refile-use-outline-path t)
 '(org-remove-highlights-with-change nil)
 '(org-return-follows-link t)
 '(org-reverse-note-order nil)
 '(org-show-entry-below t)
 '(org-show-following-heading t)
 '(org-show-hierarchy-above t)
 '(org-show-siblings (quote ((default))))
 '(org-special-ctrl-a/e (quote reversed))
 '(org-special-ctrl-k t)
 '(org-speed-commands-user (quote (("0" . ignore) ("1" . ignore) ("2" . ignore) ("3" . ignore) ("4" . ignore) ("5" . ignore) ("6" . ignore) ("7" . ignore) ("8" . ignore) ("9" . ignore) ("a" . ignore) ("d" . ignore) ("h" . bh/hide-other) ("i" progn (forward-char 1) (call-interactively (quote org-insert-heading-respect-content))) ("k" . org-kill-note-or-show-branches) ("l" . ignore) ("m" . ignore) ("q" . bh/show-org-agenda) ("r" . ignore) ("s" . org-save-all-org-buffers) ("w" . org-refile) ("x" . ignore) ("y" . ignore) ("z" . org-add-note) ("A" . ignore) ("B" . ignore) ("E" . ignore) ("F" . bh/restrict-to-file-or-follow) ("G" . ignore) ("H" . ignore) ("J" . org-clock-goto) ("K" . ignore) ("L" . ignore) ("M" . ignore) ("N" . bh/narrow-to-org-subtree) ("P" . bh/narrow-to-org-project) ("Q" . ignore) ("R" . ignore) ("S" . ignore) ("T" . bh/org-todo) ("U" . bh/narrow-up-one-org-level) ("V" . ignore) ("W" . bh/widen) ("X" . ignore) ("Y" . ignore) ("Z" . ignore))))
 '(org-src-fontify-natively t)
 '(org-src-preserve-indentation nil)
 '(org-src-window-setup (quote current-window))
 '(org-startup-folded t)
 '(org-startup-indented t)
 '(org-startup-with-inline-images nil)
 '(org-structure-template-alist (quote (("s" "#+begin_src ?

#+end_src" "<src lang=\"?\">

</src>") ("e" "#+begin_example
?
#+end_example" "<example>
?
</example>") ("m" "#+begin_src message

#+end_src" "<src lang=message>

</src>") ("q" "#+begin_quote
?
#+end_quote" "<quote>
?
</quote>") ("v" "#+begin_verse
?
#+end_verse" "<verse>
?
</verse>") ("c" "#+begin_center
?
#+end_center" "<center>
?
</center>") ("l" "#+begin_latex
?
#+end_latex" "<literal style=\"latex\">
?
</literal>") ("L" "#+latex: " "<literal style=\"latex\">?</literal>") ("h" "#+begin_html
?
#+end_html" "<literal style=\"html\">
?
</literal>") ("H" "#+html: " "<literal style=\"html\">?</literal>") ("a" "#+begin_ascii
?
#+end_ascii") ("A" "#+ascii: ") ("i" "#+index: ?" "#+index: ?") ("I" "#+include %file ?" "<include file=%file markup=\"?\">"))))
 '(org-stuck-projects (quote ("" nil nil "")))
 '(org-tag-alist (quote ((:startgroup) ("@errand" . 101) ("@office" . 111) ("@home" . 72) (:endgroup) ("PHONE" . 112) ("WAITING" . 119) ("HOLD" . 104) ("PERSONAL" . 80) ("WORK" . 87) ("SOCO" . 70) ("ORG" . 79) ("BLOG" . 78) ("crypt" . 69) ("MARK" . 77) ("NOTE" . 110) ("KGNU" . 66) ("CANCELLED" . 99) ("FLAGGED" . 63))))
 '(org-tags-exclude-from-inheritance (quote ("crypt")))
 '(org-tags-match-list-sublevels t)
 '(org-time-clocksum-format (quote (:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t)))
 '(org-todo-keyword-faces (quote (("TODO" :foreground "red" :weight bold) ("NEXT" :foreground "blue" :weight bold) ("DONE" :foreground "forest green" :weight bold) ("WAITING" :foreground "orange" :weight bold) ("HOLD" :foreground "magenta" :weight bold) ("CANCELLED" :foreground "forest green" :weight bold) ("PHONE" :foreground "forest green" :weight bold))))
 '(org-todo-keywords (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)") (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE"))))
 '(org-todo-state-tags-triggers (quote (("CANCELLED" ("CANCELLED" . t)) ("WAITING" ("WAITING" . t)) ("HOLD" ("WAITING" . t) ("HOLD" . t)) (done ("WAITING") ("HOLD")) ("TODO" ("WAITING") ("CANCELLED") ("HOLD")) ("NEXT" ("WAITING") ("CANCELLED") ("HOLD")) ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))
 '(org-treat-S-cursor-todo-selection-as-state-change nil)
 '(org-use-fast-todo-selection t)
 '(org-use-speed-commands t)
 '(org-use-sub-superscripts nil)
 '(org-yank-adjusted-subtrees t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-mode-line-clock ((t (:foreground "red" :box (:line-width -1 :style released-button)))) t))
