;;;_ , Org-mode

(load "org-settings")

;; The following setting is different from the document so that you
;; can override the document path by setting your path in the variable
;; org-mode-user-lisp-path
;;
;; (if (boundp 'org-mode-user-lisp-path)
;;     (add-to-list 'load-path org-mode-user-lisp-path)
;;   (add-to-list 'load-path (expand-file-name "~/git/org-mode/lisp")))

;; (add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$"
;; . org-mode))

(require 'org)
(require 'org-agenda)
(require 'org-smart-capture)
(require 'org-devonthink)
(require 'org-debbugs)
(require 'org-magit)
(require 'ob-emacs-lisp)
(require 'ob-sh)
(require 'async)


;; (use-package 'yasnippet)
;; (require 'org)

(use-package org-habit
  ;; :commands org-habit-toggle-habbits
  :diminish org-habit)

;; (require 'org-habit) ;; added by dkh
(require 'org-drill)
;; (org-babel-tangle "~/.emacs.d/dkh-org.org")
;; (org-babel-load-file "~/.emacs.d/dkh-org.org")

(define-key org-mode-map (kbd "C-c k") 'org-cut-subtree)

(setq org-export-with-section-numbers nil)
(setq org-html-include-timestamps nil)

(defun sacha/org-export-subtree-as-html-fragment ()
  (interactive)
  (org-export-region-as-html
   (org-back-to-heading)
   (org-end-of-subtree)
   t))

;; (setq org-link-abbrev-alist
;;   '(("google" . "http://www.google.com/search?q=")
;;     ("gmap" . "http://maps.google.com/maps?q=%s")
;;     ("blog" . "http://sachachua.com/blog/p/")))

;; (org-babel-do-load-languages
;;     'org-babel-load-languages '((python . t) (R . t) (perl . t)))

;; (bind-key "C-c 1"  (lambda () (interactive) (switch-to-buffer-other-window "*Org Agenda*")))
(bind-key "C-c 1"  (lambda () (interactive) (switch-to-buffer "*Org Agenda*")))


;;
;; Standard key bindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)


;; Custom Key Bindings
(global-set-key (kbd "<f12>") 'org-agenda)
(global-set-key (kbd "<f5>") 'bh/org-todo)
(global-set-key (kbd "<S-f5>") 'bh/widen)
(global-set-key (kbd "<f7>") 'bh/set-truncate-lines)
(global-set-key (kbd "<f8>") 'org-cycle-agenda-files)
(global-set-key (kbd "<f9> <f9>") 'bh/show-org-agenda)
(global-set-key (kbd "<f9> b") 'bbdb)
(global-set-key (kbd "<f9> c") 'calendar)
(global-set-key (kbd "<f9> f") 'boxquote-insert-file)
(global-set-key (kbd "<f9> g") 'gnus)
(global-set-key (kbd "<f9> h") 'bh/hide-other)
(global-set-key (kbd "<f9> n") 'org-narrow-to-subtree)
(global-set-key (kbd "<f9> w") 'widen)
(global-set-key (kbd "<f9> u") 'bh/narrow-up-one-level)

(global-set-key (kbd "<f9> I") 'bh/punch-in)
(global-set-key (kbd "<f9> O") 'bh/punch-out)

(global-set-key (kbd "<f9> o") 'bh/make-org-scratch)

(global-set-key (kbd "<f9> r") 'boxquote-region)
(global-set-key (kbd "<f9> s") 'bh/switch-to-scratch)

(global-set-key (kbd "<f9> t") 'bh/insert-inactive-timestamp)
(global-set-key (kbd "<f9> T") 'tabify)
(global-set-key (kbd "<f9> U") 'untabify)

(global-set-key (kbd "<f9> v") 'visible-mode)
(global-set-key (kbd "<f9> SPC") 'bh/clock-in-last-task)
(global-set-key (kbd "C-<f9>") 'previous-buffer)
(global-set-key (kbd "M-<f9>") 'org-toggle-inline-images)
(global-set-key (kbd "C-x n r") 'narrow-to-region)
(global-set-key (kbd "C-<f10>") 'next-buffer)
(global-set-key (kbd "<f11>") 'org-clock-goto)
(global-set-key (kbd "C-<f11>") 'org-clock-in)
(global-set-key (kbd "C-s-<f12>") 'bh/save-then-publish)
(global-set-key (kbd "C-M-r") 'org-capture)
(global-set-key (kbd "C-c r") 'org-capture)

(defun bh/hide-other ()
  (interactive)
  (save-excursion
    (org-back-to-heading 'invisible-ok)
    (hide-other)
    (org-cycle)
    (org-cycle)
    (org-cycle)))

(defun bh/set-truncate-lines ()
  "Toggle value of truncate-lines and refresh window display."
  (interactive)
  (setq truncate-lines (not truncate-lines))
  ;; now refresh window display (an idiom from simple.el):
  (save-excursion
    (set-window-start (selected-window)
                      (window-start (selected-window)))))

(defun bh/make-org-scratch ()
  (interactive)
  (find-file "/tmp/publish/scratch.org")
  (gnus-make-directory "/tmp/publish"))

(defun bh/switch-to-scratch ()
  (interactive)
  (switch-to-buffer "*scratch*"))





(setq org-use-fast-todo-selection t)


(require 'org-exchange-capture)

;; I use C-M-r to start capture mode
(global-set-key (kbd "C-M-r") 'org-capture)
;; I use C-c r to start capture mode when using SSH from my Android phone
(global-set-key (kbd "C-c r") 'org-capture)

;; Capture templates for: TODO tasks, Notes, appointments, phone calls, and org-protocol
(setq org-capture-templates
      (quote (("t" "todo" entry (file "~/Documents/Tasks/refile.org")
               "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
              ("r" "respond" entry (file "~/Documents/Tasks/refile.org")
               "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
              ("n" "note" entry (file "~/Documents/Tasks/refile.org")
               "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
              ("w" "org-protocol" entry (file "~/Documents/Tasks/refile.org")
               "* TODO Review %c\n%U\n" :immediate-finish t)
              ("p" "Phone call" entry (file "~/Documents/Tasks/refile.org")
               "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
              ("h" "Habit" entry (file "~/Documents/Tasks/refile.org")
               "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"<%Y-%m-%d %a .+1d/3d>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n"))))

(add-to-list 'org-capture-templates
             `
             ("u"
              "Task: Read this URL"
              entry
              (file+headline "~/Documents/Tasks/urls.org" "Articles To Read")
              ,(concat "* TODO Read article: '%:description'\nURL: %c\n\n")
              :empty-lines 1
              :immediate-finish t))

(add-to-list 'org-capture-templates
             `
             ("W"
              "Capture web snippet"
              entry
              (file+headline "~/Documents/Tasks/learn-emacs.org" "Emacs mastery")
              ,(concat "* Fact: '%:description'        :"
                       (format "%s" org-drill-question-tag)
                       ":\n:PROPERTIES:\n:DATE_ADDED: %u\n:SOURCE_URL: %c\n:END:\n\n%i\n%?\n")
              :empty-lines 1
              :immediate-finish t))


(add-to-list 'org-capture-templates
             `
             ("s" "secure" entry (file+datetree+prompt "~/Documents/Tasks/secure.org.gpg")
              "* %(format-time-string \"%H:%M\") %^{Entry} %^G\n%i%?"))

;; Remove empty LOGBOOK drawers on clock out
(defun bh/remove-empty-drawer-on-clock-out ()
  (interactive)
  (save-excursion
    (beginning-of-line 0)
    (org-remove-empty-drawer-at "LOGBOOK" (point))))

(add-hook 'org-clock-out-hook 'bh/remove-empty-drawer-on-clock-out 'append)





; Use IDO for both buffer and file completion and ido-everywhere to t
(setq ido-everywhere t)
(setq ido-max-directory-size 100000)
(ido-mode (quote both))
(setq ido-default-file-method 'selected-window)
(setq ido-default-buffer-method 'selected-window)

;;;; Refile settings
; Exclude DONE state tasks from refile targets
(defun bh/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))


;; Do not dim blocked tasks

;; Compact the block agenda view

;; Custom agenda command definitions


(defun bh/org-auto-exclude-function (tag)
  "Automatic task exclusion in the agenda with / RET"
  (and (cond
        ((string= tag "hold")
         t)
        ((string= tag "farm")
         t))
       (concat "-" tag)))


;;
;; Resume clocking task when emacs is restarted
(org-clock-persistence-insinuate)
;;
;; Show lot sof clocking history so it's easy to pick items off the C-F11 list
(setq org-clock-history-length 36)
;; Resume clocking task on clock-in if the clock is open
(setq org-clock-in-resume t)
;; Change tasks to NEXT when clocking in
(setq org-clock-in-switch-to-state 'bh/clock-in-to-next)
;; Separate drawers for clocking and logs
(setq org-drawers (quote ("PROPERTIES" "LOGBOOK")))
;; Save clock data and state changes and notes in the LOGBOOK drawer
(setq org-clock-into-drawer t)
;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)
;; Clock out when moving task to a done state
(setq org-clock-out-when-done t)
;; Save the running clock and all clock history when exiting Emacs, load it on startup
(setq org-clock-persist t)
;; Do not prompt to resume an active clock
(setq org-clock-persist-query-resume nil)
;; Enable auto clock resolution for finding open clocks
(setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))
;; Include current clocking task in clock reports
(setq org-clock-report-include-clocking-task t)

(setq bh/keep-clock-running nil)

(defun bh/clock-in-to-next (kw)
  "Switch a task from TODO to NEXT when clocking in.
Skips capture tasks, projects, and subprojects.
Switch projects and subprojects from NEXT back to TODO"
  (when (not (and (boundp 'org-capture-mode) org-capture-mode))
    (cond
     ((and (member (org-get-todo-state) (list "TODO"))
           (bh/is-task-p))
      "NEXT")
     ((and (member (org-get-todo-state) (list "NEXT"))
           (bh/is-project-p))
      "TODO"))))

(defun bh/find-project-task ()
  "Move point to the parent (project) task if any"
  (save-restriction
    (widen)
    (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
      (while (org-up-heading-safe)
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq parent-task (point))))
      (goto-char parent-task)
      parent-task)))

(defun bh/punch-in (arg)
  "Start continuous clocking and set the default task to the
selected task.  If no task is selected set the Organization task
as the default task."
  (interactive "p")
  (setq bh/keep-clock-running t)
  (if (equal major-mode 'org-agenda-mode)
      ;;
      ;; We're in the agenda
      ;;
      (let* ((marker (org-get-at-bol 'org-hd-marker))
             (tags (org-with-point-at marker (org-get-tags-at))))
        (if (and (eq arg 4) tags)
            (org-agenda-clock-in '(16))
          (bh/clock-in-organization-task-as-default)))
    ;;
    ;; We are not in the agenda
    ;;
    (save-restriction
      (widen)
      ; Find the tags on the current task
      (if (and (equal major-mode 'org-mode) (not (org-before-first-heading-p)) (eq arg 4))
          (org-clock-in '(16))
        (bh/clock-in-organization-task-as-default)))))

(defun bh/punch-out ()
  (interactive)
  (setq bh/keep-clock-running nil)
  (when (org-clock-is-active)
    (org-clock-out))
  (org-agenda-remove-restriction-lock))

(defun bh/clock-in-default-task ()
  (save-excursion
    (org-with-point-at org-clock-default-task
      (org-clock-in))))

(defun bh/clock-in-parent-task ()
  "Move point to the parent (project) task if any and clock in"
  (let ((parent-task))
    (save-excursion
      (save-restriction
        (widen)
        (while (and (not parent-task) (org-up-heading-safe))
          (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
            (setq parent-task (point))))
        (if parent-task
            (org-with-point-at parent-task
              (org-clock-in))
          (when bh/keep-clock-running
            (bh/clock-in-default-task)))))))

(defvar bh/organization-task-id "eb155a82-92b2-4f25-a3c6-0304591af2f9")

(defun bh/clock-in-organization-task-as-default ()
  (interactive)
  (org-with-point-at (org-id-find bh/organization-task-id 'marker)
    (org-clock-in '(16))))

(defun bh/clock-out-maybe ()
  (when (and bh/keep-clock-running
             (not org-clock-clocking-in)
             (marker-buffer org-clock-default-task)
             (not org-clock-resolving-clocks-due-to-idleness))
    (bh/clock-in-parent-task)))

(add-hook 'org-clock-out-hook 'bh/clock-out-maybe 'append)

(require 'org-id)
(defun bh/clock-in-task-by-id (id)
  "Clock in a task by id"
  (org-with-point-at (org-id-find id 'marker)
    (org-clock-in nil)))

(defun bh/clock-in-last-task (arg)
  "Clock in the interrupted task if there is one
Skip the default task and get the next one.
A prefix arg forces clock in of the default task."
  (interactive "p")
  (let ((clock-in-to-task
         (cond
          ((eq arg 4) org-clock-default-task)
          ((and (org-clock-is-active)
                (equal org-clock-default-task (cadr org-clock-history)))
           (caddr org-clock-history))
          ((org-clock-is-active) (cadr org-clock-history))
          ((equal org-clock-default-task (car org-clock-history)) (cadr org-clock-history))
          (t (car org-clock-history)))))
    (org-with-point-at clock-in-to-task
      (org-clock-in nil))))




;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)


(require 'bbdb)
(require 'bbdb-com)

(global-set-key (kbd "<f9> p") 'bh/phone-call)

;;
;; Phone capture template handling with BBDB lookup
;; Adapted from code by Gregory J. Grubbs
(defun bh/phone-call ()
  "Return name and company info for caller from bbdb lookup"
  (interactive)
  (let* (name rec caller)
    (setq name (completing-read "Who is calling? "
                                (bbdb-hashtable)
                                'bbdb-completion-predicate
                                'confirm))
    (when (> (length name) 0)
      ; Something was supplied - look it up in bbdb
      (setq rec
            (or (first
                 (or (bbdb-search (bbdb-records) name nil nil)
                     (bbdb-search (bbdb-records) nil name nil)))
                name)))

    ; Build the bbdb link if we have a bbdb record, otherwise just return the name
    (setq caller (cond ((and rec (vectorp rec))
                        (let ((name (bbdb-record-name rec))
                              (company (bbdb-record-company rec)))
                          (concat "[[bbdb:"
                                  name "]["
                                  name "]]"
                                  (when company
                                    (concat " - " company)))))
                       (rec)
                       (t "NameOfCaller")))
    (insert caller)))


(defun bh/is-project-p ()
  "Any task with a todo keyword subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task has-subtask))))

(defun bh/is-project-subtree-p ()
  "Any task with a todo keyword that is in a project subtree.
Callers of this function already widen the buffer view."
  (let ((task (save-excursion (org-back-to-heading 'invisible-ok)
                              (point))))
    (save-excursion
      (bh/find-project-task)
      (if (equal (point) task)
          nil
        t))))

(defun bh/is-task-p ()
  "Any task with a todo keyword and no subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task (not has-subtask)))))

(defun bh/is-subproject-p ()
  "Any task which is a subtask of another project"
  (let ((is-subproject)
        (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
    (save-excursion
      (while (and (not is-subproject) (org-up-heading-safe))
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq is-subproject t))))
    (and is-a-task is-subproject)))

(defun bh/list-sublevels-for-projects-indented ()
  "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
  This is normally used by skipping functions where this variable is already local to the agenda."
  (if (marker-buffer org-agenda-restrict-begin)
      (setq org-tags-match-list-sublevels 'indented)
    (setq org-tags-match-list-sublevels nil))
  nil)

(defun bh/list-sublevels-for-projects ()
  "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
  This is normally used by skipping functions where this variable is already local to the agenda."
  (if (marker-buffer org-agenda-restrict-begin)
      (setq org-tags-match-list-sublevels t)
    (setq org-tags-match-list-sublevels nil))
  nil)

(defun bh/skip-stuck-projects ()
  "Skip trees that are not stuck projects"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (bh/is-project-p)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (has-next ))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                (unless (member "WAITING" (org-get-tags-at))
                  (setq has-next t))))
            (if has-next
                nil
              next-headline)) ; a stuck project, has subtasks but no next task
        nil))))

(defun bh/skip-non-stuck-projects ()
  "Skip trees that are not stuck projects"
  (bh/list-sublevels-for-projects-indented)
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (bh/is-project-p)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (has-next ))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                (unless (member "WAITING" (org-get-tags-at))
                  (setq has-next t))))
            (if has-next
                next-headline
              nil)) ; a stuck project, has subtasks but no next task
        next-headline))))

(defun bh/skip-non-projects ()
  "Skip trees that are not projects"
  (bh/list-sublevels-for-projects-indented)
  (if (save-excursion (bh/skip-non-stuck-projects))
      (save-restriction
        (widen)
        (let ((subtree-end (save-excursion (org-end-of-subtree t))))
          (cond
           ((and (bh/is-project-p)
                 (marker-buffer org-agenda-restrict-begin))
            nil)
           ((and (bh/is-project-p)
                 (not (marker-buffer org-agenda-restrict-begin))
                 (not (bh/is-project-subtree-p)))
            nil)
           (t
            subtree-end))))
    (save-excursion (org-end-of-subtree t))))

(defun bh/skip-project-trees-and-habits ()
  "Skip trees that are projects"
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((bh/is-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       (t
        nil)))))

(defun bh/skip-projects-and-habits-and-single-tasks ()
  "Skip trees that are projects, tasks that are habits, single non-project tasks"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((org-is-habit-p)
        next-headline)
       ((bh/is-project-p)
        next-headline)
       ((and (bh/is-task-p) (not (bh/is-project-subtree-p)))
        next-headline)
       (t
        nil)))))

(defun bh/skip-project-tasks-maybe ()
  "Show tasks related to the current restriction.
When restricted to a project, skip project and sub project tasks, habits, NEXT tasks, and loose tasks.
When not restricted, skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
           (next-headline (save-excursion (or (outline-next-heading) (point-max))))
           (limit-to-project (marker-buffer org-agenda-restrict-begin)))
      (cond
       ((bh/is-project-p)
        next-headline)
       ((org-is-habit-p)
        subtree-end)
       ((and (not limit-to-project)
             (bh/is-project-subtree-p))
        subtree-end)
       ((and limit-to-project
             (bh/is-project-subtree-p)
             (member (org-get-todo-state) (list "NEXT")))
        subtree-end)
       (t
        nil)))))

(defun bh/skip-projects-and-habits ()
  "Skip trees that are projects and tasks that are habits"
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((bh/is-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       (t
        nil)))))

(defun bh/skip-non-subprojects ()
  "Skip trees that are not projects"
  (let ((next-headline (save-excursion (outline-next-heading))))
    (if (bh/is-subproject-p)
        nil
      next-headline)))

(setq org-archive-mark-done nil)

(defun bh/skip-non-archivable-tasks ()
  "Skip trees that are not available for archiving"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      ;; Consider only tasks with done todo headings as archivable candidates
      (if (member (org-get-todo-state) org-done-keywords)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (daynr (string-to-int (format-time-string "%d" (current-time))))
                 (a-month-ago (* 60 60 24 (+ daynr 1)))
                 (last-month (format-time-string "%Y-%m-" (time-subtract (current-time) (seconds-to-time a-month-ago))))
                 (this-month (format-time-string "%Y-%m-" (current-time)))
                 (subtree-is-current (save-excursion
                                       (forward-line 1)
                                       (and (< (point) subtree-end)
                                            (re-search-forward (concat last-month "\\|" this-month) subtree-end t)))))
            (if subtree-is-current
                next-headline ; Has a date in this month or last month, skip it
              nil))  ; available to archive
        (or next-headline (point-max))))))

(setq org-plantuml-jar-path "~/java/plantuml.jar")

(add-hook 'org-babel-after-execute-hook 'bh/display-inline-images 'append)

(defun bh/display-inline-images ()
  (condition-case nil
      (org-display-inline-images)
    (error nil)))

(org-babel-do-load-languages
 (quote org-babel-load-languages)
 (quote ((emacs-lisp . t)
         (dot . t)
         (ditaa . t)
         (R . t)
         (python . t)
         (ruby . t)
         (gnuplot . t)
         (clojure . t)
         (sh . t)
         (ledger . t)
         (org . t)
         (plantuml . t)
         (latex . t))))


; Use fundamental mode when editing plantuml blocks with C-c '
(add-to-list 'org-src-lang-modes (quote ("plantuml" . fundamental)))

;; Don't enable this because it breaks access to emacs from my Android phone
(setq org-startup-with-inline-images nil)

; experimenting with docbook exports - not finished
(setq org-export-docbook-xsl-fo-proc-command "fop %s %s")
(setq org-export-docbook-xslt-proc-command "xsltproc --output %s /usr/share/xml/docbook/stylesheet/nwalsh/fo/docbook.xsl %s")
;
; Inline images in HTML instead of producting links to the image
(setq org-export-html-inline-images t)
; Do not use sub or superscripts - I currently don't need this functionality in my documents
(setq org-export-with-sub-superscripts nil)
(setq org-export-html-style-include-default nil)
; Do not generate internal css formatting for HTML exports
(setq org-export-htmlize-output-type (quote css))
; Export with LaTeX fragments
(setq org-export-with-LaTeX-fragments t)

; List of projects
; norang       - http://www.norang.ca/
; doc          - http://doc.norang.ca/
; org-mode-doc - http://doc.norang.ca/org-mode.html and associated files
; org          - miscellaneous todo lists for publishing


; I'm lazy and don't want to remember the name of the project to publish when I modify
; a file that is part of a project.  So this function saves the file, and publishes
; the project that includes this file
;
; It's bound to C-S-F12 so I just edit and hit C-S-F12 when I'm done and move on to the next thing
(defun bh/save-then-publish ()
  (interactive)
  (save-buffer)
  (org-save-all-org-buffers)
  (org-publish-current-project))

(global-set-key (kbd "C-s-<f12>") 'bh/save-then-publish)


; Erase all reminders and rebuilt reminders for today from the agenda
(defun bh/org-agenda-to-appt ()
  (interactive)
  (setq appt-time-msg-list nil)
  (org-agenda-to-appt))

; Rebuild the reminders everytime the agenda is displayed
(add-hook 'org-finalize-agenda-hook 'bh/org-agenda-to-appt 'append)

; This is at the end of my .emacs - so appointments are set up when Emacs starts
(bh/org-agenda-to-appt)

; Activate appointments so we get notifications
(appt-activate t)

; If we leave Emacs running overnight - reset the appointments one minute after midnight
(run-at-time "24:01" nil 'bh/org-agenda-to-appt)

;; Enable abbrev-mode
(add-hook 'org-mode-hook (lambda () (abbrev-mode 1)))

;; Skeletons
;;
;; sblk - Generic block #+begin_FOO .. #+end_FOO
(define-skeleton skel-org-block
  "Insert an org block, querying for type."
  "Type: "
  "#+begin_" str "\n"
  _ - \n
  "#+end_" str "\n")

(define-abbrev org-mode-abbrev-table "sblk" "" 'skel-org-block)

;; splantuml - PlantUML Source block
(define-skeleton skel-org-block-plantuml
  "Insert a org plantuml block, querying for filename."
  "File (no extension): "
  "#+begin_src plantuml :file " str ".png :cache yes\n"
  _ - \n
  "#+end_src\n")

(define-abbrev org-mode-abbrev-table "splantuml" "" 'skel-org-block-plantuml)

;; sdot - Graphviz DOT block
(define-skeleton skel-org-block-dot
  "Insert a org graphviz dot block, querying for filename."
  "File (no extension): "
  "#+begin_src dot :file " str ".png :cache yes :cmdline -Kdot -Tpng\n"
  "graph G {\n"
  _ - \n
  "}\n"
  "#+end_src\n")

(define-abbrev org-mode-abbrev-table "sdot" "" 'skel-org-block-dot)

;; sditaa - Ditaa source block
(define-skeleton skel-org-block-ditaa
  "Insert a org ditaa block, querying for filename."
  "File (no extension): "
  "#+begin_src ditaa :file " str ".png :cache yes\n"
  _ - \n
  "#+end_src\n")

(define-abbrev org-mode-abbrev-table "sditaa" "" 'skel-org-block-ditaa)

;; selisp - Emacs Lisp source block
(define-skeleton skel-org-block-elisp
  "Insert a org emacs-lisp block"
  ""
  "#+begin_src emacs-lisp\n"
  _ - \n
  "#+end_src\n")

(define-abbrev org-mode-abbrev-table "selisp" "" 'skel-org-block-elisp)

(global-set-key (kbd "<f5>") 'bh/org-todo)

(defun bh/org-todo (rag)
  (interactive "p")
  (if (equal arg 4)
      (save-restriction
        (bh/narrow-to-org-subtree)
        (org-show-todo-tree nil))
    (bh/narrow-to-org-subtree)
    (org-show-todo-tree nil)))

(global-set-key (kbd "<S-f5>") 'bh/widen)

(defun bh/widen ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (org-agenda-remove-restriction-lock)
    (widen)
    (org-agenda-remove-restriction-lock)))

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "W" 'bh/widen))
          'append)

(defun bh/restrict-to-file-or-follow (arg)
  "Set agenda restriction to 'file or with argument invoke follow mode.
I don't use follow mode very often but I restrict to file all the time
so change the default 'F' binding in the agenda to allow both"
  (interactive "p")
  (if (equal arg 4)
      (org-agenda-follow-mode)
    (if (equal major-mode 'org-agenda-mode)
        (bh/set-agenda-restriction-lock 4)
      (widen))))

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "F" 'bh/restrict-to-file-or-follow))
          'append)

(defun bh/narrow-to-org-subtree ()
  (widen)
  (org-narrow-to-subtree))

(defun bh/narrow-to-subtree ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (org-with-point-at (org-get-at-bol 'org-hd-marker)
        (bh/narrow-to-org-subtree)
        (save-restriction
          (org-agenda-set-restriction-lock)))
    (bh/narrow-to-org-subtree)
    (save-restriction
      (org-agenda-set-restriction-lock))))

(add-hook 'org-agenda-mode-hook

          '(lambda () (org-defkey org-agenda-mode-map "N" 'bh/narrow-to-subtree))
          'append)

(defun bh/narrow-up-one-org-level ()
  (widen)
  (save-excursion
    (outline-up-heading 1 'invisible-ok)
    (bh/narrow-to-org-subtree)))

(defun bh/get-pom-from-agenda-restriction-or-point ()
  (or (org-get-at-bol 'org-hd-marker)
      (and (marker-position org-agenda-restrict-begin) org-agenda-restrict-begin)
      (and (equal major-mode 'org-mode) (point))
      org-clock-marker))

(defun bh/narrow-up-one-level ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (org-with-point-at (bh/get-pom-from-agenda-restriction-or-point)
        (bh/narrow-up-one-org-level))
    (bh/narrow-up-one-org-level)))

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "U" 'bh/narrow-up-one-level))
          'append)

(defun bh/narrow-to-org-project ()
  (widen)
  (save-excursion
    (bh/find-project-task)
    (bh/narrow-to-org-subtree)))

(defun bh/narrow-to-project ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (org-with-point-at (bh/get-pom-from-agenda-restriction-or-point)
        (bh/narrow-to-org-project)
        (save-restriction
          (org-agenda-set-restriction-lock)))
    (bh/narrow-to-org-project)
    (save-restriction
      (org-agenda-set-restriction-lock))))

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "P" 'bh/narrow-to-project))
          'append)

(defvar bh/current-view-project nil)

(defun bh/view-next-project ()
  (interactive)
  (unless (marker-position org-agenda-restrict-begin)
    (goto-char (point-min))
    (setq bh/current-view-project (point)))
  (bh/widen)
  (goto-char bh/current-view-project)
  (forward-visible-line 1)
  (while (and (< (point) (point-max))
              (or (not (org-get-at-bol 'org-hd-marker))
                  (org-with-point-at (org-get-at-bol 'org-hd-marker)
                    (or (not (bh/is-project-p))
                        (bh/is-project-subtree-p)))))
    (forward-visible-line 1))
  (setq bh/current-view-project (point))
  (if (org-get-at-bol 'org-hd-marker)
  (progn
        (bh/narrow-to-project)
        (org-agenda-redo)
        (beginning-of-buffer))
    (beginning-of-buffer)
    (error "All projects viewed.")))
 (add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "V" 'bh/view-next-project))
          'append)

(setq org-show-entry-below (quote ((default))))

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "\C-c\C-x<" 'bh/set-agenda-restriction-lock))
          'append)

(defun bh/set-agenda-restriction-lock (arg)
  "Set restriction lock to current task subtree or file if prefix is specified"
  (interactive "p")
  (let* ((pom (bh/get-pom-from-agenda-restriction-or-point))
         (tags (org-with-point-at pom (org-get-tags-at))))
    (let ((restriction-type (if (equal arg 4) 'file 'subtree)))
      (save-restriction
        (cond
         ((and (equal major-mode 'org-agenda-mode) pom)
          (org-with-point-at pom
            (org-agenda-set-restriction-lock restriction-type)))
         ((and (equal major-mode 'org-mode) (org-before-first-heading-p))
          (org-agenda-set-restriction-lock 'file))
         (pom
          (org-with-point-at pom
            (org-agenda-set-restriction-lock restriction-type))))))))

;; Always hilight the current agenda line
(add-hook 'org-agenda-mode-hook
          '(lambda () (hl-line-mode 1))
          'append)

;; Keep tasks with dates on the global todo lists
(setq org-agenda-todo-ignore-with-date nil)

;; Keep tasks with deadlines on the global todo lists
(setq org-agenda-todo-ignore-deadlines nil)

;; Keep tasks with scheduled dates on the global todo lists
(setq org-agenda-todo-ignore-scheduled nil)

;; Keep tasks with timestamps on the global todo lists
(setq org-agenda-todo-ignore-timestamp nil)


(setq org-agenda-include-diary nil)



;; Show all future entries for repeating tasks
(setq org-agenda-repeating-timestamp-show-all t)

;; Show all agenda dates - even if they are empty
(setq org-agenda-show-all-dates t)

;; Start the weekly agenda on Monday
(setq org-agenda-start-on-weekday 1)



;;
;; Agenda sorting functions
;;

(defun bh/agenda-sort (a b)
  "Sorting strategy for agenda items.
Late deadlines first, then scheduled, then non-late deadlines"
  (let (result num-a num-b)
    (cond
     ; time specific items are already sorted first by org-agenda-sorting-strategy

     ; non-deadline and non-scheduled items next
     ((bh/agenda-sort-test 'bh/is-not-scheduled-or-deadline a b))

     ; deadlines for today next
     ((bh/agenda-sort-test 'bh/is-due-deadline a b))

     ; late deadlines next
     ((bh/agenda-sort-test-num 'bh/is-late-deadline '< a b))

     ; scheduled items for today next
     ((bh/agenda-sort-test 'bh/is-scheduled-today a b))

     ; late scheduled items next
     ((bh/agenda-sort-test-num 'bh/is-scheduled-late '> a b))

     ; pending deadlines last
     ((bh/agenda-sort-test-num 'bh/is-pending-deadline '< a b))

     ; finally default to unsorted
     (t (setq result nil)))
    result))

(defmacro bh/agenda-sort-test (fn a b)
  "Test for agenda sort"
  `(cond
    ; if both match leave them unsorted
    ((and (apply ,fn (list ,a))
          (apply ,fn (list ,b)))
     (setq result nil))
    ; if a matches put a first
    ((apply ,fn (list ,a))
     (setq result -1))
    ; otherwise if b matches put b first
    ((apply ,fn (list ,b))
     (setq result 1))
    ; if none match leave them unsorted
    (t nil)))

(defmacro bh/agenda-sort-test-num (fn compfn a b)
  `(cond
    ((apply ,fn (list ,a))
     (setq num-a (string-to-number (match-string 1 ,a)))
     (if (apply ,fn (list ,b))
         (progn
           (setq num-b (string-to-number (match-string 1 ,b)))
           (setq result (if (apply ,compfn (list num-a num-b))
                            -1
                          1)))
       (setq result -1)))
    ((apply ,fn (list ,b))
     (setq result 1))
    (t nil)))

(defun bh/is-not-scheduled-or-deadline (date-str)
  (and (not (bh/is-deadline date-str))
       (not (bh/is-scheduled date-str))))

(defun bh/is-due-deadline (date-str)
  (string-match "Deadline:" date-str))

(defun bh/is-late-deadline (date-str)
  (string-match "In *\\(-.*\\)d\.:" date-str))

(defun bh/is-pending-deadline (date-str)
  (string-match "In \\([^-]*\\)d\.:" date-str))

(defun bh/is-deadline (date-str)
  (or (bh/is-due-deadline date-str)
      (bh/is-late-deadline date-str)
      (bh/is-pending-deadline date-str)))

(defun bh/is-scheduled (date-str)
  (or (bh/is-scheduled-today date-str)
      (bh/is-scheduled-late date-str)))

(defun bh/is-scheduled-today (date-str)
  (string-match "Scheduled:" date-str))

(defun bh/is-scheduled-late (date-str)
  (string-match "Sched\.\\(.*\\)x:" date-str))

(add-hook 'org-agenda-mode-hook
          (lambda ()
            (define-key org-agenda-mode-map "q" 'bury-buffer))
          'append)

;; The following setting is different from the document so that you
;; can override the document path by setting your path in the variable
;; org-mode-user-contrib-lisp-path
;;
;; (if (boundp 'org-mode-user-contrib-lisp-path)
;;     (add-to-list 'load-path org-mode-user-contrib-lisp-path)
;;   (add-to-list 'load-path (expand-file-name "~/git/dkh-org-mode/contrib/lisp")))

(require 'org-checklist)

(setq org-hide-leading-stars nil)

(setq org-insert-heading-respect-content nil)

(setq org-reverse-note-order nil)




(setq org-table-export-default-format "orgtbl-to-csv")




(setq org-log-state-notes-insert-after-drawers nil)

(setq org-clock-sound "/usr/local/lib/tngchime.wav")



(run-at-time "06:00" 86400 '(lambda () (setq org-habit-show-habits t)))

(require 'org-crypt)
; Encrypt all entries before saving
(org-crypt-use-before-save-magic)


(defun bh/show-org-agenda ()
  (interactive)
  (switch-to-buffer "*Org Agenda*")
  (delete-other-windows))

(require 'org-protocol)
(require 'org-mac-link-grabber)

(setq require-final-newline t)

(defun bh/insert-inactive-timestamp ()
  (interactive)
  (org-insert-time-stamp nil t t nil nil nil))

(defun bh/insert-heading-inactive-timestamp ()
  (save-excursion
    (org-return)
    (org-cycle)
    (bh/insert-inactive-timestamp)))

(add-hook 'org-insert-heading-hook 'bh/insert-heading-inactive-timestamp 'append)

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(org-mode-line-clock ((t (:foreground "red" :box (:line-width -1 :style released-button)))) t))

(defun bh/prepare-meeting-notes ()
  "Prepare meeting notes for email
   Take selected region and convert tabs to spaces, mark TODOs with leading >>>, and copy to kill ring for pasting"
  (interactive)
  (let (prefix)
    (save-excursion
      (save-restriction
        (narrow-to-region (region-beginning) (region-end))
        (untabify (point-min) (point-max))
        (goto-char (point-min))
        (while (re-search-forward "^\\( *-\\\) \\(TODO\\|DONE\\): " (point-max) t)
          (replace-match (concat (make-string (length (match-string 1)) ?>) " " (match-string 2) ": ")))
        (goto-char (point-min))
        (kill-ring-save (point-min) (point-max))))))


(setq org-tags-match-list-sublevels t)

(setq org-cycle-include-plain-lists t)

(defun bh/mark-next-parent-tasks-todo ()
  "Visit each parent task and change NEXT states to TODO"
  (let ((mystate (or (and (fboundp 'org-state)
                          state)
                     (nth 2 (org-heading-components)))))
    (when mystate
      (save-excursion
        (while (org-up-heading-safe)
          (when (member (nth 2 (org-heading-components)) (list "NEXT"))
            (org-todo "TODO")))))))

(add-hook 'org-after-todo-state-change-hook 'bh/mark-next-parent-tasks-todo 'append)
(add-hook 'org-clock-in-hook 'bh/mark-next-parent-tasks-todo 'append)

(setq org-startup-folded t)


(add-hook 'message-mode-hook 'orgstruct++-mode 'append)
(add-hook 'message-mode-hook 'turn-on-auto-fill 'append)
;;(add-hook 'message-mode-hook 'bbdb-define-all-aliases 'append)
(add-hook 'message-mode-hook 'orgtbl-mode 'append)
(add-hook 'message-mode-hook
          '(lambda () (setq fill-column 72))
          'append)
(add-hook 'message-mode-hook
          '(lambda () (local-set-key (kbd "C-c M-o") 'org-mime-htmlize))
          'append)

;; flyspell mode for spell checking everywhere
;; (add-hook 'org-mode-hook 'turn-on-flyspell 'append)

;; Disable C-c [ and C-c ] and C-c ; in org-mode
(add-hook 'org-mode-hook
          '(lambda ()
             ;; Undefine C-c [ and C-c ] since this breaks my
             ;; org-agenda files when directories are include It
             ;; expands the files in the directories individually
             (org-defkey org-mode-map "\C-c["    'undefined)
             (org-defkey org-mode-map "\C-c]"    'undefined)
             (org-defkey org-mode-map "\C-c;"    'undefined))
          'append)

(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c M-o") 'bh/mail-subtree))
          'append)

(defun bh/mail-subtree ()
  (interactive)
  (org-mark-subtree)
  (org-mime-subtree))

(setq org-enable-priority-commands t)

(setq org-e-html-inline-images t)
(setq org-e-html-style-extra "<link rel=\"stylesheet\" href=\"http://doc.norang.ca/org.css\" type=\"text/css\" />")
(setq org-e-html-style-include-default nil)
(setq org-e-html-xml-declaration
      (quote
       (("was-html" . "")
        ("html" . "<?xml version=\"1.0\" encoding=\"%s\"?>")
        ("php" . "<?php echo \"<?xml version=\\\"1.0\\\" encoding=\\\"%s\\\" ?>\"; ?>"))))
(setq org-e-latex-listings t)
(setq org-e-html-style-include-scripts nil)
(setq org-e-html-validation-link nil)

(setq org-export-coding-system 'utf-8)

(setq org-src-preserve-indentation nil)
(setq org-edit-src-content-indentation 0)

(setq org-catch-invisible-edits 'error)

(prefer-coding-system 'utf-8)
(set-charset-priority 'unicode)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

(setq org-odd-levels-only nil)

(defun bh/clock-in-bzflagt-task ()
  (interactive)
  (bh/clock-in-task-by-id "dcf55180-2a18-460e-8abb-a9f02f0893be"))

(defun bh/resume-clock ()
  (interactive)
  (if (marker-buffer org-clock-interrupted-task)
        (org-clock-in))
    (org-clock-out))

(run-at-time "00:59" 3600 'org-save-all-org-buffers)

(setq org-plantuml-jar-path nil)

; experimenting with docbook exports - not finished
(setq org-export-docbook-xsl-fo-proc-command "fop %s %s")
  ; Inline images in HTML instead of producting links to the image
  ; Do not use sub or superscripts - I currently don't need this functionality in my documents
  (setq org-export-with-sub-superscripts nil)
  ; Use org.css from the norang website for export document stylesheets
  ; Do not generate internal css formatting for HTML exports
  ; Export with LaTeX fragments
  (setq org-export-with-LaTeX-fragments t)



(defun org-link-to-named-task ()
  (interactive))

(fset 'org-link-to-named-task
   [?\C-  ?\C-  ?\C-e ?\C-w ?\C-s ?\M-y ?\C-a ?\M-f ?\C-c ?S ?\C-u ?\C-  ?\C-c ?\C-l return return ?\C-x ?\C-x ?\C-  ?\C- ])

(defun jump-to-org-agenda ()
  (interactive)
  ;; (let ((recordings-dir "~/Dropbox/iTalk Recordings"))
  ;;   (if (directory-files recordings-dir nil "\\`[^.]")
  ;;       (find-file recordings-dir)))
  (let ((buf (get-buffer "*Org Agenda*"))
        wind)
    (if buf
        (if (setq wind (get-buffer-window buf))
            (when (called-interactively-p 'any)
              (select-window wind)
              (org-fit-window-to-buffer))
          (if (called-interactively-p 'any)
              (progn
                (select-window (display-buffer buf t t))
                (org-fit-window-to-buffer))
            (with-selected-window (display-buffer buf)
              (org-fit-window-to-buffer))))
      (call-interactively 'org-agenda-list))))

(defun org-export-tasks ()
  (interactive)
  (let ((index 1))
    (org-map-entries
     #'(lambda ()
         (outline-mark-subtree)
         (org-export-as-html 3)
         (write-file (format "%d.html" index))
         (kill-buffer (current-buffer))
         (setq index (1+ index)))
     "LEVEL=2")))

(defun org-agenda-add-overlays (&optional line)
  "Add overlays found in OVERLAY properties to agenda items.
Note that habitual items are excluded, as they already
extensively use text properties to draw the habits graph.

For example, for work tasks I like to use a subtle, yellow
background color; for tasks involving other people, green; and
for tasks concerning only myself, blue.  This way I know at a
glance how different responsibilities are divided for any given
day.

To achieve this, I have the following in my todo file:

  * Work
    :PROPERTIES:
    :CATEGORY: Work
    :OVERLAY:  (face (:background \"#fdfdeb\"))
    :END:
  ** TODO Task
  * Family
    :PROPERTIES:
    :CATEGORY: Personal
    :OVERLAY:  (face (:background \"#e8f9e8\"))
    :END:
  ** TODO Task
  * Personal
    :PROPERTIES:
    :CATEGORY: Personal
    :OVERLAY:  (face (:background \"#e8eff9\"))
    :END:
  ** TODO Task

The colors (which only work well for white backgrounds) are:

  Yellow: #fdfdeb
  Green:  #e8f9e8
  Blue:   #e8eff9

To use this function, add it to `org-agenda-finalize-hook':

  (add-hook 'org-finalize-agenda-hook 'org-agenda-add-overlays)"
  (let ((inhibit-read-only t) l c
        (buffer-invisibility-spec '(org-link)))
    (save-excursion
      (goto-char (if line (point-at-bol) (point-min)))
      (while (not (eobp))
        (let ((org-marker (get-text-property (point) 'org-marker)))
          (when (and org-marker
                     (null (overlays-at (point)))
                     (not (get-text-property (point) 'org-habit-p))
                     (string-match "\\(sched\\|dead\\|todo\\)"
                                   (get-text-property (point) 'type)))
            (let ((overlays (org-entry-get org-marker "OVERLAY" t)))
              (when overlays
                (goto-char (line-end-position))
                (let ((rest (- (window-width) (current-column))))
                  (if (> rest 0)
                      (insert (make-string rest ? ))))
                (let ((ol (make-overlay (line-beginning-position)
                                        (line-end-position)))
                      (proplist (read overlays)))
                  (while proplist
                    (overlay-put ol (car proplist) (cadr proplist))
                    (setq proplist (cddr proplist))))))))
        (forward-line)))))

(autoload 'gnus-goto-article ".gnus")
(autoload 'gnus-string-remove-all-properties "gnus-util")

(defun org-my-message-open (message-id)
  (gnus-goto-article
   (gnus-string-remove-all-properties (substring message-id 2))))

;;(defun org-my-message-open (message-id)
;;  (condition-case err
;;      (if (get-buffer "*Group*")
;;          (gnus-goto-article
;;           (gnus-string-remove-all-properties (substring message-id 2)))
;;        (org-mac-message-open message-id))
;;    (error
;;     (org-mac-message-open message-id))))

(add-to-list 'org-link-protocols (list "message" 'org-my-message-open nil))


(defun save-org-mode-files ()
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (eq major-mode 'org-mode)
        (if (and (buffer-modified-p) (buffer-file-name))
            (save-buffer))))))

(run-with-idle-timer 25 t 'save-org-mode-files)

(defun my-org-push-mobile ()
  (interactive)
  (with-current-buffer (find-file-noselect "~/Documents/Tasks/todo.txt")
    (org-mobile-push)))

(eval-when-compile
  (defvar org-clock-current-task)
  (defvar org-mobile-directory)
  (defvar org-mobile-capture-file))

(defun org-my-auto-exclude-function (tag)
  (and (cond
        ((string= tag "call")
         (let ((hour (nth 2 (decode-time))))
           (or (< hour 8) (> hour 21))))
        ((string= tag "errand")
         (let ((hour (nth 2 (decode-time))))
           (or (< hour 12) (> hour 17))))
        ((or (string= tag "home") (string= tag "nasim"))
         (with-temp-buffer
           (call-process "/sbin/ifconfig" nil t nil "en0" "inet")
           (call-process "/sbin/ifconfig" nil t nil "en1" "inet")
           (call-process "/sbin/ifconfig" nil t nil "bond0" "inet")
           (goto-char (point-min))
           (not (re-search-forward "inet 192\\.168\\.9\\." nil t))))
        ((string= tag "net")
         (not (quickping "imap.gmail.com")))
        ((string= tag "fun")
         org-clock-current-task))
       (concat "-" tag)))

(defun my-mobileorg-convert ()
  (interactive)
  (while (re-search-forward "^\\* " nil t)
    (goto-char (match-beginning 0))
    (insert ?*)
    (forward-char 2)
    (insert "TODO ")
    (goto-char (line-beginning-position))
    (forward-line)
    (re-search-forward "^\\[")
    (goto-char (match-beginning 0))
    (let ((uuid
           (save-excursion
             (re-search-forward "^\\*\\* Note ID: \\(.+\\)")
             (prog1
                 (match-string 1)
               (delete-region (match-beginning 0)
                              (match-end 0))))))
      (insert (format "SCHEDULED: %s\n:PROPERTIES:\n"
                      (format-time-string (org-time-stamp-format))))
      (insert (format ":ID:       %s\n:CREATED:  " uuid)))
    (forward-line)
    (insert ":END:")))

(defun my-org-convert-incoming-items ()
  (interactive)
  (with-current-buffer
      (find-file-noselect (expand-file-name org-mobile-capture-file
                                            org-mobile-directory))
    (goto-char (point-min))
    (unless (eobp)
      (my-mobileorg-convert)
      (goto-char (point-max))
      (if (bolp)
          (delete-char -1))
      (let ((tasks (buffer-string)))
        (set-buffer-modified-p nil)
        (kill-buffer (current-buffer))
        (with-current-buffer (find-file-noselect "~/Documents/Tasks/todo.txt")
          (save-excursion
            (goto-char (point-min))
            (re-search-forward "^\\* Inbox$")
            (re-search-forward "^:END:")
            (forward-line)
            (goto-char (line-beginning-position))
            (if (and tasks (> (length tasks) 0))
                (insert tasks ?\n))))))))

;;;_Don't sync agendas.org to MobileOrg.  I do this because I only use
;;;_MobileOrg for recording new tasks on the phone, and never for viewing
;;;_tasks.  This allows MobileOrg to start up and sync extremely quickly.

;;(add-hook 'org-mobile-post-push-hook
;;          (function
;;           (lambda ()
;;             (shell-command "/bin/rm -f ~/Dropbox/MobileOrg/agendas.org")
;;             (shell-command
;;              (concat "perl -i -ne 'print unless /agendas\\.org/;'"
;;                      "~/Dropbox/MobileOrg/checksums.dat"))
;;             (shell-command
;;              (concat "perl -i -ne 'print unless /agendas\\.org/;'"
;;                      "~/Dropbox/MobileOrg/index.org")))))

(defun my-org-mobile-pre-pull-function ()
  (async-start
   (lambda ()
     (shell-command "open /Applications/Misc/Dropbox.app")
     (sleep-for 30)
     (shell-command "osascript -e 'tell application \"Dropbox\" to quit'"))
   (lambda (ret)
     (my-org-convert-incoming-items))))

(defun my-org-mobile-post-push-function ()
  (async-start
   (lambda ()
     (shell-command "open /Applications/Misc/Dropbox.app")
     (sleep-for 30)
     (shell-command "osascript -e 'tell application \"Dropbox\" to quit'"))
   'ignore))

(add-hook 'org-mobile-pre-pull-hook 'my-org-mobile-pre-pull-function)
(add-hook 'org-mobile-post-push-hook 'my-org-mobile-post-push-function)

(defun org-my-state-after-clock-out (state)
  (if (string= state "STARTED")
      "TODO"
    state))

(defvar org-my-archive-expiry-days 1
  "The number of days after which a completed task should be auto-archived.
This can be 0 for immediate, or a floating point value.")

(defconst org-my-ts-regexp
  "[[<]\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [^]>\r\n]*?\\)[]>]"
  "Regular expression for fast inactive time stamp matching.")

(defun org-my-closing-time ()
  (let* ((state-regexp
          (concat "- State \"\\(?:" (regexp-opt org-done-keywords)
                  "\\)\"\\s-*\\[\\([^]\n]+\\)\\]"))
         (regexp (concat "\\(" state-regexp "\\|" org-my-ts-regexp "\\)"))
         (end (save-excursion
                (outline-next-heading)
                (point)))
         begin
         end-time)
    (goto-char (line-beginning-position))
    (while (re-search-forward regexp end t)
      (let ((moment (org-parse-time-string (match-string 1))))
        (if (or (not end-time)
                (time-less-p (apply #'encode-time end-time)
                             (apply #'encode-time moment)))
            (setq end-time moment))))
    (goto-char end)
    end-time))

(defun org-my-archive-done-tasks ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((done-regexp
           (concat "^\\*\\* \\(" (regexp-opt org-done-keywords) "\\) ")))
      (while (re-search-forward done-regexp nil t)
        (if (>= (time-to-number-of-days
                 (time-subtract (current-time)
                                (apply #'encode-time (org-my-closing-time))))
                org-my-archive-expiry-days)
            (org-archive-subtree))))
    (save-buffer)))

(defun org-get-inactive-time ()
  (float-time (org-time-string-to-time
               (or (org-entry-get (point) "TIMESTAMP")
                   (org-entry-get (point) "TIMESTAMP_IA")
                   (debug)))))

(defun org-get-completed-time ()
  (let ((begin (point)))
    (save-excursion
      (outline-next-heading)
      (and (re-search-backward "\\(- State \"\\(DONE\\|DEFERRED\\|CANCELED\\)\"\\s-+\\[\\(.+?\\)\\]\\|CLOSED: \\[\\(.+?\\)\\]\\)" begin t)
           (float-time (org-time-string-to-time (or (match-string 3)
                                                    (match-string 4))))))))

(defun org-my-sort-done-tasks ()
  (interactive)
  (goto-char (point-min))
  (org-sort-entries t ?F #'org-get-inactive-time #'<)
  (goto-char (point-min))
  (while (re-search-forward "


+" nil t)
    (delete-region (match-beginning 0) (match-end 0))
    (insert "
"))
  (let (after-save-hook)
    (save-buffer))
  (org-overview))

(defun org-archive-done-tasks ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\* \\(DONE\\|CANCELED\\) " nil t)
      (if (save-restriction
            (save-excursion
              (error "Need to replace org-x-narrow-to-entry")
;;              (org-x-narrow-to-entry)
              (search-forward ":LOGBOOK:" nil t)))
          (forward-line)
        (org-archive-subtree)
        (goto-char (line-beginning-position))))))

(defun org-sort-all ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^\* " nil t)
      (goto-char (match-beginning 0))
      (condition-case err
          (progn
            (org-sort-entries t ?a)
            (org-sort-entries t ?p)
            (org-sort-entries t ?o)
            (forward-line))
        (error nil)))
    (goto-char (point-min))
    (while (re-search-forward "\* PROJECT " nil t)
      (goto-char (line-beginning-position))
      (ignore-errors
        (org-sort-entries t ?a)
        (org-sort-entries t ?p)
        (org-sort-entries t ?o))
      (forward-line))))

(defun org-cleanup ()
  (interactive)
  (org-archive-done-tasks)
  (org-sort-all)
  ;;(org-x-normalize-all-entries)
  )

(defvar my-org-wrap-region-history nil)

(defun my-org-wrap-region (&optional arg)
  (interactive "P")
  (save-excursion
    (goto-char (region-end))
    (if arg
        (insert "#+end_src\n")
      (insert ":END:\n"))
    (goto-char (region-beginning))
    (if arg
        (insert "#+begin_src "
                (read-string "Language: " nil 'my-org-wrap-region-history)
                ?\n)
      (insert ":OUTPUT:\n"))))

(defun org-inline-note ()
  (interactive)
  (switch-to-buffer-other-window "todo.txt")
  (goto-char (point-min))
  (re-search-forward "^\\* Inbox$")
  (re-search-forward "^:END:")
  (forward-line)
  (goto-char (line-beginning-position))
  (insert "** NOTE ")
  (save-excursion
    (insert (format "
:PROPERTIES:
:ID:       %s   :VISIBILITY: folded
:CREATED:  %s
:END:" (shell-command-to-string "uuidgen")
   (format-time-string (org-time-stamp-format t t))))
    (insert ?\n))
  (save-excursion
    (forward-line)
    (org-cycle)))

;;(defun org-get-apple-message-link ()
;;  (let ((subject (do-applescript "tell application \"Mail\"
;;        set theMessages to selection
;;        subject of beginning of theMessages
;;end tell"))
;;        (message-id (do-applescript "tell application \"Mail\"
;;        set theMessages to selection
;;        message id of beginning of theMessages
;;end tell")))
;;    (org-make-link-string (concat "message://" message-id) subject)))
;;
;;(defun org-get-message-sender ()
;;  (do-applescript "tell application \"Mail\"
;;        set theMessages to selection
;;        sender of beginning of theMessages
;;end tell"))
;;
;;(defun org-insert-apple-message-link ()
;;  (interactive)
;;  (insert (org-get-apple-message-link)))

(defun org-get-message-link (&optional title)
  (assert (get-buffer "*Group*"))
  (let (message-id subject)
    (with-current-buffer gnus-original-article-buffer
      (setq message-id (substring (message-field-value "message-id") 1 -1)
            subject (or title (message-field-value "subject"))))
    (org-make-link-string (concat "message://" message-id)
                          (rfc2047-decode-string subject))))

(defun org-insert-message-link (&optional arg)
  (interactive "P")
  (insert (org-get-message-link (if arg "writes"))))

(defun org-set-message-link ()
  "Set a property for the current headline."
  (interactive)
  (org-set-property "Message" (org-get-message-link)))

(defun org-get-message-sender ()
  (assert (get-buffer "*Group*"))
  (let (message-id subject)
    (with-current-buffer gnus-original-article-buffer
      (message-field-value "from"))))

(defun org-set-message-sender ()
  "Set a property for the current headline."
  (interactive)
  (org-set-property "Submitter" (org-get-message-sender)))

(defun org-get-safari-link ()
  (let ((subject (substring (do-applescript
                             (string-to-multibyte "tell application \"Safari\"
        name of document of front window
end tell")) 1 -1))
        (url (substring (do-applescript
                         (string-to-multibyte "tell application \"Safari\"
        URL of document of front window
end tell")) 1 -1)))
    (org-make-link-string url subject)))

(defun org-get-chrome-link ()
  (let ((subject (do-applescript
                  (string-to-multibyte "tell application \"Google Chrome\"
        title of active tab of front window
end tell")))
        (url (do-applescript
              (string-to-multibyte "tell application \"Google Chrome\"
        URL of active tab of front window
end tell"))))
    (org-make-link-string (substring url 1 -1) (substring subject 1 -1))))

(defun org-insert-url-link ()
  (interactive)
  (insert (org-get-chrome-link)))

(defun org-set-url-link ()
  "Set a property for the current headline."
  (interactive)
  (org-set-property "URL" (org-get-safari-link)))

;;(defun org-get-file-link ()
;;  (let ((subject (do-applescript "tell application \"Finder\"
;;      set theItems to the selection
;;      name of beginning of theItems
;;end tell"))
;;        (path (do-applescript "tell application \"Finder\"
;;      set theItems to the selection
;;      POSIX path of (beginning of theItems as text)
;;end tell")))
;;    (org-make-link-string (concat "file:" path) subject)))
;;
;;(defun org-insert-file-link ()
;;  (interactive)
;;  (insert (org-get-file-link)))
;;
;;(defun org-set-file-link ()
;;  "Set a property for the current headline."
;;  (interactive)
;;  (org-set-property "File" (org-get-file-link)))

(defun org-set-dtp-link ()
  "Set a property for the current headline."
  (interactive)
  (org-set-property "Document" (org-get-dtp-link)))

(defun org-dtp-message-open ()
  "Visit the message with the given MESSAGE-ID.
This will use the command `open' with the message URL."
  (interactive)
  (re-search-backward "\\[\\[message://\\(.+?\\)\\]\\[")
  (do-applescript
   (format "tell application \"DEVONthink Pro\"
        set searchResults to search \"%%3C%s%%3E\" within URLs
        open window for record (get beginning of searchResults)
end tell" (match-string 1))))

(fset 'orgify-line
   [?\C-k ?\C-o ?t ?o ?d ?o tab ?\C-y backspace ?\C-a ?l ?\C-u ?\C-n ?\C-n ?\C-n])

(add-hook 'org-log-buffer-setup-hook
          (lambda ()
            (setq fill-column (- fill-column 5))))

(defun org-message-reply ()
  (interactive)
  (let* ((org-marker (get-text-property (point) 'org-marker))
         (submitter (org-entry-get (or org-marker (point)) "Submitter"))
         (subject (if org-marker
                      (with-current-buffer (marker-buffer org-marker)
                        (goto-char org-marker)
                        (nth 4 (org-heading-components)))
                    (nth 4 (org-heading-components)))))
    (setq subject (replace-regexp-in-string "\\`(.*?) " "" subject))
    (compose-mail-other-window submitter (concat "Re: " subject))))

;;;_  . make-bug-link

(defun make-ledger-bugzilla-bug (product component version priority severity)
  (interactive
   (let ((omk (get-text-property (point) 'org-marker)))
     (with-current-buffer (marker-buffer omk)
       (save-excursion
         (goto-char omk)
         (let ((components
                (list "data" "doc" "expr" "lisp" "math" "python" "report"
                      "test" "util" "website" "build" "misc"))
               (priorities (list "P1" "P2" "P3" "P4" "P5"))
               (severities (list "blocker" "critical" "major"
                                 "normal" "minor" "trivial" "enhancement"))
               (product "Ledger")
               (version "3.0.0-20120217"))
           (list product
                 (ido-completing-read "Component: " components
                                      nil t nil nil (car (last components)))
                 version
                 (let ((orgpri (nth 3 (org-heading-components))))
                   (cond
                    ((and orgpri (= ?A orgpri))
                     "P1")
                    ((and orgpri (= ?C orgpri))
                     "P3")
                    (t
                     (ido-completing-read "Priority: " priorities
                                          nil t nil nil "P2"))))
                 (ido-completing-read "Severity: " severities nil t nil nil
                                      "normal") ))))))
  (let ((omk (get-text-property (point) 'org-marker)))
    (with-current-buffer (marker-buffer omk)
      (save-excursion
        (goto-char omk)
        (let ((heading (nth 4 (org-heading-components)))
              (contents (buffer-substring-no-properties
                         (org-entry-beginning-position)
                         (org-entry-end-position)))
              bug)
          (with-temp-buffer
            (insert contents)
            (goto-char (point-min))
            (delete-region (point) (1+ (line-end-position)))
            (search-forward ":PROP")
            (delete-region (match-beginning 0) (point-max))
            (goto-char (point-min))
            (while (re-search-forward "^   " nil t)
              (delete-region (match-beginning 0) (match-end 0)))
            (goto-char (point-min))
            (while (re-search-forward "^SCHE" nil t)
              (delete-region (match-beginning 0) (1+ (line-end-position))))
            (goto-char (point-min))
            (when (eobp)
              (insert "No description.")
              (goto-char (point-min)))
            (insert (format "Product: %s
Component: %s
Version: %s
Priority: %s
Severity: %s
Hardware: Other
OS: Other
Summary: %s" product component version priority severity heading) ?\n ?\n)
            (let ((buf (current-buffer)))
              (with-temp-buffer
                (let ((tmpbuf (current-buffer)))
                  (if nil
                      (insert "Bug 999 posted.")
                    (with-current-buffer buf
                      (shell-command-on-region
                       (point-min) (point-max)
                       "~/bin/bugzilla-submit http://bugs.ledger-cli.org/"
                       tmpbuf)))
                  (goto-char (point-min))
                  (or (re-search-forward "Bug \\([0-9]+\\) posted." nil t)
                      (debug))
                  (setq bug (match-string 1))))))
          (save-excursion
            (org-back-to-heading t)
            (re-search-forward "\\(TODO\\|DEFERRED\\|STARTED\\|WAITING\\|DELEGATED\\) \\(\\[#[ABC]\\] \\)?")
            (insert (format "[[bug:%s][#%s]] " bug bug)))))))
  (org-agenda-redo))

(defun make-bug-link ()
  (interactive)
  (let* ((omk (get-text-property (point) 'org-marker))
         (path (with-current-buffer (marker-buffer omk)
                 (save-excursion
                   (goto-char omk)
                   (org-get-outline-path)))))
    (cond
     ((string-match "/ledger/" (buffer-file-name (marker-buffer omk)))
      (call-interactively #'make-ledger-bugzilla-bug))
     ((string= "BoostPro" (car path))
      (call-interactively #'org-x-redmine-post-issue))
     (t
      (error "Cannot make bug, unknown category")))))

(bind-key "C-c x b"
          (lambda (bug)
            (interactive "sBug: ")
            (insert (format "[[fpco:%s][fpco#%s]]" bug bug))))
(bind-key "C-c x e" 'org-export)
(bind-key "C-c x l" 'org-insert-dtp-link)
(bind-key "C-c x L" 'org-set-dtp-link)
(bind-key "C-c x m" 'org-insert-message-link)
(bind-key "C-c x M" 'org-set-message-link)
(bind-key "C-c x u" 'org-insert-url-link)
(bind-key "C-c x U" 'org-set-url-link)
(bind-key "C-c x f" 'org-insert-file-link)
(bind-key "C-c x F" 'org-set-file-link)

(autoload 'ledger-test-create "ldg-test" nil t)
(autoload 'ledger-test-run "ldg-test" nil t)

(add-to-list 'auto-mode-alist '("\\.test$" . ledger-mode))

(org-defkey org-mode-map [(control meta return)]
            'org-insert-heading-after-current)
(org-defkey org-mode-map [(control return)] 'other-window)
(org-defkey org-mode-map [return] 'org-return-indent)
(org-defkey org-mode-map
            [(control ?c) (control ?x) ?@] 'visible-mode)
(org-defkey org-mode-map [(control ?c) (meta ?m)] 'my-org-wrap-region)

(defvar my-org-expand-map)
(define-prefix-command 'my-org-expand-map)
(define-key org-mode-map [(control ?c) (control ?e)] 'my-org-expand-map)

(define-key my-org-expand-map [(control ?t)] 'ledger-test-create)

(eval-when-compile
  (defvar yas/trigger-key)
  (defvar yas/keymap)

  (autoload 'yas/expand "yasnippet"))

(defun yas/org-very-safe-expand ()
  (let ((yas/fallback-behavior 'return-nil)) (yas/expand)))

(add-hook 'org-mode-hook
          (lambda ()
            (require 'yasnippet)
            (set (make-local-variable 'yas/trigger-key) [tab])
            (add-to-list 'org-tab-first-hook 'yas/org-very-safe-expand)
            (define-key yas/keymap [tab] 'yas/next-field-or-maybe-expand)))

(remove-hook 'kill-emacs-hook 'org-babel-remove-temporary-directory)

(defun org-fit-agenda-window ()
  "Fit the window to the buffer size."
  (and (memq org-agenda-window-setup '(reorganize-frame))
       (fboundp 'fit-window-to-buffer)
       (fit-window-to-buffer)))


(defadvice org-agenda-redo (after fit-windows-for-agenda-redo activate)
  "Fit the Org Agenda to its buffer."
  (org-fit-agenda-window))

(defadvice org-agenda (after fit-windows-for-agenda activate)
  "Fit the Org Agenda to its buffer."
  (org-fit-agenda-window))

(add-hook 'org-mode-hook
          'turn-on-visual-line-mode
          )

(when (fboundp 'adaptive-wrap-prefix-mode)
  (defun my-activate-adaptive-wrap-prefix-mode ()
    "Toggle `visual-line-mode' and `adaptive-wrap-prefix-mode' simultaneously."
    (if visual-line-mode
        (adaptive-wrap-prefix-mode 1)
      (adaptive-wrap-prefix-mode -1)))
  (add-hook 'visual-line-mode-hook 'my-activate-adaptive-wrap-prefix-mode))

(defun myorg-update-parent-cookie ()
  (when (equal major-mode 'org-mode)
    (save-excursion
      (ignore-errors
        (org-back-to-heading)
        (org-update-parent-todo-statistics)))))

(defadvice org-kill-line (after fix-cookies activate)
  (myorg-update-parent-cookie))

(defadvice kill-whole-line (after fix-cookies activate)
  (myorg-update-parent-cookie))

(if (require 'artbollocks-mode nil t)
    (progn
      (setq weasel-words-regex
            (concat "\\b" (regexp-opt
                           '("one of the"
                             "should"
                             "just"
                             "sort of"
                             "a lot"
                             "probably"
                             "maybe"
                             "perhaps"
                             "I think"
                             "really"
                             "pretty"
                             "maybe"
                             "nice"
                             "action"
                             "utilize"
                             "leverage") t) "\\b"))
      ;; Fix a bug in the regular expression to catch repeated words
      (setq lexical-illusions-regex "\\b\\(\\w+\\)\\W+\\(\\1\\)\\b")
      ;; Don't show the art critic words, or at least until I figure
      ;; out my own jargon
      (setq artbollocks nil)
      (add-hook 'org-capture-mode-hook 'artbollocks-mode)

      ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Org Capture template configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; support functions

(defun org-build-note-webpage ()
  "Define the structure of a capture note for an external link"
  (let ((title (plist-get org-store-link-plist :description))
        (link  (plist-get org-store-link-plist :link))
        (time  (format-time-string "[%Y-%m-%d %a]" (current-time)))
        (text  (plist-get org-store-link-plist :initial))
        output)
    (with-temp-buffer
      (insert (concat
               "* “" title "” :webpage:\n"
               "  "  time "\n\n"
               "  %?\n\n"
               "  --- Source: [[" link "][" title "]]" "\n\n  "))
      ;;(set-fill-prefix)
      (insert text)
      (set-fill-column 70)
      (fill-paragraph 'full)
      (setq output (buffer-string)))
    output))

(defun org-build-note-gmail ()
  "Define the structure of a capture note for an gmail link"
  (let ((title (plist-get org-store-link-plist :description))
        (link  (plist-get org-store-link-plist :link))
        (time  (format-time-string "[%Y-%m-%d %a]" (current-time)))
        (text  (plist-get org-store-link-plist :initial))
        msgid
        output)
    ;; Get the message ID
    (setq msgid (save-match-data
                  (string-match "\/\\([0-9a-f]*\\)\$" link)
                  (match-string 1 link)))
    ;; Build the template
    (with-temp-buffer
      (insert (concat
               "* “" title "” :mail:\n"
               "  "  time "\n\n"
               "  %?\n\n"
               "  --- Source: [[gmail:" msgid "][link to message]]" "\n\n  "))
      ;;(set-fill-prefix)
      (insert text)
      (set-fill-column 70)
      (fill-paragraph 'full)
      (setq output (buffer-string)))
    output))

(defun org-build-note-auto ()
  "Define the structure of a capture note for an gmail link"
  (let ((link  (plist-get org-store-link-plist :link)))
    ;; Get the message ID
    (cond ((string-match "^https?://mail\\.google\\.com/mail" (plist-get org-store-link-plist :link))
           (org-build-note-gmail))
          (t
           (org-build-note-webpage)))
     ))


;; Normally my private (and translated) configuration is used.
;; (when (not (boundp 'org-capture-templates))
;;   (setq org-capture-templates
;;         '(
;;           ;; task
;;           ("t" "task" entry (file "agenda.org")
;;            "* TODO ⌚ %?\n  %U\n\n  %i\n\n")
;;           ;; meeting and events
;;           ("e" "event" entry (file "agenda.org")
;;            "* %?\n  WHEN %^t\n  %i\n\n")
;;           ;; notebook
;;           ("n" "note" entry (file "notebook.org")
;;            "* %?\n  %U\n  %i\n  %a\n\n")
;;           ;; org-capture from the web
;;           ("w" "webpage" entry (file "agenda.org") "%(org-build-note-auto)")
;;           ("y" "macprotocol" entry (file "agenda.org") "%(org-build-note-auto)")
;;           )))

(setq org-default-capture-template "t")

(add-to-list 'org-capture-templates
             `
             ("j" "Journal" entry (file+datetree "~/Documents/Tasks/diary.org")
              "* %?%^G\n Entered on %T\n   %i\n"
              :clock-in t
              :clock-resume t))

;;              ("j" "Journal" entry (file+datetree "~/Documents/Tasks/diary.org")
;;               "* %?\n%U\n" :clock-in t :clock-resume t)

(provide 'dot-org)

;; Local Variables:
;;   mode: emacs-lisp
;;   mode: allout
;;   outline-regexp: "^;;;_\\([,. ]+\\)"
;; End:

;;; dot-org.el ends here
