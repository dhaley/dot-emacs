;;; Compiled snippets and support files for `text-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'text-mode
                     '(("email" "`(replace-regexp-in-string \"@\" \"@NOSPAM.\" user-mail-address)`" "(user's email)" nil nil nil nil nil nil)
                       ("fail" "［ ］Pass［✔］Fail" "［✔］Fail" nil nil nil nil nil nil)
                       ("fliptable" "(╯°□°）╯︵ ┻━┻" "(╯°□°）╯︵ ┻━┻" nil nil nil nil nil nil)
                       ("flower" "♡(✿ˇ◡ˇ)人(ˇ◡ˇ✿)♡" "♡(✿ˇ◡ˇ)人(ˇ◡ˇ✿)♡" nil nil nil nil nil nil)
                       ("hug" "ヽ(´ー｀)ノ" "ヽ(´ー｀)ノ" nil nil nil nil nil nil)
                       ("hug1" "(づ｡◕‿◕｡)づ" "(づ｡◕‿◕｡)づ" nil nil nil nil nil nil)
                       ("hug2" "(づ｡◕‿‿‿‿◕｡)づ" "(づ｡◕‿‿‿‿◕｡)づ" nil nil nil nil nil nil)
                       ("kiss" "( ¯3¯)" "( ¯3¯)" nil nil nil nil nil nil)
                       ("lod" "ಠ_ಠ" "ಠ_ಠ" nil nil nil nil nil nil)
                       ("lose" "［ ］Win［✔］Lose" "［✔］Lose" nil nil nil nil nil nil)
                       ("mad" "(╬ ಠ益ಠ)" "(╬ ಠ益ಠ)" nil nil nil nil nil nil)
                       ("no" "［ ］Yes［✔］No" "［✔］No" nil nil nil nil nil nil)
                       ("pass" "［✔］Pass［ ］Fail" "［✔］Pass" nil nil nil nil nil nil)
                       ("smile" "(｡◕‿◕｡)" "(｡◕‿◕｡)" nil nil nil nil nil nil)
                       ("spider" "/╲/\\╭ºoꍘoº╮/\\╱\\" "/╲/\\╭ºoꍘoº╮/\\╱\\" nil nil nil nil nil nil)
                       ("supson" "¯\\_(ツ)_/¯" "¯\\_(ツ)_/¯" nil nil nil nil nil nil)
                       ("time" "`(current-time-string)`" "(current time)" nil nil nil nil nil nil)
                       ("win" "［✔］Win［ ］Lose" "［✔］Win" nil nil nil nil nil nil)
                       ("xd" "( ˃ ヮ˂)" "( ˃ ヮ˂)" nil nil nil nil nil nil)
                       ("yes" "［✔］Yes［ ］No" "［✔］Yes" nil nil nil nil nil nil)
                       ("yudothis" "щ(ﾟДﾟщ)" "щ(ﾟДﾟщ)" nil nil nil nil nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'text-mode
                     '(("agenda" "***** NOTE BoostPro status meeting agenda for `(with-temp-buffer (org-insert-time-stamp (current-time) nil t))`\n$0\n:PROPERTIES:\n:ID:       `(shell-command-to-string \"uuidgen\")`:CREATED: `(with-temp-buffer (org-insert-time-stamp (current-time) t t))`\n:END:" "agenda" nil nil nil nil nil nil)
                       ("assem" "* NOTE Assembly meeting agenda for `(with-temp-buffer (org-insert-time-stamp (current-time) nil t))`\n:PROPERTIES:\n:ID:       `(shell-command-to-string \"uuidgen\")`:CREATED: `(with-temp-buffer (org-insert-time-stamp (current-time) t t))`\n:OVERLAY: (face (:background \"#e8f9e8\"))\n:END:\n** Opening Prayers\n** Attendance [0/9]\n- [$0 ] Caroline Delaney\n- [ ] Sisi Mereness\n- [ ] John Tempey\n- [ ] Pamela Fox\n- [ ] Nami Peymani\n- [ ] Beth Youker-Schwab\n- [ ] Gail Hill\n- [ ] Christina Stone\n- [ ] John Wiegley\n** Review prior minutes of [2013-01-01 Tue]\n** Outstanding business from [2013-01-01 Tue]\n** Current Business\n** Additions to the Agenda\n** Teaching\n** Upcoming events\n** Secretary's report\n** Feast suggestions\n** Treasurer's Report\n** Unfinished business\n** Consultation\n** New Business\n** Affairs of the Local Spiritual Assembly\n*** Children's Education: Mary Anne, Sisi, Keith, Nami\n*** International Pioneering: Marjene Streitmatter\n*** Teaching Committee: Mary Anne, Christina, JohnW, Sisi\n*** Center Improvement Committee: Beth, Nami\n** Next Assembly meeting is [2013-01-01 Tue 00:00]" "assem" nil nil nil nil nil nil)
                       ("author" "#+AUTHOR: ${1:`user-full-name`}\n" "Author" nil nil nil nil nil nil)
                       ("block" "" "block" nil nil nil nil nil nil)
                       ("desc" "#+DESCRIPTION: ${0}\n" "DESCRIPTION" nil nil nil nil nil nil)
                       ("dita" "#+BEGIN_DITAA ${1:export-file-name} -r -S -E\n${0}\n#+END_DITAA\n" "DITAA" nil nil nil nil nil nil)
                       ("docbook" "#+BEGIN_DOCBOOK\n${0}\n#+END_DOCBOOK\n" "DOCBOOK" nil nil nil nil nil nil)
                       ("email" "#+EMAIL: ${1:`user-mail-address`}\n" "Email" nil nil nil nil nil nil)
                       ("ex" "#+BEGIN_EXAMPLE\n$0\n#+END_EXAMPLE\n" "example (#+BEGIN_EXAMPLE...)" nil nil nil nil nil nil)
                       ("html" "#+BEGIN_HTML\n${0}\n#+END_HTML\n" "HTML" nil nil nil nil nil nil)
                       ("inc" "#+INCLUDE: \"${1:file}\" ${2:src-example-quote} ${3:mode}\n\n" "Author" nil nil nil nil nil nil)
                       ("keywords" "#+KEYWORDS: ${0}\n" "KEYWORDS" nil nil nil nil nil nil)
                       ("lang" "#+LANGUAGE: ${1:en}\n" "LANGUAGE" nil nil nil nil nil nil)
                       ("latex" "#+BEGIN_LATEX\n${0}\n#+END_LATEX\n" "LATEX" nil nil nil nil nil nil)
                       ("list" "*** NOTE $1 [/]\n- [ ] $0\n:PROPERTIES:\n:ID:       `(shell-command-to-string \"uuidgen\")`:CREATED: `(with-temp-buffer (org-insert-time-stamp (current-time) t t))`\n:END:" "List NOTE" nil nil nil nil nil nil)
                       ("note" "NOTE $0\n:PROPERTIES:\n:ID:       `(shell-command-to-string \"uuidgen\")`:CREATED: `(with-temp-buffer (org-insert-time-stamp (current-time) t t))`\n:END:" "NOTE" nil nil nil nil nil nil)
                       ("options" "#+OPTIONS: ${0}\n\n" "OPTIONS" nil nil nil nil nil nil)
                       (nil ":OUTPUT:\n$0:END:\n" ":OUTPUT: ... :END:" nil nil nil nil "C-c C-e C-o" nil)
                       ("prop" ":PROPERTIES:$>\n$0$>\n:END:$>\n" "prop (:PROPERTIES:....:END:)" nil nil nil nil nil nil)
                       ("res" "#+RESNAME:\n\n" "Org-Babel RESNAME Block" nil nil nil nil nil nil)
                       ("sb" "#+srcname: ${1:name}\n#+begin_src ${2:language} $3\n  $0\n#+end_src\n" "#+srcname:..#+begin_src...#+end_src" nil nil
                        ((yas/indent-line 'fixed)
                         (yas/wrap-around-region 'nil))
                        nil nil nil)
                       ("seq" "#+SEQ_TODO: ${1:STATES} | ${2:FINISHED}\n" "SEQ TODO" nil nil nil nil nil nil)
                       ("sh" "#+begin_src sh\n$0#+end_src\n" "sh" nil nil nil nil nil nil)
                       ("src" "#+BEGIN_SRC ${1:$$(yas/choose-value (reverse (mapcar (lambda (x) (symbol-name (car x))) org-babel-load-languages)))}\n$0\n#+END_SRC" "src (#+BEGIN_SRC...)" nil nil nil nil nil nil)
                       ("startup" "#+STARTUP: ${1:options}\n" "Startup" nil nil nil nil nil nil)
                       ("tags" "#+TAGS: $0\n" "Tags" nil nil nil nil nil nil)
                       ("text" "#+TEXT: ${1:text}\n" "Text" nil nil nil nil nil nil)
                       ("title" "#+TITLE: ${1:title}\n" "Title Block" nil nil nil nil nil nil)
                       ("todo" "TODO $0\nSCHEDULED: `(with-temp-buffer (org-insert-time-stamp (current-time)))`\n:PROPERTIES:\n:ID:       `(shell-command-to-string \"uuidgen\")`:CREATED: `(with-temp-buffer (org-insert-time-stamp (current-time) t t))`\n:END:" "TODO" nil nil nil nil nil nil)))


;;; Do not edit! File generated at Wed Jan 23 16:24:23 2013
