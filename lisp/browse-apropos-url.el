;; Don't know if it's the best way , but it seemed to work. (Requires emacs >= 20)
(defun browse-apropos-url (text &optional new-window)
  (interactive (browse-url-interactive-arg "Location: "))
  (let ((text (replace-regexp-in-string 
               "^ *\\| *$" "" 
               (replace-regexp-in-string "[ \t\n]+" " " text)))
        ___braplast)
    (let ((url (or (assoc-if
                    (lambda (a) (string-match a text))
                    apropos-url-alist)
                   text)))
      (browse-url (replace-regexp-in-string (car url) (cdr url) text) new-window))))

(setq apropos-url-alist
      '(("^gw?:? +\\(.*\\)" . ;; Google Web 
         "http://www.google.com/search?q=\\1")

        ("^g!:? +\\(.*\\)" . ;; Google Lucky
         "http://www.google.com/search?btnI=I%27m+Feeling+Lucky&q=\\1")
        
        ("^gl:? +\\(.*\\)" .  ;; Google Linux 
         "http://www.google.com/linux?q=\\1")
        
        ("^gi:? +\\(.*\\)" . ;; Google Images
         "http://images.google.com/images?sa=N&tab=wi&q=\\1")

        ("^gg:? +\\(.*\\)" . ;; Google Groups
         "http://groups.google.com/groups?q=\\1")

        ("^gd:? +\\(.*\\)" . ;; Google Directory
         "http://www.google.com/search?&sa=N&cat=gwd/Top&tab=gd&q=\\1")

        ("^gn:? +\\(.*\\)" . ;; Google News
         "http://news.google.com/news?sa=N&tab=dn&q=\\1")

        ("^gt:? +\\(\\w+\\)|? *\\(\\w+\\) +\\(\\w+://.*\\)" . ;; Google Translate URL
         "http://translate.google.com/translate?langpair=\\1|\\2&u=\\3")
        
        ("^gt:? +\\(\\w+\\)|? *\\(\\w+\\) +\\(.*\\)" . ;; Google Translate Text
         "http://translate.google.com/translate_t?langpair=\\1|\\2&text=\\3")

        ("^/\\.$" . ;; Slashdot 
         "http://www.slashdot.org")

        ("^/\\.:? +\\(.*\\)" . ;; Slashdot search
         "http://www.osdn.com/osdnsearch.pl?site=Slashdot&query=\\1")        
        
        ("^fm$" . ;; Freshmeat
         "http://www.freshmeat.net")

        ("^ewiki:? *?\\(.*\\)" . ;; Emacs Wiki Search
         "http://www.emacswiki.org/cgi-bin/wiki?search=\\1")
 
        ("^ewiki$" . ;; Emacs Wiki 
         "http://www.emacswiki.org")

        ("^arda$" . ;; The Encyclopedia of Arda 
         "http://www.glyphweb.com/arda/")
         
         ))

(provide 'browse-apropos-url)


;; Local Variables:
;;   mode: emacs-lisp
;;   mode: allout
;;   outline-regexp: "^;;;_\\([,. ]+\\)"
;; End:

;;; dkh-gnus.el ends here
