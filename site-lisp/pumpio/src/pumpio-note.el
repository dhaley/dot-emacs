
;;; pumpio-note.el --- 

;; Copyright 2013 Christian
;;
;; Author: cnngimenez@lavabit.com
;; Version: $Id: pumpio-message.el,v 0.0 2013/06/28 15:42:03 cng Exp $
;; Keywords: 
;; X-URL: not distributed yet

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; 

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'pumpio-message)

;;; Code:

(provide 'pumpio-note)

(require 'cl)
(require 'json)

(require 'pumpio-comment)

(defconst pmpio-note-objecttype "note"
  "This is the objectType field" )

(defstruct pmpio-note 
  uuid author content url updated published comments)

(defun pmpio-note-add-comment (note comment)
  "Add one COMMENT into the NOTE."
  (when (and 
	 (pmpio-comment-p comment)
	 (pmpio-note-p note))      
    (let ((lst-com (pmpio-note-comments note))
	  )
      (setf (pmpio-note-comments note) (add-to-list 'lst-com comment t))
      )  
    )
  )

(defun pmpio-note-write (note)
  "Write in the current buffer the given NOTE."
  (insert (format "\n%s at %s: \n%s" 
		  (pmpio-note-author note)
		  (pmpio-note-published note)
		  (pmpio-note-content note)
		  )
	  )
  )

(defun pmpio-note-json (note)
  "Translate this NOTE abstraction into a JSON string."
  (json-encode 
   (list (cons 'objectType pmpio-note-objecttype)
	 (cons 'content (pmpio-note-content note))))
  )

(defun pmpio-note-as-activity-json (nickname note &optional to cc)
  "Same as `pmpio-note-json' but considering this as an activity.

NICKNAME is the sender username.
If TO is nil or not setted, the public collection will be used.
If CC is nil or not setted, the followers collection will be used.

Example:

  (pmpio-note-as-activity-json (make-pmpio-note :content \"Hi world!!! :-P\")
			       '(\"acct:alice@microca.st\" \"acct:bob@pumpio.net\" 'public 'followers)) 
"

  (json-encode
   (list (cons 'verb "post")
	 (cons 'object (list
			(cons 'objectType "note")
			(cons 'content (pmpio-note-content note)))
	       )
	 (cons 'to 
	       (if to
		   (pmpio-note-construct-destinataries nickname to)
		 (pmpio-note-construct-destinataries nickname 'public)
		 )
	       )
	 (cons 'cc	       
	       (if cc
		   (pmpio-note-construct-destinataries nickname cc)
		 (pmpio-note-construct-destinataries nickname 'followers)
		 )
	       )
	 )
   )
  )


(defun pmpio-note-construct-destinataries (nickname lst-dest)
  "
Returns a vector of people ready for JSON encode.

LST-DEST can be:
- A string with the account address.
- A list of string with accounts address.
- A list of string with accounts address and collection names as symbols.
- A symbol with as representation of a collection name.

Collection names are symbols and can be one of the following:

- 'followers
- 'public

See `pmpio-url-collections' for more collection names.
"
  (cond
   ((stringp lst-dest) ;; Is a string
    (vector 
     (list (cons 'objectType "person")
	   (cons 'id lst-dest)))
    )
   
    ((listp lst-dest) ;; Is a mixed list of string or symbols
     (let ((res []))
       (dolist (elt lst-dest)
	 (setq res
	       (vconcat
		res
		(if (stringp elt)
		    (vector (list (cons 'objectType "person")
				  (cons 'id elt)))
		  (vector (list (cons 'objectType "collection")
				(cons 'id (pmpio-url-of-collection nickname elt))))))
	       )
	 )
       res
       )
     )
     
    ((symbolp lst-dest) ;; Just one symbol
     (vector
      (list (cons 'objectType "collection")
	    (cons 'id (pmpio-url-of-collection nickname lst-dest))))
     )
    ))

;;; pumpio-message.el ends here
