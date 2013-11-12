;;; pumpio-http.el --- 

;; Copyright 2013 Christian
;;
;; Author: cnngimenez@lavabit.com
;; Version: $Id: pumpio-http.el,v 0.0 2013/06/28 15:37:43 cng Exp $
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
;;   (require 'pumpio-http)

;;; Code:

(provide 'pumpio-http)

(require 'pumpio-comment)
(require 'pumpio-note)
(require 'pumpio-user)
(require 'pumpio-activity)
(require 'pumpio-urls)

(require 'url)
(require 'json)
(require 'oauth)

(defvar pmpio-client-id nil
  "Client id for oauth authentication. This will be automatically setted by `pmpio-http-register-client' and its callback.")
(defvar pmpio-client-secret nil
  "Client secret for oauth authentication. This will be automatically setted by `pmpio-http-register-client' and its callback.")
(defvar pmpio-auth-token nil
  "Authorization token given by `oauth-authorize-app' when user has successfully finished the OAuth process.")

(defvar pmpio-http-user-data nil
  "User data abstraction created with `make-pmpio-user'. This variable is filled using the function `pmpio-http-store-whoami'.")

(defun pmpio-http-assign-functions ()
  "Assign to the API the funcions needed for using pump-http."

  (setq pmpio-is-registered-p-hook 'pmpio-http-is-registered-p)
  (setq pmpio-register-hook 'pmpio-http-register-client)
  (setq pmpio-get-note-hook 'pmpio-http-get-note)
  (setq pmpio-get-major-feed-hook 'pmpio-http-get-major-feed)
  (setq pmpio-post-note-hook 'pmpio-http-post-note)
  )

(defun pmpio-http-is-registered-p ()
  "Return t if the OAuth authentication has been made."
  (if pmpio-auth-token
      t
    nil)
  )

(defun pmpio-http-register-client ()
  "Register the client dinamically."
  
  (let ((url-request-method "POST")
	(url-request-extra-headers	 
	 '(("Content-Type" . "application/json")
	   ("Accept-Charset" . "utf-8"))
	 )	  
	(buffer-file-coding-system 'utf-8)
	(url-request-data  "{ \"type\": \"client_associate\", \"application_name\": \"pumpio-el\", \"application_type\": \"native\" }")
	)
    (url-retrieve (pmpio-url-get-client-register) 'pmpio-http-process-registration-callback)  
    )
  )

(defun pmpio-http-get-note (uuid fnc)
  "Make a get request for the UUID.

When the note is completely retrieved, I call the given function FNC.

FNC has to recieve one parameter: the note loaded."
  
  (let ((url-request-extra-headers 
	 '(("Accept-Charset" . "utf-8")))
	(buffer-file-coding-system 'utf-8)
	)
    (oauth-url-retrieve pmpio-auth-token (pmpio-url-get-note uuid) 'pmpio-http-parse-note-callback (list fnc))
    )  
  )

(defun pmpio-http-get-major-feed (nickname fnc)
  "Get the Major Feed for the user with NICKNAME.

FNC is the callback function and must have one parameter: the activity list"
  (let ((url-request-extra-headers 
	 '(("Accept-Charset" . "utf-8")))
	(buffer-file-coding-system 'utf-8)
	)
    (oauth-url-retrieve pmpio-auth-token 
			(pmpio-url-get-major-feed nickname)
			'pmpio-http-get-major-feed-callback (list fnc))
    )  
  )

(defun pmpio-http-post-note (note fnc &optional to cc)
  "Post a new note to your Pump.io profile.

NOTE is a note created with `make-pmpio-note'.
FNC is a callback function with one argument, the note given as an answer from the server.
TO can be a string, a symbol, or a list mixing this two types. It contains the account ids or collection names as symbols.
CC analogous of TO.

If TO is not present or nil, then 'public will be used.
If CC is not present or nil, then 'followers will be used.

Example:

  (pmpio-http-post-note 
                        (make-pmpio-note :content \"Hi world!!! :-P\")
                        'my-callback-function
			'(\"acct:alice@microca.st\" \"acct:bob@pumpio.net\" public followers))
"
  
  (let ((url-request-method "POST")
	(url-request-extra-headers 
	 '(("Accept-Charset" . "utf-8")
	   ("Content-Type" . "application/json")))
	(url-request-data
	 (pmpio-note-as-activity-json (pmpio-user-preferredUsername pmpio-http-user-data)
				      note to cc))
	(buffer-file-coding-system 'utf-8)
	)
    (oauth-url-retrieve pmpio-auth-token
			(pmpio-url-post-note (pmpio-user-preferredUsername pmpio-http-user-data))
			'pmpio-http-post-note-callback
			(list fnc))
    )  
  )


(defun pmpio-http-whoami (fnc-callback)
  "Retrieve all personal information. After that, call FNC-CALLBACK with one parameter: a user structure."
  (let ((url-request-extra-headers 
	 '(("Accept-Charset" . "utf-8")))
	(buffer-file-coding-system 'utf-8)
	)
    (oauth-url-retrieve pmpio-auth-token (pmpio-url-whoami) 'pmpio-http-whoami-callback (list fnc-callback))
    )    
  )


					; ====================
					; Internal functions

(defun pmpio-http-whoami-2 (url fnc-callback)
  "In case of error 302(temporary moved error), we use this function to get the new URL and parse the JSON."

  (message (concat "Redirecting to " url))

  (let ((url-request-extra-headers 
	 '(("Accept-Charset" . "utf-8")))
	(buffer-file-coding-system 'utf-8)
	)
    (oauth-url-retrieve pmpio-auth-token url 'pmpio-http-whoami-callback-2 (list fnc-callback))
    )    
  )

(defun pmpio-http-whoami-callback (status fnc)
  "Callback function used for `pmpio-http-whoami'."
  (let ((err-number  (pmpio-http-get-number))
	)
    (if (or (equal 302 err-number) ;; HTTP Redirection?
	    (equal 400 err-number) ;; Invalid signature or bad request?
	    )
	(pmpio-http-whoami-2 (plist-get status :redirect) fnc)
      (pmpio-http-whoami-callback-2 status fnc)
      ))
  )

(defun pmpio-http-whoami-callback-2 (status fnc)
  (pmpio-http-delete-headers)
  
  (let* ((parsed (json-read))
	 (user (make-pmpio-user
		:preferredUsername (cdr (assoc 'preferredUsername parsed))
		:displayName (cdr (assoc 'displayName parsed))
		:summary (cdr (assoc 'summary parsed))
		:updated (cdr (assoc 'updated parsed))
		:id (cdr (assoc 'id parsed))
		:url (cdr (assoc 'url parsed))
		)
	       )
	 )
    (kill-buffer)
    (funcall fnc user)
    )    
  )



(defun pmpio-http-post-note-callback (status fnc)
  "Callback function used for `pmpio-http-post-note'."
  (pmpio-http-delete-headers)

  (let* ((parsed (json-read))
	 (note     (make-pmpio-note 
		    :uuid (cdr (assoc 'uuid parsed))
		    :author (cdr (assoc 'preferredUsername (cdr (assoc 'author parsed))))
		    :content (cdr (assoc 'content parsed))
		    :published (cdr (assoc 'published parsed))
		    :updated (cdr (assoc 'updated parsed))
		    :url (cdr (assoc 'url parsed))
		    ;; :comments ... 
		    )
		   )
	 )
    (kill-buffer)
    (funcall fnc note)
    )
  )

(defun pmpio-http-parse-note-callback (status fnc)
  (pmpio-http-delete-headers)
  
  ;; parse the json information
  (let* ((parsed (json-read))
	 (note     (make-pmpio-note 
		    :uuid (cdr (assoc 'uuid parsed))
		    :author (cdr (assoc 'preferredUsername (cdr (assoc 'author parsed))))
		    :content (cdr (assoc 'content parsed))
		    :published (cdr (assoc 'published parsed))
		    :updated (cdr (assoc 'updated parsed))
		    :url (cdr (assoc 'url parsed))
		    ;; :comments ... 
		    )
		   )
	 )
    
    (kill-buffer)
    (funcall fnc note)
    )
  )

(defun pmpio-http-get-major-feed-callback (status fnc)
  (pmpio-http-delete-headers)
  
  ;; parse the json information
  (let* ((parsed (json-read))
	 (items (cdr (assoc 'items parsed))) 
	 (amount (length items))
	 (activities nil) ;; This is the result list we'll return!
	 )
    
    ;; Gather each activity and create it.
    (dotimes (index amount)
      (let ((activity (aref items index)))
	(add-to-list 'activities
		     (make-pmpio-activity :actor (cdr (assoc 'preferredUsername (cdr (assoc 'actor activity))))
					  :content (cdr (assoc 'content (cdr (assoc 'object activity))))
					  :published (cdr (assoc 'published activity))
					  :verb (cdr (assoc 'verb activity)))
		     )
	)
      )
    
    (kill-buffer)
    (funcall fnc activities)
    )
  )

(defun pmpio-http-process-registration-callback (status)
  "This is a callback function for `pmpio-http-register-client'.
Gets the client secret and client id and stores it at the `pmpio-client-secret' and `pmpio-client-id' variables"
  (pmpio-http-delete-headers)
  (let ((parsed (json-read))
	)
    (setq pmpio-client-secret (cdr (assoc 'client_secret parsed)))
    (setq pmpio-client-id (cdr (assoc 'client_id parsed)))	
    )
  (kill-buffer)

  ;; Authorize
  (setq pmpio-auth-token (oauth-authorize-app pmpio-client-id
					      pmpio-client-secret 
					      (pmpio-url-request)
					      (pmpio-url-access)
					      (pmpio-url-authorize)))
  ;; Get the user information
  (pmpio-http-whoami 'pmpio-http-store-whoami)
  )

(defun pmpio-http-store-whoami (user-data)
  "Store the USER-DATA abstraction into `pmpio-http-user-data' for later use.

USER-DATA has information needed like the username for creating the URLs."
  (setq pmpio-http-user-data user-data)
  )

(defun pmpio-http-delete-headers ()
  "Delete all the HTTP headers in the current buffer."

  (let ((ending (search-forward "\n\n" nil t))
	)
    (when ending
      (delete-region (point-min) ending)
      )
    )  
  )


(defun pmpio-http-get-number ()
  "Get the HTTP number header in the current buffer."
  (goto-char (point-min))
  
  (let ((num-pos (search-forward " " nil t))
	(num nil)
	)
    (unless num-pos
      (error "HTTP error number not found!"))
    
    (goto-char num-pos)

    (string-to-number (buffer-substring (point) (search-forward " " nil t)))
    )
  )

(defun pmpio-http-get-field (field)
  "Get from the HTTP header the FIELD value in the current buffer."
  (goto-char (point-min))
  
  (let* ((case-fold-search t)
	 (data-pos (search-forward (concat field ": ") nil t))
	 (data nil)
	 )
    (unless data-pos
      (error "HTTP error number not found!"))
    
    (goto-char (match-end 0))

    (buffer-substring (point)(point-at-eol))
    )
  )
;;; pumpio-http.el ends here
