;;; pumpio-urls.el --- 

;; Copyright 2013 Christian
;;
;; Author: cnngimenez@lavabit.com
;; Version: $Id: pumpio-urls.el,v 0.0 2013/06/29 01:12:09 cng Exp $
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
;;   (require 'pumpio-urls)

;;; Code:

(provide 'pumpio-urls)

(defvar pumpio-pod "https://microca.st"
  "This is the pod where you have your account.")

(defun pmpio-url-get-note (uuid)
  "Return the URL string for getting a note with id UUID."
  (concat pumpio-pod "/api/note/" uuid)
  )
  
(defun pmpio-url-get-client-register ()
  "Return the URL string for getting the client registration URL."
  (concat pumpio-pod "/api/client/register")
  )

(defun pmpio-url-request ()
  "Return the URL string for getting the OAuth request token."
  (concat pumpio-pod "/oauth/request_token")
  )

(defun pmpio-url-authorize ()
  "Return the URL string for authorize the OAuth request token."
  (concat pumpio-pod "/oauth/authorize")
  )

(defun pmpio-url-access ()
  "Return the URL string for turning the request token into an access token."
  (concat pumpio-pod "/oauth/access_token")
  )

(defun pmpio-url-get-major-feed (nickname)
  "Return the URL string for retrieving the major feed of a user."
  (concat pumpio-pod "/api/user/" nickname "/feed/major")
  )

(defun pmpio-url-post-note (nickname)
  "Return the URL string for posting a new note to the user feed."
  (concat pumpio-pod "/api/user/" nickname "/feed")
  )
  

(defvar pmpio-url-collections '(
				 (public . "http://activityschema.org/collection/public")
				 (followers . "%s/api/user/%s/followers")
				 )
  "This variable is used for mappint the collections name with its URL."
  )

(defun pmpio-url-of-collection (nickname col-name)
  "Return the URL of the collection name. 
COL-NAME is a symbol and can be one in the `pmpio-url-collections' alist"  
  (format (cdr (assoc col-name pmpio-url-collections))
	  pumpio-pod
	  nickname
	  )
  )



(defun pmpio-url-whoami ()
  "Return the whoami URL."
  (concat pumpio-pod "/api/whoami"))

;;; pumpio-urls.el ends here
