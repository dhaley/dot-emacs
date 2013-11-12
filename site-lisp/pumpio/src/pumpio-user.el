;;; pumpio-user.el --- 
;; 
;; Filename: pumpio-user.el
;; Description: 
;; Author: Christian
;; Maintainer: 
;; Created: sáb jul  6 16:16:16 2013 (-0300)
;; Version: 
;; Last-Updated: sáb jul  6 17:39:15 2013 (-0300)
;;           By: Christian
;;     Update #: 9
;; URL: 
;; Doc URL: 
;; Keywords: 
;; Compatibility: 
;; 
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log:
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:


(provide 'pumpio-user)

(require 'cl)
(require 'json)

(defconst pmpio-user-objecttype "person"
  "This is the objectType field")

(defstruct pmpio-user
  id displayName preferredUsername url updated summary)

(defun pmpio-user-json (user)
  "Translate this USER abstraction into a JSON string."

  (json-encode
   (list (cons 'objectType pmpio-user-objecttype)
	 (cons 'preferredUsername (pmpio-user-preferredUsername user))
	 (cons 'displayName (pmpio-user-displayName user))
	 (cons 'id (pmpio-user-id))
	 (cons 'updated (pmpio-user-updated user))
	 (cons 'summary (pmpio-user-summary user))
	 (cons 'url (pmpio-user-url user))
	 )
   )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; pumpio-user.el ends here
