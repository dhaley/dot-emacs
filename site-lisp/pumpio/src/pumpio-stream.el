;;; pumpio-stream.el --- 

;; Copyright 2013 Christian
;;
;; Author: cnngimenez@lavabit.com
;; Version: $Id: pumpio-stream.el,v 0.0 2013/06/28 18:05:22 cng Exp $
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
;;   (require 'pumpio-stream)

;;; Code:

(provide 'pumpio-stream)

(require 'pumpio-http)

;;
;; This hooks must be assigned by the library that manages the connection.
;; See `pmpio-http-assign-functions'.
;;
(defvar pmpio-get-note-hook nil)
(defvar pmpio-register-hook nil)
(defvar pmpio-is-registered-p-hook nil)
(defvar pmpio-get-major-feed-hook nil)
(defvar pmpio-post-note-hook nil)


(defvar pmpio-stream-types '(
			   (http . pmpio-http-assign-functions)
			   )
  "This are the type of libraries for create a connection and retrieve information from the Pumpio server.

HTTP: This is the default and use the pumpio-http library, it calls the `pmpio-http-assign-functions' for setting all the hooks.")

(defun pmpio-activate-functions (&optional type)
  "Use the stream functions using this TYPE of application.
By default, if TYPE is nil or doesn't exists use http."
  
  (unless type 
    ;; Using nil or parameter not given 
    (set (make-local-variable 'type) 'http)
    )
  (if (assoc type pmpio-stream-types)
      (funcall (cdr (assoc type pmpio-stream-types)))
    (progn ;; Type of connection not founded!... using default!
      (message "Check the type of the connection. It doesn't exists! Using default: pumpio-http.el")
      (funcall (cdr (assoc 'http pmpio-stream-types)))
      )
    )   
  )


(defun pmpio-get-note (uuid fnc)
  "Get the note."
  (unless pmpio-get-note-hook
    (pmpio-activate-functions nil)
    )
  (run-hook-with-args 'pmpio-get-note-hook uuid fnc)
  )

(defun pmpio-register ()
  "Register the client and wait for user authorization.

Make the OAuth authentication!"
  (unless pmpio-register-hook
    (pmpio-activate-functions nil)
    )
  (message "Registering Pumpio-el...")
  (run-hook-with-args 'pmpio-register-hook)
  )

(defun pmpio-is-registered-p ()
  "Return t if this client is registered accordingly.

If the client is not registered, you can use `pmpio-register'."
  (unless pmpio-is-registered-p-hook
    (pmpio-activate-functions nil)
    )
  (funcall pmpio-is-registered-p-hook)
  )

(defun pmpio-get-major-feed (nickname fnc)
  "Get a user major feed.
NICKNAME is the nickname of the owner of the major feed.
FNC is a callback function with one parameter: the list of activities."
  (unless pmpio-get-major-feed-hook
    (pmpio-activate-functions nil)
    )
  (run-hook-with-args 'pmpio-get-major-feed-hook nickname fnc)
  )

(defun pmpio-post-note (note fnc)
  "Get a user major feed.
NICKNAME is the nickname of the owner of the major feed.
FNC is a callback function with one parameter: the list of activities."
  (unless pmpio-post-note-hook
    (pmpio-activate-functions nil)
    )
  (run-hook-with-args 'pmpio-post-note-hook note fnc)
  )

;;; pumpio-stream.el ends here
