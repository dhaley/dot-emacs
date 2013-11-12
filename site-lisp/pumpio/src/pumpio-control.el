;;; pumpio-control.el --- 

;; Copyright 2013 Christian
;;
;; Author: cnngimenez@lavabit.com
;; Version: $Id: pumpio-control.el,v 0.0 2013/06/29 06:34:45 cng Exp $
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
;;   (require 'pumpio-control)

;;; Code:

(provide 'pumpio-control)

(require 'pumpio-stream)
(require 'pumpio-note)
(require 'pumpio-post-mode)

(require 'pumpio-feed-mode)

(require 'html2text) ;; Emacs 24 has this by default!


(defconst pumpio-buffer "PumpIO"
  "This is the PumpIO Buffer name.

By convenience, we like to use only one buffer for everything."
  )

(defconst pmpio-ctrl-post-buffer-name "PumpIO New Note"
  "This is the PumpIO Buffer name for creating a new note.")

(defun pmpio-ctrl-get-note (uuid)
  "Get the note identified by its UUID.

Ensure that this client is registered."
  (unless (pmpio-is-registered-p)
    (pmpio-register)
    )
  
  (pmpio-get-note uuid 'pmpio-ctrl-get-note-callback)
  )

(defun pmpio-ctrl-get-note-callback (note)
  "This is a callback function used when the note has been retrieved.

It simply create or get the buffer and write down the note."
  (with-current-buffer (get-buffer-create pumpio-buffer)
    (delete-region (point-min) (point-max))
    (pmpio-note-write note)
    (switch-to-buffer-other-window (current-buffer))
    )		     
  )

(defun pmpio-ctrl-get-major-feed (nickname)
  "Get the major feed for a user.

Ensure that this client is registered."
  (unless (pmpio-is-registered-p)
    (pmpio-register)
    )

  (pmpio-get-major-feed nickname 'pmpio-ctrl-get-major-feed-callback)
  )

(defun pmpio-ctrl-get-major-feed-callback (activities)
  "Callback function for `pmpio-ctrl-get-major-feed'."
  (with-current-buffer (get-buffer-create pumpio-buffer)
    (delete-region (point-min) (point-max))
    (setq activities (reverse activities))
    (dolist (act activities)
      (pmpio-write-activity act)
      )
    (goto-char (point-min))
    (pmpio-apply-html-format)
    (switch-to-buffer-other-window (current-buffer))
    )		     
  )


					; Posting feature

(defvar pmpio-ctrl-selected-to nil
  "This variable has a list of people selected for completing the To field. In other words, these are the intended receptors of the message.

Is intended for temporary use, as soon as the post is sended correctly this variable will be cleaned.

This list can have these type of elements:

- string with the account address like \"acct:alice@microca.st\".
- symbol with the collection name like 'public or 'followers. See `pmpio-url-collections' variable.
")

(defvar pmpio-ctrl-selected-cc nil
  "Idem as `pmpio-ctrl-selected-to' but for carbon copy messages.")

(defun pmpio-ctrl-post-note ()
  "Create a new buffer for writing a note"
  (with-current-buffer (get-buffer-create pmpio-ctrl-post-buffer-name)
    
    (pumpio-post-mode)

    (switch-to-buffer-other-window (current-buffer))
    (fit-window-to-buffer (selected-window) 10 10)
    )
  )

(defun pmpio-ctrl-post-current-buffer (&optional kill-buffer)
  (pmpio-ctrl-post-buffer (current-buffer) kill-buffer)
  )

(defun pmpio-ctrl-post-buffer (buffer &optional kill-buffer)
  "Send a note to the server.

If KILL-BUFFER is t, then the current buffer will be killed after the note is posted."
  (when (get-buffer buffer)
    (unless (pmpio-is-registered-p)
      (pmpio-register)
      )
    
    (with-current-buffer buffer
      (goto-char (point-min))
      (replace-regexp "^[[:blank:]]*[\n\r]" "<br/>\n")
      
      (pmpio-post-note (make-pmpio-note :content (buffer-string)) 'pmpio-ctrl-post-callback)
      
      (when kill-buffer
	(pmpio-ctrl-close-new-note)
	)
      )
    )
  )

(defun pmpio-ctrl-post-callback (post-data)
  "This is a callback function for `pmpio-ctrl-post-current-buffer'."
  (message "Note posted! :-)")
  )

(defun pmpio-ctrl-close-new-note ()
  "Hide and kill the new note buffer."
  (when (get-buffer pmpio-ctrl-post-buffer-name)
    (with-current-buffer pmpio-ctrl-post-buffer-name
      (delete-window)
      (kill-buffer (current-buffer))
      )
    )
  )

;;; pumpio-control.el ends here
