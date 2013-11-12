;;; pumpio-interface.el --- 

;; Copyright 2013 Christian
;;
;; Author: cnngimenez@lavabit.com
;; Version: $Id: pumpio-interface.el,v 0.0 2013/07/06 21:13:26 cng Exp $
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
;;   (require 'pumpio-interface)

;;; Code:

(provide 'pumpio-interface)

(require 'pumpio-control)

(defun pumpio-post-current-buffer ()
  "Post the current buffer as a note. 
The buffer will not be killed."
  (interactive)

  (pmpio-ctrl-post-current-buffer)
  )

(defun pumpio-new-note ()
  "Create a temporary buffer for writing a note. Then you can post it pressing C-c C-c."

  (interactive)
  (pmpio-ctrl-post-note)
  )


(defun pumpio-post-new-note ()
  "Post the buffer created with `pumpio-new-note'. 

If it doesn't exist, do nothing.

This command is intended as a second step for `pumpio-new-note'."
  (interactive)
  (pmpio-ctrl-post-buffer pmpio-ctrl-post-buffer-name t)
  )

(defun pumpio-cancel-new-note ()
  "Cancel the new note buffer created by `pumpio-new-note'"
  (interactive)
  (pmpio-ctrl-close-new-note)
  )

;;(defun pumpio-major-feed (&optional nickname)
(defun pumpio-major-feed (nickname)
  "Retrieve and show the major feed for the given user whose nick is NICKNAME."

  ;; --> Soon: 
  ;; If NICKNAME is nil or not setted, then the current user is used. "
  ;; (interactive "MNickname?(defaults to current user)")
  (interactive "MNickname?")
  
  (pmpio-ctrl-get-major-feed nickname)
  )

(global-set-key "\C-xpn" 'pumpio-new-note)
(global-set-key "\C-xpf" 'pumpio-major-feed)

;;; pumpio-interface.el ends here
