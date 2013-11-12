;;; pumpio-activity.el --- 

;; Copyright 2013 Christian
;;
;; Author: cnngimenez@lavabit.com
;; Version: $Id: pumpio-activity.el,v 0.0 2013/06/30 06:15:43 cng Exp $
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
;;   (require 'pumpio-activity)

;;; Code:

(provide 'pumpio-activity)

(require 'pumpio-feed-mode) ;; We need some faces from this lib...

(defstruct pmpio-activity
  verb actor content updated published)


(defun pmpio-write-activity (activity)
  "Write in the current buffer a formatted ACTIVITY text."
  (insert (propertize "\n" 'face 'pumpio-activity-bar-face)
	  "\n"
	  (propertize (format  "%s at %s %s:"
			       (pmpio-activity-actor activity)
			       (pmpio-activity-published activity)
			       (pmpio-activity-verb activity))
		      'face 'pumpio-header-face
		      )
	  "\n"
	  (propertize (format "\n%s\n" 	  
			      (pmpio-activity-content activity))
		      'face 'pumpio-text-face
		      )
	  "\n"
	  )
  )

;;; pumpio-activity.el ends here
