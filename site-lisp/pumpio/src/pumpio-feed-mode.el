;;; pumpio-mode.el --- 
;; 
;; Filename: pumpio-mode.el
;; Description: 
;; Author: Christian
;; Maintainer: 
;; Created: lun jul 15 00:33:07 2013 (-0300)
;; Version: 
;; Last-Updated: lun jul 15 01:32:05 2013 (-0300)
;;           By: Christian
;;     Update #: 15
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

(defface pumpio-header-face
  '(
    (t :foreground "sea green")
    )
  
  "Face used for the header of the activity."
  )

(defface pumpio-activity-bar-face
  '(
    (t :box t)
    )
  
  "Face used for the header of the activity."
  )

(defface pumpio-text-face
  '(
    (t )
    )
  
  "Face used for the header of the activity."
  )


(defstruct pmpio-activity
  verb actor content updated published)


(defun pmpio-html2text-code (p1 p2 p3 p4)
  "Used for `html2text-format-tag-list' for replacing code and give a beautiful background."
  (add-text-properties p2 p3 '(face (:background "dark green" :foreground "wheat")))
  (html2text-delete-tags p1 p2 p3 p4)
  )

(defun pmpio-apply-html-format ()
  "Apply `html2text' properly for giving a beautiful format while removing the tags."
  ;; We replace the <pre> tag for <code> so it can apply the format correctly.
  (save-excursion
    (goto-char (point-min))
    (replace-string "<pre>" "<code>")
    (goto-char (point-min))
    (replace-string "</pre>" "</code>")
    (goto-char (point-min))
    (let ((html2text-format-tag-list (append html2text-format-tag-list
					     '(("code" . pmpio-html2text-code))))
	  )    
      (html2text)
      )
    )
  )


(provide 'pumpio-feed-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; pumpio-mode.el ends here
