;;; pumpio-post-mode.el --- 

;; Copyright 2013 Christian
;;
;; Author: cnngimenez@lavabit.com
;; Version: $Id: pumpio-post-mode.el,v 0.0 2013/07/06 21:28:13 cng Exp $
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
;;   (require 'pumpio-post-mode)

;;; Code:

(provide 'pumpio-post-mode)

(require 'pumpio-interface)

(defvar pumpio-post-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\e\t" 'ispell-complete-word)
    (define-key map "\es" 'center-line)
    (define-key map "\eS" 'center-paragraph)
    (define-key map "\C-c\C-c" 'pumpio-post-new-note)
    (define-key map "\C-c\C-k" 'pumpio-cancel-new-note)
    map)
       "Keymap for `pumpio-post-mode'.")

(define-derived-mode pumpio-post-mode nil "PumpIO-post"
  "Pumio major mode for editing new notes."
  ;; font lock para ej-mode
  ;; (set (make-local-variable 'font-lock-defaults)
  ;;     pumpio-post-mode-font-lock)
  ;;(set (make-local-variable 'font-lock-keywords)
  ;;     ej-mode-font-lock)
  )


;;; pumpio-post-mode.el ends here
