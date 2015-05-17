;;; bbdbadapt-encrypt.el --- Use the BBDB to encrypt/sign (smime or pgp)

;; Copyright (C) 2009 Uwe Brauer

;; Author: Uwe Brauer oub@mat.ucm.es
;; Maintainer: Uwe Brauer oub@mat.ucm.es
;; Created: 23 Feb 2013
;; Version: 1.1, 19 October 2013
;; Keywords:

 
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; A copy of the GNU General Public License can be obtained from this
;; program's author (send electronic mail to oub@mat.ucm.es) or from
;; the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA
;; 02139, USA.

;; This code allows to encrypt/sign message either by using pgp
;; (pgpmime) or smime according to the revant bbdb entry which should
;; read for for example:
;; encrypt: smime 

;;; Change log:
;; Adapted on 19 October 2013 to work with BBDB version 3 by 
;; Gijs Hillenius gijs@hillenius.com
;;
;; replaced bbdb-canonicalize-net-hook by
;; bbdb-canonicalize-mail-function
;;
:; replaced bbdb-search-simple by bbdb-message-search
;;
;; replaced bbdb-record-getprop record by bbdb-record-field (car
;; record)
;;
;; added local variables my-mml-use-pgpmime, my-encrypt-only-pgpmime,
;; my-encrypt-and-sign-pgpmime, my-encrypt-only-pgp and
;; my-encrypt-and-sign-pgp

;;; Code:

(defvar my-mml-use-pgpmime t
  "*Variable which controls to use pgpmime or pgp ASCII, default is pgpmime=
.")

(defun my-encrypt-only-pgpmime ()
  "Only encypt with pgpmime, DONT sign!"
  (interactive)
  (mml-secure-message-encrypt-pgpmime 1))

(defun my-encrypt-and-sign-pgpmime ()
  "Encrypt and  sign with pgpmime!"
  (interactive)
  (mml-secure-message-encrypt-pgpmime nil))


(defun my-encrypt-only-pgp ()
  "Only encypt with pgp ASCII, DONT sign!"
  (interactive)
  (mml-secure-message-encrypt-pgp 1))

(defun my-encrypt-and-sign-pgp ()
  "Encrypt and  sign with pgp ASCII!"
  (interactive)
  (mml-secure-message-encrypt-pgp nil))

(defconst bbdbadapt-encrypt-version (concat "0." (substring "$Revision: 1.1 $" 13 14))
	"$Id: bbdbadapt-encrypt.el,v 1.1 2013/02/10 08:49:57 oub Exp oub $
Report bugs to: Uwe Brauer oub@mat.ucm.es")


(defun bbdbencryptadpt-search-record (to)
  "Search a BBDB record associated to TO or return NIL."
  (let* ((data (mail-extract-address-components to))
         (name (car data))
         (net  (car (cdr data))))
    (if (equal name net) (setq name nil))
    (if (and net bbdb-canonicalize-mail-function)
        (setq net (bbdb-canonicalize-address net)))
    (bbdb-message-search name net)))

(defun bbdbencryptadpt-try-bbdbencrypt ()
  "Try to adapt non-interactively the current bbdbencrypt. 
This function looks silently in the current message to find how to
choose the bbdbencrypt. It does nothinng if not enough information is
present. This function is useful in a hook."
  (save-excursion
    (condition-case nil
        (progn
          (goto-char (point-min))
          (let ((record (bbdbencryptadpt-search-record 
                         ;;(bbdb-extract-field-value "To"))))
                         (bbdb-message-header "To"))))
			(if record
				(let ((signame (bbdbencryptadpt-retrieve-bbdbencrypt record)))
				  (when (and (stringp signame) (string= signame "smime"))
					(my-encrypt-and-sign-smime))
				  (when (and (stringp signame) (string= signame "smimencrypt"))
					(my-only-encrypt-smime))
				  (when (and (stringp signame) (string= signame "pgp"))
					(my-encrypt-and-sign-pgpmime)))))))))




(defun bbdbencryptadpt-retrieve-bbdbencrypt (&optional record) 
  "Retrieve the bbdbencrypt (a symbol) associated to a mailee.
The search is done through a BBDB record. "
  (if (not record)
      (save-excursion
        (goto-char (point-min))
        (let* ((to  (bbdb-extract-field-value "To"))
               (rec (bbdbencryptadpt-search-record to)) )
          (if rec (bbdbencryptadpt-do-retrieve-bbdbencrypt rec)
            (progn (message "No bound record")
                   nil))))
    (bbdbencryptadpt-do-retrieve-bbdbencrypt record)))

(defun bbdbencryptadpt-do-retrieve-bbdbencrypt (record)
  (let ((signame
		 (bbdb-record-field (car record) 'encrypt)))
    (if (stringp signame)
        (setq signame signame))
    signame))


(provide 'bbdbadapt-encrypt)


