;; http://ergoemacs.org/emacs/emacs_dired_convert_images.html

(defun scale-image (fileList scalePercentage)
  "Create a scaled jpg version of images of marked files in dired.
The new names have “-s” appended before the file name extension.
Requires ImageMagick shell tool."
  (interactive
   (list (dired-get-marked-files) (read-from-minibuffer "scale percentage:")))
  (require 'dired)

  (mapc
   (lambda (ξf)
     (let ( newName cmdStr )
       (setq newName (concat (file-name-sans-extension ξf) "-s" ".jpg") )
       (while (file-exists-p newName)
         (setq newName (concat (file-name-sans-extension newName) "-s" (file-name-extension newName t))) )

       ;; relative paths used to get around Windows/Cygwin path remapping problem
       (setq cmdStr (concat "convert -scale " scalePercentage "% -quality 85% " (file-relative-name ξf) " " (file-relative-name newName)) )
       (shell-command cmdStr)
       ))
   fileList ))

(defun 2jpg (fileList)
                 "Create a jpg version of images of marked files in dired.
Requires ImageMagick shell tool.
"
                 (interactive (list (dired-get-marked-files) ))
                 (require 'dired)

                 (mapc
                  (lambda (ξf)
                    (let ( newName cmdStr )
                      (setq newName (concat (file-name-sans-extension ξf) ".jpg") )
                      (while (file-exists-p newName)
                        (setq newName (concat (file-name-sans-extension newName) "-2" (file-name-extension newName t))) )

                      ;; relative paths used to get around Windows/Cygwin path remapping problem
                      (setq cmdStr (concat "convert " (file-relative-name ξf) " " (file-relative-name newName)) )

                      ;; (async-shell-command cmdStr)
                      (shell-command cmdStr)
                      ))
                  fileList ))

;;Command to zip File/Dir

(defun 2zip ()
  "Zip the current file/dir in `dired'.
If multiple files are marked, only zip the first one.
Require unix zip commandline tool."
  (interactive)
  (require 'dired)
  (let ( (fileName (elt (dired-get-marked-files) 0))  )
    (shell-command (format "zip -r '%s.zip' '%s'" (file-relative-name fileName) (file-relative-name fileName)))
    ))
