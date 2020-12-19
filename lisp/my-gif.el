(eval-when-compile
  (require 'subr-x))

(defvar my/recording-p nil
  "T if recording.")

(defvar my/win-id nil
  "Window id.")

(defvar my/win-geometry nil
  "Window geometry.")

(defun my/gif-screencast ()
  (interactive)
  (if my/recording-p
      (my/stop-recording)
    (my/start-recording)))

(defun my/start-recording ()
  (setq my/win-id (with-temp-buffer
                    (process-file "xdotool" nil t nil "getwindowfocus")
                    (string-trim-right (buffer-string)))
        my/win-geometry (with-temp-buffer
                          (process-file "xdotool" nil t nil "getwindowgeometry" my/win-id)
                          (backward-word)
                          (delete-region (point-min)
                                         (point))
                          (string-trim-right (buffer-string)))
        my/recording-p t)
  (add-hook 'post-command-hook #'my/scrot))

(defun my/stop-recording ()
  (remove-hook 'post-command-hook #'my/scrot)
  (setq my/recording-p nil)
  (let ((default-directory "/tmp/emacs1000/"))
    (ignore-errors (shell-command "rm out.gif"))
    (shell-command "convert -delay 30 *.png out.gif")
    (shell-command "rm *.png")))

(defun my/scrot ()
  (let ((frame (expand-file-name (format-time-string "frame-%F-%T:%3N.png")
                                 "/tmp/emacs1000/")))
    (process-file "maim" nil nil nil
                  "-m" "5"
                  "-w" my/win-id
                  "-g" my/win-geometry
                  frame)))

(provide 'my-gif)
;;; my-gif.el ends here
