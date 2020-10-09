;;; emms-volume-sndioctl.el --- a mode for changing volume using sndioctl

;; This file is NOT part of EMMS.

;;;###autoload
(defun emms-volume-sndioctl-change (amount)
  "Change sndioctl output.level by AMOUNT."
  (message "Playback channels: %s"
           (with-temp-buffer
             (when (zerop
                    (call-process "sndioctl" nil (current-buffer) nil
                                  "-n"
                                  (format "output.level=%s%f"
                                          (if (> amount 0) "+" "")
                                          (/ (float amount) 100))))
               (replace-regexp-in-string "\n" "" (buffer-string))))))

(provide 'emms-volume-sndioctl)
