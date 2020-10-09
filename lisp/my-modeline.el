;; -*- lexical-binding: t; -*-

(defvar my/show-minor-modes nil
  "Show the minor mode list in the modeline.")

;; probably this list can be deleted and simply check, in
;; my/alt-mode-p if the current major-mode is special or not.
(defvar my/mode-line-alt-mode '(rcirc-mode mu4e-main-mode mu4e-headers-mode
                                           mu4e-view-mode
                                           sql-interactive-mode
                                           telega-chat-mode
                                           telega-root-mode)
  "List of modes that get an alternative mode-line.")

(defun my/alt-mode-p ()
  "Return t if the current `major-mode' is within `my/mode-line-alt-mode'."
  (member major-mode my/mode-line-alt-mode))

(defun my/mode-line-toggle-minor-modes ()
  "Toggle the list of minor modes in modeline."
  (interactive)
  (setq my/show-minor-modes (not my/show-minor-modes)))

(setq-default
 mode-line-format
 `(
   ;; coding system and stuff
   (:eval (when (not (my/alt-mode-p))
            `(" " ,mode-line-mule-info)))

   ;; buffer name
   " %b "

   ;; misc info
   (:eval (when (not (my/alt-mode-p))
            `(,mode-line-remote ,mode-line-client ,mode-line-modified)))

   ;; vc can be useful
   (:eval (when-let (vc vc-mode)
            `(" "
              ,(substring vc 5)
              " ")))

   ;; percentage, line number and narrow
   (:eval (if (my/alt-mode-p)
              " "
            " %p L%l %n "))

   ;; show minor modes (eventually)
   (:eval (when my/show-minor-modes
            mode-line-modes))

   mode-line-misc-info ;; rcirc, mu4e...
   ))


;; ;; spaces to align right
;; (:eval (propertize " "
;;                    'display `((space :align-to
;;                                      (- (+ right right-fringe right-margin)
;;                                         ,(+ 3 (string-width mode-name)))))))
;;
;; ;; major mode
;; " %m")

(provide 'my-modeline)
;;; my-modeline.el ends here

