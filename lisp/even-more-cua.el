;; -*- lexical-binding: t; -*-

(cua-mode +1)

(setq mouse-drag-and-drop-region t)

(define-key global-map (kbd "C-s") 'save-buffer)
(define-key global-map (kbd "C-f") 'isearch-forward)
(define-key global-map (kbd "C-q") 'save-buffers-kill-terminal)

(define-key global-map (kbd "S-<mouse-1>")
  (lambda (arg e)
    (interactive "P\ne")
    (unless (region-active-p)
      (set-mark-command arg))
    (mouse-set-point e)))

(defun emc--move-text-impl (arg)
  "Move text up or down by ARG lines.

If ARG is negative, the text will be moved up.  ``text'' means the
current line or the current region if its active.  The region is
automatically extended to the beginning of the first line and to the
end of the last line."
  (atomic-change-group
   (cond
    ((and mark-active transient-mark-mode)
     (if (> (point) (mark))
         (exchange-point-and-mark))

     ;; extend the region to the beginning of the first line/end of
     ;; the last
     (when (not (bolp))
       (move-beginning-of-line nil))
     (exchange-point-and-mark)
     (when (not (bolp))
       (next-line)
       (move-beginning-of-line nil))
     (exchange-point-and-mark)

     (let ((column (current-column))
           (text (delete-and-extract-region (point) (mark))))
       (forward-line arg)
       (move-to-column column t)
       (set-mark (point))
       (insert text)
       (exchange-point-and-mark)
       (setq deactivate-mark nil)))
    (t
     (let ((column (current-column)))
       (beginning-of-line)
       (when (or (> arg 0) (not (bobp)))
         (forward-line)
         (when (or (< arg 0) (not (eobp)))
           (transpose-lines arg))
         (forward-line -1))
       (move-to-column column t))))))

(define-key global-map (kbd "C-S-<up>")
  (lambda (arg)
    (interactive "*p")
    (emc--move-text-impl (- arg))))

(define-key global-map (kbd "C-S-<down>")
  (lambda (arg)
    (interactive "*p")
    (emc--move-text-impl arg)))

(provide 'even-more-cua)
;;; even-more-cua.el ends here
