;; -*- lexical-binding: t; -*-
(require 'mu4e)

(setq message-citation-line-format "On %a, %b %d %Y, %f wrote\n"
      message-default-charset 'utf8)

;; disable emoji
(setq gnus-treat-display-smileys nil)

(define-key global-map (kbd "<f12>") 'mu4e)

(setq mu4e-maildir (expand-file-name "~/Maildir")
      mu4e-get-mail-command "mbsync -a"
      mu4e-update-interval 180
      mu4e-headers-auto-update t
      mu4e-compose-signature-auto-include nil
      mu4e-compose-format-flowed nil
      mu4e-compose-in-new-frame t
      mu4e-change-filenames-when-moving t ;; don't break mbsync!
      mu4e-attachment-dir "~/Downloads"
      mu4e-compose-dont-reply-to-self t
      mu4e-view-show-addresses t
      mu4e-confirm-quit nil
      mu4e-context-policy 'pick-first
      mu4e-compose-context-policy 'always-ask
      message-kill-buffer-on-exit t
      mu4e-view-show-images t

      ;; use the ``tech preview''.  mu4e uses gnus article-mode to
      ;; show the messages.
      mu4e-view-use-gnus t

      ;; use localhost to send mails
      message-send-mail-function 'smtpmail-send-it
      smtpmail-smtp-server "localhost"
      smtpmail-default-smtp-server "localhost")

(add-to-list 'mu4e-view-actions '("ViewInBrowser" . mu4e-action-view-in-browser) t)

(add-hook 'mu4e-view-mode-hook 'visual-line-mode)

(defun my/mu4e-change-headers ()
  (interactive)
  (setq mu4e-headers-fields
        `((:human-date . 10)
          (:flags . 4)
          (:mailing-list . 8)
          (:from . 20)
          (:to . 20)
          (:thread-subject))))
(add-hook 'mu4e-headers-mode-hook 'my/mu4e-change-headers)

(defun my/mu4e-do-compose-stuff ()
  (interactive)
  (set-fill-column 72)
  (auto-fill-mode)
  (flyspell-mode))
(add-hook 'mu4e-compose-mode-hook 'my/mu4e-do-compose-stuff)

(defun my/mu4e-make-bookmarks (mdir)
  (list (make-mu4e-bookmark :name "Global Unread Messages"
                            :query "flag:unread and not flag:trashed"
                            :key ?u)
        (make-mu4e-bookmark :name "Local Unread Messages"
                            :query (concat "maildir:" mdir "/* and flag:unread and not flag:trashed")
                            :key ?l)
        (make-mu4e-bookmark :name "Today's Messages"
                            :query (concat "maildir:" mdir "/* and date:today..now")
                            :key ?t)
        (make-mu4e-bookmark :name "Big Messages"
                            :query (concat "maildir:" mdir "/* and size:1M..500")
                            :key ?b)
        (make-mu4e-bookmark :name "Sent"
                            :query (concat "maildir:" mdir "/Sent")
                            :key ?s)
        (make-mu4e-bookmark :name "Drafts"
                            :query (concat "maildir:" mdir "/Drafts")
                            :key ?d)))

(defun my/mu4e-match-fn (mdir)
  (lambda (msg)
    (when msg
      (string-prefix-p mdir (mu4e-message-field msg :maildir)))))

;; loading the mu4e configuration without halting emacs if its not found
;;
;; the my-mu4e-config.el file contains:
;;   (setq mu4e-contexts
;;         (list
;;          (make-mu4e-context
;;           :name "foobar"
;;           :match-func (my/mu4e-match-fn "/maildir-name")
;;           :vars `((user-mail.address . "")
;;                   (user-full-name . "")
;;                   (mu4e-sent-folder . "")
;;                   (mu4e-drafts-folder . "")
;;                   (mu4e-trash-folder . "")
;;                   (mu4e-compose-format-flowed . t)
;;                   (mu4e-maildir-shortcuts . (("/foo/bar" . ?f)
;;                                              ...))
;;                   (mu4e-bookmarks . ,(my/mu4e-make-bookmars "/maildir-name"))))))
;;
(condition-case nil
    (load "my-mu4e-config")
  (error (message "NOT loading mu4e configuration.  It's missing!")))
