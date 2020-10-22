;; -*- lexical-binding: t -*-

;;; Org mode (& co) stuff

;; use minted when exporting pdf code blocks
;; (setq org-latex-listings 'minted
;;       org-latex-packages-alist '(("" "minted"))
;;       org-latex-pdf-process
;;       '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
;;         "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

;; (setq straight-fix-org nil)

(use-package org
  :hook ((org-mode . auto-fill-mode))
  :bind (("C-c c" . org-capture)
         ("C-c a" . org-agenda)
         :map org-src-mode-map
         ("C-x w" . org-edit-src-exit)
         ("C-x C-s" . org-edit-src-exit))
  :config
  (setq

   ;; enable some special keybindings if point is at the beginning of a
   ;; headline.
   org-use-speed-commands t

   ;; when editing a block of code, replace the current buffer with it.
   ;; Doing so prevents random changes to the window layout.
   org-src-window-setup 'current-window

   ;; where to find all my org stuff (exept for zetteldeft)
   org-directory "~/org"
   org-agenda-files '("~/org")

   ;; when moving tasks around, ask for full path
   org-refile-use-outline-path 'file
   org-outline-path-complete-in-steps t
   org-refile-targets '((nil :maxlevel . 3)
                        (org-agenda-files :maxlevel . 3))

   ;; templates for org capture
   org-capture-templates
   '(("t" "todo" entry (file "~/org/refile.org")
      "* TODO %?\n %U\n %a\n" :clock-in t :clock-resume t)
     ("n" "note" entry (file "~/org/refile.org")
      "* %? :NOTE:\n %U\n %a\n" :clock-in t :clock-resume t)
     ("j" "Journal" entry (file+datetree "~/org/diary.org")
      "* %?" :clock-in t :clock-resume t))

   org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                       (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "INACTIVE(i@/!)"))

   org-use-fast-todo-selection t
   org-adapt-indentation 1
   org-cycle-separator-lines 2
   org-outline-path-complete-in-steps nil
   org-src-fontify-natively t

   org-blank-before-new-entry '((heading . nil)
                                (plain-list-item . nil))

   org-todo-state-tags-triggers
   '(("CANCELLED" "CANCELLED" . t)
     ("WAITING" ("WAITING" . t))
     ("HOLD" ("WAITING") ("HOLD" . t))
     ("INACTIVE" ("WAITING") ("HOLD") ("INACTIVE" . t))
     (done ("WAITING") ("HOLD"))
     ("TODO" ("WAITING") ("CANCELLED") ("HOLD") ("INACTIVE"))
     ("NEXT" ("WAITING") ("CANCELLED") ("HOLD") ("INACTIVE"))
     ("DONE" ("WAITING") ("CANCELLED") ("HOLD") ("INACTIVE")))

   ;; clocking time customizations
   org-clock-out-remove-zero-time-clocks t
   org-clock-out-when-done t
   org-clock-auto-clock-resolution '(when-no-clock-is-running)
   org-clock-report-include-clocking-task t
   org-time-stamp-rounding-minutes '(1 1)
   org-clock-history-length 23
   org-clock-in-resume t

   ;; don't ask for evaluation of babel blocks
   org-confirm-babel-evaluate nil)

  ;; load some interpreters for org-babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (C . t)
     (R . t)
     (sql . t)
     (shell . t)
     (python . t)
     (gnuplot . t))))

;; let org mode exports code block with syntax highlighting
(use-package htmlize)

;; zetteldeft stuff:

(use-package deft
  :commands (deft)
  :custom ((deft-extensions '("org" "md" "txt"))
           (deft-directory "~/notes")
           (deft-use-filename-as-title t)))

(use-package org-drill)

(use-package zetteldeft
  :commands (deft zetteldeft-deft-new-search deft-refresh
              zetteldeft-search-at-point
              zetteldeft-search-current-id zetteldeft-follow-link
              zetteldeft-avy-file-search-ace-window
              zetteldeft-avy-link-search
              zetteldeft-avy-tag-search zetteldeft-tag-buffer
              zetteldeft-find-file-id-insert
              zetteldeft-find-file-full-title-insert
              zetteldeft-find-file zetteldeft-new-file
              zetteldeft-new-file-and-link zetteldeft-file-rename
              zetteldeft-count-words)
  :init
  (with-eval-after-load 'hydra
    (defhydra hydra-zetteldeft (global-map "C-c d")
      ("d" deft)
      ("D" zetteldeft-deft-new-search)
      ("R" deft-refresh)
      ("s" zetteldeft-search-at-point)
      ("c" zetteldeft-search-current-id)
      ("f" zetteldeft-follow-link)
      ("F" zetteldeft-avy-file-search-ace-window)
      ("l" zetteldeft-avy-link-search)
      ("t" zetteldeft-avy-tag-search)
      ("T" zetteldeft-tag-buffer)
      ("i" zetteldeft-find-file-id-insert)
      ("I" zetteldeft-find-file-full-title-insert)
      ("o" zetteldeft-find-file)
      ("n" zetteldeft-new-file)
      ("N" zetteldeft-new-file-and-link)
      ("r" zetteldeft-file-rename)
      ("x" zetteldeft-count-words))))

(require 'org)

(defun my/org-url-at-point ()
  "Return the URL at point in a org buffer, or nil."
  (if-let (plain-url (url-get-url-at-point))
      plain-url
    (let* ((link-info (assoc :link (org-context)))
           (text (when link-info
                   (buffer-substring-no-properties
                    (or (cadr link-info) (point-min))
                    (or (caddr link-info) (point-max))))))
      (if (not text)
          nil
        (string-match org-link-bracket-re text)
        (substring text (match-beginning 1) (match-end 1))))))

(defun my/org-yank-url-at-point ()
  "Yank the URL at point in a org buffer."
  (interactive)
  (if-let (url (my/org-url-at-point))
      (progn (kill-new url)
             (message "Copied: %s" url))
    (error "No URL at point")))
