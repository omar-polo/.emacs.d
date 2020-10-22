;; -*- lexical-binding: t; truncate-lines: t; -*-

(message "Here be dragons")

(require 'cl-lib)

(defmacro comment (&rest _body)
  "Ignore everything in its BODY and return nil."
  ())

(defun my/term ()
  "Sensible wrapper for `ansi-term'."
  (interactive)
  (ansi-term (or (getenv "SHELL") "/bin/sh")))

;; improve mark-ring navigation.  After C-u C-<SPC> one can keep
;; pressing C-<SPC> to navigate the ring backwards, instead of C-u
;; C-<SPC> again.
(setq set-mark-command-repeat-pop t)

(setq tab-always-indent 'complete)

(autoload 'ffap-file-at-point "ffap")
(defun my/complete-path-at-point+ ()
  "Return completion data for UNIX path at point."
  (let ((fn (ffap-file-at-point))
        (fap (thing-at-point 'filename)))
    (when (and (or fn (equal "/" fap))
               (save-excursion
                 (search-backward fap (line-beginning-position) t)))
      (list (match-beginning 0)
            (match-end 0)
            #'completion-file-name-table))))

(add-hook 'prog-mode-hook
          (lambda ()
            (add-hook 'completion-at-point-functions
                      'my/complete-path-at-point+
                      nil
                      'local)))

;; disable tabs everywhere (we'll enable them for the right modes
;; later)
(setq-default indent-tabs-mode nil)

;; get rid of the "yes or no" and replace with "y or n"
(fset 'yes-or-no-p 'y-or-n-p)

;; don't use (x11) windows for asking things
(setq use-dialog-box nil)

(defun my/enable-tabs ()
  "Enable `indent-tabs-mode' in the current buffer."
  (interactive)
  (setq indent-tabs-mode t))

(add-hook 'emacs-lisp-mode-hook 'checkdoc-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'prettify-symbols-mode)
(add-hook 'log-edit-mode-hook 'auto-fill-mode)

(dolist (hook '(sh-mode-hook shell-script-mode-hook c-mode-hook c++-mode-hook))
  (add-hook hook 'my/enable-tabs))

(setq-default sh-basic-offset 8
              sh-indent-after-loop-construct 8
              sh-indent-after-continuation nil)

(setq tab-width 8)

(setq x-stretch-cursor t)

(with-eval-after-load 'dired
  (setq dired-listing-switches "-lah"
        dired-dwim-target t
        dired-deletion-confirmer 'y-or-n-p)
  (add-hook 'dired-mode-hook 'dired-hide-details-mode)
  (define-key dired-mode-map (kbd "C-c w") 'wdired-change-to-wdired-mode))

;; I have numbers on shift, so to pass the arg 0 (the one that I use
;; the most I think) it's an ackward C-S-! or M-S-!.  C-! is easier
;; (and unused).
(defun my/digit-argument-zero ()
  "Like `digit-argument', but set the arg to 0 unconditionally."
  (interactive)
  (prefix-command-preserve-state)
  (setq prefix-arg 0))

(define-key global-map (kbd "C-!") #'my/digit-argument-zero)

;; more useful than zap-to-char
(define-key global-map (kbd "M-z") #'zap-up-to-char)

(define-key global-map (kbd "C-M-'") #'imenu)
(define-key global-map (kbd "C-z") nil)
(define-key global-map (kbd "C-M-/") #'dabbrev-expand)
(define-key global-map (kbd "C-x C-b") #'ibuffer)
(define-key global-map (kbd "<f5>") #'(lambda ()
                                        (interactive)
                                        (ansi-term (getenv "SHELL"))))
(define-key global-map (kbd "<f6>") #'display-line-numbers-mode)

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

(defun my/fill-or-unfill (fn &optional justify region)
  "Meant to be an adviced :around `fill-paragraph'.
FN is the original `fill-column'.  If `last-command' is
`fill-paragraph', unfill it, fill it otherwise.  Inspired from a
post on endless parentheses.  Optional argument JUSTIFY and
REGION are passed to `fill-paragraph'."
  (let ((fill-column
         (if (eq last-command 'fill-paragraph)
             (progn (setq this-command nil)
                    (point-max))
           fill-column)))
    (funcall fn justify region)))
(advice-add 'fill-paragraph :around #'my/fill-or-unfill)

(defun my/scroll-up-command ()
  "Like `scroll-up-command', but does not accept a parameter and recenter the cursor."
  (interactive)
  (move-to-window-line -1)
  (recenter))
(define-key global-map (kbd "C-v") #'my/scroll-up-command)

(defun my/scroll-down-command ()
  "Like `scroll-down-command', but doesn't accept a parameter and recenter afterwards."
  (interactive)
  (move-to-window-line 1)
  (recenter))
(define-key global-map (kbd "M-v") #'my/scroll-down-command)

;; reload tags without asking
(setq tags-revert-without-query 1)

(setq sentence-end-double-space t)

(defconst my/backup-dir (expand-file-name "backups" user-emacs-directory))

(unless (file-exists-p my/backup-dir)
  (make-directory my/backup-dir t))

(setq backup-directory-alist `(("." . ,my/backup-dir))
      ;; auto-save-file-name-transforms `((".*" ,my/backup-dir t))
      )

;; fix hangs due to pasting from xorg -- workaround, not a solution :/
(setq x-selection-timeout 1)
(add-hook 'after-make-frame-functions
          (lambda (_frame)
            (setq x-selection-timeout 1)))

(define-key global-map (kbd "C-c o") 'occur)

(defun my/list-major-modes ()
  "List all the major modes.  Inspired from ieure/scratch-el.  Naïve probably."
  (cl-loop for sym the symbols of obarray
           for name = (symbol-name sym)
           when (and (functionp sym)
                     (not (member sym minor-mode-alist))
                     (string-match "-mode$" name)
                     (not (string-match "--" name)))
           collect name))

(defun my/scratchpad-select-mode ()
  "Select an appropriate major mode to be used for the scratchpad."
  (if current-prefix-arg
      (intern (concat (completing-read
                       "Major Mode: " (my/list-major-modes)
                       nil t nil nil)))
    major-mode))

(defun my/new-scratchpad (mode)
  "Create a new *scratch* buffer for the MODE."
  (interactive (list (my/scratchpad-select-mode)))
  (let ((buf (generate-new-buffer "*scratch*")))
    (pop-to-buffer buf)
    (funcall mode)))

(defun my/narrow-or-widen-dwim (p)
  "Widen if the buffer is narrowed, narrow-dwim otherwise.
Dwim means: region, org-src-block, org-subtree or defun,
whichever applies first.  Narrowing to org-src-blocks actually
calls `org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer is already
narrowed.  With P being -, narrow to page instead of to defun.

Taken from endless parentheses."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning)
                           (region-end)))
        ((derived-mode-p 'org-mode)
         ;; `org-edit-src-code' isn't a real narrowing
         (cond ((ignore-errors (org-edit-src-code) t))
               ((ignore-errors (org-narrow-to-block) t))
               (t (org-narrow-to-subtree))))
        ((eql p '-) (narrow-to-page))
        (t (narrow-to-defun))))
(define-key global-map (kbd "C-c w") 'my/narrow-or-widen-dwim)

;; setup for packages
(setq straight-use-package-by-default t)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; (add-to-list 'straight-built-in-pseudo-packages 'package)

(defmacro my/deftranspose (name scope &optional doc key)
  "Macro to produce transposition functions.
NAME is the function's symbol.  SCOPE is the text object to
operate on.  Optional DOC is the function's docstring.

Transposition over an active region will swap the object at
mark (region beginning) with the one at point (region end).

It can optionally define a key for the defined function in the
`global-map' if KEY is passed.

Originally from protesilaos' dotemacs."
  (declare (indent defun))
  `(progn
     (defun ,name (arg)
       ,doc
       (interactive "p")
       (let ((x (format "transpose-%s" ,scope)))
         (if (use-region-p)
             (funcall (intern x) 0)
           (funcall (intern x) arg))))
     ,(when key
        `(define-key global-map (kbd ,key) #',name))))

(my/deftranspose my/transpose-lines "lines"
  "Transpose lines or swap over active region."
  "C-x C-t")

(my/deftranspose my/transpose-paragraphs "paragraphs"
  "Transpose paragraph or swap over active region."
  "C-S-t")

(my/deftranspose my/transpose-sentences "sentences"
  "Transpose sentences or swap over active region."
  "C-x M-t")

(my/deftranspose my/transpose-sexps "sexps"
  "Transpose sexps or swap over active region."
  "C-M-t")

(my/deftranspose my/transpose-words "words"
  "Transpose words or swap over active region."
  "M-t")

(straight-use-package 'use-package)

(use-package desktop
  :hook ((after-init . desktop-read)
         (after-init . desktop-save-mode))
  :custom ((desktop-base-file-name ".desktop")
           (desktop-base-lock-name ".desktop.lock")
           (desktop-restore-eager 8)))

(use-package transpose-frame
  :commands (transpose-frame flip-frame flop-frame
                             rotate-frame rotate-frame-clockwise
                             rotate-frame-anti-anticlockwise))

(use-package hideshow
  :straight nil
  :hook (prog-mode . hs-minor-mode))

(use-package flyspell
  :straight nil
  :hook (text-mode . flyspell-mode))

(use-package typo
  :hook (text-mode . typo-mode))

(use-package guess-language
  :hook (text-mode . guess-language-mode)
  :config
  (setq guess-language-langcodes '((en . ("en_GB" "English"))
                                   (it . ("it_IT" "Italian")))
        guess-language-languages '(en it)
        guess-language-min-paragraph-length 45))

(use-package term
  :straight nil
  :bind (:map term-raw-map
              ("C-c C-y" . term-paste))
  :config
  (defun my/term-exec-hook ()
    "Kill term buffer after the shell is closed.  Taken from oremacs."
    (let* ((buf (current-buffer))
           (proc (get-buffer-process buf)))
      (set-process-sentinel
       proc
       `(lambda (process event)
          (if (string= event "finished\n")
              (kill-buffer ,buf))))))
  (add-hook 'term-exec-hook 'my/term-exec-hook))

(use-package keycast
  :commands (keycast-mode)
  :custom (keycast-insert-after 'mode-line-misc-info))

(use-package emms
  :commands (emms)
  :config
  (setq emms-source-file-default-directory "~/music/"
        emms-mode-line-format "「%s」"
        emms-browser-covers 'emms-browser-cache-thumbnail-async)

  (require 'emms-setup)
  (emms-all)
  (emms-default-players)

  ;; use libtag to extract tracks info.
  ;;
  ;; XXX: this needs to be compiled from sources
  ;; (~/.emacs.d/straight/repos/emms/) and cp emms-print-metadata
  ;; ~/bin.
  (require 'emms-info)
  (require 'emms-info-libtag)
  (setq emms-info-functions '(emms-info-libtag))

  ;; save on quit and recover on startup
  (require 'emms-history)
  (emms-history-load)

  ;; use my backend for sndioctl
  (require 'emms-volume-sndioctl)
  (setq-default emms-volume-change-function 'emms-volume-sndioctl-change))

(use-package elpher
  :commands (elpher elpher-go elpher-jump))

(use-package magit
  :commands (magit)
  :bind ("C-x g" . magit))

(use-package forge
  :after (magit))

(comment
 (use-package md4rd
   :commands (md4rd md4rd-login)
   :config (md4rd--oauth-fetch-refresh-token)))

;; defer loading of both org and mu4e stuff
(run-with-idle-timer 1 nil
                     (lambda ()
                       (let ((gc-cons-threshold most-positive-fixnum))
                         (load "my-org-stuff")
                         (load "my-mail-stuff"))))

(use-package eww
  :straight nil
  :bind ("<f9>" . browse-web)
  :config (progn (setq ;; browse-url-browser-function 'eww-browse-url
                       url-cookie-trusted-urls nil
                       url-cookie-untrusted-urls '(".*"))))

(use-package rcirc
  :defer 1
  :straight nil
  :config (progn (rcirc-track-minor-mode)
                 (add-hook 'rcirc-mode-hook
                           (lambda ()
                             (flyspell-mode 1)
                             (rcirc-omit-mode)))
                 (setq rcirc-buffer-maximum-lines 1000
                       rcirc-log-flag t
                       rcirc-omit-responses '("JOIN" "PART" "QUIT"
                                              "NICK" "AWAY")
                       rcirc-fill-column 80
                       rcirc-keywords '("godot" "poedit" "mu4e"))

                 (define-key rcirc-mode-map (kbd "C-l")
                   (lambda () "Recenter to bottom line"
                     (interactive)
                     (goto-char (point-max))
                     (recenter -1)))

                 ;; contains something along the lines of
                 ;;
                 ;;   (setq rcirc-authinfo
                 ;;         '(("irc.freenode.net" nickserv
                 ;;            "USERNAME" "PASSWORD")))
                 (condition-case nil
                     (load "my-rcirc-conf")
                   (error (message "Missing rcirc configuration!")))))

(use-package telega
  :config (progn
            (add-hook 'telega-root-mode-hook 'telega-notifications-mode)
            (comment
             (add-hook 'telega-chat-mode-hook
                       (lambda ()
                         (set (make-local-variable 'company-backends)
                              (append '(telega-company-emoji
                                        telega-company-username
                                        telega-company-hashtag)
                                      (when (telega-chat-bot-p telega-chatbuf--chat)
                                        '(telega-company-botcmd)))))))))

(use-package elfeed
  :bind (("C-x w" . elfeed)
         :map elfeed-show-mode-map
         ("q" . delete-window))
  :config (progn (setq elfeed-feeds
                       '("https://undeadly.org/cgi?action=rss&full=yes&items=10"
                         "http://www.tedunangst.com/flak/rss"
                         "https://www.dragonflydigest.com/feed"
                         "https://www.mirbsd.org/news.rss"
                         "https://bentsukun.ch/index.xml"
                         "https://drewdevault.com/feed.xml"
                         "https://www.cambus.net/atom.xml"
                         "https://dataswamp.org/~solene/rss.xml"
                         "https://briancallahan.net/blog/feed.xml"
                         "https://www.poolp.org/index.xml"
                         "https://jcs.org/rss"
                         "https://sanctum.geek.nz/arabesque/feed/"
                         "https://tech.toryanderson.com/"
                         "https://alexschroeder.ch/wiki?action=journal;search=-tag:rpg -tag:rsp;lang=en;title=English Diary without RPG Pages"
                         "http://boston.conman.org/bostondiaries.rss"
                         "https://emacsninja.com/feed.atom"
                         "https://bsdly.blogspot.com/feeds/posts/default"
                         "https://crawshaw.io/atom.xml"
                         "https://nullprogram.com/feed/"
                         "http://pragmaticemacs.com/feed/"
                         "https://emacsnotes.wordpress.com/feed/"
                         "https://metaredux.com/feed.xml"
                         "https://emacsredux.com/atom.xml"
                         "https://cestlaz.github.io/rss.xml"
                         "https://utcc.utoronto.ca/~cks/space/blog/?atom"
                         "https://irreal.org/blog/?feed=rss2"
                         "https://insideclojure.org/feed.xml"
                         "https://tech.toryanderson.com/index.xml"
                         "https://vermaden.wordpress.com/feed/"
                         "https://www.arp242.net/feed.xml"
                         "https://tymoon.eu/api/reader/atom"
                         "https://venam.nixers.net/blog/feed.xml"
                         "https://www.omarpolo.com/rss.xml"
                         "https://owarisubs.lacumpa.biz/feed/"
                         "https://asenshi.moe/feed/"
                         "https://godotengine.org/rss.xml"
                         "https://github.com/yshui/picom/releases.atom"
                         "https://github.com/vslavik/poedit/releases.atom"
                         "https://github.com/TokTok/c-toxcore/releases.atom"))

                 (defun my/elfeed-display-buffer (buf &optional _act)
                   (pop-to-buffer buf)
                   (set-window-text-height (get-buffer-window)
                                           (round (* 0.7 (frame-height)))))
                 (setq elfeed-show-entry-switch 'my/elfeed-display-buffer)

                 (defun my/elfeed-search-show-entry-pre (&optional lines)
                   "Returns a function to scroll forward or back
                   in the elfeed search results, displaying
                   entries without switching to them"
                   (lambda (times)
                     (interactive "p")
                     (forward-line (* times (or lines 0)))
                     (recenter)
                     (call-interactively 'elfeed-search-show-entry)
                     (select-window (previous-window))
                     (unless elfeed-search-remain-on-entry
                       (forward-line -1))))

                 (define-key elfeed-search-mode-map (kbd "n") (my/elfeed-search-show-entry-pre +1))
                 (define-key elfeed-search-mode-map (kbd "p") (my/elfeed-search-show-entry-pre -1))
                 (define-key elfeed-search-mode-map (kbd "M-RET") (my/elfeed-search-show-entry-pre))))

(use-package projectile
  :bind-keymap (("C-c p" . projectile-command-map)
                ("s-p"   . projectile-command-map))
  :config
  (setq projectile-completion-system 'default
        projectile-enable-caching t)

  (defun my/projectile-run-term (arg)
    "Invoke `term' in hte project's root or switch to it if already exists.

Use a prefix argument ARG to indicate the creation of a new process instead."
    (interactive "P")
    (let ((project (projectile-ensure-project (projectile-project-root)))
          (buffer-name (projectile-generate-process-name "term" arg))
          (cmd (or explicit-shell-file-name
                   (getenv "ESHELL")
                   (getenv "SHELL")
                   "/bin/sh")))
      (unless (get-buffer buffer-name)
        (require 'term)
        (projectile-with-default-dir project
          (set-buffer (term-ansi-make-term buffer-name cmd))
          (term-mode)
          (term-char-mode)))
      (switch-to-buffer buffer-name)))

  (define-key projectile-command-map (kbd "x t") #'my/projectile-run-term))

(comment
 (use-package company
   :bind (:map company-active-map
               ("C-n" . company-select-next)
               ("C-p" . company-select-previous))
   :config (global-company-mode)))

(comment
 (use-package icomplete
   :straight nil
   :bind (:map icomplete-minibuffer-map
               ("<tab>" . icomplete-force-complete)
               ("<return>" . icomplete-force-complete-and-exit)
               ("C-j" . exit-minibuffer)      ; force input unconditionally
               ("C-s" . icomplete-forward-completions)
               ("C-r" . icomplete-backward-completions)
               ("C-n" . icomplete-forward-completions)
               ("C-p" . icomplete-backward-completions)
               ("<C-backspace>" . icomplete-fido-backward-updir))
   :custom ((icomplete-show-matches-on-no-input t)
            (icomplete-hide-common-prefix nil)
            (icomplete-prospect-height 1))
   :init (progn (message "before {fido,icomplete}-mode")
                (fido-mode 1)
                (icomplete-mode 1)))

 (use-package icomplete-vertical
   :bind (:map icomplete-minibuffer-map
               ("<down>" . icomplete-forward-completions)
               ("C-n" . icomplete-forward-completions)
               ("<up>" . icomplete-backward-completions)
               ("C-p" . icomplete-backward-completions))
   :config (icomplete-vertical-mode))

 (use-package orderless
   :custom (completion-styles '(orderless))))

(use-package selectrum
  :bind (("C-M-y" . my/yank-pop))
  :custom (enable-recursive-minibuffers t)
  :config (progn
            (selectrum-mode +1)
            (defun my/yank-pop ()
              "Offer a way to pick an element from the `kill-ring' using `completing-read'.  Adapted from yank-pop+ from selectrum wiki."
              (interactive)
              (let* ((old-last-command last-command)
                     (text (completing-read
                            "Yank: "
                            (cl-remove-duplicates kill-ring :test #'string= :from-end t)
                            nil t nil nil))
                     (pos (cl-position text kill-ring :test #'string=))
                     (n (+ pos (length kill-ring-yank-pointer))))
                (unless (string= text (current-kill n t))
                  (error "Could not set up for `current-kill'"))
                (setq last-command old-last-command)
                (if (eq last-command 'yank)
                    (yank-pop n)
                  (insert-for-yank text))))))

(use-package selectrum-prescient
  :config (progn
            (selectrum-prescient-mode +1)
            (prescient-persist-mode +1)))

(use-package embark
  :straight (:type git :host github :repo "oantolin/embark")
  :bind (:map minibuffer-local-completion-map
         ("M-t" . embark-act)
         ("M-T" . embark-act-noexit)
         :map completion-list-mode-map
         ("M-t" . embark-act)
         :map minibuffer-inactive-mode-map
         ("M-t" . embark-act)
         :map selectrum-minibuffer-map
         ("M-t" . embark-act)
         ("M-T" . embark-act-noexit))
  :config
  (defun my/selectrum-get-target ()
    (selectrum--get-full
     (selectrum--get-candidate
      selectrum--current-candidate-index)))
  (cl-pushnew 'my/selectrum-get-target embark-target-finders)

  (defun my/selectrum-refresh ()
    "Refresh the completion list of selectrum."
    (setq selectrum--previous-input-string nil))
  (add-hook 'embark-pre-action-hook #'my/selectrum-refresh)

  (add-hook 'embark-post-action-hook #'embark-occur--update-linked)

  (define-key embark-general-map (kbd "C-o") #'embark-occur)

  (comment
   (defun my/embark-open-file (&optional arg)
     "Open the file at embar target in the current buffer.

If ARG is not nil, open in a new buffer"
     (interactive "p")
     (let ((f (substring-no-properties (embark-target))))
       (if arg
           (find-file-other-window f)
         (find-file f))))
   (define-key embark-general-map (kbd "o") #'my/embark-open-file))
  )

(use-package hippie-exp
  :straight nil
  :bind (("M-/" . hippie-expand))
  :custom (hippie-expand-try-functions-list '(try-expand-dabbrev
                                              try-expand-dabbrev-all-buffers
                                              try-expand-dabbrev-from-kill
                                              try-complete-file-name-partially
                                              try-complete-file-name
                                              try-expand-all-abbrevs
                                              try-expand-list
                                              try-expand-line
                                              try-complete-lisp-symbol-partially
                                              try-complete-lisp-symbol)))

(use-package iedit
  :bind (("C-;" . iedit-mode)))

(use-package expand-region
  :bind (("C-=" . er/expand-region)))

;; the concept is cool, but it doesn't play well with smartparens.
;; C-w is bound to sp-kill-sexp, not whole-line-or-region-kill-region.
(comment
 (use-package whole-line-or-region
   :config (whole-line-or-region-global-mode +1)))

(use-package yasnippet
  :bind (:map yas-minor-mode-map
         ("<tab>" . nil)
         ("TAB" . nil))
  :custom (yas-wrap-around-region t)
  :config
  (yas-global-mode +1)
  (define-key yas-minor-mode-map (kbd "SPC") yas-maybe-expand)

  (defun my/inside-comment-or-string-p ()
    "t if point is inside a string or comment."
    (let ((s (syntax-ppss)))
      (or (nth 4 s)                     ;comment
          (nth 3 s))))                  ;string

  (defun my/yas-fix-local-condition (mode-hook)
    "Add a hook to MODE-HOOK that sets (locally) `yas-buffer-local-condition' to TEST."
    (add-hook mode-hook
              (lambda ()
                (setq yas-buffer-local-condition
                      '(not (my/inside-comment-or-string-p))))))

  (dolist (l '(emacs-lisp-mode-hook lisp-interaction-mode-hook
                                    clojure-mode-hook
                                    c-mode-hook))
    (my/yas-fix-local-condition l)))

(use-package beacon
  :config (beacon-mode 1))

(use-package avy
  :custom ((avy-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n ?s)))
  :commands (avy-goto-char
             avy-goto-char-2
             avy-goto-word-1
             avy-goto-line)
  ;; there's an hydra down there with bindings.
  :bind (:map isearch-mode-map
         ("C-'" . avy-isearch)))

(use-package smartparens
  :commands (turn-on-smartparens-strict-mode)
  :bind (("C-M-a" . sp-beginning-of-sexp)
         ("C-M-e" . sp-end-of-sexp)
         ("C-M-f" . sp-forward-sexp)
         ("C-M-b" . sp-backward-sexp)
         ("C-M-n" . sp-next-sexp)
         ("C-M-p" . sp-previous-sexp)
         ("C-(" . sp-forward-barf-sexp)
         ("C-)" . sp-forward-slurp-sexp)
         ("C-{" . sp-backward-barf-sexp)
         ("C-}" . sp-backward-slurp-sexp)
         ("C-k" . sp-kill-hybrid-sexp))
 :hook ((prog-mode . turn-on-smartparens-strict-mode)
        (cider-repl-mode . turn-on-smartparens-strict-mode)
        (LaTeX-mode . turn-on-smartparens-strict-mode))
 :config (progn (require 'smartparens-config)
                ;; (show-smartparens-global-mode t)

                (defun my/newline-indent (&rest _ignored)
                  (split-line)
                  (indent-for-tab-command))

                (let ((c-like '(c++mode
                                c-mode
                                css-mode
                                go-mode
                                java-mode
                                js-jsx-mode
                                js2-jsx-mod
                                js2-mode
                                json-mode
                                python-mode
                                web-mode)))
                  (dolist (x `(("{" . ,c-like)
                               ("[" . ,c-like)
                               ("(" . (sql-mode ,@c-like))))
                    (dolist (mode (cdr x))
                      (sp-local-pair mode (car x) nil :post-handlers
                                     '((my/newline-indent "RET")
                                       (my/newline-indent "<return>"))))))
                ;; it does not work.
                (sp-local-pair 'gerbil-mode "'" nil :actions :rem)
                (sp-local-pair 'gerbil-mode "`" nil :actions :rem)))

(use-package cc-mode
  :straight nil
  :bind (:map c-mode-map
              ("C-M-a" . sp-beginning-of-sexp)
              ("C-M-e" . sp-end-of-sexp)
              ("C-c c" . compile))
  :custom ((c-basic-offset 8)
           (c-default-style "K&R"))
  :hook ((c-mode . my/c-indent)
         (c-mode . my/enable-tabs))
  :config (progn
            (defun my/c-indent ()
              (interactive)
              (c-set-offset 'arglist-intro '+)
              (c-set-offset 'arglist-cont-nonempty '*))))

(use-package sql
  :straight nil
  :hook ((sql-interactive-mode . toggle-truncate-lines)))

(use-package sql-indent
  :hook ((sql-mode . sqlind-minor-mode)))

(use-package markdown-mode
  :mode "\\.md\\'"
  :hook ((markdown-mode . auto-fill-mode)))

(use-package yaml-mode
  :mode "\\.yml\\'")

(use-package toml-mode
  :mode "\\.toml\\'")

(use-package gemini-mode
  :straight nil)

(use-package go-mode
  :mode "\\.go\\'"
  :hook ((go-mode . subword-mode)))

(use-package gdscript-mode
  :mode "\\.gd\\'"
  :hook ((gdscript-mode . my/enable-tabs)))

(comment
 (use-package slime
   ;; slime binds these to next/prev sexp.  I prefer the consistency
   ;; with my smartparens configuration.
   :bind (:map slime-mode-indirect-map
               ("C-M-a" . sp-beginning-of-sexp)
               ("C-M-e" . sp-end-of-sexp))
   :config (progn (load (expand-file-name "~/quicklisp/slime-helper.el"))
                  (slime-setup '(slime-fancy slime-company))
                  (setq inferior-lisp-program "sbcl")))

 (use-package slime-company
   :config (setq slime-company-completion 'fuzzy)))

(use-package sly
  :hook ((lisp-mode . prettify-symbols-mode))
  :custom (inferior-lisp-program "sbcl"))

(use-package gerbil
  :straight nil
  :mode (("\\.ss\\'" . gerbil-mode)
         ("\\.pkg\\'" . gerbil-mode))
  :bind (:map gerbil-mode-map
              (("C-c M-o" . my/gerbil-clear-repl)))
  :init
  (setf gerbil (getenv "GERBIL_HOME"))
  :hook ((inferior-scheme-mode-hook . gambit-inferior-mode)
         (gerbil-mode . prettify-symbols-mode)
         (gerbil-mode . turn-on-smartparens-strict-mode)
         (gerbil-mode . my/scheme-setup))
  :config
  (setf scheme-program-name "gxi")
  (defun my/scheme-setup ()
    (sp-pair "'" nil :actions :rem)
    (sp-pair "`" nil :actions :rem))
  (defun my/gerbil-clear-repl ()
    "Clear the comint `*scheme*' buffer associated with current buffer."
    (interactive)
    (when scheme-buffer
      (with-current-buffer (get-buffer scheme-buffer)
        (comint-clear-buffer)))))

(use-package clojure-mode
  :mode (("\\.clj" . clojure-mode)
         ("\\.cljs" . clojurescript-mode)
         ("\\.cljc" . clojurec-mode)
         ("\\.edn" . clojure-mode))
  :hook ((clojure-mode . subword-mode)
         (clojurec-mode . subword-mode)
         (clojurescript-mode . subword-mode))
  :config
  ;; reagent.core/with-let is like let
  (add-to-list 'clojure-align-binding-forms "r/with-let" t #'string=)

  (put-clojure-indent 'for-all 2)
  (defun my/clojure-mode-hook ()
    (setq prettify-symbols-alist '(("fn" . 955)))
    (prettify-symbols-mode +1))
  (add-hook 'clojure-mode-hook 'my/clojure-mode-hook))

(use-package cider
  :bind (:map cider-repl-mode-map
              ("C-c C-l" . cider-repl-clear-buffer))
  :config
  (setq cider-enhanced-cljs-completion-p nil)
  (defun my/cider-find-var (fn &optional arg var line)
    "(Meant to be adviced around `cider-find-var') invert the
meaning of the prefix argument if it's not `-': if arg is present
remove it, otherwise add it.  `cider-find-var' without the prefix
argument will ask for a symbol (default thing at point), while I
think its more useful to jump to the symbol at point, and ask for
a symbol if an argument is given, like `xref-find-definitions'
does."
    (cond
     ((eql '- arg) (funcall fn '-  var line))
     ((null arg)   (funcall fn 4   var line))
     (t            (funcall fn nil var line))))
  (advice-add 'cider-find-var :around #'my/cider-find-var)

  (defun my/fix-cider-eldoc ()
    "Cider eldoc support for some reason doesn't work anymore fix it.

I suspect it's due to some package or dunno what that defines
`eldoc-documentation-functions', while emacs 27 use
`eldoc-documentation-function' (the former is an emacs 28 thing.)
Commit 4c924bf468961e202e172c731f45eacd116c71b6 is probably the
cause.

Anyway, set forcefully `eldoc-documentation-function' to
`cider-eldoc' fixes the situation."
    (setq eldoc-documentation-function #'cider-eldoc))
  (add-hook 'cider-mode-hook #'my/fix-cider-eldoc))

(use-package project
  :straight nil
  :config
  (let ((map  (make-sparse-keymap))
        (keys '(("f" . project-find-file)
                ("t" . my/project-spawn-term)
                ("s" . my/project-spawn-eshell)
                ("g" . project-find-regexp)
                ("c" . project-compile))))
    (cl-loop for (key . func) in keys
             do (define-key map (kbd key) func))
    ;; (define-key global-map (kbd "C-c p") map)

    (defmacro with-directory (dir &rest body)
      "Execute BODY with `default-directory' bound to DIR."
      (declare (indent defun))
      (let ((d (gensym))
            (newdir dir))
        `(let ((,d default-directory))
           (setq default-directory ,newdir)
           ,@body
           (setq default-directory ,d))))

    ;; very very very similar to projectile-generate-process-name!
    (defun my/generate-bufname (proc new)
      (let* ((p     (substring (cdr (project-current)) 0 -1))
             (pname (file-name-nondirectory p))
             (name  (format "%s %s" proc pname)))
        (if new
            (generate-new-buffer-name name)
          name)))

    (defun my/project-spawn-term (arg)
      (interactive "P")
      (with-directory (cdr (project-current))
        (ansi-term (getenv "SHELL") (my/generate-bufname "term" arg))))

    (defun my/project-spawn-eshell (arg)
      (interactive "P")
      (with-directory (cdr (project-current))
        (let ((eshell-buffer-name (my/project-spawn-term "eshell" arg)))
          (eshell))))))

(use-package flymake
  :straight nil
  :hook (prog-mode . flymake-mode)
  :custom ((flymake-fringe-indicator-position 'left-fringe)
           (flymake-suppress-zero-counters t)
           (flymake-start-on-flymake-mode t)
           (flymake-no-changes-timeout nil)
           (flymake-start-on-save-buffer t)
           (flymake-proc-compilation-prevents-syntax-check t)
           (flymake-wrap-around nil)))

(use-package eglot
  ;; there's a problem between straight.el and eglot.  Seems that the
  ;; correct version of project.el isn't picked up.  Dunno why, but
  ;; with this it just werks™
  :init (defun project-root (project)
          (car (project-roots project)))
  :commands (eglot)
  :config (progn
            (define-key eglot-mode-map (kbd "<f1>") 'eglot-code-actions)
            (define-key eglot-mode-map (kbd "<f2>") 'eglot-format)

            ;; get rid of the highlighting when I'm hovering a symbol
            (add-to-list 'eglot-ignored-server-capabilites
                         :documentHighlightProvider)

            ;; try to make sqls work
            (add-to-list 'display-buffer-alist
                         '("\\*sqls\\*"
                           (display-buffer-reuse-window display-buffer-at-bottom)
                           (reusable-frames . visible)
                           (window-height . 0.3)))

            (defclass eglot-sqls (eglot-lsp-server) ()
              :documentation "SQL's Language Server")
            (add-to-list 'eglot-server-programs '(sql-mode . (eglot-sqls "sqls")))
            (cl-defmethod eglot-execute-command
              ((server eglot-sqls) (command (eql executeQuery)) arguments)
              "For executeQuery."
              (let* ((beg (eglot--pos-to-lsp-position (if (use-region-p)
                                                          (region-beginning)
                                                        (point-min))))
                     (end (eglot--pos-to-lsp-position (if (use-region-p)
                                                          (region-end)
                                                        (point-max))))
                     (res (jsonrpc-request server :workspace/executeCommand
                                           `(:command ,(format "%s" command)
                                                      :arguments ,arguments
                                                      :timeout 0.5
                                                      :range (:start ,beg :end ,end))))
                     (buffer (generate-new-buffer "*sqls*")))
                (with-current-buffer buffer
                  (eglot--apply-text-edits `[(:range
                                              (:start (:line 0 :character 0)
                                                      :end (:line 0 :character 0))
                                              :newText ,res)])
                  (org-mode))
                (pop-to-buffer buffer)))

            (cl-defmethod eglot-execute-command
              ((server eglot-sqls) (_cmd (eql switchDatabases)) arguments)
              "For switchDatabase."
              (let* ((res (jsonrpc-request server :workspace/executeCommand
                                           `(:command "showDatabases"
                                                      :arguments ,arguments
                                                      :timeout 0.5)))
                     (menu-items (split-string res "\n"))
                     (menu `("Eglot code actions:" ("dummy" ,@menu-items)))
                     (db (if (listp last-nonmenu-event)
                             (x-popup-menu last-nonmenu-event menu)
                           (completing-read "[eglot] Pick a database: "
                                            menu-items nil t
                                            nil nil (car menu-items)))))
                (jsonrpc-request server :workspace/executeCommand
                                 `(:command "switchDatabase" :arguments [,db]
                                            :timeout 0.5))))))

(use-package rainbow-mode
    :commands (rainbow-mode)
    :hook (css-mode . rainbow-mode))



;; I'm keeping the hydra at the end so I can freely use any function
;; from any package
(use-package hydra
  :config
  (defhydra hydra-sp (:hint nil)
    "
 Moving^^^^                       Slurp & Barf^^   Wrapping^^            Sexp juggling^^^^               Destructive
------------------------------------------------------------------------------------------------------------------------
 [_a_] beginning  [_n_] down      [_h_] bw slurp   [_R_]   rewrap        [_S_] split   [_t_] transpose   [_c_] change inner  [_w_] copy
 [_e_] end        [_N_] bw down   [_H_] bw barf    [_u_]   unwrap        [_s_] splice  [_A_] absorb      [_C_] change outer
 [_f_] forward    [_p_] up        [_l_] slurp      [_U_]   bw unwrap     [_r_] raise   [_E_] emit        [_k_] kill          [_g_] quit
 [_b_] backward   [_P_] bw up     [_L_] barf       [_(__{__[_] wrap (){}[]   [_j_] join    [_o_] convolute   [_K_] bw kill       [_q_] quit"
    ("?" (hydra-set-property 'hydra-sp :verbosity 1))

    ;; moving
    ("a" sp-beginning-of-sexp)
    ("e" sp-end-of-sexp)
    ("f" sp-forward-sexp)
    ("b" sp-backward-sexp)
    ("n" sp-down-sexp)
    ("N" sp-backward-down-sexp)
    ("p" sp-up-sexp)
    ("P" sp-backward-up-sexp)

    ;; slurping & barfing
    ("h" sp-backward-slurp-sexp)
    ("H" sp-backward-barf-sexp)
    ("l" sp-forward-slurp-sexp)
    ("L" sp-forward-barf-sexp)

    ;; wrapping
    ("R" sp-rewrap-sexp)
    ("u" sp-unwrap-sexp)
    ("U" sp-backward-unwrap-sexp)
    ("(" sp-wrap-round)
    ("[" sp-wrap-square)
    ("{" sp-wrap-curly)

    ;; sexp juggling
    ("S" sp-split-sexp)
    ("s" sp-splice-sexp)
    ("r" sp-raise-sexp)
    ("j" sp-join-sexp)
    ("t" sp-transpose-sexp)
    ("A" sp-absorb-sexp)
    ("E" sp-emit-sexp)
    ("o" sp-convolute-sexp)

    ;; destructive editing
    ("c" sp-change-inner :exit t)
    ("C" sp-change-enclosing :exit t)
    ("k" sp-kill-sexp)
    ("K" sp-backward-kill-sexp)
    ("w" sp-copy-sexp)

    ("q" nil)
    ("g" nil))

  (define-key global-map (kbd "s-c")
    (lambda ()
      (interactive)
      (hydra-set-property 'hydra-sp :verbosity 0)
      (hydra-sp/body)))

  (defhydra hydra-toggle (global-map "C-c C-v"
                                     :columns 4)
    "toggle"
    ("a" abbrev-mode "abbrev")
    ("d" toggle-debug-on-error "debug")
    ("f" auto-fill-mode "fill")
    ("t" toggle-truncate-lines "truncate")
    ("v" visual-line-mode "visual")
    ("w" whitespace-mode "whitespace")
    ("q" nil "quit")
    ("h" (hydra-set-property 'hydra-toggle :verbosity 1) nil))

  ;; I find counter-intuitive the keybindings for left/right
  ;; scrolling.  I mean, C-x > is like "going to the right" and C-x <
  ;; is like "going to the left", but the default keys are the exact
  ;; opposite, so let's swap them.  While there, also make a nice
  ;; hydra!
  (defhydra hydra-hscroll (global-map "C-x" :hint nil)
    (">" scroll-left)
    ("<" scroll-right))

  (defhydra hydra-windowsize (global-map "C-x")
    ("{" shrink-window-horizontally)
    ("}" enlarge-window-horizontally))

  (defhydra hydra-pages (global-map "C-x" :hint nil)
    ("[" backward-page)
    ("]" forward-page)
    ("RET" nil :exit t)
    ("q" nil :exit t))

  (defhydra hydra-grep-like (global-map "M-g")
    ("n" next-error "next")
    ("p" previous-error "prev")
    ("RET" nil :exit t)
    ("q" nil :exit t))

  (with-eval-after-load 'flymake
    (defhydra hydra-flymake (flymake-mode-map "C-c !")
      ("n" flymake-goto-next-error)
      ("p" flymake-goto-prev-error)
      ("RET" nil :exit t)
      ("q" nil :exit t)))

  (defhydra hydra-avy (global-map "M-g")
    "avy goto"
    ("c" avy-goto-char "char")
    ("C" avy-goto-char-2 "char 2")
    ("w" avy-goto-word-1 "word")
    ("f" avy-goto-line))

  (defun my/tick-symbol (x)
    "Return a tick if X is true-ish."
    (if x "x" " "))

  (defun my/emms-player-status ()
    "Return the state of the EMMS player: `not-active', `playing', `paused' or `dunno'.

Modeled after `emms-player-pause'."
    (cond ((not emms-player-playing-p)
           ;; here we should return 'not-active.  The fact is that
           ;; when i change song, there is a short amount of time
           ;; where we are ``not active'', and the hydra is rendered
           ;; always during that short amount of time.  So we cheat a
           ;; little.
           'playing)

          (emms-player-paused-p
           (let ((resume (emms-player-get emms-player-playing-p 'resume))
                 (pause (emms-player-get emms-player-playing-p 'pause)))
             (cond (resume 'paused)
                   (pause  'playing)
                   (t      'dunno))))
          (t (let ((pause (emms-player-get emms-player-playing-p 'pause)))
               (if pause 'playing 'dunno)))))

  (defun my/emms-toggle-time-display ()
    "Toggle the display of time information in the modeline"
    (interactive "")
    (if emms-playing-time-display-p
        (emms-playing-time-disable-display)
      (emms-playing-time-enable-display)))

  (defun my/selectrum-emms ()
    "Select and play a song from the current EMMS playlist."
    (interactive)
    (with-current-emms-playlist
      (emms-playlist-mode-center-current)
      (let* ((selectrum-should-sort-p nil)
             (current-line-number (line-number-at-pos (point)))
             (lines (cl-loop
                     with min-line-number = (line-number-at-pos (point-min))
                     with buffer-text-lines = (split-string (buffer-string) "\n")
                     with lines = nil
                     for l in buffer-text-lines
                     for n = min-line-number then (1+ n)
                     do (push (propertize l' line-num n)
                              lines)
                     finally return (nreverse lines)))
             (selected-line (selectrum-read "Song: " lines
                                            :default-candidate (nth (1- current-line-number) lines)
                                            :require-match t
                                            :no-move-default-candidate t)))
        (when selected-line
          (let ((line (get-text-property 0 'line-num selected-line)))
            (goto-line line)
            (emms-playlist-mode-play-smart)
            (emms-playlist-mode-center-current))))))

  (defhydra hydra-emms (:hint nil)
    "
%(my/emms-player-status) %(emms-track-description (emms-playlist-current-selected-track))

^Volume^     ^Controls^       ^Playback^              ^Misc^
^^^^^^^^----------------------------------------------------------------
_+_: inc     _n_: next        _r_: repeat one [% s(my/tick-symbol emms-repeat-track)]     _t_oggle modeline
_-_: dec     _p_: prev        _R_: repeat all [% s(my/tick-symbol emms-repeat-playlist)]     _T_oggle only time
^ ^          _<_: seek bw     _#_: shuffle            _s_elect
^ ^          _>_: seek fw     _%_: sort               _g_oto EMMS buffer
^ ^        _SPC_: play/pause
^ ^        _DEL_: restart
  "
    ("+" emms-volume-raise)
    ("-" emms-volume-lower)
    ("n" emms-next)
    ("p" emms-previous)
    ("<" emms-seek-backward)
    (">" emms-seek-forward)
    ("SPC" emms-pause)
    ("DEL" (emms-player-seek-to 0))
    ("<backspace>" (emms-player-seek-to 0))
    ("r" emms-toggle-repeat-track)
    ("R" emms-toggle-repeat-playlist)
    ("#" emms-shuffle)
    ("%" emms-sort)
    ("t" (progn (my/emms-toggle-time-display)
                (emms-mode-line-toggle)))
    ("T" my/emms-toggle-time-display)
    ("s" my/selectrum-emms)
    ("g" (progn (emms)
                (with-current-emms-playlist
                  (emms-playlist-mode-center-current))))

    ("q" nil :exit t))

  (define-key global-map (kbd "C-z e") 'hydra-emms/body)

  (defhydra hydra-window (:hint nil)
    "
^File/Buffer^      ^Movements^        ^Misc^              ^Transpose^
^^^^^^^^------------------------------------------------------------------------------
_b_ switch buffer  ^ ^ hjkl           _0_   delete        _t_     transpose frame
_f_ find file      _o_ other window   _1_   delete other  _M-f_   flip frame
_s_ save conf      _O_ OTHER window   _2_   split below   _M-C-f_ flop frame
_r_ reload conf    ^ ^                _3_   split right   _M-s_   rotate frame
^ ^                ^ ^                _SPC_ balance       _M-r_   rotate clockw.
^^^^-------------------------------   _v_   split horiz.  _M-C-r_ rotate anti clockw.
_?_ toggle help    ^ ^                _-_   split vert.
^ ^                ^ ^                _C-l_ recenter line
"
    ("?" (hydra-set-property 'hydra-window :verbosity 1))

    ("b" switch-to-buffer)
    ("f" (if (projectile-project-p)
             (projectile-find-file)
           (call-interactively #'find-file)))

    ("s" window-configuration-to-register)
    ("r" jump-to-register)

    ("k" windmove-up)
    ("j" windmove-down)
    ("h" windmove-left)
    ("l" windmove-right)

    ("o" (other-window 1))
    ("O" (other-window -1))

    ("C-l" recenter-top-bottom)

    ("0" delete-window)
    ("1" delete-other-windows)
    ("2" split-window-below)
    ("3" split-window-right)

    ;; v is like a |, no?
    ("v" split-window-horizontally)
    ("-" split-window-vertically)

    ("SPC" balance-windows)

    ("t" transpose-frame)
    ("M-f" flip-frame)
    ("M-C-f" flop-frame)
    ("M-s" rotate-frame)
    ("M-r" rotate-frame-clockwise)
    ("M-C-r" rotate-frame-anti-anticlockwise)

    ("q" nil :exit nil)
    ("RET" nil :exit nil)
    ("C-g" nil :exit nil))

  (define-key global-map (kbd "C-z u")
    (lambda ()
      (interactive)
      (hydra-set-property 'hydra-window :verbosity 0)
      (hydra-window/body)))

  (defhydra hydra-other-window (global-map "C-x")
    ("o" other-window "next window")
    ("O" (other-window -1) "previous window"))
  (hydra-set-property 'hydra-other-window :verbosity 0)

  nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("5c327f1e2ad451afb2cc1a9d6e33ba548c23be63f9142782880c7e3c7b4a0b44" "e0ca2751f3b849d4d54056312274d5f1aa39e56d2844a1d5797743feea98faa6" "b96f9d3af74bd6b758134c0f587e1e6b39cad97f9d709d033110795cb8da0b57" "c9b17602ab7063bbbcf99f3f74800eaa6e26beaa131a374937e28d381ca259c4" "12fc489c67eb874fead9221152564fd40b1ace698d861a13bfe0683afbdb66dc" "32a3e22993c122ffc0eb716d3013b94bb86044b2536d1b63780e13283f578e11" "33724fc2ebc6972567169cbfc54139ba5f334855d7e74fd0328312ed4f97fb2f" "7bdda6056822c9a8e6d1c72ffc340568f38c54ce1813f75ef01bb69e2f80f774" "8ceaf37709be42c21b3e74597c16ad2a1866a324710b74e3f7388ce8c050d2fa" "d20f39b2c961611e9d135d8e0e92b2444141253f59520063d1119746e202f192" "0e160aa1a035935ea1d70ae8d15e22eb01d9010aa2bec13e1cff50bdc5c04332" "51a10d605e1bc9f3a7912a547d2672d5ab6457e62a00a5e3f4873e6eeee1ffac" "576817b56acd8a77a3957312a14f90b1dad6c9d5e08c0f38b37f301dd8d302c2" "442d9c6c7ab6594bd5970b2d0ab4dd8185fb9602480b309d7326f536ff145e6d" "cc8f864bf528840e09a7e10642bb738dd6edf9ca747745c4a6c1840cd79e1298" "bfae9e706314f5b9da125bf62ba68016318c1d325fd73d05e4a92c664d2da43f" "140c6b56b2ba1fb48169434a1fd8a653922be7e2957da1ffed8cfc19fb90fc43" "0862fe9bb25b1cd6e59509d3ef1c492a1bf55aca50905b9d2109036e52044140" "1617cc7ae9d6e797ef064663e71ccd8af1c7d5a67604bcc33834f25d3be5da50" "ba31dc1c26401c5d5b35de655ea9d7aee9f21527328b54b4da6280c2460f4825" "3aeea09b1181f2a5f924e68a7933920cba06adf165e775883a98749ff57bb10a" "a6b553143cd8a024a045ceeca2c533941d877c8cc16f87ed5d1acdf0c5044bac" "b2f05f6e0fa42e08069f4737e0f7dd0d663c6e3934ba7a42673d1714f7d80400" "9bfd8e0624b0e8e0ba4203901bb58c41b927f8b251272acdcf517d707b5bbc2c" "e5f768c8bf5a5dd28a8b84655b3b5e872e120a75376c669c51805b22910084ca" "5cf82b1be04e91e7024e1934a7a909c124e8e6f2f85fddfacc9724d4ad2ec9b9" "878ddd87e7ecd070bd767237178f1061b01a1eb49afe23c4358499360888339b" "92181d614e1270ac76da6439cf6a14defc9dd16454e0130c8432896632e4c91c" "e240f6d5a8ccd4b2a09518f04ca97cf195f8f337463d62e7c8baa0d5ad40d22c" "5b794603fb798c6f1d57a9c3d5991ed8dd5d0071d1a97f106be4ca772c1d61e8" "5ed24c6f7d30af9c94dfc38fc0fce8ece61e250d5ae1f666d00ef1649dc703d3" "6543614e44b95523f05a0be1e96d78468751f173625b4732d7e99cbce959e312" "0258e7050ef41bbee17959af0cfe6b4e5d2c2e49439a08770ce8f88b202487e9" "c3d13cbba1d87983ef930cda55f08f99f0abb3a4c169446f11141ed19b4af1e6" "b4de17f7a20bd9966e7752a75129df10f33b9a53319de3f1fd48c9fec9806207" "e0b2ad54031a49dd054da83287273f03536c160a2dde832f442e639122add22d" "f6686e892fc9ae1e2c6c447a7bbc4f431e9b28420689eaf01b6fba756ea5f475" "5ded9878f4a765e741f400c081ab4d8c88c78e2c0f44db5c28d4c22fb41526a6" "006a5cbca622bfa20c238c6e7f6856f052cb612381d45c0061c67d1b08b89d2d" "19fb1be04e066c498d6c674b4e61fc922c6467017ec130b5fe2f2e1646c472e9" "b611d0a5e5c71d6ce316588651f0b49c52702e98cfa95795c103fcea60a520e1" "af08c8e7732e89201fd463b3a04d1d57fecb549bf1a191195911dbe58548fb8d" "f4560d1069f889dfef0a7a2b34d6360ad1a0dd1ec80993eb56805cd06121eff5" "410201c91ed9820590f435cca51305cc83c0c900f2ff63ccd08c526e5b453d8c" "9b2818eb1152a37d9d839371473a7446b441e4360ecdd9a9a1fba72dec24c354" "1fafb9b13aadb96480014b871909a26d46c4ee9c3a57b4efea1e6a688e085451" "2b3d504022714945368ef382df8fe9c1929268f9ad49d889e30c8d5244ec8c9e" "fdfaae66c6bc93872c3e4abeb9488a926bf2fba148889a388a75d5fe81fbf44b" "d6d7f76ed646e3b4c8d11dcf004d628cee7b2900aea3c2a63474a0deaa567dd3" "564abaa9051df8d499ab08efb6830a0da279521f1bae39b4d5c99326179e651c" "2386aa5a90d6d6ae25f8c9bb3081c99bebe10622e470408e57d59e95f2902e68" "7168cb6c66db59aefaf58358d192ab8a8f60a6e50b6c2eedb239ff751cc0ed7f" default))
 '(debug-on-error nil)
 '(safe-local-variable-values
   '((elisp-lint-indent-specs
      (if-let* . 2)
      (when-let* . 1)
      (let* . defun)
      (nrepl-dbind-response . 2)
      (cider-save-marker . 1)
      (cider-propertize-region . 1)
      (cider-map-repls . 1)
      (cider--jack-in . 1)
      (cider--make-result-overlay . 1)
      (insert-label . defun)
      (insert-align-label . defun)
      (insert-rect . defun)
      (cl-defun . 2)
      (with-parsed-tramp-file-name . 2)
      (thread-first . 1)
      (thread-last . 1))
     (cider-clojure-cli-global-options . "-A:build")
     (projectile-project-compilation-cmd . "clojure -A:lint --config .clj-kondo/config.edn --lint src/")
     (checkdoc-package-keywords-flag)
     (Base . 10)
     (Package . HUNCHENTOOT)
     (Syntax . COMMON-LISP)
     (eval sql-highlight-postgres-keywords)
     (eval define-clojure-indent
           (codepoint-case 'defun))
     (eval font-lock-add-keywords nil
           `((,(concat "("
                       (regexp-opt
                        '("sp-do-move-op" "sp-do-move-cl" "sp-do-put-op" "sp-do-put-cl" "sp-do-del-op" "sp-do-del-cl")
                        t)
                       "\\_>")
              1 'font-lock-variable-name-face)))))
 '(show-paren-mode nil)
 '(show-smartparens-global-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'downcase-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'scroll-left 'disabled nil)

(provide 'init)
;;; init.el ends here
