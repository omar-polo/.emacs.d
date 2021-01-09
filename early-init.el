;; -*- lexical-binding: t -*-

;; disable bloat
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))

(defvar my/default-gc-cons-threshold gc-cons-threshold
  "Backup of the default GC threshold")

(defvar my/default-gc-cons-percentage gc-cons-percentage
  "Backup of the default GC cons percentage")

;; boost the gc during the load
(setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
      gc-cons-percentage 0.6)

;; and reset it to "normal" when done
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold my/default-gc-cons-threshold
                  gc-cons-percentage my/default-gc-cons-percentage)))

;; don't initialize package.el!
(setq package-enable-at-startup nil)

(setq inhibit-startup-message t
      initial-scratch-message nil)

(add-to-list 'load-path
             (expand-file-name "lisp" user-emacs-directory))

(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")

;; (load-theme 'op t)

(if (= 28 emacs-major-version)
    (load-theme 'modus-operandi t)
  (load-theme 'minimal-light t))

(require 'my-modeline)

(defvar my/font "Go mono 9")
(defvar my/variable-font "Input Serif Narrow 12")

;; (setq my/font "Go mono 9")
;; (set-frame-font my/font)
;; (add-to-list 'default-frame-alist `(font . ,my/font))

(set-face-attribute 'default nil :font my/font)
(set-face-attribute 'variable-pitch nil :font "DejaVu Serif 11")

;; make some symbols, such as 📌, be non-tofu
;; (set-fontset-font "fontset-default" 'unicode "Noto Emoji" nil 'prepend)

;; Resizing the Emacs frame can be a terribly expensive part of
;; changing the font. By inhibiting this, we easily halve startup
;; times with fonts that are larger than the system default.
(setq frame-inhibit-implied-resize t)
