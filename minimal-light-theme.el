;;; minimal-light-theme.el --- A light/dark minimalistic Emacs 27 theme.  -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Omar Polo
;; Copyright (C) 2014 Anler Hp

;; Author: Anler Hp <anler86 [at] gmail.com>
;; Keywords: color, theme, minimal
;; X-URL: http://github.com/ikame/minimal-theme
;; URL: http://github.com/ikame/minimal-theme

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; A minimalistic color theme to avoid distraction with
;; colors.  Based on monochrome theme.

;;; Code:
(deftheme minimal-light "minimal light theme.")

(let* ((class '((class color) (min-colors 89)))
       (foreground "#586e75")
       (background "#ffffff") ;; "#fdf6e3"
       (cursor "#333")
       (border "grey90")
       (minibuffer cursor)
       (region "grey90")
       (comment-delimiter "grey55")
       (comment "grey30")
       (constant foreground)
       (string "grey40")
       (modeline-foreground foreground)
       (modeline-background "#e1dbca")
       (modeline-foreground-inactive comment)
       (modeline-background-inactive background)
       (modeline-border-color "white")
       (isearch-background modeline-background)
       (hl-background region)
       (hl-face-background nil)
       (failure "red")
       (org-background "grey98"))
  (custom-theme-set-faces
   'minimal-light

   ;; basic stuff
   `(default ((,class (:background ,background :foreground ,foreground))))
   `(fringe ((,class (:inherit default))))
   `(cursor ((,class (:background ,cursor :inverse-video t))))
   `(vertical-border ((,class (:foreground ,border))))

   ;; minibuffer
   `(minibuffer-prompt ((,class (:foreground ,minibuffer :weight bold))))

   ;; region
   `(region ((,class (:background ,region))))
   `(secondary-selection ((,class (:background ,region))))

   ;; faces
   `(font-lock-builtin-face ((,class (:foreground ,foreground :weight bold))))
   `(font-lock-constant-face ((,class (:foreground ,foreground :weight bold))))
   `(font-lock-keyword-face ((,class (:foreground ,foreground :weight bold))))
   `(font-lock-type-face ((,class (:foreground ,foreground :slant italic))))
   `(font-lock-function-name-face ((,class (:foreground ,foreground :weight bold))))
   `(font-lock-variable-name-face ((,class (:foreground ,foreground))))

   `(font-lock-comment-delimiter-face ((,class (:foreground ,comment-delimiter))))
   `(font-lock-comment-face ((,class (:foreground ,comment
                                                  :slant italic))))
   `(font-lock-doc-face ((,class (:inherit (font-lock-comment-face)))))
   `(font-lock-string-face ((,class (:foreground ,foreground :foreground ,string))))

   ;; faces used by isearch
   `(isearch ((,class (:foreground ,foreground :background ,isearch-background :weight normal))))
   `(isearch-fail ((,class (:foreground ,failure :bold t))))
   `(lazy-highlight
     ((,class (:foreground ,foreground :background ,region))))

   ;; ido-mode
   `(ido-subdir ((,class (:foreground ,foreground :weight bold))))
   `(ido-only-match ((,class (:foreground ,foreground :weight bold))))

   ;; show-paren
   `(show-paren-match
     ((,class (:inherit highlight
                           :underline (:color ,foreground :style line)))))
   `(show-paren-mismatch
     ((,class (:foreground ,failure :weight bold))))
   `(show-paren-match-expression
     ((,class (:inherit default :background "#eee"))))

   ;; highlight-sexp
   `(hl-sexp-face
     ((,class (:inherit show-paren-match-expression))))

   ;; modeline
   `(mode-line
       ((,class (:inverse-video unspecified
                                :overline ,border
                                :underline nil
                                :foreground ,modeline-foreground
                                :background ,modeline-background
                                :overline ,border
                                :box (:line-width 3 :color ,modeline-background)
                                ))))
     `(mode-line-buffer-id ((,class (:weight bold))))
     `(mode-line-inactive
       ((,class (:inverse-video unspecified
                                :overline ,border
                                :underline nil
                                :foreground ,modeline-foreground-inactive
                                :background ,modeline-background-inactive
                                :box (:line-width 3 :color ,background)
                                ))))

      ;; hl-line-mode
     `(hl-line ((,class (:background ,hl-background))))
     `(hl-line-face ((,class (:background ,hl-face-background))))

      ;; highlight-stages-mode
     `(highlight-stages-negative-level-face ((,class (:foreground ,failure))))
     `(highlight-stages-level-1-face ((,class (:background ,org-background))))
     `(highlight-stages-level-2-face ((,class (:background ,region))))
     `(highlight-stages-level-3-face ((,class (:background ,region))))
     `(highlight-stages-higher-level-face ((,class (:background ,region))))

     ;; org-mode
     `(org-level-1 ((,class (:foreground ,foreground :height 1.6))))
     `(org-level-2 ((,class (:foreground ,foreground :height 1.5))))
     `(org-level-3 ((,class (:foreground ,foreground :height 1.4))))
     `(org-level-4 ((,class (:foreground ,foreground :height 1.3))))
     `(org-level-5 ((,class (:foreground ,foreground :height 1.2))))
     `(org-level-6 ((,class (:foreground ,foreground :height 1.1))))
     `(org-level-7 ((,class (:foreground ,foreground))))
     `(org-level-8 ((,class (:foreground ,foreground))))

     `(org-block ((,class (:inherit fixed-pitch))))
     `(org-table   ((,class (:inherit fixed-pitch))))
     `(org-meta-line ((,class (:inherit (font-lock-comment-face fixed-pitch)))))
     `(org-property-value ((,class (:inherit fixed-pitch))) t)
     `(org-verbatim ((,class (:inherit (shadow fixed-pitch)))))
     `(org-quote ((,class (:slant italic))))

     ;; outline
     `(outline-1 ((,class (:inherit org-level-1))))
     `(outline-2 ((,class (:inherit org-level-2))))
     `(outline-3 ((,class (:inherit org-level-3))))
     `(outline-4 ((,class (:inherit org-level-4))))
     `(outline-5 ((,class (:inherit org-level-5))))
     `(outline-6 ((,class (:inherit org-level-6))))
     `(outline-7 ((,class (:inherit org-level-7))))
     `(outline-8 ((,class (:inherit org-level-8))))

     `(org-document-title ((,class (:foreground ,foreground))))

     `(org-link ((,class (:foreground ,foreground :underline t))))
     `(org-tag ((,class (:background ,org-background :foreground ,foreground))))
     `(org-warning ((,class (:background ,region :foreground ,foreground :weight bold))))
     `(org-todo ((,class (:weight bold))))
     `(org-done ((,class (:weight bold))))
     `(org-headline-done ((,class (:foreground ,foreground))))

     `(org-table ((,class (:background ,org-background))))
     `(org-code ((,class (:background ,org-background))))
     `(org-date ((,class (:background ,org-background :underline t))))
     `(org-block ((,class (:background ,org-background))))
     `(org-block-background ((,class (:background ,org-background :foreground ,foreground))))
     `(org-block-begin-line
       ((,class (:background ,org-background :foreground ,comment-delimiter :weight bold))))
     `(org-block-end-line
       ((,class (:background ,org-background :foreground ,comment-delimiter :weight bold))))

     ;; js2-mode
     `(js2-external-variable ((,class (:inherit base-faces :weight bold))))
     `(js2-function-param ((,class (:inherit base-faces))))
     `(js2-instance-member ((,class (:inherit base-faces))))
     `(js2-jsdoc-html-tag-delimiter ((,class (:inherit base-faces))))
     `(js2-jsdoc-html-tag-name ((,class (:inherit base-faces))))
     `(js2-jsdoc-tag ((,class (:inherit base-faces))))
     `(js2-jsdoc-type ((,class (:inherit base-faces :weight bold))))
     `(js2-jsdoc-value ((,class (:inherit base-faces))))
     `(js2-magic-paren ((,class (:underline t))))
     `(js2-private-function-call ((,class (:inherit base-faces))))
     `(js2-private-member ((,class (:inherit base-faces))))

     ;; sh-mode
     `(sh-heredoc ((,class (:inherit base-faces :slant italic))))

     ;; telega
     `(telega-msg-heading ((,class (:inherit base-faces :underline ,comment-delimiter
                                             :foreground ,comment))))
     `(telega-msg-user-title ((,class (:inherit telega-msg-heading))))
     `(telega-msg-inline-reply ((,class (:inherit telega-msg-heading
                                                  :slant italic))))

     ;; objed
     `(objed-hl ((,class (:background ,region))))))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'minimal-light)
;;; minimal-light-theme.el ends here
