;------------------------------
; name    : packages.el
; purpose : configuration for packages
; author  : naasir.ramji@gmail.com
;------------------------------

;------------------------------
; setup package management
;------------------------------

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;; install use-package first
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;------------------------------
; cygwin
;------------------------------
; NOTE: setup-cygwin not installed via use-package
; as no package exists. Must be installed locally.
(use-package cygwin-mount
  :ensure t
  :config
  (progn
    (when (memq system-type '(windows-nt))
      (require 'setup-cygwin))))

;------------------------------
; console
;------------------------------

;; shell
(use-package shell
  :init
  (setq comint-scroll-to-bottom-on-input t)  ; always insert at the bottom
  (setq comint-scroll-to-bottom-on-output t) ; always add output at the bottom
  (setq comint-scroll-show-maximum-output t) ; scroll to show max possible output
  (setq comint-completion-autolist t)        ; show completion list when ambiguous
  (setq comint-input-ignoredups t)           ; no duplicates in command history
  (setq comint-completion-addsuffix t)       ; insert space/slash after file completion
  (setq pcomplete-ignore-case t))            ; ignore case for completion

;; need this on osx
(use-package exec-path-from-shell
  :ensure t
  :config
    (progn
      (when (memq window-system '(mac ns))
        (exec-path-from-shell-initialize))))

;------------------------------
; editing enhancements
;------------------------------

;; expand-region
(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package unfill
  :ensure t)

;------------------------------
; interface enhancements
;------------------------------

;; browse kill ring
(use-package browse-kill-ring
  :ensure t
  :bind ("M-y" . browse-kill-ring))

;; color theme
(use-package color-theme)

(use-package atom-one-dark-theme
  :ensure t
  :config (load-theme 'atom-one-dark t))

;; drag stuff
(use-package drag-stuff
  :ensure t
  :init
  (progn
    (drag-stuff-global-mode t)
    (add-to-list 'drag-stuff-except-modes 'org-mode 'drag-stuff-mode)))

;; interactive do
(use-package ido
  :init
  (progn
    (ido-mode 1)
    (ido-everywhere 1))
  :config
  (progn
    (setq ido-case-fold t)
    (setq ido-everywhere t)
    (setq ido-use-faces nil)
    (setq ido-enable-flex-matching t)
    (setq ido-create-new-buffer 'always)
    (add-to-list 'ido-ignore-files "\\.DS_Store")))

;; flx - https://github.com/lewang/flx
(use-package flx-ido
  :ensure t
  :config
  (flx-ido-mode 1))

;; interactive do completing read
(use-package ido-completing-read+
  :ensure t)

;; smart M-x enhancement
(use-package smex
  :ensure t
  :defer t
  :bind ("M-x" . smex)
  :config
  (progn
    (smex-initialize)))

;; same frame speedbar
(use-package sr-speedbar
  :ensure t
  :bind ("<f12>" . sr-speedbar-toggle)
  :config
  (progn
    (setq speedbar-show-unknown-files t)))

;; switch window visually
(use-package switch-window
  :ensure t
  :bind ("C-x o" . switch-window))

;; displays the available key bindings automatically and dynamically
(use-package guide-key
  :ensure t
  :diminish guide-key-mode
  :config
  (progn
    (setq guide-key/popup-window-position 'bottom
          guide-key/guide-key-sequence t  ; enable for all prefixes
          guide-key/recursive-key-sequence-flag t)

    (defun guide-key/org-mode-hook ()
      (guide-key/add-local-highlight-command-regexp "org-"))
    (add-hook 'org-mode-hook 'guide-key/org-mode-hook)

    (guide-key-mode 1)))

;------------------------------
; language modes
;------------------------------

;; c/c++ major mode
(use-package cc-mode
  :config
  (progn
    (defvaralias 'c-basic-offset 'tab-width))) ; set indent-level same as tab-width

;; css major mode
(use-package css-mode
  :config
  (progn
    (defvaralias 'css-indent-offset 'tab-width))) ; set indent-level same as tab-width

;; javascript major mode
(use-package js2-mode
  :ensure t
  :mode ("\\.js[x]?$" . js2-mode)
  :interpreter ("node" . js2-mode)
  :config
  (defvaralias 'js2-basic-offset 'tab-width)) ; set indent-level same as tab-width

;; json mode
(use-package json-mode
  :ensure t
  :defer t)

;; less css mode
(use-package less-css-mode
  :ensure t)

;; markdown major mode
(use-package markdown-mode
  :ensure t
  :mode ("\\.\\(m\\(ark\\)?down\\|md\\)$" . markdown-mode))

;; org mode
(use-package org
  :init
  (setq org-todo-keyword-faces
      '(
        ("MAYBE" . (:foreground "blue" :weight bold))
        ("STARTED" . (:foreground "yellow" :weight bold))
        ("DEFERRED" . (:foreground "purple" :weight bold))
        ("CANCELLED" . (:foreground "gray" :weight bold))
        ))
  (setq org-todo-keywords
      '((sequence "TODO" "MAYBE" "STARTED" "|" "DONE" "DEFERRED" "CANCELLED")))
  (setq org-completion-use-ido t)
  (setq org-confirm-babel-evaluate nil)
  (setq org-hide-leading-stars t)
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)
  ;; (setq org-startup-folded nil)
  (setq org-startup-indented t)
  (setq org-startup-truncated nil))

;; scala major mode
(use-package scala-mode2
  :ensure t)

;; sbt major mode
(use-package sbt-mode
  :ensure t)

;; major mode for html templates
(use-package web-mode
  :ensure t
  :mode  (("\\.html?\\'"    . web-mode)
          ("\\.jsx$"        . web-mode)
          ("\\.erb\\'"      . web-mode)
          ("\\.ejs\\'"      . web-mode)
          ("\\.ect\\'"      . web-mode)
          ("\\.as[cp]x\\'"  . web-mode)
          ("\\.mustache\\'" . web-mode)
          ("\\.dhtml\\'"    . web-mode))
  :config
  (progn
    (defvaralias 'web-mode-attr-indent-offset 'tab-width) ; set indent-level same as tab-width
    (defadvice web-mode-highlight-part (around tweak-jsx activate)
      (if (equal web-mode-content-type "jsx")
          (let ((web-mode-enable-part-face nil))
            ad-do-it)
        ad-do-it))))

;------------------------------
; minor modes
;------------------------------
(use-package auto-revert-tail-mode
  :mode "\\.log\\'")

(use-package yasnippet
  :ensure t
  :init
  (progn
    (use-package yasnippets)
    (yas-global-mode 1)
    (setq-default yas/prompt-functions '(yas/ido-prompt))))

;; fold/unfold code blocks
(use-package hideshow
  :bind (("C-c <right>" . hs-show-block)
         ("C-c <left>"  . hs-hide-block)
         ("C-c <down>"  . hs-show-all)
         ("C-c <up>"    . hs-hide-level))
  :init (add-hook 'prog-mode-hook 'hs-minor-mode))

;; on-the-fly syntax checking
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :config
  (progn
    (flycheck-add-mode 'javascript-eslint 'web-mode)
    (setq-default flycheck-disabled-checkers
                  (append flycheck-disabled-checkers
                          '(javascript-jshint
                            emacs-lisp-checkdoc
                            json-jsonlist)))))

;------------------------------
; utilities
;------------------------------

;; easy pg
(use-package epa-file)

;; ediff
(use-package ediff
  :config
  (progn
    (setq-default ediff-highlight-all-diffs 'nil) ; only highlight current diff
    (setq ediff-merge-split-window-function 'split-window-horizontally)
    (setq ediff-split-window-function 'split-window-horizontally)
    (setq ediff-window-setup-function 'ediff-setup-windows-plain))
    ; press 'd' to copy both a and b to c (see: http://stackoverflow.com/a/29757750)
    (defun ediff-copy-both-to-C ()
      (interactive)
      (ediff-copy-diff ediff-current-difference nil 'C nil
         (concat
           (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
           (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))
    (defun add-d-to-ediff-mode-map () (define-key ediff-mode-map "d" 'ediff-copy-both-to-C))
    (add-hook 'ediff-keymap-setup-hook 'add-d-to-ediff-mode-map))

;; git interface
(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
  :init
  (progn
    (use-package magit-blame)
    (bind-key "C-c C-a" 'magit-just-amend magit-mode-map))
  :config
  (progn
    (setq magit-default-tracking-name-function 'magit-default-tracking-name-branch-only)
    (setq magit-set-upstream-on-push t)
    (setq magit-completing-read-function 'magit-ido-completing-read)
    (setq magit-stage-all-confirm nil)
    (setq magit-unstage-all-confirm nil)
    (setq magit-restore-window-configuration t)))

;; REST client
(use-package restclient
  :ensure t)

;; tramp
(use-package tramp
  :ensure t
  :config
  (progn
    (setq tramp-default-method "scpx")))

;; project interaction library
(use-package projectile
  :ensure t
  :diminish projectile-mode
  :init
  (setq projectile-enable-caching t)
  :config
  (progn
    (add-to-list 'projectile-globally-ignored-directories "elpa")
    (add-to-list 'projectile-globally-ignored-directories ".cache")
    (add-to-list 'projectile-globally-ignored-directories "node_modules")
    (add-to-list 'projectile-globally-ignored-directories "target")
    (add-to-list 'projectile-globally-ignored-directories "target-eclipse")
    (projectile-global-mode)))

;; platinum searcher
(use-package pt
  :ensure t
  :if (executable-find "pt")
  :bind ("M-?" . projectile-pt))

;; editable  grep
(use-package wgrep
  :ensure t)

(use-package wgrep-pt
  :ensure t)
