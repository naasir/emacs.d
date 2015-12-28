;------------------------------
; name    : globals.el
; purpose : configuration for global variables
; author  : naasir.ramji@gmail.com
;------------------------------

;; user details
(setq user-full-name "Naasir Ramji")
(setq user-mail-address "naasir.ramji@gmail.com")

;; the number of bytes of consing before a garbage collection is invoked
;; @see: https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/
(setq gc-cons-threshold 100000000)

;; insert spaces instead of tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

;; default directory
(setq default-directory "~/")

;; default mode
(setq initial-major-mode 'org-mode)

;; emtpy scratch buffer
(setq initial-scratch-message nil)

;; try to improve slow performance on windows
(setq w32-get-true-file-attributes nil)

;; use UTF-8 as the default encoding system
(prefer-coding-system 'utf-8)

;; Navigate between windows using Alt-1, Alt-2, Shift-left, shift-up, shift-right
(windmove-default-keybindings)

;; mac specific settings
(when (eq system-type 'darwin)
  (setq mac-option-modifier 'super)
  (setq mac-command-modifier 'meta)

  ;; Use M-w to copy Emacs buffer selection; and Cmd-V to paste outside of Emacs.
  ;; Use Cmd-C to copy selection from OSX; and C-y to paste in Emacs
  (defun copy-from-osx ()
    (shell-command-to-string "pbpaste"))

  (defun paste-to-osx (text &optional push)
    (let ((process-connection-type nil))
      (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
        (process-send-string proc text)
        (process-send-eof proc))))

  (setq interprogram-cut-function 'paste-to-osx)
  (setq interprogram-paste-function 'copy-from-osx)
)
