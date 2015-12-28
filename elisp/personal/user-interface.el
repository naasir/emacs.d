;------------------------------
; name    : user-interface.el
; purpose : global ui configuration
; author  : naasir.ramji@gmail.com
;------------------------------

;; hide tool bar
(tool-bar-mode -1)

;; hide scroll bar
(toggle-scroll-bar -1)

;; put scroll bar on right
;; (set-scroll-bar-mode 'right)

;; enable column/line numbers in minibuffer
(column-number-mode 1)
(line-number-mode 1)

;; enable line numbers on side
(global-linum-mode 1)
(setq linum-format " %d")

;; use visible bell (flashing) instead of audible beeping
(setq visible-bell 1)
(setq ring-bell-function 'ignore)

;; type "y"/"n" instead of "yes"/"no"
(fset 'yes-or-no-p 'y-or-n-p)

;; ignore case sensitivity for tab completion
(setq completion-ignore-case t)

;; inhibit startup message
(setq inhibit-startup-message t)

;; show matching parentheses
(show-paren-mode 1)

;; default font
(set-frame-font "Consolas-16")

;; mouse avoidance
(mouse-avoidance-mode 'animate)

;; fullscreen in windows
(if (eq system-type 'windows-nt)
    (w32-send-sys-command 61488))

;; smoother scrolling
(setq redisplay-dont-pause t
  scroll-margin 1
  scroll-step 1
  scroll-conservatively 10000
  scroll-preserve-screen-position 1)

;; turn down time to echo keystrokes to be more responsive
(setq echo-keystrokes 0.1)

;; no dialog boxes!
(setq use-dialog-box nil)

;; show extra whitespace and tabs
(setq-default highlight-tabs t)
(setq-default show-trailing-whitespace t)

;; remove useless whitespace before saving a file
;; (add-hook 'before-save-hook 'whitespace-cleanup) ; TODO: too noisy when fixing merge conflicts
(add-hook 'before-save-hook (lambda() (delete-trailing-whitespace)))
