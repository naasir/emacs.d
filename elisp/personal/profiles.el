;------------------------------
; name    : profiles.el
; purpose : custom configuration profiles for different paths
; author  : naasir.ramji@gmail.com
;------------------------------

(require 'auto-minor-mode)

(define-minor-mode tabs-mode
  "Indentation via tabs (not spaces)."
  :lighter " tabs"
  (setq-local tab-width 4)
  (setq-local indent-tabs-mode t))

;------------------------------
; Work source code uses tabs instead of spaces
;------------------------------
; NOTE: Backquote syntax, see: http://emacswiki.org/emacs/BackquoteSyntax
(add-to-list 'auto-minor-mode-alist `(,(expand-file-name "~/code/main/")  . tabs-mode))
