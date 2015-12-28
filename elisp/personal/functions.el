;------------------------------
; name    : functions.el
; purpose : utility functions
; author  : naasir.ramji@gmail.com
;------------------------------

;------------------------------
; Does smart commenting of marked regions or current line.
; Insipired by: http://exceedhl.wordpress.com/2006/07/13/tweak-emacs/
;------------------------------
(defun naasir-comment-dwim (arg)
  "When a region exists, execute comment-dwim, or if comment or uncomment the current line according to if the current line is a comment."
  (interactive "*P")
  (if mark-active
      (comment-dwim arg)
    (save-excursion
    (beginning-of-line)
    (push-mark (point) nil t)
    (end-of-line)
    (comment-dwim arg))))
