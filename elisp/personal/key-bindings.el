;------------------------------
; name    : key-bindings.el
; purpose : custom keybindings
; author  : naasir.ramji@gmail.com
;------------------------------

; set C-k to kill whole line
(global-set-key (kbd "C-k") 'kill-whole-line)

; set C-d to backward kill word
(global-set-key (kbd "C-d") 'backward-kill-word)

; set C-x C-u to undo
(global-set-key (kbd "C-x C-u") 'undo)

; set C-/ to custom comment command
(global-set-key (kbd "C-/") 'naasir-comment-dwim)

; set S-C-<arrow-key> to resize windows
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

; use regex searches by default
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

; keybindings for navigating to specific character
(global-set-key (kbd "C-c f") 'iy-go-to-char)
(global-set-key (kbd "C-c F") 'iy-go-to-char-backward)
(global-set-key (kbd "C-c ;") 'iy-go-to-char-continue)
(global-set-key (kbd "C-c ,") 'iy-go-to-char-continue-backward)

; open ibuffer instead of list-buffers
(global-set-key (kbd "C-x C-b") 'ibuffer)
