;------------------------------
; name    : .emacs
; purpose : emacs init file
; author  : naasir.ramji@gmail.com
;------------------------------

;------------------------------
; set load path
;------------------------------
(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
    (let* ((my-lisp-dir (concat (file-name-directory load-file-name) "elisp/"))
           (default-directory my-lisp-dir))
      (setq load-path (cons my-lisp-dir load-path))
      (normal-top-level-add-subdirs-to-load-path)))

;------------------------------
; include other config files
;------------------------------
(load-library "globals")
(load-library "key-bindings")
(load-library "user-interface")
(load-library "advice")
(load-library "functions")
(load-library "profiles")
(load-library "packages")

;------------------------------
; auto-compile .emacs on save
;------------------------------
(defun byte-compile-user-init-file ()
  (let ((byte-compile-warnings '(unresolved)))
    ;; in case compilation fails, don't leave the old .elc around:
    (when (file-exists-p (concat user-init-file ".elc"))
      (delete-file (concat user-init-file ".elc")))
    (byte-compile-file user-init-file)
    (message "%s compiled" user-init-file)))

(defun my-emacs-lisp-mode-hook ()
  (when (equal buffer-file-name user-init-file)
    (add-hook 'after-save-hook 'byte-compile-user-init-file t t)))

;; (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'my-emacs-lisp-mode-hook)

;------------------------------
; backups
;------------------------------
;; Keep backups in one place, not all over the filesystem.
(setq backup-by-copying t ;; copy, don't symlink.
      backup-directory-alist
      '(("." . "~/.emacsbackups")) ;; keep everything in one place.
      delete-old-versions t
      kept-new-versions 2
      kept-old-versions 2
      version-control t) ;; use versioned backups.

;------------------------------
; startup
;------------------------------
(shell) ;;always run a shell
