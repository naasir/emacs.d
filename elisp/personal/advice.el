;------------------------------
; name    : advice.el
; purpose : advice to tweak existing functionality in emacs
; author  : naasir.ramji@gmail.com
;------------------------------

;------------------------------
; name: grep-compute-defaults
; desc: prevent issues with the Windows null device (NUL) when using cygwin find with rgrep
; link: http://www.emacswiki.org/emacs/GrepMode#toc3
;------------------------------
(defadvice grep-compute-defaults (around grep-compute-defaults-advice-null-device)
  "Use cygwin's /dev/null as the null-device."
  (let ((null-device "/dev/null"))
  ad-do-it))

;------------------------------
; name: shell-quote-argument
; desc: windows needs special quoting to make rgrep/find work
; link: http://www.emacswiki.org/emacs/GrepMode#toc3
;------------------------------
(defadvice shell-quote-argument (after windows-nt-special-quote (argument) activate)
  "Add special quotes to ARGUMENT in case the system type is 'windows-nt."
  (when
      (and (eq system-type 'windows-nt) (w32-shell-dos-semantics))
    (if (string-match "[\\.~]" ad-return-value)
        (setq ad-return-value
              (replace-regexp-in-string
               "\\([\\.~]\\)"
               "\\\\\\1"
               ad-return-value)))))

;------------------------------
; name: hexl-ediff
; desc: automatically diff binary files using hexl-mode
; link: http://trey-jackson.blogspot.com/2010/10/emacs-tip-38-automatically-diff-binary.html
;------------------------------
(defvar ediff-do-hexl-diff nil
  "variable used to store trigger for doing diff in hexl-mode")
(defadvice ediff-files-internal (around ediff-files-internal-for-binary-files activate)
  "catch the condition when the binary files differ

the reason for catching the error out here (when re-thrown from the inner advice)
is to let the stack continue to unwind before we start the new diff
otherwise some code in the middle of the stack expects some output that
isn't there and triggers an error"
  (let ((file-A (ad-get-arg 0))
        (file-B (ad-get-arg 1))
        ediff-do-hexl-diff)
    (condition-case err
        (progn
          ad-do-it)
      (error
       (if ediff-do-hexl-diff
           (let ((buf-A (find-file-noselect file-A))
                 (buf-B (find-file-noselect file-B)))
             (with-current-buffer buf-A
               (hexl-mode 1))
             (with-current-buffer buf-B
               (hexl-mode 1))
             (ediff-buffers buf-A buf-B))
         (error (error-message-string err)))))))

(defadvice ediff-setup-diff-regions (around ediff-setup-diff-regions-for-binary-files activate)
  "when binary files differ, set the variable "
  (condition-case err
      (progn
        ad-do-it)
    (error
     (setq ediff-do-hexl-diff
           (and (string-match-p "^Errors in diff output.  Diff output is in.*"
                                (error-message-string err))
                (string-match-p "^\\(Binary \\)?[fF]iles .* and .* differ"
                                (buffer-substring-no-properties
                                 (line-beginning-position)
                                 (line-end-position)))
                (y-or-n-p "The binary files differ, look at the differences in hexl-mode? ")))
     (error (error-message-string err)))))
