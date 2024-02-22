(defun go--goto-line (line)
  (goto-char (point-min))
  (forward-line (1- line)))

(defun go--delete-whole-line (&optional arg)
  "Delete the current line without putting it in the `kill-ring'.
Derived from function `kill-whole-line'.  ARG is defined as for that
function."
  (setq arg (or arg 1))
  (if (and (> arg 0)
           (eobp)
           (save-excursion (forward-visible-line 0) (eobp)))
      (signal 'end-of-buffer nil))
  (if (and (< arg 0)
           (bobp)
           (save-excursion (end-of-visible-line) (bobp)))
      (signal 'beginning-of-buffer nil))
  (cond ((zerop arg)
         (delete-region (progn (forward-visible-line 0) (point))
                        (progn (end-of-visible-line) (point))))
        ((< arg 0)
         (delete-region (progn (end-of-visible-line) (point))
                        (progn (forward-visible-line (1+ arg))
                               (unless (bobp)
                                 (backward-char))
                               (point))))
        (t
         (delete-region (progn (forward-visible-line 0) (point))
                        (progn (forward-visible-line arg) (point))))))

(defun go--apply-rcs-patch (patch-buffer)
  "Apply an RCS-formatted diff from PATCH-BUFFER to the current buffer."
  (let ((target-buffer (current-buffer))
        ;; Relative offset between buffer line numbers and line numbers
        ;; in patch.
        ;;
        ;; Line numbers in the patch are based on the source file, so
        ;; we have to keep an offset when making changes to the
        ;; buffer.
        ;;
        ;; Appending lines decrements the offset (possibly making it
        ;; negative), deleting lines increments it. This order
        ;; simplifies the forward-line invocations.
        (line-offset 0)
        (column (current-column)))
    (save-excursion
      (with-current-buffer patch-buffer
        (goto-char (point-min))
        (while (not (eobp))
          (unless (looking-at "^\\([ad]\\)\\([0-9]+\\) \\([0-9]+\\)")
            (error "Invalid rcs patch or internal error in go--apply-rcs-patch"))
          (forward-line)
          (let ((action (match-string 1))
                (from (string-to-number (match-string 2)))
                (len  (string-to-number (match-string 3))))
            (cond
             ((equal action "a")
              (let ((start (point)))
                (forward-line len)
                (let ((text (buffer-substring start (point))))
                  (with-current-buffer target-buffer
                    (cl-decf line-offset len)
                    (goto-char (point-min))
                    (forward-line (- from len line-offset))
                    (insert text)))))
             ((equal action "d")
              (with-current-buffer target-buffer
                (go--goto-line (- from line-offset))
                (cl-incf line-offset len)
                (go--delete-whole-line len)))
             (t
              (error "Invalid rcs patch or internal error in go--apply-rcs-patch")))))))
    (move-to-column column)))


(defun gofmt ()
  "Format the current buffer according to the formatting tool."
  (interactive)
  (let ((tmpfile (make-nearby-temp-file "gofmt" nil ".go"))
        (patchbuf (get-buffer-create "*Gofmt patch*"))
        (coding-system-for-read 'utf-8)
        (coding-system-for-write 'utf-8)
        our-gofmt-args)
    
    (unwind-protect
        (save-restriction
          (widen)
          (with-current-buffer patchbuf
            (erase-buffer))
          (write-region nil nil tmpfile)
          
          (setq our-gofmt-args
                (append our-gofmt-args
                        (list "-w" (file-local-name tmpfile))))
          (message "Calling gofmt: %s %s" "gofmt" our-gofmt-args)
          ;; We're using errbuf for the mixed stdout and stderr output. This
          ;; is not an issue because gofmt -w does not produce any stdout
          ;; output in case of success.
          (if (zerop (apply #'process-file "gofmt" nil nil nil our-gofmt-args))
              (progn
                ;; There is no remote variant of ‘call-process-region’, but we
                ;; can invoke diff locally, and the results should be the same.
                (if (zerop (let ((local-copy (file-local-copy tmpfile)))
                             (unwind-protect
                                 (call-process-region
                                  (point-min) (point-max) "diff" nil patchbuf
                                  nil "-n" "-" (or local-copy tmpfile))
                               (when local-copy (delete-file local-copy)))))
                    (message "Buffer is already gofmted")
                  (go--apply-rcs-patch patchbuf)
                  (message "Applied gofmt")))
            (message "Could not apply gofmt")))

      (kill-buffer patchbuf)
      (delete-file tmpfile))))


(major-mode-hydra-define go-ts-mode
  (:hint nil :color blue :quit-key "q")
  ("format"
   (("f" gofmt "format"))))

(setq go-ts-mode-indent-offset 4)


(provide 'init-go)
