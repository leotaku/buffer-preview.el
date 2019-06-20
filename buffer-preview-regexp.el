;;; buffer-preview-regexp --- visual-regexp replacement

(require 'buffer-preview-core)

(defun bpvr/replace ()
  (interactive)
  (let (bpvr/last-regexp replace replacements)
    (setq bpvr/last-regexp
          (bp/read-from-minibuffer "Search: " 'bpvr/update-regexp-replace))
    (setq replace
          (bp/read-from-minibuffer "Replace: " 'bpvr/update-regexp-replace 'bpvr//perform-replace))))

(defun bpvr//perform-replace (_ overlays)
  (mapc (lambda (ov)
          (let ((beg (overlay-start ov))
                (end (overlay-end ov))
                (replace (overlay-get ov 'after-string)))
            (delete-region beg end)
            (save-excursion
              (goto-char beg)
              (when replace
                (insert replace)))))
        overlays))

(defun bpvr//put-overlays (regexp replace &optional beg end)
  (let* ((matches 0)
         (even t)
         (string (buffer-string))
         (pos (or beg 0))
         (max (or end (length string))))
    (save-match-data
      (cl-block top-level
        (cl-loop
         while (and (string-match regexp string pos))
         do
         (setq pos (match-end 0))
         (when (> pos max)
           (cl-return))
         (setq even (not even))
         (setq matches (1+ matches))
         (setq this-replace replace)
         (let ((beg (match-beginning 0))
               (end (match-end 0)))
           (cond ((= beg end)
                  (cl-return-from top-level "Infinite"))
                 (t
                  (cl-loop
                   for i from 1
                   do
                   (let ((beg (match-beginning i))
                         (end (match-end i)))
                     (if (not beg)
                         (cl-return)
                       (bp/add-overlay
                        (1+ beg) (1+ end)
                        'face (intern (format "vr/group-%s" (% i 3)))
                        'priority 1)
                       (setq this-replace (replace-regexp-in-string
                                           (format "[\\]%s" i)
                                           (buffer-substring (1+ beg) (1+ end))
                                           this-replace t t)))))
                  (setq this-replace (replace-regexp-in-string
                                      "[\\]&"
                                      (buffer-substring (1+ beg) (1+ end))
                                      this-replace t t))
                  (bp/add-overlay
                   (1+ beg) (1+ end)
                   'face (if even
                             'vr/match-0
                           'vr/match-1)
                   'after-string this-replace)))))
        matches))))

(defun bpvr/update-regexp-replace (contents mini)
  (if (bound-and-true-p bpvr/last-regexp)
      (setq regexp bpvr/last-regexp
            replace contents)
    (setq regexp contents
          replace ""))
  (bp/set-minibuffer-suffix mini "")
  (let ((message
         (condition-case err
             (format
              "%s matches"
              (if (region-active-p)
                  (bpvr//put-overlays (pcre-to-elisp regexp) replace
                                      (region-beginning) (region-end))
                (bpvr//put-overlays (pcre-to-elisp regexp) replace)))
           (error
            (format "error: %s" (cadr err))))))
    (bp/set-minibuffer-suffix mini (format " [%s]" message))))

(provide 'bpvr)




