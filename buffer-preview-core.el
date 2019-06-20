;;; buffer-preview --- simple to use previews from the minibuffer -*- lexical-binding: t; -*-

;; Author: Leo Gaskin <leo.gaskin@brg-feldkirchen.at>
;; Created: 26 May 2019
;; Homepage: https://github.com/leotaku/buffer-preview
;; Keywords: visual-regexp, regex, preview, anzu
;; Package-Version: 0.1.0
;; Package-Requires: ((emacs "25.1"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary

;;; Code

;;;###autoload
(defun bp/read-from-minibuffer (prompt update-fn &optional finish-fn)
  (when (not finish-fn)
    (setq finish-fn (lambda (read _)
                      read)))
  (let ((inhibit-quit t))
    (minibuffer-with-setup-hook 'bp//minibuffer-setup
      (setq bp/target-buffer-var (current-buffer))
      (setq bp/update-fn-var update-fn)
      (let ((read (with-local-quit
                    (read-from-minibuffer prompt))))
        (prog1
            (when read (funcall finish-fn read (bp//get-overlays)))
          (bp//remove-overlays))))))

(defun bp//minibuffer-setup ()
  "Function that is run just after the minibuffer is entered.
Sets up the after-change hooks as well as future cleanup."
  (let ((after-change-function
         (bp//gen-after-change
          (current-buffer) bp/target-buffer-var bp/update-fn-var)))
    (add-hook 'after-change-functions
              after-change-function
              nil t)
    (unless (bound-and-true-p bp//minibuffer-previously-setup)
      (add-hook 'minibuffer-exit-hook
                (lambda ()
                  (setq-local after-change-functions nil)
                  (bp//remove-overlays)))
      (setq bp/minibuffer-previously-setup nil)))
  (run-hooks 'after-change-functions))

(defun bp//gen-after-change (mini target update-fn)
  "Function that generates a lambda suitable to be added to `after-change-functions'."
  `(lambda (&rest _)
     (let ((contents (with-current-buffer ,mini (minibuffer-contents))))
       (with-current-buffer ,target
         (remove-overlays nil nil 'bp-val t)
         (funcall ',update-fn contents ,mini)))))

(defun bp/add-overlay (beg end &rest props)
  "Add a temporary overlay from `beg' to `end' with properties `props'.
Only call this function from within your buffer-preview update function."
  (let ((ov (make-overlay beg end (current-buffer) nil nil)))
    (while (not (null props))
      (overlay-put ov 'bp-val t)
      (overlay-put ov (car props) (cadr props))
      (setq props (cddr props)))))

(defun bp/set-minibuffer-suffix (minibuffer text)
  (with-current-buffer minibuffer
    (message nil)
    (remove-overlays nil nil 'bp-mini t)
    (unless (zerop (length text))
      ;; The current C cursor code doesn't know to use the overlay's
      ;; marker's stickiness to figure out whether to place the cursor
      ;; before or after the string, so let's spoon-feed it the pos.
      (put-text-property 0 1 'cursor t text))
    (let ((ov (make-overlay (point-max) (point-max) nil t t)))
      (move-overlay ov (point-max) (point-max))
      (overlay-put ov 'after-string text)
      (overlay-put ov 'bp-mini t))))

(defun bp/set-minibuffer-prefix (minibuffer text)
  (with-current-buffer minibuffer
    (let ((inhibit-read-only t))
      (put-text-property (point-min) (minibuffer-prompt-end) 'display text))))

(defun bp//get-overlays ()
  (let (result)
    (mapc
     (lambda (ov)
       (when (eq (overlay-get ov 'bp-val) t)
         (push ov result)))
     (overlays-in
      (point-min)
      (point-max)))
    (nreverse result)))

(defun bp//remove-overlays ()
  (remove-overlays nil nil 'bp-val t))

(provide 'buffer-preview-core)
