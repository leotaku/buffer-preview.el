;;; simple-regex --- simple to use previews from the minibuffer -*- lexical-binding: t; -*-

;; Author: Leo Gaskin <leo.gaskin@brg-feldkirchen.at>
;; Created: 26 May 2019
;; Homepage: https://github.com/leotaku/simple-regex
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

(defun sr/interactive-read (prompt update-fn)
  (minibuffer-with-setup-hook 'sr/minibuffer-setup
    (setq sr/target-buffer-var (current-buffer))
    (setq sr/update-fn-var update-fn)
    (read-from-minibuffer prompt)))

(defun sr/minibuffer-setup ()
  "Function that is run just after the minibuffer is entered.
Sets up the after-change hooks as well as future cleanup."
  (add-hook 'after-change-functions
            (sr/gen-after-change
             (current-buffer) sr/target-buffer-var sr/update-fn-var)
            nil t)
  (add-hook 'minibuffer-exit-hook
            `(lambda ()
               (kill-buffer ,(current-buffer))
               (with-current-buffer ,sr/target-buffer-var
                 (remove-overlays nil nil 'sr-val t))))
  (run-hooks 'after-change-functions))

(defun sr/gen-after-change (mini target update-fn)
  "Function that generates a lambda suitable to be added to `after-change-functions'."
  `(lambda (&rest _)
     (let ((contents (with-current-buffer ,mini (minibuffer-contents))))
       (with-current-buffer ,target
         (remove-overlays nil nil 'sr-val t)
         (funcall ',update-fn contents ,mini)))))

(defun sr/add-overlay (beg end &rest props)
  "Add a temporary overlay from `beg' to `end' with properties `props'.
Only call this function from within your simple-regex update function."
  (let ((ov (make-overlay beg end (current-buffer) nil nil)))
    (while (not (null props))
      (overlay-put ov 'sr-val t)
      (overlay-put ov (car props) (cadr props))
      (setq props (cddr props)))))

(defun sr/set-minibuffer-suffix (minibuffer text)
  (with-current-buffer minibuffer
    (message nil)
    (unless (zerop (length text))
      ;; The current C cursor code doesn't know to use the overlay's
      ;; marker's stickiness to figure out whether to place the cursor
      ;; before or after the string, so let's spoon-feed it the pos.
      (put-text-property 0 1 'cursor t text))
    (let ((ov (or
               (car-safe (overlays-in (point-max) (point-max)))
               (make-overlay (point-max) (point-max) nil t t))))
      (move-overlay ov (point-max) (point-max))
      (overlay-put ov 'after-string text))))

(defun sr/set-minibuffer-prefix (minibuffer text)
  (with-current-buffer minibuffer
    (let ((inhibit-read-only t))
      (put-text-property (point-min) (minibuffer-prompt-end) 'display text))))

(defun sr/vr-regex (regexp mini)
  (let ((subexp-depth 0)
        (string (buffer-string)))
    (save-match-data
      (let ((pos 0))
        (cl-loop
         while (and (string-match regexp string pos)
                    (< pos (length string)))
         do
         (when (= (match-beginning subexp-depth)
                  (match-end subexp-depth))
           (cl-return nil))
         (let ((beg (match-beginning subexp-depth))
               (end (match-end subexp-depth)))
           (sr/add-overlay (1+ beg) (1+ end) 'face 'hl-line)
           (setq pos (match-end 0))))
        (sr/set-minibuffer-prefix mini "s/")
        (sr/set-minibuffer-suffix mini "/g")))))

(provide 'simple-regex-core)
