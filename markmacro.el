;;; markmacro.el --- Keyboard macro for marked objects   -*- lexical-binding: t; -*-

;; Filename: markmacro.el
;; Description: Keyboard macro for marked objects
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2022, Andy Stewart, all rights reserved.
;; Created: 2022-11-24 13:57:16
;; Version: 0.1
;; Last-Updated: 2022-11-24 13:57:16
;;           By: Andy Stewart
;; URL: https://www.github.org/manateelazycat/markmacro
;; Keywords:
;; Compatibility: GNU Emacs 28.2
;;
;; Features that might be required by this library:
;;
;;
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Keyboard macro for marked objects
;;

;;; Installation:
;;
;; Put markmacro.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'markmacro)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET markmacro RET
;;

;;; Change log:
;;
;; 2022/11/24
;;      * First released.
;;

;;; Acknowledgements:
;;
;;
;;

;;; TODO
;;
;;
;;

;;; Require


;;; Code:

(defgroup markmacro nil
  "Keyboard macro for marked objects."
  :group 'markmacro)

(defvar-local markmacro-overlays '())
(defvar-local markmacro-start-overlay nil)

(defvar-local markmacro-rect-start-point nil)
(defvar-local markmacro-rect-overlays '())

(defface markmacro-mark-face
  '((t (:foreground "White" :background "#007aff" :bold t)))
  "Face for marked regions."
  :group 'markmacro)

(defface markmacro-mark-rect-face
  '((t (:foreground "White" :background "coral" :bold t)))
  "Face for marked regions."
  :group 'markmacro)

(cl-defmacro with-markmacro-rect (&rest body)
  `(progn
     (remove-hook 'post-command-hook #'markmacro-rect-monitor-post-command t)

     (save-excursion
       ,@body)

     (markmacro-rect-delete-overlays)))

(cl-defmacro with-markmacro-rect-mark (&rest body)
  `(progn
     (remove-hook 'post-command-hook #'markmacro-rect-monitor-post-command t)
     (markmacro-delete-overlays)

     ,@body

     (markmacro-rect-delete-overlays)

     (markmacro-select-last-overlay)))

(defun markmacro-mark-words ()
  (interactive)
  (let ((bound (if (region-active-p)
                   (cons (region-beginning) (region-end))
                 (bounds-of-thing-at-point 'symbol)))
        (mark-bounds '()))
    (when bound
      (when (region-active-p)
        (deactivate-mark))

      (let ((mark-bound-start (car bound))
            (mark-bound-end (cdr bound))
            current-bound)
        (save-excursion
          (goto-char mark-bound-start)
          (while (<= (point) mark-bound-end)
            (setq current-bound (bounds-of-thing-at-point 'word))
            (when current-bound
              (add-to-list 'mark-bounds current-bound t))
            (forward-word))
          ))

      (dolist (bound mark-bounds)
        (let* ((overlay (make-overlay (car bound) (cdr bound))))
          (overlay-put overlay 'face 'markmacro-mark-face)
          (add-to-list 'markmacro-overlays overlay t)))

      (markmacro-select-last-overlay))))

(defun markmacro-mark-lines ()
  (interactive)
  (let ((bound (if (region-active-p)
                   (cons (region-beginning) (region-end))
                 (cons (point-min) (point-max))))
        (mark-bounds '()))
    (when bound
      (when (region-active-p)
        (deactivate-mark))

      (let ((mark-bound-start (car bound))
            (mark-bound-end (cdr bound))
            current-bound)
        (save-excursion
          (goto-char mark-bound-start)
          (while (< (point) mark-bound-end)
            (unless (string-match-p "^[ ]*$" (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
              (setq current-bound (bounds-of-thing-at-point 'line))
              (when current-bound
                (add-to-list 'mark-bounds current-bound t)))

            (forward-line))
          ))

      (dolist (bound mark-bounds)
        (let* ((overlay (make-overlay (car bound) (cdr bound))))
          (overlay-put overlay 'face 'markmacro-mark-face)
          (add-to-list 'markmacro-overlays overlay t)))

      (markmacro-select-last-overlay))))

(defun markmacro-select-last-overlay ()
  (when (> (length markmacro-overlays) 0)
    (goto-char (overlay-start (nth (- (length markmacro-overlays) 1) markmacro-overlays)))
    (markmacro-kmacro-start)))

(defun markmacro-kmacro-start ()
  (setq-local markmacro-start-overlay
              (dolist (overlay markmacro-overlays)
                (when (and (>= (point) (overlay-start overlay))
                           (< (point) (overlay-end overlay)))
                  (cl-return overlay))))
  (kmacro-start-macro 0))

(defun markmacro-apply-all ()
  (interactive)
  (markmacro-apply t))

(defun markmacro-apply-all-except-first ()
  (interactive)
  (markmacro-apply nil))

(defun markmacro-apply (include-first)
  (end-kbd-macro)

  (save-excursion
    (dolist (overlay (if include-first markmacro-overlays (cdr markmacro-overlays)))
      (unless (equal overlay markmacro-start-overlay)
        (goto-char (overlay-start overlay))
        (call-last-kbd-macro)
        )))

  (markmacro-exit))

(defun markmacro-exit ()
  (interactive)
  (markmacro-delete-overlays))

(defun markmacro-delete-overlays ()
  (when markmacro-overlays
    (dolist (overlay markmacro-overlays)
      (delete-overlay overlay))
    (setq-local markmacro-overlays nil)))

(defun markmacro-rect-set ()
  (interactive)
  (setq-local markmacro-rect-start-point (point))
  (add-hook 'post-command-hook #'markmacro-rect-monitor-post-command nil t)
  (message "markmacro set rectangle start point."))

(defun markmacro-rect-delete ()
  (interactive)
  (with-markmacro-rect
   (dolist (overlay markmacro-rect-overlays)
     (delete-region (overlay-start overlay) (overlay-end overlay)))))

(defun markmacro-rect-replace (new-string)
  (interactive "sReplace with: ")
  (with-markmacro-rect
   (dolist (overlay markmacro-rect-overlays)
     (delete-region (overlay-start overlay) (overlay-end overlay))
     (goto-char (overlay-start overlay))
     (insert new-string))))

(defun markmacro-rect-insert (new-string)
  (interactive "sInsert with: ")
  (with-markmacro-rect
   (dolist (overlay markmacro-rect-overlays)
     (goto-char (overlay-start overlay))
     (insert new-string))))

(defun markmacro-rect-mark-columns ()
  (interactive)
  (with-markmacro-rect-mark
   (dolist (rect-overlay markmacro-rect-overlays)
     (let ((overlay (make-overlay (overlay-start rect-overlay) (overlay-end rect-overlay))))
       (overlay-put overlay 'face 'markmacro-mark-face)
       (add-to-list 'markmacro-overlays overlay t)))))

(defun markmacro-rect-mark-symbols ()
  (interactive)
  (with-markmacro-rect-mark
   (dolist (rect-overlay markmacro-rect-overlays)
     (let* ((overlay-bound (save-excursion
                             (goto-char (overlay-start rect-overlay))
                             (bounds-of-thing-at-point 'symbol)))
            (overlay (make-overlay (car overlay-bound) (cdr overlay-bound))))
       (overlay-put overlay 'face 'markmacro-mark-face)
       (add-to-list 'markmacro-overlays overlay t)))))

(defun markmacro-rect-delete-overlays ()
  (when markmacro-rect-overlays
    (dolist (overlay markmacro-rect-overlays)
      (delete-overlay overlay))
    (setq-local markmacro-rect-overlays nil)))

(defun markmacro-rect-monitor-post-command ()
  (when markmacro-rect-start-point
    (markmacro-rect-delete-overlays)

    (let* ((start-point-info (save-excursion
                               (goto-char markmacro-rect-start-point)
                               (cons (line-number-at-pos) (current-column))))
           (start-line (car start-point-info))
           (start-column (cdr start-point-info))
           (current-line (line-number-at-pos))
           (current-column (current-column))
           (rect-start-line (min start-line current-line))
           (rect-end-line (max start-line current-line))
           (rect-start-column (min start-column current-column))
           (rect-end-column (max start-column current-column)))
      (dotimes (i (1+ (- rect-end-line rect-start-line)))
        (let ((overlay (make-overlay (save-excursion
                                       (goto-line (+ rect-start-line i))
                                       (move-to-column rect-start-column)
                                       (point))
                                     (save-excursion
                                       (goto-line (+ rect-start-line i))
                                       (move-to-column rect-end-column)
                                       (point)))))
          (overlay-put overlay 'face 'markmacro-mark-rect-face)
          (add-to-list 'markmacro-rect-overlays overlay t))))))

(provide 'markmacro)

;;; markmacro.el ends here
