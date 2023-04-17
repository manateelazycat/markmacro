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

(require 'cl-macs)

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

(defcustom markmacro-secondary-region-mark-cursors-type 'symbol
  "The default type used by `markmacro-secondary-region-mark-cursors-type'.

See `thing-at-point' for more information."
  :type '(choice
          (const :tag "Sym" symbol)
          (const :tag "List" list)
          (const :tag "Sexp" sexp)
          (const :tag "Defun" defun)
          (const :tag "Word" word)
          (const :tag "Line" line)
          (const :tag "Num" number))
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

(cl-defmacro markmacro-mark-objects (mark-bound func)
  `(when ,mark-bound
     (dolist (bound (funcall ,func ,mark-bound))
       (let* ((overlay (make-overlay (car bound) (cdr bound))))
         (overlay-put overlay 'face 'markmacro-mark-face)
         (add-to-list 'markmacro-overlays overlay t)))

     ;; Deactivate mark after update `markmacro-overlays'
     (when (region-active-p)
       (deactivate-mark))

     (markmacro-select-last-overlay)))

(defun markmacro-mark-symbols ()
  (interactive)
  (markmacro-mark-objects
   (if (region-active-p)
       (cons (region-beginning) (region-end))
     (cons (point-min) (point-max)))
   (lambda (bound)
     (let ((mark-bound-start (car bound))
           (mark-bound-end (cdr bound))
           (symbol (thing-at-point 'symbol))
           (mark-bounds '())
           (found-symbol t)
           current-bound)
       (save-excursion
         (goto-char mark-bound-start)
         (while (and (<= (point) mark-bound-end)
                     found-symbol)
           (setq found-symbol (search-forward symbol nil t))
           (setq current-bound (cons (save-excursion
                                       (backward-char (length symbol))
                                       (point))
                                     (point)))
           (when current-bound
             (add-to-list 'mark-bounds current-bound t)))

         mark-bounds)))))

(defun markmacro-mark-words ()
  (interactive)
  (markmacro-mark-objects
   (if (region-active-p)
       (cons (region-beginning) (region-end))
     (bounds-of-thing-at-point 'symbol))
   (lambda (bound)
     (let ((mark-bound-start (car bound))
           (mark-bound-end (cdr bound))
           (last-point 0)
           (mark-bounds '())
           current-bound)
       (save-excursion
         (goto-char mark-bound-start)
         (while (and (<= (point) mark-bound-end)
                     (> (point) last-point))
           (setq current-bound (bounds-of-thing-at-point 'word))
           (when current-bound
             (add-to-list 'mark-bounds current-bound t))
           (setq last-point (point))
           (forward-word))

         mark-bounds)))))

(defun markmacro-mark-chars ()
  (interactive)
  (when-let* ((char (char-after))
              (current-char (char-to-string char)))
    (markmacro-mark-objects
     (cond ((region-active-p)
            (cons (region-beginning) (region-end)))
           ((overlay-start mouse-secondary-overlay)
            (cons (overlay-start mouse-secondary-overlay) (overlay-end mouse-secondary-overlay)))
           (t
            (bounds-of-thing-at-point 'string)))
     (lambda (bound)
       (let ((mark-bound-start (car bound))
             (mark-bound-end (cdr bound))
             (mark-bounds '())
             (last-point 0))
         (save-excursion
           (goto-char mark-bound-start)
           (while (and (<= (point) mark-bound-end)
                       (> (point) last-point))
             (when (string-equal (char-to-string (char-after)) current-char)
               (add-to-list 'mark-bounds (cons (point) (1+ (point)))))
             (setq last-point (point))
             (forward-char))

           mark-bounds))))))

(defun markmacro-mark-lines ()
  (interactive)
  (markmacro-mark-objects
   (if (region-active-p)
       (cons (region-beginning) (region-end))
     (cons (point-min) (point-max)))
   (lambda (bound)
     (let ((mark-bound-start (car bound))
           (mark-bound-end (cdr bound))
           (mark-bounds '())
           current-bound)
       (save-excursion
         (goto-char mark-bound-start)
         (while (< (point) mark-bound-end)
           (unless (string-match-p "^[ ]*$" (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
             (setq current-bound (bounds-of-thing-at-point 'line))
             (when current-bound
               (add-to-list 'mark-bounds current-bound t)))

           (forward-line))

         mark-bounds
         )))))

(defun markmacro-find-parent-node-match (node-types)
  (treesit-parent-until
   (treesit-node-at (point))
   (lambda (parent)
     (member (treesit-node-type parent) node-types))))

(defun markmacro-mark-parameters ()
  (interactive)
  (when (require 'treesit nil t)
    (let* ((argument-lis-node (markmacro-find-parent-node-match '("argument_list" "arguments" "tuple" "tuple_pattern" "pair" "dictionary" "list")))
           (function-node (markmacro-find-parent-node-match '("call_expression" "declaration" "function_definition" "identifier")))
           (import-node (treesit-node-on (line-beginning-position) (line-end-position)))
           param-nodes)
      (cond
       ;; Mark argument list.
       (argument-lis-node
        (setq param-nodes (treesit-filter-child
                           argument-lis-node
                           (lambda (c)
                             (not (member (treesit-node-type c) '("(" ")")))))))
       ;; Mark parameters in function.
       (function-node
        (when-let (parameters-node (treesit-filter-child
                                    function-node
                                    (lambda (c)
                                      (member (treesit-node-type c) '("parameters")))))
          (setq param-nodes (treesit-filter-child
                             (nth 0 parameters-node)
                             (lambda (c)
                               (member (treesit-node-type c) '("identifier")))))))
       ;; Mark parameters in import code.
       (import-node
        (when (string-equal (treesit-node-type import-node) "import_from_statement")
          (setq param-nodes (cdr (treesit-filter-child
                                  import-node
                                  (lambda (c)
                                    (member (treesit-node-type c) '("dotted_name")))))))))

      (dolist (node param-nodes)
        (let* ((overlay (make-overlay (treesit-node-start node) (treesit-node-end node))))
          (overlay-put overlay 'face 'markmacro-mark-face)
          (add-to-list 'markmacro-overlays overlay t)))

      (markmacro-select-last-overlay))))

(defun markmacro-mark-imenus ()
  (interactive)
  (markmacro-mark-objects
   (if (region-active-p)
       (cons (region-beginning) (region-end))
     (cons (point-min) (point-max)))
   (lambda (bound)
     (let* ((mark-bound-start (car bound))
            (mark-bound-end (cdr bound))
            (candidates (markmacro-imenu-get-candidates))
            (mark-bounds '())
            current-bound)
       (save-excursion
         (goto-char mark-bound-start)
         (while (< (point) mark-bound-end)
           (dolist (candidate candidates)
             (when (= (point) (cadr candidate))
               (when (search-forward (car candidate) (point-at-eol) t)
                 (setq current-bound (cons (save-excursion
                                             (backward-char (length (car candidate)))
                                             (point))
                                           (point)))
                 (add-to-list 'mark-bounds current-bound t))))

           (forward-line))

         mark-bounds
         )))))

(defun markmacro-imenu-get-candidates ()
  (mapcar (lambda (info) (list (car info) (marker-position (cdr info))))
          (let* ((index (ignore-errors (imenu--make-index-alist t))))
            (when index
              (markmacro-imenu-build-candidates
               (delete (assoc "*Rescan*" index) index))))))

(defun markmacro-imenu-build-candidates (alist)
  (cl-remove-if
   (lambda (c)
     (or (string-equal (car c) "Types")
         (string-equal (car c) "Variables")
         ))
   (cl-loop for elm in alist
            nconc (cond
                   ((imenu--subalist-p elm)
                    (markmacro-imenu-build-candidates
                     (cl-loop for (e . v) in (cdr elm) collect
                              (cons
                               e
                               (if (integerp v) (copy-marker v) v)))))
                   ((listp (cdr elm))
                    (and elm (list elm)))
                   (t
                    (and (cdr elm)
                         (setcdr elm (pcase (cdr elm)
                                       ((and ov (pred overlayp))
                                        (copy-overlay ov))
                                       ((and mk (or (pred markerp)
                                                    (pred integerp)))
                                        (copy-marker mk))))
                         (list elm)))))))

(defun markmacro-secondary-region-set ()
  "Create secondary selection or a marker if no region available."
  (interactive)
  (cond ((region-active-p)
         (secondary-selection-from-region)
         (advice-add 'keyboard-quit :before #'markmacro-exit))
        (t
         (delete-overlay mouse-secondary-overlay)
         (move-overlay mouse-secondary-overlay (point-at-bol) (point-at-eol))
         (advice-add 'keyboard-quit :before #'markmacro-exit)))
  (deactivate-mark t))

(defun markmacro-secondary-region-mark-cursors (arg)
  "Mark all in the region that is the same as the word under the cursor.

Usage:
1. Select a region and use `markmacro-secondary-region-set' mark secondary region, mark all buffer if not call `markmacro-secondary-region-set'
2. Jump to an entity in the region, region call `markmacro-secondary-region-mark-cursors' or call `markmacro-secondary-region-mark-cursors' directly
3. Type something.
4. Call `markmacro-apply-all' apply kmacro to all mark entities."
  (interactive "P")
  (when-let
      ((sec-region-start (or (overlay-start mouse-secondary-overlay) (point-min)))
       (sec-region-end (or (overlay-end mouse-secondary-overlay) (point-max)))
       (target
        (cond (arg
               (thing-at-point 'char t))
              ((region-active-p)
               (buffer-substring-no-properties (region-beginning) (region-end)))
              (t
               (thing-at-point markmacro-secondary-region-mark-cursors-type t))))
       (mark-bounds '(t))
       (current-point (point))
       (temp-bound 'bound))
    (save-excursion
      (goto-char sec-region-start)
      (pop mark-bounds)
      (while (search-forward target sec-region-end t)
        (let ((mstart (match-beginning 0))
              (mend (match-end 0)))
          (if (and (<= mstart current-point)
                   (>= mend current-point))
              (setq temp-bound (cons mstart mend))
            (push (cons mstart mend) mark-bounds)))))
    (add-to-list 'mark-bounds temp-bound t)

    (dolist (bound mark-bounds)
      (let* ((overlay (make-overlay (car bound) (cdr bound))))
        (overlay-put overlay 'face 'markmacro-mark-face)
        (add-to-list 'markmacro-overlays overlay t)))

    (delete-overlay mouse-secondary-overlay)
    (markmacro-select-last-overlay)))

(defun markmacro-select-first-overlay ()
  (if (> (length markmacro-overlays) 0)
      (progn
        (goto-char (overlay-start (nth 0 markmacro-overlays)))
        (markmacro-kmacro-start))
    (markmacro-exit)
    (message "Nothing to selected, exit markmarco.")))

(defun markmacro-select-last-overlay ()
  (if (> (length markmacro-overlays) 0)
      (progn
        (goto-char (overlay-start (nth (- (length markmacro-overlays) 1) markmacro-overlays)))
        (markmacro-kmacro-start))
    (markmacro-exit)
    (message "Nothing to selected, exit markmarco.")))

(defun markmacro-kmacro-start ()
  (setq-local markmacro-start-overlay
              (cl-dolist (overlay markmacro-overlays)
                (when (or (and (>= (point) (overlay-start overlay))
                               (< (point) (overlay-end overlay)))
                          (= (point) (overlay-start overlay) (overlay-end overlay)))
                  (cl-return overlay))))
  (advice-add 'keyboard-quit :before #'markmacro-exit)
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
  (markmacro-delete-overlays)

  (delete-overlay mouse-secondary-overlay)
  (setq mouse-secondary-start (make-marker))
  (move-marker mouse-secondary-start (point))

  (deactivate-mark t))

(defun markmacro-delete-overlays ()
  (when markmacro-overlays
    (dolist (overlay markmacro-overlays)
      (delete-overlay overlay))
    (setq-local markmacro-overlays nil)
    (advice-remove 'keyboard-quit #'markmacro-exit)))

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
            overlay)
       (when overlay-bound
         (setq overlay (make-overlay (car overlay-bound) (cdr overlay-bound)))
         (overlay-put overlay 'face 'markmacro-mark-face)
         (add-to-list 'markmacro-overlays overlay t))))))

(defun markmacro-rect-delete-overlays ()
  (when markmacro-rect-overlays
    (dolist (overlay markmacro-rect-overlays)
      (delete-overlay overlay))
    (setq-local markmacro-rect-overlays nil)))

(defun markmacro-rect-monitor-post-command ()
  (if (eq this-command 'keyboard-quit)
      (progn
        (markmacro-rect-delete-overlays)
        (remove-hook 'post-command-hook #'markmacro-rect-monitor-post-command t))
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
            (add-to-list 'markmacro-rect-overlays overlay t)))))))

(provide 'markmacro)

;;; markmacro.el ends here
