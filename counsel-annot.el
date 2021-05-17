;;; counsel-annot.el --- counsel interface to list all annotations. -*- lexical-binding: t; -*-

;; Copyright (C) 2011-2020 zbelial

;; Author:     zbelial
;; Maintainer: zbelial
;; Created on: February 05, 2020
;; Keywords:   counsel, annotation, tools

;; This file is not (yet) part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Description:
;;
;; Requirement:
;; 
;; GNU Emacs 23 or higher.
;; 
;; Installation:
;;
;; Insert the following line to your .emacs:
;;
;;  (require 'counsel-annot)
;; 
;; Keybindings:
;;
;;
;; TODO:
;; 1. 选择某个注解时，buffer中自动展示对应行
;; 2. 错误处理
;;
;;
;; User Commands:
;; 
;; * `counsel-annot-annotations' - List all annotations
;; * `counsel-annot-text-annotations'  - List text annotations
;; * `counsel-annot-highlight-annotations' - List highlight annotations


;;; Code:

(require 'annot)
(require 'cl-lib)
(require 'compile) ;; compilation-info-face, compilation-line-face
(require 'ivy)
(require 's)
(require 'rx)

(defgroup counsel-annot nil
  "Annotation manager in Emacs."
  :prefix "counsel-annot-"
  :group 'annot)

(defcustom counsel-annot-join-lines nil
  "If enabled, join lines of multiline highlight annotation."
  :type 'boolean
  :group 'annot)


(defun counsel-annot-toggle-join-lines ()
  (interactive)
  (setq counsel-annot-join-lines (not counsel-annot-join-lines)))

(defun counsel-annot--overlay-begin (ov)
  (or (overlay-get ov :beg)
      (overlay-get ov :pos)))

(defun counsel-annot--overlay-end (ov)
  (or (overlay-get ov :end)
      (overlay-get ov :pos)))

(defun counsel-annot--overlay-text (type begin end ov)
  (if (equal type 'text)
      (substring-no-properties
       (or (overlay-get ov 'before-string) ""))
    (if counsel-annot-join-lines
        (replace-regexp-in-string (rx (zero-or-more " ") (one-or-more "\n") (zero-or-more " ")) " " (buffer-substring-no-properties begin end))
      (buffer-substring-no-properties begin end))))

(defun counsel-annot--candidate (ov)
  (let* ((begin (counsel-annot--overlay-begin ov))
         (end (counsel-annot--overlay-end ov))
         (type (overlay-get ov :type))
         (text (counsel-annot--overlay-text type begin end ov))
         (linum (line-number-at-pos begin))
         )
    (list :linum linum :type type :text text :begin begin :end end)
    ))

(defun counsel-annot< (a1 a2)
  (if (= (plist-get a1 :linum) (plist-get a2 :linum))
      (< (plist-get a1 :begin) (plist-get a2 :begin))
    (< (plist-get a1 :linum) (plist-get a2 :linum))
  ))

(defun counsel-annot--make-annotations()
  (let* ((point-min (point-min))
         (point-max (point-max))
         (annotations (annot-overlays-in point-min point-max))
         candidates)
    (when annotations
      (dolist (annot annotations)
        (add-to-list 'candidates (counsel-annot--candidate annot)))
    (cl-sort candidates 'counsel-annot<)
    )))

(defun counsel-annot-candidate-transformer (annots)
  (mapcar #'(lambda (candidate)
              (let ((type (plist-get candidate :type)))
                (if (eq type 'text)
                    (cons (format " %-4s %s  : %s" (propertize (number-to-string (plist-get candidate :linum)) 'face compilation-line-face) (propertize "<T>" 'face compilation-info-face) (annot-trim (plist-get candidate :text))) candidate)
                  (cons (format " %-4s %s  : %s" (propertize (number-to-string (plist-get candidate :linum)) 'face compilation-line-face) (propertize "<H>" 'face compilation-info-face) (plist-get candidate :text)) candidate))))
          annots)
  )

(defun counsel-annot-collector ()
  (let ((annots (counsel-annot-candidate-transformer (counsel-annot--make-annotations))))
    annots
    ))

(defvar-local counsel-annot--ostart nil)
(defvar-local counsel-annot--opoint nil)
(defun counsel-annot-annotations-update-fn ()
  (with-ivy-window
    (let ((current (ivy-state-current ivy-last))
          item
          marker
          linum
          )
      ;; (message "current %S" current)
      (when (not (string-empty-p current))
        (setq item (nth (get-text-property 0 'idx current) (ivy-state-collection ivy-last)))
        ;; (message "item %S" item)
        (setq linum (plist-get (cdr item) :linum))
        ;; (message "linum %S" linum)
        (goto-line linum)
        (recenter)
        (let ((pulse-delay 0.5))
          (pulse-momentary-highlight-one-line (point)))
        )
      ))
  )

(defun counsel-annot-annotations ()
  (interactive)
  (setq counsel-annot--ostart (window-start))
  (setq counsel-annot--opoint (point))
  
  (let ((annots (counsel-annot-collector))
        (preselect 0)
        (linum (line-number-at-pos))
        res)
    (dolist (a annots)
      (when (< (plist-get (cdr a) :linum) linum)
        (setq preselect (1+ preselect))))
    (unwind-protect
        (progn
          (setq res (ivy-read "Annotations: "
                              annots
                              :preselect preselect
                              :dynamic-collection nil
                              :update-fn #'counsel-annot-annotations-update-fn
                              :action #'(lambda (candidate)
                                          (let ((annot (cdr candidate)))
                                            (goto-line (plist-get annot :linum))))
                              ))
          )
      (unless res
        (when (and counsel-annot--ostart counsel-annot--opoint)
          (with-ivy-window
            (goto-char counsel-annot--opoint)
            (set-window-start (selected-window) counsel-annot--ostart)
            )
          ))
      )
    )
  )

(defun counsel-annot--invalid-overlay-text (type begin end ov)
  (if (equal type 'text)
      (substring-no-properties
       (or (plist-get ov 'before-string) ""))
    (if counsel-annot-join-lines
        (replace-regexp-in-string (rx (zero-or-more " ") (one-or-more "\n") (zero-or-more " ")) " " (buffer-substring-no-properties begin end))
      (buffer-substring-no-properties begin end))))

(defun counsel-annot--invalid-candidate (ov)
  (let* ((begin (annot-get-beg ov))
         (end (annot-get-end ov))
         (type (plist-get ov :type))
         (text (counsel-annot--invalid-overlay-text type begin end ov))
         (linum (line-number-at-pos begin))
         )
    (list :linum linum :type type :text text :begin begin :end end)
    ))


(defun counsel-annot--make-invalid-annotations()
  (when annot-buffer-invalid-annotations
    (dolist (annot annot-buffer-invalid-annotations)
      (add-to-list 'candidates (counsel-annot--candidate annot)))
    (cl-sort candidates 'counsel-annot<)
    ))


(defun counsel-annot-invalid-collector ()
  (let ((annots (counsel-annot-candidate-transformer (counsel-annot--make-invalid-annotations))))
    annots
    ))

(defun counsel-annot-invalid-annotations ()
  (interactive)
  (let ((annots (counsel-annot-invalid-collector))
        )
    (ivy-read "Annotations: "
              annots
              :dynamic-collection nil
              :action #'(lambda (candidate)
                          (let ((annot (cdr candidate)))
                            (goto-line (plist-get annot :linum))))
              ))
  )


(provide 'counsel-annot)
;;; annot.el ends here
