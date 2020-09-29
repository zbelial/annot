;;; counsel-annot.el --- counsel interface to list all annotations.

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
        (s-replace "\n" " " (buffer-substring-no-properties begin end))
      (buffer-substring-no-properties begin end))))

(defun counsel-annot--candidate (ov)
  (let* ((begin (counsel-annot--overlay-begin ov))
         (end (counsel-annot--overlay-end ov))
         (type (overlay-get ov :type))
         (text (counsel-annot--overlay-text type begin end ov))
         (linum (line-number-at-pos begin))
         )
    (list linum type text begin end)
    ))

(defun counsel-annot< (a1 a2)
  (if (= (car a1) (car a2))
      (< (nth 3 a1) (nth 3 a2))
    (< (car a1) (car a2))
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
              (let ((type (nth 1 candidate)))
                (if (eq type 'text)
                    (format " %-4s %s  : %s" (propertize (number-to-string (nth 0 candidate)) 'face compilation-line-face) (propertize "<T>" 'face compilation-info-face) (annot-trim (nth 2 candidate)))
                  (format " %-4s %s  : %s" (propertize (number-to-string (nth 0 candidate)) 'face compilation-line-face) (propertize "<H>" 'face compilation-info-face) (nth 2 candidate)))))
          annots)
  )

(defun counsel-annot-collector ()
  (let ((annots (counsel-annot-candidate-transformer (counsel-annot--make-annotations))))
    annots
    ))

(defun counsel-annot-annotations ()
  (interactive)
  (let ((annots (counsel-annot-collector)))
    (ivy-read "Annotations: "
              annots
              :dynamic-collection nil
              :action #'(lambda (candidate)
                          (setq line (nth 0 (split-string candidate ":")))
                          (goto-line (string-to-number line))
                          )
              ))
  )

(provide 'counsel-annot)
;;; annot.el ends here
