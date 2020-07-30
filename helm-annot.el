;;; helm-annot.el --- helm interface to list all annotations.

;; Copyright (C) 2011-2020 zbelial

;; Author:     zbelial
;; Maintainer: zbelial
;; Created on: February 05, 2020
;; Keywords:   helm, annotation, tools

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
;;  (require 'helm-annot)
;; 
;; Keybindings:
;;
;;
;; TODO:
;; 1. 通过多
;; 2. 选择某个注解时，buffer中自动展示对应行
;;
;;
;; User Commands:
;; 
;; * `helm-annot-annotations' - List all annotations
;; * `helm-annot-text-annotations'  - List text annotations
;; * `helm-annot-highlight-annotations' - List highlight annotations


;;; Code:

(require 'annot)
(require 'cl-lib)

(defgroup helm-annot nil
  "Annotation manager in Emacs."
  :prefix "helm-annot-"
  :group 'tools)

(defun helm-annot--overlay-begin (ov)
  (or (overlay-get ov :beg)
      (overlay-get ov :pos)))

(defun helm-annot--overlay-end (ov)
  (or (overlay-get ov :end)
      (overlay-get ov :pos)))

(defun helm-annot--overlay-text (type begin end ov)
  (if (equal type 'text)
      (substring-no-properties
       (or (overlay-get ov 'before-string) ""))
    (save-excursion
      (buffer-substring-no-properties begin end)
    )))

(defun helm-annot--candidate (ov)
  (let* ((begin (helm-annot--overlay-begin ov))
         (end (helm-annot--overlay-end ov))
         (type (overlay-get ov :type))
         (text (helm-annot--overlay-text type begin end ov))
         (linum (line-number-at-pos begin))
         )
    (list linum type text begin end)
    ))

(defvar helm-annot--annotations nil)

(defun helm-annot-init (&optional type)
  (if type 
      (setq helm-annot--annotations (delq nil (mapcar #'(lambda (candidate)
                                                          (when (equal type (nth 1 candidate))
                                                            candidate))
                                                      (helm-annot--make-annotations))))
    (setq helm-annot--annotations (helm-annot--make-annotations))
    ))

(defun helm-annot< (a1 a2)
  (if (= (car a1) (car a2))
      (< (nth 3 a1) (nth 3 a2))
    (< (car a1) (car a2))
  ))

(defun helm-annot--make-annotations()
  (let* ((point-min (point-min))
         (point-max (point-max))
         (annotations (annot-overlays-in point-min point-max))
         candidates)
    (when annotations
      (dolist (annot annotations)
        (add-to-list 'candidates (helm-annot--candidate annot)))
    (cl-sort candidates 'helm-annot<)
    )))

(defun helm-annot-candidate-transformer ()
  (mapcar #'(lambda (candidate)
              (let ((type (nth 1 candidate)))
                (if (eq type 'text)
                    (format " %-4d <T>     : %s" (nth 0 candidate) (annot-trim (nth 2 candidate)))
                  (format " %-4d <H>     : %s" (nth 0 candidate) (nth 2 candidate)))))
          helm-annot--annotations)
  )

(setq helm-annot-source-annotations
  `((name . "Annotations")
    (init . (lambda () (helm-annot-init)))
    ;; (multiline)
    (volatile)
    (fuzzy-match . nil)
    (candidates . helm-annot-candidate-transformer)
    (action . (lambda (candidate)
                (setq line (nth 0 (split-string candidate ":")))
                (goto-line (string-to-number line))
                ))
    ))

(setq helm-annot-source-text-annotations
  `((name . "Text Annotations")
    (init . (lambda () (helm-annot-init 'text)))
    (multiline)
    (volatile)
    (fuzzy-match . nil)
    (candidates . helm-annot-candidate-transformer)
    (action . (lambda (candidate)
                (setq line (nth 0 (split-string candidate ":")))
                (goto-line (string-to-number line))
                ))
    ))

(setq helm-annot-source-highlight-annotations
  `((name . "Highlight Annotations")
    (init . (lambda () (helm-annot-init 'highlight)))
    (multiline)
    (volatile)
    (fuzzy-match . nil)
    (candidates . helm-annot-candidate-transformer)
    (action . (lambda (candidate)
                (setq line (nth 0 (split-string candidate ":")))
                (goto-line (string-to-number line))
                ))
    ))

(defun helm-annot-annotations ()
  (interactive)
  (helm :sources '(helm-annot-source-annotations)
        :buffer "*helm annot*"))

(defun helm-annot-text-annotations ()
  (interactive)
  (helm :sources '(helm-annot-source-text-annotations)
        :buffer "*helm annot*"))

(defun helm-annot-hightlight-annotations ()
  (interactive)
  (helm :sources '(helm-annot-source-highlight-annotations)
        :buffer "*helm annot*"))

(provide 'helm-annot)
;;; annot.el ends here
