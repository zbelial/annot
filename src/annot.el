;;; annot.el --- a global annotation manager for GNU Emacs

;; Copyright (C) 2010 tkykhs

;; Author:     tkykhs <tkykhs@gmail.com>
;; Maintainer: tkykhs
;; Created:    March 28, 2010
;; Keywords:   annotations

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
;; `annot.el' is a general and scalable annotation manager that works on GNU
;; Emacs.  It lets you add/remove/edit annotations on any file (or buffer) and
;; manages them for later use. All annotations are stored separately for each
;; annotated file and get reproduced when the file is opened again. You can even
;; store annotations on non-editable files.  Because annot keeps track of md5
;; checksums of annotated files, annotations won't disappear even when file
;; names are changed.
;; 
;; Installation:
;;
;; Insert the following line to your .emacs:
;;
;;  (require 'annot)
;; 
;; Keybindings:
;;
;; * [C-x a]    -  add new annotation or edit existing annotation.
;;                 You can also use [C-x C-a]. (annot-edit/add)
;; * [C-x r]    -  remove annotation at point. (annot-remove)
;;
;; User Commands:
;; 
;; * `annot-edit/add' - either edit the annotation at point, if there is,
;;                      or else add a new one.
;; * `annot-remove'   - remove the annotation at point.
;; * `annot-add'      - add an annotation at point.
;; * `annot-edit'     - edit the annotation at point.
;;
;; * it saves annotatioins only if current buffer is a file-buffer.  This
;; prevents, among other thing, indirect buffers from messing up with its
;; original.


;;; Todo:

;; * File symlink support for later reference.
;; * Add option to do strict checking = :prev support.
;; * Sync annotations with indirect buffers.
;; * Primitive undo management.

;;; Code:

(defgroup annot nil
  "Annotation manager in Emacs."
  :prefix "annot-"
  :group 'tools)

(defcustom annot-directory "~/.annot"
  "Directory under which all annotations are, and will be, saved."
  :type  'string
  :group 'annot)

(defcustom annot-text-decoration-function 'annot-decorate-text
  "Function to decorate an annotation text."
  :type  'symbol
  :group 'annot)

(defcustom annot-md5-max-chars 300000
  "Maximum number of characters to sample for getting the md5 \
checksum from the top of the buffer.
Keep in mind though that there is a speed-reliability tradeoff here."
  :type '(choice (const   :tag "No limit" nil)
                 (integer :tag "Number of chars"))
  :group 'annot)

(defface annot-text-face
  '((((class color) (background light)) (:foreground "red" :background "yellow"))
    (((class color) (background dark)) (:foreground "red" :background "white")))
  "Face for annotation text."
  :group 'annot)


(defconst annot-subsequence-length 24
  "Number of characters to be stored before and after \
annotation's position in a file buffer.")

(defconst annot-contents-dirname "contents" 
  "Name of the annotation directory.")

(defvar annot-buffer-overlays nil
  "List of overlays in the buffer.")
(make-variable-buffer-local 'annot-buffer-overlays)

(defvar annot-buffer-plist nil
  "Plist containing annotation information in the buffer.
Unfortunately `make-variable-buffer-local' does not make the
symbol plist buffer-local; so this variable has to exist
separately.")
(make-variable-buffer-local 'annot-buffer-plist)


;;; User commands.

(defun annot-add ()
  "Add an annotation on the current point."
  (interactive)
  (let ((text (read-string "Annotation: ")) ov)
    (unless (zerop (length (annot-trim text)))
      (setq ov (annot-create-overlay (point) text))
      (push ov annot-buffer-overlays)
      (annot-save-annotations))))


(defun annot-edit ()
  "Edit a nearby annotation on the current line."
  (interactive)
  (let ((p (point))
        (ov (annot-get-annotation-at-point)))
    (if (null ov)
        (message "No annotation to edit at point.")
      (let ((text (read-string "Annotation: "
                               (annot-trim
                                (substring-no-properties
                                 (overlay-get ov 'before-string))))))
        (if (zerop (length (annot-trim text)))
            (annot-remove ov)
          (overlay-put ov 'before-string
                       (funcall annot-text-decoration-function text))
          (annot-save-annotations))))))


(defun annot-edit/add ()
  "Either edit the annotation at point, if there is, or else add a new one."
  (interactive)
  (if (annot-get-annotation-at-point)
      (annot-edit)
    (annot-add)))


(defun annot-remove (&optional ov)
  "Remove a nearby annotation on the current line."
  (interactive)
  (let ((ov (or ov (annot-get-annotation-at-point))))
    (when ov
      (setq annot-buffer-overlays (delq ov annot-buffer-overlays))
      (delete-overlay ov)
      (annot-save-annotations)
      (when (called-interactively-p 'any)
        (message "Annotation removed.")))))


;;; Internal functions.

(defsubst annot-trim (s)
  "Trim non-graphic chars from both ends of string s." 
  (replace-regexp-in-string
   "\\`[^[:graph:]]+\\|[^[:graph:]]+\\'"  "" s))


(defsubst annot-contents-directory ()
  (format "%s/%s" annot-directory annot-contents-dirname))


(defsubst annot-get-annot-filename (md5)
  "Return the full path of the annotation filename."
  (expand-file-name (format "%s/%s"
                            (annot-contents-directory) md5)))


(defsubst annot-md5 (&optional buffer)
  "Get md5 of the buffer content with max chars `annot-md5-max-chars'.
If `annot-md5-max-chars' is nil, no limit is imposed." 
  (let ((buffer (or buffer (current-buffer))))
    (md5 buffer nil
         (if (null annot-md5-max-chars)
             nil
           (min annot-md5-max-chars (point-max)))
         nil t)))


(defun annot-decorate-text (text)
  "Decorate an annotation text."
  (let* ((n (length text))
         (last-newline-p (string= (substring text (max 0 (1- n)) n) "\n"))
         (text (annot-trim text)))
    (format "%s%s"
            (if (bolp) "" " ")
            (propertize (format " %s%s" text (if last-newline-p "\n" " "))
                        'face 'annot-text-face))))


(defun annot-get-annotation-at-point ()
  "Get annotation \(equiv. overlay) at point, or if none found, 1+ point." 
  (let ((p (point)))
    (car (or (overlays-in p p)
             (overlays-in (min (point-max) (1+ p))
                          (min (point-max) (1+ p)))))))


(defun annot-create-overlay (pos text)
  "Create an overlay for the annotation text."
  ;; (assert (not (zerop (length (annot-trim text)))))
  (let ((ov (make-overlay pos pos nil t nil)))
    (overlay-put ov 'before-string    ;; after-string
                 (funcall annot-text-decoration-function text))
    (overlay-put ov :pos pos)    ;; it will be saved later anyways...
    (overlay-put ov :prev
                 (buffer-substring-no-properties
                  (max (point-min) (- (point) annot-subsequence-length)) (point)))
    (overlay-put ov :next
                 (buffer-substring-no-properties
                  (point) (min (point-max) (+ (point) annot-subsequence-length))))
    ov))


(defun annot-format-overlays (md5 filename bufname annot-filename modtime)
  "Generate a string containing all information necessary to reproduce annotations."
  (let ((ov-plists-s
         (let ((print-escape-newlines t))    ;; newline is represented as "\n"
           (mapconcat (lambda (ov)
                        (format "%S" (overlay-properties ov)))
                      annot-buffer-overlays "\n"))))
    (format ";;; -*- mode: emacs-lisp -*-
\(annot-recover-annotations '\(
:md5 %S
:filename %S
:bufname %S
:annot-filename %S
:modtime %S
:annotations \(
%s
)))" md5 filename bufname annot-filename modtime ov-plists-s)))


(defun annot-save-annotations ()
  "Save all annotations in the buffer, updating all information.
It updates `annot-buffer-plist' information as well.
If the new filename \(or equivalently md5) is different from
previous filename, return delete the previous file."
  (setq annot-buffer-plist
        (plist-put annot-buffer-plist :modtime (float-time)))
  
  (cond
   ;; In case no annotations are left, delete the
   ;; associated annot file.
   ((null annot-buffer-overlays)
    (let ((md5 (plist-get annot-buffer-plist :md5))
          old-annot-filename)
      (when md5
        (setq old-annot-filename (annot-get-annot-filename md5))
        (if (file-readable-p old-annot-filename)
            (delete-file old-annot-filename)))))
   
   (t
    ;; Create annot directory if it does not exist.
    (unless (file-exists-p (annot-contents-directory))
      (make-directory (annot-contents-directory) t))
    
    (let* ((buffer (current-buffer))
           (prev-md5 (plist-get annot-buffer-plist :md5))
           (md5 (annot-md5 buffer))
           (filename (buffer-file-name))
           (annot-filename (annot-get-annot-filename md5))
           (modtime (or (plist-get annot-buffer-plist :modtime) (float-time)))
           (bufname (buffer-name)) s)
      ;; Update :pos and :next of each overlay
      (dolist (ov annot-buffer-overlays)
        (let ((p (overlay-start ov)))
          (overlay-put ov :pos p)
          (overlay-put ov :prev
                       (buffer-substring-no-properties
                        (max (point-min) (- p annot-subsequence-length)) p))
          (overlay-put ov :next
                       (buffer-substring-no-properties
                        p (min (point-max) (+ p annot-subsequence-length))))))
      
      ;; Get the S-expression and save the annotations if it is a file-buffer.
      ;; It is possible to load annotations for non-file-buffer, but it is
      ;; not supported yet just to play safe.
      (when filename
        (setq s (annot-format-overlays md5 filename bufname annot-filename modtime))
        (condition-case error
            (with-temp-file annot-filename
              (erase-buffer)
              (insert s))
          (error
           (warn "annot-save-annotations: %s" (error-message-string error)))))
      
      ;; Update `annot-buffer-plist'
      (dolist (e '(md5 filename bufname annot-filename modtime))
        (setq annot-buffer-plist
              (plist-put annot-buffer-plist
                         (intern (format ":%S" e)) (symbol-value e))))
      
      ;; If md5 doesn't matche the previous one,
      ;; delete the old annotation file.
      (when (and prev-md5 (not (string= prev-md5 md5)))
        (let ((old-annot-filename (annot-get-annot-filename prev-md5)))
          (if (file-readable-p old-annot-filename)
              (delete-file old-annot-filename))))
      annot-filename))))


(defun annot-recover-annotations (annotations-info)
  "Recover annotations.
Only annotation files use this function internally."
  (let ((annotations (plist-get annotations-info :annotations))
        (modtime     (plist-get annotations-info :modtime))
        (var-modtime (plist-get annot-buffer-plist :modtime)))
    (when (or (null var-modtime)
              (not (< modtime var-modtime))
              ;; If annot-buffer-overlays has updated modtime, ask.
              (y-or-n-p (concat "Modification time for stored annotations"
                                " appears be older. Load them anyways? ")))
      ;; If by any chance `annot-buffer-overlays' contains some overlays,
      ;; delete them all.
      (when annot-buffer-overlays
        (dolist (ov annot-buffer-overlays)
          (delete-overlay ov))
        (setq annot-buffer-overlays nil))
      ;; Mmmkay, let's reproduce annotations.
      (dolist (ov-plist annotations)
        (let ((pos (plist-get ov-plist :pos))
              ;; (next-string (plist-get ov-plist :next))
              (prev-string (plist-get ov-plist :prev)))
          (when (string= prev-string
                         (buffer-substring-no-properties
                          (max (point-min) (- pos (length prev-string))) pos))
              ;; (string= next-string
              ;;          (buffer-substring-no-properties
              ;;           pos (min (point-max) (+ pos (length next-string)))))
            (push (annot-recover-overlay ov-plist) annot-buffer-overlays))))
      (setq annot-buffer-plist
            `(:md5            ,(plist-get annotations-info :md5)
              :filename       ,(plist-get annotations-info :filename)
              :bufname        ,(plist-get annotations-info :bufname)
              :annot-filename ,(plist-get annotations-info :annot-filename)
              :modtime        ,modtime)))))


(defun annot-recover-overlay (ov-plist)
  "Recover an overlay."
  (let* ((pos (plist-get ov-plist :pos))
         (ov  (make-overlay pos pos nil t nil)))
    (dotimes (i (length ov-plist))
      (when (eq (logand i 1) 0)
        (overlay-put ov (nth i ov-plist) (nth (1+ i) ov-plist))))
    ov))


(defun annot-load-annotations ()
  "Load the annotation file corresponding to the current buffer.
If current `annot-buffer-overlays' looks newer in a rare occasion,
it asks whether to load the file or not."
  (interactive)
  (let ((filename (annot-get-annot-filename (annot-md5 (current-buffer)))))
	(cond
	 ((file-readable-p filename)
      (load-file filename))
	 (t
      (when (called-interactively-p 'any)
        (message "Either the file \"%s\" does not exist or is not readable."
                 filename))))))


(defun annot-delete-annotations-region (start end)
  "Delete all annotations in region."
  (interactive "r")
  (dolist (ov annot-buffer-overlays)
    (let ((p (overlay-start ov)))
      (if (and (>= end p) (>= p start))
          (annot-remove ov)))))


;;; Keybindings.

(define-key ctl-x-map "a"    'annot-edit/add)
(define-key ctl-x-map "\C-a" 'annot-edit/add)
(define-key ctl-x-map "r"    'annot-remove)


;;; Hooks and Advices.

(defvar annot-buffer-modified-p nil)
(make-variable-buffer-local 'annot-buffer-modified-p)

(defun annot-before-save-hook ()
  (setq annot-buffer-modified-p (buffer-modified-p)))
(add-hook 'before-save-hook 'annot-before-save-hook)

(defun annot-after-save-hook ()
  (when annot-buffer-modified-p
    (annot-save-annotations))
  (setq annot-buffer-modified-p nil))
(add-hook 'after-save-hook 'annot-after-save-hook)
(add-hook 'find-file-hook 'annot-load-annotations)

(defadvice delete-region (before annot-delete-region activate)
  "Enable deletion of annotations within the specified region."
  (annot-delete-annotations-region start end))


(provide 'annot)
;;; annot.el ends here