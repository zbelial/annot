;;; annot.el --- a global annotator/highlighter for GNU Emacs

;; Copyright (C) 2010-2016 tkykhs

;; Author:     tkykhs
;; Maintainer: tkykhs
;; Created on: March 28, 2010
;; Keywords:   convenience, frames, local, multimedia, terminals, tools

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
;; `annot.el' is a general (and scalable) annotation manager that works on GNU
;; Emacs.  It lets you add/edit/remove annotations, highlights, or even
;; annotated images on any file and manages them for you.  All annotations are
;; stored separately for each annotated file and get reproduced when the file is
;; opened again. You can even store annotations on non-editable files.  Because
;; annot keeps track of md5 checksums of annotated files, annotations won't
;; disappear even when file names are changed.
;; 
;; Requirement:
;; 
;; GNU Emacs 23 or higher.
;; 
;; Installation:
;;
;; Insert the following line to your .emacs:
;;
;;  (require 'annot)
;; 
;; Keybindings:
;;
;; * [C-x C-a] - Add a new annotation
;;               Highlight the current region if any (annot-edit/add)
;; * [C-x C-r] - Remove the annotation/highlight at point (annot-remove)
;; * [C-x C-i] - Insert a new image at point (annot-add-image)
;;
;; User Commands:
;; 
;; * `annot-edit/add'   - Add a new annotation or highlight if a region is
;;                        specified.  Edit an annotation if there's one already.
;; * `annot-remove'     - Remove the annotation/highlight at point.
;; * `annot-add-image'  - Insert an image at point.
;; * `annot-convert'    - Convert text within the active region into an
;;                        annot text annotation
;; * `annot-to-comment' - Convert a text annotation at point to a comment
;; * `annot-goto-next'  - Go to the next annot overlay
;; * `annot-goto-previous' - Go to the previous annot overlay

;; TODO:

;;; Code:

(require 's)

(defgroup annot nil
  "Annotation manager in Emacs."
  :prefix "annot-"
  :group 'tools)

(defcustom annot-default-directory "~/.annot"
  "Directory under which all annotations are, and will be, saved."
  :type  'string
  :group 'annot)

(defcustom annot-directory-alist nil
  "Alist of directory, in which files will be annotated, and annot directory names."
  :group 'annot
  :type '(repeat (cons string (cons (directory :tag "Directory be annotated")
                                    (directory :tag "Annot directory name")))))

(defcustom annot-text-decoration-function 'annot-decorate-text
  "Function to decorate an annotation text."
  :type  'symbol
  :group 'annot)

(defcustom annot-md5-max-chars 300000
  "Max number of characters to sample for getting the md5 checksum.
DO NOT MODIFY THIS VARIABLE WHEN YOU HAVE ANNOTATIONS YOU DO NOT
WANT TO LOSE. Depending on what you specify here, some md5's of
annotated files might also change \(for long files especially),
resulting in loss of annotations.  Sampling is from the top of
the buffer. Keep in mind that there is a speed-reliability
tradeoff here."
  :risky t
  :type '(choice (const   :tag "No limit" nil)
                 (integer :tag "Number of chars"))
  :group 'annot)

(defcustom annot-load-disabled-modes '(hexl-mode)
  "A list of major-modes for which file-buffer annot load should not take place."
  :type '(repeat (symbol :tag "Major mode"))
  :group 'annot)

(defcustom annot-enable-symlinking (eval-when-compile
                                     (member system-type '(gnu
                                                           gnu/linux
                                                           gnu/kfreebsd
                                                           darwin
                                                           cygwin
                                                           berkeley-unix
                                                           irix)))
  "Whether to enable symlink support.
Depending on your editing style, this makes annot more robust in
reproducing annotations."
  :type 'boolean
  :group 'annot)

(defcustom annot-enable-strict-integrity-checking nil
  "If enabled, check the forward string as well.
By default, annot only checks backward subsequence relative to
annotation's position."
  :type 'boolean
  :group 'annot)

(defcustom annot-image-directory "~/"
  "Default image directory for `annot-add-image'."
  :type 'string
  :group 'annot)

(defcustom annot-save-exclusion-regexp "\\(\\`/tmp/\\)"
  "Any \(annot-buffer-file-name) matching this expression is to
be excluded from annots being saved under `annot-default-directory'.
This is useful for cases when you are expected to annotate a file
buffer only once, if ever. By default, `/tmp/' directory is
excluded from annots being saved due to its temporalities in
modern operating systems."
  :type 'regexp
  :group 'annot)

(defcustom annot-execute-last-sexp-p nil
  "If enabled, execute the sexp specified in the last annotation.
It has to be of the form: (<existing-function> ...)."
  :type 'boolean
  :group 'annot)

(defcustom annot-broader-removal-p nil
  "If enabled, `annot-remove' removes a non-annot overlay that
happens to be on the current point. This is regardless of the
existence of an annot-type overlay on the point."
  :type 'boolean
  :group 'annot)

(defcustom annot-prefer-eshell nil
  "If enabled, run and obtain result through eshell instead of a shell."
  :type 'boolean
  :group 'annot)

(defcustom annot-enable-fuf-support nil
  "If enabled, enable annot's support of `fuf.el' \(fold-or-unfold), in which case,
annot will save fuf's folding & unfolding states."
  :type 'boolean
  :group 'annot)

(defconst annot-default-tag "__DEFAULT_TAG__")

(defface annot-text-face
  '((((class color) (background light)) (:foreground "dark red"))
    (((class color) (background dark)) (:foreground "IndianRed4")))
  "Face for annotation text."
  :group 'annot)

(defface annot-highlighter-face
  '((((class color) (background light)) (:background "yellow"))
    (((class color) (background dark)) (:foreground "white" :background "IndianRed4")))
  "Face for highlighted text."
  :group 'annot)

(defconst annot-subsequence-length 24
  "Number of characters to be stored before and after \
annotation's position in a file buffer.")

(defconst annot-contents-dirname "contents"
  "Name of the annotation directory.")

(defconst annot-symlinks-dirname "symlinks"
  "Name of the annotation symlink directory.")

(defconst annot-available-types '(text highlight image ellipsis)
  "A list of :type overlay properties that annot uses.
`ellipsis' is not really used, unless you enable
`annot-enable-fuf-support'.")

(defvar annot-buffer-overlays nil
  "List of overlays in the buffer.")
(make-variable-buffer-local 'annot-buffer-overlays)

(defvar annot-buffer-plist nil
  "Plist containing annotation information in the buffer.
Unfortunately `make-variable-buffer-local' does not make the
symbol plist buffer-local; so this variable has to exist
separately.")
(make-variable-buffer-local 'annot-buffer-plist)


;;;; Macros

(defmacro annot-with-message-suppression (&rest body)
  "Suppress any incoming messages within `body' while keeping the
currently displayed message, if any."
  (let ((msg (make-symbol "msg-temp")))
    `(let ((,msg (current-message))
           (message-log-max nil))
       (unwind-protect
           (progn ,@body)
         (if ,msg
             (message ,msg)
           (message nil))))))


(defmacro annot-without-modifying-buffer (&rest body)
  (let ((tmp-var (make-symbol "buf-modified-p")))
    `(let ((,tmp-var (buffer-modified-p)))
       (unwind-protect
           (progn ,@body)
         (unless ,tmp-var (set-buffer-modified-p nil))))))


;;;; User commands.

(defun annot-add (&optional text/image/region)
  "Add an annotation on the current point.
If a marked region is present, highlight it."
  (interactive)
  (let* ((text/image/region (or text/image/region
                                (if (region-active-p)
                                    `(,(region-beginning) . ,(region-end))
                                  (read-string "Annotation: "))))
         (ov-list (annot-create-new text/image/region)))
    (when ov-list
      (dolist (ov ov-list)
        (push ov annot-buffer-overlays))
      (annot-save-annotations)
      ;; When on indirect buffer, sync with its base buffer as well.
      (annot-base-buffer-add text/image/region))))


(defun annot-add-image (&optional image-filename)
  "Insert an image on the current point."
  (interactive)
  (if (not (annot-buffer-local-file-p))
      (error "buffer is not local file."))
  (if (and window-system (display-images-p))
      (let ((image-filename
             (or image-filename
                 (car (let ((default-directory
                              (or (and (file-directory-p annot-image-directory)
                                       annot-image-directory)
                                  default-directory)))
                        (find-file-read-args "Image: " t))))))
        (annot-add (propertize image-filename 'display
                               (create-image (expand-file-name image-filename)))))
    (message "You are not on a window-system that can display images.")))

(defun annot-edit (&optional ov)
  "Edit a nearby annotation on the current line."
  (interactive)
  (let ((p (point))
        (ov (or ov (annot-get-annotation-at-point))))
    (cond
     ((null ov)
      (message "No annotation to edit at point."))
     ((region-active-p)
      (message "Highlight cannot be edited."))
     (t
      (let ((text (read-string "Annotation: "
                               (annot-trim
                                (substring-no-properties
                                 (or (overlay-get ov 'before-string) ""))))))
        (if (zerop (length (annot-trim text)))
            (annot-remove ov)
          (overlay-put ov 'before-string
                       (funcall annot-text-decoration-function text))
          (overlay-put ov :modtime (float-time))
          (annot-save-annotations)
          (annot-base-buffer-edit text)))))))

(defun annot-remove (&optional ov silent)
  "Remove a nearby annotation on the current line.
If a regioin is specified, remove all annotations and highlights within it."
  (interactive)
  (if (region-active-p)
      (let ((beg (region-beginning))
            (end (region-end)))
        (deactivate-mark t)    ;; Avoid endless recursion.
        (annot-delete-annotations-region beg end))
    (let* ((annot-ov (or ov (annot-get-annotation-at-point))) ov p)
      (when annot-ov
        (setq annot-buffer-overlays (delq annot-ov annot-buffer-overlays))
        (delete-overlay annot-ov)
        (annot-save-annotations)
        (annot-base-buffer-remove)
        (unless silent
          (message "Annotation removed.")))
      (while (and annot-broader-removal-p
                 (setq ov (or
                           (car (overlays-in (setq p (point)) p))
                           ;; relax finding of an overlay
                           (car (overlays-in (max (point-min) (1- p)) p))
                           (car (overlays-in p (min (point-max) (1+ p)))))))
        (delete-overlay ov)))))

(defun annot-edit/add ()
  "Either edit the annotation at point, if there is, or else add a new one.
If a region is specified, a highlight annotation will be added or edited."
  (interactive)
  (if (not (annot-buffer-local-file-p))
      (error "buffer is not local file."))
  (let (ov)
    (cond
     ((region-active-p)
      (annot-add)
      (deactivate-mark t))
     ((and (setq ov (annot-get-annotation-at-point))
           (not (annot-highlight-p ov)))
      (annot-edit ov))
     (t (annot-add)))))

(defun annot-convert ()
  "Convert text within the active region into an annot text annotation."
  (interactive)
  (if (not (annot-buffer-local-file-p))
      (error "buffer is not local file."))
  (when (and (region-active-p)
             (> (- (region-end) (region-beginning)) 0))
    (let ((text (prog1
                    (buffer-substring-no-properties
                     (region-beginning) (region-end))
                  (deactivate-mark))))
      (kill-region (region-beginning) (region-end))
      (annot-add text))))

(defun annot-load-annotations ()
  "Load the annotation file corresponding to the current buffer.
If current `annot-buffer-overlays' looks newer \(which shouldn't
happen as long as you keep using annot), it asks whether to load
the file or not."
  (interactive)
  (unless (or (member major-mode annot-load-disabled-modes)  (not (annot-buffer-local-file-p)) (not (file-exists-p (annot-buffer-file-name))))
    (let ((current-md5 (annot-md5 (current-buffer)))
          (filename (annot-buffer-file-name)))
      (when (or (file-readable-p
                 (setq filename (annot-get-annot-filename (annot-buffer-file-name) current-md5)))
                ;; If md5 fails, try symlink.
                (and (setq filename (annot-get-symlink (annot-buffer-file-name)))
                     (file-readable-p filename)))
        (annot-with-message-suppression
         (load-file filename))))))

(defun annot-to-comment ()
  "Convert a text annotation at point to a comment."
  (interactive)
  (if buffer-read-only
      (message "This buffer cannot be edited.")
    (let ((ov (annot-get-annotation-at-point)))
      (when (and ov (not (annot-highlight-p ov)))
        (save-excursion
          (annot-remove ov t)
          (insert (format " %s%s%s"
                          comment-start
                          (annot-trim (overlay-get ov 'before-string))
                          comment-end)))))))

(defun annot-goto-previous (&optional annot-types)
  "Go to the previous annot overlay from the current point.
If any is found, return t; nil otherwise. Specify `annot-types'
as a list if you want specific types of annotation to be
captured."
  (interactive)
  (let (pt ov)
    (when (catch 'finished
            (save-excursion
              (save-restriction
                (while (< (point-min)
                          (setq pt (previous-overlay-change (point))))
                  (goto-char pt)
                  (setq ov (car (annot-overlays-in pt (1+ pt))))
                  (when (and ov
                             (member (overlay-get ov :type)
                                     (or (and (listp annot-types) annot-types)
                                         annot-available-types))
                             (not (annot-invisible-p pt)))
                    (throw 'finished t)))
                (message "(No previous visible annot found)")
                nil)))
      (goto-char pt))))


(defun annot-goto-next (&optional annot-types)
  "Go to the next annot overlay beginning from the current point.
If any is found, return t; nil otherwise. Specify `annot-types'
as a list if you want specific types of annotation to be
captured."
  (interactive)
  (let (pt ov)
    (when (catch 'finished
            (save-excursion
              (save-restriction
                (while (< (setq pt (next-overlay-change (point)))
                          (point-max))
                  (goto-char pt)
                  (setq ov (car (annot-overlays-in pt (1+ pt))))
                  (when (and ov
                             (member (overlay-get ov :type)
                                     (or (and (listp annot-types) annot-types)
                                         annot-available-types))
                             (not (annot-invisible-p pt)))
                    (throw 'finished t)))
                (message "(No further visible annot found)")
                nil)))
      (goto-char pt))))


;;;; Indirect buffer functions.
;; These are functions for synching up with an indirect buffer's annotations
;; (but not the oppositve - i.e. upward direction only).  This supports no more
;; than single indirect buffer: multiple indirect buffers pointing to the same
;; base buffer may not be fully reflected.
;; TODO: Use a macro for annot-base-* functions below.

(defun annot-base-buffer-add (text/image/region)
  (let ((base-buffer (buffer-base-buffer)))
    (when base-buffer
      (let ((p (point)))
        (with-current-buffer base-buffer
          (save-excursion
            (goto-char p)
            (let ((ov-list (annot-create-new text/image/region)))
              (when ov-list
                (dolist (ov ov-list)
                  (push ov annot-buffer-overlays))
                ;; This saving, while it may seem to be unnecessary at first
                ;; glance, is necessary since you may be mainly editing on the
                ;; base buffer
                (annot-save-annotations)))))))))

(defun annot-base-buffer-remove ()
  (let ((base-buffer (buffer-base-buffer)))
    (when base-buffer
      (let ((p (point)) ov)
        (with-current-buffer base-buffer
          (save-excursion
            (goto-char p)
            (when (setq ov (annot-get-annotation-at-point))
              (setq annot-buffer-overlays (delq ov annot-buffer-overlays))
              (delete-overlay ov)
              (annot-save-annotations))))))))

(defun annot-base-buffer-edit (text)
  (let ((base-buffer (buffer-base-buffer)))
    (when (and base-buffer (not (zerop (length (annot-trim text)))))
      (let ((p (point)))
        (with-current-buffer base-buffer
          (save-excursion
            (goto-char p)
            (overlay-put (annot-get-annotation-at-point) 'before-string
                         (funcall annot-text-decoration-function text))
            (annot-save-annotations)))))))

;;;; Internal functions.

;; (defsubst annot-buffer-file-name ()
;;   "Same as \(buffer-file-name) but is extended to be compatible
;; with indirect buffers."
;;   (buffer-file-name
;;    (or (buffer-base-buffer)
;;        (current-buffer))))

(defsubst annot-remove-html-anchor (file-name)
  ""
  (let* ((len (length file-name))
         (pos-of-anchor (s-index-of "#" file-name)))
    (if (not pos-of-anchor) ;; no anchor part
        file-name
      (substring file-name 0 pos-of-anchor))
    )
  )

(defsubst annot-buffer-file-name ()
  "Same as \(buffer-file-name) but is extended to be compatible
with indirect buffers."
  (cond
   ((eq major-mode 'eww-mode)
    (setq buffer-url (eww-current-url))
    (when (string-prefix-p "file://" buffer-url)
      (annot-remove-html-anchor (substring buffer-url (length "file://")))))
   ((eq major-mode 'w3m-mode)
    (setq buffer-url w3m-current-url)
    (when (string-prefix-p "file://" buffer-url)
      (annot-remove-html-anchor (substring buffer-url (length "file://")))))
   (t
    (buffer-file-name
     (or (buffer-base-buffer)
         (current-buffer))))
   ))

(defsubst annot-buffer-local-file-p ()
  (if (annot-buffer-file-name)
      t))

(defsubst annot-trim (s)
  "Trim non-graphic chars from both ends of string s."
  (replace-regexp-in-string
   "\\`[^[:graph:]]+\\|[^[:graph:]]+\\'"  "" s))

(defsubst annot-assoc-matched-filename (filename)
  "Return annot-directory-alist item which matches filename most."
  (let ((max 0)
        result)
    (dolist (assoc annot-directory-alist)
      (when (string-prefix-p (cadr assoc) filename)
        (when (> (length (cadr assoc)) max)
          (setq result assoc)
          (setq max (length (cadr assoc))))))
    (identity result)))

(defsubst annot-assoc-matched-tag (tag)
  "Return annot-directory-alist item matching tag."
  (cl-find-if
   (lambda (x)
     (string= (car x) tag))
   annot-directory-alist))

(defun annot-check-annot ()
  (interactive)
  (message "%S" (annot-file-tag (annot-buffer-file-name))))

(defsubst annot-contents-directory (filename)
  "Return contents directory of filename"
  (let ((annot-dir-assoc (annot-assoc-matched-filename filename)))
    (if annot-dir-assoc
        (format "%s/%s" (cddr annot-dir-assoc) annot-contents-dirname)
      (format "%s/%s" annot-default-directory annot-contents-dirname))))

(defsubst annot-file-tag (filename)
  "Return base directory of filename"
  (let ((annot-dir-assoc (annot-assoc-matched-filename filename)))
    (if annot-dir-assoc
        (car annot-dir-assoc)
      annot-default-tag
      )))

(defsubst annot-contents-directory-tag (tag)
  "Return contents directory using tag"
  (let ((annot-dir-assoc (annot-assoc-matched-tag tag)))
    (if annot-dir-assoc
        (format "%s/%s" (cddr annot-dir-assoc) annot-contents-dirname)
      (format "%s/%s" annot-default-directory annot-contents-dirname))))

(defsubst annot-file-base-directory (filename)
  "Return base directory of filename"
  (let ((annot-dir-assoc (annot-assoc-matched-filename filename)))
    (if annot-dir-assoc
        (cadr annot-dir-assoc)
      "/"
      )))

(defsubst annot-file-base-directory-tag (tag)
  "Return base directory of tag"
  (let ((annot-dir-assoc (annot-assoc-matched-tag tag)))
    (if annot-dir-assoc
        (cadr annot-dir-assoc)
      "/"
      )))

(defsubst annot-file-relative-path (filename)
  "Return relative path of filename to annot-file-base-directory"
  (let ((annot-dir-assoc (annot-assoc-matched-filename filename)))
    (if annot-dir-assoc
        (s-chop-prefix (cadr annot-dir-assoc) filename)
      (s-chop-prefix "/" filename)
      )))

(defsubst annot-symlinks-directory (filename)
  "Return contents directory of filename"
  (let ((annot-dir-assoc (annot-assoc-matched-filename filename)))
    (if annot-dir-assoc
        (format "%s/%s" (cddr annot-dir-assoc) annot-symlinks-dirname)
      (format "%s/%s" annot-default-directory annot-symlinks-dirname))))

(defsubst annot-get-annot-filename (filename md5)
  "Return the full path of the annotation filename."
  (expand-file-name (format "%s/%s"
                            (annot-contents-directory filename) md5)))


;; (defsubst annot-md5 (&optional buffer)
;;   "Get md5 of the buffer content with max chars `annot-md5-max-chars'.
;; If `annot-md5-max-chars' is nil, no limit is imposed."
;;   (let ((buffer (or buffer (current-buffer))))
;;     (md5 buffer nil
;;          (if (null annot-md5-max-chars)
;;              nil
;;            (min annot-md5-max-chars (point-max)))
;;          nil t)))

(defsubst annot-md5 (&optional buffer)
  "Get md5 of the buffer content with max chars `annot-md5-max-chars'.
If `annot-md5-max-chars' is nil, no limit is imposed."
  (with-current-buffer (or buffer (current-buffer))
    (md5 (f-read-bytes (annot-buffer-file-name)))))

(defsubst annot-argmax (L fn)
  (let* ((best (car L))
         (best-score (funcall fn best)) score)
    (dolist (e L)
      (when (> (setq score (funcall fn e))
               best-score)
        (setq best-score score
              best e)))
    best))


(defsubst annot-highlight-p (ov)
  "Non-nil if an overlay `ov' is of type highlight."
  (equal (overlay-get ov :type) 'highlight))


(defsubst annot-find-highlight (ov-list)
  "Find, if any, a highlight in a list of overlays."
  (catch 'found
    (dolist (ov ov-list)
      (when (annot-highlight-p ov)
        (throw 'found ov)))))


(defsubst annot-overlays-in (beg end)
  "Get a list of, not pure, but annot overlays between beg and end points."
  (let (L)
    (dolist (ov (overlays-in beg end))
      (when (member (overlay-get ov :type)
                    annot-available-types)
        (push ov L)))
    L))


(defsubst annot-invisible-p (pos &optional object)
  "Non-nil if char at `pos' is invisible."
  (let ((invisible (get-char-property pos 'invisible object)))
    (if (eq buffer-invisibility-spec t)
        invisible
      (catch :result
        (dolist (prop (if (consp invisible)
                          invisible
                        (list invisible)))
          (when (or (memq prop buffer-invisibility-spec)
                    (assq prop buffer-invisibility-spec))
            (throw :result t)))))))


;; (defun annot-create-new (text/image/region)
;;   "Create a new list of overlay(s) depending of the content of `text/image/region'.
;; In particular, a text highlight may yield multiple overlays depending on
;; the region ends."
;;   (cond
;;    ((or (null text/image/region)
;;         (stringp text/image/region))
;;     (let ((text/image (or text/image/region (read-string "Annotation: "))))
;;       (unless (zerop (length (annot-trim text/image)))
;;         (list (annot-create-overlay (point) text/image)))))
;;    ((listp text/image/region)
;;     (let ((beg (car text/image/region))
;;           (end (cdr text/image/region))
;;           (modtime (float-time))
;;           ov-list a b)
;;       (save-excursion
;;         (goto-char beg)
;;         (while (and
;;                 (< (point) end)
;;                 (re-search-forward "[[:graph:]]" end t)
;;                 (setq a (goto-char (match-beginning 0))))
;;           (if (and
;;                (re-search-forward "[[:graph:]][^[:graph:]]*?$" end t)
;;                (setq b (goto-char (1+ (match-beginning 0)))))
;;               (push (annot-create-highlight-overlay a b modtime) ov-list)
;;             (goto-char end)
;;             (save-excursion
;;               (when (re-search-backward "[[:graph:]]" beg t)
;;                 (push (annot-create-highlight-overlay a (match-end 0) modtime)
;;                       ov-list))))))
;;       ov-list))))

(defun annot-create-new (text/image/region)
  "Create a new list of overlay(s) depending of the content of `text/image/region'.
In particular, a text highlight may yield multiple overlays depending on
the region ends."
  (cond
   ((or (null text/image/region)
        (stringp text/image/region))
    (let ((text/image (or text/image/region (read-string "Annotation: "))))
      (unless (zerop (length (annot-trim text/image)))
        (list (annot-create-overlay (point) text/image)))))
   ((listp text/image/region)
    (let ((beg (car text/image/region))
          (end (cdr text/image/region))
          (modtime (float-time))
          ov-list a b)
      (setq ov-list (list (annot-create-highlight-overlay beg end modtime)))
      ov-list))))

(defun annot-file-exists-p ()
  "Returns an annotation filename if current buffer accompanies with it."
  (let ((annot-filename (annot-get-annot-filename (annot-md5 (current-buffer)))))
    (and (file-exists-p annot-filename) annot-filename)))


(defun annot-decorate-text (text)
  "Decorate an annotation text."
  (let* ((n (length text))
         (last-newline-p (string= (substring text (max 0 (1- n)) n) "\n"))
         (text (annot-trim text))
         (text-is-image-p (get-text-property 0 'display text))
         (annot-text (format "%s%s%s"
                             (if text-is-image-p "" " ")
                             text
                             (or (and last-newline-p "\n")
                                 (and text-is-image-p "")
                                 " "))))
    (format "%s%s" (if (bolp) "" " ")
            (propertize annot-text 'face 'annot-text-face))))


(defun annot-get-annotation-at-point ()
  "Get annotation or highlight \(equiv. overlay) at point."
  (let ((p (point)))
    (or (car (annot-overlays-in p p))
        ;; Relax by 1 point for highlights
        (annot-find-highlight (annot-overlays-in (max (point-min) (1- p)) p))
        (annot-find-highlight (annot-overlays-in p (min (point-max) (1+ p)))))))


(defun annot-create-overlay (pos text/image)
  "Create a text overlay or image overlay."
  ;; (assert (not (zerop (length (annot-trim text/image)))))
  (let ((ov (make-overlay pos pos nil t nil)))
    (overlay-put ov 'before-string
                 (funcall annot-text-decoration-function text/image))
    (overlay-put ov :pos pos)    ;; it will be saved later anyways...
    (overlay-put ov :prev (buffer-substring-no-properties
                           (max (point-min) (- pos annot-subsequence-length)) pos))
    (overlay-put ov :next (buffer-substring-no-properties
                           pos (min (point-max) (+ pos annot-subsequence-length))))
    (if (get-text-property 0 'display text/image)
        (overlay-put ov :type 'image)
      (overlay-put ov :type 'text))
    (overlay-put ov :modtime (float-time))
    ov))


(defun annot-create-highlight-overlay (beg end &optional modtime)
  "Create a highlight overlay starting from `beg' to `end'."
  (let ((ov (make-overlay beg end nil t nil)))
    (overlay-put ov 'face 'annot-highlighter-face)
    (overlay-put ov 'evaporate t)
    (overlay-put ov :prev (buffer-substring-no-properties
                           (max (point-min) (- beg annot-subsequence-length)) beg))
    (overlay-put ov :next (buffer-substring-no-properties
                           end (min (point-max) (+ end annot-subsequence-length))))
    (overlay-put ov :beg beg)
    (overlay-put ov :end end)
    (overlay-put ov :type 'highlight)
    (overlay-put ov :modtime (or modtime (float-time)))
    ov))

(defun annot-format-overlays (md5 tag relative-filename modtime)
  "Generate a string containing all information necessary to reproduce annotations."
  (let ((ov-plists-s
         (let ((print-escape-newlines t))    ;; newline is represented as "\n"
           (mapconcat (lambda (ov)
                        (format "%S" (overlay-properties ov)))
                      annot-buffer-overlays "\n"))))
    (format ";;; -*- mode: emacs-lisp -*-
\(annot-recover-annotations '\(
:md5 %S
:tag %S
:relative-filename \"%s\"
:modtime %S
:md5-max-chars %S
:annotations \(
%s
)))" md5 tag relative-filename modtime annot-md5-max-chars ov-plists-s)))

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
    (let* ((md5 (plist-get annot-buffer-plist :md5))
          (tag (plist-get annot-buffer-plist :tag))
          (dirname (annot-file-base-directory-tag tag))
          (relative-filename (plist-get annot-buffer-plist :relative-filename))
          (filename (concat dirname relative-filename))
          old-annot-filename symlink)
      (when (and filename md5)
        (setq old-annot-filename (annot-get-annot-filename filename md5))
        (if (file-readable-p old-annot-filename)
            (delete-file old-annot-filename)))
      (when filename
        (setq symlink (annot-get-symlink filename))
        (if (file-symlink-p symlink)
            (delete-file symlink)))))
   (t
    (let* ((filename (annot-buffer-file-name))
           (tag (annot-file-tag filename))
           (dirname (annot-file-base-directory filename))
           (relative-filename (annot-file-relative-path filename)))
      ;; Only save annotations if the current buffer is a file-buffer whose
      ;; (annot-buffer-file-name) does not match `annot-save-exclusion-regexp'.
      ;; It is possible to save/load annotations for non-file-buffers, but it is
      ;; not supported yet just to play safe.
      (when (and filename
                 (not (and annot-save-exclusion-regexp
                           (string-match annot-save-exclusion-regexp filename))))
        (save-restriction
          (widen)    ;; in case narrowing is in effect.
          (let* ((buffer (current-buffer))
                 (prev-md5 (plist-get annot-buffer-plist :md5))
                 (prev-tag (plist-get annot-buffer-plist :tag))
                 (prev-dirname (annot-file-base-directory-tag tag))
                 (prev-relative-filename (plist-get annot-buffer-plist :relative-filename))
                 ;; (prev-filename (plist-get annot-buffer-plist :filename))
                 (prev-filename (concat prev-dirname prev-relative-filename))
                 (md5 (annot-md5 buffer))
                 (annot-filename (annot-get-annot-filename filename md5))
                 (modtime (or (plist-get annot-buffer-plist :modtime) (float-time)))
                 s)
            ;; Update ((:beg/:end)|:pos) and :prev/:next of each overlay
            (dolist (ov annot-buffer-overlays)
              (let ((beg (overlay-start ov))
                    (end (overlay-end ov)))
                ;; avoid a rare case where overlay-start or overlay-end is nil.
                (when (and beg end)
                  (if (annot-highlight-p ov)
                      (progn
                        (overlay-put ov :beg beg)
                        (overlay-put ov :end end))
                    (overlay-put ov :pos end))
                  (overlay-put ov :prev
                               (buffer-substring-no-properties
                                (max (point-min) (- beg annot-subsequence-length)) beg))
                  (overlay-put ov :next
                               (buffer-substring-no-properties
                                end (min (point-max) (+ end annot-subsequence-length)))))))

            ;; Delete previous symlink, if any, before creating one.
            (when (and prev-md5 prev-filename (not (string= prev-md5 md5)))
              (if (file-symlink-p (annot-get-symlink prev-filename))
                  (delete-file (annot-get-symlink prev-filename))))

            ;; Get the S-expression and save the annotations
            (setq s (annot-format-overlays md5 tag relative-filename modtime))
            (condition-case error
                (progn
                  (annot-save-content s filename annot-filename)
                  (when annot-enable-symlinking
                    (annot-save-symlink md5 filename)))
              (error
               (warn "annot-save-annotations: %s" (error-message-string error))))

            ;; Update `annot-buffer-plist'
            (dolist (e '(md5 tag dirname relative-filename modtime))
              (setq annot-buffer-plist
                    (plist-put annot-buffer-plist
                               (intern (format ":%S" e)) (symbol-value e))))

            ;; If md5 doesn't match the previous one, and both files
            ;; refer to the same file, delete the old annotation file.
            ;; In the case of
            ;; $ cp a b ; emacs b   # then modify b and maybe add/edit/remove annotation/highlight.
            ;; we still want to keep annotations for a (and b).
            ;; On the other hand, we do not want to keep obsolete annotations
            ;; if prev and current versions both point to the same file.
            (when (and prev-md5
                       (not (string= prev-md5 md5))
                       (string= prev-filename filename))
              (let ((old-annot-filename (annot-get-annot-filename prev-filename prev-md5)))
                (if (file-readable-p old-annot-filename)
                    (delete-file old-annot-filename))))
            annot-filename)))))))


(defun annot-save-content (content filename annot-filename)
  "Write `content' into a `annot-filename'.
Create the annot content directory if it does not exist."
  (unless (file-exists-p (annot-contents-directory filename))
    (make-directory (annot-contents-directory filename) t))
  (with-temp-file annot-filename
    (erase-buffer)
    (insert content)))

(defun annot-save-symlink (md5 filename)
  "Make a symbolic link pointing to an annot-filename."
  (let ((symlinks-dir (annot-symlinks-directory filename)))
    (unless (file-exists-p symlinks-dir)
      (make-directory symlinks-dir))
    (make-symbolic-link
     (format "../%s/%s" annot-contents-dirname md5)
     (annot-get-symlink filename)
     'ok-if-already-exists)))

(defun annot-get-symlink (filename)
  "Return symlink path."
  (when filename
  (let ((backup-directory-alist `(("." . ,(annot-symlinks-directory filename)))))
    (make-backup-file-name (expand-file-name filename)))))

(defsubst annot-get-beg (ov-plist)
  "Get the starting point of an annotation represented by `ov-plist'."
  (or (plist-get ov-plist :beg)
      (plist-get ov-plist :pos)))

(defsubst annot-get-end (ov-plist)
  "Get the end point of an annotation represented by `ov-plist'."
  (or (plist-get ov-plist :end)
      (plist-get ov-plist :pos)))


(defun annot-recover-overlay (ov-plist)
  "Recover an overlay."
  (let* ((ov (make-overlay (annot-get-beg ov-plist)
                           (annot-get-end ov-plist)
                           nil t nil)))
    (dotimes (i (length ov-plist))
      (when (eq (logand i 1) 0)
        (overlay-put ov (nth i ov-plist) (nth (1+ i) ov-plist))))
    ov))


(defun annot-valid-p (ov-plist &optional force-strict-checking)
  "Do an integrity checking on annotation/highlight plist.
In particular, the prev-string of each `ov-plist' gets verified.
If `annot-enable-strict-integrity-checking' is non-nil, check
the next-string as well."
  (let ((prev-string (plist-get ov-plist :prev))
        (beg (annot-get-beg ov-plist))
        (end (annot-get-end ov-plist))
        (good-region-types (if annot-enable-fuf-support
                               '(highlight ellipsis)
                             '(highlight))))
    (and
     (<= end (point-max))
     (if (member (plist-get ov-plist :type) good-region-types)
         (< beg end)
       (> (length (plist-get ov-plist 'before-string)) 0))
     (string= prev-string
              (buffer-substring-no-properties
               (max (point-min) (- beg (length prev-string))) beg))
     (or (and (null annot-enable-strict-integrity-checking)
              (null force-strict-checking))
         (let ((next-string (plist-get ov-plist :next)))
           (string= next-string
                    (buffer-substring-no-properties
                     end (min (point-max)
                              (+ end (length next-string))))))))))

(defun annot-recover-annotations (annotations-info)
  "Restore annotations.
Only annotation files use this function internally."
  (let ((annotations (plist-get annotations-info   :annotations))
        (modtime     (plist-get annotations-info   :modtime))
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

      ;; Sort annotations/highlights by beg position.
      ;;
      ;; The following constraint would omit some extra computation. However,
      ;; since we cannot guarantee that `annot-md5-max-chars' would cover all
      ;; lengths of annotated files that are to be opened, not imposing it is
      ;; more accurate and indeed desirable.
      ;; 
      ;; (when (boundp 'current-md5)
      ;;   (unless (string= current-md5 (plist-get annotations-info :md5))
      ;;
      (setq annotations (sort annotations
                              (lambda (op1 op2)
                                (< (annot-get-beg op1)
                                   (annot-get-beg op2)))))

      ;; Mmmkay, let's reproduce annotations.
      (save-excursion
        (let ((last-valid-point (point-min))
              invalid-found-p)
          (dolist (ov-plist annotations)
            (cond
             ((and (null invalid-found-p) (annot-valid-p ov-plist))
              ;;; FIXME: this is not exactly an elegant solution.
              ;; Make fuf invisibility available globally.  fuf is more useful this
              ;; way rather than creating a minor mode and hooking up the code
              ;; below every single time.
              (when (and annot-enable-fuf-support
                         (eq (plist-get ov-plist 'invisible) 'fuf))
                (unless (and (listp buffer-invisibility-spec)
                             (assoc 'fuf buffer-invisibility-spec))
                  (add-to-invisibility-spec '(fuf . t))))
              ;; Juice - actually create an annot
              (push (annot-recover-overlay ov-plist) annot-buffer-overlays)
              (setq last-valid-point (annot-get-beg ov-plist)))
             ;; Linear recovery in case some invalid annotation is found.
             (t
              (when (null invalid-found-p) ;; first time
                (setq invalid-found-p t))
              (let ((prev-string (plist-get ov-plist :prev))
                    (type (plist-get ov-plist :type)))
                (goto-char (max (point-min) (- last-valid-point
                                               (length prev-string))))
                (catch 'found
                  (while (search-forward prev-string nil t)
                    ;; Change beg/end points before validation.
                    (if (not (equal type 'highlight))
                        (setq ov-plist (plist-put ov-plist :pos (point)))
                      (setq ov-plist (plist-put ov-plist :end (+ (point) (- (annot-get-end ov-plist)
                                                                            (annot-get-beg ov-plist)))))
                      (setq ov-plist (plist-put ov-plist :beg (point))))
                    ;; Check and, if ok, create an overlay.
                    (when (annot-valid-p ov-plist 'force-strict-checking)
                      (push (annot-recover-overlay ov-plist) annot-buffer-overlays)
                      (setq last-valid-point (annot-get-beg ov-plist))
                      (throw 'found t))))))))))

      (setq annot-buffer-plist
            `(:md5 ,(plist-get annotations-info :md5)
                   :tag       ,(plist-get annotations-info :tag)
                   :relative-filename       ,(plist-get annotations-info :relative-filename)
                   :modtime        ,modtime)))))

(defun annot-delete-annotations-region (r-beg r-end)
  "Delete all annotations in region."
  (interactive "r")
  (dolist (ov annot-buffer-overlays)
    (let ((beg (overlay-start ov))
          (end (overlay-end ov)))
      (when (or (null beg) (null end)
                (and (>= r-end end) (>= beg r-beg)))
        (annot-remove ov t)))))


(defun annot-annot-filename (&optional filename)
  (interactive)
  (let ((filename (or filename (buffer-file-name))))
    (if (and filename (f-exists-p filename))
        (message (format "%s" (annot-get-annot-filename filename (md5 (f-read-bytes filename)))))
      (message "Not a valid file to create annotations for it."))))

;;;; Keybindings.

;; (define-key ctl-x-map "\C-a" 'annot-edit/add)
;; (define-key ctl-x-map "\C-r" 'annot-remove)
;; (define-key ctl-x-map "\C-i" 'annot-add-image)
;; (define-key ctl-x-map "a" 'annot-edit/add)
;; (define-key ctl-x-map "r" 'annot-remove)
;; (define-key ctl-x-map "w" 'annot-add-image)
;; (define-key ctl-x-map "A" 'annot-convert)


;;;; Hooks and Advices.

(defvar annot-buffer-modified-p nil)
(make-variable-buffer-local 'annot-buffer-modified-p)

(defun annot-before-save-hook ()
  (setq annot-buffer-modified-p (buffer-modified-p)))
(add-hook 'before-save-hook 'annot-before-save-hook)

(defun annot-after-save-hook ()
  (when annot-buffer-modified-p
    (annot-save-annotations)
    ;; Let's make it a rule that if the current buffer is modified and the last
    ;; annotation in the buffer is of the form "after-save: <s-exp>", then
    ;; evaluate <s-exp> just after save-buffer.
    ;; Let's make another rule that if the current buffer is modified and the last
    ;; annotation in the buffer is of the form "$ <shell-command>", then
    ;; execute the <shell-command> with `shell-command-to-string'.
    ;; This means you can either have "after-save" rule or "$" rule at the end, if any.
    (let (last-ov s s-exp command)
      (when (and annot-buffer-overlays
                 (setq last-ov (annot-argmax annot-buffer-overlays
                                             (lambda (ov) (or (overlay-start ov) 1))))
                 (setq s (overlay-get last-ov 'before-string)))
        (cond
         ;; the "after-save" rule
         ;; Example: after-save: (message "yellow")
         ((string-match "\\` *after-save: *\\((.+\\)" s)
          (message "Evaluating: %s"
                   (setq s-exp (match-string-no-properties 1 s)))
          (eval (read s-exp))
          (message "Finished evaluating: %s" s-exp))
         ;; (<existing-function> ...) rule (experimental)
         ;; This rule is somewhat controversial and so is disabled by default.
         ;; Example: (message "yellow")
         ((and
           annot-execute-last-sexp-p
           (string-match "\\` *\\((\\([[:graph:]]+\\).*)\\) *\\'" s))
          (when (fboundp (intern (match-string-no-properties 2 s)))
            (message "Evaluating: %s"
                     (setq s-exp (match-string-no-properties 1 s)))
            (eval (read s-exp))
            (message "Finished evaluating: %s" s-exp)))
         ;; the "$" rule
         ;; Example: $ scp annot.el $host:/tmp/
         ((string-match "\\` *\\$ *\\([[:graph:]].*\\)" s)
          (setq command (replace-regexp-in-string "%s\\b" (buffer-name)
                                                  (match-string-no-properties 1 s)))
          (message "%s %s" (if (= (user-uid) 0) "#" "$") command)
          (message (if (and annot-prefer-eshell (featurep 'eshell))
                       (eshell-command-result command)
                     (shell-command-to-string command))))))))
  (setq annot-buffer-modified-p nil))

;; (add-hook 'after-save-hook 'annot-after-save-hook)
;; (add-hook 'find-file-hook 'annot-load-annotations)

(add-hook 'after-save-hook 'annot-after-save-hook)
(add-hook 'find-file-hook 'annot-load-annotations)

(defadvice delete-region (before annot-delete-region activate)
  "Enable deletion of annotations within the specified region."
  (dolist (ov (annot-overlays-in start end))
    (annot-remove ov t)))

;;;; Kill/yank (copy/paste) support

(defun annot-add-annots-to-text (r-beg r-end)
  "Add text-property representation of annotations/highlights to
text within the region. It does so by appending `annot-exists'
and `annot-positions' text properties at the beginning of the
region; it also appends, for each annot overlay, `annot' text
property, representing each annot overlay."
  (unless buffer-read-only
    (let* ((ov-list (annot-overlays-in r-beg r-end))
           (ov-exists (> (length ov-list) 0))
           (offset r-beg)
           annot-positions)
      (annot-without-modifying-buffer
       (dolist (ov ov-list)
         (let ((ov-start (overlay-start ov))
               (ov-end (overlay-end ov))
               (ov-plist (overlay-properties ov)))
           (when
               ;; Change position so that each position is relative to 'offset'
               (or
                (and (equal (plist-get ov-plist :type) 'highlight)
                     (plist-put ov-plist :beg (- ov-start offset))
                     (plist-put ov-plist :end (- ov-end offset)))
                (and (member (plist-get ov-plist :type) '(text image))
                     (plist-get ov-plist 'before-string)
                     (plist-put ov-plist :pos (- ov-start offset))))
             ;; Add text property for this particular ov
             (add-text-properties
              ov-start
              (min (point-max) (1+ ov-start))
              (list 'annot ov-plist))
             (push (- ov-start offset) annot-positions))))
       (when ov-exists
         (add-text-properties
          r-beg (min (point-max) (1+ r-beg))
          (list 'annot-exists t
                'annot-positions annot-positions)))))))

(defadvice kill-region (before annot-kill-region activate)
  "annot support for kill-region."
  (let ((a (min beg end))
        (b (max beg end)))
    (annot-add-annots-to-text a b)
    (annot-delete-annotations-region a b)))

(defadvice kill-ring-save (before annot-kill-ring-save activate)
  "annot support for kill-ring-save."
  (let ((a (min beg end))
        (b (max beg end)))
    (annot-add-annots-to-text a b)))

(defadvice yank (around annot-yank activate)
  "annot support for yank.
This advice interprets text properties appended by `annot-add-annots-to-text'
and create, for each annot text property, an annot overlay."
  (let ((annot-yank-start (point)))
    ad-do-it
    ;; Do this annot yank stuff only if annot-exists is at the current
    ;; point.
    (when (get-text-property annot-yank-start 'annot-exists)
      (save-excursion
        (let ((offset annot-yank-start)
              (annot-positions (get-text-property annot-yank-start 'annot-positions)))
          (dolist (annot-pos annot-positions)
            (let* ((ov-plist (get-text-property (+ annot-pos offset) 'annot))
                   (ov-type (plist-get ov-plist :type))
                   s)
              (when ov-plist
                (cond
                 ((equal ov-type 'highlight)
                  (annot-add `(,(+ annot-yank-start (plist-get ov-plist :beg))
                               . ,(+ annot-yank-start (plist-get ov-plist :end)))))
                 ((member ov-type '(text image))
                  (goto-char (+ offset annot-pos))
                  (when (setq s (plist-get ov-plist 'before-string))
                    (annot-add s)))))
              ;; Remove annot text property added by annot-add-annots-to-text.
              (remove-text-properties (+ annot-pos offset)
                                      (min (point-max) (1+ (+ annot-pos offset)))
                                      '(annot nil))))
          ;; Lastly, remove annot-exists and annot-positions at the start of
          ;; yank'ed text.
          (remove-text-properties offset
                                  (min (point-max) (1+ offset))
                                  '(annot-exists nil annot-positions nil))))))
  ad-return-value)


;;; annot's fuf support

(when (and annot-enable-fuf-support
           (featurep 'fuf))
  (defun annot-fuf-create-ellipsis-overlay (beg end)
    "Create an overlay that represents \"...\" for text from `beg'
  to `end'."
    (let ((ov (make-overlay beg end nil t nil)))
      (overlay-put ov 'evaporate t)
      (overlay-put ov :prev (buffer-substring-no-properties
                             (max (point-min) (- beg annot-subsequence-length)) beg))
      (overlay-put ov :next (buffer-substring-no-properties
                             end (min (point-max) (+ end annot-subsequence-length))))
      (overlay-put ov :beg beg)
      (overlay-put ov :end end)
      (overlay-put ov :type 'ellipsis)
      (overlay-put ov :modtime (float-time))
      (overlay-put ov 'invisible 'fuf) ;; core
      ;; This is merely a temporary solution. In particular, the following does
      ;; not take into account its indirect buffer. (TODO)
      (push ov annot-buffer-overlays)
      (annot-save-annotations)
      ov))
  (setq fuf-create-ellipsis-overlay-function 'annot-fuf-create-ellipsis-overlay)

  (defun annot-fuf-remove-ellipsis-overlay (beg end)
    "Remove an overlay that represents \"...\" for text from `beg'
  to `end'."
    (dolist (ov (overlays-in beg end))
      (when (eq (overlay-get ov 'invisible) 'fuf)
        (setq annot-buffer-overlays (delq ov annot-buffer-overlays))
        (delete-overlay ov)
        (annot-save-annotations)))
    (remove-overlays beg end 'invisible 'fuf))
  (setq fuf-remove-ellipsis-overlay-function 'annot-fuf-remove-ellipsis-overlay))


(provide 'annot)
;;; annot.el ends here
