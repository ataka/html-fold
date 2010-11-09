;;; html-fold.el --- Fold HTML elements.

;; Copyright (C) 2005, 2010  Masayuki Ataka

;; Author: Masayuki Ataka <masayuki.ataka@gmail.com>
;; Maintainer: Masayuki Ataka <masayuki.ataka@gmail.com>
;; Created: 2005-05-10
;; Version: alpha3 2005-07-15
;; Keywords: html, wp

;; This file is not part of Emacs.

;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This file provides support for hiding and unhiding HTML elements.
;;
;; Acknowledgements:
;;
;; html-fold is depeloped based on tex-fold.el in AUCTeX, written by 
;; Ralf Angeli <angeli@iwi.uni-sb.de>.
;;
;; Caveats:
;;
;; The display string of content which should display part of itself
;; is made by copying the text from the buffer together with its text
;; properties.  If fontification has not happened when this is done
;; (e.g. because of lazy or just-in-time font locking) the intended
;; fontification will not show up.  Maybe this could be improved by
;; using some sort of "lazy folding" or refreshing the window upon
;; scrolling.  As a workaround fontification of the whole buffer
;; currently is forced before folding it.

;;; Code:

(when (featurep 'xemacs)
  (require 'overlay))

(defgroup html-fold nil
  "Fold html elements."
  :group 'wp)

(defcustom html-fold-block-list
  '("script" "style" "table")
  "List of block elements to fold."
  :type '(repeat :tag "Block Elements" (string))
  :group 'html-fold)

(defcustom html-fold-block-format "[%s]"
  "Format string of block elements
%s will be replaced by the element name."
  :type 'string
  :group 'html-fold)

(defcustom html-fold-inline-list
  '(("[a:" ("a"))
    ("[c:" ("code"))
    ("[k:" ("kbd"))
    ("[v:" ("var"))
    ("[s:" ("samp"))
    ("[ab:" ("abbr" "acronym"))
    ("[lab:" ("label"))
    ("[opt:" ("option")))
  "List of inline elements to fold."
  :type '(repeat (group (string :tag "Display String")
			(repeat :tag "Inline Elements" (string))))
  :group 'html-fold)

(defcustom html-fold-inline-close-string "]"
  "String for close tag of inline element"
  :type 'string
  :group 'html-fold)

(defcustom html-fold-unfold-around-mark t
  "Unfold text around the mark, if active."
  :type 'boolean
  :group 'html-fold)

(defcustom html-fold-help-echo-max-length 70
  "Maximum length of help echo message for folded overlays.
Set it to zero in order to disable help echos."
  :type 'integer
  :group 'html-fold)

(defcustom html-fold-force-fontify t
  "Force the buffer to be fully fontified by folding it."
  :type 'boolean
  :group 'html-fold)

(defface html-fold-folded-face
  '((((class color) (background light))
     (:foreground "SlateBlue"))
    (((class color) (background dark))
     (:foreground "SlateBlue1"))
    (((class grayscale) (background light))
     (:foreground "DimGray"))
    (((class grayscale) (background dark))
     (:foreground "LightGray"))
    (t (:slant italic)))
  "Face for the display string of folded content."
  :group 'html-fold)

(defvar html-fold-folded-face 'html-fold-folded-face
  "Face for the display string of folded content.")

(defface html-fold-unfolded-face
  '((((class color) (background light))
     (:background "#f2f0fd"))
    (((class color) (background dark))
     (:background "#38405d"))
    (((class grayscale) (background light))
     (:background "LightGray"))
    (((class grayscale) (background dark))
     (:background "DimGray"))
    (t (:inverse-video t)))
  "Face for folded content when it is temporarily opened."
  :group 'html-fold)

(defvar html-fold-unfolded-face 'html-fold-unfolded-face
  "Face for folded content when it is temporarily opened.")

(defvar html-fold-ellipsis "..."
  "String used as display string for overlays instead of a zero-length string.")

(defvar html-fold-open-spots nil)
(make-variable-buffer-local 'html-fold-open-spots)

(defvar html-fold-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-o\C-o" 'html-fold-dwim)
    (define-key map "\C-c\C-o\C-b" 'html-fold-buffer)
    (define-key map "\C-c\C-o\C-r" 'html-fold-region)
    (define-key map "\C-c\C-o\C-p" 'html-fold-paragraph)
    (define-key map "\C-c\C-o\C-m" 'html-fold-inline)
    (define-key map "\C-c\C-o\C-e" 'html-fold-block)
    (define-key map "\C-c\C-ob"    'html-fold-clearout-buffer)
    (define-key map "\C-c\C-or"    'html-fold-clearout-region)
    (define-key map "\C-c\C-op"    'html-fold-clearout-paragraph)
    (define-key map "\C-c\C-oi"    'html-fold-clearout-item)
    map))

(defvar html-overlay-priority-step 16
  "Numerical difference of priorities between nested overlays.
The step should be big enough to allow setting a priority for new
overlays between two existing ones.")

;;; Special support for XEmacs

(when (featurep 'xemacs)

  (defun html-active-mark ()
    (and zmacs-regions (mark)))

  (defun html-overlay-prioritize (start end)
    "Calculate a priority for an overlay extending from START to END.
The calculated priority is lower than the minimum of priorities
of surrounding overlays and higher than the maximum of enclosed
overlays."
    (let (inner-priority outer-priority
			 (prios (cons nil nil)))
      (map-extents
       #'(lambda (ov prios)
	   (and
	    (eq (extent-property ov 'category) 'html-fold)
	    (setcar prios
		    (max (or (car prios) 0)
			 (extent-property ov 'priority))))
	   nil)
       nil start end prios 'start-and-end-in-region 'priority)
      (map-extents
       #'(lambda (ov prios)
	   (and
	    (eq (extent-property ov 'category) 'html-fold)
	    (setcdr prios
		    (min (or (cdr prios) most-positive-fixnum)
			 (extent-property ov 'priority))))
	   nil)
       nil start end prios
       '(start-and-end-in-region negate-in-region) 'priority)
      (setq inner-priority (car prios) outer-priority (cdr prios))
      (cond ((and inner-priority (not outer-priority))
	     (+ inner-priority html-overlay-priority-step))
	    ((and (not inner-priority) outer-priority)
	     (/ outer-priority 2))
	    ((and inner-priority outer-priority)
	     (+ (/ (- outer-priority inner-priority) 2) inner-priority))
	    (t html-overlay-priority-step))))

)

;;; Special support for GNU Emacs

(unless (featurep 'xemacs)

  (defun html-active-mark ()
    (and transient-mark-mode mark-active))

  (defun html-overlay-prioritize (start end)
    "Calculate a priority for an overlay extending from START to END.
The calculated priority is lower than the minimum of priorities
of surrounding overlays and higher than the maximum of enclosed
overlays."
    (let (outer-priority inner-priority ov-priority)
      (dolist (ov (overlays-in start end))
	(when (eq (overlay-get ov 'category) 'html-fold)
	  (setq ov-priority (overlay-get ov 'priority))
	  (if (>= (overlay-start ov) start)
	      (setq inner-priority (max ov-priority (or inner-priority
							ov-priority)))
	    (setq outer-priority (min ov-priority (or outer-priority
						      ov-priority))))))
      (cond ((and inner-priority (not outer-priority))
	     (+ inner-priority html-overlay-priority-step))
	    ((and (not inner-priority) outer-priority)
	     (/ outer-priority 2))
	    ((and inner-priority outer-priority)
	     (+ (/ (- outer-priority inner-priority) 2) inner-priority))
	    (t html-overlay-priority-step))))

)

;;; Folding

(defun html-fold-dwim ()
  "Hide or show elements according to the current context.
If there is folded content, unfold it.  If there is a marked
region, fold all configured content in this region.  If there is
no folded content but a element, fold it."
  (interactive)
  (cond ((html-fold-clearout-item))
	((html-active-mark) (html-fold-region (mark) (point)))
	((html-fold-item 'inline))
	((html-fold-item 'block))))

(defun html-fold-buffer ()
  "Hide all configured elements in the current buffer.
The relevant elements are specified in the variable
`html-fold-block-list', `html-fold-inline-list' and
`html-fold-custom-element-list'."
  (interactive)
  (html-fold-clearout-region (point-min) (point-max))
  (when (and html-fold-force-fontify
	     (boundp 'jit-lock-mode)
	     jit-lock-mode
	     (fboundp 'jit-lock-fontify-now))
    ;; We force fontification here only because it should rarely be
    ;; needed for the other folding commands.
    (jit-lock-fontify-now))
  (html-fold-region (point-min) (point-max)))

(defun html-fold-paragraph ()
  "Hide all configured elements in the current paragraph.
The relevant elements are specified in the variable
`html-fold-block-list', `html-fold-inline-list' and
`html-fold-custom-element-list'."
  (interactive)
  (save-excursion
    (let ((end (progn (html-beginning-of-paragraph) (point)))
	  (start (progn (html-end-of-paragraph) (point))))
      (html-fold-clearout-region start end)
      (html-fold-region start end))))

(defun html-fold-region (start end &optional type)
  "Fold all items in region starting at position START and ending at END.
If optional parameter TYPE is given, fold only items of the
specified type.  TYPE can be one of the symbols 'block for
block elements or 'inline for inline elements."
  (interactive "r")
  (if (null type)
      (progn
	(html-fold-region start end 'block)
	(html-fold-region start end 'inline))
    (when (or (eq type 'block) (eq type 'inline))
      (save-excursion
	(let (fold-list item-list regexp)
	  (if (eq type 'block)
	      (setq fold-list (mapcar (lambda (i) (list i (format html-fold-block-format i)))
				      html-fold-block-list)
		    item-list html-fold-block-list)
	    (dolist (item html-fold-inline-list)
	      (dolist (i (cadr item))
		(add-to-list 'fold-list (list i (car item)))
		(add-to-list 'item-list i))))
	  (setq regexp (concat "<" (regexp-opt item-list t) "\\(?:\\s-\\|>\\)"))
	  (save-restriction
	    (narrow-to-region start end)
	    ;; Start from the bottom so that it is easier to prioritize
	    ;; nested macros.
	    (goto-char (point-max))
	    (let ((case-fold-search nil))
	      (while (re-search-backward regexp nil t)
		(let* ((item-start (match-beginning 0))
		       (element (match-string 1))
		       (display-string-spec (cadr (assoc element fold-list)))
		       (item-end (html-fold-item-end item-start type element))
		       (ov (html-fold-make-overlay item-start item-end type
						   display-string-spec)))
		  (html-fold-hide-item ov)))
	      (when (eq type 'inline)
		(setq regexp (concat "</" (regexp-opt item-list t) ">"))
		(goto-char (point-max))
		(while (re-search-backward regexp nil t)
		  (let ((ov (html-fold-make-overlay (match-beginning 0) (match-end 0)
						    type html-fold-inline-close-string)))
		    (html-fold-hide-item ov)))))))))))

(defun html-fold-inline ()
  "Hide the inline elements on which point currently is located."
  (interactive)
  (unless (html-fold-item 'inline)
    (message "No inline element found.")))

(defun html-fold-block ()
  "Hide the block elements on which point currently is located."
  (interactive)
  (unless (html-fold-item 'block)
    (message "No block found.")))

(defun html-fold-item (type)
  "Hide the item on which point currently is located.
TYPE specifies the type of item and can be one of the symbols
'block or 'inline.
Return non-nil if an item was found and folded, nil otherwise."
  (let ((item-start (save-excursion (html-find-element-start))))
    (when item-start
      (let* ((item-name (save-excursion
			  (goto-char item-start)
			  (looking-at "<\\([^> \t]+\\)")
			  (if (fboundp 'match-string-no-properties)
			      (match-string-no-properties 1)
			    (match-string 1))))
	     (fold-list (if (eq type 'block)
			    html-fold-block-list
			  html-fold-inline-list))
	     fold-item
	     (display-string-spec (if (eq type 'block)
				      (format html-fold-block-format item-name)
				    (catch 'found
				      (while fold-list
					(setq fold-item (car fold-list))
					(setq fold-list (cdr fold-list))
					(when (member item-name (cadr fold-item))
					  (throw 'found (car fold-item)))))))
	     (item-end (html-fold-item-end item-start type item-name))
	     (ov (when display-string-spec
		   (html-fold-make-overlay item-start item-end type
					   display-string-spec))))
	(when display-string-spec
	  (html-fold-hide-item ov)
	  (when (eq type 'inline)
	    (save-excursion
	      (goto-char item-start)
	      (let ((case-fold-search nil)
		    (regexp (concat "</" item-name ">")))
		(re-search-forward regexp nil t)
		(let ((ov (html-fold-make-overlay (match-beginning 0) (match-end 0)
						  type html-fold-inline-close-string)))
		  (html-fold-hide-item ov))))))))))

;;; Utilities

(defun html-fold-make-overlay (ov-start ov-end type display-string-spec)
  "Make a html-fold overlay extending from OV-START to OV-END.
TYPE is a symbol which is used to describe the content to hide
and may be 'inline and 'block.
DISPLAY-STRING-SPEC is the original specification of the display
string in the variables `html-fold-macro-spec-list' or
`html-fold-env-spec-list' and may be a string or an integer."
  ;; Calculate priority before the overlay is instantiated.  We don't
  ;; want `html-overlay-prioritize' to pick up a non-prioritized one.
  (let ((priority (html-overlay-prioritize ov-start ov-end))
	(ov (make-overlay ov-start ov-end (current-buffer) t nil)))
    (overlay-put ov 'category 'html-fold)
    (overlay-put ov 'priority priority)
    (overlay-put ov 'evaporate t)
    (overlay-put ov 'html-fold-type type)
    (overlay-put ov 'html-fold-display-string-spec display-string-spec)
    ov))

(defun html-fold-item-end (start type &optional element)
  "Return the end of an item of type TYPE starting at START.
TYPE can be either 'block or 'inline."
  (save-excursion
    (goto-char start)
    (if (eq type 'inline)
	(re-search-forward "[^$]>") ; For blog template (like Blogger, MovableType, etc...)
      (if element
	  (search-forward (concat "</" element ">"))
	(search-forward "</")
	(search-forward ">")))))

(defun html-fold-overfull-p (ov-start ov-end display-string)
  "Return t if an overfull line will result after adding an overlay.
The overlay extends from OV-START to OV-END and will display the
string DISPLAY-STRING."
  (and (not (featurep 'xemacs)) ; Linebreaks in glyphs don't
				; work in XEmacs anyway.
       (save-excursion
	 (goto-char ov-end)
	 (search-backward "\n" ov-start t))
       (not (string-match "\n" display-string))
       (> (+ (- ov-start
		(save-excursion
		  (goto-char ov-start)
		  (line-beginning-position)))
	     (length display-string)
	     (- (save-excursion
		  (goto-char ov-end)
		  (line-end-position))
		ov-end))
	  (current-fill-column))))

(defun html-fold-inline-nth-arg (n inline-start &optional inline-end)
  "Return a property list of the argument number N of a inline.
The start of the inline to examine is given by INLINE-START, its
end optionally by INLINE-END.

The first item in the returned list is the string specified in
the argument, the second item may be a face if the argument
string was fontified.  In Emacs the string holds text properties
as well, so the second item is always nil.  In XEmacs the string
does not enclose any faces, so these are given in the second item
of the resulting list."
  (save-excursion
    (let ((inline-end (or inline-end
			 (save-excursion (goto-char inline-start)
					 (html-find-inline-end))))
	  content-start content-end)
      (goto-char inline-start)
      (if (condition-case nil
	      (progn
		(while (> n 0)
		  (skip-chars-forward "^{" inline-end)
		  (when (not (looking-at "{")) (error nil))
		  (setq content-start (progn
					(skip-chars-forward "{ \t")
					(point)))
		  (goto-char (html-find-closing-brace))
		  (setq content-end (save-excursion
				      (backward-char)
				      (skip-chars-backward " \t")
				      (point)))
		  (setq n (1- n)))
		t)
	    (error nil))
	  (list (html-fold-buffer-substring content-start content-end)
		(when (and (featurep 'xemacs)
			   (extent-at content-start))
		  ;; A glyph in XEmacs does not seem to be able to hold more
		  ;; than one face, so we just use the first one we get.
		  (car (extent-property (extent-at content-start) 'face))))
	nil))))

(defun html-fold-buffer-substring (start end)
  "Return the contents of buffer from START to END as a string.
Like `buffer-substring' but copy overlay display strings as well."
  ;; Swap values of `start' and `end' if necessary.
  (when (> start end) (let ((tmp start)) (setq start end end tmp)))
  (let ((overlays (overlays-in start end))
	result)
    ;; Get rid of overlays not under our control or not completely
    ;; inside the specified region.
    (dolist (ov overlays)
      (when (or (not (eq (overlay-get ov 'category) 'html-fold))
		(< (overlay-start ov) start)
		(> (overlay-end ov) end))
	(setq overlays (remove ov overlays))))
    (if (null overlays)
	(buffer-substring start end)
      ;; Sort list according to ascending starts.
      (setq overlays (sort (copy-sequence overlays)
			   (lambda (a b)
			     (< (overlay-start a) (overlay-start b)))))
      ;; Get the string from the start of the region up to the first overlay.
      (setq result (buffer-substring start (overlay-start (car overlays))))
      (let (ov)
	(while overlays
	  (setq ov (car overlays)
		overlays (cdr overlays))
	  ;; Add the display string of the overlay.
	  (setq result (concat result (overlay-get ov 'display)))
	  ;; Remove overlays contained in the current one.
	  (dolist (elt overlays)
	    (when (< (overlay-start elt) (overlay-end ov))
	      (setq overlays (remove elt overlays))))
	  ;; Add the string from the end of the current overlay up to
	  ;; the next overlay or the end of the specified region.
	  (setq result (concat result (buffer-substring (overlay-end ov)
							(if overlays
							    (overlay-start
							     (car overlays))
							  end))))))
      result)))

(defun html-fold-make-help-echo (start end)
  "Return a string to be used as the help echo of folded overlays.
The text between START and END will be used for this but cropped
to the length defined by `html-fold-help-echo-max-length'.  Line
breaks will be replaced by spaces."
  (let* ((spill (+ start html-fold-help-echo-max-length))
	 (lines (split-string (buffer-substring start (min end spill)) "\n"))
	 (result (pop lines)))
    (dolist (line lines)
      ;; Strip leading whitespace
      (when (string-match "^[ \t]+" line)
	(setq line (replace-match "" nil nil line)))
      ;; Strip trailing whitespace
      (when (string-match "[ \t]+$" line)
	(setq line (replace-match "" nil nil line)))
      (setq result (concat result " " line)))
    (when (> end spill) (setq result (concat result "...")))
    result))

(defun html-fold-update-at-point ()
  "Update all html-fold overlays at point displaying computed content."
  (let (overlays)
    ;; Get all overlays at point under our control.
    (dolist (ov (overlays-at (point)))
      (when (and (eq (overlay-get ov 'category) 'html-fold)
		 (numberp (overlay-get ov 'html-fold-display-string-spec)))
	(add-to-list 'overlays ov)))
    (when overlays
      ;; Sort list according to descending starts.
      (setq overlays (sort (copy-sequence overlays)
			   (lambda (a b)
			     (> (overlay-start a) (overlay-start b)))))
      (dolist (ov overlays)
	(html-fold-hide-item ov)))))


;;; Removal

(defun html-fold-clearout-buffer ()
  "Permanently show all elements in the buffer."
  (interactive)
  (html-fold-clearout-region (point-min) (point-max)))

(defun html-fold-clearout-paragraph ()
  "Permanently show all elements in the paragraph point is located in."
  (interactive)
  (save-excursion
    (let ((end (progn (html-end-of-paragraph) (point)))
	  (start (progn (html-beginning-of-paragraph) (point))))
      (html-fold-clearout-region start end))))

(defun html-fold-clearout-region (start end)
  "Permanently show all elelemnts in region starting at START and ending at END."
  (interactive "r")
  (let ((overlays (overlays-in start end)))
    (html-fold-remove-overlays overlays)))

(defun html-fold-clearout-item ()
  "Permanently show the elements on which point currently is located."
  (interactive)
  (let ((overlays (overlays-at (point))))
    (html-fold-remove-overlays overlays)))

(defun html-fold-remove-overlays (overlays)
  "Remove all overlays set by html-fold in OVERLAYS.
Return non-nil if a removal happened, nil otherwise."
  (let (found)
    (while overlays
      (when (eq (overlay-get (car overlays) 'category) 'html-fold)
	(delete-overlay (car overlays))
	(setq found t))
      (setq overlays (cdr overlays)))
    found))


;;; Toggling

(defun html-fold-hide-item (ov)
  "Hide a single element.
That means, put respective properties onto overlay OV."
  (let* ((ov-start (overlay-start ov))
	 (ov-end (overlay-end ov))
	 (spec (overlay-get ov 'html-fold-display-string-spec))
	 (computed (if (stringp spec)
		       spec
		     (or (html-fold-inline-nth-arg spec ov-start ov-end)
			 "[Error: No content found]")))
	 (display-string (if (listp computed) (car computed) computed))
	 (face (when (listp computed) (cadr computed))))
    ;; Cater for zero-length display strings.
    (when (string= display-string "") (setq display-string html-fold-ellipsis))
    ;; Add a linebreak to the display string and adjust the overlay end
    ;; in case of an overfull line.
    (when (html-fold-overfull-p ov-start ov-end display-string)
      (setq display-string (concat display-string "\n"))
      (move-overlay ov ov-start (save-excursion
				  (goto-char ov-end)
				  (skip-chars-forward " \t")
				  (point))))
    (overlay-put ov 'mouse-face 'highlight)
    (overlay-put ov 'display display-string)
    (if (featurep 'xemacs)
	(let ((glyph (make-glyph (if (listp display-string)
				     (car display-string)
				   display-string))))
	  (overlay-put ov 'invisible t)
	  (when font-lock-mode
	    (if face
		(set-glyph-property glyph 'face face)
	      (set-glyph-property glyph 'face html-fold-folded-face)))
	  (set-extent-property ov 'end-glyph glyph))
      (when font-lock-mode
	(overlay-put ov 'face html-fold-folded-face))
      (unless (zerop html-fold-help-echo-max-length)
	(overlay-put ov 'help-echo (html-fold-make-help-echo
				    (overlay-start ov) (overlay-end ov)))))))

(defun html-fold-show-item (ov)
  "Show a single element.
Remove the respective properties from the overlay OV."
  (overlay-put ov 'mouse-face nil)
  (if (featurep 'xemacs)
      (progn
	(set-extent-property ov 'end-glyph nil)
	(overlay-put ov 'invisible nil))
    (overlay-put ov 'display nil)
    (overlay-put ov 'help-echo nil)
    (when font-lock-mode
      (overlay-put ov 'face html-fold-unfolded-face))))

;; Copy and adaption of `reveal-post-command' from reveal.el in GNU
;; Emacs on 2004-07-04.
(defun html-fold-post-command ()
  ;; `with-local-quit' is not supported in XEmacs.
  (condition-case nil
      (let ((inhibit-quit nil))
	(condition-case err
	    (let* ((spots (html-fold-partition-list
			   (lambda (x)
			     ;; We refresh any spot in the current
			     ;; window as well as any spots associated
			     ;; with a dead window or a window which
			     ;; does not show this buffer any more.
			     (or (eq (car x) (selected-window))
				 (not (window-live-p (car x)))
				 (not (eq (window-buffer (car x))
					  (current-buffer)))))
			   html-fold-open-spots))
		   (old-ols (mapcar 'cdr (car spots))))
	      (setq html-fold-open-spots (cdr spots))
	      (when (or (and (boundp 'disable-point-adjustment)
			     disable-point-adjustment)
			(and (boundp 'global-disable-point-adjustment)
			     global-disable-point-adjustment)
			;; See preview.el on how to make this configurable.
			(memq this-command (list (key-binding [left])
						 (key-binding [right])
						 'forward-char
						 'backward-char
						 'mouse-set-point)))
		;; Open new overlays.
		(dolist (ol (nconc (when (and html-fold-unfold-around-mark
					      (boundp 'mark-active)
					      mark-active)
				     (overlays-at (mark)))
				   (overlays-at (point))))
		  (when (eq (overlay-get ol 'category) 'html-fold)
		    (push (cons (selected-window) ol) html-fold-open-spots)
		    (setq old-ols (delq ol old-ols))
		    (html-fold-show-item ol))))
	      ;; Close old overlays.
	      (dolist (ol old-ols)
		(when (and (eq (current-buffer) (overlay-buffer ol))
			   (not (rassq ol html-fold-open-spots))
			   (or (not (featurep 'xemacs))
			       (and (featurep 'xemacs)
				    (not (extent-detached-p ol)))))
		  (if (and (>= (point) (overlay-start ol))
			   (<= (point) (overlay-end ol)))
		      ;; Still near the overlay: keep it open.
		      (push (cons (selected-window) ol) html-fold-open-spots)
		    ;; Really close it.
		    (html-fold-hide-item ol)))))
	  (error (message "html-fold: %s" err))))
    (quit (setq quit-flag t))))


;;; Misc

;; Copy and adaption of `cvs-partition' from pcvs-util.el in GNU Emacs
;; on 2004-07-05 to make html-fold.el mainly self-contained.
(defun html-fold-partition-list (p l)
  "Partition a list L into two lists based on predicate P.
The function returns a `cons' cell where the `car' contains
elements of L for which P is true while the `cdr' contains
the other elements.  The ordering among elements is maintained."
  (let (car cdr)
    (dolist (x l)
      (if (funcall p x) (push x car) (push x cdr)))
    (cons (nreverse car) (nreverse cdr))))


;;; The mode

;;; This autoload cookie had to be changed because of XEmacs.  This is
;;; very dissatisfactory, because we now don't have the full doc string
;;; available to tell people what to expect when using this mode
;;; before loading it.

;;;###autoload (autoload 'html-fold-mode "html-fold" "Minor mode for hiding and revealing elements.")
(define-minor-mode html-fold-mode
  "Minor mode for hiding and revealing elements.

Called interactively, with no prefix argument, toggle the mode.
With universal prefix ARG (or if ARG is nil) turn mode on.
With zero or negative ARG turn mode off."
  nil nil html-fold-keymap
  (if html-fold-mode
      (progn
	(set (make-local-variable 'search-invisible) t)
	(add-hook 'post-command-hook 'html-fold-post-command nil t))
    (kill-local-variable 'search-invisible)
    (remove-hook 'post-command-hook 'html-fold-post-command t)
    (html-fold-clearout-buffer))
  (if html-fold-mode
      (setq mode-name (concat mode-name (when html-fold-mode "/F")))
    (setq mode-name (substring mode-name 0 -2)))
  (set-buffer-modified-p (buffer-modified-p)))

(defun html-beginning-of-paragraph ()
  (re-search-backward "<p\\(?:\\s-\\|>\\)"))
(defun html-end-of-paragraph ()
  (re-search-forward "</p>"))
(defun html-find-element-start ()
  (re-search-backward "<[A-Za-z]"))

(provide 'html-fold)

;;; html-fold.el ends here
