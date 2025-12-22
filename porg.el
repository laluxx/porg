;;; porg.el --- Bring org-mode features to any prog mode  -*- lexical-binding: t; -*-

;; Author: Laluxx
;; Version: 0.0.8
;; Package-Requires: ((emacs "26.1"))
;; Keywords: folding, convenience, org-mode
;; URL: https://github.com/laluxx/porg

;; This file is not part of GNU Emacs.

;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;

;;; Commentary:

;; Porg brings Org-mode's structural editing to any programming language
;; by treating specially-formatted comments as hierarchical headings.

;; Features:
;;   - Visual bullets (◉ ○ ●) replace comment markers
;;   - Optional block backgrounds between sections
;;   - TAB/S-TAB cycling for folding (like Org-mode)
;;   - Navigate with C-c C-n/C-c C-p, search with C-c C-j
;;   - TODO/DONE keywords with M-S-left/M-S-right
;;   - Promote/demote with M-left/M-right
;;   - Works with any comment syntax (automatically detected)

;; Usage:
;;   (add-hook 'prog-mode-hook #'porg-mode)

;; Example:
;;   /// Main Section          <- Level 1 heading
;;   //// TODO Subsection      <- Level 2 heading
;;   ///// Implementation      <- Level 3 heading

;; The number of comment characters determines heading level.
;; Default base level is 3 (configurable via `porg-base-level').

;;; Code:

(require 'org)


(defgroup porg nil
  "Org-like features for programming modes."
  :group 'faces
  :group 'convenience)


;;;; Bullets Configuration

(defcustom porg-bullet-list
  '("◉" "○" "●" "●" "•")
  "List of bullets used for comment headings.
It can contain any number of symbols, which will be repeated."
  :group 'porg
  :type '(repeat (string :tag "Bullet character")))

(defcustom porg-base-level 3
  "Number of comment characters for the first bullet level.
When set to 3, /// becomes level 1 bullet in C, ### in Python, etc.
Must be a positive integer."
  :group 'porg
  :type 'integer
  :set (lambda (symbol value)
         (set-default symbol value)
         (dolist (buf (buffer-list))
           (with-current-buffer buf
             (when (and (boundp 'porg-mode) porg-mode)
               (porg--fontify-buffer)
               (porg--update-overlays))))))

(defcustom porg-todo-keywords '("TODO" "DONE")
  "List of todo keywords to cycle through.
An empty string represents no todo keyword."
  :group 'porg
  :type '(repeat string))

(defcustom porg-face-name nil
  "Face used for bullets in comment headings.
If set to the name of a face, that face is used.
Otherwise uses org-level-N faces."
  :group 'porg
  :type 'symbol)

(defcustom porg-blank-before-new-entry '((heading . auto))
  "Should a blank line be inserted before a new comment heading?
The value is an alist, with `heading' as key and a boolean or
`auto' as value."
  :group 'porg
  :type '(alist :key-type (choice (const heading))
                :value-type (choice (const auto) (const t) (const nil))))

;;;; Blocks Configuration

(defface porg-block
  '((t :inherit org-block :extend t))
  "Face used for block backgrounds between comment sections.
Inherits from org-block to match your theme's block styling."
  :group 'porg)

(defface porg-todo
  '((t :inherit org-todo))
  "Face for TODO keywords in porg headings."
  :group 'porg)

(defface porg-done
  '((t :inherit org-done))
  "Face for DONE keywords in porg headings."
  :group 'porg)


(defface porg-special-keyword
  '((t :inherit org-special-keyword))
  "Face used for special keywords."
  :group 'porg)

(defface porg-date
  '((t :inherit org-date))
  "Face for date/time stamps."
  :group 'porg)

(defcustom porg-blocks-padding 1
  "Number of blank lines to leave at the end of blocks."
  :type 'integer
  :group 'porg
  :set (lambda (symbol value)
         (set-default symbol value)
         (dolist (buf (buffer-list))
           (with-current-buffer buf
             (when (and (boundp 'porg-mode) porg-mode)
               (porg--update-overlays))))))

(defcustom porg-blocks-files nil
  "List of file names where block backgrounds should be shown.
If nil, blocks are shown in all buffers.
If set to a list of file names (e.g., \\='(\"init.el\" \"config.el\")),
blocks will only be shown in buffers visiting files with those names.
File name matching is case-sensitive and matches the base name only."
  :type '(choice (const :tag "All files" nil)
                 (repeat :tag "Specific files" string))
  :group 'porg
  :set (lambda (symbol value)
         (set-default symbol value)
         (dolist (buf (buffer-list))
           (with-current-buffer buf
             (when (and (boundp 'porg-mode) porg-mode)
               (porg--update-overlays))))))

;;;; Internal Variables

(defvar porg-bullet-map (make-sparse-keymap))

(defvar-local porg--overlays nil
  "List of overlays used for block backgrounds.")
(defvar-local porg--comment-start nil
  "The comment start string for the current buffer.")
(defvar-local porg--comment-char nil
  "The single character used for comments in the current buffer.")

;;;; Comment Detection Functions

(defun porg--detect-comment-syntax ()
  "Detect and cache the comment syntax for the current buffer.
Returns the comment start string or nil if none found."
  (or porg--comment-start
      (setq porg--comment-start
            (cond
             ;; Use comment-start if available
             ((and comment-start (not (string-empty-p (string-trim comment-start))))
              (string-trim comment-start))
             ;; Fallback to parsing syntax table
             (t
              (let ((syntax-chars '()))
                (map-char-table
                 (lambda (key value)
                   (when (and (characterp key)
                              (or (eq (char-syntax key) ?<)
                                  (and (consp value)
                                       (memq (car value) '(11 12)))))
                     (push key syntax-chars)))
                 (syntax-table))
                (when syntax-chars
                  (char-to-string (car syntax-chars)))))))))

(defun porg--get-comment-char ()
  "Get the single character used for comments.
Returns the first character of the comment syntax."
  (or porg--comment-char
      (setq porg--comment-char
            (let ((comment-str (porg--detect-comment-syntax)))
              (when comment-str
                (string-to-char comment-str))))))

(defun porg--comment-line-p ()
  "Return non-nil if current line starts with a comment."
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (let ((comment-char (porg--get-comment-char)))
      (and comment-char
           (eq (char-after) comment-char)))))

(defun porg--comment-count ()
  "Return the number of comment characters at the beginning of the current line."
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (let ((comment-char (porg--get-comment-char))
          (count 0))
      (when comment-char
        (while (eq (char-after) comment-char)
          (setq count (1+ count))
          (forward-char)))
      (if (> count 0) count nil))))

;;;; Utility Functions

(defun porg--logical-level (comment-count)
  "Convert COMMENT-COUNT to logical level based on `porg-base-level'.
Returns nil if the comment count is less than base level."
  (when (and comment-count (>= comment-count porg-base-level))
    (1+ (- comment-count porg-base-level))))

(defun porg-current-level ()
  "Return the logical level of the current comment line.
Returns nil if not on a heading."
  (save-excursion
    (beginning-of-line)
    (let ((comment-count (porg--comment-count)))
      (porg--logical-level comment-count))))

(defun porg--on-heading-p ()
  "Return non-nil if point is on a heading line."
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (let ((count (porg--comment-count)))
      (and count (>= count porg-base-level)))))

(defun porg--should-show-blocks-p ()
  "Return non-nil if blocks should be shown in the current buffer."
  (or (null porg-blocks-files)
      (and buffer-file-name
           (member (file-name-nondirectory buffer-file-name)
                   porg-blocks-files))))

(defun porg--org-string-nw-p (s)
  "Return S if S is a string containing a non-whitespace character."
  (and (stringp s)
       (string-match-p "[^ \r\t\n]" s)
       s))

;;;; Bullet Functions

(defun porg-get-face (level)
  "Get the appropriate face for the given comment LEVEL."
  (if (facep porg-face-name)
      porg-face-name
    (intern (format "org-level-%d" (1+ (mod (1- level) 8))))))

(defun porg-level-char (level)
  "Return the desired bullet for the given comment LEVEL."
  (string-to-char
   (nth (mod (1- level) (length porg-bullet-list))
        porg-bullet-list)))

(defun porg--make-heading-regexp ()
  "Create a regexp to match comment headings in the current buffer."
  (let ((comment-char (porg--get-comment-char)))
    (when comment-char
      (format "^[ \t]*%s\\{%d,\\}.*$"
              (regexp-quote (char-to-string comment-char))
              porg-base-level))))

(defvar-local porg--keywords nil
  "Font-lock keywords for the current buffer.")


(defun porg--setup-keywords ()
  "Setup font-lock keywords based on current buffer's comment syntax."
  (let ((regexp (porg--make-heading-regexp))
        (comment-char (porg--get-comment-char)))
    (when (and regexp comment-char)
      (setq porg--keywords
            `(;; CLOSED timestamp line
              (,(format "^[ \t]*%s\\{2\\}CLOSED:.*$"
                        (regexp-quote (char-to-string comment-char)))
               (0 (let* ((bol (line-beginning-position))
                         (eol (line-end-position))
                         (indent-end (save-excursion
                                      (beginning-of-line)
                                      (skip-chars-forward " \t")
                                      (point))))
                    ;; Hide the two comment characters
                    (put-text-property indent-end (+ indent-end 2) 'invisible t)
                    ;; Find and fontify CLOSED:
                    (save-excursion
                      (goto-char (+ indent-end 2))
                      (when (looking-at "CLOSED:")
                        (put-text-property (point) (match-end 0) 'face 'porg-special-keyword))
                      ;; Find and fontify the timestamp
                      (when (re-search-forward "\\[\\([^]]+\\)\\]" eol t)
                        (put-text-property (match-beginning 0) (match-end 0) 'face 'porg-date)))
                    nil)))
              ;; Main heading fontification
              (,regexp
               (0 (let* ((comment-count (porg--comment-count))
                         (level (porg--logical-level comment-count)))
                    (when level
                      (let* ((bol (line-beginning-position))
                             (eol (line-end-position))
                             (indent-end (save-excursion
                                          (beginning-of-line)
                                          (skip-chars-forward " \t")
                                          (point)))
                             (bullet-pos (1- level))
                             (char-to-replace (+ indent-end bullet-pos))
                             (text-start (+ indent-end comment-count)))
                        (remove-text-properties bol eol
                                                '(invisible nil display nil composition nil))
                        (when (> bullet-pos 0)
                          (put-text-property indent-end
                                             char-to-replace
                                             'invisible t)
                          (put-text-property indent-end
                                             char-to-replace
                                             'display (make-string bullet-pos ?\s)))
                        (compose-region char-to-replace
                                        (1+ char-to-replace)
                                        (porg-level-char level))
                        (when (> comment-count (1+ bullet-pos))
                          (put-text-property (1+ char-to-replace)
                                             (+ indent-end comment-count)
                                             'invisible t))
                        
                        ;; Check for TODO/DONE and apply appropriate face
                        (save-excursion
                          (goto-char text-start)
                          (skip-chars-forward " \t")
                          (let ((keyword-start (point)))
                            (skip-chars-forward "A-Z")
                            (let* ((keyword (buffer-substring-no-properties keyword-start (point)))
                                   (level-face (porg-get-face level))
                                   (base-face-height (or (plist-get (face-attribute level-face :inherit) :height)
                                                         (face-attribute level-face :height))))
                              (cond
                               ;; If it's DONE, apply done face to heading text (not bullet)
                               ((string= keyword "DONE")
                                ;; Apply level face to bullet area first
                                (put-text-property bol text-start 'face level-face)
                                ;; Apply done face with proper height to text
                                (put-text-property text-start eol 'face
                                                   (list :inherit 'porg-done
                                                         :height (face-attribute level-face :height))))
                               ;; If it's TODO, apply todo face only to the keyword
                               ((string= keyword "TODO")
                                (put-text-property bol eol 'face level-face)
                                (put-text-property keyword-start (point) 'face
                                                   (list :inherit 'porg-todo
                                                         :height (face-attribute level-face :height))))
                               ;; Otherwise just use the level face
                               (t
                                (put-text-property bol eol 'face level-face))))))
                        
                        (put-text-property bol
                                           (1+ text-start)
                                           'keymap
                                           porg-bullet-map)))
                    nil))))))))

(defun porg--after-change (beg _end _len)
  "Refontify the line when changes occur in the bullet area.
BEG is the start of the changed region."
    (save-excursion
      (goto-char beg)
      (let ((line-start (line-beginning-position))
            (line-end (line-end-position)))
        (when (and (>= beg line-start)
                   (<= beg (+ line-start 20)))
          (remove-text-properties line-start (1+ line-end)
                                  '(invisible nil display nil composition nil))
          (font-lock-flush line-start (1+ line-end)))
        (porg--update-overlays))))

(defun porg--fontify-buffer ()
  "Fontify the current buffer."
  (when font-lock-mode
    (save-restriction
      (widen)
      (font-lock-flush)
      (font-lock-ensure))))

;;;; Block Functions

(defun porg--clear-overlays ()
  "Remove all background overlays."
  (mapc #'delete-overlay porg--overlays)
  (setq porg--overlays nil))

(defun porg--find-heading-positions ()
  "Find all heading positions (lines with base-level or more comment chars)."
  (save-excursion
    (goto-char (point-min))
    (let (headings
          (comment-char (porg--get-comment-char)))
      (when comment-char
        (let ((regexp (format "^[ \t]*%s\\{%d,\\}"
                              (regexp-quote (char-to-string comment-char))
                              porg-base-level)))
          (while (re-search-forward regexp nil t)
            (push (line-beginning-position) headings))))
      (nreverse headings))))

(defun porg--find-last-content-line (start end)
  "Find the last line with content between START and END.
Returns the position of the last non-empty line, leaving padding."
  (save-excursion
    (goto-char end)
    (forward-line -1)
    (while (and (> (point) start)
                (looking-at "^[ \t]*$"))
      (forward-line -1))
    (forward-line porg-blocks-padding)
    (line-end-position)))

(defun porg--find-block-regions ()
  "Find all regions between headings that need backgrounds."
  (let ((headings (porg--find-heading-positions))
        (comment-char (porg--get-comment-char))
        blocks)
    (when headings
      (let ((prev-heading (car headings)))
        (dolist (heading (cdr headings))
          (let ((block-start (save-excursion
                               (goto-char prev-heading)
                               (forward-line 1)
                               ;; Skip CLOSED line if it exists
                               (when (and (not (eobp))
                                          (looking-at (format "^[ \t]*%s\\{2\\}CLOSED:"
                                                              (regexp-quote (char-to-string comment-char)))))
                                 (forward-line 1))
                               (line-beginning-position)))
                (block-end (save-excursion
                             (goto-char heading)
                             (forward-line -1)
                             (line-end-position))))
            (when (< block-start block-end)
              (let ((content-end (porg--find-last-content-line block-start block-end)))
                (when (< block-start content-end)
                  (push (cons block-start content-end) blocks)))))
          (setq prev-heading heading))
        (let ((block-start (save-excursion
                             (goto-char prev-heading)
                             (forward-line 1)
                             ;; Skip CLOSED line if it exists
                             (when (and (not (eobp))
                                        (looking-at (format "^[ \t]*%s\\{2\\}CLOSED:"
                                                            (regexp-quote (char-to-string comment-char)))))
                               (forward-line 1))
                             (line-beginning-position)))
              (block-end (point-max)))
          (when (< block-start block-end)
            (let ((content-end (porg--find-last-content-line block-start block-end)))
              (when (< block-start content-end)
                (push (cons block-start content-end) blocks))))))
    (nreverse blocks))))

(defun porg--find-current-block ()
  "Find the block region containing point."
  (let ((blocks (porg--find-block-regions))
        (point (point)))
    (catch 'found
      (dolist (block blocks)
        (when (and (>= point (car block))
                   (<= point (cdr block)))
          (throw 'found block)))
      nil)))

(defun porg--update-overlays ()
  "Update background overlays for all blocks between headings."
  (porg--clear-overlays)
  (when (porg--should-show-blocks-p)
    (dolist (block (porg--find-block-regions))
      (let* ((start (car block))
             (end (cdr block))
             (ov (make-overlay start end)))
        (overlay-put ov 'face 'porg-block)
        (overlay-put ov 'priority -50)
        (overlay-put ov 'porg-block t)
        (push ov porg--overlays)))))

;;;; Heading Manipulation

(defun porg--blank-before-heading-p (&optional _parent)
  "Non-nil when an empty line should precede a new comment heading here."
  (pcase (assq 'heading porg-blank-before-new-entry)
    (`(heading . auto)
     (save-excursion
       (let ((current-level (porg-current-level)))
         (when current-level
           (beginning-of-line)
           (while (and (not (bobp))
                       (progn (forward-line -1)
                              (not (porg-current-level)))))
           (cond ((bobp) nil)
                 ((porg-current-level)
                  (forward-line 1)
                  (looking-at "^[ \t]*$"))
                 (t nil))))))
    (`(heading . ,value) value)
    (_ nil)))

(defun porg-insert-heading (&optional arg _invisible-ok level)
  "Insert a new comment heading LEVEL with the same depth at point.
With ARG, respect content structure by finding the end of the current
section and inserting there."
  (interactive "P")
  (let* ((blank? (porg--blank-before-heading-p (equal arg '(16))))
         (current-level (porg-current-level))
         (logical-level (or (and level (if (wholenump level) level 1))
                           current-level 1))
         (num-comments (+ porg-base-level (1- logical-level)))
         (comment-char (porg--get-comment-char))
         (comment-str (make-string num-comments comment-char))
         (respect-content (member arg '((4) (16)))))
    (unless comment-char
      (user-error "No comment syntax detected for this buffer"))
    (cond
     (respect-content
      ;; Move to end of current section
      (when current-level
        (beginning-of-line)
        ;; Skip forward past all content until next heading of same or higher level
        (let ((target-level (or current-level 1)))
          (forward-line 1)
          (while (and (not (eobp))
                      (let ((line-level (porg-current-level)))
                        (or (not line-level)
                            (> line-level target-level))))
            (forward-line 1))
          ;; Now we're at the next heading or end of buffer
          (unless (eobp)
            (forward-line -1)
            (end-of-line))))
      (unless (bolp) (insert "\n"))
      (unless (and blank? (save-excursion (forward-line -1) (looking-at "^[ \t]*$")))
        (when blank? (insert "\n")))
      (insert comment-str " \n")
      (backward-char))
     ((porg-current-level)
      (cond ((bolp)
             (when blank? (save-excursion (insert "\n")))
             (save-excursion (insert comment-str " \n"))
             (end-of-line))
            (t
             (let ((split-text (delete-and-extract-region (point) (line-end-position))))
               (when blank? (insert "\n"))
               (insert "\n" comment-str " ")
               (when (porg--org-string-nw-p split-text) (insert split-text))))))
     ((bolp)
      (insert comment-str " ")
      (unless (and blank? (save-excursion (forward-line -1) (looking-at "^[ \t]*$")))
        (when blank?
          (save-excursion (beginning-of-line) (insert "\n") (forward-line -1)))))
     (t
      (let ((split-text (delete-and-extract-region (point) (line-end-position))))
        (when blank? (insert "\n"))
        (insert "\n" comment-str " ")
        (when (porg--org-string-nw-p split-text) (insert split-text)))))
    
    (font-lock-flush (line-beginning-position) (1+ (line-end-position)))
    (porg--update-overlays)))

(defun porg-insert-heading-respect-content (&optional invisible-ok)
  "Insert heading with respect for content structure.
This finds the end of the current section and inserts the new heading there.
When INVISIBLE-OK is non-nil, don't error on invisible headlines."
  (interactive)
  (porg-insert-heading '(4) invisible-ok))

(defun porg-insert-subheading ()
  "Insert a new subheading below the current heading."
  (interactive)
  (let ((current-level (porg-current-level)))
    (if current-level
        (progn
          (end-of-line)
          (insert "\n")
          (let* ((new-level (1+ current-level))
                 (num-comments (+ porg-base-level (1- new-level)))
                 (comment-char (porg--get-comment-char))
                 (comment-str (make-string num-comments comment-char)))
            (insert comment-str " ")
            (font-lock-flush (line-beginning-position) (1+ (line-end-position)))
            (porg--update-overlays)))
      (message "Not on a heading"))))

(defun porg-promote (&optional arg)
  "Promote the current heading by removing one comment character ARG times."
  (interactive "p")
  (unless arg (setq arg 1))
  (if (porg--on-heading-p)
      (let* ((comment-count (porg--comment-count))
             (times (min arg (- comment-count porg-base-level))))
        (if (> times 0)
            (progn
              (save-excursion
                (beginning-of-line)
                (skip-chars-forward " \t")
                (delete-char times)
                (font-lock-flush (line-beginning-position) (1+ (line-end-position)))
                (porg--update-overlays)))
          (message "Cannot promote further")))
    (message "Not on a heading")))

(defun porg-demote (&optional arg)
  "Demote the current heading by adding one comment character ARG times."
  (interactive "p")
  (unless arg (setq arg 1))
  (if (porg--on-heading-p)
      (let ((comment-char (porg--get-comment-char)))
        (save-excursion
          (beginning-of-line)
          (skip-chars-forward " \t")
          (insert (make-string arg comment-char))
          (font-lock-flush (line-beginning-position) (1+ (line-end-position)))
          (porg--update-overlays)))
    (message "Not on a heading")))

(defun porg-metaleft (&optional arg)
  "Promote heading ARG times or call `backward-word' if not on a heading.
With shift held, select while moving backward."
  (interactive "^p")  ; ^ enables shift-selection
  (unless arg (setq arg 1))
  (if (porg--on-heading-p)
      (porg-promote arg)
    (backward-word arg)))

(defun porg-metaright (&optional arg)
  "Demote heading ARG times or call `forward-word' if not on a heading.
With shift held, select while moving forward."
  (interactive "^p")  ; ^ enables shift-selection
  (unless arg (setq arg 1))
  (if (porg--on-heading-p)
      (porg-demote arg)
    (forward-word arg)))

(defun porg--get-todo-keyword ()
  "Get the todo keyword at the current heading, if any.
Returns nil if not on a heading or no todo keyword present."
  (when (porg--on-heading-p)
    (save-excursion
      (beginning-of-line)
      (skip-chars-forward " \t")
      (let ((comment-char (porg--get-comment-char))
            (comment-count (porg--comment-count)))
        (when (and comment-char comment-count)
          (forward-char comment-count)
          (skip-chars-forward " \t")
          (let ((end (save-excursion
                       (skip-chars-forward "A-Z")
                       (point))))
            (when (> end (point))
              (let ((word (buffer-substring-no-properties (point) end)))
                (when (member word porg-todo-keywords)
                  word)))))))))

(defun porg--get-closed-line ()
  "Get the CLOSED timestamp line after current heading, if it exists.
Returns the line position or nil."
  (when (porg--on-heading-p)
    (save-excursion
      (forward-line 1)
      (when (and (not (eobp))
                 (looking-at (format "^[ \t]*%s\\{2\\}CLOSED:"
                                     (regexp-quote (char-to-string (porg--get-comment-char))))))
        (line-beginning-position)))))

(defun porg--remove-closed-line ()
  "Remove the CLOSED timestamp line after current heading, if it exists."
  (when-let* ((closed-pos (porg--get-closed-line)))
    (save-excursion
      (goto-char closed-pos)
      (delete-region (line-beginning-position) (1+ (line-end-position))))))

(defun porg--insert-closed-timestamp ()
  "Insert a CLOSED timestamp line after the current heading."
  (when (porg--on-heading-p)
    (let ((comment-char (porg--get-comment-char))
          (timestamp (format-time-string "[%Y-%m-%d %a %H:%M]")))
      (save-excursion
        ;; Remove existing CLOSED line if present
        (porg--remove-closed-line)
        ;; Insert new CLOSED line with 2 leading spaces
        (end-of-line)
        (insert "\n  " (make-string 2 comment-char) "CLOSED: " timestamp)
        ;; Fontify the new line
        (font-lock-flush (line-beginning-position) (1+ (line-end-position)))))))

(defun porg--set-todo-keyword (keyword)
  "Set the todo KEYWORD for the current heading.
If KEYWORD is nil or empty string, remove any existing keyword."
  (when (porg--on-heading-p)
    (save-excursion
      (beginning-of-line)
      (skip-chars-forward " \t")
      (let ((comment-char (porg--get-comment-char))
            (comment-count (porg--comment-count))
            (previous-keyword (porg--get-todo-keyword)))
        (when (and comment-char comment-count)
          (forward-char comment-count)
          (skip-chars-forward " \t")
          ;; Remove existing keyword if present
          (let ((current-keyword (porg--get-todo-keyword)))
            (when current-keyword
              (delete-char (length current-keyword))
              (when (looking-at " ")
                (delete-char 1))))
          ;; Insert new keyword if provided
          (when (and keyword (not (string-empty-p keyword)))
            (insert keyword " "))
          
          ;; Handle CLOSED timestamp
          (cond
           ;; Adding DONE keyword - insert CLOSED timestamp
           ((string= keyword "DONE")
            (porg--insert-closed-timestamp))
           ;; Removing DONE or changing to something else - remove CLOSED timestamp
           ((string= previous-keyword "DONE")
            (porg--remove-closed-line)))
          
          ;; Refontify the line
          (font-lock-flush (line-beginning-position) (1+ (line-end-position))))))))

(defun porg-todo (&optional arg)
  "Cycle the todo state of the current heading.
Cycles through: none -> TODO -> DONE -> none -> ...
With prefix ARG, cycle backwards."
  (interactive "P")
  (if (porg--on-heading-p)
      (let* ((current (porg--get-todo-keyword))
             (keywords (append '("") porg-todo-keywords))
             (current-idx (or (cl-position current keywords :test #'equal) 0))
             (next-idx (if arg
                           (mod (1- current-idx) (length keywords))
                         (mod (1+ current-idx) (length keywords))))
             (next-keyword (nth next-idx keywords)))
        (porg--set-todo-keyword next-keyword)
        (message "%s" (if (string-empty-p next-keyword)
                          "Removed todo keyword"
                        next-keyword)))
    (message "Not on a heading")))

(defun porg-shiftmetaleft (&optional arg)
  "Cycle todo state backward or `backward-word' if not on a heading ARG times."
  (interactive "p")
  (unless arg (setq arg 1))
  (if (porg--on-heading-p)
      (porg-todo t)
    (backward-word arg)))

(defun porg-shiftmetaright (&optional arg)
  "Cycle todo state forward or `forward-word' if not on a heading ARG times."
  (interactive "p")
  (unless arg (setq arg 1))
  (if (porg--on-heading-p)
      (porg-todo)
    (forward-word arg)))

;;; Headline
;;;; Navigation

(defun porg-next-visible-heading (&optional arg)
  "Move to the next heading line ARG times.
If moving to a level 1 heading, check if the last non-blank line before
the next level 1 heading is visible.  If not, recenter."
  (interactive "p")
  (unless arg (setq arg 1))
  (let ((count 0)
        (start-pos (point))
        (regexp (porg--make-heading-regexp)))
    (unless regexp
      (user-error "No comment syntax detected"))
    (end-of-line)
    (while (and (< count arg)
                (re-search-forward regexp nil t))
      (setq count (1+ count))
      (beginning-of-line))
    (if (< count arg)
        (progn
          (goto-char start-pos)
          (message "No next heading"))
      (beginning-of-line)
      (when (and (porg--on-heading-p)
                 (= (porg-current-level) 1))
        (let ((last-nonblank-pos
               (save-excursion
                 (let ((comment-char (porg--get-comment-char)))
                   (forward-line 1)
                   (if (re-search-forward (format "^[ \t]*%s\\{%d\\}[^%c]"
                                                  (regexp-quote (char-to-string comment-char))
                                                  porg-base-level
                                                  comment-char)
                                          nil t)
                       (progn
                         (forward-line -1)
                         (while (and (not (bobp))
                                     (looking-at "^[ \t]*$"))
                           (forward-line -1))
                         (end-of-line)
                         (point))
                     (goto-char (point-max))
                     (while (and (not (bobp))
                                 (looking-at "^[ \t]*$"))
                       (forward-line -1))
                     (end-of-line)
                     (point))))))
          (unless (pos-visible-in-window-p last-nonblank-pos)
            (recenter-top-bottom 0)))))))

(defun porg-previous-visible-heading (&optional arg)
  "Move to the previous heading line ARG times.
If moving to a level 1 heading, check if the last non-blank line before
the next level 1 heading is visible.  If not, recenter."
  (interactive "p")
  (unless arg (setq arg 1))
  (let ((count 0)
        (start-pos (point))
        (regexp (porg--make-heading-regexp)))
    (unless regexp
      (user-error "No comment syntax detected"))
    (beginning-of-line)
    (while (and (< count arg)
                (re-search-backward regexp nil t))
      (setq count (1+ count))
      (beginning-of-line))
    (if (< count arg)
        (progn
          (goto-char start-pos)
          (message "No previous heading"))
      (beginning-of-line)
      (when (and (porg--on-heading-p)
                 (= (porg-current-level) 1))
        (let ((last-nonblank-pos
               (save-excursion
                 (let ((comment-char (porg--get-comment-char)))
                   (forward-line 1)
                   (if (re-search-forward (format "^[ \t]*%s\\{%d\\}[^%c]"
                                                  (regexp-quote (char-to-string comment-char))
                                                  porg-base-level
                                                  comment-char)
                                          nil t)
                       (progn
                         (forward-line -1)
                         (while (and (not (bobp))
                                     (looking-at "^[ \t]*$"))
                           (forward-line -1))
                         (end-of-line)
                         (point))
                     (goto-char (point-max))
                     (while (and (not (bobp))
                                 (looking-at "^[ \t]*$"))
                       (forward-line -1))
                     (end-of-line)
                     (point))))))
          (unless (pos-visible-in-window-p last-nonblank-pos)
            (recenter-top-bottom 0)))))))

;;;; Consult

(defun porg--get-headings ()
  "Get all headings with their positions and levels.
Returns a list of (position level text) tuples."
  (save-excursion
    (goto-char (point-min))
    (let (headings
          (regexp (porg--make-heading-regexp)))
      (when regexp
        (while (re-search-forward regexp nil t)
          (let* ((pos (line-beginning-position))
                 (level (porg-current-level))
                 (text (string-trim
                        (buffer-substring-no-properties
                         (line-beginning-position)
                         (line-end-position)))))
            (when level
              (push (list pos level text) headings)))))
      (nreverse headings))))

(defun porg--format-heading (heading)
  "Format a HEADING for display in consult.
HEADING is a (position level text) tuple."
  (let* ((pos (nth 0 heading))
         (level (nth 1 heading))
         (text (nth 2 heading))
         (comment-char (porg--get-comment-char))
         (indent (make-string (* 2 (1- level)) ?\s))
         (bullet (char-to-string (porg-level-char level)))
         ;; Use outline faces instead of org-level faces for consistent height
         (outline-face (intern (format "outline-%d" (1+ (mod (1- level) 8)))))
         ;; Strip comment characters from text
         (clean-text (replace-regexp-in-string
                      (format "^[ \t]*%s+" (regexp-quote (char-to-string comment-char)))
                      ""
                      text)))
    (cons (propertize (format "%s%s %s" indent bullet (string-trim clean-text))
                      'face outline-face)
          pos)))

(defun porg-consult ()
  "Jump to a porg heading using consult."
  (interactive)
  (unless (require 'consult nil t)
    (user-error "This feature requires the 'consult' package.  Install it to use porg-consult"))
  (let* ((headings (porg--get-headings))
         (candidates (mapcar #'porg--format-heading headings)))
    (if candidates
        (let ((selected (consult--read
                         candidates
                         :prompt "Heading: "
                         :category 'porg-heading
                         :sort nil
                         :require-match t
                         :lookup #'consult--lookup-cdr
                         :state (consult--jump-state))))
          (when selected
            (goto-char selected)
            (recenter)))
      (message "No headings found"))))

;;;; Block Operations

(defun porg-eval-block ()
  "Evaluate the current block containing point."
  (interactive)
  (let ((block (porg--find-current-block)))
    (if block
        (eval-region (car block) (cdr block))
      (message "No block found at point"))))

(defun porg-copy-block ()
  "Copy the current block containing point."
  (interactive)
  (let ((block (porg--find-current-block)))
    (if block
        (kill-ring-save (car block) (cdr block))
      (message "No block found at point"))))

(defun porg-kill-block ()
  "Kill the current block containing point."
  (interactive)
  (let ((block (porg--find-current-block)))
    (if block
        (progn
          (kill-region (car block) (cdr block))
          (open-line 2)
          (forward-line 1)
          (open-line 1))
      (message "No block found at point"))))

;;;; Outline

(defun porg--setup-outline ()
  "Configure `outline-minor-mode' for porg headings."
  (let ((comment-char (porg--get-comment-char)))
    (when comment-char
      ;; Set outline regexp to match our headings
      (setq-local outline-regexp
                  (format "^[ \t]*%s\\{%d,\\}"
                          (regexp-quote (char-to-string comment-char))
                          porg-base-level))
      
      ;; Set outline level function
      (setq-local outline-level
                  (lambda ()
                    (let ((count (porg--comment-count)))
                      (if count
                          (porg--logical-level count)
                        1))))
      
      ;; Enable outline-minor-mode
      (outline-minor-mode 1))))

(defun porg-cycle (&optional arg)
  "Cycle visibility of current heading.
With prefix ARG, cycle global visibility."
  (interactive "P")
  (if arg
      (porg-cycle-global)
    (porg-cycle-local)))

(defun porg-cycle-local ()
  "Cycle visibility of the current section.

Rotates through these states:
1. FOLDED:   Show only the current heading.
2. CHILDREN: Show direct children headings (if any).
3. SUBTREE:  Show entire subtree (if has children).

If not on a heading, perform normal tab indentation."
  (interactive)
  (cond
   ;; Not on a heading - do normal tab behavior
   ((not (save-excursion
           (beginning-of-line)
           (outline-on-heading-p t)))
    (indent-for-tab-command))
   
   ;; On a heading - cycle visibility
   (t
    (let ((eoh (save-excursion (outline-end-of-heading) (point)))
          (eos (save-excursion (outline-end-of-subtree) (point))))
      (cond
       ;; Empty heading (no content)
       ((or (= eos eoh)
            (= (1+ eoh) (point-max)))
        (outline-show-entry)
        (message "EMPTY"))
       
       ;; Has no children - just toggle between folded/show
       ((not (save-excursion
               (outline-back-to-heading)
               (forward-line 1)
               (and (not (eobp))
                    (< (point) eos)
                    (or (outline-on-heading-p t)
                        (re-search-forward (concat "^" outline-regexp) eos t)))))
        (if (outline-invisible-p eoh)
            (progn
              (outline-show-entry)
              (message "SHOW"))
          (outline-hide-entry)
          (message "HIDE")))
       
       ;; Has children - cycle through states
       (t
        (cond
         ;; Currently folded -> show children
         ((outline-invisible-p eoh)
          (outline-show-entry)
          (outline-show-children)
          (message "CHILDREN")
          (setq this-command 'porg-cycle-children))
         
         ;; Was showing children -> show subtree
         ((eq last-command 'porg-cycle-children)
          (outline-show-subtree)
          (message "SUBTREE")
          (setq this-command 'porg-cycle-subtree))
         
         ;; Was showing subtree -> fold
         (t
          (outline-hide-subtree)
          (message "FOLDED")
          (setq this-command 'porg-cycle-folded)))))))))

(defun porg-cycle-global ()
  "Cycle global visibility through states.

1. OVERVIEW: Show only top-level headings.
2. CONTENTS: Show all headings.
3. ALL:      Show everything."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (cond
     ;; From OVERVIEW -> CONTENTS
     ((eq last-command 'porg-cycle-overview)
      (outline-show-all)
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward (porg--make-heading-regexp) nil t)
          (outline-hide-entry)))
      (message "CONTENTS")
      (setq this-command 'porg-cycle-contents))
     
     ;; From CONTENTS -> ALL
     ((eq last-command 'porg-cycle-contents)
      (outline-show-all)
      (message "SHOW ALL")
      (setq this-command 'porg-cycle-all))
     
     ;; From ALL or initial -> OVERVIEW
     (t
      (outline-hide-sublevels 1)
      (message "OVERVIEW")
      (setq this-command 'porg-cycle-overview)))))

;;;; Mode Definition

;;;###autoload
(define-minor-mode porg-mode
  "Org-like features for programming modes."
  :lighter " ◉"
  :group 'porg
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-<return>") 'porg-insert-heading-respect-content)
            (define-key map (kbd "M-<return>") 'porg-insert-subheading)
            (define-key map (kbd "M-<left>")   'porg-metaleft)
            (define-key map (kbd "M-<right>")  'porg-metaright)
            (define-key map (kbd "M-b")        'porg-metaleft)
            (define-key map (kbd "M-f")        'porg-metaright)
            (define-key map (kbd "M-B")        'porg-shiftmetaleft)
            (define-key map (kbd "M-F")        'porg-shiftmetaright)
            (define-key map (kbd "C-c C-t")    'porg-todo)
            (define-key map (kbd "<tab>")      'porg-cycle)
            (define-key map (kbd "<backtab>")  'porg-cycle-global)
            (define-key map (kbd "C-c C-n")    'porg-next-visible-heading)
            (define-key map (kbd "C-c C-p")    'porg-previous-visible-heading)
            (define-key map (kbd "C-c C-j")    'porg-consult)
            (define-key map (kbd "C-c e")      'porg-eval-block)
            (define-key map (kbd "C-c M-w")    'porg-copy-block)
            (define-key map (kbd "C-c w")      'porg-kill-block)
            map)
  (if porg-mode
      (progn
        ;; Detect comment syntax and setup keywords
        (porg--detect-comment-syntax)
        (when (porg--get-comment-char)
          (porg--setup-keywords)
          (when porg--keywords
            (font-lock-add-keywords nil porg--keywords)
            (add-hook 'after-change-functions #'porg--after-change nil t)
            (porg--fontify-buffer)
            (porg--update-overlays)
            (porg--setup-outline))))
    (save-excursion
      (goto-char (point-min))
      (when porg--keywords
        (font-lock-remove-keywords nil porg--keywords))
      (remove-hook 'after-change-functions #'porg--after-change t)
      (let ((regexp (porg--make-heading-regexp)))
        (when regexp
          (while (re-search-forward regexp nil t)
            (decompose-region (match-beginning 0) (match-end 0))
            (remove-text-properties (match-beginning 0) (match-end 0)
                                    '(invisible nil display nil)))))
      (porg--clear-overlays)
      (when (bound-and-true-p outline-minor-mode)
        (outline-minor-mode -1))
      (porg--fontify-buffer))))

(provide 'porg)

;;; porg.el ends here
