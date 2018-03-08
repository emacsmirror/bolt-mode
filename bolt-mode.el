;;; bolt-mode.el --- Editing support for Bolt language  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Mikhail Pontus

;; Author: Mikhail Pontus <m.pontus@gmail.com>
;; Keywords: languages

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Provides major mode for Bolt files with syntax highlighting and indentation support.

;;; Code:

(defconst bolt-mode-identifier-re "[a-zA-Z_$][a-zA-Z0-9]+")

(defvar bolt-highlights
  (let ((keywords '("type" "path" "extends" "is" "this" "now" "true" "false"))
	(types '("String" "Number" "Boolean" "Object" "Any" "Null" "Map")))
    `((,(regexp-opt keywords 'words) . font-lock-keyword-face)
      (,(regexp-opt types 'words) . font-lock-type-face)
      // function calls
      (,(concat "\\<\\(" bolt-mode-identifier-re "\\)(") . (1 font-lock-function-name-face)))))

(defun bolt-mode-previous-nonblank-line ()
  "Move cursor to previous non-blank line."
  (goto-char (line-beginning-position))
  (skip-chars-backward "\r\n\s\t"))

(defun bolt-mode-find-unclosed-pair ()
  "Return non-nil value if unclosed pairs found on current line."
  (goto-char (line-beginning-position))
  (let (unclosed-pairs)
    (while (not (or (looking-at "\n")
		    (eobp)))
      (let ((char (following-char)))
	(cond
	 ((eq (char-syntax char) ?\()
	  ;; found opening pair
	  (push char unclosed-pairs))
	 ((and (eq (char-syntax char) ?\))
	       (eq (matching-paren char) (car unclosed-pairs)))
	  ;; found a match to most recent pair
	  (pop unclosed-pairs))))
      (forward-char))
    unclosed-pairs))

(defun bolt-mode-indent-line ()
  "Indent current line."
  (let (base-indent should-indent)
    (save-excursion
      (bolt-mode-previous-nonblank-line)
      (setq base-indent (current-indentation))
      (setq should-indent (bolt-mode-find-unclosed-pair)))
    (indent-line-to
     (cond (should-indent (+ base-indent tab-width))
	   ;; Dedent when current line starts with a closing pair
	   ((save-excursion
	      (goto-char (line-beginning-position))
	      (skip-syntax-forward " " (line-end-position))
	      (eq (char-syntax (following-char)) ?\)))
	    (- base-indent tab-width))
	   (t base-indent)))))

;;;###autoload
(define-derived-mode bolt-mode fundamental-mode "Bolt"
  "Major mode for editing Bolt files"
  (setq font-lock-defaults '(bolt-highlights))
  (setq indent-line-function #'bolt-mode-indent-line))

;;;###autoload
(add-to-list 'auto-mode-alist
             '("/\\.bolt\\'" . bolt-mode))

(provide 'bolt-mode)
;;; bolt-mode.el ends here
