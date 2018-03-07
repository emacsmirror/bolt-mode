(defconst bolt-mode-identifier-re "[a-zA-Z_$][a-zA-Z0-9]+")

(defvar bolt-highlights
  (let ((keywords '("type" "path" "extends" "is" "this" "now" "true" "false"))
	(types '("String" "Number" "Boolean" "Object" "Any" "Null" "Map")))
    `((,(regexp-opt keywords 'words) . font-lock-keyword-face)
      (,(regexp-opt types 'words) . font-lock-type-face)
      // function calls
      (,(concat "\\<\\(" bolt-mode-identifier-re "\\)(") . (1 font-lock-function-name-face)))))

(defun bolt-mode-previous-nonblank-line ()
  "Move cursor to previous non-blank line"
  (goto-char (line-beginning-position))
  (skip-chars-backward "\r\n\s\t"))

(defun bolt-mode-find-unclosed-pair ()
  "Return non-nil value if current line contains unclosed pairs."
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
  (let (base should-indent)
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
	   (t base-indent))))))

;;;###autoload
(define-derived-mode bolt-mode fundamental-mode "Bolt"
  "Major mode for editing Bolt files"
  (setq font-lock-defaults '(bolt-highlights))
  (setq indent-line-function #'bolt-mode-indent-line))

(provide 'bolt-mode)
