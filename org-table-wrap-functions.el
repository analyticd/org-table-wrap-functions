(provide 'org-table-wrap-functions)

(defun org-table-column-wrap-to-width (width)
  "Wrap current column to WIDTH."
  (interactive (list (read-number "Enter column width: ")))
  (org-table-check-inside-data-field)
  (org-table-align)
  (let (cline
        (ccol (org-table-current-column))
        new-row-count
        (created-new-row-at-bottom nil)
        (orig-line (org-table-current-line))
        (more t))
    (org-table-goto-line 1)
    (org-table-goto-column ccol)
    (while more
      (setq cline (org-table-current-line))
      ;; Cut current field (sets org-table-clip)
      (org-table-copy-region (point) (point) 'cut)
      ;; Justify for width
      (ignore-errors
        (setq org-table-clip
              (mapcar 'list (org-wrap (caar org-table-clip) width nil))))
      ;; Add new lines and fill
      (setq new-row-count (1- (length org-table-clip)))
      (org-table-goto-line cline)
      (if (> new-row-count 0)
          (setq created-new-row-at-bottom (org-table-insert-n-row-below new-row-count)))
      (org-table-goto-line cline)
      (org-table-goto-column ccol)
      (org-table-paste-rectangle)
      (org-table-goto-line (+ cline new-row-count))
      ;; Move to next line
      (setq more (org-table-goto-line (+ cline new-row-count 1)))
      (org-table-goto-column ccol))
    (when created-new-row-at-bottom
      (org-shiftmetaup))
    (org-table-goto-line orig-line)
    (org-table-goto-column ccol)))

(defun org-table-column-wrap-to-point ()
  "Wrap text in current column to current point as rightmost
boundary."
  (interactive)
  (let (begin
        (end (point))
        cell-contents-to-point
        width
        cline
        (ccol (org-table-current-column))
        new-row-count
        (created-new-row-at-bottom nil)
        (orig-line (org-table-current-line))
        (more t))
    (save-excursion
      (search-backward-regexp "|")
      (forward-char 1)
      (setq begin (point))
      (setq cell-contents-to-point (buffer-substring begin end))
      (setq width (length cell-contents-to-point)))
    (org-table-check-inside-data-field)
    (org-table-align)
    (org-table-goto-line 1)
    (org-table-goto-column ccol)
    (while more
      (setq cline (org-table-current-line))
      ;; Cut current field (sets org-table-clip)
      (org-table-copy-region (point) (point) 'cut)
      ;; Justify for width
      (ignore-errors
        (setq org-table-clip
              (mapcar 'list (org-wrap (caar org-table-clip) width nil))))
      ;; Add new lines and fill
      (setq new-row-count (1- (length org-table-clip)))
      (org-table-goto-line cline)
      (if (> new-row-count 0)
          (setq created-new-row-at-bottom (org-table-insert-n-row-below new-row-count)))
      (org-table-goto-line cline)
      (org-table-goto-column ccol)
      (org-table-paste-rectangle)
      (org-table-goto-line (+ cline new-row-count))
      ;; Move to next line
      (setq more (org-table-goto-line (+ cline new-row-count 1)))
      (org-table-goto-column ccol))
    (when created-new-row-at-bottom
      (org-shiftmetaup))
    (org-table-goto-line orig-line)
    (org-table-goto-column ccol)))

(defun org-table-insert-n-row-below (n)
  "Insert N new lines below the current."
  (let (created-new-row-at-bottom)
    (dotimes (_ n)
      (setq line (buffer-substring (point-at-bol) (point-at-eol)))
      (forward-line 1)
      (when (looking-at "^ *$")
        (insert (org-table-clean-line line))
        (setq created-new-row-at-bottom t))
      (org-shiftmetadown))
    created-new-row-at-bottom))

(defun org-table-wrap-cell-at-point ()
  "Wrap text of cell at point as rightmost boundary."
  (interactive)
  (org-table-check-inside-data-field)
  (let ((ccol (org-table-current-column)))
    (zap-up-to-char 1 ?|)
    (forward-line 1)
    (org-shiftmetadown)
    (org-table-goto-column ccol)
    (yank)
    (org-table-align)
    (forward-line -1)
    (org-table-goto-column ccol)))


(defun jump-up-to-non-whitespace-char-in-same-column ()
  "Go up in the same column until we encounter a non-whitespace
character."
    (interactive)
    (previous-line)
    (while (or (= (char-after (point)) 32)
               (= (char-after (point)) 10))
      (previous-line)))

(defun org-table-unwrap-cell-region (beg end)
  "Join a rectangle of text into one longer string."
  (interactive "r")
  (let ((result)
        (ccol (org-table-current-column)))
    (org-table-cut-region beg end)
    (setq result (string-trim (replace-regexp-in-string
                               "[ \t]+" " "
                               (substring-no-properties (string-join (mapcar #'car org-table-clip) " ")))))
    (org-table-goto-column ccol)
    (jump-up-to-non-whitespace-char-in-same-column)
    (next-line)
    (insert result)
    (forward-char 1)
    (org-table-align)))
