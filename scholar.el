(defun scholar-get-pdfs ()
  "Return a list of pdf files in the current bib files"
  (interactive)
  (with-temp-buffer
    (dolist ( f (file-expand-wildcards "*.bib"))
       (insert-file-contents f))
    (delete-non-matching-lines "^ ?file")
    (replace-regexp "^file = {.*?:\\(.*?\\):.*$" "\\1")
    (concat "ag \"" (mapconcat 'identity (split-string (buffer-string) "\n" t) "\" \"") "\"")
  )
)

(defun scholar-extract-filename (entry)
  (string-match ".*?:\\(.*?\\):.*" entry)
  (match-string 1 entry)
)

(require 'reftex)
(require 'reftex-cite)
(require 'zotelo)
(require 'org)

; Enable the custom-id to be copied by a command
(defun scholar-copy-org-id ()
  (interactive)
  (kill-new (with-output-to-string (princ (org-entry-get nil "CUSTOM_ID"))))
)

(defun scholar-insert-org-link ()
  ;; This really does the work of reftex-citation.
  (interactive)
  
  ;; Update the bibtex database
  (zotelo-update-database)

  (let* ((selected-entries (reftex-offer-bib-menu))
	 (insert-entries selected-entries)
	 entry string cite-view)

    (unless selected-entries (error "Quit"))

    (if (stringp selected-entries)
	;; Nonexistent entry
	(setq selected-entries nil
	      insert-entries (list (list selected-entries
					 (cons "&key" selected-entries))))
      ;; It makes sense to compute the cite-view strings.
      (setq cite-view t))

    (when (eq (car selected-entries) 'concat)
      ;; All keys go into a single command - we need to trick a little
      (pop selected-entries)
      (let ((concat-keys (mapconcat 'car selected-entries ",")))
	(setq insert-entries 
	      (list (list concat-keys (cons "&key" concat-keys))))))
      (while (setq entry (pop insert-entries))
	;; Format the citation and insert it
	(setq title (reftex-get-bib-field "title" entry))
	(setq filename (scholar-extract-filename (reftex-get-bib-field "file" entry)))
    (unless filename
      (setq filename (reftex-get-bib-field "URL" entry)))
    (setq key (reftex-get-bib-field "&key" entry))
    (insert "* ")
    (if (string= "" filename)
      (insert (concat title " (" key ")"))
      (org-insert-link nil filename (concat title " (" key ")")))
    (org-set-property "CUSTOM_ID" key)
    (org-todo)
    (scholar-copy-org-id)
)))

;; Jump back to top level heading:
(defun org-back-to-top-level-heading ()
  "Go back to the current top level heading."
  (interactive)
  (or (re-search-backward "^\* " nil t)
      (goto-char (point-min))))
