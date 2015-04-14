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
(require 'org)

; Enable the custom-id to be copied by a command
(defun scholar-copy-org-id ()
  (interactive)
  (kill-new (with-output-to-string (princ (org-entry-get nil "CUSTOM_ID"))))
)

(defun scholar-get-bibtex-entries ()
  (interactive)

  (let* ((selected-entries (reftex-offer-bib-menu))
	 (entries selected-entries)
	 entry string cite-view)

    (unless selected-entries (error "Quit"))

    (if (stringp selected-entries)
	;; Nonexistent entry
	(setq selected-entries nil
	      entries (list (list selected-entries
					 (cons "&key" selected-entries))))
      ;; It makes sense to compute the cite-view strings.
      (setq cite-view t))

    (when (eq (car selected-entries) 'concat)
      ;; All keys go into a single command - we need to trick a little
      (pop selected-entries)
      (let ((concat-keys (mapconcat 'car selected-entries ",")))
	(setq entries 
	      (list (list concat-keys (cons "&key" concat-keys))))))
    entries)
)

(defun scholar-insert-org-link ()
  (interactive)

  (let* ((entries (scholar-get-bibtex-entries)))
    (while (setq entry (pop entries))
        ;; Format the citation and insert it
        (setq key (reftex-get-bib-field "&key" entry))
        (insert (concat "[[file:" key ".org][" key "]]"))
    )
))

(defun scholar-new-note ()
  (interactive)

  (let* (filename key bibfiles (entries (scholar-get-bibtex-entries)))
    (while (setq entry (pop entries))
      (setq key (reftex-get-bib-field "&key" entry))
      (setq filename (concat key ".org"))
      (if (file-exists-p filename) (find-file filename)
	(progn 
	  (setq bibfiles (reftex-get-bibfile-list))
	  (find-file filename)
	  (insert
	   (mapconcat 'identity
		      (mapcar
		       (lambda (b)
			 (concat "# \\bibliography{"
				 (file-name-nondirectory (file-name-sans-extension b)) "}")) bibfiles)
		      "\n"))

	  (newline)
	  (newline)
	  (reftex-parse-one)
	  (scholar-insert-title-link entry)
	  (goto-char (point-max)))))))

(defun scholar-insert-title-link (&optional entry)
  ;; This really does the work of reftex-citation.
  (interactive)
  
  (unless entry
    (let* ((entries (scholar-get-bibtex-entries)))
      (while (setq entry-last (pop entries))
	(setq entry entry-last))))

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
)

;; Jump back to top level heading:
(defun org-back-to-top-level-heading ()
  "Go back to the current top level heading."
  (interactive)
  (or (re-search-backward "^\* " nil t)
      (goto-char (point-min))))
