;; Find URLs in Article
;; --------------------
(defun gnus-article-browse-urls ()
  "Visit a URL from the `gnus-article-buffer' by prompting via a
    poping up a buffer showing the list of URLs found with the
    `gnus-button-url-regexp'."
  (interactive)
  (gnus-configure-windows 'article)
  (gnus-summary-select-article nil nil 'pseudo)
  (let ((temp-buffer (generate-new-buffer " *Article URLS*"))
        (urls (gnus-article-get-current-urls))
        (this-window (selected-window))
        (browse-window (or (get-buffer-window gnus-article-buffer)
                           ))
        (count 0))
    (save-excursion
      (save-window-excursion
        (set-buffer temp-buffer)
        (mapcar
         (lambda (string)
           (insert (format "\t%d: %s\n" count string))
           (setq count (1+ count))) urls)
        (not-modified)
        (pop-to-buffer temp-buffer)
        (setq count (string-to-number (read-input "Browse which URL: ")))
        (kill-buffer temp-buffer))
      (if browse-window
          (progn (select-window browse-window)
                 (browse-url (nth count urls)))))
    (select-window this-window)))

(defun gnus-article-get-current-urls ()
  "Return a list of the urls found in the current `gnus-article-buffer'"
  (let (url-list)
    (save-excursion
      (set-buffer gnus-article-buffer)
      (setq url-list (gnus-article-get-urls-region (point-min) (point-max))))
    url-list))

(defun gnus-article-get-urls-region (min max)
  "Return a list of urls found in the region between MIN and MAX"
  (let (url-list)
    (save-excursion
      (save-restriction
        (narrow-to-region min max)
        (goto-char (point-min))
        (while (re-search-forward gnus-button-url-regexp nil t)
          (let ((match-string (match-string-no-properties 0)))
            (if (and (not (equal (substring match-string 0 4) "file"))
                     (not (member match-string url-list)))
                (setq url-list (cons match-string url-list)))))))
    url-list))
