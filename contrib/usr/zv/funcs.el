;; Empty the definition of some spacemacs init methods
(defun spacemacs/goto-link-line ())
(defun spacemacs//insert-banner ())

;; erc
(defun erc-connect ()
  "Connect to IRC."
  (interactive)
  ;; disable powerline for ERC ----------------------------
  (erc :server "irc.freenode.net" :port 6667 :nick "zv")
  (erc :server "irc.mozilla.org" :port 6667 :nick "zv")
  (erc :server "irc.oftc.net" :port 6667 :nick "zv"))

(defun add-semicolon-to-end-of-line ()
    "Unsurprisingly, this adds a semicolon to the end of the line"
    (interactive)
    (save-excursion (end-of-line) (insert ";")))

(defun org-path (file)
        "Returns the full path of an org directory file"
        (expand-file-name (concat org-directory "/" file)))

(defun zv/define-keymap (mode-map keymap)
  (mapc (lambda (binding)
          (define-key mode-map (car binding) (cdr binding)))
        keymap))

(defun zv/declare-prefix-for-mode (mode prefix name)
  "Declare a prefix PREFIX. MODE is the mode in which this prefix command should
be added. PREFIX is a string describing a key sequence. NAME is a symbol name
used as the prefix command."
  (let ((command (intern (concat spacemacs/prefix-command-string name))))
    (define-prefix-command command)
    (evil-leader/set-key-for-mode mode prefix command)))

(defun zv/enlarge-window-by-dominant-dimension (magnitude)
  "Enlarge the current window by height if vertically split, or width otherwise"
  (cond ((window-full-width-p) (enlarge-window magnitude))
        ((window-full-height-p) (enlarge-window-horizontally magnitude))
        (t (enlarge-window (/ magnitude 2)))))

(defun zv/tile-split-window ()
  "If our current window width / height is greater than 1.68, split vertically"
  (interactive)
  (let* ((window-ratio (/ (float (window-pixel-width)) (float (window-pixel-height))))
         (golden-ratio (/ (+ 1 (sqrt 5)) 2)))
    (if (> window-ratio golden-ratio)
        (evil-window-vsplit)
      (evil-window-split))))

;; Org Mode
;; --------

(defun clever-insert-item ()
  "Clever insertion of org item."
  (if (not (org-in-item-p))
      (insert "\n")
    (org-insert-item))
  )

(defun evil-org-eol-call (fun)
  "Go to end of line and call provided function.
FUN function callback"
  (end-of-line)
  (funcall fun)
  (evil-append nil)
  )

;; Restart `tern-mode`
(defun delete-tern-process ()
  (interactive)
  (delete-process "Tern"))

