(defvar zv-pre-extensions '())

(defvar zv-post-extensions '(org-mime))

(defun zv/init-typescript ()
  (use-package typescript
    :mode (("\\.d.ts$" . typescript-mode)
           ("\\.ts$" . typescript-mode))))

(defun zv/init-org-mime ()
  (use-package org-mime
    :load-path (concat configuration-layer-contrib-directory "/usr/zv/extensions/org/contrib/lisp")
    :defer t
    :init
    (progn
      (setq org-mime-library 'mml)
      ;; A configuration setting that can not go unconfigured for org-mime
      (setq org-list-allow-alphabetical nil)
      ;; Don't include table of contents
      (setq org-export-with-toc nil)
      ;; Don't include section numbers with our headings
      (setq org-export-with-section-numbers nil))
    :config
    (evil-leader/set-key-for-mode 'message-mode "mpo" 'org-mime-htmlize)))
