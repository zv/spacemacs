(defvar zv-pre-extensions '())

(defvar zv-post-extensions '())

(defun zv/init-org ()
  (use-package org
    :mode ("\\.org$" . org-mode)
    :init
    (progn
      (add-to-list 'load-path "/home/zv/.emacs.d/contrib/!usr/zv/extensions/org/lisp")
      (add-to-list 'load-path "/home/zv/.emacs.d/contrib/!usr/zv/extensions/org/contrib/lisp")
      (setq org-log-done t)

      (add-hook 'org-mode-hook 'org-indent-mode)

      ;; Automatically colorize source code results
      (setq org-src-fontify-natively t)

      (org-babel-do-load-languages
       (quote org-babel-load-languages)
       (quote ((emacs-lisp . t)
               (dot        . t)
               (ditaa      . t)
               (R          . t)
               (python     . t)
               (ruby       . t)
               (gnuplot    . t)
               (clojure    . t)
               (sh         . t)
               (ledger     . t)
               (org        . t)
               (plantuml   . t)
               (latex      . t)))))
    :config
    (progn
      (require 'org-indent)
      ;; Org-mime configuration
      (require 'org-mime)
      (setq org-mime-library 'mml)
      ;; A configuration setting that can not go unconfigured for org-mime
      (setq org-list-allow-alphabetical nil)
      ;; Don't include table of contents
      (setq org-export-with-toc nil)
      ;; Don't include section numbers with our headings
      (setq org-export-with-section-numbers nil)
      (evil-leader/set-key-for-mode 'message-mode "mpo" 'org-mime-htmlize))))

