(defvar zv-packages '(
                      ;; ggtags
                      ;; helm-gtags
                      bbdb
                      erc-hl-nicks
                      evil-org
                      jade-mode
                      nodejs-repl
                      org
                      org-bullets
                      org-pomodoro
                      org-repo-todo
                      ))

(defvar zv-excluded-packages '())

(defun zv/init-erc-hl-nicks ()
  (use-package erc-hl-nicks
    :commands erc))

(defun zv/init-nodejs-repl ()
  (use-package nodejs-repl ()
    :config (setq nodejs-repl-command "/home/zv/.nvm/versions/io.js/v1.0.3/bin/node")))

(defun zv/init-bbdb ()
  (use-package bbdb
    :defer t
    :init (bbdb-initialize)
    :config (setq bbdb-expand-mail-aliases t
                  bbdb-complete-name-full-completion t
                  bbdb-file (concat user-emacs-directory "/bbdb.gpg"))))

(defun zv/init-ggtags ()
  (use-package ggtags
    :defer t
    :init
    (progn
      (add-hook 'c-mode-common-hook
                (lambda ()
                  (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
                    (ggtags-mode 1))))
      (define-key ggtags-mode-map (kbd "C-c g y") 'ggtags-find-other-symbol)
      (define-key ggtags-mode-map (kbd "C-c g h") 'ggtags-view-tag-history)
      (define-key ggtags-mode-map (kbd "C-c g c") 'ggtags-create-tags)
      (define-key ggtags-mode-map (kbd "C-c g u") 'ggtags-update-tags)
      (define-key ggtags-mode-map (kbd "C-c g /") 'ggtags-grep)
      (define-key ggtags-mode-map (kbd "C-c g r") 'ggtags-query-replace)
      (define-key ggtags-mode-map (kbd "C-c g s") 'ggtags-find-other-symbol)

      ;; Replace our find file definition
      (define-key evil-motion-state-map "gf" 'ggtags-find-file)
      (define-key evil-motion-state-map "\C-]" 'ggtags-find-definition)
      (define-key evil-motion-state-map (kbd "g]") 'ggtags-find-tag-dwim)
      (define-key evil-motion-state-map "g\C-]" 'ggtags-find-reference)

      (define-key ggtags-mode-map (kbd "M-,") 'pop-tag-mark))))

(defun zv/init-helm-gtags ()
  (use-package helm-gtags
    :defer t
    :init
    (progn
      (setq
       helm-gtags-ignore-case t
       helm-gtags-auto-update t
       helm-gtags-use-input-at-cursor t
       helm-gtags-pulse-at-cursor t
       helm-gtags-prefix-key "\C-cg"
       helm-gtags-suggested-key-mapping t)

      (diminish 'helm-gtags-mode)
      ;; Enable helm-gtags-mode
      ;; Tag select
      (add-hook 'dired-mode-hook 'helm-gtags-mode)
      (add-hook 'eshell-mode-hook 'helm-gtags-mode)
      (add-hook 'c-mode-hook 'helm-gtags-mode)
      (add-hook 'c++-mode-hook 'helm-gtags-mode)
      (add-hook 'asm-mode-hook 'helm-gtags-mode)

      ;; (define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
      (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
      (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack))))

(defun zv/init-jade-mode ()
  (use-package jade-mode
    :mode "\\.jade$"))

(defun zv/init-evil-org ()
  (use-package evil-org
    :commands evil-org-mode
    :init
    (add-hook 'org-mode-hook 'evil-org-mode)
    :config
    (progn
      (evil-leader/set-key-for-mode 'org-mode
           "a" nil "ma" 'org-agenda
           "c" nil "mA" 'org-archive-subtree
           "o" nil "mC" 'evil-org-recompute-clocks
           "l" nil "ml" 'evil-org-open-links
           "t" nil "mt" 'org-show-todo-tree)
      (spacemacs|diminish evil-org-mode " ⓔ" " e"))))

(defun zv/init-org-bullets ()
  (use-package org-bullets
    :defer t
    :init (add-hook 'org-mode-hook 'org-bullets-mode)))

(defun zv/init-org-pomodoro ()
  (use-package org-pomodoro
    :defer t
    :init
    (progn
      (when (system-is-mac)
        (setq org-pomodoro-audio-player "/usr/bin/afplay"))
      (evil-leader/set-key-for-mode 'org-mode
        "mp" 'org-pomodoro))))

(defun zv/init-org-repo-todo ()
  (use-package org-repo-todo
    :commands (ort/capture-todo
               ort/capture-todo-check
               ort/goto-todos)
    :init
    (progn
      (evil-leader/set-key
        "Ct"  'ort/capture-todo
        "CT"  'ort/capture-todo-check)
      (evil-leader/set-key-for-mode 'org-mode
        "mgt" 'ort/goto-todos))))



(defun zv/init-org-mime ()
  (use-package org-mime
    :load-path "/home/zv/.emacs.d/contrib/usr/zv/extensions/org/lisp"
    :init
    (progn
      (setq org-mime-library 'mml)
      ;; A configuration setting that can not go unconfigured for org-mime
      (setq org-list-allow-alphabetical nil)
      ;; Don't include table of contents
      (setq org-export-with-toc nil)
      ;; Don't include section numbers with our headings
      (setq org-export-with-section-numbers nil)
      (evil-leader/set-key-for-mode 'message-mode "mpo" 'org-mime-htmlize))))

