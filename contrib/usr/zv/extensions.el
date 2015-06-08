(defvar zv-pre-extensions '(org))

(defvar zv-post-extensions '())

(defun zv/init-typescript ()
  (use-package typescript
    :mode (("\\.d.ts$" . typescript-mode)
           ("\\.ts$" . typescript-mode))))

(defun zv/init-org ()
  (use-package org
    :mode ("\\.org$" . org-mode)
    :init
    (progn
      (add-to-list 'load-path "/home/zv/.emacs.d/contrib/usr/zv/extensions/org/lisp")
      (add-to-list 'load-path "/home/zv/.emacs.d/contrib/usr/zv/extensions/org/contrib/lisp")
      (setq org-log-done t)

      (eval-after-load 'org-indent
        '(spacemacs|hide-lighter org-indent-mode))
      (add-hook 'org-mode-hook 'org-indent-mode)

      (evil-leader/set-key-for-mode 'org-mode
        "mc" 'org-capture
        "md" 'org-deadline
        "me" 'org-export-dispatch
        "mf" 'org-set-effort
        "mi" 'org-clock-in
        "mo" 'org-clock-out
        "mm" 'org-ctrl-c-ctrl-c
        "mq" 'org-clock-cancel
        "mr" 'org-refile
        "ms" 'org-schedule)

      (eval-after-load "org-agenda"
        '(progn
           (define-key org-agenda-mode-map "j" 'org-agenda-next-line)
           (define-key org-agenda-mode-map "k" 'org-agenda-previous-line)
           (define-key org-agenda-mode-map
             (kbd "SPC") evil-leader--default-map)))

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
      (define-key global-map "\C-cl" 'org-store-link)
      (define-key global-map "\C-ca" 'org-agenda)
      (eval-after-load 'org '(zv/configure-org-evil-bindings))
      ;; Org-mime configuration
      (require 'org-mime)
      (setq org-mime-library 'mml)
      ;; A configuration setting that can not go unconfigured for org-mime
      (setq org-list-allow-alphabetical nil)
      ;; Don't include table of contents
      (setq org-export-with-toc nil)
      ;; Don't include section numbers with our headings
      (setq org-export-with-section-numbers nil)
      (evil-leader/set-key-for-mode 'message-mode "mpo" 'org-mime-htmlize)
      )))

(defun zv/configure-org-evil-bindings ()
  ;; Ensure we can still use M-j/M-k to move windows
  (evil-define-key 'motion org-mode-map
    next-buffer-key 'evil-window-next
    prev-buffer-key 'evil-window-prev)

  ;; normal state shortcuts
  (evil-define-key 'normal org-mode-map
    "gh" 'outline-up-heading
    ;; to be backward compatible with older org version
    "gj" (if (fboundp 'org-forward-same-level)
             'org-forward-same-level
           'org-forward-heading-same-level)
    "gk" (if (fboundp 'org-backward-same-level)
             'org-backward-same-level
           'org-backward-heading-same-level)
    "gl" 'outline-next-visible-heading
    "t" 'org-todo
    "T" '(lambda () (interactive) (evil-org-eol-call (lambda() (org-insert-todo-heading nil))))
    "H" 'org-beginning-of-line
    "L" 'org-end-of-line
    "$" 'org-end-of-line
    "^" 'org-beginning-of-line
    (kbd "TAB") 'org-cycle)

  ;; normal & insert state shortcuts.
  (mapc (lambda (state)
          (evil-define-key state org-mode-map
            (kbd "M-l") 'org-metaright
            (kbd "M-h") 'org-metaleft
            (kbd "H-k") 'org-metaup
            (kbd "H-j") 'org-metadown
            (kbd "M-L") 'org-shiftmetaright
            (kbd "M-H") 'org-shiftmetaleft
            (kbd "H-K") 'org-shiftmetaup
            (kbd "H-J") 'org-shiftmetadown
            (kbd "M-o") '(lambda () (interactive)
                           (evil-org-eol-call
                            '(lambda()
                               (org-insert-heading)
                               (org-metaright))))
            (kbd "M-t") '(lambda () (interactive)
                           (evil-org-eol-call
                            '(lambda()
                               (org-insert-todo-heading nil)
                               (org-metaright))))
            ))
        '(normal insert))

  (eval-after-load 'org
    ;; move the leader bindings to `m` prefix to be consistent with
    ;; the rest of spacemacs bindings
    '(evil-leader/set-key-for-mode 'org-mode
       "a" nil "ma" 'org-agenda
       "c" nil "mA" 'org-archive-subtree
       "t" nil "mt" 'org-show-todo-tree)))
