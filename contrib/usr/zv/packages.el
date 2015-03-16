(defvar zv-packages '(bbdb
                      erc-hl-nicks
                      nodejs-repl
                      org
                      ;; ggtags
                      ;; helm-gtags
                      ;; Typescript
                      ;; tss
                      ;;;; angular-snippets
                      jade-mode
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

 ;; jade-mode ------------------------------------------------------------------
(defun zv/init-jade-mode ()
  (use-package jade-mode
    :mode "\\.jade$"))

;; Include typescript mode
(defun zv/init-tss ()
  (use-package tss
    :mode ("\\.tsc$" . tss-mode)))

;; Include typescript mode
(defun zv/init-angular-snippets () (use-package angular-snippets))

(defun zv/init-rust-mode ()
  (use-package rust-mode
    :commands rust-mode
    :mode "\\.rs$'"
    :interpreter "rustc"))

;; Org Mode
(defun zv/init-org ()
  (use-package org
    :mode ("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode)
    :defer t
    ;; Global Keybindings
    :bind (("<f12>"   . org-agenda)
           ("<f8>"    . org-cycle-agenda-files)
           ("<f9> b"  . bbdb)
           ("<f9> c"  . calendar)
           ("<f9> l"  . org-toggle-link-display)
           ("M-<f9>"  . org-toggle-inline-images)
           ("<f11>"   . org-clock-goto)
           ("C-<f11>" . org-clock-in)
           ("C-c c"   . org-capture))
    :init
    (progn
      (setq org-log-done t)
      (add-hook 'org-mode-hook 'org-indent-mode)
      :config
      (progn
        (setq org-default-notes-file (org-path "notes.org"))

        ;; Configure evil keybindings
        (eval-after-load 'org
          '(zv/configure-org-evil-bindings))

        ;; Automatically colorize source code results
        (setq org-src-fontify-natively t)
        ;; Refile settings ------------------------------------
        ;; Targets include this file and any file contributing to the agenda - up to 9 levels deep
        ;; Use full outline paths for refile targets - we file directly with IDO
        (setq org-refile-use-outline-path nil)
        (setq org-refile-targets (quote ((org-agenda-files :maxlevel . 3))))

        ;; Agenda Mode ----------------------------------------
        ;; Set span of time agenda describes
        (setq org-agenda-span 'day)

        ;; Clocking ------------------------------------------
        ;; Show lot of clocking history so it's easy to pick items off the C-F11 list
        (setq org-clock-history-length 23)
        ;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
        (setq org-clock-out-remove-zero-time-clocks t)

        ;; Todo Mode configuration
        (setq org-todo-keywords
              (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                      (sequence "WAITING(w@/!)" "FEEDBACK(f@/1)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "MEETING"))))

        (setq org-todo-keyword-faces
              (quote (("TODO"      :foreground "red"          :weight bold)
                      ("NEXT"      :foreground "blue"         :weight bold)
                      ("DONE"      :foreground "forest green" :weight bold)
                      ("WAITING"   :foreground "orange"       :weight bold)
                      ("HOLD"      :foreground "magenta"      :weight bold)
                      ("CANCELLED" :foreground "forest green" :weight bold)
                      ("MEETING"   :foreground "forest green" :weight bold))))

        ;; Fast todo selection allows changing from any task todo state to
        ;; any other state directly by selecting the appropriate key from
        ;; the fast todo selection key menu. This is a great feature!
        (setq org-use-fast-todo-selection t)

        ;; allows changing todo states with S-left and S-right skipping all
        ;; of the normal processing when entering or leaving a todo
        ;; state. This cycles through the todo states but skips setting
        ;; timestamps and entering notes which is very convenient when all
        ;; you want to do is fix up the status of an entry.
        (setq org-treat-S-cursor-todo-selection-as-state-change nil)

        ;; My tag state triggers
        (setq org-todo-state-tags-triggers
              (quote (("CANCELLED" ("CANCELLED" . t))
                      ("WAITING" ("WAITING" . t))
                      ("HOLD" ("WAITING") ("HOLD" . t))
                      (done ("WAITING") ("HOLD"))
                      ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
                      ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
                      ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))

        ;; Capture templates for: TODO tasks, Notes, appointments, phone calls, meetings, and org-protocol
        (setq org-capture-templates
              (quote
               (("t" "todo"         entry (file (org-path "gtd.org"))            "* TODO %?\n%U\n%a\n")
                ("n" "note"         entry (file (org-path "notes.org"))          "* %? :NOTE:\n%U\n%a\n")
                ("c" "conversation" entry (file (org-path "conversation.org"))   "* CONVERSATION %? :PHONE:\n%U")
                ("i" "ideas"        entry (file (org-path "ideas.org"))          "* %?\n%U")
                ("q" "quotes"       item  (file (org-path "quotes.org"))         "%?\n%U")
                ("j" "journal"      entry (file+datetree (org-path "diary.org")) "* %?\n%U\n")
                )))

        ;; Org graphics
        (setq org-ditaa-jar-path "~/.emacs.d/contrib/zv/extensions/org/contrib/scripts/ditaa.jar")
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
                 (latex      . t))))))))

(defun zv/init-clojure-mode ()
  (use-package clojure
    :defer t
    :idle t
    :idle-priority 5
    :init
    (progn
      (evil-leader/set-key-for-mode 'clojure-mode
        "ml" 'evil-lisp-state
        "mz" 'cider-switch-to-repl-buffer
        "mcj" 'cider-jack-in
        "mcc" 'cider-connect
        "mcq" 'cider-quit
        "mck" 'cider-load-buffer
        "mcl" 'cider-load-file
        "mcn" 'cider-repl-set-ns
        "mdd" 'cider-doc
        "mdg" 'cider-grimoire
        "mda" 'cider-apropos
        "mgv" 'cider-jump-to-var
        "mgr" 'cider-jump-to-resource
        "mge" 'cider-jump-to-compilation-error
        "mgs" 'cider-jump
        "mtt" 'cider-test-run-tests))))

(defun zv/init-cider ()
  (use-package cider
    :defer t
    :idle t
    :idle-priority 5
    :init
    (progn
      (defun cider-repl-mode-defaults ()
        (smartparens-strict-mode +1)
        (rainbow-delimiters-mode +1)
        (whitespace-mode -1))

      (setq nrepl-log-messages t)
      (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
      (add-hook 'cider-repl-mode-hook 'cider-repl-mode-defaults)


      (evil-leader/set-key-for-mode 'cider-repl-mode
        "ml" 'evil-lisp-state
        "mz" 'cider-switch-to-last-clojure-buffer
        "mcq" 'cider-quit))))

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
