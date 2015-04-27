;;---------------------------------------------------------------------
;; Configuration Layer Parameters
;;---------------------------------------------------------------------
(defconst zv-configuration-layer-directory
  (expand-file-name (concat configuration-layer-contrib-directory "usr/zv/"))
  "zv contribution layer base directory.")

(setq spacemacs-repository "emacs.d")
(setq spacemacs-repository-owner "zv")

(setq-default
 ;; Org Mode
 org-directory (expand-file-name "~/org")
 ;; ERC
 zv-erc-directory (expand-file-name (concat user-emacs-directory ".erc/"))
 ignored-irc-commands '("JOIN" "PART" "QUIT" "NICK" "AWAY")
 ;; C Mode
 c-electric-mode t
 c-basic-offset  4
 ;; Javascript
 js2-global-externs '("module" "assert" "buster" "clearInterval" "clearTimeout" "console"
                      "__dirname" "JSON" "location" "refute" "require" "setInterval" "setTimeout"
                      "sinon" "Quad" "quad" "DS")
 js2-basic-offset                 2
 ;; js2-strict-missing-semi-warning  t
 js2-include-node-externs         t
 js2-include-browser-externs      t
 )

;; Additional emacs modes ------------------------------------
(mapc (lambda (x) (evil-set-initial-state x 'emacs))
      '(
        epa-key-list-mode
        epa-key-mode
        epa-mail-mode
        Info-mode
        Man-mode
        ))

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist `(("/home/.*" . ,temporary-file-directory))
      auto-save-default t
      auto-save-file-name-transforms `(("/home/.*" ,temporary-file-directory t)))

;; Fix this dagnabbit evil-escape f/t issue
(define-key evil-motion-state-map "f" 'evil-find-char)
(define-key evil-motion-state-map "F" 'evil-find-char-backward)
(define-key evil-motion-state-map "t" 'evil-find-char-to)
(define-key evil-motion-state-map "T" 'evil-find-char-to-backward)

;; default browser used
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome")

;; tern
(setq tern-command '("node" "/bin/tern"))

;; tramp
(require 'tramp)
(setq tramp-default-method "ssh")
(add-to-list 'tramp-default-method-alist '("" "zv" "ssh"))
(add-to-list 'tramp-default-method-alist
             '("\\`localhost\\'" "\\`root\\'" "su"))
(add-to-list 'tramp-default-method-alist
             '("\\`localhost\\'" "\\`root\\'" "sudo"))
(add-to-list 'tramp-default-method-alist
             '("\\`sigstkflt\\'" "\\`root\\'" "sudo"))
(add-to-list 'tramp-default-method-alist
             '("\\`sigstkflt\\'" "\\`root\\'" "sudo"))

(add-to-list 'tramp-default-user-alist '("su" "localhost" "root"))
(add-to-list 'tramp-default-user-alist '("sudo" "localhost" "root"))

;; Use .authinfo for local su
(tramp-set-completion-function "su" '((tramp-parse-netrc "~/.authinfo")))
(tramp-set-completion-function "sudo" '((tramp-parse-netrc "~/.authinfo")))

;; VC Mode
(eval-after-load 'vc
  '(progn
     (define-key vc-git-log-view-mode-map "j" 'log-view-msg-next)
     (define-key vc-git-log-view-mode-map "J" 'log-view-file-next)
     (define-key vc-git-log-view-mode-map "k" 'log-view-msg-prev)
     (define-key vc-git-log-view-mode-map "K" 'log-view-file-prev)
     (define-key vc-git-log-view-mode-map (kbd "<RET>") 'log-view-find-revision)))

(setq
 ;; Don't make backups of git history files
 vc-make-backup-files nil
 ;; Always follow a symlink inside of a git repository that slnz things
 vc-follow-symlinks t)


;; encrypt hook ------------------------------------------------------------------

"Install a hook to encrypt some files after saving"
(defun zv/encrypt-secrets ()
  "Encrypt this file if it is in one of our `dirs-to-encrypt'"
  (require 'epa-mail)
  (let* ((zv-dotfiles (expand-file-name "~/Development/dotfilez/"))
         (files-to-encrypt `(,(expand-file-name "~/.authinfo")))
         (dirs-to-encrypt `(,(expand-file-name "~/.gnupg")
                            ,(expand-file-name (concat org-directory "/"))
                            ,(concat zv-dotfiles "gnupg/")
                            ,(concat zv-dotfiles "ssh/")
                            ,(expand-file-name "~/.ssh/")))
         (recipient (epg-list-keys (epg-make-context epa-protocol) "<zv@nxvr.org>" 'public)))
    (when (or (member (file-name-directory (buffer-file-name)) dirs-to-encrypt) (member buffer-file-name files-to-encrypt))
      (epa-encrypt-file (buffer-file-name) recipient))))

(add-hook 'after-save-hook 'zv/encrypt-secrets)

;; hippie expand is dabbrev expand on steroids
(setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-list
                                         try-expand-line
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))

(setq dotspacemacs-themes '(solarized-dark
                            solarized-dark
                            leuven))

;; dired -----------------------------------------------------
(eval-after-load 'dired
  '(progn (define-key dired-mode-map "u" 'dired-up-directory )
          (define-key dired-mode-map "j" 'dired-next-line )
          (define-key dired-mode-map "k" 'dired-prev-line )
          (define-key dired-mode-map "f" 'dired-goto-file )
          (define-key dired-mode-map "\C-h" 'dired-tree-up)
          (define-key dired-mode-map "\C-l" 'dired-tree-down)
          (define-key dired-mode-map "\C-j"'dired-next-subdir)
          (define-key dired-mode-map "\C-k"'dired-prev-subdir)
          ;; dired-do-hardlink hard link [h]
          ;; dired-do-load
          (define-key dired-mode-map "r" 'dired-unmark )
          (define-key dired-mode-map (kbd "<f5>") 'dired-do-redisplay )))

(eval-after-load 'calendar
  '(progn
     (evil-set-initial-state 'calendar-mode 'emacs)
     (setq diary-file (concat org-directory "appointments"))
     (add-hook 'calendar-mode-hook
               (lambda ()
                 (define-key calendar-mode-map "l" 'calendar-forward-day)
                 (define-key calendar-mode-map "h" 'calendar-backward-day)
                 (define-key calendar-mode-map "j" 'calendar-forward-week)
                 (define-key calendar-mode-map "k" 'calendar-backward-week)
                 (define-key calendar-mode-map "{" 'calendar-forward-month)
                 (define-key calendar-mode-map "}" 'calendar-backward-month)
                 (define-key calendar-mode-map "0" 'calendar-beginning-of-week)
                 (define-key calendar-mode-map "$" 'calendar-end-of-week)
                 (define-key calendar-mode-map "[" 'calendar-beginning-of-month)
                 (define-key calendar-mode-map "]" 'calendar-end-of-month)
                 (define-key calendar-mode-map "gg" 'calendar-beginning-of-year)
                 (define-key calendar-mode-map "G" 'calendar-end-of-year)))))

(eval-after-load 'calc
  '(progn
     (define-key calc-mode-map next-buffer-key 'next-buffer)))

(eval-after-load 'eshell
  '(progn
     (require 'em-smart)
     ;; Ensure we set the path correctly
     (setq   eshell-path-env (concat "/usr/local/bin" ":" eshell-path-env))
     ;; Ensure eshell
     (evil-define-key 'normal eshell-mode-map (kbd "0") 'eshell-bol)
     (evil-define-key 'normal eshell-mode-map (kbd "C-p") 'eshell-previous-prompt)
     (evil-define-key 'normal eshell-mode-map (kbd "C-n") 'eshell-next-prompt)
     (evil-define-key 'normal eshell-mode-map (kbd "i") 'evil-insert-state)

     (setq eshell-prompt-regexp "^[^#$\n]*[#$] "
           eshell-review-quick-commands   nil
           eshell-smart-space-goes-to-end t
           eshell-where-to-jump           'begin
           eshell-buffer-maximum-lines     20000
           eshell-buffer-shorthand         t)))


(eval-after-load 'web-mode
  '(progn
     ;; web-mode ------------------------------------------------
     (setq web-mode-enable-css-colorization t
           web-mode-markup-indent-offset 2
           web-mode-css-indent-offset 2
           web-mode-code-indent-offset 2)

     (add-hook 'web-mode-hook (lambda () (turn-off-smartparens-mode)))))

(eval-after-load 'eww
  '(progn
     (define-key eww-mode-map "q" 'eww-quit)
     (define-key eww-mode-map "\C-h" 'eww-previous-url)
     (define-key eww-mode-map "\C-l" 'eww-next-url)))

(use-package man
  :init
  (evil-set-initial-state 'Man-mode 'motion)
  :config
  (evil-define-key 'motion Man-mode-map
    " "    'scroll-up-command
    "\177" 'scroll-down-command
    "["    'Man-next-section
    "]"    'Man-previous-section
    ">"    'end-of-buffer
    "<"    'beginning-of-buffer
    "."    'beginning-of-buffer
    "d"    'scroll-up-command
    "u"    'scroll-down-command
    "gs"   'Man-goto-section
    "ga"   'Man-goto-see-also-section
    "q"    'Man-quit
    "m"    'man))
