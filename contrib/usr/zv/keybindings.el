;; ---------------------------------------------------------------------------
;; Prefixes
;; ---------------------------------------------------------------------------
;; swap C-j for C-x prefix keys
;; (global-set-key (kbd "C-j") ctl-x-map)

(add-to-list 'spacemacs/key-binding-prefixes '("ar" . "applications-repl"))

;; global bindings
;; ---------------------------------------------------------------------------
;; tab/window split manipulation]
(define-key evil-normal-state-map "Q" 'evil-quit)
(global-set-key next-buffer-key 'evil-window-next)
(global-set-key prev-buffer-key 'evil-window-prev)
(global-set-key "\C-\M-j" (lambda () (interactive) (rotate-windows 1)))
(global-set-key "\C-\M-k" (lambda () (interactive) (rotate-windows -1)))
(global-set-key "\M-h" (lambda () (interactive) (zv/enlarge-window-by-dominant-dimension -10)))
(global-set-key "\M-l" (lambda () (interactive) (zv/enlarge-window-by-dominant-dimension 10)))
(global-set-key (kbd "C-M-<return>") 'zv/tile-split-window)
(global-set-key (kbd "<Scroll_Lock>") 'scroll-lock-mode)

;; buffer
(global-set-key (kbd "C-x C-k") 'switch-to-prev-buffer)
(global-set-key (kbd "C-x C-j") 'switch-to-next-buffer)

;; utilities
(global-set-key (kbd "C-x l") 'find-library)
(global-set-key (kbd "C-x C-r") 're-builder)
(global-set-key (kbd "M-/") 'hippie-expand)

;; applications
(global-set-key (kbd "<XF86Calculator>") 'calc)
(global-set-key (kbd "<XF86Mail>") 'gnus)

(defun zv/swap-bracket-behavior ()
  "Swap the behavior of our brackets between section/paragraph jumping"
  (interactive)
  (if (eq 'evil-backward-paragraph (lookup-key evil-motion-state-map "["))
      (progn (define-key evil-motion-state-map "}" 'evil-forward-paragraph)
             (define-key evil-motion-state-map "{" 'evil-backward-paragraph)
             (define-key evil-motion-state-map "]" 'evil-forward-section-begin)
             (define-key evil-motion-state-map "[" 'evil-backward-section-begin))

      (progn (define-key evil-motion-state-map "]" 'evil-forward-paragraph)
             (define-key evil-motion-state-map "[" 'evil-backward-paragraph)
             (define-key evil-motion-state-map "}" 'evil-forward-section-begin)
             (define-key evil-motion-state-map "{" 'evil-backward-section-begin))))

(evil-leader/set-key
  "t[" 'zv/swap-bracket-behavior)

;; previously was evil-lookup
(defun zv/join-up ()
  "hacky way to join parent's lines"
  (interactive)
  (save-excursion
    (progn
      (previous-line 2)
      (evil-join (point) (+ 1 (point))))))

(define-key evil-normal-state-map "K" 'zv/join-up)

(evil-leader/set-key
  ;; helm-find
  "hf" 'helm-ag)

;; ---------------------------------------------------------------------------
;; evil state bindings
;; ---------------------------------------------------------------------------
(setq evil-cross-lines t)
(define-key evil-normal-state-map "\C-p" 'helm-projectile-find-file)
(define-key evil-normal-state-map (kbd "RET") 'evil-scroll-down)
(define-key evil-normal-state-map (kbd "<backspace>") 'evil-scroll-up)

;; Although this is `helm' package configuration, this is more
;; accurately a keybinding for a very, very commonly used function
(use-package helm
  :idle
  :config
  ;; I literally have never used the default `jj' keybinding.
  ;; Good riddance
  (evil-leader/set-key "jj" 'helm-mini))

;; insert mode
(define-key evil-insert-state-map (kbd "C-h") 'backward-char)
(define-key evil-insert-state-map (kbd "C-l") 'forward-char)
;; (define-key evil-insert-state-map (kbd "C-d") 'delete-char)
;; (define-key evil-insert-state-map (kbd "C-i") 'backward-delete-char)
;; (define-key evil-insert-state-map (kbd "C-s") 'undo-tree-undo)

;; visual mode
(define-key evil-visual-state-map (kbd "C-e") 'eval-region)

;; As I never distinguish between [[ & [{, I might as well get the
;; benefit of use of the easier one
(define-key evil-motion-state-map "]" 'evil-forward-paragraph)
(define-key evil-motion-state-map "[" 'evil-backward-paragraph)
(define-key evil-motion-state-map "}" 'evil-forward-section-begin)
(define-key evil-motion-state-map "{" 'evil-backward-section-begin)
(define-key evil-motion-state-map (kbd "M-]") 'evil-forward-section-end)
(define-key evil-motion-state-map (kbd "M-[") 'evil-backward-section-end)
;;(define-key evil-motion-state-map "(" 'evil-previous-open-paren)
;;(define-key evil-motion-state-map ")" 'evil-next-close-paren)
(define-key evil-motion-state-map (kbd "M-{") 'evil-previous-open-brace)
(define-key evil-motion-state-map (kbd "M-}") 'evil-next-close-brace)

;; H/L should go to the first / last non blank character respectively
;; (define-key evil-visual-state-map "L" 'evil-last-non-blank)
;; (define-key evil-visual-state-map "H" 'evil-first-non-blank)
;; (define-key evil-normal-state-map "L" 'evil-last-non-blank)
;; (define-key evil-normal-state-map "H" 'evil-first-non-blank)

;; Add surround-inner-word keybindings at s{char} and S{char} appropriately
;; e.x 9" -> ysiw"
(let ((inner-word-key "s")
      (outer-word-key "S"))
  (progn
  (define-key evil-normal-state-map inner-word-key nil)
  (define-key evil-normal-state-map outer-word-key nil)
  (mapcar (lambda (char)
            (define-key evil-normal-state-map (concat inner-word-key char) (concat "ysiw" char))
            (define-key evil-normal-state-map (concat outer-word-key char) (concat "ysiW" char))
            ) '("'" "\"" ")" "(" "[" "]" "{" "}"))))


;; ---------------------------------------------------------------------------
;; evil-leader key bindings
;; ---------------------------------------------------------------------------
(evil-leader/set-key
  ;; applications
  "ai" 'erc-connect
  "aw" 'woman
  "am" 'man
  "ag" 'gnus
  ;; Should check if we're in a visual select mode really.
  "an" 'remember
  ;; repls
  "arn" 'nodejs-repl
  "are" 'ielm
  ;; [l]ast
  ;; "l" 'previous-buffer
  ;; temporary hack to fix sc remote highlihg
  "sc" 'evil-search-highlight-persist-remove-all
  ;; Align keybinding
  "al" 'align-regexp
  ";"  'evilnc-comment-operator
  "cl" 'evilnc-comment-or-uncomment-lines
  "ci" 'evilnc-toggle-invert-comment-line-by-line
  "cp" 'evilnc-comment-or-uncomment-paragraphs
  "ct" 'evilnc-quick-comment-or-uncomment-to-the-line
  "cy" 'evilnc-copy-and-comment-lines)

(defun zv//initial-path-keybinding (key-file-map)
  "Create leader keybindings from an alist of the form (KEYS . PATH)"
  (mapc (lambda (binding)
          (let* ((path        (cdr binding))
                 (keybinding  (car binding)))
            (evil-leader/set-key keybinding (if (string-match "\/$" path)
                                                ;; use ido-find-file-in-dir if we're binding a directory
                                                `(lambda () (interactive) (ido-find-file-in-dir ,path))
                                              ;; Otherwise we're looking at a file, jump directly to it
                                              `(lambda () (interactive) (find-file-existing ,path))))))
        key-file-map))

(zv//initial-path-keybinding `(("fea" . "~/Development/quad/newadmin")
                               ("feq" . "~/Development/quad")
                               ("fez" . "~/.emacs.d/contrib/zv/")
                               ("feg" . "~/.gnus.el")
                               ("fzd" . "~/Development/dotfilez")
                               ("fer" . "~/.emacs.d/.ercrc.el")
                               ("fzo" . ,org-directory)))

;; ---------------------------------------------------------------------------
;; mode bindings
;; ---------------------------------------------------------------------------
;; Magit
(eval-after-load 'magit
  (evil-leader/set-key-for-mode 'magit-status-mode
    "mf" 'magit-key-mode-popup-gitflow))

;; cc mode
(define-key c-mode-map next-buffer-key 'evil-window-next)
(evil-leader/set-key-for-mode 'c-mode
  ;; guess style
  "mq" 'c-guess)

(evil-leader/set-key-for-mode 'emacs-lisp-mode "mxe" 'eval-region)

;; js2 mode
(eval-after-load 'js2-mode
  '(progn
     (define-key js2-mode-map (kbd "H-s") 'add-semicolon-to-end-of-line)
     (define-key js2-mode-map next-buffer-key 'evil-window-next)
     (define-key js2-mode-map prev-buffer-key 'evil-window-prev)))

;; calc
;;; These keys are typically bound to `kill line' which I rarely use.
(eval-after-load "calc"
  (lambda () (define-key calc-mode-map next-buffer-key 'evil-window-prev)))

;; delete line

(eval-after-load 'helm
  '(define-key helm-map (kbd "C-u") 'helm-delete-minibuffer-contents))


;; evil leader eval region
(evil-leader/set-key-for-mode 'evil-visual-state-map "xe" 'eval-region)

;; Autocomplete
(eval-after-load 'auto-complete
  '(global-set-key (kbd "<backtab>") 'ac-start))

(eval-after-load 'company
  '(progn
     (define-key company-active-map (kbd "C-j") 'company-select-next)
     (define-key company-active-map (kbd "C-k") 'company-select-previous)
     ;; Don't forget C-s (search candidates)
     (global-set-key (kbd "<backtab>") 'company-complete)))

(eval-after-load 'shell
  '(progn
     (define-key shell-mode-map "\C-d" nil)))

;; Info Mode
(evil-add-hjkl-bindings Info-mode-map 'emacs
  "0" 'evil-digit-argument-or-evil-beginning-of-line
  (kbd "\M-h") 'Info-help   ; "h"
  "/" 'Info-search
  "?" 'Info-search-backward
  "\C-u" 'Info-scroll-down
  "\C-d" 'Info-scroll-up
  "\C-t" 'Info-history-back ; "l"
  "\C-o" 'Info-history-back
  "\C-]" 'Info-follow-nearest-node
  (kbd "DEL") 'Info-scroll-down)

;; neotree
(eval-after-load "neotree"
  (lambda ()
    (define-key neotree-mode-map "p" 'neotree-jump-to-parent)
    (define-key neotree-mode-map "u" 'neotree-up-dir)
    (define-key neotree-mode-map "C" 'neotree-change-root)
    (define-key neotree-mode-map "I" 'neotree-hidden-file-toggle)
    (define-key evil-normal-state-map (kbd "C-\\") 'neotree-find)))
