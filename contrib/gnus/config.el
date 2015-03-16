(evil-define-state gnus
                  "Gnus state -- many features of Evil are irrelevent in gnus"
                  :tag " <G> "
                  :entry-hook nil
                  :exit-hook nil
                  :suppress-keymap
                  :cursor (bar . 2))

;; Article Mode
;; ------------

(define-key gnus-article-mode-map "/" 'evil-search)
