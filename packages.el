(use-package vterm
  :ensure t
  :config
  (define-key vterm-mode-map (kbd "C-S-v") 'vterm-yank)
  (add-hook 'vterm-mode-hook (lambda () (display-line-numbers-mode -1)))
  (define-key vterm-mode-map (kbd "C-d") '(lambda() (interactive) (vterm--self-insert) (delete-window))))


(use-package multiple-cursors
  :ensure t
  :config
  (multiple-cursors-mode)
  (setq mc/always-run-for-all t)
  (global-set-key (kbd "M-S-<up>") 'mc/mark-previous-like-this)
  (global-set-key (kbd "M-S-<down>") 'mc/mark-next-like-this)
  (global-set-key (kbd "M-S-<left>") 'mc/skip-to-previous-like-this)
  (global-set-key (kbd "M-S-<right>") 'mc/skip-to-next-like-this)
  (global-set-key (kbd "M-a") 'mc/mark-all-like-this))


(use-package windmove
  :ensure t
  :config
  (global-set-key (kbd "C-M-<up>") 'windmove-up)
  (global-set-key (kbd "C-M-<left>") 'windmove-left)
  (global-set-key (kbd "C-M-<right>") 'windmove-right)
  (global-set-key (kbd "C-M-<down>") 'windmove-down))


(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))


(use-package git-gutter
  :ensure t
  :config
  (global-git-gutter-mode))


(use-package git-gutter-fringe
  :ensure t
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))


(use-package ultra-scroll
  :ensure t
  :config
  (ultra-scroll-mode))


(use-package fzf
  :ensure t
  :config
  (setq fzf/args "-x --color bw --print-query --margin=1,0 --no-hscroll"
        fzf/executable "fzf"
        fzf/git-grep-args "-i --line-number %s"
        ;; command used for `fzf-grep-*` functions
        ;; example usage for ripgrep:
        ;; fzf/grep-command "rg --no-heading -nH"
        fzf/grep-command "grep -nrH"
        ;; If nil, the fzf buffer will appear at the top of the window
        fzf/position-bottom nil
        fzf/window-height 15))


(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode))


(use-package fasm-mode
  :ensure t
  :vc (:url "https://github.com/emacsattic/fasm-mode.git")
  :config
  (add-hook 'fasm-mode-hook (lambda () (electric-indent-local-mode -1))))


(use-package kaolin-themes
  :ensure t)


(use-package counsel
  :ensure t)


(use-package vertico
  :ensure t
  :config
  (vertico-mode))


(use-package surround
  :ensure t)


(use-package gruber-darker-theme
  :ensure t)


(use-package company
  :ensure t
  :config
  (global-company-mode))


(use-package company-ctags
  :ensure t
  :config
  (with-eval-after-load 'company
    (company-ctags-auto-setup)))


(use-package expand-region
  :ensure t
  :config
  (global-set-key (kbd "C-=") 'er/expand-region))


(load-theme 'gruber-darker t)
