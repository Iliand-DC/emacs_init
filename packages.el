(use-package vterm
  :ensure t
  :config
  (define-key vterm-mode-map (kbd "C-S-v") 'vterm-yank)
  (add-hook 'vterm-mode-hook (lambda () (display-line-numbers-mode -1)))
  (add-hook 'vterm-mode-hook 'centaur-tabs-local-mode)
  (define-key vterm-mode-map (kbd "C-d") '(lambda() (interactive) (vterm--self-insert) (delete-window))))


(use-package neotree
  :ensure t
  :config
  (global-set-key (kbd "C-b") 'neotree-toggle)
  (global-set-key (kbd "C-e") 'neotree-show)
  (setq neo-theme (if (display-graphic-p) 'nerd-icons 'arrow))
  (add-hook 'neotree-mode-hook (lambda() (display-line-numbers-mode -1)))
  (add-hook 'neo-after-create-hook (lambda (_)(if (display-graphic-p) (call-interactively 'text-scale-once)))))


(use-package eldoc-box
  :ensure t
  :config
  (setq-default eldoc-box-hover-mode t)
  (setq-default eldoc-box-max-pixel-height 80))


(use-package multiple-cursors
  :ensure t
  :config
  (multiple-cursors-mode)
  (global-set-key (kbd "M-S-<up>") 'mc/mark-previous-like-this)
  (global-set-key (kbd "M-S-<down>") 'mc/mark-next-like-this))


(use-package treemacs
  :ensure t
  :config
  (define-key treemacs-mode-map (kbd "<right>") 'treemacs-RET-action)
  (define-key treemacs-mode-map (kbd "<left>") 'treemacs-COLLAPSE-action)
  (global-set-key (kbd "M-o") 'treemacs-select-window)
  (treemacs-resize-icons 15))


(use-package centaur-tabs
  :ensure t
  :config
  (global-set-key (kbd "M-<up>") 'centaur-tabs-backward-tab)
  (global-set-key (kbd "M-<down>") 'centaur-tabs-forward-tab)
  (centaur-tabs-mode)
  (centaur-tabs-local-mode))


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


(use-package dap-mode
  :ensure t)


(use-package nerd-icons
  :vc (:url "https://github.com/rainstormstudio/nerd-icons.el.git"))


(use-package doom-modeline
  :ensure t
  :config
  (doom-modeline-mode))


(use-package nyan-mode
  :ensure t
  :config
  (nyan-mode))
