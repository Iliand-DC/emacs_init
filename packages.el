(use-package vterm
  :ensure t
  :config
  (define-key vterm-mode-map (kbd "C-S-v") 'vterm-yank)
  (add-hook 'vterm-mode-hook (lambda () (display-line-numbers-mode -1)))
  (define-key vterm-mode-map (kbd "C-d") '(lambda() (interactive) (vterm--self-insert) (delete-window))))


(use-package neotree
  :ensure t
  :config
  (global-set-key (kbd "C-b") 'neotree-toggle)
  (global-set-key (kbd "C-e") 'neotree-show)
  (setq neo-theme (if (display-graphic-p) 'nerd-icons 'arrow))
  (add-hook 'neotree-mode-hook (lambda() (display-line-numbers-mode -1)))
  (add-hook 'neo-after-create-hook (lambda (_)(if (display-graphic-p) (call-interactively 'text-scale-once)))))


(use-package multiple-cursors
  :ensure t
  :config
  (multiple-cursors-mode)
  (global-set-key (kbd "M-S-<up>") 'mc/mark-previous-like-this)
  (global-set-key (kbd "M-S-<down>") 'mc/mark-next-like-this))


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


(use-package nerd-icons
  :vc (:url "https://github.com/rainstormstudio/nerd-icons.el.git"))


(use-package kaolin-themes
  :vc (:url "https://github.com/ogdenwebb/emacs-kaolin-themes.git")
  :ensure t
  :config
  (load-theme 'kaolin-bubblegum t))


(use-package nyan-mode
  :ensure t
  :config
  (nyan-mode))


(use-package ultra-scroll
  :ensure t
  :config
  (ultra-scroll-mode))


(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (python-mode . lsp)
         (verilog-ts-mode . lsp)
         (c-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)


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
        fzf/position-bottom t
        fzf/window-height 15))
