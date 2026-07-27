(use-package vterm
  :ensure t
  :config
  (define-key vterm-mode-map (kbd "C-S-v") 'vterm-yank)
  (add-hook 'vterm-mode-hook (lambda () (display-line-numbers-mode -1)))
  (define-key vterm-mode-map (kbd "C-d") '(lambda() (interactive) (vterm--self-insert) (delete-window)))
  (global-set-key (kbd "M-[") 'vterm))


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
        fzf/window-height 15)
  (global-set-key (kbd "M-o") 'fzf))


(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode))


(use-package fasm-mode
  :ensure t
  :vc (:url "https://github.com/emacsattic/fasm-mode.git")
  :config
  (add-hook 'fasm-mode-hook (lambda () (electric-indent-local-mode -1))))


(use-package surround
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


(use-package flash-emacs
  :ensure t
  :vc (:url "https://github.com/JiaweiChenC/flash-emacs.git")
  :config
  (global-set-key (kbd "M-z") 'flash-emacs-jump))


(use-package lsp-mode
  :ensure t
  :vc(:url "https://github.com/emacs-lsp/lsp-mode.git")
  :config
  (add-hook 'python-mode-hook #'lsp)
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
  (with-eval-after-load 'lsp-mode
    (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)))


(use-package odin-mode
  :ensure t
  :vc(:url "https://github.com/mattt-b/odin-mode.git"))


(use-package lsp-pyright
  :ensure t
  :custom (lsp-pyright-langserver-command "basedpyright") ;; or pyright
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))  ; or lsp-deferred


(use-package lsp-ui
  :ensure t
  :config
  (setq lsp-ui-doc-show-with-cursor t)
  (setq lsp-ui-doc-show-with-mouse nil)
  (setq lsp-ui-sideline-update-mode "line")
  (setq lsp-ui-sideline-show-code-actions t))


(use-package ruff-format
  :ensure t
  :config
  (add-hook 'python-mode-hook 'ruff-format-on-save-mode))


(use-package lambda-line
  :ensure t
  :vc (:url "https://codeberg.org/Lambda-Emacs/lambda-line" :rev :newest) ;; Emacs 30+
  :custom
  (lambda-line-icon-time t) ;; requires ClockFace font (see below)
  (lambda-line-clockface-update-fontset "ClockFaceRect") ;; set clock icon
  (lambda-line-position 'top) ;; Set position of status-line
  (lambda-line-abbrev t) ;; abbreviate major modes
  (lambda-line-hspace "  ")  ;; add some cushion
  (lambda-line-prefix t) ;; use a prefix symbol
  (lambda-line-prefix-padding nil) ;; no extra space for prefix
  (lambda-line-status-invert nil)  ;; no invert colors
  (lambda-line-gui-ro-symbol  " ⨂") ;; symbols
  (lambda-line-gui-mod-symbol " ⬤")
  (lambda-line-gui-rw-symbol  " ◯")
  (lambda-line-space-top +.50)  ;; padding on top and bottom of line
  (lambda-line-space-bottom -.50)
  (lambda-line-symbol-position 0.1) ;; adjust the vertical placement of symbol
  :config
  ;; activate lambda-line
  (lambda-line-mode)
  ;; set divider line in footer
  (when (eq lambda-line-position 'top)
    (setq-default mode-line-format (list "%_"))
    (setq mode-line-format (list "%_"))))
