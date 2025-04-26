;; Enable melpa packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;; Disable ugly ui
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)


(ivy-mode)


(global-evil-leader-mode)
(evil-leader/set-leader "<SPC>")


(evil-leader/set-key
  "pf" 'projectile-find-file
  "b" 'ibuffer
  "k" 'kill-current-buffer
  "p?" 'projectile-ripgrep
  "<SPC>" 'fzf
  "t" 'vterm
  "pt" 'projectile--vterm
  "f" 'find-file
  "/" 'comment-line)



(global-set-key (kbd "C-x C-a") 'eval-buffer)



;; Enable vterm
(use-package vterm
  :ensure t)

;; Enable cua mode by default to normal C+c, C+v
(cua-mode t)


(global-set-key (kbd "M-p") 'ace-window)
(ace-window-display-mode)
(ace-window-posframe-mode)


;; Load evil mode, cause i'm vimloved
;; (evil-mode)

;; Load fancy default theme
(use-package kanagawa-themes
  :ensure t
  :config
  (load-theme 'kanagawa-wave t))


(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-one t))

;; Maximize window at startup
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; Indentation setup
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Disable annoyning bell
(setq ring-bell-function 'ignore)

;; Enable which key
(which-key-mode)

;; Set cursor style to fancy bar
(setq-default cursor-type 'bar)

(global-set-key (kbd "s-v") 'vterm-yank)
(global-set-key (kbd "C-v") 'vterm-yank)

;; Setup font
(set-face-attribute 'default nil :height 170)

;; Set tab to insert tab, btw
(global-set-key (kbd "TAB") 'tab-to-tab-stop)

;; Enable better buffer control unit
(global-unset-key (kbd "C-x C-b"))
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Open vterm
(global-set-key (kbd "C-x C-\\") 'vterm)

;; Function that will delete 4 spaces
(defun un-indent-by-removing-4-spaces ()
  "Remove 4 spaces from beginning of of line."
  (interactive)
  (save-excursion
    (save-match-data
      (beginning-of-line)
      ;; get rid of tabs at beginning of line
      (when (looking-at "^\\s-+")
        (untabify (match-beginning 0) (match-end 0)))
      (when (looking-at "^    ")
        (replace-match "")))))

;; Define backtab key
;; (global-set-key (kbd "<backtab>") 'un-indent-by-removing-4-spaces)


(global-set-key (kbd "<backtab>") 'indent-rigidly-left-to-tab-stop)


;; Enable mood-line
(mood-line-mode t)

;; Enable ultra scroll mode
(ultra-scroll-mode)

;; Disable warnings on emacs startup
(setq warning-minimum-level :emergency)

;; Define kbd to kill buffer
(global-set-key (kbd "s-k") 'kill-current-buffer)
(global-set-key (kbd "M-k") 'kill-current-buffer)

(treemacs-resize-icons 15)

;; Verilog mode disable auto formatting
(eval-after-load 'verilog-mode
    '(progn
        ;; same for all the electric-verilog-* commands in
        ;; the mode's map (see verilog-mode.el)
        (define-key verilog-mode-map (kbd ";") 'self-insert-command)
        (define-key verilog-mode-map (kbd ":") 'self-insert-command)
        (define-key verilog-mode-map (kbd "RET") 'newline)
        (define-key verilog-mode-map (kbd "TAB") 'tab-to-tab-stop)
        (define-key verilog-mode-map (kbd "`") 'self-insert-command)
        (define-key verilog-mode-map (kbd "C-M-f") 'forward-sexp)
        (define-key verilog-mode-map (kbd "C-M-b") 'backward-sexp)
        (define-key verilog-mode-map (kbd "C-;") 'self-insert-command)
        (define-key verilog-mode-map (kbd "M-RET") 'newline)))

;; Set verible-verilog-ls as lsp for verilog
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(verilog-mode . ("/Users/ilya/.local/bin/verible-verilog-ls" "--rules=-parameter-name-style,-case-missing-default"))))

;; Add jedi for python mode
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(python-mode . ("/opt/homebrew/bin/jedi-language-server"))))

(defun open-config-file()
  "Open config file."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

;; Open that config file
(global-unset-key (kbd "C-h C-c"))
(global-set-key (kbd "C-h C-c") 'open-config-file)

;; Enable global company mode
(global-company-mode)

;; Set commands to move between paragraphs
(global-set-key (kbd "s-<up>") 'backward-paragraph)
(global-set-key (kbd "s-<down>") 'forward-paragraph)
(global-set-key (kbd "M-<up>") 'backward-paragraph)
(global-set-key (kbd "M-<down>") 'forward-paragraph)


;; Redefine behaviour of <end>, <home> keys
(global-unset-key (kbd "<end>"))
(global-unset-key (kbd "<home>"))
(global-set-key (kbd "<end>") 'end-of-line)
(global-set-key (kbd "<home>") 'beginning-of-line)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("daa27dcbe26a280a9425ee90dc7458d85bd540482b93e9fa94d4f43327128077"
     default))
 '(dired-kill-when-opening-new-dired-buffer t)
 '(package-selected-packages
   '(company dape doom-themes eglot-inactive-regions evil-leader
             evil-surround fzf ivy kanagawa-themes lua-mode mood-line
             projectile projectile-ripgrep python-mode rust-mode
             ultra-scroll verilog-ts-mode vterm zig-mode))
 '(package-vc-selected-packages
   '((ultra-scroll :vc-backend Git :url
                   "https://github.com/jdtsmith/ultra-scroll")))
 '(verilog-indent-level 4)
 '(verilog-indent-level-behavioral 4)
 '(verilog-indent-level-declaration 4)
 '(verilog-indent-level-module 4)
 '(verilog-tab-always-indent nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
