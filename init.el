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


;; Maximize window at startup
(add-to-list 'initial-frame-alist '(fullscreen . maximized))


(setq-default evil-want-keybinding nil)


(global-evil-leader-mode)



(setq-default truncate-lines t)



(global-set-key (kbd "C-c r") 'eglot-rename)


(evil-leader/set-leader "<SPC>")


;; Set keys for leader key
(evil-leader/set-key
  "f" 'find-file
  "d" 'dired
  "pf" 'projectile-find-file
  "pd" 'projectile-find-dir
  "b" 'ibuffer
  "k" 'kill-current-buffer
  "pc" 'project-compile
  "tt" 'vterm
  "pt" 'projectile-run-vterm
  "/" 'projectile-ripgrep
  "\"" 'comment-line
  "<SPC>" 'fzf
  "e" 'treemacs)



(global-set-key (kbd "C-<tab>") 'indent-rigidly)



(use-package vterm
  :ensure t
  :config
  (define-key vterm-mode-map (kbd "C-S-v") 'vterm-yank))



;; Enable cua mode by default to normal C+c, C+v
(cua-mode t)


(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-badger t))



;; (use-package kanagawa-themes
;;   :ensure t
;;   :config
;;   (load-theme 'kanagawa-wave t))



(use-package eldoc-box
  :ensure t
  :config
  (setq-default eldoc-box-hover-at-point-mode t)
  (setq-default eldoc-box-max-pixel-height 80))



(global-set-key (kbd "C-x C-a") 'eval-buffer)


;; Disable annoyning bell
(setq ring-bell-function 'ignore)




(centaur-tabs-mode)




(defun open-term()
  "Open terminal in a splitted below window."
  (interactive)
  (windmove-display-down)
  (projectile-run-vterm))



(global-set-key (kbd "C-x C-o") 'open-term)



;; Load evil mode, cause i'm vimloved
;; (evil-mode)



(use-package evil-goggles
  :ensure t
  :config
  (evil-goggles-mode)

  ;; optionally use diff-mode's faces; as a result, deleted text
  ;; will be highlighed with `diff-removed` face which is typically
  ;; some red color (as defined by the color theme)
  ;; other faces such as `diff-added` will be used for other actions
  (evil-goggles-use-diff-faces))



(evil-collection-init)




(global-set-key (kbd "<backtab>") 'indent-rigidly-left-to-tab-stop)





;; Enable mood line
(mood-line-mode)


(evil-define-key 'insert vterm-mode-map (kbd "C-d") 'vterm--self-insert)


;; Hints about keys
(which-key-mode)

;; Set cursor style to fancy bar
(setq-default cursor-type 'bar)

;; Set tab to insert tab, btw
(global-set-key (kbd "TAB") 'tab-to-tab-stop)

;; Enable better buffer control unit
(global-unset-key (kbd "C-x C-b"))
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Open vterm
(global-set-key (kbd "C-x C-\\") 'vterm)


;; Set default font bigger
(set-frame-font "JetBrainsMono Nerd Font 12" nil t)

(setq buffer-face-mode-face '(:family "JetBrainsMono Nerd Font": height 110))
(buffer-face-mode t)

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

;; define backtab key
(global-set-key (kbd "<backtab>") 'un-indent-by-removing-4-spaces)

;; setup indentation
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; define kbd to kill buffer
(global-set-key (kbd "s-k") 'kill-current-buffer)

(defun open-config-file()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

;; Set open config kbd
(global-set-key (kbd "C-h C-c") 'open-config-file)


(global-unset-key (kbd "C-x f"))
(global-set-key (kbd "C-x f") 'fzf-projectile)



(ivy-mode)



(global-unset-key (kbd "M-k"))
(global-set-key (kbd "M-p") 'ace-window)
(global-set-key (kbd "M-k") 'ace-delete-window)
(ace-window-display-mode)
;; (ace-window-posframe-mode)



(setq backup-directory-alist '((".*" . "~/.Trash")))



;; Disable warnings on startup
(setq warning-minimum-level :error)

;; Enable company mode by default
(global-company-mode)

;; Verilog mode disable auto formatting
(eval-after-load 'verilog-mode
    '(progn
        ;; same for all the electric-verilog-* commands in
        ;; the mode's map (see verilog-mode.el)
        (define-key verilog-mode-map (kbd ";") 'self-insert-command)
        (define-key verilog-mode-map (kbd ":") 'self-insert-command)
        (define-key verilog-mode-map (kbd "RET") 'newline-and-indent)
        (define-key verilog-mode-map (kbd "TAB") 'tab-to-tab-stop)
        (define-key verilog-mode-map (kbd "`") 'self-insert-command)
        (define-key verilog-mode-map (kbd "C-M-f") 'forward-sexp)
        (define-key verilog-mode-map (kbd "C-M-b") 'backward-sexp)
        (define-key verilog-mode-map (kbd "C-;") 'self-insert-command)
        (define-key verilog-mode-map (kbd "M-RET") 'newline)))


(global-display-line-numbers-mode)



(global-set-key (kbd "TAB") 'tab-to-tab-stop)



;; (add-hook 'verilog-mode-hook
;;           '(lambda ()
;;              (define-key verilog-mode-map (kbd "RET") 'self-insert-command))



;; Set verible-verilog-ls as lsp for verilog
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(verilog-mode . ("verible-verilog-ls" "--rules=-parameter-name-style,-case-missing-default"))))

;; Set jedi ls to python mode
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
	           '(python-mode . ("jedi-language-server"))))


(setq-default hs-minor-mode t)


(add-hook 'python-mode-hook 'eglot-ensure)
(add-hook 'python-mode-hook #'eldoc-box-hover-at-point-mode t)

(add-hook 'verilog-mode 'eglot-ensure)
;; (add-hook 'verilog-mode-hook #'eldoc-box-hover-at-point-mode t)


;; (define-key treemacs-mode-map
;; (global-set-key (kbd "<left>") 'treemacs-COLLAPSE-action)
;; (global-set-key (kbd "<right>") 'treemacs-RET-action)

(set-default 'truncate-long-lines t)


(global-unset-key (kbd "C-x C-p"))



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
  (global-set-key (kbd "M-<down>") 'centaur-tabs-forward-tab))



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(buffer-face-mode-face '(:family "JetBrainsMono Nerd Font" : height 110))
 '(custom-safe-themes
   '("4d5d11bfef87416d85673947e3ca3d3d5d985ad57b02a7bb2e32beaf785a100e"
     "b5fd9c7429d52190235f2383e47d340d7ff769f141cd8f9e7a4629a81abc6b19"
     "456697e914823ee45365b843c89fbc79191fdbaff471b29aad9dcbe0ee1d5641"
     "0325a6b5eea7e5febae709dab35ec8648908af12cf2d2b569bedc8da0a3a81c1"
     "daa27dcbe26a280a9425ee90dc7458d85bd540482b93e9fa94d4f43327128077"
     "c20728f5c0cb50972b50c929b004a7496d3f2e2ded387bf870f89da25793bb44"
     "d2ab3d4f005a9ad4fb789a8f65606c72f30ce9d281a9e42da55f7f4b9ef5bfc6"
     "5c7720c63b729140ed88cf35413f36c728ab7c70f8cd8422d9ee1cedeb618de5"
     default))
 '(dired-clean-confirm-killing-deleted-buffers nil)
 '(dired-confirm-shell-command nil)
 '(dired-create-destination-dirs 'ask)
 '(dired-hide-details-preserved-columns '(1))
 '(dired-kill-when-opening-new-dired-buffer t)
 '(dired-listing-switches "-al")
 '(dired-no-confirm
   '(byte-compile chgrp chmod chown compress copy delete hardlink load
                  move print shell symlink touch uncompress))
 '(dired-switches-in-mode-line 'as-is)
 '(list-directory-verbose-switches "-al")
 '(package-selected-packages
   '(all-the-icons centaur-tabs company dape eglot-inactive-regions
                   eldoc-box fzf ivy kanagawa-themes modern-tab-bar
                   mood-line multiple-cursors treemacs-tab-bar
                   ultra-scroll verilog-ext verilog-ts-mode vterm
                   yaml-mode yasnippet zig-mode zig-ts-mode))
 '(package-vc-selected-packages
   '((modern-tab-bar :vc-backend Git :url
                     "https://github.com/aaronjensen/emacs-modern-tab-bar.git")
     (ultra-scroll :vc-backend Git :url
                   "https://github.com/jdtsmith/ultra-scroll")))
 '(treemacs-move-files-by-mouse-dragging t)
 '(verilog-auto-newline nil)
 '(verilog-indent-level 0)
 '(verilog-indent-level-behavioral 0)
 '(verilog-indent-level-declaration 0)
 '(verilog-indent-level-directive 0)
 '(verilog-indent-level-module 0)
 '(verilog-tab-always-indent nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-goggles-change-face ((t (:inherit diff-removed))))
 '(evil-goggles-delete-face ((t (:inherit diff-removed))))
 '(evil-goggles-paste-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-add-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-change-face ((t (:inherit diff-changed))))
 '(evil-goggles-undo-redo-remove-face ((t (:inherit diff-removed))))
 '(evil-goggles-yank-face ((t (:inherit diff-changed))))
 '(tab-bar ((t (:inherit mode-line)))))
(put 'scroll-left 'disabled nil)
