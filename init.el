;; Enable melpa packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;; Disable ugly ui
(scroll-bar-mode -1)
(tool-bar-mode -1)


;; Maximize window at startup
(add-to-list 'initial-frame-alist '(fullscreen . maximized))


(setq-default truncate-lines t)



(global-set-key (kbd "C-c r") 'eglot-rename)


;; No more typing the whole yes or no. Just y or n will do.
(fset 'yes-or-no-p 'y-or-n-p)


(global-set-key (kbd "C-<tab>") 'indent-rigidly)


(global-unset-key (kbd "C-s"))
(global-unset-key (kbd "C-x C-s"))


(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "C-f") 'isearch-forward)
(define-key isearch-mode-map (kbd "C-f") 'isearch-repeat-forward)

(pixel-scroll-precision-mode t)


(use-package vterm
  :ensure t
  :config
  (define-key vterm-mode-map (kbd "C-S-v") 'vterm-yank))


(use-package neotree
  :ensure t
  :config
  (global-set-key (kbd "C-b") 'neotree-toggle)
  (global-set-key (kbd "C-e") 'neotree-show))


;; Enable cua mode by default to normal C+c, C+v
(cua-mode t)

(use-package eldoc-box
  :ensure t
  :config
  (setq-default eldoc-box-hover-at-point-mode t)
  (setq-default eldoc-box-max-pixel-height 80))


(add-to-list 'auto-mode-alist '("\\.s?vh?\\'" . verilog-ts-mode))

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

(global-set-key (kbd "<backtab>") 'indent-rigidly-left-to-tab-stop)

;; Makes *scratch* empty.
(setq initial-scratch-message "")

;; Removes *scratch* from buffer after the mode has been set.
(defun remove-scratch-buffer ()
  (if (get-buffer "*scratch*")
      (kill-buffer "*scratch*")))
(add-hook 'after-change-major-mode-hook 'remove-scratch-buffer)

;; Removes *messages* from the buffer.
(setq-default message-log-max nil)


;; Removes *Completions* from buffer after you've opened a file.
(add-hook 'minibuffer-exit-hook
      '(lambda ()
         (let ((buffer "*Completions*"))
           (and (get-buffer buffer)
                (kill-buffer buffer)))))

;; Don't show *Buffer list* when opening multiple files at the same time.
(setq inhibit-startup-buffer-menu t)

;; Show only one active window when opening multiple files at the same time.
(add-hook 'window-setup-hook 'delete-other-windows)


;; Enable mood line
(mood-line-mode)



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


(global-unset-key (kbd "C-x C-;"))
(global-set-key (kbd "C-;") 'comment-line)


(global-set-key (kbd "C-w") 'kill-current-buffer)


(global-unset-key (kbd "C-a"))
(global-set-key (kbd "C-a") 'avy-goto-char)


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
  "Quick open this config file."
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


;; Disable treemacs line numbers
(add-hook 'treemacs-mode-hook (lambda() (display-line-numbers-mode -1)))


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

(eval-after-load 'verilog-ts-mode
    '(progn
        ;; same for all the electric-verilog-* commands in
        ;; the mode's map (see verilog-mode.el)
        (define-key verilog-ts-mode-map (kbd ";") 'self-insert-command)
        (define-key verilog-ts-mode-map (kbd ":") 'self-insert-command)
        (define-key verilog-ts-mode-map (kbd "RET") 'newline-and-indent)
        (define-key verilog-ts-mode-map (kbd "TAB") 'tab-to-tab-stop)
        (define-key verilog-ts-mode-map (kbd "`") 'self-insert-command)
        (define-key verilog-ts-mode-map (kbd "C-M-f") 'forward-sexp)
        (define-key verilog-ts-mode-map (kbd "C-M-b") 'backward-sexp)
        (define-key verilog-ts-mode-map (kbd "C-;") 'self-insert-command)
        (define-key verilog-ts-mode-map (kbd "M-RET") 'newline)))


(global-display-line-numbers-mode)



(global-set-key (kbd "TAB") 'tab-to-tab-stop)




(defun insert-line-above ()
  "Insert an empty line above the current line."
  (interactive)
  (save-excursion
    (end-of-line 0)
    (open-line 1)))

(global-set-key (kbd "C-c N") 'insert-line-below)


 ;; Set veridian as lsp for verilog
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(verilog-mode . ("veridian"))))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(verilog-ts-mode . ("veridian"))))

;; Set jedi ls to python mode
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
	           '(python-mode . ("jedi-language-server"))))


(setq-default hs-minor-mode t)

(add-hook 'python-mode-hook 'eglot-ensure)
(add-hook 'python-mode-hook #'eldoc-box-hover-at-point-mode t)

(add-hook 'verilog-mode-hook 'eglot-ensure)

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



(use-package neotree
  :ensure t
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (add-hook 'neotree-mode-hook (lambda() (display-line-numbers-mode -1))))



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
 '(buffer-face-mode-face '(:family "JetBrainsMono Nerd Font" : height 130))
 '(custom-enabled-themes '(modus-vivendi-tinted))
 '(custom-safe-themes
   '("8d3ef5ff6273f2a552152c7febc40eabca26bae05bd12bc85062e2dc224cde9a"
     "13096a9a6e75c7330c1bc500f30a8f4407bd618431c94aeab55c9855731a95e1"
     "6bf350570e023cd6e5b4337a6571c0325cec3f575963ac7de6832803df4d210a"
     "5e39e95c703e17a743fb05a132d727aa1d69d9d2c9cde9353f5350e545c793d4"
     "77f281064ea1c8b14938866e21c4e51e4168e05db98863bd7430f1352cab294a"
     "4dcf06273c9f5f0e01cea95e5f71c3b6ee506f192d75ffda639d737964e2e14e"
     "4ade6b630ba8cbab10703b27fd05bb43aaf8a3e5ba8c2dc1ea4a2de5f8d45882"
     "e4a702e262c3e3501dfe25091621fe12cd63c7845221687e36a79e17cf3a67e0"
     "4594d6b9753691142f02e67b8eb0fda7d12f6cc9f1299a49b819312d6addad1d"
     "7964b513f8a2bb14803e717e0ac0123f100fb92160dcf4a467f530868ebaae3e"
     "6f1f6a1a3cff62cc860ad6e787151b9b8599f4471d40ed746ea2819fcd184e1a"
     "dccf4a8f1aaf5f24d2ab63af1aa75fd9d535c83377f8e26380162e888be0c6a9"
     "0d2c5679b6d087686dcfd4d7e57ed8e8aedcccc7f1a478cd69704c02e4ee36fe"
     "34cf3305b35e3a8132a0b1bdf2c67623bc2cb05b125f8d7d26bd51fd16d547ec"
     "c1d5759fcb18b20fd95357dcd63ff90780283b14023422765d531330a3d3cec2"
     "8b148cf8154d34917dfc794b5d0fe65f21e9155977a36a5985f89c09a9669aa0"
     "dfb1c8b5bfa040b042b4ef660d0aab48ef2e89ee719a1f24a4629a0c5ed769e8"
     "d6b934330450d9de1112cbb7617eaf929244d192c4ffb1b9e6b63ad574784aad"
     "7c28419e963b04bf7ad14f3d8f6655c078de75e4944843ef9522dbecfcd8717d"
     "f053f92735d6d238461da8512b9c071a5ce3b9d972501f7a5e6682a90bf29725"
     "e978b5106d203ba61eda3242317feff219f257f6300bd9b952726faf4c5dee7b"
     "014cb63097fc7dbda3edf53eb09802237961cbb4c9e9abd705f23b86511b0a69"
     "4990532659bb6a285fee01ede3dfa1b1bdf302c5c3c8de9fad9b6bc63a9252f7"
     "4d5d11bfef87416d85673947e3ca3d3d5d985ad57b02a7bb2e32beaf785a100e"
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
   '(all-the-icons centaur-tabs company dap-mode dape
                   eglot-inactive-regions eldoc-box ergoemacs-mode fzf
                   ivy kanagawa-themes modern-tab-bar mood-line
                   multiple-cursors neotree tokyo-theme treemacs-evil
                   treemacs-tab-bar ultra-scroll verilog-ext
                   verilog-ts-mode vterm yaml-mode yasnippet zig-mode
                   zig-ts-mode))
 '(package-vc-selected-packages
   '((tokyo-theme :vc-backend Git :url
                  "https://github.com/rawleyfowler/tokyo-theme.el")
     (modern-tab-bar :vc-backend Git :url
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
 '(verilog-tab-always-indent nil)
 '(xterm-mouse-mode t))
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
