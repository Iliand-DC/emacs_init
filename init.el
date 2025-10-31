;; Enable melpa packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(load-file "~/.emacs.d/core.el")
(load-file "~/.emacs.d/keybindings.el")
(load-file "~/.emacs.d/languages.el")
(load-file "~/.emacs.d/packages.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(buffer-face-mode-face '(:family "JetBrainsMono Nerd Font" : height 130))
 '(custom-enabled-themes '(doom-challenger-deep))
 '(custom-safe-themes
   '("13096a9a6e75c7330c1bc500f30a8f4407bd618431c94aeab55c9855731a95e1"
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
   '(all-the-icons async bui centaur-tabs company dap-mode dashboard
                   doom-themes eglot-inactive-regions eldoc-box
                   flycheck fzf git-gutter-fringe ivy magit
                   magit-section markdown-mode mood-line moody
                   multiple-cursors neotree outshine projectile
                   pythonic right-click-context ripgrep ruff-format
                   scala-mode scala-ts-mode spinner surround transient
                   treemacs-tab-bar typst-ts-mode ultra-scroll
                   verilog-ts-mode vterm with-editor yaml yaml-mode
                   yasnippet zig-mode zig-ts-mode))
 '(package-vc-selected-packages
   '((minimal-dashboard :url
                        "https://github.com/dheerajshenoy/minimal-dashboard.el")))
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
 '(ansi-color-bright-white ((t (:background "white" :foreground "white"))))
 '(ansi-color-white ((t (:background "white" :foreground "white"))))
 '(tab-bar ((t (:inherit mode-line)))))
(put 'scroll-left 'disabled nil)
