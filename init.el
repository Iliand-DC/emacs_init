;; Enable melpa packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(load-file "~/.emacs.d/numbers.el")
(load-file "~/.emacs.d/core.el")
(load-file "~/.emacs.d/keybindings.el")
(load-file "~/.emacs.d/languages.el")
(load-file "~/.emacs.d/packages.el")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(ace-window atom-one-dark-theme bui cfrs corfu counsel dashboard
                fasm-mode fzf git-gutter-fringe goto-chg hydra
                jinja2-mode kaolin-themes lsp-docker lsp-treemacs
                mood-line moody multiple-cursors nano-theme neotree
                nerd-icons pfuture surround typst-ts-mode ultra-scroll
                undo-tree vertico vterm yaml-mode yasnippet))
 '(package-vc-selected-packages '((moody :url "https://github.com/tarsius/moody.git"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(line-number ((t (:inherit default))))
 '(line-number-current-line ((t (:inherit default)))))
