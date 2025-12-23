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
   '(avy company counsel dashboard fasm-mode fzf git-gutter-fringe
         kaolin-themes lsp-mode mood-line multiple-cursors
         nano-modeline nerd-icons powerline rich-minority shrink-path
         typst-ts-mode ultra-scroll vertico vterm yaml-mode yasnippet)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(line-number ((t (:inherit default))))
 '(line-number-current-line ((t (:inherit default)))))
