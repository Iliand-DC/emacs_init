;; Enable melpa packages
(require 'package)
(load-file ".emacs.d/core.el")
(load-file ".emacs.d/keybindings.el")
(load-file ".emacs.d/languages.el")
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)


(use-package vterm
  :ensure t
  :config
  (define-key vterm-mode-map (kbd "C-S-v") 'vterm-yank))


(use-package neotree
  :ensure t
  :config
  (global-set-key (kbd "C-b") 'neotree-toggle)
  (global-set-key (kbd "C-e") 'neotree-show))


(use-package eldoc-box
  :ensure t
  :config
  (setq-default eldoc-box-hover-mode t)
  (setq-default eldoc-box-max-pixel-height 80))


(defun open-term()
  "Open terminal in a splitted below window."
  (interactive)
  (windmove-display-down)
  (projectile-run-vterm))


(defun open-config-file()
  "Quick open this config file."
  (interactive)
  (find-file "~/.emacs.d/init.el"))


(defun insert-line-above ()
  "Insert an empty line above the current line."
  (interactive)
  (save-excursion
    (end-of-line 0)
    (open-line 1)))


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


(use-package tokyonight-themes
  :vc (:url "https://github.com/xuchengpeng/tokyonight-themes")
  :config
  (load-theme 'tokyonight-moon :no-confirm))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(buffer-face-mode-face '(:family "JetBrainsMono Nerd Font" : height 130))
 '(custom-enabled-themes '(doom-tokyo-night))
 '(custom-safe-themes
   '("e8bd9bbf6506afca133125b0be48b1f033b1c8647c628652ab7a2fe065c10ef0"
     "1f292969fc19ba45fbc6542ed54e58ab5ad3dbe41b70d8cb2d1f85c22d07e518"
     "7e377879cbd60c66b88e51fad480b3ab18d60847f31c435f15f5df18bdb18184"
     "77fff78cc13a2ff41ad0a8ba2f09e8efd3c7e16be20725606c095f9a19c24d3d"
     "0c83e0b50946e39e237769ad368a08f2cd1c854ccbcd1a01d39fdce4d6f86478"
     "93011fe35859772a6766df8a4be817add8bfe105246173206478a0706f88b33d"
     "2078837f21ac3b0cc84167306fa1058e3199bbd12b6d5b56e3777a4125ff6851"
     "4b6cc3b60871e2f4f9a026a5c86df27905fb1b0e96277ff18a76a39ca53b82e1"
     "e8ceeba381ba723b59a9abc4961f41583112fc7dc0e886d9fc36fa1dc37b4079"
     "56044c5a9cc45b6ec45c0eb28df100d3f0a576f18eef33ff8ff5d32bac2d9700"
     "3061706fa92759264751c64950df09b285e3a2d3a9db771e99bcbb2f9b470037"
     "9d5124bef86c2348d7d4774ca384ae7b6027ff7f6eb3c401378e298ce605f83a"
     "2b501400e19b1dd09d8b3708cefcb5227fda580754051a24e8abf3aff0601f87"
     "f5f80dd6588e59cfc3ce2f11568ff8296717a938edd448a947f9823a4e282b66"
     "22d73dce2d6712154900097ac8f9146c51deea66a92a2406c8c3f341ee9eb30a"
     "0ca4a8417a19ecbbf4538550b90424bf11d5e8caff99c605165cf0a058b52fef"
     "a7d492b6d2d0940ef70f376e82e94144c2493a5a687c514f607a45587920f803"
     "c9aac9e4be9968088ce5117887ebf74da92b4eb5f3f67009a99516056f774916"
     "f4d1b183465f2d29b7a2e9dbe87ccc20598e79738e5d29fc52ec8fb8c576fcfd"
     "48042425e84cd92184837e01d0b4fe9f912d875c43021c3bcb7eeb51f1be5710"
     "aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8"
     "8d3ef5ff6273f2a552152c7febc40eabca26bae05bd12bc85062e2dc224cde9a"
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
 '(package-selected-packages nil)
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
