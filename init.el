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
;; (use-package kanagawa-themes
;;   :ensure t
;;   :config
;;   (load-theme 'kanagawa-wave t))


(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-one-light t))

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
(set-face-attribute 'default nil :height 140)

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


(use-package treemacs
  :ensure t
  :config
  (define-key treemacs-mode-map (kbd "<right>") 'treemacs-RET-action)
  (define-key treemacs-mode-map (kbd "<left>") 'treemacs-COLLAPSE-action) 
  (global-set-key (kbd "M-o") 'treemacs-select-window)
  (treemacs-resize-icons 12))


(global-unset-key (kbd "M-<up>"))
(global-unset-key (kbd "M-<down>"))


(global-set-key (kbd "C-<left>") '(backward-word))
(global-set-key (kbd "C-<right>") '(forward-word))


(use-package centaur-tabs
  :ensure t
  :config
  (global-set-key (kbd "M-<up>") 'centaur-tabs-backward-tab)
  (centaur-tabs-mode)
  (global-set-key (kbd "M-<down>") 'centaur-tabs-forward-tab))



(setq backup-directory-alist '((".*" . "~/.Trash")))



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("2721b06afaf1769ef63f942bf3e977f208f517b187f2526f0e57c1bd4a000350"
     "e8bd9bbf6506afca133125b0be48b1f033b1c8647c628652ab7a2fe065c10ef0"
     "1f292969fc19ba45fbc6542ed54e58ab5ad3dbe41b70d8cb2d1f85c22d07e518"
     "4594d6b9753691142f02e67b8eb0fda7d12f6cc9f1299a49b819312d6addad1d"
     "77fff78cc13a2ff41ad0a8ba2f09e8efd3c7e16be20725606c095f9a19c24d3d"
     "0d2c5679b6d087686dcfd4d7e57ed8e8aedcccc7f1a478cd69704c02e4ee36fe"
     "014cb63097fc7dbda3edf53eb09802237961cbb4c9e9abd705f23b86511b0a69"
     "ffafb0e9f63935183713b204c11d22225008559fa62133a69848835f4f4a758c"
     "f64189544da6f16bab285747d04a92bd57c7e7813d8c24c30f382f087d460a33"
     "93011fe35859772a6766df8a4be817add8bfe105246173206478a0706f88b33d"
     "4b6cc3b60871e2f4f9a026a5c86df27905fb1b0e96277ff18a76a39ca53b82e1"
     "56044c5a9cc45b6ec45c0eb28df100d3f0a576f18eef33ff8ff5d32bac2d9700"
     "3061706fa92759264751c64950df09b285e3a2d3a9db771e99bcbb2f9b470037"
     "2b501400e19b1dd09d8b3708cefcb5227fda580754051a24e8abf3aff0601f87"
     "b754d3a03c34cfba9ad7991380d26984ebd0761925773530e24d8dd8b6894738"
     "9013233028d9798f901e5e8efb31841c24c12444d3b6e92580080505d56fd392"
     "a6920ee8b55c441ada9a19a44e9048be3bfb1338d06fc41bce3819ac22e4b5a1"
     "d481904809c509641a1a1f1b1eb80b94c58c210145effc2631c1a7f2e4a2fdf4"
     "34cf3305b35e3a8132a0b1bdf2c67623bc2cb05b125f8d7d26bd51fd16d547ec"
     "571661a9d205cb32dfed5566019ad54f5bb3415d2d88f7ea1d00c7c794e70a36"
     "8d3ef5ff6273f2a552152c7febc40eabca26bae05bd12bc85062e2dc224cde9a"
     "b9761a2e568bee658e0ff723dd620d844172943eb5ec4053e2b199c59e0bcc22"
     "452068f2985179294c73c5964c730a10e62164deed004a8ab68a5d778a2581da"
     "f053f92735d6d238461da8512b9c071a5ce3b9d972501f7a5e6682a90bf29725"
     "ff24d14f5f7d355f47d53fd016565ed128bf3af30eb7ce8cae307ee4fe7f3fd0"
     "df6dfd55673f40364b1970440f0b0cb8ba7149282cf415b81aaad2d98b0f0290"
     "f4d1b183465f2d29b7a2e9dbe87ccc20598e79738e5d29fc52ec8fb8c576fcfd"
     "4990532659bb6a285fee01ede3dfa1b1bdf302c5c3c8de9fad9b6bc63a9252f7"
     "e978b5106d203ba61eda3242317feff219f257f6300bd9b952726faf4c5dee7b"
     "dd4582661a1c6b865a33b89312c97a13a3885dc95992e2e5fc57456b4c545176"
     "48042425e84cd92184837e01d0b4fe9f912d875c43021c3bcb7eeb51f1be5710"
     "a9eeab09d61fef94084a95f82557e147d9630fbbb82a837f971f83e66e21e5ad"
     "c1d5759fcb18b20fd95357dcd63ff90780283b14023422765d531330a3d3cec2"
     "32f22d075269daabc5e661299ca9a08716aa8cda7e85310b9625c434041916af"
     "dfb1c8b5bfa040b042b4ef660d0aab48ef2e89ee719a1f24a4629a0c5ed769e8"
     "02d422e5b99f54bd4516d4157060b874d14552fe613ea7047c4a5cfa1288cf4f"
     "aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8"
     "13096a9a6e75c7330c1bc500f30a8f4407bd618431c94aeab55c9855731a95e1"
     "8b148cf8154d34917dfc794b5d0fe65f21e9155977a36a5985f89c09a9669aa0"
     "b5fd9c7429d52190235f2383e47d340d7ff769f141cd8f9e7a4629a81abc6b19"
     "dccf4a8f1aaf5f24d2ab63af1aa75fd9d535c83377f8e26380162e888be0c6a9"
     "6f1f6a1a3cff62cc860ad6e787151b9b8599f4471d40ed746ea2819fcd184e1a"
     "4ade6b630ba8cbab10703b27fd05bb43aaf8a3e5ba8c2dc1ea4a2de5f8d45882"
     "456697e914823ee45365b843c89fbc79191fdbaff471b29aad9dcbe0ee1d5641"
     "0325a6b5eea7e5febae709dab35ec8648908af12cf2d2b569bedc8da0a3a81c1"
     "da75eceab6bea9298e04ce5b4b07349f8c02da305734f7c0c8c6af7b5eaa9738"
     "5c7720c63b729140ed88cf35413f36c728ab7c70f8cd8422d9ee1cedeb618de5"
     "daa27dcbe26a280a9425ee90dc7458d85bd540482b93e9fa94d4f43327128077"
     default))
 '(dired-kill-when-opening-new-dired-buffer t)
 '(package-selected-packages
   '(company dape doom-themes eglot-inactive-regions evil-leader
             evil-surround fzf ivy jupyter kanagawa-themes lua-mode
             mood-line projectile projectile-ripgrep python-mode
             rust-mode ultra-scroll verilog-ts-mode vterm zig-mode))
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
