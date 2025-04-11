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

(use-package vterm
  :ensure t)

;; Enable cua mode by default to normal C+c, C+v
(cua-mode t)

;; Load fancy default theme
(load-theme 'modus-vivendi-tinted)

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
(set-frame-font "JetBrainsMono Nerd Font 13" nil t)

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
(global-set-key (kbd "<backtab>") 'un-indent-by-removing-4-spaces)

;; Define kbd to kill buffer
(global-set-key (kbd "s-k") 'kill-current-buffer)

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
               '(verilog-mode . ("verible-verilog-ls" "--rules=-parameter-name-style,-case-missing-default"))))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(company eglot-inactive-regions vterm)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
