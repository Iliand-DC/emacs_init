(global-set-key (kbd "C-c r") 'eglot-rename)

(global-set-key (kbd "C-<tab>") 'indent-rigidly)


(global-unset-key (kbd "C-s"))
(global-unset-key (kbd "C-x C-s"))


(global-set-key (kbd "C-s") 'save-buffer)

(global-set-key (kbd "C-f") 'isearch-forward)
(define-key isearch-mode-map (kbd "C-f") 'isearch-repeat-forward)

(global-set-key (kbd "C-S-f") 'isearch-backward)
(define-key isearch-mode-map (kbd "C-S-f") 'isearch-repeat-backward)


(global-set-key (kbd "C-x C-a") 'eval-buffer)
;; (global-set-key (kbd "C-x C-r") 'reload-config)


(global-set-key (kbd "C-x C-z") 'kill-other-buffers)


(global-set-key (kbd "C-x C-o") 'projectile-open-term)

(global-set-key (kbd "<backtab>") 'indent-rigidly-left-to-tab-stop)


(global-unset-key (kbd "M-\\"))
(global-set-key (kbd "M-\\") '(lambda() (interactive) (split-window-right) (windmove-right)))

(global-unset-key (kbd "M--"))
(global-set-key (kbd "M--") '(lambda () (interactive) (split-window-below) (windmove-down)))

(global-set-key (kbd "M-w") 'delete-window)

;; Set tab to insert tab, btw
(global-set-key (kbd "TAB") 'tab-to-tab-stop)

;; Enable better buffer control unit
(global-unset-key (kbd "C-x C-b"))
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Open vterm
(global-set-key (kbd "C-x C-\\") 'open-term)


(global-unset-key (kbd "C-x C-;"))
(define-key prog-mode-map (kbd "C-;") 'comment-line )


(global-set-key (kbd "C-w") 'kill-current-buffer)


(global-unset-key (kbd "C-a"))
(global-set-key (kbd "C-a") 'avy-goto-char)

;; define backtab key
(global-set-key (kbd "<backtab>") 'un-indent-by-removing-4-spaces)


;; Set open config kbd
(global-set-key (kbd "C-h C-c") 'open-config-file)


(global-unset-key (kbd "C-x f"))
(global-set-key (kbd "C-x f") 'fzf-projectile)

(global-unset-key (kbd "M-k"))
(global-set-key (kbd "M-p") 'ace-window)
(global-set-key (kbd "M-k") 'ace-delete-window)

(global-set-key (kbd "TAB") 'tab-to-tab-stop)

(global-set-key (kbd "C-c N") 'insert-line-below)

(global-unset-key (kbd "C-x C-p"))

(global-set-key (kbd "C-d") 'surround-insert)
