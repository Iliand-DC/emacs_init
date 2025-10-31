(defun text-scale-once ()
  "Scale down buffer"
  (interactive)
  (progn
    (text-scale-adjust 0)
    (text-scale-decrease 1)))


(defun projectile-open-term()
  "Open terminal in a splitted below window in project dir."
  (interactive)
  (windmove-display-down)
  (projectile-run-vterm))


(defun open-term()
  "Open terminal in a splitted below window."
  (interactive)
  (windmove-display-down)
  (vterm))


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


;; Disable ugly ui
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; Maximize window at startup
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

(setq-default truncate-lines t)

;; No more typing the whole yes or no. Just y or n will do.
(fset 'yes-or-no-p 'y-or-n-p)

(pixel-scroll-precision-mode t)

;; Enable cua mode by default to normal C+c, C+v
(cua-mode t)

(add-to-list 'auto-mode-alist '("\\.s?vh?\\'" . verilog-ts-mode))

;; Disable annoyning bell
(setq ring-bell-function 'ignore)

(centaur-tabs-mode)

;; Makes *scratch* empty.
(setq initial-scratch-message "")

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

;; Set default font bigger
(set-frame-font "JetBrainsMono Nerd Font 12" nil t)

(setq buffer-face-mode-face '(:family "JetBrainsMono Nerd Font": height 110))
(buffer-face-mode t)


;; setup indentation
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(ivy-mode)

(ace-window-display-mode)

;; Disable treemacs line numbers
(add-hook 'treemacs-mode-hook (lambda() (display-line-numbers-mode -1)))

(setq backup-directory-alist '((".*" . "~/.Trash")))

;; Disable warnings on startup
(setq warning-minimum-level :error)

;; Enable company mode by default
(global-company-mode)

(global-display-line-numbers-mode)

;; Removes *scratch* from buffer after the mode has been set.
(defun remove-scratch-buffer ()
  (if (get-buffer "*scratch*")
      (kill-buffer "*scratch*")))

;; (add-hook 'after-change-major-mode-hook 'remove-scratch-buffer)

(setq-default hs-minor-mode t)

(global-hl-line-mode)

(setq scroll-conservatively 101)

(setq scroll-preserve-screen-position t)

;; search selected text
(defadvice isearch-mode (around isearch-mode-default-string (forward &optional regexp op-fun recursive-edit word-p) activate)
  (if (and transient-mark-mode mark-active (not (eq (mark) (point))))
      (progn
        (isearch-update-ring (buffer-substring-no-properties (mark) (point)))
        (deactivate-mark)
        ad-do-it
        (if (not forward)
            (isearch-repeat-backward)
          (goto-char (mark))
          (isearch-repeat-forward)))
    ad-do-it))

(electric-pair-mode)

(add-hook 'write-file-hooks 'delete-trailing-whitespace)

(context-menu-mode)
