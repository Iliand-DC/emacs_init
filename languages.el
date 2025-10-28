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


(add-hook 'python-mode-hook 'eglot-ensure)
(add-hook 'python-mode-hook #'eldoc-box-hover-mode t)
(add-hook 'verilog-mode-hook 'eglot-ensure)
