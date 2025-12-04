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


(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration
               '(verilog-ts-mode . "sv"))

  (add-to-list 'lsp-language-id-configuration
               '(typst-ts-mode . "typ"))

  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "veridian")
                    :activation-fn (lsp-activate-on "sv")
                    :server-id 'veridian))

  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "jedi-language-server")
                    :activation-fn (lsp-activate-on "python")
                    :server-id 'jedi-language-server))

  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "tinymist")
                    :activation-fn (lsp-activate-on "typ")
                    :server-id 'tinymist)))



 ;; Set veridian as lsp for verilog
;; (with-eval-after-load 'eglot
;;   (add-to-list 'eglot-server-programs
;;                '(verilog-mode . ("veridian"))))


;; (with-eval-after-load 'eglot
;;   (add-to-list 'eglot-server-programs
;;                '(verilog-ts-mode . ("veridian"))))


;; ;; Set jedi ls to python mode
;; (with-eval-after-load 'eglot
;;   (add-to-list 'eglot-server-programs
;; 	           '(python-mode . ("jedi-language-server"))))


;; (with-eval-after-load 'eglot
;;   (add-to-list 'eglot-server-programs
;; 	           '(c-mode . ("clangd"))))


;; (with-eval-after-load 'eglot
;;   (add-to-list 'eglot-server-programs
;;                '(typst-ts-mode . ("tinymist"))))



;; (add-hook 'python-mode-hook 'eglot-ensure)
;; (add-hook 'verilog-mode-hook 'eglot-ensure)
;; (add-hook 'c-mode-hook 'eglot-ensure)
