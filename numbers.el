(global-display-line-numbers-mode)

(eval-after-load "display-line-numbers"
  '(custom-set-faces
    '(line-number ((t (:inherit default))))
    '(line-number-current-line ((t (:inherit default))))))
