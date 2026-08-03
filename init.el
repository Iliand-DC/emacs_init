;; Enable melpa packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(load "~/.emacs.d/numbers.elc")
(load "~/.emacs.d/core.elc")
(load "~/.emacs.d/languages.elc")
(load "~/.emacs.d/packages.elc")
(load-theme 'doom-oksolar-dark t) ;; doom-palenight/doom-moonlight/doom-monokai-pro they are pretty cool
