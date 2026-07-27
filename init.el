;; Enable melpa packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(load "~/.emacs.d/numbers.el")
(load "~/.emacs.d/core.el")
(load "~/.emacs.d/languages.el")
(load "~/.emacs.d/packages.el")
(load-theme 'doom-palenight t) ;; or doom-monokai-pro and doom-moonlight they are pretty cool too
