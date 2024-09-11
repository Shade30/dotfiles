;;; this loads the package manager - disabled in favor of elpaca
;; (require 'package)

;; disable package.el
(setq package-enable-at-startup nil)

;;; here there's a variable named package-archives, and we are adding the MELPA repository to it
;;(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))
;;(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
;;(add-to-list 'package-archives
;;             '("melpa-stable" . "http://stable.melpa.org/packages/") t)
;;(add-to-list 'package-archives
;;             '("melpa" . "http://melpa.org/packages/") t)
