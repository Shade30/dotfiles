;;; this loads the package manager
(require 'package)

;;; here there's a variable named package-archives, and we are adding the MELPA repository to it
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives
			 '("melpa" . "http://melpa.milkbox.net/packages/") t)

;;; loads packages and activates them
(package-initialize)

;;; packages to install
(setq package-list '(color-theme-sanityinc-tomorrow
		     evil
		     goto-chg
		     monokai-theme
		     undo-tree
                     smooth-scrolling
		     ace-jump-mode
                     icicles
		     cider
		     clojure-mode
		     auto-complete
                     nyan-mode
                     htmlize
                     flx-ido
                     groovy-mode
                     projectile
                     rainbow-delimiters
                     cider-eval-sexp-fu
		     ))

(defun ensure-package-installed (&rest packages)
  "Assure every package is installed, ask for installation if it's not.

Return a list of installed packages or nil for every skipped package."
  (mapcar
   (lambda (package)
     ;; (package-installed-p 'evil)
     (if (package-installed-p package)
         nil
       (if (y-or-n-p (format "Package %s is missing. Install it? " package))
           (package-install package)
         package)))
   packages))

;; make sure to have downloaded archive description.
;; Or use package-archive-contents as suggested by Nicolas Dudebout
(or (file-exists-p package-user-dir)
    (package-refresh-contents))

(dolist (package package-list)
  (ensure-package-installed package)) ;  --> (nil nil) if iedit and magit are already installed

;;; loads packages and activates them
(package-initialize)

;;; Save emacs sessions
(desktop-save-mode 1)
(setq desktop-load-locked-desktop nil)

;;; evil mode by default
(require 'evil)
(evil-mode t)

;;; ace jump mode
(require 'ace-jump-mode)
(define-key global-map (kbd "C-;") 'ace-jump-mode)
;; evil-mode+ace-jump-mode bindings
(define-key evil-motion-state-map (kbd "C-;") #'evil-ace-jump-char-mode)
(define-key evil-motion-state-map (kbd "C-SPC") #'evil-ace-jump-word-mode)

(define-key evil-operator-state-map (kbd "C-;") #'evil-ace-jump-char-mode) ; similar to f
(define-key evil-operator-state-map (kbd "C-SPC") #'evil-ace-jump-char-to-mode) ; similar to t
(define-key evil-operator-state-map (kbd "M-SPC") #'evil-ace-jump-word-mode)

;; different jumps for different visual modes
(defadvice evil-visual-line (before spc-for-line-jump activate)
(define-key evil-motion-state-map (kbd "C-;") #'evil-ace-jump-line-mode))

(defadvice evil-visual-char (before spc-for-char-jump activate)
(define-key evil-motion-state-map (kbd "C-;") #'evil-ace-jump-char-mode))

(defadvice evil-visual-block (before spc-for-char-jump activate)
(define-key evil-motion-state-map (kbd "C-;") #'evil-ace-jump-char-mode))

;;; icicles
(require 'icicles)

;;; projectile
(projectile-global-mode)

;;; custom color theme
;;(require 'color-theme-sanityinc-tomorrow)

;;; Org mode
(define-key global-map "\C-Cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq calendar-week-start-day 1)
(setq org-agenda-custom-commands
      '(("P" "Printed agenda"
         ((todo "IMPOSSIBLE_PATTERN" ((org-agenda-overriding-header "Week\n------------------")))
          (agenda "" ((org-agenda-ndays 7)                      ;; overview of appointments
                      (org-agenda-start-on-weekday 1)           ;; calendar begins on monday
                      (org-agenda-repeating-timestamp-show-all t)
                      (org-agenda-entry-types '(:timestamp :sexp))))
          (todo "IMPOSSIBLE_PATTERN" ((org-agenda-overriding-header "\nToday\n------------------")))
          (agenda "" ((org-agenda-ndays 1)                      ;; daily agenda
                      (org-deadline-warning-days 30)            ;; 30 days advanced warning for deadlines
                      (org-agenda-todo-keyword-format "[ ]")
                      (org-agenda-scheduled-leaders '("" ""))
                      (org-agenda-prefix-format "%t%s")))
          (todo "TODO"                                          ;; todos sorted by context
                ((org-agenda-prefix-format "[ ] %T: ")
                 (org-agenda-sorting-strategy '(tag-up priority-down))
                 (org-agenda-todo-keyword-format "")
                 (org-agenda-overriding-header "\nTasks by Context\n------------------\n")))
          )
         ((org-agenda-with-colors t)
          (org-agenda-compact-blocks t)
          (org-agenda-remove-tags t))
         ("~/org/theagenda.html"))
        ))

;;; general auto-complete
(require 'auto-complete-config)
(setq ac-delay 0.0)
(setq ac-quick-help-delay 0.5)
(ac-config-default)

;;; clojure mode
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
(require 'cider-eval-sexp-fu)

;;; from mooc
;;; global settings
(require 'cl)
(require 'ffap)
(require 'uniquify)
(require 'ansi-color)
(require 'recentf)
(require 'linum)
(require 'smooth-scrolling)
(require 'whitespace)
(require 'dired-x)
(require 'compile)
(menu-bar-mode -1)
(normal-erase-is-backspace-mode 1)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(setq column-number-mode t)
(setq inhibit-startup-message t)
(setq save-abbrevs nil)
(setq show-trailing-whitespace t)
(setq suggest-key-bindings t)
(setq vc-follow-symlinks t)
;; use spaces instead of tabs when indenting
(setq-default indent-tabs-mode nil)
(setq-default whitespace-style '(tabs spaces trailing lines space-before-tab newline indentation:space empty space-after-tab space-mark tab-mark newline-mark))

;;; ido mode
(require 'ido)
(ido-mode t)
(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights
(setq ido-use-faces nil)

;;; nyan mode
(if window-system (nyan-mode t))
      
;;; with animation - buggy
;; (if window-system
;;     (progn
;;       (nyan-mode t)
;;       (setq nyan-wavy-trail t)
;;       (nyan-start-animation)))

;;; hide ^M in logs
(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

;;; groovy mode
(add-to-list 'auto-mode-alist '("\\.groovy\\'" . groovy-mode))

(set-face-attribute 'default nil :family "Liberation Mono")

;;; custom mode for log viewing
(define-derived-mode log4j-view-mode fundamental-mode
  (toggle-truncate-lines t)
  (toggle-read-only t)
  (auto-revert-tail-mode t)
  (setq mode-name "Log4J")
  ;; hide ^M
  (remove-dos-eol)
)
(add-to-list 'auto-mode-alist '("\\app.log\\'" . log4j-view-mode))

;;; custom font - linux
(add-to-list 'default-frame-alist '(font . "Liberation Mono-11"))

;; load in customizations
(if (file-exists-p "~/init_custom.el") (load-library "~/init_custom.el"))

;;; added automatically
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#ffffff" "#ff9da4" "#d1f1a9" "#ffeead" "#bbdaff" "#ebbbff" "#99ffff" "#002451"))
 '(custom-enabled-themes (quote (sanityinc-tomorrow-night)))
 '(custom-safe-themes
   (quote
    ("06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "4e262566c3d57706c70e403d440146a5440de056dfaeb3062f004da1711d83fc" default)))
 '(fci-rule-color "#00346e")
 '(inhibit-startup-screen t)
 '(org-agenda-files (quote ("~/org/main.org")))
 '(package-selected-packages
   (quote
    (smooth-scrolling rainbow-delimiters projectile nyan-mode monokai-theme icicles htmlize groovy-mode flx-ido evil color-theme-sanityinc-tomorrow cider-eval-sexp-fu cider auto-complete ace-jump-mode)))
 '(sql-connection-alist
   (quote
    (("pg_beirut_trunk"
      (sql-product
       (quote postgres))
      (sql-user "root")
      (sql-database "taxi_beirut")
      (sql-server "localhost")))))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#ff9da4")
     (40 . "#ffc58f")
     (60 . "#ffeead")
     (80 . "#d1f1a9")
     (100 . "#99ffff")
     (120 . "#bbdaff")
     (140 . "#ebbbff")
     (160 . "#ff9da4")
     (180 . "#ffc58f")
     (200 . "#ffeead")
     (220 . "#d1f1a9")
     (240 . "#99ffff")
     (260 . "#bbdaff")
     (280 . "#ebbbff")
     (300 . "#ff9da4")
     (320 . "#ffc58f")
     (340 . "#ffeead")
     (360 . "#d1f1a9"))))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
