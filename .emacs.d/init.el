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
                     ac-cider
                     auto-complete
                     nyan-mode
                     htmlize
                     flx-ido
                     groovy-mode
                     rainbow-delimiters
                     smartparens
                     evil-smartparens
                     clj-refactor
                     projectile
                     cider-eval-sexp-fu
                     org-pomodoro
                     scss-mode
                     markdown-mode
                     helm
                     yasnippet
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

;;; Start server on startup
(require 'server)
(unless (server-running-p)
  (server-start))

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

;;; projectile
(projectile-global-mode)

;; load in customizations
(load "~/.emacs.d/init_customizations.el")

;;; system-type definition
(defun system-is-linux()
    (string-equal system-type "gnu/linux"))
(defun system-is-windows()
    (string-equal system-type "windows-nt"))
(defun system-is-cygwin()
    (string-equal system-type "cygwin"))

;;; custom font - linux
(when (system-is-linux)
  (set-face-attribute 'default nil :family "Liberation Mono")
  (add-to-list 'default-frame-alist '(font . "Liberation Mono-11"))
)

;;; custom font - cygwin and windows
(when (or (system-is-cygwin) (system-is-windows))
  (set-face-attribute 'default nil :family "Consolas")
  (add-to-list 'default-frame-alist '(font . "Consolas-12"))
)
  

;;; custom color theme
;;(require 'color-theme-sanityinc-tomorrow)
(customize-set-variable 'custom-safe-themes (quote ("06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" default)))
(customize-set-variable 'custom-enabled-themes (quote (sanityinc-tomorrow-night)))

;;; hide splash screen
(customize-set-variable 'inhibit-startup-screen t)

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
         ("~/org/theagenda.html"))))

;;; yasnippet
(require 'yasnippet)
(yas-reload-all)
(add-hook 'prog-mode-hook #'yas-minor-mode)
(defun yas/org-very-safe-expand ()
  (let ((yas/fallback-behavior 'return-nil)) (yas/expand)))
(add-hook 'org-mode-hook
          (lambda ()
            (make-variable-buffer-local 'yas/trigger-key)
            (setq yas/trigger-key [tab])
            (add-to-list 'org-tab-first-hook 'yas/org-very-safe-expand)
            (define-key yas/keymap [tab] 'yas/next-field)))

;;; gntp alert for org-pomodoro
(require 'alert)
(require 'gntp)

(setq alert-fade-time 10)
(setq gntp-server "localhost")

(condition-case nil
  (let ((notifications
	 `((alert
	    :enabled t))))
    (gntp-register notifications gntp-server)
    (setq alert-default-style 'gntp))
  (error
   (setq alert-default-style 'message)))

;;; general auto-complete
(require 'auto-complete-config)
(setq ac-delay 0.0)
(setq ac-quick-help-delay 0.5)
(ac-config-default)

;;; clojure mode
(add-hook 'clojure-mode-hook #'subword-mode)
(add-hook 'clojure-mode-hook #'smartparens-strict-mode)
(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
(require 'cider-eval-sexp-fu)

;;; cider autocomplete
(require 'ac-cider)
(add-hook 'cider-mode-hook 'ac-flyspell-workaround)
(add-hook 'cider-mode-hook 'ac-cider-setup)
(add-hook 'cider-repl-mode-hook 'ac-cider-setup)
(eval-after-load "auto-complete"
  '(progn
     (add-to-list 'ac-modes 'cider-mode)
     (add-to-list 'ac-modes 'cider-repl-mode)))

;;; clojure mode for org-babel
(require 'ob-clojure)
(setq org-babel-clojure-backend 'cider)

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
;; (require 'ido)
;; (ido-mode t)
;; (ido-everywhere 1)
;; (flx-ido-mode 1)
;; ;; disable ido faces to see flx highlights
;; (setq ido-use-faces nil)

;;; icicles
;; (require 'icicles)

;;; helm
(require 'helm)
(require 'helm-config)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

;; Helm's generic functions
(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") #'helm-find-files)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t)

(helm-mode 1)

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

;; highlight hex colors
(defun xah-syntax-color-hex ()
  "Syntax color text of the form #ff1100 in current buffer.
URL 'http://ergoemacs.org/emacs/emacs_CSS_colors.html'
With enchancements from 'https://www.emacswiki.org/emacs/HexColour'
Version 2016-03-15"
  (interactive)
  (font-lock-add-keywords
   nil
   '(("#[ABCDEFabcdef[:digit:]]\\{6\\}"
      (0 (put-text-property
          (match-beginning 0)
          (match-end 0)
          'face (list :background (match-string-no-properties 0)
                      :foreground (if (>= (apply '+ (x-color-values
                                                     (match-string-no-properties 0)))
                                          (* (apply '+ (x-color-values "white")) .6))
                                      "black"
                                    "white"
                                    )))))))
  (font-lock-fontify-buffer))

(add-hook 'css-mode-hook 'xah-syntax-color-hex)
(add-hook 'scss-mode-hook 'xah-syntax-color-hex)
(add-hook 'php-mode-hook 'xah-syntax-color-hex)
(add-hook 'html-mode-hook 'xah-syntax-color-hex)

;;; load custom settings
(setq custom-file "~/.emacs.d/init_custom.el")
(if (file-exists-p "~/.emacs.d/init_custom.el") (load-library "~/.emacs.d/init_custom.el"))
