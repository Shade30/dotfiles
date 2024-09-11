;; system-type definition
(defun system-is-linux()
  (string-equal system-type "gnu/linux"))
(defun system-is-windows()
  (string-equal system-type "windows-nt"))
(defun system-is-cygwin()
  (string-equal system-type "cygwin"))

;;; elpaca
(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                 ,@(when-let ((depth (plist-get order :depth)))
                                                     (list (format "--depth=%d" depth) "--no-single-branch"))
                                                 ,(plist-get order :repo) ,repo))))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; disable symlinks on windows
(when (system-is-windows)
  (elpaca-no-symlink-mode))

;; install use-package support
(elpaca elpaca-use-package
  ;; enable use-package :ensure support for elpaca
  (elpaca-use-package-mode))

;;; Save emacs sessions
(desktop-save-mode 1)
(setq desktop-load-locked-desktop nil)

;;; Start server on startup
(require 'server)
(unless (server-running-p)
  (server-start))

;;; backup files and auto-saves
;; Put backup files neatly away
(let ((backup-dir "~/tmp/emacs/backups")
      (auto-saves-dir "~/tmp/emacs/auto-saves/"))
  (dolist (dir (list backup-dir auto-saves-dir))
    (when (not (file-directory-p dir))
      (make-directory dir t)))
  (setq backup-directory-alist `(("." . ,backup-dir))
        auto-save-file-name-transforms `((".*" ,auto-saves-dir t))
        auto-save-list-file-prefix (concat auto-saves-dir ".saves-")
        tramp-backup-directory-alist `((".*" . ,backup-dir))
        tramp-auto-save-directory auto-saves-dir))

(setq backup-by-copying t    ; Don't delink hardlinks
      delete-old-versions t  ; Clean up the backups
      version-control t      ; Use version numbers on backups,
      kept-new-versions 5    ; keep some new versions
      kept-old-versions 2)   ; and some old ones, too

;; Show bent arrow in the window fringe to distinguish visual lines
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))

;;; evil mode
(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-i-jump nil)

  :config
  (evil-mode t))

(use-package undo-tree
  :ensure t
  :config
  (evil-set-undo-system 'undo-redo))

(use-package evil-collection
  :ensure t
  :init
  (evil-collection-init))

(use-package evil-replace-with-register
  :ensure t
  :init
  (evil-replace-with-register-install))

;; goto-chg - goto last change in current buffer
;; bound to g;
(use-package goto-chg
  :ensure t)

;;; avy
(use-package avy
  :ensure t
  :config
  (global-set-key (kbd "C-;") 'avy-goto-char))

;;; projectile
(use-package projectile
  :ensure t
  :config
  (projectile-global-mode)

  ;; (define-key projectile-mode-map projectile-keymap-prefix nil)
  (define-key projectile-mode-map (kbd "M-p") #'projectile-command-map))

;; load in customizations
(load "~/.emacs.d/init_customizations.el")

;;; custom font - linux
(when (system-is-linux)
  (set-face-attribute 'default nil :family "Hack")
  (add-to-list 'default-frame-alist '(font . "Hack-12"))
)

;;; custom font - cygwin and windows
(when (or (system-is-cygwin) (system-is-windows))
  (set-face-attribute 'default nil :family "Consolas")
  (add-to-list 'default-frame-alist '(font . "Consolas-12"))
)
  
;;; color themes
(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :config
  (customize-set-variable 'custom-safe-themes (quote ("04aa1c3ccaee1cc2b93b246c6fbcd597f7e6832a97aaeac7e5891e6863236f9f" default)))
  (customize-set-variable 'custom-enabled-themes (quote (sanityinc-tomorrow-eighties)))
  (load-theme 'sanityinc-tomorrow-eighties t))
;(use-package idea-darkula-theme
;  :ensure t)
;(use-package base16-theme
;  :ensure t)
;(use-package monokai-theme
;  :ensure t)

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

;;; org-trello
(use-package org-trello
  :ensure t
  ;; org-trello major mode for all .trello files
  :mode ("\\.trello$" . org-mode)

  :init
  ;; add a hook function to check if this is trello file, then activate the org-trello minor mode.
  (add-hook 'org-mode-hook
            (lambda ()
              (let ((filename (buffer-file-name (current-buffer))))
                (when (and filename (string= "trello" (file-name-extension filename)))
                  (org-trello-mode))))))

;;; secretaria
(use-package secretaria
  :ensure t
  :init
  (add-hook 'after-init-hook #'secretaria-unknown-time-always-remind-me)

  ;; fix erroneous function
  (require 'secretaria)
  (defun secretaria-alert-due-appt ()
    "Tell the user about due TODOs tasks."
    (let ( (appts (secretaria-get-appt 'due)) )
      (when (< 0 (length appts))
        (alert (format "Due entries: %s" (length appts))
               :title "Attention, boss!"
               :severity 'high
               :mode 'org-mode))))

  (defun secretaria-alert-unknown-time-appt ()
    "Tell the user about tasks scheduled for today.
Those tasks have no time of the day specified"
    (let ( (appts (secretaria-get-appt 'unknown)))
      (dolist (entry appts)
        (alert "Task for today, time unspecified"
               :title (or entry "(no title)")
               :severity (secretaria--conditional-severity)
               :mode 'org-mode)))))

;;; yasnippet
(use-package yasnippet
  :ensure t
  :init
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  (add-hook 'org-mode-hook
            (lambda ()
              (yas-minor-mode)
              (make-variable-buffer-local 'yas/trigger-key)
              (setq yas/trigger-key [tab])
              (add-to-list 'org-tab-first-hook 'yas/org-very-safe-expand)
              (define-key yas/keymap [tab] 'yas/next-field)))
  (defun yas/org-very-safe-expand ()
    (let ((yas/fallback-behavior 'return-nil)) (yas/expand)))

  :config
  (yas-reload-all))

;;; org-pomodoro
(use-package org-pomodoro
  :ensure t)

;;; general auto-complete
;; (use-package auto-complete
;;   :ensure t
;;   :init
;;   (setq ac-delay 0.0)
;;   (setq ac-quick-help-delay 0.5)
;; 
;;   :config
;;   (ac-config-default))

;;; company auto-complete
(use-package company
  :ensure t
  :defer t
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  (add-hook 'after-init-hook 'company-quickhelp-mode)
  (setq company-idle-delay 0.0)
  (setq company-dabbrev-downcase nil)
  (global-set-key [C-tab] #'company-complete) ; use C-TAB as manual trigger
  (define-key evil-insert-state-map (kbd "C-SPC") #'company-complete))

(use-package company-quickhelp
  :ensure t)

;;; magit mode
(use-package transient
  :ensure t)

(use-package magit
  :ensure t)

;; (use-package evil-magit
;;   :ensure t
;;   :init
;;   (global-set-key (kbd "C-x g") 'magit-status))

;;; smartparens
(use-package smartparens
  :ensure t
  :config
  (use-package smartparens-config))

(use-package evil-smartparens
  :ensure t)

(use-package rainbow-delimiters
  :ensure t)

;;; clojure mode
(use-package clojure-mode
  :ensure t
  :init
  (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode)
  (add-hook 'clojure-mode-hook #'subword-mode)
  (add-hook 'clojure-mode-hook #'smartparens-strict-mode)
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'clojure-mode-hook #'show-smartparens-mode))

(use-package cider
  :ensure t
  :defer t)

(use-package cider-eval-sexp-fu
  :ensure t
  :defer t)

;; (use-package clj-refactor
;;   :ensure t
;;   :defer t)

;;; cider autocomplete
;; (use-package ac-cider
;;   :ensure t
;;   :init
;;   (add-hook 'cider-mode-hook 'ac-flyspell-workaround)
;;   (add-hook 'cider-mode-hook 'ac-cider-setup)
;;   (add-hook 'cider-repl-mode-hook 'ac-cider-setup)
;; 
;;   :config
;;   (eval-after-load "auto-complete"
;;     '(progn
;;        (add-to-list 'ac-modes 'cider-mode)
;;        (add-to-list 'ac-modes 'cider-repl-mode))))

;;; clojure mode for org-babel
(require 'ob-clojure)
(setq org-babel-clojure-backend 'cider)

;;; from mooc
;;; global settings
;;(require 'cl)
;;(require 'ffap) ; find file at point
(require 'uniquify)
(require 'ansi-color)
(require 'recentf)
(use-package smooth-scrolling
  :ensure t)
(require 'whitespace)
(require 'dired-x)
(require 'compile)
(tool-bar-mode -1)
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

;;; helm
(use-package helm
  :ensure t
  :defer t
  :init
  ;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
  ;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
  ;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-unset-key (kbd "C-x c"))

  ;; Helm's generic functions
  (global-set-key (kbd "M-x") #'helm-M-x)
  (global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
  (global-set-key (kbd "C-x C-f") #'helm-find-files)

  (when (executable-find "curl")
    (setq helm-google-suggest-use-curl-p t))

  (setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
        helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
        helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
        helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
        helm-ff-file-name-history-use-recentf t)
  :config
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
  (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

  (helm-mode 1))

(use-package helm-swoop
  :ensure t
  :init
  ;; helm-swoop keybindings
  (global-set-key (kbd "M-i") 'helm-swoop)
  (global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)
  (global-set-key (kbd "C-c M-i") 'helm-multi-swoop)
  (global-set-key (kbd "C-x M-i") 'helm-multi-swoop-all)

  ;; If nil, you can slightly boost invoke speed in exchange for text color
  (setq helm-swoop-speed-or-color t)

  ;; helm-swoop fuzzy matching
  (setq helm-swoop-use-fuzzy-match t)

  ;; Disable pre-input
  (setq helm-swoop-pre-input-function
        (lambda () ""))

  :config
  ;; When doing isearch, hand the word over to helm-swoop
  (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
  ;; From helm-swoop to helm-multi-swoop-all
  (define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)
  ;; When doing evil-search, hand the word over to helm-swoop
  (define-key evil-motion-state-map (kbd "M-i") 'helm-swoop-from-evil-search)

  ;; Instead of helm-multi-swoop-all, you can also use helm-multi-swoop-current-mode
  (define-key helm-swoop-map (kbd "M-m") 'helm-multi-swoop-current-mode-from-helm-swoop))

;;; nyan mode
(use-package nyan-mode
  :ensure t
  :config
  (nyan-mode t)
  
  ;; ;; with animation - buggy
  ;; (if window-system
  ;;     (progn
  ;;       (nyan-mode t)
  ;;       (setq nyan-wavy-trail t)
  ;;         (nyan-start-animation)))
  )

;;; htmlize - Convert buffer text and decorations to HTML
(use-package htmlize
  :ensure t)

;;; hide ^M in logs
(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

;;; groovy mode
(use-package groovy-mode
  :ensure t
  :mode "\\.groovy\\'")

(use-package scss-mode
  :ensure t)

(use-package markdown-mode
  :ensure t)

(use-package restclient
  :ensure t)

;;; typescript
(use-package flycheck
  :ensure t)

(use-package tide
  :ensure t)

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

(add-hook 'typescript-mode-hook #'setup-tide-mode)
;; enable evil collection for tide
(add-hook 'typescript-mode-hook 'evil-collection-tide-setup)

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
