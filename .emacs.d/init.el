;;; packages to install
(setq package-list '(color-theme-sanityinc-tomorrow
                     idea-darkula-theme
                     base16-theme
                     evil
                     evil-collection
                     evil-replace-with-register
                     goto-chg
                     monokai-theme
                     undo-tree
                     smooth-scrolling
                     ace-jump-mode
                     cider
                     clojure-mode
                     ac-cider
                     auto-complete
                     nyan-mode
                     htmlize
                     groovy-mode
                     rainbow-delimiters
                     smartparens
                     evil-smartparens
                     clj-refactor
                     projectile
                     cider-eval-sexp-fu
                     org-pomodoro
                     org-trello
                     scss-mode
                     markdown-mode
                     helm
                     helm-swoop
                     yasnippet
                     restclient
                     flycheck
                     tide
                     company
                     company-quickhelp
                     magit
                     ;;evil-magit
                     secretaria
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

;;; evil mode by default
;; also enable evil-collection
(setq evil-want-integration t)
(setq evil-want-keybinding nil)
(require 'evil)
(evil-mode t)
(when (require 'evil-collection nil t)
  (evil-collection-init))
;; enable undo-redo
(evil-set-undo-system 'undo-redo)
;; enable evil-replace-with-register
(require 'evil-replace-with-register)
(evil-replace-with-register-install)

;;; ace jump mode
(require 'ace-jump-mode)
(define-key global-map (kbd "C-;") 'ace-jump-mode)
;; evil-mode+ace-jump-mode bindings
(define-key evil-motion-state-map (kbd "C-;") #'evil-ace-jump-char-mode)
;; (define-key evil-motion-state-map (kbd "C-SPC") #'evil-ace-jump-word-mode)

(define-key evil-operator-state-map (kbd "C-;") #'evil-ace-jump-char-mode) ; similar to f
;; (define-key evil-operator-state-map (kbd "C-SPC") #'evil-ace-jump-char-to-mode) ; similar to t
;; (define-key evil-operator-state-map (kbd "M-SPC") #'evil-ace-jump-word-mode)

;; different jumps for different visual modes
(defadvice evil-visual-line (before spc-for-line-jump activate)
(define-key evil-motion-state-map (kbd "C-;") #'evil-ace-jump-line-mode))

(defadvice evil-visual-char (before spc-for-char-jump activate)
(define-key evil-motion-state-map (kbd "C-;") #'evil-ace-jump-char-mode))

(defadvice evil-visual-block (before spc-for-char-jump activate)
(define-key evil-motion-state-map (kbd "C-;") #'evil-ace-jump-char-mode))

;;; projectile
(projectile-global-mode)
;; (define-key projectile-mode-map projectile-keymap-prefix nil)
(define-key projectile-mode-map (kbd "M-p") #'projectile-command-map)

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
  (set-face-attribute 'default nil :family "Hack")
  (add-to-list 'default-frame-alist '(font . "Hack-12"))
)

;;; custom font - cygwin and windows
(when (or (system-is-cygwin) (system-is-windows))
  (set-face-attribute 'default nil :family "Consolas")
  (add-to-list 'default-frame-alist '(font . "Consolas-12"))
)
  

;;; custom color theme
;;(require 'color-theme-sanityinc-tomorrow)
(customize-set-variable 'custom-safe-themes (quote ("f700bc979515153bef7a52ca46a62c0aa519950cc06d539df4f3d38828944a2c" "cfce7968302b78671dca1e940b5d5f38f997df79c85b16dc2886e7b735f00798" "5a04c3d580e08f5fc8b3ead2ed66e2f0e5d93643542eec414f0836b971806ba9" "16dd114a84d0aeccc5ad6fd64752a11ea2e841e3853234f19dc02a7b91f5d661" "420689cc31d01fe04b8e3adef87b8838ff52faa169e69ca4e863143ae9f3a9f9" "e068203104e27ac7eeff924521112bfcd953a655269a8da660ebc150c97d0db8" default)))
(customize-set-variable 'custom-enabled-themes (quote (base16-default-dark)))

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
(require 'org-trello)                   ;
;; org-trello major mode for all .trello files
(add-to-list 'auto-mode-alist '("\\.trello$" . org-mode))

;; add a hook function to check if this is trello file, then activate the org-trello minor mode.
(add-hook 'org-mode-hook
          (lambda ()
            (let ((filename (buffer-file-name (current-buffer))))
              (when (and filename (string= "trello" (file-name-extension filename)))
              (org-trello-mode)))))


;;; secretaria
(require 'secretaria)
(add-hook 'after-init-hook #'secretaria-unknown-time-always-remind-me)

;;; yasnippet
(require 'yasnippet)
(yas-reload-all)
(add-hook 'prog-mode-hook #'yas-minor-mode)
(defun yas/org-very-safe-expand ()
  (let ((yas/fallback-behavior 'return-nil)) (yas/expand)))
(add-hook 'org-mode-hook
          (lambda ()
            (yas-minor-mode)
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

;; ;;; general auto-complete
;; (require 'auto-complete-config)
;; (setq ac-delay 0.0)
;; (setq ac-quick-help-delay 0.5)
;; (ac-config-default)

;;; company auto-complete
(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'after-init-hook 'company-quickhelp-mode)
(setq company-idle-delay 0.0)
(setq company-dabbrev-downcase nil)
(global-set-key [C-tab] #'company-complete) ; use C-TAB as manual trigger
(define-key evil-insert-state-map (kbd "C-SPC") #'company-complete)

;; ;;; magit mode
;; (require 'evil-magit)
;; (global-set-key (kbd "C-x g") 'magit-status)

;;; clojure mode
(add-hook 'smartparens-enabled-hook #'evil-smartparens-mode)
(add-hook 'clojure-mode-hook #'subword-mode)
(add-hook 'clojure-mode-hook #'smartparens-strict-mode)
(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook #'show-smartparens-mode)
(require 'cider-eval-sexp-fu)

;; ;;; cider autocomplete
;; (require 'ac-cider)
;; (add-hook 'cider-mode-hook 'ac-flyspell-workaround)
;; (add-hook 'cider-mode-hook 'ac-cider-setup)
;; (add-hook 'cider-repl-mode-hook 'ac-cider-setup)
;; (eval-after-load "auto-complete"
;;   '(progn
;;      (add-to-list 'ac-modes 'cider-mode)
;;      (add-to-list 'ac-modes 'cider-repl-mode)))

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
(require 'smooth-scrolling)
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
(require 'helm)

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

(require 'helm-swoop)

;; helm-swoop keybindings
(global-set-key (kbd "M-i") 'helm-swoop)
(global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)
(global-set-key (kbd "C-c M-i") 'helm-multi-swoop)
(global-set-key (kbd "C-x M-i") 'helm-multi-swoop-all)

;; When doing isearch, hand the word over to helm-swoop
(define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
;; From helm-swoop to helm-multi-swoop-all
(define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)
;; When doing evil-search, hand the word over to helm-swoop
(define-key evil-motion-state-map (kbd "M-i") 'helm-swoop-from-evil-search)

;; Instead of helm-multi-swoop-all, you can also use helm-multi-swoop-current-mode
(define-key helm-swoop-map (kbd "M-m") 'helm-multi-swoop-current-mode-from-helm-swoop)

;; If nil, you can slightly boost invoke speed in exchange for text color
(setq helm-swoop-speed-or-color t)

;; helm-swoop fuzzy matching
(setq helm-swoop-use-fuzzy-match t)

;; Disable pre-input
(setq helm-swoop-pre-input-function
      (lambda () ""))

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

;;; typescript
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

;;; secretaria
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
             :mode 'org-mode))))

;;; load custom settings
(setq custom-file "~/.emacs.d/init_custom.el")
(if (file-exists-p "~/.emacs.d/init_custom.el") (load-library "~/.emacs.d/init_custom.el"))
