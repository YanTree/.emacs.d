;;; init.el -*- lexical-binding: t -*-
;;; Commentary:
;; 1. emacs 28
;;
;;; Code:


;; PERF: Garbage collection is a big contributor to startup times. This fends it
;;   off, but will be reset later to normal. Not resetting it later will
;;   cause stuttering/freezes.
(let ((normal-gc-cons-threshold (* 16 1024 1024))
      (init-gc-cons-threshold (* 512 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;;
;;; About operating system
(defconst IS-MAC      (eq system-type 'darwin))
(defconst IS-LINUX    (memq system-type '(gnu gnu/linux gnu/kfreebsd berkeley-unix)))
(defconst IS-WINDOWS  (memq system-type '(cygwin windows-nt ms-dos)))
(defconst IS-BSD      (memq system-type '(darwin berkeley-unix gnu/kfreebsd)))

;;; Font setting for default font, font for all unicode characters, emoji and
;; chinese characters
(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))

(defun cat-setup-fonts ()
  "Setup fonts."
  (when (display-graphic-p)
    ;; Set default font
    (cl-loop for font in '("Cascadia Code" "Fira Code" "Jetbrains Mono"
                           "SF Mono" "Hack" "Source Code Pro" "Menlo"
                           "Monaco" "DejaVu Sans Mono" "Consolas")
             when (font-installed-p font)
             return (set-face-attribute 'default nil
                                        :family font
                                        :height (cond (IS-MAC 130)
                                                      (IS-WINDOWS 110)
                                                      (t 100))))
    ;; Specify font for all unicode characters
    (cl-loop for font in '("Segoe UI Symbol" "Symbola" "Symbol")
             when (font-installed-p font)
             return (if (< emacs-major-version 27)
                        (set-fontset-font "fontset-default" 'unicode font nil 'prepend)
                      (set-fontset-font t 'symbol (font-spec :family font) nil 'prepend)))

    ;; Emoji
    (cl-loop for font in '("Noto Color Emoji" "Apple Color Emoji" "Segoe UI Emoji")
             when (font-installed-p font)
             return (cond
                     ((< emacs-major-version 27)
                      (set-fontset-font "fontset-default" 'unicode font nil 'prepend))
                     ((< emacs-major-version 28)
                      (set-fontset-font t 'symbol (font-spec :family font) nil 'prepend))
                     (t
                      (set-fontset-font t 'emoji (font-spec :family font) nil 'prepend))))

    ;; Specify font for Chinese characters
    (cl-loop for font in '("WenQuanYi Micro Hei" "PingFang SC" "Microsoft Yahei" "STFangsong")
             when (font-installed-p font)
             return (progn
                      (setq face-font-rescale-alist `((,font . 1.3)))
                      (set-fontset-font t '(#x4e00 . #x9fff) (font-spec :family font))))))

(cat-setup-fonts)
(add-hook 'window-setup-hook #'cat-setup-fonts)
(add-hook 'server-after-make-frame-hook #'cat-setup-fonts)


;;
;;; Startup
(progn
  (defvar before-user-init-time (current-time)
    "Value of `current-time' when Emacs begins loading `user-init-file'.")
  (message "Loading Emacs...done (%.3fs)"
           (float-time (time-subtract before-user-init-time
                                      before-init-time)))
  (setq user-init-file (or load-file-name buffer-file-name))
  (setq user-emacs-directory (file-name-directory user-init-file))
  (message "Loading %s..." user-init-file)
  (when (< emacs-major-version 27)
    ;;; Suppress package.el
    ;; Since Emacs 27, package initialization occurs before `user-init-file' is
    ;; loaded, but after `early-init-file'. Doom handles package initialization, so
    ;; we must prevent Emacs from doing it again.
    (setq package-enable-at-startup nil)
    ;; (package-initialize)
    (load-file (expand-file-name "early-init.el" user-emacs-directory)))
  (setq inhibit-startup-buffer-menu t)
  (setq inhibit-startup-screen t)
  (setq inhibit-startup-echo-area-message "locutus")
  (setq initial-buffer-choice t))

;;
;;; Data directory
(defvar cat-local-dir (expand-file-name "local/" user-emacs-directory)
  "Root directory for local storage")

(defvar cat-cache-dir (expand-file-name "cache/" cat-local-dir)
  "Subfolder for `local', where stores its global cache files")

(defvar cat-data-dir (expand-file-name "data/" cat-local-dir)
  "Subfolder for `local', where stores its global data files")

(defvar cat-modules-dir (expand-file-name "modules/" user-emacs-directory)
  "Where stores all git submodules")

(defvar cat-lisp-dir (expand-file-name "lisp/" user-emacs-directory)
  "Where stores user personal config files")

(defvar cat-templates-dir (expand-file-name "templates/" user-emacs-directory)
  "Where sotres snippets and other template files")

(defconst cat-custom-example-file (expand-file-name "custom.example.el" cat-templates-dir)
  "The path to example custom file")


;;
;;; Core 

;; Load `borg'
(eval-and-compile
  (add-to-list 'load-path (expand-file-name "borg" cat-modules-dir))
  (require 'borg)
  (borg-initialize))

;; `use-package'
(eval-and-compile
  (setq use-package-verbose t)
  (setq use-package-enable-imenu-support t)
  (require  'use-package))

;; Package epkg config
(use-package epkg
  :defer t
  :init
  (setq epkg-repository
        (expand-file-name "epkgs/" cat-data-dir))
  (setq epkg-database-connector
        (if (>= emacs-major-version 29) 'sqlite-builtin 'sqlite-module)))


;; ---------------------------------------------------------------------------
;; Better default settings

;;
;;; Better default
(progn
  ;; Load custom.el file
  (setq custom-file (expand-file-name "custom.el" cat-data-dir))
  (when (file-exists-p custom-file)
    (load custom-file))

  ;; Load server
  ;; (or (server-running-p) (server-mode))

  ;; Paste overwrite marked text
  (delete-selection-mode 1)

  ;; Keep font caches to avoid compact during GC
  (setq inhibit-compacting-font-caches t)

  ;; Disable ring bell, it's annoying
  (setq ring-bell-function #'ignore)

  ;; Don't generate backups or lockfiles. While auto-save maintains a copy so long
  ;; as a buffer is unsaved, backups create copies once, when the file is first
  ;; written, and never again until it is killed and reopened. This is better
  ;; suited to version control, and I don't want world-readable copies of
  ;; potentially sensitive material floating around our filesystem.
  (setq create-lockfiles nil
        make-backup-files nil
        ;; But in case the user does enable it, some sensible defaults:
        version-control t     ; number each backup file
        backup-by-copying t   ; instead of renaming current file (clobbers links)
        delete-old-versions t ; clean up after itself
        kept-old-versions 5
        kept-new-versions 5
        backup-directory-alist (list (cons "." (concat cat-data-dir "backup/")))
        tramp-backup-directory-alist backup-directory-alist)

  ;; But turn on auto-save, so we have a fallback in case of crashes or lost data.
  ;; Use `recover-file' or `recover-session' to recover them.
  (setq auto-save-default t
        ;; Don't auto-disable auto-save after deleting big chunks. This defeats
        ;; the purpose of a failsafe. This adds the risk of losing the data we
        ;; just deleted, but I believe that's VCS's jurisdiction, not ours.
        auto-save-include-big-deletions t
        ;; Keep it out of `cat-emacs-dir' or the local directory.
        auto-save-list-file-prefix (concat cat-data-dir "auto-save/sessions/")
        tramp-auto-save-directory  (concat cat-data-dir "tramp/auto-save/")
        tramp-persistency-file-name(concat cat-data-dir "tramp/persistency.el")
        auto-save-file-name-transforms
        (list (list "\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
                    ;; Prefix tramp autosaves to prevent conflicts with local ones
                    (concat auto-save-list-file-prefix "tramp-\\2") t)
              (list ".*" auto-save-list-file-prefix t)))


  ;;
  ;;; Formatting

  ;; Favor spaces over tabs, 8-space tabs more consistent
  ;; 4-space tabs.
  (setq-default indent-tabs-mode nil
          tab-width 8)

  ;; First hitting TAB tries to indent current line, if the line was already indent,
  ;; then try to complete the thing at point
  (setq tab-always-indent 'complete)

  ;; Column beyond 80 which automatic line-wrapping should happen
  (setq-default fill-column 80)


  ;;
  ;;; Line numbers

  ;; Explicitly define a width to reduce the cost of on-the-fly computation
  (setq-default display-line-numbers-width 3)

  ;; Show absolute line numbers for narrowed regions to make it easier to tell the
  ;; buffer is narrowed, and where you are, exactly.
  (setq-default display-line-numbers-widen t)

  ;; Enable line numbers in most text-editing modes. We avoid
  ;; `global-display-line-numbers-mode' because there are many special and
  ;; temporary modes where we don't need/want them.
  (dolist (mode-alist '(prog-mode-hook
                        text-mode-hook
                        conf-mode-hook))
    (add-hook mode-alist 'display-line-numbers-mode))


  ;;
  ;;; Scrolling

  (setq hscroll-margin 2
        hscroll-step 1
        ;; Emacs spends too much effort recentering the screen if you scroll the
        ;; cursor more than N lines past window edges (where N is the settings of
        ;; `scroll-conservatively'). This is especially slow in larger files
        ;; during large-scale scrolling commands. If kept over 100, the window is
        ;; never automatically recentered.
        scroll-conservatively 101
        scroll-margin 0
        scroll-preserve-screen-position t
        ;; Reduce cursor lag by a tiny bit by not auto-adjusting `window-vscroll'
        ;; for tall lines.
        auto-window-vscroll nil)


  ;;
  ;;; Cursor

  ;; The blinking cursor is distracting
  (blink-cursor-mode -1)

  ;; Don't stretch the cursor to fit wide characters, it is disorienting,
  ;; especially for tabs.
  (setq x-stretch-cursor nil)


  ;;
  ;;; Window/frames

  ;; A simple frame title
  (setq frame-title-format '("%b"))

  ;; Disable GNU guide start screen
  (setq inhibit-startup-screen t)

  ;; Frame size
  (add-to-list 'initial-frame-alist '(height . 40))
  (add-to-list 'initial-frame-alist '(width . 120))


  ;;
  ;;; Minibuffer

  ;; Allow for minibuffer-ception. Sometimes we need another minibuffer command
  ;; while we're in the minibuffer.
  (setq enable-recursive-minibuffers t)

  ;; Show current key-sequence in minibuffer ala 'set showcmd' in vim. Any
  ;; feedback after typing is better UX than no feedback at all.
  (setq echo-keystrokes 0.02)

  ;; Typeing yes/no is obnoxious when y/n will do
  (if (boundp 'use-short-answers)
      (setq use-short-answers t))

  ;; Try to keep the cursor out of the read-only portions of the minibuffer.
  (setq minibuffer-prompt-properties '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)


  ;;
  ;;; Extra file extensions to support

  (nconc
  auto-mode-alist
  '(("/LICENSE\\'" . text-mode)
    ("\\.log\\'" . text-mode)
    ("rc\\'" . conf-mode)
    ("\\.\\(?:hex\\|nes\\)\\'" . hexl-mode)))
)

;; Startup
(progn
  (message "Loading early birds...done (%.3fs)"
           (float-time (time-subtract (current-time)
                                      before-user-init-time))))

;;
;;; Built-in packages

;;;###package bookmark
(setq bookmark-default-file (concat cat-data-dir "bookmarks.el"))

;;;###package ansi-color
(setq ansi-color-for-comint-mode t)

;;;###package whitespace
(setq whitespace-line-column nil
      whitespace-style
      '(face indentation tabs tab-mark spaces space-mark newline newline-mark
        trailing lines-tail)
      whitespace-display-mappings
      '((tab-mark ?\t [?› ?\t])
        (newline-mark ?\n [?¬ ?\n])
        (space-mark ?\  [?·] [?.])))

;; highlight matching delimiters   
(use-package paren     
  :config
  (setq show-paren-delay 0.1
        show-paren-highlight-openparen t
        show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t)

  ;; Fire show paren mode
  (show-paren-mode))


;; Highlights the current line
(use-package hl-line
  :custom
  ;; Fire global hl line mode
  (global-hl-line-mode 1)
  :config
  (defvar global-hl-line-modes
  '(prog-mode text-mode conf-mode special-mode
    org-agenda-mode dired-mode)
  "What modes to enable `hl-line-mode' in.")

  ;; HACK I reimplement `global-hl-line-mode' so we can white/blacklist modes in
  ;;      `global-hl-line-modes' _and_ so we can use `global-hl-line-mode',
  ;;      which users expect to control hl-line in Emacs.
  (define-globalized-minor-mode global-hl-line-mode hl-line-mode
    (lambda ()
      (and (cond (hl-line-mode nil)
                 ((null global-hl-line-modes) nil)
                 ((eq global-hl-line-modes t))
                 ((eq (car global-hl-line-modes) 'not)
                  (not (derived-mode-p global-hl-line-modes)))
                 ((apply #'derived-mode-p global-hl-line-modes)))
           (hl-line-mode +1))))

  ;; Temporarily disable `hl-line' when selection is active, since it doesn't
  ;; serve much purpose when the selection is so much more visible.
  (defvar cat--hl-line-mode nil)

  (add-hook 'hl-line-mode-hook
    (defun cat-truly-disable-hl-line-h ()
      (unless hl-line-mode
        (setq-local cat--hl-line-mode nil))))

  (add-hook 'activate-mark-hook
    (defun cat-disable-hl-line-h ()
      (when hl-line-mode
        (hl-line-mode -1)
        (setq-local cat--hl-line-mode t))))

  (add-hook 'deactivate-mark-hook
    (defun cat-enable-hl-line-maybe-h ()
      (when cat--hl-line-mode
        (hl-line-mode +1)))))


;; revert buffers when their files/state have changed
(use-package autorevert
  :custom
  (auto-revert-verbose t)           ;; let us know when it happens
  (auto-revert-use-notify nil)
  (auto-revert-stop-on-user-input nil)
  (revert-without-query (list ".")) ;; Only prompts for confirmation when buffer is unsaved.
  :config
  ;; Fire global auto revert mode
  (global-auto-revert-mode))


;; persist variables across sessions
(use-package savehist
  :custom (savehist-file (concat cat-data-dir "savehist.el"))
  :config
  (setq savehist-save-minibuffer-history t
        savehist-autosave-interval nil     ; save on kill only
        savehist-additional-variables
        '(kill-ring                        ; persist clipboard
          register-alist                   ; persist macros
          mark-ring global-mark-ring       ; persist marks
          search-ring regexp-search-ring)) ; persist searches
  (add-hook 'savehist-save-hook
    (defun cat-savehist-unpropertize-variables-h ()
      "Remove text properties from `kill-ring' to reduce savehist cache size."
      (setq kill-ring
            (mapcar #'substring-no-properties
                    (cl-remove-if-not #'stringp kill-ring))
            register-alist
            (cl-loop for (reg . item) in register-alist
                     if (stringp item)
                     collect (cons reg (substring-no-properties item))
                     else collect (cons reg item))))
    (defun cat-savehist-remove-unprintable-registers-h ()
      "Remove unwriteable registers (e.g. containing window configurations).
Otherwise, `savehist' would discard `register-alist' entirely if we don't omit
the unwritable tidbits."
      ;; Save new value in the temp buffer savehist is running
      ;; `savehist-save-hook' in. We don't want to actually remove the
      ;; unserializable registers in the current session!
      (setq-local register-alist
                  (cl-remove-if-not #'savehist-printable register-alist))))

  ;; Fire savehist mode
  (savehist-mode))


;; persistent point location in buffers
(use-package saveplace
  :custom (save-place-file (concat cat-data-dir "save-place.el"))
  :config
  ;; Fire save place mode
  (save-place-mode))


;; Keep track of recently opened files
(use-package recentf
  :demand t
  :commands recentf-open-files
  :custom (recentf-save-file (concat cat-data-dir "recentf-save.el"))
  :config
  (setq recentf-auto-cleanup nil     ;; Don't. We'll auto-cleanup on shutdown
        recentf-max-saved-items 200) ;; default is 20

  (setq recentf-show-abbreviated t)

  ;; Text properties inflate the size of recentf's files, and there is
  ;; no purpose in persisting them (Must be first in the list!)
  (add-to-list 'recentf-filename-handlers #'substring-no-properties)

  (add-hook 'dired-mode-hook
    (defun cat--recentf-add-dired-directory-h ()
      "Add dired directories to recentf file list."
      (recentf-add-file default-directory)))

  ;; The most sensible time to clean up your recent files list is when you quit
  ;; Emacs (unless this is a long-running daemon session).
  (setq recentf-auto-cleanup (if (daemonp) 300))
  (add-hook 'kill-emacs-hook #'recentf-cleanup))

;; File manager
(use-package dired
  :config
  ;; More convenitent for Copy/Move files, deleted files move to system trash,
  ;; delete/copy whole folder
  (setq dired-dwim-target t
        delete-by-moving-to-trash t
        dired-recursive-deletes 'always
        dired-recursive-copies 'always)
  
  ;; Avoid open too many dired buffer
  (put 'dired-find-alternate-file 'disabled nil))


;; ---------------------------------------------------------------------------
;; Minibuffer

;; Show candidate vertically at minibuffer
(use-package vertico
  :demand t
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
  :config
  ;; (vertico-indexed-mode)
  (setq vertico-count 12)

  (defvar +vertico-current-arrow t)
  
  ;; Prefix the current candidate with “▶ “.
  (cl-defmethod vertico--format-candidate :around
    (cand prefix suffix index start &context ((and +vertico-current-arrow
                                                   (not (bound-and-true-p vertico-flat-mode)))
                                              (eql t)))
    (setq cand (cl-call-next-method cand prefix suffix index start))
    (if (bound-and-true-p vertico-grid-mode)
        (if (= vertico--index index)
            (concat #("▶" 0 1 (face vertico-current)) cand)
          (concat #("_" 0 1 (display " ")) cand))
      (if (= vertico--index index)
          (concat
           #(" " 0 1 (display (left-fringe right-triangle vertico-current)))
           cand)
        cand)))

   ;; Fire vertico     
  (vertico-mode))


;; More marginalia info for minibuffer
(use-package marginalia
  :init (marginalia-mode))


;; ---------------------------------------------------------------------------
;; completion

;; Fuzzy matching for minibuffer/completion
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))


;; Enhance in-buffer completion
(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 3)
  :config
  ;; Disable corfu-auto at eshell
  (with-eval-after-load 'eshell
    (add-hook 'eshell-mode-hook (lambda () (setq-local corfu-auto nil))))

  ;; WARNING: borg can't complie corfu-popupinfo.el Turn extra info
  (with-eval-after-load 'corfu
     (corfu-popupinfo-mode))
  
  ;; Fire global corfu mode
  (global-corfu-mode))


;; Add icon for corfu completion result(nerd-icons)
(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ;; to compute blended backgrounds correctly
  :config  
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))


;; ---------------------------------------------------------------------------
;; edit

;; Useful to delete huge whitespace
(use-package hungry-delete
  :config (global-hungry-delete-mode))


;; Rainbow brackets
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))


;; High light same symbol at buffer, with rename... feature
(use-package symbol-overlay
  :hook ((prog-mode html-mode yaml-mode conf-mode) . symbol-overlay-mode))


;; Multiple cursor to edit
(use-package multiple-cursors
  :custom
  (setq mc/list-file (concat cat-data-dir "mc-lists.el")))


;; Pop a window show relative keybinding
(use-package which-key
  :config
  (setq-default which-key-idle-delay 2)
  
  ;; Fire which key mode
  (which-key-mode))


;; M-<UP>/M-<DOWN> move current line up/down
(use-package move-dup
  :config (global-move-dup-mode))


;; ---------------------------------------------------------------------------
;; version control

;; Magic for git intergeration with emacs
(use-package magit
  :config
  (magit-add-section-hook 'magit-status-sections-hook
                            'magit-insert-modules
                            'magit-insert-stashes
                            'append))


;; ---------------------------------------------------------------------------
;; keybinding

;; All package keybinding here
(use-package keybinding
  :no-require t
  ;; 
  ;;; vertico Ido-like directory navigation commands
  :bind (:map vertico-map
            ("RET"   . vertico-directory-enter)
            ("DEL"   . vertico-directory-delete-char)
            ("M-DEL" . vertico-directory-delete-word))
  
  ;; 
  ;;; hungry-delete
  :bind ("C-=" . er/expand-region)
  
  ;;
  ;;; symbol-overlay
  :bind (:map symbol-overlay-mode-map
              (("M-i" . symbol-overlay-put)
               ("M-I" . symbol-overlay-remove-all)
               ("M-n" . symbol-overlay-jump-next)
               ("M-p" . symbol-overlay-jump-prev)))
  
  ;;
  ;;; multiple-cursors
  :bind (("C-<"     . mc/mark-previous-like-this)
         ("C->"     . mc/mark-next-like-this)
         ("C-+"     . mc/mark-next-like-this)
         ("C-c C-<" . mc/mark-all-like-this))

  ;; 
  ;;; move-dup
  :bind (("M-<up>"     . move-dup-move-lines-up)
        ("M-<down>"   . move-dup-move-lines-down)
        ("M-S-<up>"   . move-dup-duplicate-up)
        ("M-S-<down>" . move-dup-duplicate-down))
  
  ;; 
  ;;; magit
  :bind ("C-x g" . magit-status))


;; ---------------------------------------------------------------------------
;; Loading done
(progn ;     startup
  (message "Loading %s...done (%.3fs)" user-init-file
           (float-time (time-subtract (current-time)
                                      before-user-init-time)))
  (add-hook 'after-init-hook
            (lambda ()
              (message
               "Loading %s...done (%.3fs) [after-init]" user-init-file
               (float-time (time-subtract (current-time)
                                          before-user-init-time))))
            t))

;; (progn ;     personalize
;;   (let ((file (expand-file-name (concat (user-real-login-name) ".el")
;;                                 user-emacs-directory)))
;;     (when (file-exists-p file)
;;       (load file))))

;;; init.el ends here
