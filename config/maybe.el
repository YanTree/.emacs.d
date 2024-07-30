;;; config/maybe.el -*- lexical-binding: t; -*-
;;; Commentary:
;;
;;; Code:


;;
;;; Encodings

;; Perfer utf-8 encoding for file saving.
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(unless (eq system-type 'windows-nt)
  (set-selection-coding-system 'utf-8))


;;
;;; About operating system

(defconst system-macos-p   (eq system-type 'darwin))
(defconst system-linux-p   (memq system-type '(gnu gnu/linux gnu/kfreebsd berkeley-unix)))
(defconst system-windows-p (memq system-type '(cygwin windows-nt ms-dos)))


;;
;;; Data directory variables

(defvar maybe-emacs-dir user-emacs-directory
  "The path to the currently loaded .emacs.d directory. Must end with a slash.")

(defconst maybe-config-dir (file-name-directory load-file-name)
  "The root directory of Maybe's core files. Must end with a slash.")

(defconst maybe-packages-dir (expand-file-name "packages/" maybe-emacs-dir)
  "The root directory of Maybe's packages files. Must end with a slash.")

(defvar maybe-data-dir (expand-file-name
                        (format "_emacs%s.%s_data/"
                                emacs-major-version
                                emacs-minor-version)
                        maybe-emacs-dir)
  "Local storage for all package's cache files.")


;;
;;; Custom hooks

(defcustom maybe-first-input-hook ()
  "Transient hooks run before the first user input.")

(defcustom maybe-first-file-hook ()
  "Transient hooks run before the first interactively opened file.")

(defcustom maybe-first-buffer-hook ()
  "Transient hooks run before the first interactively opened buffer.")

(defcustom maybe-switch-buffer-hook nil
  "A list of hooks run after changing the current buffer.")

(defcustom maybe-switch-window-hook nil
  "A list of hooks run after changing the focused windows.")

(defcustom maybe-switch-frame-hook nil
  "A list of hooks run after changing the focused frame.")

;; Fire `maybe-switch-buffer-hook'
(defun maybe-run-switch-buffer-hooks(&optional _)
  (let ((gc-cons-threshold most-positive-fixnum)
        (inhibit-redisplay t))
    (run-hooks 'maybe-switch-buffer-hook)))

;; Fire `maybe-switch-frame-hook' and `maybe-switch-window-hook'
(defun maybe-run-switch-window-or-frame-hooks(&optional _)
  (let ((gc-cons-threshold most-positive-fixnum)
        (inhibit-redisplay t))
    (unless (equal (old-selected-frame) (selected-frame))
      (run-hooks 'maybe-switch-frame-hook))
    (unless (or (minibufferp)
                (equal (old-selected-window) (minibuffer-window)))
      (run-hooks 'maybe-switch-window-hook))))

(defun run-hook-once-after (hook-var hook-triggers)
  "Configure HOOK-VAR to be invoked exactly once when any of the TRIGGER-HOOKS
are invoked *after* Emacs has initialized (to reduce false positives). Once
HOOK-VAR is triggered, it is reset to nil.

HOOK-VAR is a quoted hook.
TRIGGER-HOOK is a list of quoted hooks and/or sharp-quoted functions."
  (dolist (hook hook-triggers)
    (let ((fn (make-symbol (format "chain-%s-to-%s" hook-var hook)))
          (running-p nil))
      (fset fn (lambda(&rest _)
        (when (and (not running-p)
              (not (eq hook-var nil)))
          (setq running-p t)
          (run-hooks hook-var)
          (setq hook-var nil))))
         (add-hook hook fn -101)
         fn)))


;;
;;; Startup optimizations

;; Don't resize Emacs frame appears to impact startup time dramatically. The
;; larger the delta, the greater the delay.
(setq frame-inhibit-implied-resize t)

(setq inhibit-startup-screen t     ; Disable *Gnu Emacs* buffer
      ;initial-major-mode 'fundamental-mode ; Reset lisp to fundamental mode for
      initial-scratch-message nil) ; *Scratch* buffer keep empty
;; Prevent "For information about GNU Emacs..." line in *Messages*.
(advice-add #'display-startup-echo-area-message :override #'ignore)
;; Suppress the vanilla startup screen completely. We've disabled it
;; with `inhibit-startup-screen', but it would still initialize anyway.
;; This involves file IO and/or bitmap work (depending on the frame type)
;; that we can no-op for a free 50-100ms saving in startup time.
(advice-add #'display-startup-screen :override #'ignore)

; UNKOWN: From lazycat
;; Premature redisplays/redraws can substantially affect startup
;; times and/or flash a white/unstyled Emacs frame during startup, so I
;; try real hard to suppress them until we're sure the session is ready.
(setq-default inhibit-redisplay t
              inhibit-message t)
;; COMPAT: If the above vars aren't reset, Emacs could appear frozen or
;;   garbled after startup (or in case of an startup error).
(add-hook 'window-setup-hook
    (lambda ()
        (setq-default inhibit-redisplay nil
                      inhibit-message nil)
        (redisplay)))

; UNKOWN: From doom-start.el
;; Performance on Windows is considerably worse than elsewhere. We'll need
(if (boundp 'w32-get-true-file-attributes)
  (setq w32-get-true-file-attributes nil    ; decrease file IO workload
        w32-pipe-read-delay 0               ; faster IPC
        w32-pipe-buffer-size (* 256 1024))) ; read more at a time (was 4K)

;; Cache font, this increases memory usage, however!
(setq inhibit-compacting-font-caches t)

;; Config font/theme/ui
(defun maybe-init-font()
  (defvar default-font-properties '("Maple Mono SC NF" 10 "regular")
    "Font property: default font + size + weight.")

  (let ((default-font (format "%s-%s:%s" (car default-font-properties)
                              (cadr default-font-properties)
                              (caddr default-font-properties))))
    ; default font
    (add-to-list 'default-frame-alist (cons 'font default-font))
    ; set default font
    (set-frame-font default-font)
      ; Specify font for Chinese characters
    (set-fontset-font t 'han default-font))

  ; Specify font for all unicode characters
  (set-fontset-font t 'symbol (font-spec :family "Segoe UI Symbol") nil 'prepend)
  ; emoji
  (set-fontset-font t 'emoji (font-spec :family "Segoe UI Emoji") nil 'prepend))

(defun maybe-init-theme()
  (doom-themes-visual-bell-config) ; Flashing mode-line on errors
  (load-theme 'doom-tomorrow-night t))        ; load one theme of doom-themes

(defun maybe-init-ui()
  ;; Initialize `maybe-switch-window-hook' and `maybe-switch-frame-hook'
  (add-hook 'window-selection-change-functions #'maybe-run-switch-window-or-frame-hooks)
  ;; Initialize `maybe-switch-buffer-hook'
  (add-hook 'window-buffer-change-functions #'maybe-run-switch-buffer-hooks)
  ;; `window-buffer-change-functions' doesn't trigger for files visited via the server.
  (add-hook 'server-visit-hook #'maybe-run-switch-buffer-hooks))

;; Apply font, theme then ui
(add-hook 'after-init-hook   #'maybe-init-font -100)
(add-hook 'after-init-hook   #'maybe-init-theme -90)
(add-hook 'window-setup-hook #'maybe-init-ui -100)

;; When show GUI screen, fire `maybe-first-buffer-hook'
;; Then when press any key(include mouse click), fire `maybe-first-input-hook'
;; Then when open any file(include enter dired and create new file), fire `maybe-first-file-hook'
;;; `maybe-first-buffer-hook' -> `maybe-first-input-hook' -> `maybe-first-file-hook'
(run-hook-once-after 'maybe-first-buffer-hook '(find-file-hook maybe-switch-buffer-hook))
(run-hook-once-after 'maybe-first-input-hook  '(pre-command-hook))
(run-hook-once-after 'maybe-first-file-hook   '(find-file-hook dired-initial-position-hook))

;; The hook invoke turn.
;;   - hook: `after-init-hook'
;;   - hook: `emacs-startup-hook'
;;   - hook: `window-setup-hook'
;;   > After startup is complete:
;;     - On first input:              `maybe-first-input-hook'
;;     - On first switched-to buffer: `maybe-first-buffer-hook'
;;     - On first opened file:        `maybe-first-file-hook'


;;
;;; General UX

;; Not blinking cursor.
(blink-cursor-mode -1)

;; Selection and replaced.
(delete-selection-mode 1)

;; No beeping or blinking please.
(setq ring-bell-function #'ignore)

;; Middle-click paste at point of cursor, not at point of mouse click pos.
(setq mouse-yank-at-point t)

(setq hscroll-margin 7  ; Keep 7 column will touch to left/right edge
      scroll-margin 7)  ; Keep 7 row will touch to top/bottom edge


;;
;;; Fringes

; UNKOWN: From doom-emacs
;; Reduce the clutter in the fringes; we'd like to reserve that space for more
;; useful information, like diff-hl and flycheck.
(setq indicate-buffer-boundaries nil
      indicate-empty-lines nil)


;;
;;; Windows/frames

;; Setting frame title to show current file path.
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

; UNKOWN: From doom-emacs
;; Don't resize the frames in steps; it looks weird, especially in tiling window
;; managers, where it can leave unseemly gaps.
(setq frame-resize-pixelwise t)


;;
;;; Minibuffer

; UNKOWN: From doom-emacs
;; Allow for minibuffer-ception. Sometimes we need another minibuffer command
;; while we're in the minibuffer.
(setq enable-recursive-minibuffers t)

;; Typing `y/n' instead of `yes/no'
(setq use-short-answers t)

;; Echo area show key info more faster after typed.
(setq echo-keystrokes 0.02)


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
(dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
    (add-hook hook #'display-line-numbers-mode))


;;
;;; Time

;; Time format
(setq system-time-locale "C"      ; Timestamp use english instead of chinese
      display-time-24hr-format t) ; Use 00:00 - 24:00 instead of 00:00 - 12:00


;;
;;; File handling

;; When delete file or directory, try move to system trash.
(setq delete-by-moving-to-trash t)

; UNKOWN: From doom-editor.el
;; Resolve symlinks when opening files, so that any operations are conducted
;; from the file's true directory (like `find-file').
(setq find-file-visit-truename t
      vc-follow-symlinks t)

;; More useful kit. Create missing directories when we open a file that doesn't
;; exist under a directory tree that may not exist.
(add-hook 'find-file-not-found-functions
  (defun maybe-create-missing-directories()
    "Automatically create missing directories when creating new files."
    (unless (file-remote-p buffer-file-name)
      (let ((parent-directory (file-name-directory buffer-file-name)))
        (and (not (file-directory-p parent-directory))
             (y-or-n-p (format "Directory `%s' does not exist! Create it?"
                               parent-directory))
             (progn (make-directory parent-directory 'parents)
                    t))))))

;; Disable backup files, we use Git to control version.
(setq make-backup-files nil
      backup-directory-alist (list (cons "." (concat maybe-data-dir "backup/")))
      tramp-backup-directory-alist (list (cons "." (concat maybe-data-dir "tramp-backup/"))))

;; Enable auto save, so we have a fallback in case of crashes or lost data.
;; Use `recover-file' or `recover-session' to recover them.
(setq auto-save-default t
      ; Don't auto-disable auto-save after deleting big chunks.
      auto-save-include-big-deletions t
      auto-save-list-file-prefix (concat maybe-data-dir "auto-save-list/")
      tramp-auto-save-directory  (concat maybe-data-dir "tramp-auto-save-list/"))

;; Put server.el file to `DATA/' folder
(setq custom-file (file-name-concat maybe-data-dir "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;; Put server.el file to `DATA/server/' folder
(setq server-auth-dir (file-name-concat maybe-data-dir "server/"))

;; Redirect eln-cache folder to `DATA/eln-cache/'
(startup-redirect-eln-cache (expand-file-name "eln-cache/" maybe-data-dir))

;; User themes should live in packages/doom-themes/themes, not ~/.emacs.d
(setq custom-theme-directory (concat maybe-packages-dir "doom-themes/themes"))

;; doom-themes put first at `custom-theme-load-path'
(setq custom-theme-load-path
      (cons 'custom-theme-directory
            (delq 'custom-theme-directory custom-theme-load-path)))


;;
;;; Formatting

;; Use spaces instead of tabs.
(setq-default indent-tabs-mode nil
              tab-width 4)

;; Allow tab with other task, not only indent, like completion.
(setq-default tab-always-indent nil)

;; Chars of one line touch the max limit, then create a newline.
(setq-default fill-column 80)

;; Continue wrapped words at whitespace, rather than in the middle of a word.
(setq-default word-wrap t)


;;
;;; Extra file extensions to support

(nconc
 auto-mode-alist
 '(("/LICENSE\\'" . text-mode)
   ("\\.log\\'" . text-mode)))


;;
;;; Global keybind settings
(cond
 (system-windows-p
  (setq w32-lwindow-modifier 'super   ; Left `WIN' key to 'super key
        w32-rwindow-modifier 'super)))


;;
;;; Built-in packages

;; ###Package: `bookmark'
;; Bookmark
(setq bookmark-default-file (expand-file-name "bookmarks" maybe-data-dir))


;; ###Package: `hl-line'
;; Hightlight the line of current cursor positon.
(add-hook 'maybe-first-buffer-hook #'global-hl-line-mode)


;; ###Package: `autorevert'
;; Sync file state when edit at another editor.
(add-hook 'maybe-first-buffer-hook #'global-auto-revert-mode)


;; ###Package: `recentf'
;; Create `recentf' file to note opened files recently.
(defun config-recentf()
  ;; Redirect to `DATA' folder.
  (setq recentf-save-file (concat maybe-data-dir "recentf"))

  (setq recentf-max-saved-items 200        ; Increase limit 20 to 200
        recentf-exclude `("/tmp/" "/ssh:")); These files don't put to `recentf'

  (recentf-mode t))

(add-hook 'maybe-first-input-hook #'config-recentf)


;; ###Package: `savehist'
;; Persist variables across sessions
(defun config-savehist()
  (setq savehist-file (concat maybe-data-dir "history"))

  (savehist-mode t))

(add-hook 'maybe-first-buffer-hook #'config-savehist)


;; ###Package: `saveplace'
;; Persistent cursor point location in buffers
(defun config-saveplace()
  (setq save-place-file (concat maybe-data-dir "places"))

  (save-place-mode t))

(add-hook 'maybe-first-input-hook #'config-saveplace)


;; ###C Source Code: `display-fill-column-indicator-mode'
;; Show one vertical line at `fill-column' position.
(defun config-fill-column-indicator()
  (defvar enable-hook '(emacs-lisp-mode-hook))
  (dolist (hook enable-hook) (add-hook hook #'display-fill-column-indicator-mode)))

(add-hook 'maybe-first-buffer-hook #'config-fill-column-indicator)


;; ###C Source Code: `trailing-whitespace'
;; Empty space on buffer, usually it's annoying, we remove it before save.
;; Don't show trailing whitespace by default
(setq-default show-trailing-whitespace nil)
(defun config-trailing-whitespace()
  (setq show-trailing-whitespace t)
  (add-hook 'before-save-hook #'delete-trailing-whitespace))

(dolist (hook '(prog-mode-hook conf-mode-hook))
  (add-hook hook #'config-trailing-whitespace))


;;
;;; Third packages

;; ###Package: `vertico'
;; Show completion candiantes vertically(M-x + ...)
(with-eval-after-load 'savehist
  (vertico-mode t)) ; Vertico sorts by save history


;; ###Package: `orderless'
;; Fuzzy search by type a space, compatible with vertico
(with-eval-after-load 'vertico
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides nil))


;; ###Package: `corfu'
;; Completion at the position of cursor
  ; M-space fuzzy search
  ; M-h     popup doc
  ; M-g     popup location
(defun config-corfu()
  (setq corfu-auto t
        corfu-auto-prefix 2
        corfu-auto-delay 0.1)

  (global-corfu-mode t)
  (corfu-popupinfo-mode t) ; Popup a window instead of split window

  (with-eval-after-load 'eshell
    (add-hook 'eshell-mode-hook (lambda ()(setq-local corfu-auto nil)))))

(add-hook 'maybe-first-input-hook #'config-corfu)


;;;###package `rainbow-delimiters'
;; Helps us distinguish stacked delimiter pairs, especially in parentheses-drunk
;; languages like Lisp.
(defun config-rainbow-delimiters()
  ; reduce the complexity of the font-lock keyword and hopefully buy us a few ms
  ; of performance.
  (setq rainbow-delimiters-max-face-count 4)

  (rainbow-delimiters-mode t))

(add-hook 'prog-mode-hook #'config-rainbow-delimiters)


;;;###package `diff-hl'
;; Show different base on VERSION control(git, svn...)
  ; `diff-hl-diff-goto-hunk'     C-x v =
  ; `diff-hl-revert-hunk'        C-x v n
  ; `diff-hl-previous-hunk'      C-x v [
  ; `diff-hl-next-hunk'          C-x v ]
  ; `diff-hl-show-hunk'          C-x v *
  ; `diff-hl-stage-current-hunk' C-x v S
  ; `diff-hl-set-reference-rev'
  ; `diff-hl-reset-reference-rev'
  ; `diff-hl-unstage-file'
(defun config-diff-hl()
  ; A slightly faster algorithm for diffing.
  (setq vc-git-diff-switches '("--histogram"))
  ; don't block Emacs when updating vc gutter
  (setq diff-hl-update-async t)

  (add-hook 'find-file-hook #'diff-hl-mode)
  (add-hook 'dired-mode-hook #'diff-hl-dired-mode)
  (add-hook 'diff-hl-mode-hook #'diff-hl-flydiff-mode))

(add-hook 'maybe-first-input-hook #'config-diff-hl)


;;;###package `which-key'
;; Popup a window show keybindings of around pressed key
(defun config-which-key()
  (setq which-key-sort-uppercase-first nil
        which-key-add-column-padding 1)

  (which-key-mode t))

(add-hook 'maybe-first-input-hook #'config-which-key)


(provide 'maybe)
