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

(defconst system-macos-p    (eq system-type 'darwin))
(defconst system-linux-p    (memq system-type '(gnu gnu/linux gnu/kfreebsd berkeley-unix)))
(defconst system-windows-p  (memq system-type '(cygwin windows-nt ms-dos)))


;;
;;; Data directory variables

(defvar maybe-emacs-dir user-emacs-directory
  "The path to the currently loaded .emacs.d directory. Must end with a slash.")

(defconst maybe-config-dir (file-name-directory load-file-name)
  "The root directory of Maybe's core files. Must end with a slash.")

(defvar maybe-data-dir (expand-file-name (format "_emacs%s.%s_data/" emacs-major-version emacs-minor-version)
                        maybe-emacs-dir)
  "Local storage for all package's cache files.")


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


;;
;;; Hook


;;
;;; Useful global defaults

;; Put server.el file to `DATA/' folder
(setq custom-file (file-name-concat maybe-data-dir "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;; Put server.el file to `DATA/server/' folder
(setq server-auth-dir (file-name-concat maybe-data-dir "server/"))

;; Redirect eln-cache folder to `DATA/eln-cache/' 
(startup-redirect-eln-cache (expand-file-name "eln-cache/" maybe-data-dir))

;; Cache font, this increases memory usage, however!
(setq inhibit-compacting-font-caches t)

; UNKOWN: From doom-start.el
;; Performance on Windows is considerably worse than elsewhere. We'll need
;; everything we can get.
(setq w32-get-true-file-attributes nil    ; decrease file IO workload
      w32-pipe-read-delay 0               ; faster IPC
      w32-pipe-buffer-size (* 64 1024))   ; read more at a time (was 4K)


;;
;;; General UX

;; Not blinking cursor.
(blink-cursor-mode -1)

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


(provide 'maybe)
