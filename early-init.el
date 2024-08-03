;;; early-init.el --- earliest birds               -*- lexical-binding: t -*-
;;; Commentary:
;;
;; early-init.el was introduced in Emacs 27.1. It is loaded before init.el,
;; before Emacs initializes its UI or package.el, and before site files are
;; loaded. This is great place for startup optimizing, because only here can you
;; *prevent* things from loading, rather than turn them off after-the-fact.
;;
;;; Code:


;; This will be set back to normal at the end of the init file
(defvar maybe-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Garbage collection is a big contributor to startup times. This fends it
;; off, but will be reset later by `gcmh-mode'. Not resetting it later causes
;; stuttering/freezes.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 1.0)

;; Don't use precious startup time checking mtime on elisp bytecode.
(setq load-prefer-newer noninteractive)

;; Suppress package.el. Since Emacs 27, package initialization occurs before
;; `user-init-file' is loaded, but after `early-init-file'. We use `borg' instead
;; `package.el' to install package.
(setq package-enable-at-startup nil)

;; I intentionally avoid calling `menu-bar-mode', `tool-bar-mode', and
;; `scroll-bar-mode' because their manipulation of frame parameters can
;; trigger/queue a superfluous (and expensive, depending on the window system)
;; frame redraw at startup. The variables must be set to `nil' as well so
;; users don't have to call the functions twice to re-enable them.
;; (push '(menu-bar-lines . 0) default-frame-alist)   ; Disable menus
(push '(tool-bar-lines . 0) default-frame-alist)   ; Disable icon button at menus
(push '(vertical-scroll-bars) default-frame-alist) ; Disable scroll bar
(setq ;menu-bar-mode nil    ; Disable menus
      tool-bar-mode nil    ; Disable icon button at menus
      scroll-bar-mode nil) ; Disable scroll bar

;; Set frame position and size.
(setq initial-frame-alist '((top   . 0.5)   (left   . 0.5)
                            (width . 0.628) (height . 0.8)
                            (fullscreen)))


;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; End:
;;; early-init.el ends here
