;;; early-init.el --- earliest birds               -*- lexical-binding: t -*-
;;; Commentary:
;;
;; early-init.el was introduced in Emacs 27.1. It is loaded before init.el,
;; before Emacs initializes its UI or package.el, and before site files are
;; loaded. This is great place for startup optimizing, because only here can you
;; *prevent* things from loading, rather than turn them off after-the-fact.
;;
;;; Code:


;; Load the latest changed file in .el and .elc files. If you modify a .el file
;; and not compile it to .elc, then emacs will load .el file.
(setq load-prefer-newer t)

;; We use `borg' instead `package.el' to install package, not need `package.el'
;; to download, compile, load packages, so disable it.
(setq package-enable-at-startup nil)

;; Setting frame title to show file path.
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; Menu, icon button and scroll bar settings, menu is so useful to turn on.
;; (push '(menu-bar-lines . 0) default-frame-alist)   ; Disable menus
(push '(tool-bar-lines . 0) default-frame-alist)   ; Disable icon button at menus
(push '(vertical-scroll-bars) default-frame-alist) ; Disable scroll bar


;; Perfer utf-8 encoding for file saving.
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(unless (eq system-type 'windows-nt)
  (set-selection-coding-system 'utf-8))


;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; End:
;;; early-init.el ends here
