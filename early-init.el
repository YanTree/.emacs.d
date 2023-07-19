;;; early-init.el -*- lexical-binding: t -*-
;;; Commentary:
;;
;; early-init.el was introduced in Emacs 27.1. It is loaded before init.el,
;; before Emacs initializes its UI or package.el, and before site files are
;; loaded. This is great place for startup optimizing, because only here can you
;; *prevent* things from loading, rather than turn them off after-the-fact.
;;
;;; Code:

;; PERF: Garbage collection is a big contributor to startup times. This fends it
;;   off, but will be reset later by `gcmh-mode'. Not resetting it later will
;;   cause stuttering/freezes.
(setq gc-cons-threshold most-positive-fixnum)

;; PERF: Don't use precious startup time checking mtime on elisp bytecode.
;;   Ensuring correctness is 'doom sync's job, not the interactive session's.
;;   Still, stale byte-code will cause *heavy* losses in startup efficiency.
(setq load-prefer-newer noninteractive)

;; UX: Respect DEBUG envvar as an alternative to --debug-init, and to make are
;;   startup sufficiently verbose from this point on.
(when (getenv-internal "DEBUG")
  (setq init-file-debug t
        debug-on-error t))

;; UI: Disable text button at top, disable image button below text button for
;; rename file, cut file, save file function, disable right scroll bar
;; (push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;;; early-init.el ends here
