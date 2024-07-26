;;; init.el --- user-init-file                    -*- lexical-binding: t -*-
;;; Commentary:
;;
;;; Code:


;; Not limit read from processes size in a single chunk.
(setq process-adaptive-read-buffering nil)
;; Increase how much is read from processes in a single chunk (default is 4kb).
;; This is further increased elsewhere, where needed (like our LSP module).
(setq read-process-output-max (* 256 1024))  ; 256kb

;; Load heart of configs
(require 'maybe)

(load-theme 'leuven)


;;
;;; Core 

;; ###Package: `borg'
;; Manage packages, download, build, add to load-path
(eval-and-compile
  (add-to-list 'load-path (expand-file-name "packages/borg" maybe-emacs-dir))
  (require 'borg)
  (borg-initialize))


;;
;;; Restore

;; I make no assumptions about the config we're going to load, so undo this
;; file's global side-effects.
(setq load-prefer-newer t)

;; Garbage collection is a big contributor to startup times. This fends it off,
;; but will be reset later to normal by gcmh. (16mb)
(add-hook 'maybe-first-input-hook
          (lambda () (setq gc-cons-threshold (* 16 1024 1024))))

;;
(defun config-gcmh()
    (setq gcmh-idle-delay 'auto          ; 1. Switch to auto(default is 15s)
          gcmh-auto-idle-delay-factor 10 ; 2. Then we can use custom delay time
          gcmh-high-cons-threshold (* 16 1024 1024)) ; 16mb
    (gcmh-mode 1)) ; Enable gcmh

;; Fire `gcmh'
(add-hook 'maybe-first-input-hook #'config-gcmh)


;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; init.el ends here
