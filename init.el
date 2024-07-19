;;; init.el --- user-init-file                    -*- lexical-binding: t -*-
;;; Early birds


;; Increase how much is read from processes in a single chunk (default is 4kb).
;; This is further increased elsewhere, where needed (like our LSP module).
(setq read-process-output-max (* 128 1024))  ; 128kb

;; PERF: Garbage collection is a big contributor to startup times. This fends it
;;   off, but will be reset later to normal. Not resetting it later will
;;   cause stuttering/freezes.
(let ((normal-gc-cons-threshold (* 16 1024 1024)) ; 16mb
      (init-gc-cons-threshold (* 512 1024 1024))) ; 512mb
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))


;;
;;; Load init.el

;; Loading init.el time
(progn 
  (defvar before-user-init-time (current-time)
    "Value of `current-time' when Emacs begins loading `user-init-file'.")

  (message "Loading Emacs...done (%.3fs)"
           (float-time (time-subtract before-user-init-time
                                      before-init-time)))
  (setq user-init-file (or load-file-name buffer-file-name))
  (setq user-emacs-directory (file-name-directory user-init-file))
  (message "Loading %s..." user-init-file)
  
  ;; Theme, light theme `leuven'; dark theme 
  (load-theme 'leuven)
  ;(setq inhibit-startup-buffer-menu t) ; TODO: not clear
  ;(setq inhibit-startup-screen t)      ; Disable `welcome' buffer
  )


;;
;;; About operating system

(defconst IS-MAC      (eq system-type 'darwin))
(defconst IS-LINUX    (memq system-type '(gnu gnu/linux gnu/kfreebsd berkeley-unix)))
(defconst IS-WINDOWS  (memq system-type '(cygwin windows-nt ms-dos)))
(defconst IS-BSD      (memq system-type '(darwin berkeley-unix gnu/kfreebsd)))


;;
;;; Data directory

(defvar maybe-data-dir (expand-file-name (format "%s.%s-data" emacs-major-version emacs-minor-version)
                        user-emacs-directory)
  "Local storage for package's cache files.")


;;
;;; Core 

;; Package: `borg'
;; Use to manage packages
(eval-and-compile
  (add-to-list 'load-path (expand-file-name "packages/borg" user-emacs-directory))
  (require 'borg)
  (borg-initialize))

;; Package: `use-package'
;; Use to config emacs package
(eval-and-compile
  (require  'use-package)
  ;; (setq use-package-verbose t)              ; TODO: not clear
  ;; (setq use-package-enable-imenu-support t) ; TODO: not clear
  ;; (setq use-package-expand-minimally t)     ; TODO: not clear
  ;; (setq use-package-compute-statistics t)   ; TODO: not clear
  )


;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; init.el ends here
