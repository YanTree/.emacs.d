;;; init.el --- user-init-file                    -*- lexical-binding: t -*-
;;; Early birds


;; Increase how much is read from processes in a single chunk (default is 4kb).
;; This is further increased elsewhere, where needed (like our LSP module).
(setq read-process-output-max (* 128 1024))  ; 128kb

;; Garbage collection is a big contributor to startup times. This fends it off,
;; but will be reset later to normal by gcmh.
(setq gc-cons-threshold (* 512 1024 1024))  ; 512mb


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
  (setq ring-bell-function #'ignore)   ; Disable ring bell, it's annoying
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

;; Package: `gcmh'
;; More smarter garbage collection
(use-package gcmh
  :hook (after-init . gcmh-mode)
  :init (setq gcmh-high-cons-threshold (* 128 1024 1024))) ; 128mb


;;
;;; Better default

(progn
  (setq system-time-locale "C"      ; If show current time at modeline, use EN instead of CN
        display-time-24hr-format t) ; 00:00~23:00 instead of 00:00~12:00

  ; Load custom.el file
  (setq custom-file (expand-file-name "custom.el" maybe-data-dir))
  (when (file-exists-p custom-file)
    (load custom-file))
  )


;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; init.el ends here
