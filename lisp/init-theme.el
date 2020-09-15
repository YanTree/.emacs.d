;;; -*- lexical-binding: t; -*-

;;----------------------------------------------------------------
;; doom-theme
(leaf doom-themes
  :ensure t
  :init
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  :config
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)
  ;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each theme
  ;; may have their own settings.


  ;; 图形界面使用 doom-themes,终端下使用 manoj-dark theme
  (if (display-graphic-p)
      (;; 深色主题
       ;; load-theme 'doom-one t
       ;; load-theme 'doom-city-lights t
       ;; load-theme 'doom-dracula t
       ;; load-theme 'doom-monokai-classic t
       load-theme 'doom-monokai-pro t
       ;; load-theme 'doom-nord t
       ;; load-theme 'doom-opera t
       ;; load-theme 'doom-tomorrow-night t
       ;; 浅色主题
       ;; load-theme 'doom-solarized-light t
       ;; load-theme 'doom-nord-light t
       ;; load-theme 'doom-one-light t
       ;; load-theme 'doom-opera-light t
       )
    (load-theme 'manoj-dark t))
  )


;;----------------------------------------------------------------
;; solaire-mode (Make certain buffers grossly incandescent)
(leaf solaire-mode
  :ensure t
  :leaf-defer t
  ;; :hook (((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
  ;;        (minibuffer-setup . solaire-mode-in-minibuffer)
  ;;        (after-load-theme . solaire-mode-swap-bg))
  :config
  (setq solaire-mode-remap-fringe nil)
  (solaire-global-mode 1)
  (solaire-mode-swap-bg))


;;----------------------------------------------------------------
;; doom-modeline
(leaf doom-modeline
  :ensure t
  :init
  (setq doom-modeline-buffer-file-name-style 'buffer-name)
  :config
  (doom-modeline-mode))

(provide 'init-theme)
;;; init-theme.el ends here
