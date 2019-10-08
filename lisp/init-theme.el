;;; -*- lexical-binding: t; -*-

;;----------------------------------------------------------------------------
;; leuven-theme
;; (use-package leuven-theme
;;   :ensure t
;;   :init
;;   (setq org-fontify-whole-heading-line t)
;;   :config
;;   (load-theme 'leuven t))


;;----------------------------------------------------------------------------
;; doom-theme
(use-package doom-themes
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
       load-theme 'doom-one t
       ;; load-theme 'doom-city-lights t
       ;; load-theme 'doom-dracula t
       ;; load-theme 'doom-molokai t
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


;;----------------------------------------------------------------------------
;; 让当前的buffer突出显出
(use-package dimmer
  :ensure t
  :hook (after-init . dimmer-mode)
  :init
  ;; TODO: file upstream as a PR
  (setq-default dimmer-fraction 0.15)
  (with-eval-after-load 'dimmer
    (advice-add 'frame-set-background-mode :after (lambda (&rest args) (dimmer-process-all)))))


;;----------------------------------------------------------------------------
;; doom-modeline
(use-package doom-modeline
  :ensure t
  :init
  (setq doom-modeline-buffer-file-name-style 'buffer-name)
  :config
  (doom-modeline-mode))

(provide 'init-theme)
;;; init-theme.el ends here
