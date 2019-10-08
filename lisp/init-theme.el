;;; -*- lexical-binding: t; -*-


;;-------------------------------------------------------------------------
;; solarized-theme
;;-------------------------------------------------------------------------
;;(use-package solarized-theme
;;:ensure t
;;:config
;;(load-theme 'solarized-dark t)
;;(load-theme 'solarized-light t)

;;:custom-face
;;solarized-dark for modeline
;;(mode-line ((t (:background "#073642" :foreground "#839496" :box (:line-width 1 :color "#073642" :style unspecified) :overline nil :underline nil))))
;;(mode-line-inactive ((t (:background "#002b36" :foreground "#586e75" :box (:line-width 1 :color "#002b36" :style unspecified) :overline nil :underline nil)))))


;;-------------------------------------------------------------------------
;; leuven-theme
;;-------------------------------------------------------------------------
;; (use-package leuven-theme
;;   :ensure t
;;   :init
;;   (setq org-fontify-whole-heading-line t)
;;   :config
;;   (load-theme 'leuven t))


;;-------------------------------------------------------------------------
;; doom-theme
;;-------------------------------------------------------------------------
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
      (
       load-theme 'doom-one t
       ;; load-theme 'doom-dracula t
       ;; load-theme 'doom-molokai t
       ;; load-theme 'doom-nord t
       ;; load-theme 'doom-nord-light t
       ;; load-theme 'doom-opera t
       ;; load-theme 'doom-opera-light t
       ;; load-theme 'doom-solarized-light t
       )
    (load-theme 'manoj-dark t))
  )



;;--------------------------------------------------------
;; 让当前的buffer突出显出
(use-package dimmer
  :ensure t
  :hook (after-init . dimmer-mode)
  :init
  ;; TODO: file upstream as a PR
  (setq-default dimmer-fraction 0.15)
  (with-eval-after-load 'dimmer
    (advice-add 'frame-set-background-mode :after (lambda (&rest args) (dimmer-process-all))))
  )



;;--------------------------------------------------------
;; doom-modeline
(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :init
  (setq doom-modeline-buffer-file-name-style 'buffer-name)
  :config
  (doom-modeline-mode))



;;--------------------------------------------------------
;;       modeline
;;以 modeline 闪烁代替 警报声音
;; (setq ring-bell-function 'ignore)
;; (setq ring-bell-function
;;       (lambda ()
;;         (let ((orig-fg (face-background 'mode-line)))
;;           (set-face-background 'mode-line "#bf5150"
;;                                )
;;           (run-with-idle-timer 0.15 nil
;;                                (lambda (fg) (set-face-background 'mode-line fg))
;;                                orig-fg))))



;; (setq-default mode-line-format
;;            '("%e "
;;              mode-line-front-space
;;              mode-line-mule-info
;;              mode-line-client
;;              mode-line-modified
;;              mode-line-remote
;;              mode-line-frame-identification
;;              mode-line-buffer-identification
;;              " "
;;                 mode-line-position
;;              "(" ;;只显示主模式
;;              (:eval (propertize "%m" 'help-echo buffer-file-coding-system))
;;              ") " ;;其他信息
;;              (vc-mode vc-mode)
;;              " "
;;                 (:eval (propertize (format-time-string "%H:%M "))) ;;显示时间

;;              (:eval (propertize (format-time-string "- %a %d %b"))) ;;显示日期，并采用注释的颜色
;;              "  "
;;              mode-line-misc-info
;;              mode-line-end-spaces
;;              ))



(provide 'init-theme)
;;; init-theme.el ends here
