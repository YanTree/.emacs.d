;;; -*- lexical-binding: t; -*-


;;--------------------------------------------------
;;        这份文件是用于配置scheme编程环境的
;;--------------------------------------------------



;;--------------------------------------------------
;;        chez-scheme
;;--------------------------------------------------
(setq scheme-program-name "chez-scheme")
(use-package geiser
  :ensure t
  :defer t
  :init
  (setq geiser-chez-binary "chez-scheme")
  (setq geiser-active-implementations '(chez)))


;; (defalias 'scheme-start 'run-scheme)
;;(setq scheme-program-name "racket")
;;(setq geiser-chez-binary "chez-scheme")
;;(setq geiser-active-implementations '(chez))


(provide 'init-scheme)
;;; init-scheme.el ends here
