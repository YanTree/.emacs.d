;;; -*- lexical-binding: t; -*-

;;----------------------------------------------------------------
;; js2-mode
(use-package js2-mode
  :ensure t
  :defer t
  :init
  (setq-default js2-mode-show-parse-errors nil
                js2-mode-show-strict-warnings nil
                )
  (autoload 'flycheck-get-checker-for-buffer "flycheck")
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

  (defun yantree/enable-js2-checks-if-flycheck-inactive ()
    (unless (flycheck-get-checker-for-buffer)
      (setq-local js2-mode-show-parse-errors t)
      (setq-local js2-mode-show-strict-warnings t)))
  :hook((js2-mode . (lambda () (setq mode-name "JS2")))
        (js2-mode . yantree/enable-js2-checks-if-flycheck-inactive)))


;;----------------------------------------------------------------
;; HTML
(use-package mhtml-mode
  :ensure nil
  :bind(:map html-mode-map
             ("<return>"                 . newline-and-indent))
  :preface
  ;; 在删除一个 tag 之后, 缩进
  (defadvice sgml-delete-tag (after reindent activate)
    (indent-region (point-min) (point-max))))


;;----------------------------------------------------------------
;; 将 CSS 嵌入 html
(use-package mmm-mode
  :ensure t
  :defer t
  :config
  (with-eval-after-load 'mmm-vars
    (mmm-add-group
     'html-css
     '((css-cdata
        :submode css-mode
        :face mmm-code-submode-face
        :front "<style[^>]*>[ \t\n]*\\(//\\)?<!\\[CDATA\\[[ \t]*\n?"
        :back "[ \t]*\\(//\\)?]]>[ \t\n]*</style>"
        :insert ((?c css-tag nil @ "<style type=\"text/css\">"
                     @ "\n" _ "\n" @ "</style>" @)))
       (css
        :submode css-mode
        :face mmm-code-submode-face
        :front "<style[^>]*>[ \t]*\n?"
        :back "[ \t]*</style>"
        :insert ((?c css-tag nil @ "<style type=\"text/css\">"
                     @ "\n" _ "\n" @ "</style>" @)))
       (css-inline
        :submode css-mode
        :face mmm-code-submode-face
        :front "style=\""
        :back "\"")))
    (dolist (mode (list 'html-mode 'nxml-mode))
      (mmm-add-mode-ext-class mode "\\.r?html\\(\\.erb\\)?\\'" 'html-css))))


;;----------------------------------------------------------------
;;; SCSS
;; (unless (fboundp 'scss-mode)
;;   (use-package scss-mode
;;     :ensure t
;;     :defer t
;;     :init
;;     (setq-default scss-compile-at-save nil)))


;;----------------------------------------------------------------
;; LESS
;; (unless (fboundp 'less-css-mode)
;;   (use-package less-css-mode
;;     :ensure t
;;     :defer t
;;     :config
;;     (use-package skewer-less
;;       :ensure t
;;       :hook (less-css-mode . skewer-less-mode))))


;;----------------------------------------------------------------
;; skewer
(use-package skewer-mode
  :ensure t
  :defer t
  :diminish
  :hook (((js-mode js2-mode). skewer-mode)
         (css-mode . skewer-css-mode)
         ;;(web-mode . skewer-html-mode)
         (html-mode . skewer-html-mode))
  :init
  (defun skewer-start()
    "This's function manual start a skewer with current html file"
    (interactive)
    (httpd-start)
    (message "Now, you can Live Web development in HTML mode")
    (browse-current-file))

  (defun skewer-demo()
    (interactive)
    (let ((httpd-port 8024))
      (run-skewer)
      (skewer-repl))))


;;----------------------------------------------------------------
;; 使用 eldoc 语法提示
(use-package css-eldoc
  :ensure t
  :defer t
  :config
  (autoload 'turn-on-css-eldoc "css-eldoc")
  (add-hook 'css-mode-hook 'turn-on-css-eldoc))


;;----------------------------------------------------------------
;; web-mode
;; (use-package web-mode
;;   :ensure t
;;   :defer t
;;   :mode "\\.\\(phtml\\|php|[gj]sp\\|as[cp]x\\|erb\\|djhtml\\|html?\\|hbs\\|ejs\\|jade\\|swig\\|tm?pl\\|vue\\)$")


;;----------------------------------------------------------------
;; emmet-mode 强劲的 html, css 模板插件
(use-package emmet-mode
  :ensure t
  :hook ((html-mode . emmet-mode)
         (sgml-mode . emmet-mode)
         (css-mode  . emmet-mode)))

(provide 'init-web)
;;; init-web.el ends here
