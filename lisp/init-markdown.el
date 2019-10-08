;;; -*- lexical-binding: t; -*-

;;----------------------------------------------------------------------------
;; markdown
(use-package markdown-mode
  :ensure t
  :defer t
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config
  (with-eval-after-load 'whitespace-cleanup-mode
    (push 'markdown-mode whitespace-cleanup-mode-ignore-modes)))

(provide 'init-markdown)
;;; init-markdown.el ends here
