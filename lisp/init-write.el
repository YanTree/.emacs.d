;;; -*- lexical-binding: t; -*-

;;----------------------------------------------------------------
;; markdown
(leaf markdown-mode
  :package t
  :leaf-defer t
  :commands (markdown-mode gfm-mode)
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  ;; Use `which-key' instead
  (advice-add #'markdown--command-map-prompt :override #'ignore)

  :config
  (with-eval-after-load 'whitespace-cleanup-mode
    (push 'markdown-mode whitespace-cleanup-mode-ignore-modes)))

;; Table of contents
(leaf markdown-toc
  :package t
  :after markdown-mode
  :bind (:markdown-mode-command-map
              ("r" . markdown-toc-generate-or-refresh-toc)))


(provide 'init-write)
;;; init-write.el ends here
