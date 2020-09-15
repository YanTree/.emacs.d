;;; -*- lexical-binding: t; -*-

;;----------------------------------------------------------------
;; magit
(leaf magit
  :ensure t
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch))
  :bind (:magit-mode-map
              ("U" . magit-unstage-all))
  :hook (magit-mode-hook . hl-line-mode)
  :config
  (setq magit-git-debug t)
  (setq-default magit-diff-refine-hunk t)

  ;; Integration with `magit'
  (with-eval-after-load 'magit
    (setq magit-completing-read-function 'ivy-completing-read)))


;;----------------------------------------------------------------
;; fullframe
(leaf fullframe
  :ensure t
  :after magit
  :config
  (fullframe magit-status magit-mode-quit-window))


;;----------------------------------------------------------------
;; git commit (for magit, edit git commit message
(leaf git-commit
  :ensure t
  :hook (git-commit-mode-hook . goto-address-mode))


;;----------------------------------------------------------------
;; diff-hl (show different
(leaf diff-hl
  :hook (magit-post-refresh-hook . diff-hl-magit-post-refresh)
  :bind (:diff-hl-mode-map
              ("<left-fringe> <mouse-1>" . diff-hl-diff-goto-hunk)))


(provide 'init-magit)
;;; init-magit.el ends here
