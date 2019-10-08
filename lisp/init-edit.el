;;; -*- lexical-binding: t; -*-

;;----------------------------------------------------------------------------
;; anzu (displays current match and total matches information in the mode-line
(use-package anzu
  :ensure t
  :diminish
  :init
  (setq anzu-mode-lighter "")
  :bind (([remap query-replace]        . anzu-query-replace)
         ([remap query-replace-regexp] . anzu-query-replace-regexp))
  :config
  (global-anzu-mode 1))


;;----------------------------------------------------------------------------
;; avy (jumping to visible text using a char-based decision tree
(use-package avy
  :ensure t
  :bind* ("C-'" . avy-goto-char-timer))


;;----------------------------------------------------------------------------
;; beacon 像一个彗星尾巴一样闪烁当前行
(use-package beacon
  :ensure t
  :diminish
  :init
  (setq-default beacon-lighter ""
                beacon-size 20
                ;;beacon-color "#51afef" ;;doom-one-theme
                ;;beacon-color "#839496"   ;;solarized-dark
                )
  :config
  (beacon-mode 1))


;;----------------------------------------------------------------------------
;; Zap and browse-kill-ring 为 M-y 提供更好的交互
(use-package browse-kill-ring
  :ensure t
  :diminish
  :init
  (setq browse-kill-ring-separator "\f")

  ;; Zap *up* to char is a handy pair for zap-to-char
  (autoload 'zap-up-to-char "misc" "Kill up to, but not including ARGth occurrence of CHAR.")
  :bind (("M-Y" . browse-kill-ring)
         ("M-z" . zap-up-to-char)
         :map browse-kill-ring-mode-map
         ("C-g" . browse-kill-ring-quit)
         ("M-n" . browse-kill-ring-forward)
         ("M-p" . browse-kill-ring-previous)))


;;----------------------------------------------------------------------------
;; evil-nerd-commenter
(use-package evil-nerd-commenter
  :ensure t
  :diminish
  :bind ("M-;" . evilnc-comment-or-uncomment-lines))


;;----------------------------------------------------------------------------
;; expand-region
(use-package expand-region
  :ensure t
  :diminish
  :bind ("C-=" . er/expand-region))


;;----------------------------------------------------------------------------
;; emojify
;; (use-package emojify
;;   :ensure t
;;   :defer t
;;   :config
;;   (global-emojify-mode))


;;----------------------------------------------------------------------------
;; goto-line-preview
(use-package goto-line-preview
  :ensure t
  :bind ([remap goto-line] . goto-line-preview)
  :config
  (when (fboundp 'display-line-numbers-mode)
    (defun sanityinc/with-display-line-numbers (f &rest args)
      (let ((display-line-numbers t))
        (apply f args)))
    (advice-add 'goto-line-preview :around #'sanityinc/with-display-line-numbers)))


;;----------------------------------------------------------------------------
;; hungry-delete
(use-package hungry-delete
  :ensure t
  :diminish hungry-delete-mode
  :config
  (global-hungry-delete-mode 1))


;;----------------------------------------------------------------------------
;; move-dup
(use-package move-dup
  :ensure t
  :bind
  (([M-up]     . md-move-lines-up)
   ([M-down]   . md-move-lines-down)
   ([M-S-up]   . md-duplicate-up)
   ([M-S-down] . md-duplicate-down)))


;;----------------------------------------------------------------------------
;;  multiple-cursors
(use-package multiple-cursors
  :ensure t
  :diminish
  :init
  (setq mc/list-file "~/.emacs.d/auto-save-list/.mc-lists.el")  ;;改变配置文件位置的位置
  :bind (;; From active region to multiple cursors:
         ("C-S-c C-S-c"  . mc/edit-lines)
         ("C-S-c C-a"    . mc/edit-beginnings-of-lines)
         ("C-S-c C-e"    . mc/edit-ends-of-lines)
         ;; multiple-cursors
         ("C-<"          . mc/mark-previous-like-this)
         ("C->"          . mc/mark-next-like-this)
         ("C-+"          . mc/mark-next-like-this)
         ("C-c C-<"      . mc/mark-all-like-this)
         ("C-r"          . mc/mark-all-dwim)))


;;----------------------------------------------------------------------------
;; smartparens
(use-package smartparens
  :ensure t
  :hook (prog-mode . smartparens-mode)
  :config
  (require 'smartparens-config)
  (require 'smartparens-html)
  :bind (("C-M-f"   . sp-forward-sexp)
         ("C-M-b"   . sp-backward-sexp)

         ("C-M-d"   . sp-down-sexp)
         ("C-M-u"   . sp-backward-up-sexp)

         ("C-M-n"   . sp-up-sexp)
         ("C-M-p"   . sp-backward-down-sexp)

         ("C-M-k"   . sp-kill-sexp)
         ("C-M-w"   . sp-copy-sexp)
         ("C-M-SPC" . sp-mark-sexp)

         ;; ("C-M-t"   . sp-transpose-sexp)

         ("M-("     . sp-wrap-round)
         ("M-<delete>" . sp-unwrap-sexp)))


;;----------------------------------------------------------------------------
;; symbol-overlay 高亮同一个symbol,并对其编辑(例如:下面的 define-key)
(use-package symbol-overlay
  :ensure t
  :diminish symbol-overlay-mode
  :hook ((prog-mode html-mode yaml-mode conf-mode) . symbol-overlay-mode)
  :init
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<up>") 'symbol-overlay-jump-prev)
    (define-key map (kbd "M-p") 'symbol-overlay-jump-prev)
    (define-key map (kbd "<down>") 'symbol-overlay-jump-next)
    (define-key map (kbd "M-n") 'symbol-overlay-jump-next)
    (define-key map (kbd "<") 'symbol-overlay-jump-first)
    (define-key map (kbd ">") 'symbol-overlay-jump-last)
    (define-key map (kbd "r") 'symbol-overlay-rename)
    (define-key map (kbd "s") 'symbol-overlay-save-symbol)
    (setq symbol-overlay-map map))
  (setq symbol-overlay-idle-time 0.01)
  :custom-face
  (symbol-overlay-default-face ((t (:inherit 'background-color :underline t))))
  :bind ("M-i" . symbol-overlay-put))


;;----------------------------------------------------------------------------
;; volatile-highlights  高亮显示一些操作(例如：C-y) 很有用
(use-package volatile-highlights
  :ensure t
  :diminish
  :config
  (volatile-highlights-mode 1))


;;----------------------------------------------------------------------------
;; wgrep ( 直接编辑 ag 或 rg 的搜索结果 )
;; ag 的安装
(when (and (executable-find "ag")
           (use-package ag
             :ensure t
             :defer t
             :init
             (setq-default ag-highlight-search t)))
  (use-package wgrep
    :ensure t
    :init
    (setq wgrep-auto-save-buffer t
          wgrep-change-readonly-file t)
    :bind ("M-?"    . ag-project)))

;; rg 的安装
;; (when (and (executable-find "rg")
;;            (use-package rg
;;              :ensure t
;;              :config
;;              (rg-enable-default-bindings)))
;;   (use-package deadgrep
;;     :ensure t
;;     :bind ("M-?"    . rg-project)))


;;----------------------------------------------------------------------------
;; yasnippet
(use-package yasnippet
  :ensure t
  :commands (yas-reload-all)
  :diminish yas-minor-mode
  :hook ((org-mode prog-mode snippet-mode) . yas-minor-mode)
  :init
  (setq yas-snippet-dirs '("~/.emacs.d/site-lisp/snippets"))
  :config
  (yas-reload-all))

(provide 'init-edit)
;;; init-edit.el ends here
