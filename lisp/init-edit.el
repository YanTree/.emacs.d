;;; -*- lexical-binding: t; -*-

;;----------------------------------------------------------------
;; beacon 像一个彗星尾巴一样闪烁当前行
(leaf beacon
  :ensure t
  :init
  (setq-default beacon-lighter ""
                beacon-size 30)
  :config
  (beacon-mode 1)
  :diminish)


;;----------------------------------------------------------------
;; Zap and browse-kill-ring 为 M-y 提供更好的交互
(leaf browse-kill-ring
  :ensure t
  :init
  (setq browse-kill-ring-separator "\f")

  ;; Zap *up* to char is a handy pair for zap-to-char
  (autoload 'zap-up-to-char "misc" "Kill up to, but not including ARGth occurrence of CHAR.")
  :bind (("M-Y" . browse-kill-ring)
         ("M-z" . zap-up-to-char)
         (:browse-kill-ring-mode-map
          ("C-g" . browse-kill-ring-quit)
          ("M-n" . browse-kill-ring-forward)
          ("M-p" . browse-kill-ring-previous)))
  :config
  ;; 会将分隔符号 "^L" 替换成一条水平线
  (with-eval-after-load 'page-break-lines
    (push 'browse-kill-ring-mode page-break-lines-modes))
  :diminish)


;;----------------------------------------------------------------
;; expand-region
(leaf expand-region
  :ensure t
  :bind ("C-=" . er/expand-region)
  :diminish)


;;----------------------------------------------------------------
;; hungry-delete
(leaf hungry-delete
  :ensure t
  :config
  (global-hungry-delete-mode 1)
  :diminish)


;;----------------------------------------------------------------
;; move-dup
(leaf move-dup
  :ensure t
  :bind
  (([M-up]     . md-move-lines-up)
   ([M-down]   . md-move-lines-down)
   ([M-S-up]   . md-duplicate-up)
   ([M-S-down] . md-duplicate-down)))


;;----------------------------------------------------------------
;;  multiple-cursors
(leaf multiple-cursors
  :ensure t
  :init
  (setq mc/list-file "~/.emacs.d/auto-save-list/.mc-lists.el")  ;;改变配置文件位置的位置
  :bind (;; From active region to multiple cursors:
         ("C-S-c C-S-c"  . mc/edit-lines)
         ("C-S-c C-S-a"  . mc/edit-beginnings-of-lines)
         ("C-S-c C-S-e"  . mc/edit-ends-of-lines)
         ;; multiple-cursors
         ("C-<"          . mc/mark-previous-like-this)
         ("C->"          . mc/mark-next-like-this)
         ("C-+"          . mc/mark-next-like-this)
         ("C-c C-<"      . mc/mark-all-like-this)
         ("C-r"          . mc/mark-all-dwim))
  :diminish)


;;----------------------------------------------------------------
;; smartparens
(leaf smartparens
  :ensure t
  :hook (prog-mode-hook . smartparens-mode)
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

         ("C-M-t"   . sp-transpose-sexp)

         ("M-("     . sp-wrap-round)
         ("M-<delete>" . sp-unwrap-sexp)))


;;----------------------------------------------------------------
;; symbol-overlay 高亮同一个symbol,并对其编辑(例如:下面的 define-key)
(leaf symbol-overlay
  :ensure t
  :hook ((prog-mode-hook html-mode-hook yaml-mode-hook conf-mode-hook) . symbol-overlay-mode)
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
  (symbol-overlay-default-face . '((t (:inherit 'background-color :underline t))))
  :bind ("M-i" . symbol-overlay-put)
  :diminish)


;;----------------------------------------------------------------
;; volatile-highlights  高亮显示一些操作(例如：C-y) 很有用
(leaf volatile-highlights
  :ensure t
  :config
  (volatile-highlights-mode 1)
  :diminish)


;;----------------------------------------------------------------
;; wgrep and rg
(leaf wgrep
  :ensure t
  :leaf-defer t
  :init
  (setq wgrep-auto-save-buffer t)
  (setq wgrep-change-readonly-file t))

;; `ripgrep'
(when (executable-find "rg")
  (leaf rg
    :ensure t
    :config
    (setq rg-group-result t
          rg-show-columns t)))


;;----------------------------------------------------------------
;; whole-line-or-region (Cut/copy the current line if no region is active)
(leaf whole-line-or-region
  :ensure t
  :hook (after-init-hook . whole-line-or-region-global-mode)
  :bind (("M-;"     . whole-line-or-region-comment-dwim))
  :diminish)


;;----------------------------------------------------------------
;; markdown
(leaf markdown-mode
  :ensure t
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
  :ensure t
  :after markdown-mode
  :bind (:markdown-mode-command-map
         ("r" . markdown-toc-generate-or-refresh-toc)))


(provide 'init-edit)
;;; init-edit.el ends here
