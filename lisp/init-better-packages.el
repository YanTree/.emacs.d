;;; -*- lexical-binding: t; -*-

;;----------------------------------------------------------------
;; autorevert
(leaf autorevert
  :doc "revert buffers when files on disk change"
  :require nil
  :init
  ;;不用显示messay,when revert file
  (setq auto-revert-verbose nil)
  :config
  (global-auto-revert-mode 1)
  :diminish)


;;----------------------------------------------------------------
;; bookmarks 书签
(leaf bookmark
  :require nil
  :init
  ;;; everytime bookmark is changed, automatically save it
  (setq bookmark-save-flag 1)
  (setq-default
   bookmark-default-file (expand-file-name "auto-save-list/bookmarks" user-emacs-directory)))


;;----------------------------------------------------------------
;; display-line-numbers 显示行号
(if (fboundp 'display-line-numbers-mode)   ;;;判断是否已经显示行号
    (leaf display-line-numbers
      :require nil
      :init
      (setq-default display-line-numbers-width 3)
      :hook prog-mode-hook))


;;----------------------------------------------------------------
;; ediff ()
(leaf ediff
  :doc "A comprehensive visual interface to diff & patch"
  :require nil
  :hook(ediff-quit-hook . winner-undo)
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-merge-split-window-function 'split-window-horizontally))


;;----------------------------------------------------------------
;; hippie-expand
(leaf hippie-expand
  :require nil
  :init
  (setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                           try-expand-dabbrev-all-buffers
                                           try-expand-dabbrev-from-kill
                                           try-complete-file-name
                                           try-complete-file-name-partially
                                           try-complete-lisp-symbol-partially
                                           try-complete-lisp-symbol))
  :bind ("M-/" . hippie-expand))


;;----------------------------------------------------------------
;; hl-line 高亮当前行
(leaf hl-line
  :require nil
  :config
  (global-hl-line-mode 1))


;;----------------------------------------------------------------
;; paren 高亮括号
(leaf parens
  :require nil
  :config
  (show-paren-mode 1))


;;----------------------------------------------------------------
;; prettify-symbols(像这样 lambda 美化成一个 "入" 子符号 )
(when (fboundp 'global-prettify-symbols-mode)
  (leaf prettify-symbols
    :require nil
    :config
    (global-prettify-symbols-mode 1)))


;;----------------------------------------------------------------
;; recentf 最近打开的文件
(leaf recentf
  :require nil
  :init
  (setq recentf-max-saved-items 500
        recentf-exclude '("/tmp/" "/ssh:"))
  (setq-default
   recentf-save-file (expand-file-name "auto-save-list/recentf" user-emacs-directory))
  :bind ("C-x C-r" . recentf-open-files)
  :config
  (recentf-mode 1))


;;----------------------------------------------------------------
;; savehist
(leaf savehist
  :doc "save minibuffer history"
  :require nil
  :init (setq enable-recursive-minibuffers t;;Allow commands in minibuffers
              history-length 1000
              savehist-file (expand-file-name "auto-save-list/savehist" user-emacs-directory))
  :config
  (savehist-mode 1))


;;----------------------------------------------------------------
;; saveplace
(leaf saveplace
  :doc "save mouse History"
  :require nil
  :init
  (setq save-place-file
        (expand-file-name "auto-save-list/saveplace" user-emacs-directory))
  :config
  (save-place-mode 1))


;;----------------------------------------------------------------
;; server ( Start server )
(leaf server
  :hook (after-init . (lambda () (require 'server)
                        (unless (server-running-p)
                          (server-start)))))


;;----------------------------------------------------------------
;; subword(这样辨别一个词 EmacsFrameClass    =>  "Emacs", "Frame" and "Class")
(leaf subword
  :require nil
  :hook ((prog-mode-hook minibuffer-setup-hook) . subword-mode)
  :diminish)


;;----------------------------------------------------------------
;; 时间显示
(leaf time
  :require nil
  :init
  (setq display-time-24hr-format t)
  (setq display-time-format " %H:%M - %b %d")                    ;;格式
  (setq display-time-load-average-threshold nil)                 ;;不显示 load average
  (setq system-time-locale "C")                                  ;;英文显示日期
  :config
  (display-time-mode 1))


;;----------------------------------------------------------------
;; whitespace 高亮显示 space
(leaf whitespace
  :require nil
  :init
  (setq-default show-trailing-whitespace nil)

  ;; Whitespace
  (defun yantree/show-trailing-whitespace ()
    "Enable display of trailing whitespace in this buffer."
    (setq-local show-trailing-whitespace t))
  :hook ((prog-mode-hook text-mode conf-mode-hook) . yantree/show-trailing-whitespace))

;; 用于清除多余的 space
(leaf whitespace-cleanup-mode
  :ensure t
  :config
  (global-whitespace-cleanup-mode 1)
  :diminish)


;;----------------------------------------------------------------
;; uniquify ( file name )
(leaf uniquify
  :init
  ;;何种方式显示文件路径
  (setq uniquify-buffer-name-style 'reverse)
  (setq uniquify-separator " • ")
  ;;kill buffer之后,可以使用它的名字(释放他的名字)
  (setq uniquify-after-kill-buffer-p t)
  (setq uniquify-ignore-buffers-re "^\\*")
  :diminish)

(provide 'init-better-packages)
;;; init-better-packages.el ends here
