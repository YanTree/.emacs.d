;;; -*- lexical-binding: t; -*-

;;----------------------------------------------------------------------------
;; autorevert
(use-package autorevert
  :ensure nil
  :diminish
  :init
  ;;不用显示messay,when revert file
  (setq auto-revert-verbose nil)
  :config
  (global-auto-revert-mode 1))


;;----------------------------------------------------------------------------
;; bookmarks 书签
(use-package bookmark
  :ensure nil
  :defer t
  :init
  ;;; everytime bookmark is changed, automatically save it
  (setq bookmark-save-flag 1)
  (setq-default
   bookmark-default-file (expand-file-name "auto-save-list/bookmarks" user-emacs-directory)))


;;----------------------------------------------------------------------------
;; display-line-numbers 显示行号
(if (fboundp 'display-line-numbers-mode)   ;;;判断是否已经显示行号
    (use-package display-line-numbers
      :ensure nil
      :init
      (setq-default display-line-numbers-width 3)
      :hook (prog-mode . display-line-numbers-mode)))


;;----------------------------------------------------------------------------
;; flyspell (需要安装 aspell 和 aspell-en)
(use-package flyspell
  :ensure nil
  :diminish
  :if (executable-find "aspell")
  :hook (((text-mode org-mdoe markdown-mode) . flyspell-mode)
         ;;(prog-mode . flyspell-prog-mode)
         (flyspell-mode . (lambda ()
                            (dolist (key '("C-;" "C-," "C-."))
                              (unbind-key key flyspell-mode-map)))))
  :init
  (setq ispell-program-name "aspell"
        ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together")))


;;----------------------------------------------------------------------------
;; grep-mode 文本查找工具
(use-package grep-mode
  :no-require t
  :init
  (setq-default grep-highlight-matches t
                grep-scroll-output t)

  (with-eval-after-load 'grep
    (dolist (key (list (kbd "C-c C-q") (kbd "w")))
      (define-key grep-mode-map key 'wgrep-change-to-wgrep-mode))))


;;----------------------------------------------------------------------------
;; hippie-expand
(use-package hippie-expand
  :ensure nil
  :defer t
  :init
  (setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                           try-expand-dabbrev-all-buffers
                                           try-expand-dabbrev-from-kill
                                           try-complete-file-name
                                           try-complete-file-name-partially
                                           try-complete-lisp-symbol-partially
                                           try-complete-lisp-symbol))
  :bind ("M-/" . hippie-expand))


;;----------------------------------------------------------------------------
;; hl-line 高亮当前行
(use-package hl-line
  :ensure nil
  :config
  (global-hl-line-mode 1))


;;----------------------------------------------------------------------------
;; ielm REPL for emacs-lisp
(use-package ielm
  :ensure nil
  :hook (ielm-mode . turn-on-smartparens-strict-mode))


;;----------------------------------------------------------------------------
;; display relative line number in emacs.可能你会喜欢这个package
;; (use-package linum-relative
;;   :ensure t
;;   :init (setq linum-relative-current-symbol ">")
;;   :hook (prog-mode . linum-relative-mode))


;;----------------------------------------------------------------------------
;; paren 高亮括号
(use-package parens
  :no-require t
  :config
  (show-paren-mode 1))


;;----------------------------------------------------------------------------
;; prettify-symbols(像这样 lambda 美化成一个 "入" 子符号 )
(when (fboundp 'global-prettify-symbols-mode)
  (use-package prettify-symbols
    :ensure nil
    :defer t
    :config
    (global-prettify-symbols-mode 1)))


;;----------------------------------------------------------------------------
;; recentf 最近打开的文件
(use-package recentf
  :ensure nil
  :commands (recentf-add-file
             recentf-apply-filename-handlers)
  :init
  (setq recentf-max-saved-items 500
        recentf-exclude '("/tmp/" "/ssh:"))
  (setq-default
   recentf-save-file (expand-file-name "auto-save-list/recentf" user-emacs-directory))
  :preface
  (defun recentf-add-dired-directory ()
    (if (and dired-directory
             (file-directory-p dired-directory)
             (not (string= "/" dired-directory)))
        (let ((last-idx (1- (length dired-directory))))
          (recentf-add-file
           (if (= ?/ (aref dired-directory last-idx))
               (substring dired-directory 0 last-idx)
             dired-directory)))))
  :hook (dired-mode . recentf-add-dired-directory)
  :bind ("C-x C-r" . recentf-open-files)
  :config
  (recentf-mode 1))


;;----------------------------------------------------------------------------
;; savehist ( minibuffer history )
(use-package savehist
  :ensure nil
  :init (setq enable-recursive-minibuffers t;;Allow commands in minibuffers
              history-length 1000
              savehist-file (expand-file-name "auto-save-list/savehist" user-emacs-directory))
  :config
  (savehist-mode 1))


;;----------------------------------------------------------------------------
;; saveplace ( mouse History )
(use-package saveplace
  :ensure nil
  :init
  (setq save-place-file
        (expand-file-name "auto-save-list/saveplace" user-emacs-directory))
  :config
  (save-place-mode 1))


;;----------------------------------------------------------------------------
;; server ( Start server )
(use-package server
  :ensure nil
  :functions server-running-p
  :hook (after-init . (lambda () (require 'server)
                        (unless (server-running-p)
                          (server-start)))))


;;----------------------------------------------------------------------------
;; subword(这样辨别一个词 EmacsFrameClass    =>  "Emacs", "Frame" and "Class")
(use-package subword
  :ensure nil
  :diminish
  :hook ((prog-mode . subword-mode)
         (minibuffer-setup . subword-mode)))


;;----------------------------------------------------------------------------
;; 时间显示
;; (use-package time
;;   :no-require
;;   :init
;;   (setq display-time-24hr-format t)
;;   (setq display-time-format " %H:%M - %d %B")                    ;;格式
;;   (setq system-time-locale "C")                                  ;;英文显示日期
;;   :config
;;   (display-time-mode 1))


;;----------------------------------------------------------------------------
;; whitespace 高亮显示 space
(use-package whitespace
  :ensure nil
  :init
  (setq-default show-trailing-whitespace nil)

  ;; Whitespace
  (defun yantree/show-trailing-whitespace ()
    "Enable display of trailing whitespace in this buffer."
    (setq-local show-trailing-whitespace t))
  :hook ((prog-mode text-mode conf-mode) . yantree/show-trailing-whitespace))

;; 用于清除多余的 space
(use-package whitespace-cleanup-mode
  :ensure t
  :diminish whitespace-cleanup-mode
  :config
  (global-whitespace-cleanup-mode 1))


;;----------------------------------------------------------------------------
;; uniquify ( file name )
(use-package uniquify
  :ensure nil
  :diminish
  :init
  ;;何种方式显示文件路径
  (setq uniquify-buffer-name-style 'reverse)
  (setq uniquify-separator " • ")
  ;;kill buffer之后,可以使用它的名字(释放他的名字)
  (setq uniquify-after-kill-buffer-p t)
  (setq uniquify-ignore-buffers-re "^\\*"))

(provide 'init-better-packages)
;;; init-better-packages.el ends here
