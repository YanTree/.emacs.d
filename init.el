;;; -*- lexical-binding: t; -*-

(setq debug-on-error t)                        ;; 当加载配置出现错误时，显示详细的出错信息

;; ------------------------------ Start ------------------------------
;; From DOOM-EMACS  优化 GC ☞ 不明白的话就理解成加快 Emacs 启动速度，减少卡顿
(defvar YanTree-gc-cons-threshold 16777216)

(defvar YanTree-file-name-handler-alist file-name-handler-alist)

(defun YanTree|restore-startup-optimizations ()
  (setq file-name-handler-alist YanTree-file-name-handler-alist)
  (run-with-idle-timer
   3 nil
   (lambda ()
     (setq-default gc-cons-threshold YanTree-gc-cons-threshold)

     (defun YanTree|defer-garbage-collection ()
       (setq gc-cons-threshold most-positive-fixnum))

     (defun YanTree|restore-garbage-collection ()
       (run-at-time 1 nil (lambda () (setq gc-cons-threshold YanTree-gc-cons-threshold))))

     (add-hook 'minibuffer-setup-hook #'YanTree|defer-garbage-collection)
     (add-hook 'minibuffer-exit-hook  #'YanTree|restore-garbage-collection)
     (add-hook 'focus-out-hook #'garbage-collect))))


(if (ignore-errors (or after-init-time noninteractive))
    (setq gc-cons-threshold YanTree-gc-cons-threshold)
  (setq gc-cons-threshold most-positive-fixnum)
  (setq file-name-handler-alist nil)
  (add-hook 'after-init-hook #'YanTree|restore-startup-optimizations))
;; ------------------------------ End ------------------------------

;; Load path
;; 最优化: Force "lisp"" and "site-lisp" at the head to reduce the startup time.
(defun update-load-path (&rest _)
  "Update `load-path'."
  (push (expand-file-name "lisp" user-emacs-directory) load-path))
(advice-add #'package-initialize :after #'update-load-path)
(update-load-path)
(require 'init-benchmarking)       ;;测试启动时间

;;-----------------------------------------------------------------------
;; Bootstrap config
;;-----------------------------------------------------------------------
(setq custom-file (expand-file-name "auto-save-list/custom.el" user-emacs-directory))
(require 'init-package)
(require 'init-utils)

;;-----------------------------------------------------------------------
;; 基本插件 (大多是是增强build-in 插件的功能,也有安装来自 melpa 的插件)
;;-----------------------------------------------------------------------
(require 'init-better-defaults)    ;; 更改 emacs 初始设置
(require 'init-better-packages)    ;; 增强 emacs 原装 packages
(require 'init-dired)              ;; 增强 dired 配置
(require 'init-ibuffer)            ;; 增强 ibuffer 配置
(require 'init-windows)            ;; 增强切换 windows 的速度



;;-----------------------------------------------------------------------
;; melpa插件  加载目录为 melpa
;;-----------------------------------------------------------------------
(require 'init-ivy)
(require 'init-company)
(require 'init-simple-packages)
(require 'init-theme)
(require 'init-edit)
(require 'init-magit)
(require 'init-scheme)
(require 'init-org)
(require 'init-web)
(require 'init-markdown)
(require 'init-c)



;;-----------------------------------------------------------------------
;;加载‘customize’
;;-----------------------------------------------------------------------
(when (file-exists-p custom-file)
  (load custom-file))



(provide 'init)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
