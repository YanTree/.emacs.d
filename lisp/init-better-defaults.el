;;; -*- lexical-binding: t; -*-

;;----------------------------------------------------------------------------
;; 设置utf-8编码
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))

;; 代码来自 purcell 师傅
(defun yantree/utf8-locale-p (v)
  "Return whether locale string V relates to a UTF-8 locale."
  (and v (string-match-p "UTF-8" v)))

(defun yantree/locale-is-utf8-p ()
  "Return t if the \"locale\" command or environment variables prefer UTF-8."
  (or (yantree/utf8-locale-p (and (executable-find "locale") (shell-command-to-string "locale")))
      (yantree/utf8-locale-p (getenv "LC_ALL"))
      (yantree/utf8-locale-p (getenv "LC_CTYPE"))
      (yantree/utf8-locale-p (getenv "LANG"))))

(when (or window-system (yantree/locale-is-utf8-p))
  (set-language-environment 'utf-8)
  (setq locale-coding-system 'utf-8)
  (set-selection-coding-system (if (eq system-type 'windows-nt) 'utf-16-le 'utf-8))
  (prefer-coding-system 'utf-8))


;;----------------------------------------------------------------------------
;; 用户信息
(setq user-full-name "YanTree"
      user-mail-address "yanshudream@outlook.com")


;;----------------------------------------------------------------------------
;; disabled 一些不必要的工具
(if (< emacs-major-version 27)
    (mapc (lambda (mode) (if (fboundp mode) (funcall mode -1)))
          '(scroll-bar-mode       ;;禁用滚动栏
            tool-bar-mode         ;;禁用工具栏
            menu-bar-mode         ;;禁用菜单栏，按F10可开启
            )))


;;----------------------------------------------------------------------------
;; 更改默认设置, 增强体验
(setq-default
 ;; 让鼠标的滚动更平滑
 ;; Vertical Scroll
 scroll-preserve-screen-position 'always
 scroll-margin 0
 scroll-conservatively 1001
 ;; Horizontal Scroll
 hscroll-step 1
 hscroll-margin 2
 ;; Mouse Scroll
 mouse-wheel-scroll-amount '(1 ((shift) . 5) ((control)))
 mouse-wheel-progressive-speed nil

 make-backup-files nil                   ;; 关闭自动备份功能
 auto-save-default nil                   ;; 关闭自动保存
 ;; 禁止 byte-compile-warnings 显示以下警告
 byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local)
 save-interprogram-paste-before-kill t   ;; 先将kill的保存在kill-ring
 ;; 显示
 delete-trailing-lines nil
 fill-column 80
 sentence-end-double-space nil
 ;; 空格
 indent-tabs-mode nil                    ;; 不使用tab来缩进
 tab-always-indent 'complete             ;; 更加贴心的tab,用于缩进或者补全
 ;; Wrapping
 truncate-lines nil                      ;; 折叠行，如果行字符数超过
 truncate-partial-width-windows nil      ;; windows的宽度
 ;; Compilation
 compilation-always-kill t               ;; kill compilation process before starting another
 compilation-ask-about-save nil          ;; save all buffers on `compile'
 compilation-scroll-output 'first-error  ;; 定位到编译第一次出现 error 的位置

 inhibit-startup-screen t                ;; 关闭start screen
 initial-scratch-message                 ;; 更改 *scratch* buffer的显示内容
 (concat ";; Welcome back Master " "  - Emacs ♥ you!\n\n")
 initial-frame-alist (quote ((fullscreen . maximized))) ;;最大化窗口
 inhibit-compacting-font-caches t        ;; 使用字体缓存，避免卡顿
 select-enable-clipboard t               ;; 合并 system clipboard with Emacs
 mouse-yank-at-point t                   ;; 取消鼠标 click 就 yank
 mouse-avoidance-mode 'animate           ;; 光标靠近鼠标指针时,鼠标指针自动让开
 echo-keystrokes 0.02                    ;; echo 快捷键的时间
 font-lock-maximum-decoration t          ;; 只渲染当前屏幕语法高亮，加快显示速度
 default-major-mode 'text-mode           ;; 缺省的 major-mode 设置为 text-mode
 shift-select-mode nil                   ;; 关闭 shift 的 mark 功能
 column-number-mode 1                    ;; 在 modeline 里显示鼠标当前位置的行列数
 create-lockfiles nil                    ;; 当 buffer 未保存,又再次在另一个 buffer 打开时,别锁住这个 buffer
 frame-title-format '(" BUFFER :  [    " ;; 在标题栏里显示当前 buffer 的路径
                      (:eval (if (buffer-file-name) ;;如果当前 buffer 有路径则显示路径,否则显示 buffer 名字
                                 (abbreviate-file-name (buffer-file-name)) "%b")) "    ]"))
;; 禁止光标闪烁
(blink-cursor-mode 0)
;; 直接替换标记区域的字符串
(delete-selection-mode 1)
;; 显示编辑图片
(auto-image-file-mode)
;;以y,n,作为yes,no的回答
(fset 'yes-or-no-p 'y-or-n-p)


;;----------------------------------------------------------------------------
;; 设置字体
;; (add-to-list 'default-frame-alist
;;              '(font . "SF Mono 10"))


;;----------------------------------------------------------------------------
;; 一些有用的mode 和 禁用的功能
;; Don't disable narrowing commands
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;; Don't disable case-change functions
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(provide 'init-better-defaults)
;;; init-better-defaults.el ends here
