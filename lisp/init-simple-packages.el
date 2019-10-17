;;; -*- lexical-binding: t; -*-
;; 配置不超过五十行代码的插件 or......我想把它放这儿

;;----------------------------------------------------------------
;; aggresive-indent ( 时时保持缩进 )
(use-package aggressive-indent
  :ensure t
  :diminish
  :hook
  ;; FIXME: Disable in big files due to the performance issues
  (find-file .  (lambda ()
                  (if (> (buffer-size) (* 3000 80))
                      (aggressive-indent-mode -1))))
  :config
  (global-aggressive-indent-mode 1)
  ;; Disable in some modes
  (dolist (mode '(web-mode html-mode css-mode go-mode))
    (push mode aggressive-indent-excluded-modes))

  ;; Disable in some commands
  (add-to-list 'aggressive-indent-protected-commands #'delete-trailing-whitespace t))


;;----------------------------------------------------------------
;; diff-hl 高亮显示未提交的change
(use-package diff-hl
  :ensure t
  :defines (diff-hl-margin-symbols-alist desktop-minor-mode-table)
  :functions  my-diff-hl-fringe-bmp-function
  :custom-face (diff-hl-change ((t (:foreground ,(face-background 'highlight)))))
  :bind (:map diff-hl-command-map
              ("SPC" . diff-hl-mark-hunk))
  :hook ((dired-mode . diff-hl-dired-mode)
         (after-init . global-diff-hl-mode))
  :config
  ;; Highlight on-the-fly
  (diff-hl-flydiff-mode 1)

  ;; Set fringe style
  (setq-default fringes-outside-margins t)

  (defun my-diff-hl-fringe-bmp-function (_type _pos)
    "Fringe bitmap function for use as `diff-hl-fringe-bmp-function'."
    (define-fringe-bitmap 'my-diff-hl-bmp
      (vector #b11111100)
      1 8
      '(center t)))
  (setq diff-hl-fringe-bmp-function #'my-diff-hl-fringe-bmp-function)

  (unless (display-graphic-p)
    (setq diff-hl-margin-symbols-alist
          '((insert . " ") (delete . " ") (change . " ")
            (unknown . " ") (ignored . " ")))
    ;; Fall back to the display margin since the fringe is unavailable in tty
    (diff-hl-margin-mode 1)
    ;; Avoid restoring `diff-hl-margin-mode'
    (with-eval-after-load 'desktop
      (add-to-list 'desktop-minor-mode-table
                   '(diff-hl-margin-mode nil)))))


;;----------------------------------------------------------------
;; exec-path-from-shell(for macos
(when (or (and (display-graphic-p) (eq system-type 'gnu/linux)) ;; 判断是否为 GNU/Linux 系统
          (and (display-graphic-p) (eq system-type 'darwin)))   ;; 判断是否为 GNU/Linux 系统
  (use-package exec-path-from-shell
    :ensure t
    :defer t
    :init
    (setq exec-path-from-shell-check-startup-files nil
          exec-path-from-shell-variables '("PATH" "MANPATH" "PYTHONPATH" "GOPATH")
          exec-path-from-shell-arguments '("-l")))
  (exec-path-from-shell-initialize))


;;----------------------------------------------------------------
;; flycheck (语法检查
(use-package flycheck
  :ensure t
  :defer t
  :diminish flycheck-mode
  :config
  (defalias 'show-error-at-point-soon
    'flycheck-show-error-at-point)

  (setq flycheck-emacs-lisp-load-path 'inherit)
  (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))

  (defun magnars/adjust-flycheck-automatic-syntax-eagerness ()
    "Adjust how often we check for errors based on if there are any.
  This lets us fix any errors as quickly as possible, but in a
  clean buffer we're an order of magnitude laxer about checking."
    (setq flycheck-idle-change-delay
          (if flycheck-current-errors 0.3 3.0)))

  ;; Each buffer gets its own idle-change-delay because of the
  ;; buffer-sensitive adjustment above.
  (make-variable-buffer-local 'flycheck-idle-change-delay)

  (add-hook 'flycheck-after-syntax-check-hook
            'magnars/adjust-flycheck-automatic-syntax-eagerness)

  ;; Remove newline checks, since they would trigger an immediate check
  ;; when we want the idle-change-delay to be in effect while editing.
  (setq-default flycheck-check-syntax-automatically '(save
                                                      idle-change
                                                      mode-enabled))

;;; Display Flycheck errors in GUI tooltips
  (if (display-graphic-p)
      (if (>= emacs-major-version 26)
          (use-package flycheck-posframe
            :ensure t
            :after (flycheck)
            :hook (flycheck-mode . flycheck-posframe-mode)
            :config
            (add-to-list 'flycheck-posframe-inhibit-functions
                         #'(lambda () (bound-and-true-p company-backend)))))))


;;----------------------------------------------------------------
;; Highlight indentions
(when (display-graphic-p)
  (use-package highlight-indent-guides
    :ensure t
    :defer t
    :diminish
    :functions (ivy-cleanup-string
                my-ivy-cleanup-indentation)
    :commands highlight-indent-guides--highlighter-default
    :functions my-indent-guides-for-all-but-first-column
    :hook (prog-mode . highlight-indent-guides-mode)
    :init (setq highlight-indent-guides-method 'character
                highlight-indent-guides-responsive 'top)
    :config
    ;; Don't display indentations while editing with `company'
    (with-eval-after-load 'company
      (add-hook 'company-completion-started-hook
                (lambda (&rest _)
                  "Trun off indentation highlighting."
                  (when highlight-indent-guides-mode
                    (highlight-indent-guides-mode -1))))
      (add-hook 'company-after-completion-hook
                (lambda (&rest _)
                  "Trun on indentation highlighting."
                  (when (and (derived-mode-p 'prog-mode)
                             (not highlight-indent-guides-mode))
                    (highlight-indent-guides-mode 1)))))

    ;; Don't display first level of indentation
    (defun my-indent-guides-for-all-but-first-column (level responsive display)
      (unless (< level 1)
        (highlight-indent-guides--highlighter-default level responsive display)))
    (setq highlight-indent-guides-highlighter-function
          #'my-indent-guides-for-all-but-first-column)

    ;; Don't display indentations in `swiper'
    ;; https://github.com/DarthFennec/highlight-indent-guides/issues/40
    (with-eval-after-load 'ivy
      (defun my-ivy-cleanup-indentation (str)
        "Clean up indentation highlighting in ivy minibuffer."
        (let ((pos 0)
              (next 0)
              (limit (length str))
              (prop 'highlight-indent-guides-prop))
          (while (and pos next)
            (setq next (text-property-not-all pos limit prop nil str))
            (when next
              (setq pos (text-property-any next limit prop nil str))
              (ignore-errors
                (remove-text-properties next pos '(display nil face nil) str))))))
      (advice-add #'ivy-cleanup-string :after #'my-ivy-cleanup-indentation))))


;;----------------------------------------------------------------
;; hl-todo 在注释或者 string 里高亮 TODO 和类似的关键字
(use-package hl-todo
  :ensure t
  :hook ((prog-mode org-mode) . hl-todo-mode)
  :config
  (dolist (keyword '("BUG" "ISSUE" "PROMPT" "ATTENTION"))
    (cl-pushnew `(,keyword . ,(face-foreground 'error)) hl-todo-keyword-faces))
  (dolist (keyword '("HACK" "TRICK"))
    (cl-pushnew `(,keyword . ,(face-foreground 'warning)) hl-todo-keyword-faces)))


;;----------------------------------------------------------------
;; macrostep 展开当前宏
(use-package macrostep
  :ensure t
  :custom-face
  (macrostep-expansion-highlight-face ((t (:background ,(face-background 'tooltip)))))
  :bind (:map emacs-lisp-mode-map
              ("C-c e" . macrostep-expand)
              :map lisp-interaction-mode-map
              ("C-c e" . macrostep-expand))
  :config
  (add-hook 'after-load-theme-hook
            (lambda ()
              (set-face-background 'macrostep-expansion-highlight-face
                                   (face-background 'tooltip)))))


;;----------------------------------------------------------------
;; Page break lines
(use-package page-break-lines
  :ensure t
  :diminish page-break-lines-mode
  :hook (after-init . global-page-break-lines-mode))


;;----------------------------------------------------------------
;; projectile
(use-package projectile
  :ensure t
  :diminish
  :bind* (("C-c TAB"  . projectile-find-file)
          ;; ("C-c p" . (lambda () (interactive)
          ;;              (projectile-cleanup-known-projects)
          ;;              (projectile-discover-projects-in-search-path)))
          )
  :bind-keymap ("C-c p" . projectile-command-map)
  :custom
  (projectile-known-projects-file
   "~/.emacs.d/auto-save-list/projectile-bookmarks.eld")
  :init
  ;;; Shorter modeline
  (setq-default projectile-mode-line-prefix " ProJ"
                projectile-sort-order 'recentf
                projectile-use-git-grep t)
  :config
  (projectile-mode 1)
  ;; Integration with `projectile' 将 projectile 的补全系统设置为 ivy
  (with-eval-after-load 'projectile
    (setq projectile-completion-system 'ivy)))


;;----------------------------------------------------------------
;; rainbow-delimiters 用不同的方法颜色高亮不同层次的括号 (彩虹括号)
(use-package rainbow-delimiters
  :ensure t
  :diminish rainbow-delimiters-mode
  ;;; 在大多数编程语言(prog-mode-hook)中启动rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(if (fboundp 'global-prettify-symbols-mode)
    (add-hook 'after-init-hook 'global-prettify-symbols-mode))


;;----------------------------------------------------------------
;; rainbow-mode 显示字符串对应的颜色(例如: red 显示为红色)
(use-package rainbow-mode
  :ensure t
  :diminish rainbow-mode
  :hook ((css-mode html-mode sass-mode js-mode js2-mode) . rainbow-mode)
  :init
  (defun yantree/enable-rainbow-mode-if-theme ()
    (when (and (buffer-file-name) (string-match-p "\\(color-theme-\\|-theme\\.el\\)" (buffer-file-name)))
      (rainbow-mode)))
  :hook ((emacs-lisp-mode . yantree/enable-rainbow-mode-if-theme)
         (help-mode       . rainbow-mode)))


;;----------------------------------------------------------------
;; sicp 魔法书
(use-package sicp
  :ensure t
  :defer t)


;;----------------------------------------------------------------
;; which-key
(use-package which-key
  :ensure t
  :diminish
  :hook (after-init . which-key-mode)
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+ ")
  :bind (:map help-map
              ("C-h" . which-key-C-h-dispatch)))

(provide 'init-simple-packages)
;;; init-simple-packages.el ends here
