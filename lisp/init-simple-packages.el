;;; -*- lexical-binding: t; -*-
;; 配置不超过五十行代码的插件 or......我想把它放这儿

;;----------------------------------------------------------------
;; aggresive-indent ( 时时保持缩进 )
(leaf aggressive-indent
  :ensure t
  :hook((after-init-hook . global-aggressive-indent-mode)
        ;; FIXME: Disable in big files due to the performance issues
        (find-file .  (lambda ()
                        (if (> (buffer-size) (* 3000 80))
                            (aggressive-indent-mode -1)))))
  :config
  ;; Disable in some modes
  (dolist (mode '(web-mode html-mode css-mode go-mode))
    (push mode aggressive-indent-excluded-modes))

  ;; Disable in some commands
  (add-to-list 'aggressive-indent-protected-commands #'delete-trailing-whitespace t)
  :diminish)


;;----------------------------------------------------------------
;; diff-hl 高亮显示未提交的change
(leaf diff-hl
  :ensure t
  :bind (:diff-hl-command-map
         ("SPC" . diff-hl-mark-hunk))
  :hook ((dired-mode-hook . diff-hl-dired-mode)
         (after-init-hook . global-diff-hl-mode))
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
  (leaf exec-path-from-shell
               :ensure t
               :leaf-defer t
               :init
               (setq exec-path-from-shell-check-startup-files nil
                     exec-path-from-shell-variables '("PATH" "MANPATH" "PYTHONPATH" "GOPATH")
                     exec-path-from-shell-arguments '("-l")))
  (exec-path-from-shell-initialize))


;;----------------------------------------------------------------
;; flycheck (语法检查
(leaf flycheck
  :ensure t
  :leaf-defer t
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
          (leaf flycheck-posframe
            :ensure t
            :after flycheck
            :hook (flycheck-mode-hook . flycheck-posframe-mode)
            :config
            (add-to-list 'flycheck-posframe-inhibit-functions
                         #'(lambda () (bound-and-true-p company-backend))))))
  :diminish )


;;----------------------------------------------------------------
;; Highlight indentions
(when (display-graphic-p)
  (leaf highlight-indent-guides
    :ensure t
    :leaf-defer t
    :commands highlight-indent-guides--highlighter-default
    :hook (prog-mode-hook . highlight-indent-guides-mode)
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
      (advice-add #'ivy-cleanup-string :after #'my-ivy-cleanup-indentation)))
  :diminish)


;;----------------------------------------------------------------
;; hl-todo 在注释或者 string 里高亮 TODO 和类似的关键字
(leaf hl-todo
  :ensure t
  :hook ((prog-mode-hook org-mode-hook) . hl-todo-mode)
  :config
  (dolist (keyword '("BUG" "ISSUE" "PROMPT" "ATTENTION"))
    (cl-pushnew `(,keyword . ,(face-foreground 'error)) hl-todo-keyword-faces))
  (dolist (keyword '("HACK" "TRICK"))
    (cl-pushnew `(,keyword . ,(face-foreground 'warning)) hl-todo-keyword-faces)))


;;----------------------------------------------------------------
;; Page break lines
(leaf page-break-lines
  :ensure t
  :hook (after-init-hook . global-page-break-lines-mode)
  :diminish)


;;----------------------------------------------------------------
;; rainbow-delimiters 用不同的方法颜色高亮不同层次的括号 (彩虹括号)
(leaf rainbow-delimiters
  :ensure t
  ;;; 在大多数编程语言(prog-mode-hook)中启动rainbow-delimiters
  :hook (prog-mode-hook . rainbow-delimiters-mode)
  :diminish)

(if (fboundp 'global-prettify-symbols-mode)
    (add-hook 'after-init-hook 'global-prettify-symbols-mode))


;;----------------------------------------------------------------
;; which-key
(leaf which-key
  :ensure t
  :hook (after-init-hook . which-key-mode)
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+ ")
  :bind (:help-map
         ("C-h" . which-key-C-h-dispatch))
  :diminish)

(provide 'init-simple-packages)
;;; init-simple-packages.el ends here
