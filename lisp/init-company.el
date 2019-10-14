;;; -*- lexical-binding: t; -*-

;;----------------------------------------------------------------
;; company
(use-package company
  :ensure t
  :diminish company-mode
  :bind (:map company-mode-map
              ("<backtab>" . company-yasnippet)
              :map company-active-map
              ("<tab>"     . company-complete-common-or-cycle)
              ("<backtab>" . yantree-company-yasnippet)
              ("C-n"       . company-select-next)
              ("C-p"       . company-select-previous))
  :hook (after-init . global-company-mode)
  :init
  (defun yantree-company-yasnippet ()
    "Hide the current completeions and show snippets."
    (interactive)
    (company-abort)
    (call-interactively 'company-yasnippet))

  (setq-default
   company-dabbrev-other-buffers 'all                    ;;在所有 buffer 里搜索补全候选项
   company-tooltip-align-annotations t
   company-require-match nil
   company-global-modes '(not shell-mode eshell-mode)
   company-echo-delay (if (display-graphic-p) nil 0)
   company-show-numbers t
   company-idle-delay 0.2
   company-minimum-prefix-length 2))


;;----------------------------------------------------------------
;; Suspend page-break-lines-mode while company menu is active
;; (see https://github.com/company-mode/company-mode/issues/416)
(with-eval-after-load 'company
  (with-eval-after-load 'page-break-lines
    (defvar-local yantree/page-break-lines-on-p nil)

    (defun yantree/page-break-lines-disable (&rest ignore)
      (when (setq yantree/page-break-lines-on-p (bound-and-true-p page-break-lines-mode))
        (page-break-lines-mode -1)))

    (defun yantree/page-break-lines-maybe-reenable (&rest ignore)
      (when yantree/page-break-lines-on-p
        (page-break-lines-mode 1)))

    (add-hook 'company-completion-started-hook 'yantree/page-break-lines-disable)
    (add-hook 'company-after-completion-hook 'yantree/page-break-lines-maybe-reenable)))


;;----------------------------------------------------------------
;; 更好的过滤和筛选(nice!)
(use-package company-prescient
  :ensure t
  :config
  (company-prescient-mode 1))


;;----------------------------------------------------------------
;; 他会额外安装pos-tip
(use-package company-quickhelp
  :ensure t
  :init (setq company-quickhelp-delay 0.5)
  :bind (:map company-active-map
              ([remap company-show-doc-buffer] . company-quickhelp-manual-begin))
  :hook (global-company-mode . company-quickhelp-mode))

(provide 'init-company)
;;; init-company.el ends here
