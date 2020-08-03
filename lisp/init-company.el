;;; -*- lexical-binding: t; -*-

;;----------------------------------------------------------------
;; company
(use-package company
  :ensure t
  :diminish company-mode
  :bind (:map company-mode-map
              ("<backtab>" . company-yasnippet)   ;;shift+tab
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
;; Icons 图标
(use-package company-box
  :ensure t
  :diminish
  :defines company-box-icons-all-the-icons
  :hook (company-mode . company-box-mode)
  :init (setq company-box-enable-icon t
              company-box-backends-colors nil
              company-box-show-single-candidate t
              company-box-max-candidates 50
              company-box-doc-delay 0.2)
  :config
  (with-no-warnings
    ;; FIXME: Display common text correctly
    (defun my-company-box--update-line (selection common)
      (company-box--update-image)
      (goto-char 1)
      (forward-line selection)
      (let* ((beg (line-beginning-position))
             (txt-beg (+ company-box--icon-offset beg)))
        (move-overlay (company-box--get-ov) beg (line-beginning-position 2))
        (move-overlay (company-box--get-ov-common) txt-beg
                      (+ (length common) txt-beg)))
      (let ((color (or (get-text-property (point) 'company-box--color)
                       'company-box-selection)))
        (overlay-put (company-box--get-ov) 'face color)
        (overlay-put (company-box--get-ov-common) 'face 'company-tooltip-common-selection)
        (company-box--update-image color))
      (run-hook-with-args 'company-box-selection-hook selection
                          (or (frame-parent) (selected-frame))))
    (advice-add #'company-box--update-line :override #'my-company-box--update-line)

    (defun my-company-box--render-buffer (string)
      (let ((selection company-selection)
            (common (or company-common company-prefix)))
        (with-current-buffer (company-box--get-buffer)
          (erase-buffer)
          (insert string "\n")
          (setq mode-line-format nil
                display-line-numbers nil
                truncate-lines t
                cursor-in-non-selected-windows nil)
          (setq-local scroll-step 1)
          (setq-local scroll-conservatively 10000)
          (setq-local scroll-margin  0)
          (setq-local scroll-preserve-screen-position t)
          (add-hook 'window-configuration-change-hook 'company-box--prevent-changes t t)
          (company-box--update-line selection common))))
    (advice-add #'company-box--render-buffer :override #'my-company-box--render-buffer)

    ;; Prettify icons
    (defun my-company-box-icons--elisp (candidate)
      (when (derived-mode-p 'emacs-lisp-mode)
        (let ((sym (intern candidate)))
          (cond ((fboundp sym) 'Function)
                ((featurep sym) 'Module)
                ((facep sym) 'Color)
                ((boundp sym) 'Variable)
                ((symbolp sym) 'Text)
                (t . nil)))))
    (advice-add #'company-box-icons--elisp :override #'my-company-box-icons--elisp))

  (when (and (display-graphic-p)
             (require 'all-the-icons nil t))
    (declare-function all-the-icons-faicon 'all-the-icons)
    (declare-function all-the-icons-material 'all-the-icons)
    (declare-function all-the-icons-octicon 'all-the-icons)
    (setq company-box-icons-all-the-icons
          `((Unknown . ,(all-the-icons-material "find_in_page" :height 0.8 :v-adjust -0.15))
            (Text . ,(all-the-icons-faicon "text-width" :height 0.8 :v-adjust -0.02))
            (Method . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.02 :face 'all-the-icons-purple))
            (Function . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.02 :face 'all-the-icons-purple))
            (Constructor . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.02 :face 'all-the-icons-purple))
            (Field . ,(all-the-icons-octicon "tag" :height 0.85 :v-adjust 0 :face 'all-the-icons-lblue))
            (Variable . ,(all-the-icons-octicon "tag" :height 0.85 :v-adjust 0 :face 'all-the-icons-lblue))
            (Class . ,(all-the-icons-material "settings_input_component" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-orange))
            (Interface . ,(all-the-icons-material "share" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-lblue))
            (Module . ,(all-the-icons-material "view_module" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-lblue))
            (Property . ,(all-the-icons-faicon "wrench" :height 0.8 :v-adjust -0.02))
            (Unit . ,(all-the-icons-material "settings_system_daydream" :height 0.8 :v-adjust -0.15))
            (Value . ,(all-the-icons-material "format_align_right" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-lblue))
            (Enum . ,(all-the-icons-material "storage" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-orange))
            (Keyword . ,(all-the-icons-material "filter_center_focus" :height 0.8 :v-adjust -0.15))
            (Snippet . ,(all-the-icons-material "format_align_center" :height 0.8 :v-adjust -0.15))
            (Color . ,(all-the-icons-material "palette" :height 0.8 :v-adjust -0.15))
            (File . ,(all-the-icons-faicon "file-o" :height 0.8 :v-adjust -0.02))
            (Reference . ,(all-the-icons-material "collections_bookmark" :height 0.8 :v-adjust -0.15))
            (Folder . ,(all-the-icons-faicon "folder-open" :height 0.8 :v-adjust -0.02))
            (EnumMember . ,(all-the-icons-material "format_align_right" :height 0.8 :v-adjust -0.15))
            (Constant . ,(all-the-icons-faicon "square-o" :height 0.8 :v-adjust -0.1))
            (Struct . ,(all-the-icons-material "settings_input_component" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-orange))
            (Event . ,(all-the-icons-octicon "zap" :height 0.8 :v-adjust 0 :face 'all-the-icons-orange))
            (Operator . ,(all-the-icons-material "control_point" :height 0.8 :v-adjust -0.15))
            (TypeParameter . ,(all-the-icons-faicon "arrows" :height 0.8 :v-adjust -0.02))
            (Template . ,(all-the-icons-material "format_align_left" :height 0.8 :v-adjust -0.15)))
          company-box-icons-alist 'company-box-icons-all-the-icons)))


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
