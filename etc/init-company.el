;;; init-company.el --- Completion with company -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;----------------------------------------------------------------
;; company
(use-package company
  :ensure t
  :diminish company-mode
  :commands (company-complete-common
             company-complete-common-or-cycle
             company-manual-begin
             company-grab-line)
  :bind (:map company-mode-map
              :map company-active-map
              ("C-n"       . company-select-next)
              ("C-p"       . company-select-previous))
  :hook (after-init . global-company-mode)
  :init
  (setq
   company-idle-delay 0.6
   company-minimum-prefix-length 2
   company-tooltip-limit 14
   company-tooltip-align-annotations t
   company-require-match 'never
   company-global-modes
   '(not erc-mode
         message-mode
         help-mode
         gud-mode
         vterm-mode)
   company-frontends
   '(company-pseudo-tooltip-frontend  ;; always show candidates in overlay tooltip
     company-echo-metadata-frontend)  ;; show selected candidate docs in echo area
   ;; Buffer-local backends will be computed when loading a major mode, so
   ;; only specify a global default here.
   company-backends '(company-capf)

   ;; These auto-complete the current selection when
   ;; `company-auto-complete-chars' is typed. This is too magical. We
   ;; already have the much more explicit RET and TAB.
   company-auto-complete nil
   company-auto-complete-chars nil
   ;; Only search the current buffer for `company-dabbrev' (a backend that
   ;; suggests text your open buffers). This prevents Company from causing
   ;; lag once you have a lot of buffers open.
   company-dabbrev-other-buffers nil
   ;; Make `company-dabbrev' fully case-sensitive, to improve UX with
   ;; domain-specific words with particular casing.
   company-dabbrev-ignore-case nil
   company-dabbrev-downcase nil)

  ;;----------------------------------------------------------------
  ;; Suspend page-break-lines-mode while company menu is active
  ;; (see https://github.com/company-mode/company-mode/issues/416)
  :config
  (with-eval-after-load 'eldoc
    (eldoc-add-command 'company-complete-selection
                       'company-complete-common
                       'company-capf
                       'company-abort))

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
      (add-hook 'company-after-completion-hook 'yantree/page-break-lines-maybe-reenable))))


;;----------------------------------------------------------------
;; 更好的过滤和筛选(nice!)
(use-package company-prescient
  :ensure t
  :init (company-prescient-mode 1))
;; (use-package company-fuzzy
;;   :ensure t
;;   :hook (company-mode . global-company-fuzzy-mode)
;;   :init
;;   (setq company-fuzzy-sorting-function (lambda (candidates)
;;                                          (message "%s" candidates)
;;                                          candidates))  ; Don't forget to return the candidaites!
;;   (setq company-fuzzy-sorting-backend 'alphabetic
;;         company-fuzzy-prefix-ontop t))


;;----------------------------------------------------------------
;; Icons 图标
(use-package company-box
  :ensure t
  :hook (company-mode . company-box-mode)
  :config
  (setq company-box-show-single-candidate t
        company-box-backends-colors nil
        company-box-max-candidates 50
        company-box-icons-alist 'company-box-icons-all-the-icons
        company-box-icons-functions
        (cons #'+company-box-icons--elisp-fn
              (delq 'company-box-icons--elisp
                    company-box-icons-functions))
        company-box-icons-all-the-icons
        (let ((all-the-icons-scale-factor 0.8))
          `((Unknown       . ,(all-the-icons-material "find_in_page"             :face 'all-the-icons-purple))
            (Text          . ,(all-the-icons-material "text_fields"              :face 'all-the-icons-green))
            (Method        . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
            (Function      . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
            (Constructor   . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
            (Field         . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
            (Variable      . ,(all-the-icons-material "adjust"                   :face 'all-the-icons-blue))
            (Class         . ,(all-the-icons-material "class"                    :face 'all-the-icons-red))
            (Interface     . ,(all-the-icons-material "settings_input_component" :face 'all-the-icons-red))
            (Module        . ,(all-the-icons-material "view_module"              :face 'all-the-icons-red))
            (Property      . ,(all-the-icons-material "settings"                 :face 'all-the-icons-red))
            (Unit          . ,(all-the-icons-material "straighten"               :face 'all-the-icons-red))
            (Value         . ,(all-the-icons-material "filter_1"                 :face 'all-the-icons-red))
            (Enum          . ,(all-the-icons-material "plus_one"                 :face 'all-the-icons-red))
            (Keyword       . ,(all-the-icons-material "filter_center_focus"      :face 'all-the-icons-red))
            (Snippet       . ,(all-the-icons-material "short_text"               :face 'all-the-icons-red))
            (Color         . ,(all-the-icons-material "color_lens"               :face 'all-the-icons-red))
            (File          . ,(all-the-icons-material "insert_drive_file"        :face 'all-the-icons-red))
            (Reference     . ,(all-the-icons-material "collections_bookmark"     :face 'all-the-icons-red))
            (Folder        . ,(all-the-icons-material "folder"                   :face 'all-the-icons-red))
            (EnumMember    . ,(all-the-icons-material "people"                   :face 'all-the-icons-red))
            (Constant      . ,(all-the-icons-material "pause_circle_filled"      :face 'all-the-icons-red))
            (Struct        . ,(all-the-icons-material "streetview"               :face 'all-the-icons-red))
            (Event         . ,(all-the-icons-material "event"                    :face 'all-the-icons-red))
            (Operator      . ,(all-the-icons-material "control_point"            :face 'all-the-icons-red))
            (TypeParameter . ,(all-the-icons-material "class"                    :face 'all-the-icons-red))
            (Template      . ,(all-the-icons-material "short_text"               :face 'all-the-icons-green))
            (ElispFunction . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
            (ElispVariable . ,(all-the-icons-material "check_circle"             :face 'all-the-icons-blue))
            (ElispFeature  . ,(all-the-icons-material "stars"                    :face 'all-the-icons-orange))
            (ElispFace     . ,(all-the-icons-material "format_paint"             :face 'all-the-icons-pink)))))

  ;; HACK Fix oversized scrollbar in some odd cases
  ;; REVIEW `resize-mode' is deprecated and may stop working in the future.
  ;; TODO PR me upstream?
  (setq x-gtk-resize-child-frames 'resize-mode)

  ;; Disable tab-bar in company-box child frames
  ;; TODO PR me upstream!
  (add-to-list 'company-box-frame-parameters '(tab-bar-lines . 0))

  (defun +company-box-icons--elisp-fn (candidate)
    (when (derived-mode-p 'emacs-lisp-mode)
      (let ((sym (intern candidate)))
        (cond ((fboundp sym)  'ElispFunction)
              ((boundp sym)   'ElispVariable)
              ((featurep sym) 'ElispFeature)
              ((facep sym)    'ElispFace))))))

(provide 'init-company)
;;; init-company.el ends here
