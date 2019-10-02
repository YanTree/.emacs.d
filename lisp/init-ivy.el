;;; -*- lexical-binding: t; -*-


;;--------------------------------------------------------
;;       ivy,counsel,swiper,ivy-xref and smex(增强ivy--regex-fuzzy)
;;---------------------------------------------------------
(use-package ivy
  :ensure t
  :diminish ivy-mode
  :hook (after-init . ivy-mode)
  :bind (:map ivy-switch-buffer-map
              ("C-k"     . ivy-switch-buffer-kill)

              :map ivy-minibuffer-map
              ("RET"     . ivy-alt-done))

  :init
  ;; about ivy
  (setq-default ivy-use-virtual-buffers t ;;add recent files and bookmarks to `ivy-switch-buffer'
                enable-recursive-minibuffers t
                ivy-count-format "(%d/%d) "
                ivy-height 13
                ivy-virtual-abbreviate 'fullpath
                projectile-completion-system 'ivy
                ivy-display-style 'fancy
                ivy-magic-tilde nil
                ivy-use-selectable-prompt t
                ivy-initial-inputs-alist nil)

  ;;你可以查看 ivy-format-functions-alist，默认有三种选择，可以自己 hack 一下
  (defun yantree-ivy-format-function-arrow (cands)
    "Transform CANDS into a string for minibuffer."
    (ivy--format-function-generic
     (lambda (str)
       (concat "  -> " (ivy--add-face str 'ivy-current-match)))
     (lambda (str)
       (concat "     " str))
     cands
     "\n"))

  (setq ivy-format-functions-alist '((t . yantree-ivy-format-function-arrow)))

  (defun ivy-switch-buffer-kill ()
    (interactive)
    (debug)
    (let ((bn (ivy-state-current ivy-last)))
      (when (get-buffer bn)
        (kill-buffer bn))
      (unless (buffer-live-p (ivy-state-buffer ivy-last))
        (setf (ivy-state-buffer ivy-last)
              (with-ivy-window (current-buffer))))
      (setq ivy--all-candidates (delete bn ivy--all-candidates))
      (ivy--exhibit)))

  (defun yantree/enable-ivy-flx-matching ()
    "Make `ivy' matching work more like IDO."
    (interactive)
    (use-package flx :ensure t :defer t)
    (setq-default ivy-re-builders-alist
                  '((swiper          . ivy--regex-plus)
                    (swiper-all      . ivy--regex-plus)
                    (swiper-isearch  . ivy--regex-plus)
                    (counsel-ag      . ivy--regex-plus)
                    (counsel-rg      . ivy--regex-plus)
                    (counsel-pt      . ivy--regex-plus)
                    (counsel-grep    . ivy--regex-plus)
                    (t               . ivy--regex-fuzzy)))))



;;-------------------------------------------------------------------------
(use-package counsel
  :ensure t
  :defines (projectile-completion-system
            magit-completing-read-function recentf-list)
  :diminish ivy-mode counsel-mode
  :bind (("C-s"           . swiper-isearch)
         ;;("s-f"         . swiper)
         ("C-S-s"         . isearch-forward)
         ("C-S-r"         . isearch-backward)

         :map counsel-mode-map
         ([remap swiper]  . counsel-grep-or-swiper)
         ([remap dired]   . counsel-dired)
         ("C-x C-r"       . counsel-recentf)     ;;打开 recentf 文件
         ("C-c b"         . counsel-bookmark)    ;;打开书签
         ("C-h k"         . counsel-descbinds)   ;;查找绑定快捷键
         ("C-c s"         . counsel-grep)        ;;当前 buffer 里查找字符串
         ("C-c g"         . counsel-git-grep)    ;;在当前版本控制下的文件里搜索字符串
         ("C-c r"         . counsel-rg)          ;;使用rg,这是在当前目录下搜索
         )
  :hook (ivy-mode . counsel-mode)
  :init
  ;; 用counsel的功能覆盖初始变量
  (setq-default counsel-mode-override-describe-bindings t)

  (setq swiper-action-recenter t)

  ;;Build abbreviated recent file list, use "~/" instead of "/home/username" .
  (defun yantree-counsel-recentf ()
    "Find a file on `recentf-list'."
    (interactive)
    (require 'recentf)
    (recentf-mode)
    (ivy-read "Recentf: " (mapcar #'abbreviate-file-name recentf-list)
              :action (lambda (f)
                        (with-ivy-window
                          (find-file f)))
              :require-match t
              :caller 'counsel-recentf))
  (advice-add #'counsel-recentf :override #'yantree-counsel-recentf)

  )



;;-------------------------------------------------------------------------
(use-package swiper
  :ensure t
  :init
  (defun yantree/swiper-at-point (sym)
    "Use `swiper' to search for the symbol at point."
    (interactive (list (thing-at-point 'symbol)))
    (swiper sym))
  :bind (:map ivy-mode-map
              ("M-s /" . yantree/swiper-at-point)))



;;-------------------------------------------------------------------------
;;Enhance fuzzy matching
;; (use-package prescient
;;   :ensure t
;;   :defer t
;;   :commands prescient-persist-mode
;;   :init
;;   (setq-default prescient-save-file
;;                 (expand-file-name "auto-save-list/prescient-save.el" user-emacs-directory))
;;   (setq prescient-filter-method '(literal regexp initialism fuzzy))
;;   (prescient-persist-mode 1))

;; (use-package ivy-prescient
;;   :ensure t
;;   :defer t
;;   :commands ivy-prescient-re-builder
;;   :preface
;;   (defun ivy-prescient-non-fuzzy (str)
;;     (let ((prescient-filter-method '(literal regexp)))
;;       (ivy-prescient-re-builder str)))
;;   :init
;;   (setq ivy-prescient-enable-filtering t
;;         ivy-prescient-retain-classic-highlighting t
;;         ivy-re-builders-alist '((counsel-ag . ivy-prescient-non-fuzzy)
;;                                 (counsel-rg . ivy-prescient-non-fuzzy)
;;                                 (counsel-pt . ivy-prescient-non-fuzzy)
;;                                 (counsel-grep . ivy-prescient-non-fuzzy)
;;                                 (swiper . ivy-prescient-non-fuzzy)
;;                                 (swiper-isearch . ivy-prescient-non-fuzzy)
;;                                 (swiper-all . ivy-prescient-non-fuzzy)
;;                                 (t . ivy-prescient-re-builder)))
;;   (ivy-prescient-mode 1))




;;-------------------------------------------------------------------------
(use-package ivy-xref
  :ensure t
  :defer t
  :init
  (when (boundp 'xref-show-definitions-function)
    (setq xref-show-definitions-function #'ivy-xref-show-defs)))



;;-------------------------------------------------------------------------
(use-package smex
  :ensure t
  :init
  (setq-default smex-save-file (expand-file-name "auto-save-list/.smex-items" user-emacs-directory))
  :bind ([remap execute-extended-command] . smex))



;;-------------------------------------------------------------------------
;; snails
;; (use-package snails
;;   :defer t
;;   :commands snails
;;   :load-path "site-lisp/extensions/snails"
;;   )




(provide 'init-ivy)
;;; init-ivy.el ends here
