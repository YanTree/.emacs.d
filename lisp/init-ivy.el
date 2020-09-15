;;; -*- lexical-binding: t; -*-

;;----------------------------------------------------------------
;; ivy
(leaf ivy
  :ensure t
  :hook (after-init-hook . ivy-mode)
  :bind (("C-x b"   . ivy-switch-buffer)
         (:ivy-switch-buffer-map
          ("C-k"     . ivy-switch-buffer-kill))
         )
  :config
  ;; about ivy
  (setq-default ivy-use-virtual-buffers t          ;;将最近打开的文件和书签放进 `ivy-switch-buffer'
                enable-recursive-minibuffers t     ;;允许在 minibuffer 里使用命令(M-x:)
                ivy-count-format "(%d/%d) "        ;;ivy 计数的样式
                ivy-height 13                      ;;ivy 弹窗的高度(13行)
                ivy-virtual-abbreviate 'fullpath   ;;用绝对路径显示未高亮的 buffer
                ivy-display-style 'fancy           ;;在 ivy 里高粱显示匹配的字符
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
                  '((swiper           . ivy--regex-plus)
                    (swiper-all       . ivy--regex-plus)
                    (swiper-isearch   . ivy--regex-plus)
                    (counsel-ag       . ivy--regex-plus)
                    (counsel-rg       . ivy--regex-plus)
                    (counsel-pt       . ivy--regex-plus)
                    (counsel-git-grep . ivy--regex-plus)
                    (counsel-grep     . ivy--regex-plus)
                    (t                . ivy--regex-fuzzy))))
  :diminish)


;;----------------------------------------------------------------
;; counsel
(leaf counsel
  :ensure t
  :bind (("C-s"           . swiper-isearch)
         ("M-x"           . counsel-M-x)
         ("C-x C-f"       . counsel-find-file)
         ("C-S-s"         . isearch-forward)
         ("C-S-r"         . isearch-backward)
         ([remap swiper]  . counsel-grep-or-swiper)
         ([remap dired]   . counsel-dired)
         ("C-x C-r"       . counsel-recentf)     ;;打开 recentf 文件
         ("C-c C-b"       . counsel-bookmark)    ;;打开书签
         ("C-h k"         . counsel-descbinds)   ;;查找绑定快捷键
         ("M-?"           . yantree/counsel-search-project))
  :init
  ;; 用counsel的功能覆盖初始变量
  (setq-default counsel-mode-override-describe-bindings t)
  :config
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

  ;; project search
  (let ((search-function
         (cond
          ((executable-find "rg") 'counsel-rg)
          ((executable-find "ag") 'counsel-ag)
          ((executable-find "pt") 'counsel-pt)
          ((executable-find "ack") 'counsel-ack))))
    (when search-function
      (defun yantree/counsel-search-project (initial-input &optional use-current-dir)
        "Search using `counsel-rg' or similar from the project root for INITIAL-INPUT.
If there is no project root, or if the prefix argument
USE-CURRENT-DIR is set, then search from the current directory
instead."
        (interactive (list current-prefix-arg))
        (let ((current-prefix-arg)
              (dir (if use-current-dir
                       default-directory
                     (condition-case err
                         (projectile-project-root)
                       (error default-directory)))))
          (funcall search-function initial-input dir)))))

  (with-eval-after-load 'ivy
    (add-to-list 'ivy-height-alist (cons 'counsel-rg 25))
    (add-to-list 'ivy-height-alist (cons 'counsel-ag 25)))

  ;; Pre-fill search keywords
  (defvar my-ivy-fly-commands '(query-replace-regexp
                                flush-lines
                                keep-lines
                                ivy-read
                                swiper
                                swiper-backward
                                swiper-all
                                swiper-isearch
                                swiper-isearch-backward
                                counsel-grep-or-swiper
                                counsel-grep
                                counsel-ack
                                counsel-ag
                                counsel-rg
                                counsel-pt
                                counsel-git-grep
                                yantree/counsel-search-project))

  (defun my-ivy-fly-back-to-present ()
    (cond ((and (memq last-command my-ivy-fly-commands)
                (equal (this-command-keys-vector) (kbd "M-p")))
           ;; repeat one time to get straight to the first history item
           (setq unread-command-events
                 (append unread-command-events
                         (listify-key-sequence (kbd "M-p")))))
          ((or (memq this-command '(self-insert-command
                                    yank
                                    ivy-yank-word
                                    counsel-yank-pop))
               (equal (this-command-keys-vector) (kbd "M-n")))
           (delete-region (point)
                          (point-max)))))

  (defun my-ivy-fly-time-travel ()
    (when (memq this-command my-ivy-fly-commands)
      (let* ((kbd (kbd "M-n"))
             (cmd (key-binding kbd))
             (future (and cmd
                          (with-temp-buffer
                            (when (ignore-errors
                                    (call-interactively cmd) t)
                              (buffer-string))))))
        (when future
          (save-excursion
            (insert (propertize (replace-regexp-in-string
                                 "\\\\_<" ""
                                 (replace-regexp-in-string
                                  "\\\\_>" ""
                                  future))
                                'face 'shadow)))
          (add-hook 'pre-command-hook 'my-ivy-fly-back-to-present nil t)))))

  (add-hook 'minibuffer-setup-hook #'my-ivy-fly-time-travel)
  (add-hook 'minibuffer-exit-hook
            (lambda ()
              (remove-hook 'pre-command-hook 'my-ivy-fly-back-to-present t)))
  :diminish)


;;----------------------------------------------------------------
;; swiper
(leaf swiper
  :ensure t
  :leaf-defer t
  :init
  ;;recenter after exiting ‘swiper’.
  (setq swiper-action-recenter t))


;;----------------------------------------------------------------
;; ivy-xref (Select from xref candidates with Ivy
(leaf ivy-xref
  :ensure t
  :init
  (setq xref-show-xrefs-function 'ivy-xref-show-xrefs))


;;----------------------------------------------------------------
;; enhance M-x, record use times of commands
(leaf smex
  :ensure t
  :init
  (setq-default smex-save-file (expand-file-name "auto-save-list/.smex-items" user-emacs-directory))
  :bind ([remap execute-extended-command] . smex))


;;----------------------------------------------------------------
;; ivy-rich
;; (leaf ivy-rich
;;   :ensure t
;;   :hook(ivy-mode-hook . (lambda () (ivy-rich-mode ivy-mode)))
;;   :init
;;   (setq ivy-virtual-abbreviate 'abbreviate
;;         ivy-rich-switch-buffer-align-virtual-buffer nil
;;         ivy-rich-path-style 'abbrev
;;         ivy-rich-parse-remote-buffer nil))


(provide 'init-ivy)
;;; init-ivy.el ends here
