;;; -*- lexical-binding: t; -*-

;;----------------------------------------------------------------
;; org
(use-package org
  :ensure nil
  :defer t
  :hook ((org-indent-mode . (lambda()
                              (diminish 'org-indent-mode)
                              ;; WORKAROUND: Prevent text moving around while using brackets
                              ;; @see https://github.com/seagle0128/.emacs.d/issues/88
                              (make-variable-buffer-local 'show-paren-mode)
                              (setq show-paren-mode nil))))
  :bind ("C-c b" . org-switchb)        ;;switch buffer for org files
  :init
  ;; Various preferences
  (setq-default
   org-log-done 'time                  ;;done时,添加时间标签
   org-startup-indented t              ;;打开任意一个org文件,自动indented
   org-hide-leading-stars t            ;;隐藏 stars
   org-tags-column 80
   org-export-coding-system 'utf-8
   org-deadline-warning-days 5         ;;最后期限到达前５天即给出警告
   org-use-sub-superscripts nil        ;; the_sliver_search
   org-agenda-skip-deadline-if-done t
   org-directory "~/.emacs.d/private/org"
   org-default-notes-file (concat org-directory "/note.org")
   ;; Babel
   org-src-fontify-natively t          ;;fontify code in code blocks
   )

  ;;; To-do settings
  (setq org-todo-keywords
        (quote ((sequence "TODO(t)" "|" "DONE(d!/!)" "CANCELLED(c@/!)")))))


;;----------------------------------------------------------------
;; agenda
(use-package org-agenda
  :ensure nil
  :defer t
  :hook (org-agenda-mode . (lambda () (add-hook 'window-configuration-change-hook 'org-agenda-align-tags nil t)))
  :bind ("C-c a" . org-agenda)
  :init
  (setq org-agenda-files '("~/.emacs.d/private/org/Agenda") ;; 设置默认 Org Agenda 文件目录
        org-agenda-compact-blocks t                         ;; 使 agenda 看起来更简洁
        org-agenda-sticky t                                 ;; 保留 *agenda* buffer
        org-agenda-start-on-weekday nil                     ;; 总是预览今天的日程
        org-agenda-span 'day)
  ;;Simple agenda view
  (setq org-agenda-custom-commands
        '(("c" "Simple agenda view"
           ((agenda "")
            (alltodo "")))))
  :config
  (add-to-list 'org-agenda-after-show-hook 'org-show-entry)
  (advice-add 'org-refile :after (lambda (&rest _) (org-save-all-org-buffers)))

  ;; 把 DONE state tasks 从refile中里去除掉
  (defun yantree/verify-refile-target ()
    "Exclude(排除) todo keywords with a done state from refile targets."
    (not (member (nth 2 (org-heading-components)) org-done-keywords)))
  (setq org-refile-target-verify-function 'yantree/verify-refile-target)
  ;; ends
  ;; ----
  ;; (defun yantree/org-refile-anywhere (&optional goto default-buffer rfloc msg)
  ;;   "A version of `org-refile' which allows refiling to any subtree."
  ;;   (interactive "P")
  ;;   (let ((org-refile-target-verify-function))
  ;;     (org-refile goto default-buffer rfloc msg)))
  ;; ;; ----
  ;; (defun yantree/org-agenda-refile-anywhere (&optional goto rfloc no-update)
  ;;   "A version of `org-agenda-refile' which allows refiling to any subtree."
  ;;   (interactive "P")
  ;;   (let ((org-refile-target-verify-function))
  ;;     (org-agenda-refile goto rfloc no-update))))
  )


;;----------------------------------------------------------------
;; capture
(use-package org-capture
  :ensure nil
  :defer t
  :bind ("C-c c" . org-capture)
  :config
  (setq org-capture-templates
        `(("i" "Idea"
           entry (file+headline "~/.emacs.d/private/org/Capture/note.org" "Ideas")
           "* %?\n%T\n" :prepend t)

          ("l" "Link"
           entry (file+headline "~/.emacs.d/private/org/Capture/links.org" "Links")
           "* %? %^L %^g \n%T\n" :prepend t)

          ("n" "Note"
           entry (file+headline "~/.emacs.d/private/org/Capture/note.org" "Learning Notes!")
           "* %? :Note:\n%T\n")

          ("t" "Task"
           entry (file+headline "~/.emacs.d/private/org/Capture/todo.org" "Tasks!")
           "* TODO %? %U %^G\nDEADLINE: %T\n%i" :clock-resume t)

          ("s" "Screencast"
           entry (file+headline "~/.emacs.d/private/org/Capture/note.org" "Screencast")
           "* %? \n%i\n"))))


;;----------------------------------------------------------------
;; Table of contents
(use-package toc-org
  :ensure t
  :hook (org-mode . toc-org-mode))


;;----------------------------------------------------------------
;; 让星星变得更好看
(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode))


;;----------------------------------------------------------------
;; 预览 org 文件(html)
;; (use-package org-preview-html
;;   :ensure t
;;   :diminish)


;;----------------------------------------------------------------
;; 让优先级标志更好看
;; (use-package org-fancy-priorities
;;   :ensure t
;;   :hook (org-mode . org-fancy-priorities)
;;   :config
;;   (setq org-fancy-priorities-list '("⚡" "⬆" "⬇" "☕")))


;;----------------------------------------------------------------
;; epresent ( 适合用来做演讲,一步一步的放映每一级的标题)
;; (use-package epresent
;;   :ensure t
;;   :defer t)

(provide 'init-org)
;;; init-org.el ends here
