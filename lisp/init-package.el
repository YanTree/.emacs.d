;;; -*- lexical-binding: t; -*-


;;; Initialize packages
(unless (bound-and-true-p package--initialized) ; To avoid warnings in 27
  (setq package-enable-at-startup nil)          ; To prevent initializing twice
  (package-initialize))


(defvar yantree-package-archives 'tuna
  "Set package archives from which to fetch, you can choice melpa, melpa-mirror, emacs-china, netease, or tuna")


;;增加额外的package repositories
(defun set-package-archives (archives)
  "Set specific package ARCHIVES repository. Currently, there are 5 repository"
  (interactive
   (list (intern (completing-read "Choose package archives: "
                                  '(melpa melpa-mirror emacs-china netease tuna)))))

  (setq package-archives
        (let* ((no-ssl (and (memq system-type '(windows-nt ms-dos));;判断是什么类型的系统，选择互联网前缀
                            (not (gnutls-available-p))))
               (proto (if no-ssl "http" "https"))) ;;Windows系统前缀是http
          (pcase archives
            ('melpa
             `(,(cons "gnu"   (concat proto "://elpa.gnu.org/packages/"))
               ,(cons "melpa" (concat proto "://melpa.org/packages/"))
               ,(cons "org"   (concat proto "://orgmode.org/elpa/"))))
            ('melpa-mirror
             `(,(cons "gnu"   (concat proto "://elpa.gnu.org/packages/"))
               ,(cons "melpa" (concat proto "://www.mirrorservice.org/sites/melpa.org/packages/"))))
            ('emacs-china
             `(,(cons "gnu"   (concat proto "://elpa.emacs-china.org/gnu/"))
               ,(cons "melpa" (concat proto "://elpa.emacs-china.org/melpa/"))
               ,(cons "org"   (concat proto "://elpa.emacs-china.org/org/"))))
            ('netease
             `(,(cons "gnu"   (concat proto "://mirrors.163.com/elpa/gnu/"))
               ,(cons "melpa" (concat proto "://mirrors.163.com/elpa/melpa/"))
               ,(cons "org"   (concat proto "://mirrors.163.com/elpa/org/"))))
            ('tencent
             `(,(cons "gnu"   (concat proto "://mirrors.cloud.tencent.com/elpa/gnu/"))
               ,(cons "melpa" (concat proto "://mirrors.cloud.tencent.com/elpa/melpa/"))
               ,(cons "org"   (concat proto "://mirrors.cloud.tencent.com/elpa/org/"))))
            ('tuna
             `(,(cons "gnu"   (concat proto "://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/"))
               ,(cons "melpa" (concat proto "://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/"))
               ,(cons "org"   (concat proto "://mirrors.tuna.tsinghua.edu.cn/elpa/org/"))))
            (archives
             (error "Unknown archives: `%s'" archives)))))

  (message "Set package archives to `%s'." archives))

(set-package-archives yantree-package-archives)


;; keep the installed packages in .emacs.d
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))


;; Always load newest byte code
(setq load-prefer-newer t)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; 安装'use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; 说出最小的报错信息
(eval-and-compile
  (setq use-package-expand-minimally t)
  (setq use-package-enable-imenu-support t))

(eval-when-compile
  (require 'use-package)
  (require 'bind-key))

;; 取消 C-z C-x C-z 键的绑定
(bind-key "C-z" nil)
(bind-key "C-x C-z" nil)

;;; Libraries
(use-package all-the-icons             :ensure t :defer t)
(use-package async                     :ensure t :defer t)
(use-package diminish                  :ensure t :demand t)
(use-package epl                       :ensure t :defer t)
;; Update GPG keyring for GNU ELPA
;; (use-package gnu-elpa-keyring-update   :ensure t :defer t)
(use-package pkg-info                  :ensure t :defer t)
(use-package pos-tip                   :ensure t :defer t)
(use-package posframe                  :ensure t :defer t)
(use-package dash                      :ensure t :defer t)


(provide 'init-package)
;;; init-package.el ends here
