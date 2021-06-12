;;; init-window.el --- Window setting -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;----------------------------------------------------------------
;; shackle Enforce popup rules
(use-package shackle
  :ensure t
  :functions org-switch-to-buffer-other-window
  :commands shackle-display-buffer
  :hook (after-init . shackle-mode)
  :init
  (defvar shackle--popup-window-list nil) ;; all popup windows
  (defvar-local shackle--current-popup-window nil) ;; current popup window
  :config
  (put 'shackle--current-popup-window 'permanent-local t)
  (eval-and-compile
    ;; Add keyword: `autoclose'
    (defun shackle-display-buffer-hack (fn buffer alist plist)
      (let ((window (funcall fn buffer alist plist)))
        (setq shackle--current-popup-window window)

        (when (plist-get plist :autoclose)
          (push (cons window buffer) shackle--popup-window-list))
        window))

    (defun shackle-close-popup-window-hack (&rest _)
      "Close current popup window via `C-g'."
      (setq shackle--popup-window-list
            (cl-loop for (window . buffer) in shackle--popup-window-list
                     if (and (window-live-p window)
                             (equal (window-buffer window) buffer))
                     collect (cons window buffer)))
      ;; `C-g' can deactivate region
      (when (and (called-interactively-p 'interactive)
                 (not (region-active-p)))
        (let (window buffer)
          (if (one-window-p)
              (progn
                (setq window (selected-window))
                (when (equal (buffer-local-value 'shackle--current-popup-window
                                                 (window-buffer window))
                             window)
                  (winner-undo)))
            (setq window (caar shackle--popup-window-list))
            (setq buffer (cdar shackle--popup-window-list))
            (when (and (window-live-p window)
                       (equal (window-buffer window) buffer))
              (delete-window window)
              (pop shackle--popup-window-list))))))

    (advice-add #'keyboard-quit :before #'shackle-close-popup-window-hack)
    (advice-add #'shackle-display-buffer :around #'shackle-display-buffer-hack))

  ;; HACK: compatibility issuw with `org-switch-to-buffer-other-window'
  (advice-add #'org-switch-to-buffer-other-window :override #'switch-to-buffer-other-window)

  ;; rules
  (setq shackle-default-size 0.4
        shackle-default-alignment 'below
        shackle-default-rule nil
        shackle-rules
        '(("*Help*" :select t :size 0.3 :align 'below :autoclose t)
          ("*Apropos*" :select t :size 0.3 :align 'below :autoclose t)
          ("*compilation*" :select t :size 0.3 :align 'below :autoclose t)
          ("*Completions*" :size 0.3 :align 'below :autoclose t)
          ("*Pp Eval Output*" :size 15 :align 'below :autoclose t)
          ("*ert*" :align 'below :autoclose t)
          ("*Backtrace*" :select t :size 15 :align 'below)
          ("*Warnings*" :size 0.3 :align 'below :autoclose t)
          ("*Messages*" :size 0.3 :align 'below :autoclose t)
          ("^\\*.*Shell Command.*\\*$" :regexp t :size 0.3 :align 'below :autoclose t)
          ("\\*[Wo]*Man.*\\*" :regexp t :select t :align 'below :autoclose t)
          ("*Calendar*" :select t :size 0.3 :align 'below)
          ("*quickrun*" :select t :size 15 :align 'below)
          ("*tldr*" :align 'below :autoclose t)
          ("^\\*elfeed-entry" :regexp t :size 0.7 :align 'below :autoclose t)
          ("*lsp-help*" :size 0.3 :align 'below :autoclose t)
          ("*lsp session*" :size 0.4 :align 'below :autoclose t)
          (" *Org todo*" :select t :size 4 :align 'below :autoclose t)
          ("*gud-debug*" :select t :size 0.4 :align 'below :autoclose t)
          ("*Gofmt Errors*" :select t :size 0.3 :align 'below :autoclose t)
          ("^\\*macro expansion\\**" :regexp t :size 0.4 :align 'below)
          (" *Install vterm" :size 0.3 :align 'below)

          (Buffer-menu-mode :select t :size 20 :align 'below :autoclose t)
          (comint-mode :align 'below)
          (helpful-mode :select t :size 0.3 :align 'below :autoclose t)
          (list-environment-mode :select t :size 0.3 :align 'below :autoclose t)
          (profiler-report-mode :select t :size 0.5 :align 'right)
          (tabulated-list-mode :align 'below))))


;;----------------------------------------------------------------
;; Navigate window layouts with "C-c <left>" and "C-c <right>"
(use-package winner
  :ensure nil
  :hook (after-init . winner-mode)
  :init
  (setq winner-boring-buffers
        '("*Completions*"
          "*Compile-Log*"
          "*inferior-lisp*"
          "*Fuzzy Completions*"
          "*Apropos*"
          "*Help*"
          "*cvs*"
          "*Buffer List*"
          "*Ibuffer*"
          "*esh command on file*")))


;;----------------------------------------------------------------
;; Shift <left> <right> <up> <down> move cursor
(use-package windmove
  :ensure nil
  :hook (after-init . windmove-default-keybindings))


;;----------------------------------------------------------------
;; 个人设置，每当打开一个新的 buffer 时，光标跟随移动
(defun yantree/set-tabulated-list-column-width (col-name width)
  "Set any column with name COL-NAME to the given WIDTH."
  (when (> width (length col-name))
    (cl-loop for column across tabulated-list-format
             when (string= col-name (car column))
             do (setf (elt column 1) width))))

(defun yantree/maybe-widen-package-menu-columns ()
  "Widen some columns of the package menu table to avoid truncation."
  (when (boundp 'tabulated-list-format)
    (yantree/set-tabulated-list-column-width "Version" 13)
    (let ((longest-archive-name (apply 'max (mapcar 'length (mapcar 'car package-archives)))))
      (yantree/set-tabulated-list-column-width "Archive" longest-archive-name))))

(add-hook 'package-menu-mode-hook 'yantree/maybe-widen-package-menu-columns)

;;;When splitting window, show (other-buffer) in the new window
(defun split-window-func-with-other-buffer (split-function)
  (lambda (&optional arg)
    "Split this window and switch to the new window unless ARG is provided."
    (interactive "P")
    (funcall split-function)
    (let ((target-window (next-window)))
      (set-window-buffer target-window (other-buffer))
      (unless arg
        (select-window target-window)))))

(global-set-key (kbd "C-x 2") (split-window-func-with-other-buffer 'split-window-vertically))
(global-set-key (kbd "C-x 3") (split-window-func-with-other-buffer 'split-window-horizontally))

(defun yantree/toggle-delete-other-windows ()
  "Delete other windows in frame if any, or restore previous window config."
  (interactive)
  (if (and winner-mode
           (equal (selected-window) (next-window)))
      (winner-undo)
    (delete-other-windows)))

;; Rearrange split windows
(defun split-window-horizontally-instead ()
  "Kill any other windows and re-split such that the current window is on the top half of the frame."
  (interactive)
  (let ((other-buffer (and (next-window) (window-buffer (next-window)))))
    (delete-other-windows)
    (split-window-horizontally)
    (when other-buffer
      (set-window-buffer (next-window) other-buffer))))

(defun split-window-vertically-instead ()
  "Kill any other windows and re-split such that the current window is on the left half of the frame."
  (interactive)
  (let ((other-buffer (and (next-window) (window-buffer (next-window)))))
    (delete-other-windows)
    (split-window-vertically)
    (when other-buffer
      (set-window-buffer (next-window) other-buffer))))

(defun yantree/toggle-current-window-dedication ()
  "Toggle whether the current window is dedicated to its current buffer."
  (interactive)
  (let* ((window (selected-window))
         (was-dedicated (window-dedicated-p window)))
    (set-window-dedicated-p window (not was-dedicated))
    (message "Window %sdedicated to %s"
             (if was-dedicated "no longer " "")
             (buffer-name))))


;;----------------------------------------------------------------
;; autoload bindings
(use-package windows
  :ensure nil
  ;;;下面快捷键的设置是上面自定义的快捷键,为了lazy-load
  :bind (("C-x 1"      . yantree/toggle-delete-other-windows)
         ("C-x |"      . split-window-horizontally-instead)
         ("C-x _"      . split-window-vertically-instead)
         ("C-c <down>" . yantree/toggle-current-window-dedication)))


;;----------------------------------------------------------------
;; ace-window(快速切换窗口)
(use-package ace-window
  :ensure t
  :bind ([remap other-window] . ace-window)
  :init
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)) ;; 用字母代替数字 1-9 ,数字的键程有点远
  :custom-face
  (aw-leading-char-face ((t (:inherit font-lock-keyword-face :bold t :height 6.0))))
  ;; 在modeline里显示window的个数
  ;; (aw-mode-line-face ((t (:inherit mode-line-emphasis :bold t))))
  )

(provide 'init-window)
;;; init-window.el ends here
