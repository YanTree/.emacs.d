;;; -*- lexical-binding: t; -*-

;;----------------------------------------------------------------------------
;; popwin
(use-package popwin
  :ensure t
  :diminish popwin-mode
  :hook (after-init . popwin-mode))


;;----------------------------------------------------------------------------
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


;;----------------------------------------------------------------------------
;; Shift <left> <right> <up> <down> move cursor
(use-package windmove
  :ensure nil
  :hook (after-init . windmove-default-keybindings))


;;----------------------------------------------------------------------------
;; personal configuration
(defun sanityinc/set-tabulated-list-column-width (col-name width)
  "Set any column with name COL-NAME to the given WIDTH."
  (when (> width (length col-name))
    (cl-loop for column across tabulated-list-format
             when (string= col-name (car column))
             do (setf (elt column 1) width))))

(defun sanityinc/maybe-widen-package-menu-columns ()
  "Widen some columns of the package menu table to avoid truncation."
  (when (boundp 'tabulated-list-format)
    (sanityinc/set-tabulated-list-column-width "Version" 13)
    (let ((longest-archive-name (apply 'max (mapcar 'length (mapcar 'car package-archives)))))
      (sanityinc/set-tabulated-list-column-width "Archive" longest-archive-name))))

(add-hook 'package-menu-mode-hook 'sanityinc/maybe-widen-package-menu-columns)

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

(defun sanityinc/toggle-delete-other-windows ()
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

(defun sanityinc/toggle-current-window-dedication ()
  "Toggle whether the current window is dedicated to its current buffer."
  (interactive)
  (let* ((window (selected-window))
         (was-dedicated (window-dedicated-p window)))
    (set-window-dedicated-p window (not was-dedicated))
    (message "Window %sdedicated to %s"
             (if was-dedicated "no longer " "")
             (buffer-name))))


;;----------------------------------------------------------------------------
;; autoload bindings
(use-package windows
  :ensure nil
  ;;;下面快捷键的设置是上面自定义的快捷键,为了lazy-load
  :bind (("C-x 1"      . sanityinc/toggle-delete-other-windows)
         ("C-x |"      . split-window-horizontally-instead)
         ("C-x _"      . split-window-vertically-instead)
         ("C-c <down>" . sanityinc/toggle-current-window-dedication)))


;;----------------------------------------------------------------------------
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

(provide 'init-windows)
;;; init-windows.el ends here
