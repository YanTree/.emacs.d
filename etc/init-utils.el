;;; init-utils.el --- Micro utils -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;----------------------------------------------------------------
;; 链接到我的github
(defun yantree/github()
  "Open my github"
  (interactive)
  (browse-url "https://github.com/YanTree"))


;;----------------------------------------------------------------
;; 删除当前文件和buffer
(defun delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (unless (buffer-file-name)
    (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)
    (message "File '%s' successfully removed" buffer-file-name)))


;;----------------------------------------------------------------
;; 标记整段
(defun mark-sentence (&optional arg)
  (interactive "P")
  (backward-sentence)
  (mark-end-of-sentence arg))


;;----------------------------------------------------------------
;; 删除整行
(defun delete-current-line (&optional arg)
  (interactive "p")
  (let ((here (point)))
    (beginning-of-line)
    (kill-line arg)
    (goto-char here)))


;;----------------------------------------------------------------
;; 重命名当前文件和buffer
(defun rename-this-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (progn
      (when (file-exists-p filename)
        (rename-file filename new-name 1))
      (set-visited-file-name new-name)
      (rename-buffer new-name))))


;;----------------------------------------------------------------
;; cleanup buffer
(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer.
Including indent-buffer, which should not be called automatically on save."
  (interactive)
  (untabify-buffer)
  (delete-trailing-whitespace)
  (indent-buffer))


;;----------------------------------------------------------------
;; 定义一个函数用于快速定位到自己的init.el文件
(defun yantree/init-file()
  "opern my init.el file"
  (interactive)
  (find-file "~/.emacs.d/init.el"))


;;----------------------------------------------------------------
;; copy当前文件的路径
(defun yantree/copy-file-path (&optional @dir-path-only-p)
  "Copy the current buffer's file path or dired path to `kill-ring'.
Result is full path."
  (interactive "P")
  (let (($fpath
         (if (string-equal major-mode 'dired-mode)
             (progn
               (let (($result (mapconcat 'identity (dired-get-marked-files) "\n")))
                 (if (equal (length $result) 0)
                     (progn default-directory )
                   (progn $result))))
           (if (buffer-file-name)
               (buffer-file-name)
             (expand-file-name default-directory)))))
    (kill-new
     (if @dir-path-only-p
         (progn
           (message "Directory path copied: 「%s」" (file-name-directory $fpath))
           (file-name-directory $fpath))
       (progn
         (message "File path copied: 「%s」" $fpath)
         $fpath )))))


;;----------------------------------------------------------------
;; 个人的快捷键设置
(use-package yantree
  :bind (([remap just-one-space] . cycle-spacing)
         ("C-S-d"                . delete-current-line)
         ("C-c C-f"              . yantree/copy-file-path)
         ("C-c n"                . cleanup-buffer)
         ("C-h C-f"              . find-function)
         ("C-h C-v"              . find-variable)
         ("C-h C-k"              . find-function-on-key)))


;;----------------------------------------------------------------
;; 有道词典翻译 zh_cn -> en_us
(use-package youdao-dictionary
  :ensure t
  :commands youdao-dictionary-play-voice-of-current-word
  :bind (("C-c t" . YanTree-youdao-dictionary-search-at-point)
         :map youdao-dictionary-mode-map
         ("h" . youdao-dictionary-hydra/body)
         ("?" . youdao-dictionary-hydra/body))
  :init
  (setq url-automatic-caching t
        youdao-dictionary-use-chinese-word-segmentation t) ; 中文分词

  (defun YanTree-youdao-dictionary-search-at-point ()
    "Search word at point and display result with `posframe', `pos-tip', or buffer."
    (interactive)
    (if (display-graphic-p)
        (youdao-dictionary-search-at-point-posframe)
      (youdao-dictionary-search-at-point)))
  :config
  (with-no-warnings
    (defun my-youdao-dictionary--posframe-tip (string)
      "Show STRING using posframe-show."
      (unless (and (require 'posframe nil t) (posframe-workable-p))
        (error "Posframe not workable"))

      (let ((word (youdao-dictionary--region-or-word)))
        (if word
            (progn
              (with-current-buffer (get-buffer-create youdao-dictionary-buffer-name)
                (let ((inhibit-read-only t))
                  (erase-buffer)
                  (youdao-dictionary-mode)
                  (insert (propertize "\n" 'face '(:height 0.5)))
                  (insert string)
                  (insert (propertize "\n" 'face '(:height 0.5)))
                  (set (make-local-variable 'youdao-dictionary-current-buffer-word) word)))
              (posframe-show youdao-dictionary-buffer-name
                             :position (point)
                             :left-fringe 16
                             :right-fringe 16
                             :background-color (face-background 'tooltip nil t)
                             :internal-border-color (face-foreground 'font-lock-comment-face nil t)
                             :internal-border-width 1)
              (unwind-protect
                  (push (read-event) unread-command-events)
                (progn
                  (posframe-hide youdao-dictionary-buffer-name)
                  (other-frame 0))))
          (message "Nothing to look up"))))
    (advice-add #'youdao-dictionary--posframe-tip
                :override #'my-youdao-dictionary--posframe-tip)))


(provide 'init-utils)
;;; init-utils.el ends here
