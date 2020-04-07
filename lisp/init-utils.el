;;; -*- lexical-binding: t; -*-

;;----------------------------------------------------------------
;; 链接到我的github
(defun yantree/github()
  (interactive)
  (browse-url "https://github.com/YanTree"))


;;----------------------------------------------------------------
;; 浏览当前HTML文件
(defun browse-current-file ()
  "Open the current file as a URL using `browse-url'."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if (and (fboundp 'tramp-tramp-file-p)
             (tramp-tramp-file-p file-name))
        (error "Cannot open tramp file")
      (browse-url (concat "file://" file-name)))))


;;----------------------------------------------------------------
;; start a httpd-server in current directory
(defun httpd-start-here (directory port)
  (interactive (list (read-directory-name "Root directory: " default-directory nil t)
                     (read-number "Port: " 8017)))
  (setq httpd-root directory)
  (setq httpd-port port)
  (httpd-start)
  (browse-url (concat "http://localhost:" (number-to-string port) "/")))


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
;; 删除整个单词
(defun kill-whole-word()
  (interactive)
  (backward-word)
  (kill-word 1))


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
;; HTML cleanup buffer
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
         ("C-c w"                . kill-whole-word)
         ("C-S-d"                . delete-current-line)
         ("ESC ESC c"            . yantree/init-file)
         ("<s-return>"           . eshell)
         ("C-c C-f"              . yantree/copy-file-path)
         ("C-c n"                . cleanup-buffer)
         ("C-h C-f"              . find-function)
         ("C-h C-v"              . find-variable)
         ("C-h C-k"              . find-function-on-key)))


;;----------------------------------------------------------------
;; sdcv 英语查词插件
;; (use-package sdcv
;;   :load-path "site-lisp/extensions/sdcv"
;;   :config
;;   ;; 词典链接 http://download.huzheng.org/zh_CN/
;;   ;; 根据懒猫的配置,下面一行要这样自定义启动目录
;;   (defvar yantree-sdcv-data-dir (file-truename "~/.emacs.d/site-lisp/sdcv-dict"))

;;   (setq sdcv-say-word-p t)               ;;打开语音功能
;;   (setq sdcv-dictionary-simple-list      ;;星际译王屏幕取词词典, 简单, 快速
;;         '("懒虫简明英汉词典"
;;           "懒虫简明汉英词典"
;;           "KDic11万英汉词典"))
;;   (setq sdcv-dictionary-complete-list    ;;星际译王的词典, 完全, 详细
;;         '(
;;           "懒虫简明英汉词典"
;;           "英汉汉英专业词典"
;;           "XDICT英汉辞典"
;;           "stardict1.3英汉辞典"
;;           "WordNet"
;;           "XDICT汉英辞典"
;;           "Jargon"
;;           "懒虫简明汉英词典"
;;           "FOLDOC"
;;           "新世纪英汉科技大词典"
;;           "KDic11万英汉词典"
;;           "朗道汉英字典5.0"
;;           "CDICT5英汉辞典"
;;           "新世纪汉英科技大词典"
;;           "牛津英汉双解美化版"
;;           "21世纪双语科技词典"
;;           "quick_eng-zh_CN"
;;           ))

;;   (setq sdcv-dictionary-data-dir yantree-sdcv-data-dir);;设置词典的路径

;;   :bind ("C-c t" . sdcv-search-pointer+))

(provide 'init-utils)
;;; init-utils.el ends here
