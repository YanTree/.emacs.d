;;; -*- lexical-binding: t; -*-

;;----------------------------------------------------------------
;; dired
(use-package dired
  :ensure nil
  :defer t
  :bind (:map dired-mode-map
              ("RET"      . dired-find-alternate-file))
  :config
  ;; 避免打开过多的 dried buffer
  (put 'dired-find-alternate-file 'disabled nil)

  ;; Quickly copy/move file in Dired
  (setq dired-dwim-target t)

  (setq dired-recursive-deletes (quote always)     ;;递归删除文件
        dired-recursive-copies (quote always))     ;;递归复制文件

  ;; Move files to trash when deleting
  (setq delete-by-moving-to-trash t)

  ;; Prefer g-prefixed coreutils version of standard utilities when available
  (let ((gls (executable-find "gls")))
    (when gls (setq insert-directory-program gls)))

  ;; Colourful dired
  (use-package diredfl
    :ensure t
    :config
    (diredfl-global-mode 1))

  ;; show icons
  (use-package all-the-icons-dired
    :ensure t
    :defer t
    :diminish
    :custom-face
    (all-the-icons-dired-dir-face ((t (:foreground nil))))
    :hook (dired-mode . all-the-icons-dired-mode)
    :config
    (defun yantree-all-the-icons-dired--display ()
      "Display the icons of files without colors in a dired buffer."
      ;; Don't display icons after dired commands (e.g insert-subdir, create-directory)
      ;; @see https://github.com/jtbm37/all-the-icons-dired/issues/11
      (all-the-icons-dired--reset)

      (when (and (not all-the-icons-dired-displayed) dired-subdir-alist)
        (setq-local all-the-icons-dired-displayed t)
        (let ((inhibit-read-only t)
              (remote-p (and (fboundp 'tramp-tramp-file-p)
                             (tramp-tramp-file-p default-directory))))
          (save-excursion
            ;; TRICK: Use TAB to align icons
            (setq-local tab-width 1)
            (goto-char (point-min))
            (while (not (eobp))
              (when (dired-move-to-filename nil)
                (insert " ")
                (let ((file (dired-get-filename 'verbatim t)))
                  (unless (member file '("." ".."))
                    (let ((filename (file-local-name (dired-get-filename nil t))))
                      (if (file-directory-p filename)
                          (let ((icon (cond
                                       (remote-p
                                        (all-the-icons-octicon "file-directory"
                                                               :v-adjust all-the-icons-dired-v-adjust
                                                               :face 'all-the-icons-dired-dir-face))
                                       ((file-symlink-p filename)
                                        (all-the-icons-octicon "file-symlink-directory"
                                                               :v-adjust all-the-icons-dired-v-adjust
                                                               :face 'all-the-icons-dired-dir-face))
                                       ((all-the-icons-dir-is-submodule filename)
                                        (all-the-icons-octicon "file-submodule"
                                                               :v-adjust all-the-icons-dired-v-adjust
                                                               :face 'all-the-icons-dired-dir-face))
                                       ((file-exists-p (format "%s/.git" filename))
                                        (all-the-icons-octicon "repo"
                                                               :height 1.1
                                                               :v-adjust all-the-icons-dired-v-adjust
                                                               :face 'all-the-icons-dired-dir-face))
                                       (t (let ((matcher (all-the-icons-match-to-alist
                                                          file all-the-icons-dir-icon-alist)))
                                            (apply (car matcher)
                                                   (list (cadr matcher)
                                                         :face 'all-the-icons-dired-dir-face
                                                         :v-adjust all-the-icons-dired-v-adjust)))))))
                            (insert icon))
                        (insert (all-the-icons-icon-for-file file :v-adjust all-the-icons-dired-v-adjust))))
                    (insert "\t"))))
              (forward-line 1))))))
    (advice-add #'all-the-icons-dired--display :override #'yantree-all-the-icons-dired--display)

    ;; TRICK: The buffer isn't refreshed after some operations due to the TAB
    ;; before the file name. Refresh it by force.
    (advice-add #'dired-do-rename :after #'dired-revert)
    (advice-add #'dired-do-delete :after #'dired-revert)
    (advice-add #'dired-do-flagged-delete :after #'dired-revert)
    )

  ;; Extra Dired functionality
  (use-package dired-x   :ensure nil  :after dired))

(provide 'init-dired)
;;; init-dired.el ends here
