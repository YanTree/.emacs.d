;;; config/maybe-keybindings.el -*- lexical-binding: t; -*-
;;; Commentary:
;;
;;; Code:


;;;###package `move-dup'
;; Mark lines, then move up/down or duplicate then move up/down
(bind-keys ("M-<up>"     . move-dup-move-lines-up)
           ("M-<down>"   . move-dup-move-lines-down)
           ("C-M-<up>"   . move-dup-duplicate-up)
           ("C-M-<down>" . move-dup-duplicate-down))

;;;###package `consult'
;; Consult provides search and navigation commands based on the Emacs completion
;; function completing-read.
(bind-keys ([remap bookmark-jump]       . consult-bookmark)
           ([remap goto-line]           . consult-goto-line)
           ([remap imenu]               . consult-imenu)
           ([remap Info-search]         . consult-info)
           ([remap locate]              . consult-locate)
           ([remap load-theme]          . consult-themes)
           ([remap man]                 . consult-man)
           ([remap recentf-open-files]  . consult-recent-file)
           ([remap switch-to-buffer]    . consult-buffer)
           ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
           ([remap switch-to-buffer-other-frame]  . consult-buffer-other-frame)
           ([remap yank-pop]           . consult-yank-pop))


(provide 'maybe-keybindings)
