;;; config/maybe-keybindings.el -*- lexical-binding: t; -*-
;;; Commentary:
;;
;;; Code:


;;;###package `move-dup'
;; Mark lines, then move up/down or duplicate then move up/down
(global-set-key (kbd "M-<up>")     'move-dup-move-lines-up)
(global-set-key (kbd "M-<down>")   'move-dup-move-lines-down)
(global-set-key (kbd "C-M-<up>")   'move-dup-duplicate-up)
(global-set-key (kbd "C-M-<down>") 'move-dup-duplicate-down)


(provide 'maybe-keybindings)
