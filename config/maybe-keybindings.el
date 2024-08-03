;;; config/maybe-keybindings.el -*- lexical-binding: t; -*-
;;; Commentary:
;;
;;; Code:


;; Bind keys
(bind-keys ("C-h C-f" . find-function)
           ("C-h C-v" . find-variable )
           ("C-h C-k" . find-function-on-key)
;;;###package `move-dup'
;; Mark lines, then move up/down or duplicate then move up/down
           ("M-<up>"     . move-dup-move-lines-up)
           ("M-<down>"   . move-dup-move-lines-down)
           ("C-M-<up>"   . move-dup-duplicate-up)
           ("C-M-<down>" . move-dup-duplicate-down)
;;;###package `consult'
;; Consult provides search and navigation commands based on the Emacs completion
;; function completing-read.
  ; `yank-pop'            ->  `consult-yank-pop'
  ; `Info-search'         ->  `consult-info'
  ; `locate'              ->  `consult-locate'
  ; `load-theme'          ->  `consult-themes'
  ; `man'                 ->  `consult-man'
  ; `recentf-open-files'  ->  `consult-recent-file'
           ("C-x b"   . consult-buffer)
           ("C-x 4 b" . consult-buffer-other-window)
           ("C-x 5 b" . consult-buffer-other-frame)
           ("C-x r b" . consult-bookmark)
           ("M-g g"   . consult-goto-line)
           ("M-g i"   . consult-imenu))


(provide 'maybe-keybindings)
