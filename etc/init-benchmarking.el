;;; init-benchmarking.el --- Measure startup and require times -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(defun yantree/time-subtract-millis (b a)
  (* 1000.0 (float-time (time-subtract b a))))

(defvar yantree/require-times nil
  "A list of (FEATURE LOAD-START-TIME LOAD-DURATION).
LOAD-DURATION is the time taken in milliseconds to load FEATURE.")

(defun yantree/require-times-wrapper (orig feature &rest args)
  "Note in `yantree/require-times' the time taken to require each feature."
  (let* ((already-loaded (memq feature features))
         (require-start-time (and (not already-loaded) (current-time))))
    (prog1
        (apply orig feature args)
      (when (and (not already-loaded) (memq feature features))
        (let ((time (yantree/time-subtract-millis (current-time) require-start-time)))
          (add-to-list 'yantree/require-times
                       (list feature require-start-time time)
                       t))))))

(advice-add 'require :around 'yantree/require-times-wrapper)


(define-derived-mode yantree/require-times-mode tabulated-list-mode "Require-Times"
  "Show times taken to `require' packages."
  (setq tabulated-list-format
        [("Start time (ms)" 20 yantree/require-times-sort-by-start-time-pred)
         ("Feature" 30 t)
         ("Time (ms)" 12 yantree/require-times-sort-by-load-time-pred)])
  (setq tabulated-list-sort-key (cons "Start time (ms)" nil))
  ;; (setq tabulated-list-padding 2)
  (setq tabulated-list-entries #'yantree/require-times-tabulated-list-entries)
  (tabulated-list-init-header)
  (when (fboundp 'tablist-minor-mode)
    (tablist-minor-mode)))

(defun yantree/require-times-sort-by-start-time-pred (entry1 entry2)
  (< (string-to-number (elt (nth 1 entry1) 0))
     (string-to-number (elt (nth 1 entry2) 0))))

(defun yantree/require-times-sort-by-load-time-pred (entry1 entry2)
  (> (string-to-number (elt (nth 1 entry1) 2))
     (string-to-number (elt (nth 1 entry2) 2))))

(defun yantree/require-times-tabulated-list-entries ()
  (cl-loop for (feature start-time millis) in yantree/require-times
           with order = 0
           do (cl-incf order)
           collect (list order
                         (vector
                          (format "%.3f" (yantree/time-subtract-millis start-time before-init-time))
                          (symbol-name feature)
                          (format "%.3f" millis)))))

(defun yantree/require-times ()
  "Show a tabular view of how long various libraries took to load."
  (interactive)
  (with-current-buffer (get-buffer-create "*Require Times*")
    (yantree/require-times-mode)
    (tabulated-list-revert)
    (display-buffer (current-buffer))))

(defun yantree/show-init-info ()
  (if (bound-and-true-p package-alist)
      (message "%d packages loaded in %.2fms"
               (length package-activated-list) (yantree/time-subtract-millis after-init-time before-init-time))
    (if (and (boundp 'straight--profile-cache) (hash-table-p straight--profile-cache))
        (message "%d packages loaded in %.2fms"
                 (hash-table-size straight--profile-cache) (yantree/time-subtract-millis after-init-time before-init-time))
      (message "init completed in %.2fms" (yantree/time-subtract-millis after-init-time before-init-time)))))

(add-hook 'after-init-hook 'yantree/show-init-info)

(provide 'init-benchmarking)
;;; init-benchmarking.el ends here
