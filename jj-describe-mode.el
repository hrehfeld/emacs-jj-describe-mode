;;; jj-describe-mode.el --- Major mode for jujutsu change descriptions -*- lexical-binding: t; -*-

;; Author: Hauke Rehfeld <emacs@haukerehfeld.de>
;; Version: 0.1
;; Keywords: convenience
;; Package-Requires: ((emacs "24.3"))
;; URL: https://github.com/hrehfeld/emacs-jj-describe-mode

;;; Commentary:

;; This package provides a major mode for editing temporary files
;; created by jj describe or the like.

;;; Code:

(defvar jj-describe-mode-hook nil
  "Hook run when entering `jj-describe-mode'.")

(defcustom jj-describe-mode-temp-dirs '("/tmp/")
  "List of directories considered temporary."
  :type '(repeat directory)
  :group 'jj-describe-mode)

(defcustom jj-describe-mode-info-functions '(jj-describe-mode-insert-status)
  "List of functions to call for inserting additional info into the buffer."
  :type '(repeat function)
  :group 'jj-describe-mode)

(defvar jj-describe-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'jj-describe-mode-finish)
    map)
  "Keymap for `jj-describe-mode'.")

(defun jj-describe-mode-finish ()
  "Finish editing the change description."
  (interactive)
  (save-buffer)
  (kill-buffer))

(defun jj-describe-mode-in-temp-dir-p ()
  "Check if `default-directory' is in a temporary directory."
  (let ((normalized-dirs (mapcar #'directory-file-name jj-describe-mode-temp-dirs)))
    (seq-some (lambda (dir) (file-in-directory-p default-directory dir)) normalized-dirs)))

(defun jj-describe-mode-get-repo-root ()
  (or (locate-dominating-file default-directory ".jj")
      (locate-dominating-file (buffer-file-name) ".jj")))

(defun jj-describe-mode-insert-status ()
  "Insert the output of `jj status` at the end of the buffer and comment it out."
  (let ((root (jj-describe-mode-get-repo-root))
        (point (point)))
    (when root
      (let ((output (shell-command-to-string "jj status")))
        (if (string-empty-p output)
            (user-error "Failed to retrieve status.")
          (let ((end-pos (point-max)))
            (goto-char end-pos)
            (insert output)
            (comment-region end-pos (point-max))
            (goto-char point)))))))

(defun jj-describe-mode-insert-info ()
  "Centralized function to insert additional info comments into the buffer."
  (run-hooks 'jj-describe-mode-info-functions))

(defun jj-describe-mode-setup ()
  "Setup function for `jj-describe-mode'."
  (setq-local comment-start "JJ: ")
  (setq-local comment-end "")
  (setq-local comment-start-skip "JJ: *")
  (add-hook 'jj-describe-mode-hook #'jj-describe-mode-insert-info nil t))

;;;###autoload
(define-derived-mode jj-describe-mode text-mode "JJ-Describe"
  "Major mode for editing change description files."
  :keymap jj-describe-mode-map
  (jj-describe-mode-setup))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\CHANGE-" . jj-describe-mode))

(provide 'jj-describe-mode)

;;; jj-describe-mode.el ends here
