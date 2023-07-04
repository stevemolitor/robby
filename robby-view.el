;;; robby-view.el  --- mode for viewing OpenAI responses in robby  -*- lexical-binding:t -*-

;;; Commentary:

;; This mode just extends markdown-preview-mode. By making it its own
;; derived mode users can more easily customize its display via
;; `display-buffer-alist`.

(require 'markdown-mode)

;;; Code:

(defvar robby--view-buffer "*robby*" "Buffer to view robby OpenAI responses.")

(define-derived-mode robby-view-mode markdown-view-mode
  "robby"
  "Mode for viewing read-only OpenAI robby responses. Press `q` to quit.")

(defmacro robby--with-robby-view (&rest body)
  `(let ((buf (get-buffer-create robby--view-buffer)))
     (with-current-buffer buf
       (display-buffer buf 'display-buffer-reuse-window)
       (let ((inhibit-read-only t))
         ,@body)
       (robby-view-mode))))

(defun robby--view-message ()
  (interactive)
  (message "%s" (substitute-command-keys "Type \\<markdown-view-mode-map>\\[kill-this-buffer] to delete robby view")))

;;;###autoload
(cl-defun robby-respond-with-robby-view (&key text &allow-other-keys)
  "Show TEXT in robby-view-mode buffer."
  (robby--with-robby-view
   (erase-buffer)
   (insert text))
  (robby--view-message))

(cl-defun robby-respond-in-conversation (&key text &allow-other-keys)
  "Show TEXT in help window, keep minibuffer open."
  (robby--with-robby-view
   (goto-char (point-max))
   (insert text))
  (setq unread-command-events (listify-key-sequence (kbd "M-x robby-conversation RET"))))

(provide 'robby-view)

;; robby-view-mode.el ends here
