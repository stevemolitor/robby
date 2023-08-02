;;; robby-actions.el  --- robby OpenAI API response actions  -*- lexical-binding:t -*-

;;; Commentary:

;; Functions to handle OpenAI responses.  These functions can be used
;; as the `:action' parameter when defining custom robby commands via
;; `robby-define-command'.

(require 'diff)
(require 'robby-utils)

;;; Code:

;;; message actions
(cl-defun robby-respond-with-message (&key text &allow-other-keys)
  "Show TEXT in minibuffer message."
  (message "")                          ;; clear any end of line from a previous message
  (message (robby--format-message-text text)))

;;; region actions
(cl-defun robby-prepend-response-to-region (&key text beg chars-processed &allow-other-keys)
  "Prepend AI response to region, or buffer if no selected region."
  (when (eq chars-processed 0)
    (goto-char beg)
    (insert "\n"))
  (goto-char (+ beg chars-processed))
  (insert (format "%s" text)))

(cl-defun robby-append-response-to-region (&key text end chars-processed completep &allow-other-keys)
  "Append AI response to region, or buffer if no selected region."
  (when (eq chars-processed 0)
    (goto-char end)
    (insert "\n"))
  (goto-char (+ 1 end chars-processed))
  (insert (format "%s" text)))

(defvar-local robby--old-temp-buffer nil)
(defvar-local robby--new-temp-buffer nil)

(cl-defun robby-replace-region-with-response (&key arg text beg end chars-processed completep &allow-other-keys)
  "Replace region with AI response, or buffer if no selected region."

  ;; confirm before replacing
  (when arg
    ;; first time:  capture current region text in old temp buffer
    (when (eq chars-processed 0)
      (setq robby--old-temp-buffer (generate-new-buffer (format "*robby-old-temp-buffer--%s*" (buffer-name))))
      (setq robby--new-temp-buffer (generate-new-buffer (format "*robby-new-temp-buffer--%s*" (buffer-name))))
      (let ((old-text (buffer-substring beg end)))
        (with-current-buffer robby--old-temp-buffer
          (insert old-text))))

    ;; every time: insert new text received into new temp buffer
    (with-current-buffer robby--new-temp-buffer
      (goto-char (+ beg chars-processed))
      (insert text))

    ;; last time: show diff, and if confirmed apply changes
    (when completep
      (let ((diff-buf (get-buffer-create "*robby-diff*")))
        (unwind-protect
            (progn
              (display-buffer
               (diff-no-select robby--old-temp-buffer robby--new-temp-buffer nil t diff-buf))
              (let ((apply-changes-p (y-or-n-p "Apply changes?")))
                (when apply-changes-p
                  (delete-region beg end)
                  (goto-char beg)
                  (insert-buffer-substring robby--new-temp-buffer))))
          (kill-buffer robby--old-temp-buffer)
          (kill-buffer robby--new-temp-buffer)
          (with-current-buffer diff-buf
            (kill-buffer-and-window))
          (message "")))))

  ;; replace without confirming
  (when (not arg)
    (if (eq chars-processed 0)
        (delete-region beg end))
    (goto-char (+ beg chars-processed))
    (insert text)))

;;; robby-view
(defvar robby--view-buffer "*robby*" "Buffer to view robby OpenAI responses.")

(define-derived-mode robby-view-mode markdown-view-mode
  "robby"
  "Mode for viewing read-only OpenAI robby responses. Press `q` to quit.")

(defmacro robby--with-robby-view (&rest body)
  ;; TODO handle case when view buffer has no window
  `(let ((buf (get-buffer-create robby--view-buffer)))
     (with-current-buffer buf
       (if (not (window-live-p (get-buffer-window buf)))
           (display-buffer buf))
       (when (eq (point-max) 1)
         ;; (display-buffer buf 'display-buffer-reuse-window)
         (robby-view-mode))
       (let ((inhibit-read-only t))
         ,@body))))

(cl-defun robby-respond-with-robby-view (&key text completep &allow-other-keys)
  "Show TEXT in robby-view-mode buffer."
  (robby--with-robby-view
   (goto-char (point-max))
   (insert text)
   (when (eq completep t)
     (insert "\n___\n")
     (message "%s" (substitute-command-keys "Type \\<markdown-view-mode-map>\\[kill-this-buffer] to delete robby view")))))

(cl-defun robby-respond-in-conversation (&key text prompt &allow-other-keys)
  "Show TEXT in help window, keep minibuffer open."
  (robby--with-robby-view
   (goto-char (point-max))
   (insert "> " prompt "\n\n")
   (insert text "\n\n"))
  (setq unread-command-events (listify-key-sequence (kbd "M-x robby-conversation RET"))))

(provide 'robby-actions)

;;; robby-actions.el ends here

