;;; robby-actions.el  --- robby OpenAI API response actions  -*- lexical-binding:t -*-

;;; Commentary:

;; Functions to handle OpenAI responses.  These functions can be used
;; as the `:action' parameter when defining custom robby commands via
;; `robby-define-command'.

;;; Code:
(require 'diff)
(require 'markdown-mode)

(require 'robby-utils)

;;; message actions
(cl-defun robby-respond-with-message (&key text &allow-other-keys)
  "Show TEXT in minibuffer message."
  (message "") ;; clear any end of line from a previous message
  (message (robby--format-message-text text)))

;;; region actions
(cl-defun robby-prepend-response-to-region (&key text beg chars-processed &allow-other-keys)
  "Insert TEXT before point at BEG + CHARS-PROCESSED.

Use to prepend possibly chunked AI response to region, or insert
at point if no selected region."
  (when (eq chars-processed 0)
    (goto-char beg)
    (insert "\n"))
  (goto-char (+ beg chars-processed))
  (insert (format "%s" text)))

(cl-defun robby-append-response-to-region (&key text end chars-processed &allow-other-keys)
  "Insert TEXT after point at END + CHARS-PROCESSED.

Use to append possibly chunked AI response to region, or insert
at point if no selected region.

Handles proper formatting by inserting a newline or extra
character if necessary."
  (when (eq chars-processed 0)
    (goto-char end)
    (insert "\n"))
  (goto-char (+ 1 end chars-processed))
  (insert (format "%s" text)))

(defvar-local robby--old-temp-buffer nil)
(defvar-local robby--new-temp-buffer nil)

(cl-defun robby-replace-region-with-response (&key arg text beg end chars-processed completep &allow-other-keys)
  "Replace region with AI response, or insert at point no selected region.

Replaces region between BEG + CHARS-PROCESSED and END with TEXT.
TEXT may be part of a chunked response.

COMPLETEP indicates whether this is the last chunk. If COMPLETEP
is t and ARG is not nil, show a diff buffer of the changes and
ask the user for confirmation before applying."
  ;; confirm before replacing
  (when arg
    ;; first time: capture current region text in old temp buffer
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
               (diff-no-select robby--old-temp-buffer robby--new-temp-buffer "" t diff-buf))
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
        (delete-region beg (min end (point-max))))
    (goto-char (+ beg chars-processed))
    (insert text)))

;;; robby-chat

(defvar robby-chat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" 'kill-this-buffer)
    (define-key map "v" 'robby-chat)
    map))

(define-derived-mode robby-chat-mode markdown-view-mode "robby"
  "Mode for viewing read-only OpenAI robby responses."
  (use-local-map robby-chat-mode-map))

(defconst robby--end-view-message "\n___\n")

(cl-defun robby--show-robby-chat (&key show-prompt-p chars-processed prompt text completep response-buffer &allow-other-keys)
  "Insert PROMPT followed by TEXT in `robby-chat-mode' buffer RESPONSE-BUFFER.

When SHOW-PROMPT-P it t also insert PROMPT before TEXT. TEXT may
be part of a chunked response.

When COMPLETEP is t show a message to the user explaining how to
continue the conversation or quit.

CHARS-PROCESSED indicates the number of characters already
processed in a chunked response. It is used to determine if this
is the first chunk, and if so display the RESPONSE-BUFFER."
  (with-current-buffer response-buffer
    (when (and (eq chars-processed 0) (not (window-live-p (get-buffer-window))))
      (display-buffer (current-buffer) '(display-buffer-reuse-window . ((dedicated . t) (body-function . select-window)))))
    (when (eq (point-max) 1)
      (robby-chat-mode))
    (goto-char (point-max))
    (let ((inhibit-read-only t))
      (when (and show-prompt-p (zerop chars-processed))
        (insert "> " prompt "\n\n"))
      (insert text)
      (when (eq completep t)
        (insert robby--end-view-message)
        (message "%s" (substitute-command-keys "Type \\<robby-chat-mode-map>\\[robby-chat] to continue the conversation, or \\<robby-chat-mode-map>\\[kill-this-buffer] to stop and kill this buffer."))))))

(cl-defun robby-respond-with-robby-chat (&key chars-processed prompt text completep response-buffer &allow-other-keys)
  "Show PROMPT and TEXT in `robby-chat-mode' buffer RESPONSE-BUFFER.

When COMPLETEP is t show a message to the user explaining how to
continue the conversation or quit.

CHARS-PROCESSED indicates the number of characters already
processed in a chunked response. It is used to determine if this
is the first chunk, and if so display the RESPONSE-BUFFER."
  (robby--show-robby-chat :show-prompt-p t :chars-processed chars-processed :prompt prompt :text text :completep completep :response-buffer response-buffer))

(cl-defun robby-respond-with-robby-chat-without-prompt (&key chars-processed text completep response-buffer &allow-other-keys)
  "Show TEXT in `robby-chat-mode' buffer RESPONSE-BUFFER.

When COMPLETEP is t show a message to the user explaining how to
continue the conversation or quit.

CHARS-PROCESSED indicates the number of characters already
processed in a chunked response. It is used to determine if this
is the first chunk, and if so display the RESPONSE-BUFFER."
  (robby--show-robby-chat :show-prompt-p nil :chars-processed chars-processed :text text :completep completep :response-buffer response-buffer))

(provide 'robby-actions)

;;; robby-actions.el ends here

