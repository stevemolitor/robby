;;; robby-mode.el  --- robby-mode minor mode  -*- lexical-binding:t -*-

;;; Commentary:

;; Provides robby-minor-mode. This mode binds the default robby keybindings,
;; and adds a robby spinner lighter.

;;; Code:

(require 'robby-commands)
(require 'robby-example-commands)
(require 'robby-process)
(require 'robby-spinner)

;;;###autoload
(defvar robby-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map "a" 'robby-append-region)
    (define-key map "b" 'robby-builder)
    (define-key map "e" 'robby-example-commands)
    (define-key map "i" 'robby-insert-last-command)
    (define-key map "k" 'robby-kill-last-process)
    (define-key map "l" 'robby-clear-history)
    (define-key map "m" 'robby-message)
    (define-key map "o" 'robby-api-options)
    (define-key map "p" 'robby-prepend-region)
    (define-key map "r" 'robby-replace-region)
    (define-key map "u" 'robby-run-last-command)
    (define-key map "v" 'robby-view)
    (define-key map "w" 'robby-view-from-region)

    (define-key map "fd" 'robby-describe-code)
    (define-key map "ff" 'robby-fix-code)
    (define-key map "fg" 'robby-git-commit-message)
    (define-key map "fo" 'robby-add-comment)
    (define-key map "ft" 'robby-write-tests)
    (define-key map "fs" 'robby-summarize)
    (define-key map "fx" 'robby-proof-read)

    map)
  "Robby command map.")

;;;###autoload
(defvar robby-keymap-prefix (kbd "C-c C-r"))

;;;###autoload
(defvar robby-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map robby-keymap-prefix robby-command-map)
    map))

(defun robby--kill-robby-process ()
  "Kill robby process associated with the current buffer.

Silently kill any robby process associated with the current
buffer."
  (when (and (boundp 'robby-mode) robby-mode)
    (robby-kill-last-process t)))

;;;###autoload
(define-minor-mode robby-mode
  "Minor mode for running robby commands."
  :global t
  :lighter (:eval (robby-spinner-modeline))
  :keymap robby-mode-map
  :group 'robby
  ;; add robby-kill-robby-process to kill-buffer-hook when entering robby-mode, remove when leaving
  (if robby-mode
      (add-hook 'kill-buffer-hook #'robby--kill-robby-process)
    (remove-hook 'kill-buffer-hook #'robby--kill-robby-process)))

(provide 'robby-mode)

;;; robby-mode.el ends here
