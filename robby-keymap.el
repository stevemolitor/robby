;;; robby-keymap.el  --- robby keymap  -*- lexical-binding:t -*-

;;; Commentary:

;; Robby keymap.

;;; Code:

(defvar robby-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map "m" 'robby-message)
    (define-key map "p" 'robby-prepend-region)
    (define-key map "a" 'robby-append-region)
    (define-key map "r" 'robby-replace-region)
    (define-key map "h" 'robby-help-window)
    (define-key map "l" 'robby-clear-history)
    (define-key map "k" 'robby-kill-last-request)
    (define-key map "c" 'robby-chat)
    map)
  "Robby command map.")

(defvar robby-keymap-prefix (kbd "C-c C-r"))

(defvar robby-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map robby-keymap-prefix robby-command-map)
    map))

(provide 'robby-keymap)

;;; robby-keymap.el ends here
