;;; robby-keymap.el  --- robby keymaps  -*- lexical-binding:t -*-

;;; Code:

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
(defvar robby-spinner-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map robby-keymap-prefix robby-command-map)
    map))

(provide 'robby-keymap)

;; robby-keymap.el ends here
