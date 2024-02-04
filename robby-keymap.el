;;; robby-keymap.el  --- robby keymaps  -*- lexical-binding:t -*-

;;; Code:

;;;###autoload
(defvar robby-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map "b" 'robby-builder)
    (define-key map "a" 'robby-append-region)
    (define-key map "c" 'robby-save-command)
    (define-key map "d" 'robby-describe-code)
    (define-key map "e" 'robby-name-last-command)
    (define-key map "f" 'robby-fix-code)
    (define-key map "g" 'robby-replace-region)
    (define-key map "j" 'robby-proof-read)
    (define-key map "k" 'robby-kill-last-process)
    (define-key map "l" 'robby-clear-history)
    (define-key map "m" 'robby-message)
    (define-key map "n" 'robby-conversation)
    (define-key map "o" 'robby-add-comment)
    (define-key map "s" 'robby-summarize)
    (define-key map "t" 'robby-write-tests)
    (define-key map "p" 'robby-prepend-region)
    (define-key map "u" 'robby-run-last-command)
    (define-key map "v" 'robby-view)
    (define-key map "w" 'robby-view-from-region)
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
