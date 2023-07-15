;;; robby-keymap.el  --- robby keymap  -*- lexical-binding:t -*-

;;; Commentary:

;; Robby keymap.

;;; Code:

;;;###autoload
(defvar robby-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map "r" 'robby)
    (define-key map "a" 'robby-append-region)
    (define-key map "g" 'robby-replace-region)
    (define-key map "k" 'robby-kill-last-process)
    (define-key map "l" 'robby-clear-history)
    (define-key map "m" 'robby-message)
    (define-key map "n" 'robby-conversation)
    (define-key map "p" 'robby-prepend-region)
    (define-key map "v" 'robby-view)
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

;;; robby-keymap.el ends here
