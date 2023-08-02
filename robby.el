;;; robby --- Extensible OpenAI Integration for Emacs  -*- lexical-binding:t -*-

;; Version: 0.0.1
;; Package-Requires: ((emacs "28.1") (request "0.3.0") (ert-async "0.1.2") (spinner "1.7.2") (transient "0.4.1") (markdown-mode "2.5")
;; Homepage: https://github.com/stevemolitor/robby

;;; Commentary

;; Robby provides an extensible Emacs interface to OpenAI. It provides
;; basic commands to use OpenAI with regions, the minibuffer, and help
;; windows. It also provides a mechanism for defining your own
;; interactive AI commands, and to save the last executed command as a
;; reusable custom AI command. See the README for more details.

;;; check for missing requires on compilation
(eval-when-compile
  (require 'cl)
  (require 'cl-generic)
  (require 'cl-lib)
  (require 'cl-macs)
  (require 'cus-edit)
  (require 'diff)
  (require 'files)
  (require 'json)
  (require 'markdown-mode)
  (require 'map)
  (require 'seq)
  (require 'spinner)
  (require 'transient))

;;; require files with exported / autoloaded commands or functions
(require 'robby-commands)
(require 'robby-customization)
(require 'robby-keymap)
(require 'robby-spinner)
(require 'robby-transients)

;;; keymap

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

(provide 'robby)

;;; robby.el ends here
