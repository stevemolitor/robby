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

(eval-when-compile
  (require 'cl-generic)
  (require 'cl-lib)
  (require 'cl-macs)
  (require 'cus-edit)
  (require 'json)
  (require 'markdown-mode)
  (require 'map)
  (require 'request)
  (require 'seq)
  (require 'transient))

(require 'robby-curl)
(require 'robby-customization)
(require 'robby-prompts)
(require 'robby-actions)
(require 'robby-run-command)
(require 'robby-define-command)
(require 'robby-keymap)
(require 'robby-commands)
(require 'robby-spinner-mode)
(require 'robby-transients)

(provide 'robby)

;;; robby.el ends here
