;;; robby --- Extensible OpenAI Integration for Emacs  -*- lexical-binding:t -*-

;; Version: 0.0.1
;; Package-Requires: ((emacs "28.1") (request "0.3.0") (ert-async "0.1.2") (spinner "1.7.2"))
;; Homepage: https://github.com/stevemolitor/robby

;;; Commentary

;; Robby provides an extensible Emacs interface to OpenAI. It provides
;; basic commands to use OpenAI with regions, the minibuffer, and help
;; windows. It also provides a mechanism for defining your own
;; interactive AI commands, and to save the last executed command as a
;; reusable custom AI command. See the README for more details.

(require 'cl-lib)
(require 'json)
(require 'request)

(require 'robby-customization)
(require 'robby-logging)
(require 'robby-request)
(require 'robby-prompts)
(require 'robby-actions)
(require 'robby-define-command)
(require 'robby-keymap)
(require 'robby-commands)
(require 'robby-history)
(require 'robby-mode)
(require 'robby-transient)

(provide 'robby)

;;; robby.el ends here
