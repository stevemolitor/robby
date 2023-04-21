;;; robby-transient.el  --- transient menus for Robby  -*- lexical-binding:t -*-

;;; Commentary:

;; Transient menus for Robby, to let user specify OpenAI API options and such.

(require 'transient)
(require 'transient)
(require 'robby-define-command)
(require 'robby-actions)

;;; Code:

(defun robby--respond-to-transient-action (action)
  (let* ((prompt-arg (transient-arg-value "prompt=" (transient-args transient-current-command)))
         (prompt-from-region-p (transient-arg-value "prompt-from-region-p" (transient-args transient-current-command)))
         (prompt-from-region (robby--get-region-or-buffer-text))
         (prompt (cond
                  ((and prompt-arg prompt-from-region-p)
                   (format "%s\n%s" prompt-arg prompt-from-region))
                  (prompt-arg
                   prompt-arg)
                  (prompt-from-region-p
                   prompt-from-region)
                  (t
                   prompt-from-region))))
    (robby--run-command :prompt (lambda (_arg) `(,prompt . ,prompt)) :action action)))

(defmacro robby--define-suffix (name action)
  `(transient-define-suffix ,name ()
     (interactive)
     (robby--respond-to-transient-action #',action)))

(robby--define-suffix robby--message-suffix robby--respond-with-message)
(robby--define-suffix robby--help-window-suffix robby--show-response-in-help-window)
(robby--define-suffix robby--prepend-to-region-suffix robby--prepend-response-to-region)
(robby--define-suffix robby--append-after-region-suffix robby--append-response-after-region)
(robby--define-suffix robby--replace-region-suffix robby--replace-region-with-response)

;;;###autoload (autoload 'robby-chat "robby" "Robby chat transient command" t)
(transient-define-prefix robby-chat ()
  "Select Chat API Options"
  ["Prompt Sources"
   ("-p" "prompt" "prompt=" :prompt "OpenAI prompt or prompt prefix: ")
   ("-r" "prompt from region or buffer" "prompt-from-region-p")]
  ["Actions"
   ("m" "respond with message" robby--message-suffix)
   ("h" "show response in help window" robby--help-window-suffix)
   ("p" "prepend response to region" robby--prepend-to-region-suffix)
   ("a" "append response after region" robby--append-after-region-suffix)
   ("r" "replace region with response" robby--replace-region-suffix)])

(provide 'robby-transient)
