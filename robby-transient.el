;;; robby-transient.el  --- transient menus for Robby  -*- lexical-binding:t -*-

;;; Commentary:

;; Transient menus for Robby, to let user specify OpenAI API options and such.

(require 'transient)
(require 'cl)

(require 'robby-define-command)
(require 'robby-actions)

;;; Code:

(defun robby--get-transient-options ()
  (let ((options '())
        (model (transient-arg-value "model=" (transient-args transient-current-command)))
        (max-tokens (transient-arg-value "max-tokens=" (transient-args transient-current-command))))
    (when model
      (setq options (plist-put options :model model)))
    (when max-tokens
      (setq options (plist-put options :max-tokens (string-to-number max-tokens))))
    options))

(defun robby--respond-to-transient-action (action)
  (let* ((model (transient-arg-value "model=" (transient-args transient-current-command)))
         (max-tokens (transient-arg-value "max-tokens=" (transient-args transient-current-command)))
         (output-buffer (transient-arg-value "buffer=" (transient-args transient-current-command)))
         (prompt-arg (transient-arg-value "prompt=" (transient-args transient-current-command)))
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
                   prompt-from-region)))
         (options (robby--get-transient-options)))
    (robby--run-command :prompt (lambda (_arg) `(,prompt . ,prompt)) :action action :output-buffer output-buffer :options options)))

(defmacro robby--define-suffix (name action)
  `(transient-define-suffix ,name ()
     (interactive)
     (robby--respond-to-transient-action #',action)))

(robby--define-suffix robby--message-suffix robby--respond-with-message)
(robby--define-suffix robby--help-window-suffix robby--show-response-in-help-window)
(robby--define-suffix robby--prepend-to-region-suffix robby--prepend-response-to-region)
(robby--define-suffix robby--append-after-region-suffix robby--append-response-after-region)
(robby--define-suffix robby--replace-region-suffix robby--replace-region-with-response)

(defun robby--buffer-reader (prompt initial-input history)
  (interactive)
  (read-buffer "Select buffer: "))

;;;###autoload (autoload 'robby-chat "robby" "Robby chat transient command" t)
(transient-define-prefix robby-chat ()
  "Select Chat API Options"
  
  ["Prompt Sources"
   ("-p" "prompt" "prompt=" :prompt "prompt or prompt prefix: ")
   ("-r" "get prompt from region or buffer" "prompt-from-region-p")
   ("-b" "output buffer" "buffer=" :prompt "Select output buffer" :reader robby--buffer-reader)]
  ["Options"
   ("-m" "model" "model=" :choices ("gpt-4" "gpt-3.5-turbo"))
   ("-t" "max tokens" "max-tokens=" :reader transient-read-number-N+)]
  ["Actions"
   ("m" "respond with message" robby--message-suffix)
   ("h" "show response in help window" robby--help-window-suffix)
   ("p" "prepend response to region or beginning of buffer" robby--prepend-to-region-suffix)
   ("a" "append response after region or end of buffer" robby--append-after-region-suffix)
   ("r" "replace region or buffer with response" robby--replace-region-suffix)])

(provide 'robby-transient)

;; robby-transient.el ends here
