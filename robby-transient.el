;;; robby-transient.el  --- transient menus for Robby  -*- lexical-binding:t -*-

;;; Commentary:

;; Transient menus for Robby, to let user specify OpenAI API options and such.

(require 'transient)
(require 'cl)

(require 'robby-define-command)
(require 'robby-actions)

;;; Code:

;;; robby transient utility functions
(defun robby--custom-type (symbol)
  (let ((type (get symbol 'custom-type)))
    (if (sequencep type)
        (nth 1 type)
      type)))

(defun robby--convert-arg-value (arg-value type)
  (cond ((eq type 'integer) (string-to-number arg-value))
        ((eq type 'number) (string-to-number arg-value))
        (t arg-value)))

(defun robby--get-transient-options (api)
  (let ((args (transient-args transient-current-command)))
    (seq-reduce
     (lambda (options custom-var)
       (let* ((arg-name (replace-regexp-in-string (format "^robby-%s-" (symbol-name api)) "" (symbol-name custom-var)))
              (arg-value (transient-arg-value (format "%s=" arg-name) args)))
         (if arg-value
             (plist-put
              options
              (intern (format ":%s" arg-name)) (robby--convert-arg-value arg-value (robby--custom-type custom-var)))
           options)))
     (seq-map #'car (custom-group-members (intern-soft (format "robby-%s-api" (symbol-name api))) nil))
     '())))

(defun robby--respond-to-transient-action (action api)
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
         (options (robby--get-transient-options api)))
    (robby--run-command :prompt (lambda (_arg) `(,prompt . ,prompt)) :action action :output-buffer output-buffer :api api :options options)))

(defmacro robby--define-suffix (name action api)
  `(transient-define-suffix ,name ()
     (interactive)
     (robby--respond-to-transient-action #',action #',api)))

;;; robby-chat transient
(robby--define-suffix robby--chat-message-suffix robby--respond-with-message chat)
(robby--define-suffix robby--chat-help-window-suffix robby--show-response-in-help-window chat)
(robby--define-suffix robby--chat-prepend-to-region-suffix robby--prepend-response-to-region chat)
(robby--define-suffix robby--chat-append-after-region-suffix robby--append-response-after-region chat)
(robby--define-suffix robby--chat-replace-region-suffix robby--replace-region-with-response chat)

(defun robby--buffer-reader (prompt initial-input history)
  (interactive)
  (read-buffer "Select buffer: "))

(defun robby--decimal-reader (prompt initial-input history)
  (interactive)
  (format "%s" (read-number prompt)))

;;;###autoload (autoload 'robby-chat "robby" "Robby chat transient command" t)
(transient-define-prefix robby-chat ()
  "Select Chat API Options"
  
  ["Prompt Sources"
   ("-p" "prompt" "prompt=" :prompt "prompt or prompt prefix: ")
   ("-r" "get prompt from region or buffer" "prompt-from-region-p")
   ("-b" "output buffer" "buffer=" :prompt "Select output buffer" :reader robby--buffer-reader)]
  ["Options"
   ("-m" "model" "model=" :choices ("gpt-4" "gpt-3.5-turbo"))
   ("-e" "temperature" "temperature=" :reader robby--decimal-reader)
   ("-t" "max tokens" "max-tokens=" :reader robby--decimal-reader)
   ("-p" "top p" "top-p=")
   ("-f" "frequency penalty" "frequency-penalty=" :reader robby--decimal-reader)
   ("-r" "presence penalty" "presence-penalty=" :reader robby--decimal-reader)]
  ["Actions"
   ("m" "respond with message" robby--chat-message-suffix)
   ("h" "show response in help window" robby--chat-help-window-suffix)
   ("p" "prepend response to region or beginning of buffer" robby--chat-prepend-to-region-suffix)
   ("a" "append response after region or end of buffer" robby--chat-append-after-region-suffix)
   ("r" "replace region or buffer with response" robby--chat-replace-region-suffix)])

;;; robby-completions transient
(robby--define-suffix robby--completions-message-suffix robby--respond-with-message completions)
(robby--define-suffix robby--completions-help-window-suffix robby--show-response-in-help-window completions)
(robby--define-suffix robby--completions-prepend-to-region-suffix robby--prepend-response-to-region completions)
(robby--define-suffix robby--completions-append-after-region-suffix robby--append-response-after-region completions)
(robby--define-suffix robby--completions-replace-region-suffix robby--replace-region-with-response completions)

(defun robby--buffer-reader (prompt initial-input history)
  (interactive)
  (read-buffer "Select buffer: "))

;;;###autoload (autoload 'robby-completions "robby" "Robby completions transient command" t)
(transient-define-prefix robby-completions ()
  "Select Completions API Options"
  
  ["Prompt Sources"
   ("-p" "prompt" "prompt=" :prompt "prompt or prompt prefix: ")
   ("-r" "get prompt from region or buffer" "prompt-from-region-p")
   ("-b" "output buffer" "buffer=" :prompt "Select output buffer" :reader robby--buffer-reader)]
  ["Options"
   ("-m" "model" "model=" :choices ("text-davinci-003" "text-curie-001" "text-babbage-001" "text-ada-001"))
   ("-e" "temperature" "temperature=" :reader robby--decimal-reader)
   ("-t" "max tokens" "max-tokens=" :reader transient-read-number-N+)
   ;; TODO stop sequences
   ("-p" "top p" "top-p=" :reader robby--decimal-reader)
   ("-f" "frequency penalty" "frequency-penalty=" :reader robby--decimal-reader)
   ("-r" "presence penalty" "presence-penalty=" :reader robby--decimal-reader)
   ("-b" "best of" "best-of=" :reader transient-read-number-N+)]
  ["Actions"
   ("m" "respond with message" robby--completions-message-suffix)
   ("h" "show response in help window" robby--completions-help-window-suffix)
   ("p" "prepend response to region or beginning of buffer" robby--completions-prepend-to-region-suffix)
   ("a" "append response after region or end of buffer" robby--completions-append-after-region-suffix)
   ("r" "replace region or buffer with response" robby--completions-replace-region-suffix)])

(provide 'robby-transient)

;; robby-transient.el ends here
