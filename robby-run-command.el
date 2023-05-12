;;; robby-run-command.el  --- function to run Robby comments  -*- lexical-binding:t -*-

;;; Commentary:

;; robby-run-command function, which provides a generic way to run Robby commands defined by robby-define-command.

;;; Code:

(require 'robby-customization)
(require 'robby-prompts)
(require 'robby-request)

(defun robby--get-response-region (action-args)
  (let ((response-buffer (or (plist-get action-args :response-buffer) (current-buffer))))
    (with-current-buffer response-buffer
      (if (use-region-p)
          (cons (region-beginning) (region-end))
        (cons (point-min) (point-max))))))

(defun robby--run-command-callback (text beg end)
  (message "callback, %s text, beg %S end %S" text beg end))

(cl-defun robby-run-command (&key prompt prompt-args action action-args historyp api api-options)
  "Run a command using OpenAI.

PROMPT is a string or a function. If a string it used as is as
the prompt to send to OpenAI. If PROMPT is a function it is
called with PROMPT-ARGS to produce the prompt. PROMPT-ARGS is a
key / value style property list.

When the response text is received from OpenAI, ACTION is called
with the property list ACTION-ARGS and `:text text`, where text
is the text response from OpenAI.

HISTORYP indicates whether or not to use conversation history.

API specifies which OpenAI API to use, for example \"chat\" or
\"completions\". It defaults to the value of `'robby-api'.

API-OPTIONS is an optional property list of options to pass to
the OpenAI API. Kebab case keys are converted to snake case JSON
keys. For example `'max-tokens' becomes \"max_tokens\". The
values in API-OPTIONS are merged with and overwrite equivalent
values in the customization options specified in for example
`'robby-chat-options' or `'robby-completion-options'."
  
  ;; save command history
  (setq
   robby--last-command-options
   `(:prompt prompt :prompt-args prompt-args :action action :action-args action-args :historyp historyp :api api :api-options api-options))
  
  (let* ((basic-prompt (if (functionp prompt) (apply prompt prompt-args) (format "%s" prompt)))
         (request-api (intern (or api robby-api)))
         (complete-prompt (robby--request-input request-api basic-prompt historyp))
         (response-region (robby--get-response-region action-args)))
    (robby--request
     :basic-prompt basic-prompt
     :complete-prompt complete-prompt
     :historyp historyp
     :api request-api
     :api-options api-options
     :spinner-buffer (or (plist-get action-args :response-buffer) (current-buffer))
     :response-region (robby--get-region)
     :action action
     :action-args action-args)))

(provide 'robby-run-command)

;; robby-run-command.el ends here
