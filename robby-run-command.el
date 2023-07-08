;;; robby-run-command.el  --- function to run Robby comments  -*- lexical-binding:t -*-

;;; Commentary:

;; robby-run-command function, which provides a generic way to run Robby commands defined by robby-define-command.

;;; Code:

(require 'robby-apis)
(require 'robby-customization)
(require 'robby-history)
(require 'robby-logging)
(require 'robby-options)
(require 'robby-prompts)
(require 'robby-request)
(require 'robby-save-commands)
(require 'robby-spinner)

(defvar robby-command-complete-hook nil
  "Hook called when a robby OpenAI command completes successfully.")

(defvar-local robby--last-request nil)

(defun robby--request-running-p ()
  "Return non-nil if robby request is currently running."
  (and
   (not (null robby--last-request))
   (null (request-response-symbol-status robby--last-request))))

(defun robby-kill-last-request ()
  "If a request is currently running, kill it.

Do nothing if no request is currently running."
  (interactive)
  (robby--spinner-stop (current-buffer))
  (if (robby--request-running-p)
      (request-abort robby--last-request)))

(defun robby--get-response-region (action-args)
  (let ((response-buffer (or (plist-get action-args :response-buffer) (current-buffer))))
    (with-current-buffer response-buffer
      (if (use-region-p)
          (cons (region-beginning) (region-end))
        (cons (point-min) (point-max))))))

(cl-defun robby--handle-success (&key
                                   action
                                   action-args
                                   api
                                   basic-prompt
                                   data
                                   (response-region nil)
                                   spinner-buffer)
  (robby--spinner-stop spinner-buffer)
  (robby--log (format "# Raw response:\n%S\n" data))
  (let ((text (robby--parse-response api data))
        (beg (car response-region))
        (end (cdr response-region)))
    (robby--history-push basic-prompt text)
    (apply action (map-merge 'plist action-args `(:text ,text :beg ,beg :end ,end :prompt ,basic-prompt)))
    (run-hooks 'robby-command-complete-hook)))

(cl-defun robby--handle-error (&key error-thrown data symbol-status spinner-buffer)
  (unless (robby--request-running-p)
    (robby--spinner-stop spinner-buffer))
  (robby--log (format "# Error thrown:\n%S\n# Raw error response data:\n%S\n# symbol-status: %S" error-thrown data symbol-status))
  (cond
    ((robby--request-running-p)
     (message "Making another request to our AI overlords…"))
    ((eq symbol-status 'abort)
     (message "Robby request aborted"))
    (t
     (message (robby--parse-error-response data)))))

(defun robby--parse-error-response (data)
  "Parse raw error response from DATA and try to return descriptive message."
  (or (cdr (assoc 'message (assoc 'error data))) "unknown error"))

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
  (robby--save-last-command-options
   :prompt prompt :prompt-args prompt-args :action action :action-args action-args :historyp historyp :api api :api-options api-options)

  (let* ((basic-prompt (if (functionp prompt) (apply prompt prompt-args) (format "%s" prompt)))
         (request-api (intern (or api robby-api)))
         (complete-prompt (robby--request-input request-api basic-prompt historyp))
         (payload (append complete-prompt (robby--options api-options)))
         (spinner-buffer (or (plist-get action-args :response-buffer) (current-buffer)))
         (response-region (robby--get-response-region action-args))
         (url (robby--request-url request-api)))

    (robby--log (format "# Prompt:\n%S\n# Request body:\n%s\n" complete-prompt payload))
    (robby-kill-last-request)
    (if (bound-and-true-p robby-spinner-mode)
        (robby--spinner-start))

    (setq robby--last-request
          (robby--request
           :url url
           :payload payload
           :success
           (cl-function
            (lambda (&key data &allow-other-keys)
              (robby--handle-success
               :action action
               :action-args action-args
               :api request-api
               :basic-prompt basic-prompt
               :data data
               :response-region response-region
               :spinner-buffer spinner-buffer)))
           :error
           (cl-function
            (lambda (&rest args &key error-thrown &key data &key symbol-status &allow-other-keys)
              (robby--handle-error
               :error-thrown error-thrown
               :data data
               :symbol-status symbol-status
               :spinner-buffer spinner-buffer)))))))

(provide 'robby-run-command)

;; robby-run-command.el ends here
