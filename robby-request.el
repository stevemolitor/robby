;;; robby-request.el  --- make robby OpenAI requests  -*- lexical-binding:t -*-

;;; Commentary:

;; Make HTTP requests to OpenAI API.

(require 'json)
(require 'request)

(require 'robby-prompts)
(require 'robby-history)
(require 'robby-customization)
(require 'robby-logging)
(require 'robby-options)
(require 'robby-apis)
(require 'robby-spinner)

;;; Code:

(defvar-local robby--last-request nil)

(defun robby--request-running-p ()
  "Return non-nil if robby request is currently running."
  (and
   (not (null robby--last-request))
   (null (request-response-symbol-status robby--last-request))))

(defun robby--parse-error-response (data)
  "Parse raw error response from DATA and try to return descriptive message."
  (or (cdr (assoc 'message (assoc 'error data))) "unknown error"))

(defun robby-kill-last-request ()
  "If a request is currently running, kill it.

Do nothing if no request is currently running."
  (interactive)
  (if (robby--request-running-p)
      (request-abort robby--last-request)))

(defun robby--request (prompt basic-prompt historyp api options beg end callback)
  "Make HTTP request to OpenAI API.

PROMPT is the complete prompt possibly including conversation history.

BASIC-PROMPT is the prompt string, without any conversation
history.  If HISTORYP is t record complete PROMPT and TEXT
response in history when HTTP request completes.

API is the symbol of the api to use, for example `'chat' or
`'completions'.

OPTIONS is a property list of options to pass to the OpenAI
API. It is merged in with the customization options for the API.

Call CALLBACK with result.  CALLBACK function must be of the
form `(TEXT BEG END), where TEXT is the value returned by OpenAI,
and BEG and END are the bounds of the selected region if any when
the command was invoked."
  (cl-assert robby-openai-api-key t "Please set robby-openai-api-key customization variable to your OpenAI API Key.")

  (let* ((input-obj (append prompt (robby--options options)))
         (input-json (json-encode input-obj))
         (buf (current-buffer)))
    (if (bound-and-true-p robby-mode)
        (robby--spinner-start))
    (robby--log (format "# Prompt:\n%S\n# Request body:\n%s\n" prompt input-json))
    (robby-kill-last-request)
    (setq robby--last-request
          (request
            (robby--request-url api)
            :type "POST"
            :headers `(("Content-Type" . "application/json")
                       ("Authorization" . ,(format "Bearer %s" robby-openai-api-key)))
            ;; TODO change robby-api to be a symbol
            :data input-json
            :parser 'json-read
            :success
            (cl-function
             (lambda (&key data &allow-other-keys)
               (robby--spinner-stop buf)
               (robby--log (format "# Raw response:\n%S\n" data))
               (let ((text (robby--parse-response api data)))
                 (robby--history-push basic-prompt text)
                 (funcall callback text beg end))))
            :error
            (cl-function
             (lambda (&rest args &key error-thrown &key data &key symbol-status &allow-other-keys)
               (unless (robby--request-running-p)
                 (robby--spinner-stop buf))
               (robby--log (format "# Error thrown:\n%S\n# Raw error response data:\n%S\n# symbol-status: %S" error-thrown data symbol-status))
               (cond
                ((robby--request-running-p)
                 (message "Making another request to our AI overlords…"))
                ((eq symbol-status 'abort)
                 (message "Robby request aborted"))
                (t
                 (message (robby--parse-error-response data))))))))))

(provide 'robby-request)

;;; robby-request.el ends here
