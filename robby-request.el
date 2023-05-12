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

;; TODO move me
(defvar robby-command-complete-hook nil
  "Hook called when a robby OpenAI command completes successfully.")

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

(cl-defun robby--request-handle-success (&key
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
    (apply action (map-merge 'plist action-args `(:text ,text :beg ,beg :end ,end)))
    (run-hooks 'robby-command-complete-hook)))

(cl-defun robby--request-handle-error (&key error-thrown data symbol-status spinner-buffer)
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

(cl-defun robby--request (&key action
                               action-args
                               basic-prompt
                               complete-prompt
                               historyp
                               api
                               api-options
                               spinner-buffer
                               response-region)
  "Make HTTP request to OpenAI API.

BASIC-PROMPT is the prompt string, without any conversation
history.

COMPLETE-PROMPT is the complete prompt possibly including conversation history.

If HISTORYP is t record COMPLETE-PROMPT and text response in
history when HTTP request completes.

API is the symbol of the api to use, for example `'chat' or
`'completions'.

API-OPTIONS is a property list of options to pass to the OpenAI
API. It is merged in with the customization options for the API.

SPINNER-BUFFER is buffer with the buffer local spinner value. You
can have different Robby commands running in different buffers,
each with their own spinner.

Call ACTION with result. ACTION is called with the ACTION-ARGS
property list, and a property list with the keys `':text', (the
response text), `:beg' and `:end' (from `response-region') and
their values merged in."
  (cl-assert robby-openai-api-key t "Please set robby-openai-api-key customization variable to your OpenAI API Key.")

  (let* ((input-obj (append complete-prompt (robby--options api-options)))
         (input-json (json-encode input-obj))
         (buf (current-buffer)))
    (if (bound-and-true-p robby-mode)
        (robby--spinner-start))
    (robby--log (format "# Prompt:\n%S\n# Request body:\n%s\n" complete-prompt input-json))
    (robby-kill-last-request)
    (setq robby--last-request
          (request
            (robby--request-url api)
            :type "POST"
            :headers `(("Content-Type" . "application/json")
                       ("Authorization" . ,(format "Bearer %s" robby-openai-api-key)))
            :data input-json
            :parser 'json-read
            :success
            (cl-function
             (lambda (&key data &allow-other-keys)
               (robby--request-handle-success
                :action action
                :action-args action-args
                :api api
                :basic-prompt basic-prompt
                :data data
                :response-region response-region
                :spinner-buffer buf)))
            :error
            (cl-function
             (lambda (&rest args &key error-thrown &key data &key symbol-status &allow-other-keys)
               (robby--request-handle-error
                :error-thrown error-thrown
                :data data
                :symbol-status symbol-status
                :spinner-buffer spinner-buffer)))))))

(provide 'robby-request)

;;; robby-request.el ends here
