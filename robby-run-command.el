;;; robby-run-command.el  --- function to run Robby comments  -*- lexical-binding:t -*-

;;; Commentary:

;; robby-run-command function, which provides a generic way to run Robby commands defined by robby-define-command.

;;; Code:

(require 'robby-apis)
(require 'robby-curl)
(require 'robby-customization)
(require 'robby-history)
(require 'robby-logging)
(require 'robby-options)
(require 'robby-prompts)
(require 'robby-save-commands)
(require 'robby-spinner)

(defvar robby-command-complete-hook nil
  "Hook called when a robby OpenAI command completes successfully.")

(defvar-local robby--last-process nil)
(put 'robby--last-process 'permanent-local t)

(defun robby--process-running-p ()
  "Return non-nil if robby process is currently running."
  (and
   (not (null robby--last-process))
   (process-live-p robby--last-process)))

(defun robby-kill-last-process (&optional silentp)
  "If a robby process is currently running, kill it.

Do nothing if no process is currently running. If called from
Emacs Lisp, do not print messages if SILENTP is t."
  (interactive)
  (if (robby--process-running-p)
      (progn
        (robby--spinner-stop)
        (kill-process robby--last-process)
        (if (not silentp)
            (message "robby process killed")))
    (if (not silentp)
        (message "no robby process running"))))

(defun robby--get-response-region (action-args)
  (let ((response-buffer (or (plist-get action-args :response-buffer) (current-buffer))))
    (with-current-buffer response-buffer
      (if (use-region-p)
          (cons (region-beginning) (region-end))
        (cons (point-min) (point-max))))))

(defun robby--cleanup-process (err)
  (robby--log (format "# unexpected error in robby process: %S" err))
  (robby--spinner-stop)
  (kill-process robby--last-process))

;; TODO rename to match callback - on-text (or rename cb)
(cl-defun robby--handle-success (&key
                                 action
                                 action-args
                                 api
                                 basic-prompt
                                 chars-processed
                                 completep
                                 text
                                 response-region)
  (if completep
      (robby--spinner-stop))
  (robby--log (format "# response text:\n%S\ncomplete: %S, chars-processed %d" text completep chars-processed))
  (let ((beg (car response-region))
        (end (cdr response-region)))
    (if completep
        (robby--history-push basic-prompt text))
    (if (not (string-empty-p text))
        (apply action (map-merge 'plist action-args `(:text ,text :beg ,beg :end ,end :prompt ,basic-prompt :chars-processed ,chars-processed :completep ,completep))))
    (run-hooks 'robby-command-complete-hook)))

(defun robby--handle-error (err)
  (let ((msg (format "Error processing robby request: %s" (error-message-string err))))
    (robby--log msg)
    (message msg))
  (robby--spinner-stop)
  (kill-process robby--last-process))

(defun robby--parse-error-response (data)
  "Parse raw error response from DATA and try to return descriptive message."
  (or (cdr (assoc 'message (assoc 'error data))) "unknown error"))

(cl-defun robby-run-command (&key prompt prompt-args action action-args historyp api api-options)
  "Run a command using OpenAI.

PROMPT is a string or a function. If a string it used as is as
the prompt to send to OpenAI. If PROMPT is a function it is
called with PROMPT-ARGS to produce the prompt. PROMPT-ARGS is a
key / value style property list.
d
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
         (response-buffer (get-buffer-create (or (plist-get action-args :response-buffer) (current-buffer))))
         (response-region (robby--get-response-region action-args))
         (url (robby--request-url request-api))
         (chars-processed 0))

    (robby--log (format "# Prompt:\n%S\n# Request body:\n%s\n" complete-prompt payload))

    (with-current-buffer response-buffer
      (robby-kill-last-process t)
      (robby--spinner-start)
      (setq robby--last-process
            (robby--curl
             ;; :url url
             :payload payload
             :on-text
             (cl-function
              (lambda (&key text chars completep &allow-other-keys)
                (if (buffer-live-p response-buffer)
                    (condition-case err
                        (with-current-buffer response-buffer
                          (robby--handle-success
                           :action action
                           :action-args action-args
                           :api request-api
                           :basic-prompt basic-prompt
                           :chars-processed chars-processed
                           :completep completep
                           :response-region response-region
                           :text text))
                      (error (robby--handle-error err))))
                (setq chars-processed (+ chars-processed (length text)))))
             :on-error
             (lambda (err)
               (with-current-buffer response-buffer
                 (robby--handle-error err))))))))

(provide 'robby-run-command)

;; robby-run-command.el ends here
