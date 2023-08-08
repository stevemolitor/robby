;;; robby-run-command.el  --- function to defined and run Robby commands  -*- lexical-binding:t -*-

;;; Commentary:

;; robby-run-command function, which provides a generic way to run Robby commands defined by robby-define-command.

;;; Code:

(require 'cl-lib)
(require 'map)

(require 'robby-apis)
(require 'robby-request)
(require 'robby-customization)
(require 'robby-history)
(require 'robby-logging)
(require 'robby-spinner)

(defvar robby-command-complete-hook nil
  "Hook called when a robby OpenAI command completes successfully.")

(defvar-local robby--last-process nil)
(put 'robby--last-process 'permanent-local t)

;;; save last command
(defvar robby--last-command-options nil
  "Last robby command.")

(cl-defun robby--save-last-command-options (&key arg prompt prompt-args action action-args historyp api api-options)
  (setq robby--last-command-options
        `(:arg
          ,arg
          :prompt
          ,(if (functionp prompt) `#',prompt prompt)
          :prompt-args
          ',prompt-args
          :action
          ,`#',action
          :action-args
          ',action-args
          :historyp
          ,historyp
          :api
          ,api
          :api-options
          ',api-options)))

(defun robby-insert-last-command (name)
  "Insert a definition for the last command invoked into current
buffer.

NAME specifies the new command name, a symbol."
  (interactive "sCommand name: ")
  (let* ((docstring (read-string "Doc string: "))
         (cmd `(robby-define-command ,(intern name) ,docstring ,@robby--last-command-options)))
    (insert (format "%S" cmd))))

;;; run command 
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
  ;; stop the spinner no matter what - it's harmless if already stopped
  (robby--spinner-stop)

  (if (robby--process-running-p)
      (progn
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
  (robby-kill-last-process t))

(cl-defun robby--handle-text (&key
                              arg
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
  (robby--log (format "# robby--handle-text, text:\n%S\ncompletep: %S, chars-processed %d" text completep chars-processed))
  (let ((beg (car response-region))
        (end (cdr response-region)))
    (if completep
        (robby--history-push basic-prompt text))
    (apply
     action
     (map-merge
      'plist action-args
      `(:arg ,arg :text ,text :beg ,beg :end ,end :prompt ,basic-prompt :chars-processed ,chars-processed :completep ,completep)))
    (if completep
        (run-hooks 'robby-command-complete-hook))))

(defun robby--handle-error (err)
  (robby--spinner-stop)
  (let* ((err-msg (if (stringp err) err (error-message-string err)))
         (log-msg (format "Error processing robby request: %s" err-msg)))
    (robby--log log-msg)
    (message log-msg))
  (if (process-live-p robby--last-process)
      (robby-kill-last-process t)))

(defun robby--parse-error-response (data)
  "Parse raw error response from DATA and try to return descriptive message."
  (or (cdr (assoc 'message (assoc 'error data))) "unknown error"))

(cl-defun robby-run-command (&key arg prompt prompt-args action action-args api api-options historyp never-stream-p)
  "Run a command using OpenAI.

ARG is the interactive prefix arg, if any. It is pass to the
PROMPT and ACTION functions.

PROMPT is a string or a function. If a string it used as is as
the prompt to send to OpenAI. If PROMPT is a function it is
called with PROMPT-ARGS to produce the prompt. PROMPT-ARGS is a
key / value style property list.
d
When the response text is received from OpenAI, ACTION is called
with the property list ACTION-ARGS and `:text text`, where text
is the text response from OpenAI.

API specifies which OpenAI API to use, for example \"chat\" or
\"completions\". It defaults to the value of `'robby-api'.

API-OPTIONS is an optional property list of options to pass to
the OpenAI API. Kebab case keys are converted to snake case JSON
keys. For example `'max-tokens' becomes \"max_tokens\". The
values in API-OPTIONS are merged with and overwrite equivalent
values in the customization options specified in for example
`'robby-chat-options' or `'robby-completion-options'.

HISTORYP indicates whether or not to use conversation history.

NEVER-STREAM-P - Never stream response if t. if present this value overrides
the `robby-stream' customization variable."
  ;; save command history
  (robby--save-last-command-options
   :arg arg :prompt prompt :prompt-args prompt-args :action action :action-args action-args :historyp historyp :api api :api-options api-options)

  (let* ((prompt-args-with-arg (map-merge 'plist prompt-args `(:arg ,arg)))
         (basic-prompt (if (functionp prompt) (apply prompt prompt-args-with-arg) (format "%s" prompt)))
         (request-api (intern (or api robby-api)))
         (complete-prompt (robby--request-input request-api basic-prompt historyp))
         (payload (append complete-prompt (robby--options (or api robby-api) api-options)))
         (response-buffer (get-buffer-create (or (plist-get action-args :response-buffer) (current-buffer))))
         (response-region (robby--get-response-region action-args))
         (streamp (and (not never-stream-p) robby-stream-p))
         (chars-processed 0))

    (robby--log (format "# Prompt:\n%S\n# Request body:\n%s\n" complete-prompt payload))

    (if (not (window-live-p (get-buffer-window response-buffer)))
           (display-buffer response-buffer))

    (with-current-buffer response-buffer
      (robby-kill-last-process t)
      (robby--spinner-start)
      ;; TODO can't kill url-retrieve process
      (setq robby--last-process
            (condition-case curl-err
                (robby--request
                 :api request-api
                 :payload payload
                 :streamp streamp
                 :on-text
                 (cl-function
                  (lambda (&key text completep)
                    (if (buffer-live-p response-buffer)
                        (condition-case err
                            (with-current-buffer response-buffer
                              (robby--handle-text
                               :arg arg
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
                     (robby--handle-error err))))
              (error (robby--handle-error curl-err)))))))

;;; define command
(cl-defmacro robby-define-command (name
                                   docstring
                                   &key
                                   prompt
                                   prompt-args
                                   action
                                   action-args
                                   api
                                   api-options
                                   historyp
                                   never-stream-p)
  "Define a new Robby command.

NAME is the command name, a symbol.
DOCSTRING is the documentation string for the new command.

Keyword parameters:

PROMPT - Function or string.  If a function, command will call
with interactive prefix arg to obtain the prompt.  If a string,
grab prompt from region or entire buffer context if no region,
and prefix region text with PROMPT string to build prompt.

ACTION - function to invoke when request is complete.  The
function is passed the response text and the selected region, and
must be of the form `(TEXT BEG END)'.

API - the OpenAI API to use, either `chat' or `completions`'.
Defaults to the value of the `robby-api' customization variable
if not supplied.

API-OPTIONS - property list of options to pass to the OpenAI
API. These options are merged in with the customization options
specified in the api customization group, either `robby-chat-api'
or `robby-completions-api'.

HISTORYP - include conversation history in OpenAI request if t.

NEVER-STREAM-P - Stream response if t. if present this value overrides
the `robby-stream' customization variable."
  `(defun ,name (arg)
     ,docstring
     (interactive "P")
     (robby-run-command
      :arg arg
      :prompt ,prompt
      :prompt-args ,prompt-args
      :action ,action
      :action-args ,action-args
      :api ,api
      :api-options ,api-options
      :historyp ,historyp
      :never-stream-p ,never-stream-p)))

(provide 'robby-run-command)

;; robby-run-command.el ends here
