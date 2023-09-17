;;; robby-run-command.el  --- function to defined and run Robby commands  -*- lexical-binding:t -*-

;;; Commentary:

;; robby-run-command function, which provides a generic way to run Robby commands defined by robby-define-command.

;;; Code:

(require 'cl-lib)
(require 'map)

(require 'robby-request)
(require 'robby-customization)
(require 'robby-history)
(require 'robby-logging)
(require 'robby-spinner)

;;; variables
(defvar robby-command-complete-hook nil
  "Hook called when a robby OpenAI command completes successfully.")

(defvar-local robby--last-process nil)
(put 'robby--last-process 'permanent-local t)

;;; save last command
(defvar robby--last-command-options nil
  "Last robby command.")

(cl-defun robby--save-last-command-options (&key arg prompt prompt-args action action-args historyp api-options never-stream-p)
  (setq robby--last-command-options
        `(:prompt
          ,prompt
          :prompt-args
          ,prompt-args
          :action
          ,action
          :action-args
          ,action-args
          :historyp
          ,historyp
          :api-options
          ,api-options
          :never-stream-p
          ,never-stream-p)))

(defun robby--check-for-last-command ()
  "Signal a user-error if there is no `robby--last-command-options'.

Return t if there is a last command."
  (when (not robby--last-command-options)
    (user-error "No last robby command to run. Run at least one command first."))
  t)

(defun robby-run-last-command ()
  "Re-run the robby command last executed."
  (interactive)
  (robby--check-for-last-command)
  (apply #'robby-run-command robby--last-command-options))

(defun robby--pp-cmd (cmd)
  "Insert `cmd' into current buffer."
  (if (version< emacs-version "29.0")
      (pp cmd)
    (let ((pp-max-width 70))
      (pp-emacs-lisp-code cmd))))

(defun robby-save-last-command (name docstring)
  "Insert elisp definition of the last robby command in current buffer at point."
  (interactive "Sname: \nsdocstring: ")
  (let* ((options robby--last-command-options)
         (prompt (plist-get options :prompt))
         (prompt-args (plist-get options :prompt-args))
         (action (plist-get options :action))
         (action-args (plist-get options :action-args))
         (historyp (plist-get options :historyp))
         (api-options (plist-get options :api-options))
         (quoted-options `(:prompt
                           ,(if (functionp prompt) `#',prompt prompt)
                           :prompt-args
                           ',prompt-args
                           :action
                           ,`#',action
                           :action-args
                           ',action-args
                           :historyp
                           ,historyp
                           :api-options
                           ',api-options))
         (cmd `(robby-define-command ,name ,docstring ,@quoted-options)))
    (insert (robby--pp-cmd cmd))))

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
        (message "no robby process associated with current buffer"))))

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
                              basic-prompt
                              chars-processed
                              completep
                              grounding-fn
                              never-stream-p
                              text
                              response-region)
  (if completep
      (robby--spinner-stop))
  (robby--log (format "# robby--handle-text, text:\n%S\ncompletep: %S, chars-processed %d" text completep chars-processed))
  (let ((beg (car response-region))
        (end (cdr response-region))
        (grounded-text (if (and never-stream-p grounding-fn)
                           (funcall grounding-fn text)
                         text)))
    (if completep
        (robby--history-push basic-prompt text))
    (apply
     action
     (map-merge
      'plist action-args
      `(:arg ,arg :text ,grounded-text :beg ,beg :end ,end :prompt ,basic-prompt :chars-processed ,chars-processed :completep ,completep)))
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

(cl-defun robby--validate-args (&key arg action)
  ;; confirm before replacing entire buffer
  (when (and (eq action 'robby-replace-region-with-response)
             robby-confirm-whole-buffer-p
             (not (use-region-p)))
    (let ((proceedp (and robby-confirm-whole-buffer-p (yes-or-no-p "Overwrite entire buffer contents with response?"))))
      (when (not proceedp)
        (user-error "Select a region and then re-run robby command.")))))

(cl-defun robby-run-command (&key arg prompt prompt-args action action-args api-options grounding-fn historyp never-stream-p)
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

API-OPTIONS is an optional property list of options to pass to
the OpenAI API. Kebab case keys are converted to snake case JSON
keys. For example `'max-tokens' becomes \"max_tokens\". The
values in API-OPTIONS are merged with and overwrite equivalent
values in the customization options specified in for example
`'robby-chat-options' or `'robby-completion-options'.

GROUNDING-FN - Format the response from OpenAI before returning
it. Only used if `NEVER-STREAM-P' is t.

HISTORYP indicates whether or not to use conversation history.

NEVER-STREAM-P - Never stream response if t. if present this value overrides
the `robby-stream' customization variable."
  (robby--validate-args :arg arg :action action)
  
  ;; save command history
  (robby--save-last-command-options
   :arg arg :prompt prompt :prompt-args prompt-args :action action :action-args action-args :historyp historyp :api-options api-options :never-stream-p never-stream-p)

  (let* ((prompt-args-with-arg (map-merge 'plist prompt-args `(:arg ,arg)))
         (basic-prompt (if (functionp prompt) (apply prompt prompt-args-with-arg) (format "%s" prompt)))
         (complete-prompt (robby--request-input basic-prompt historyp))
         (payload (append complete-prompt (robby--options api-options)))
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
                 :payload payload
                 :streamp streamp
                 :on-text
                 (cl-function
                  (lambda (&key text completep)
                    (if (buffer-live-p response-buffer)
                        (condition-case err
                            (with-current-buffer response-buffer
                              (robby--handle-text
                               :action action
                               :action-args action-args
                               :arg arg
                               :basic-prompt basic-prompt
                               :chars-processed chars-processed
                               :completep completep
                               :grounding-fn grounding-fn
                               :never-stream-p never-stream-p
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
                                   api-options
                                   grounding-fn
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

API-OPTIONS - property list of options to pass to the OpenAI
API. These options are merged in with the customization options
specified in the api customization group, either `robby-chat-api'
or `robby-completions-api'.

GROUNDING-FN - Format the response from OpenAI before returning
it. Only used if `NEVER-STREAM-P' is t.

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
      :api-options ,api-options
      :grounding-fn ,grounding-fn
      :historyp ,historyp
      :never-stream-p ,never-stream-p)))

(provide 'robby-run-command)

;; robby-run-command.el ends here
