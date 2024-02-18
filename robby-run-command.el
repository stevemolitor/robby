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

(defun robby-insert-last-command (name docstring)
  "Insert elisp definition of the last robby command in current buffer at point."
  (interactive "Sname: \nsdocstring: ")
  (let* ((options robby--last-command-options)
         (prompt (plist-get options :prompt))
         (prompt-args (plist-get options :prompt-args))
         (action (plist-get options :action))
         (action-args (plist-get options :action-args))
         (api-options (plist-get options :api-options))
         (historyp (plist-get options :historyp))
         (quoted-options `(:prompt
                           ,(if (functionp prompt) `#',prompt prompt)
                           :action
                           ,`#',action)))
    (when prompt-args
      (setq quoted-options (plist-put quoted-options :prompt-args `',prompt-args)))
    (when action-args
      (setq quoted-options (plist-put quoted-options :action-args `',action-args)))
    (when api-options
      (setq quoted-options (plist-put quoted-options :api-options `',api-options)))
    (when historyp
      (setq quoted-options (plist-put quoted-options :historyp t)))
    (robby--pp-cmd `(robby-define-command ,name ,docstring ,@quoted-options))))

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

(defun robby--get-response-region (response-buffer action-args)
  (with-current-buffer response-buffer
    (if (use-region-p)
        (cons (region-beginning) (region-end))
      (cons (point) (point)))))

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
                              grounding-fns
                              no-op-pattern
                              no-op-message
                              text
                              response-buffer
                              response-region)
  (when completep
    (robby--spinner-stop))
  (robby--log (format "# robby--handle-text, text:\n%S\ncompletep: %S, chars-processed %d" text completep chars-processed))
  (let ((beg (car response-region))
        (end (cdr response-region))
        (grounded-text (robby--ground-response text grounding-fns)))
    (when completep
      (robby--history-push basic-prompt text))
    (if (and no-op-pattern (string-match-p no-op-pattern text))
        (message (or no-op-message) "no action to perform")
      (when (or completep (> (length grounded-text) 0))
        (apply
         action
         (map-merge
          'plist action-args
          `(:arg ,arg :text ,grounded-text :response-buffer ,response-buffer :beg ,beg :end ,end :prompt ,basic-prompt :chars-processed ,chars-processed :completep ,completep)))))
    (when completep
      (run-hooks 'robby-command-complete-hook))))

(defun robby--handle-error (err)
  (robby--spinner-stop)
  (let* ((err-msg (if (stringp err) err (error-message-string err)))
         (log-msg (format "Error processing robby request: %s" err-msg)))
    (robby--log log-msg)
    (message log-msg))
  (when (process-live-p robby--last-process)
      (robby-kill-last-process t)))

(defun robby--parse-error-response (data)
  "Parse raw error response from DATA and try to return descriptive message."
  (or (cdr (assoc 'message (assoc 'error data))) "unknown error"))

(cl-defun robby--get-stream-p (&key never-stream-p no-op-pattern grounding-fns)
  (let ((streaming-on-p (not (or never-stream-p (not robby-stream-p)))))
    (cond
     ;; no-op-pattern can only be used when streaming is off
     ((and no-op-pattern streaming-on-p) nil)

     ;; grounding-fns only make sense when streaming is off
     ((and grounding-fns streaming-on-p) nil)

     ;; otherwise, use the what is specified by never-stream-p or the robby-stream-p customization variable
     (t streaming-on-p))))

(defun robby--get-response-buffer (action action-args)
  (or
   ;; use the response buffer specified in the action-args if supplied 
   (plist-get action-args :response-buffer)
   ;; make sure robby views use the robby view buffer unless otherwise specified:
   (and (eq action 'robby-respond-with-robby-view) "*robby*")
   ;; default to current buffer
   (current-buffer)))

(cl-defun robby-run-command (&key arg prompt prompt-args action action-args api-options grounding-fns no-op-pattern no-op-message historyp never-stream-p)
  "Run a command using OpenAI.

ARG is the interactive prefix arg, if any. It is pass to the
PROMPT and ACTION functions.

PROMPT is a string or a function. If a string it used as is as
the prompt to send to OpenAI. If PROMPT is a function it is
called with PROMPT-ARGS to produce the prompt. PROMPT-ARGS is a
key / value style property list.

When the response text is received from OpenAI, ACTION is called
with the property list ACTION-ARGS and `:text text`, where text
is the text response from OpenAI.

API-OPTIONS is an optional property list of options to pass to
the OpenAI API. Kebab case keys are converted to snake case JSON
keys. For example `'max-tokens' becomes \"max_tokens\". The
values in API-OPTIONS are merged with and overwrite equivalent
values in the customization options specified in for example
`'robby-chat-options' or `'robby-completion-options'.

GROUNDING-FNS - Format the response from OpenAI before returning
it. Only used if `NEVER-STREAM-P' is t.

NO-OP-PATTERN - If the response matches this regular expression,
do not perform the action. Useful with a prompt that tells OpenAI
to respond with a certain response if there is nothing to do. For
example with a prompt of \"Fix this code. Respond with 'the code
is correct' if the code is correct\", then a NO-OP-PATTERN of
\"code is correct\" will tell robby to not replace the region
when the pattern matches. Only use NO-OP-PATTERN when
NEVER-STREAM-P is t.

NO-OP-MESSAGE - Message to display when NO-OP-PATTERN matches. Optional.

HISTORYP indicates whether or not to use conversation history.

NEVER-STREAM-P - Never stream response if t. if present this
value overrides the `robby-stream' customization variable."
  ;; save command history
  (robby--save-last-command-options
   :arg arg :prompt prompt :prompt-args prompt-args :action action :action-args action-args :historyp historyp :api-options api-options :never-stream-p never-stream-p)

  (let* ((prompt-args-with-arg (map-merge 'plist prompt-args `(:arg ,arg)))
         (prompt-result (if (functionp prompt) (apply prompt prompt-args-with-arg) (format "%s" prompt)))
         (basic-prompt (robby--format-prompt prompt-result))
         (request-input (robby--request-input basic-prompt historyp))
         (payload (append request-input (robby--options api-options)))
         (response-buffer (get-buffer-create (robby--get-response-buffer action action-args)))
         (response-region (robby--get-response-region response-buffer action-args))
         (streamp (robby--get-stream-p :never-stream-p never-stream-p :no-op-pattern no-op-pattern :grounding-fns grounding-fns))
         (chars-processed 0))

    (robby--log (format "# Request body:\n%s\n" payload))
    
    (with-undo-amalgamate
      (with-current-buffer response-buffer
        (robby-kill-last-process t)
        (robby--spinner-start)
        (setq robby--last-process
              (condition-case curl-err
                  (robby--request
                   :payload payload
                   :streamp streamp
                   :on-text
                   (cl-function
                    (lambda (&key text completep)
                      (when (buffer-live-p response-buffer)
                        (condition-case err
                            (with-current-buffer response-buffer
                              (robby--handle-text
                               :action action
                               :action-args action-args
                               :arg arg
                               :basic-prompt basic-prompt
                               :chars-processed chars-processed
                               :completep completep
                               :grounding-fns grounding-fns
                               :no-op-pattern no-op-pattern
                               :no-op-message no-op-message
                               :response-buffer response-buffer
                               :response-region response-region
                               :text text))
                          (error (robby--handle-error err))))
                      (setq chars-processed (+ chars-processed (length text)))))
                   :on-error
                   (lambda (err)
                     (with-current-buffer response-buffer
                       (robby--handle-error err))))
                (error (robby--handle-error curl-err))))))))

(provide 'robby-run-command)

;; robby-run-command.el ends here
