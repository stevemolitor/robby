;;; robby-define-command.el  --- macro to define robby custom commands  -*- lexical-binding:t -*-

;;; Commentary:

;; Provides `robby-define-command' function, used to define custom
;; Robby commands, and internal helper functions used by
;; `robby-define-command'.

(require 'cl-lib)

(require 'robby-request)
(require 'robby-save-commands)
(require 'robby-prefix-args)
(require 'robby-actions)
(require 'robby-apis)

;;; Code:

(defvar robby-command-complete-hook nil
  "Hook called when a robby OpenAI command completes successfully.")

(defun robby--get-prompt (prompt arg)
  "If PROMPT is a function, call it with ARG and return result.

Else grab prompt from region, or entire buffer if no region, and
prefix with PROMPT."
  (if (functionp prompt)
      (funcall prompt arg)
    ;; String prompts come from custom commands. Force them to use
    ;; region, and prefix with supplied prompt:
    (let* ((region-text (robby--get-region-or-buffer-text))
           (prompt-with-prefix (format "%s\n%s" prompt region-text)))
      `(,prompt-with-prefix . ,prompt))))

(cl-defun robby--run-command (&key arg historyp prompt action options output-buffer)
  "Make OpenAI API request.  Get PROMPT via `robby--get-prompt'.

Save current buffer positions, pass to ACTION when request is
complete.  ACTION is invoked asynchronously with the current
buffer set to the original buffer when command was invoked.

If PROMPT is a function, call it with ARG and return result.
Else grab prompt from region, or entire buffer if no region, and
prefix with PROMPT string.

If HISTORYP is t pass the `robby-history' conversation history
to the OpenAI request.

OPTIONS is property list list of options to pass to the OpenAI
API.

OUTPUT-BUFFER is the output buffer to put response in for buffer
commands. Defaults to current buffer."
  (let* ((prompt-and-prompt-prefix (robby--get-prompt prompt arg))
         (basic-prompt (car prompt-and-prompt-prefix))
         (prompt-prefix (cdr prompt-and-prompt-prefix))
         (complete-prompt (robby--request-input (intern robby-api) basic-prompt historyp)))
    (message "Awaiting AI overlords…")
    (setq robby--last-command-options
          `(:historyp ,historyp :prompt ,prompt-prefix :action ,action))
    (let ((buffer (current-buffer))
          (beg (if (use-region-p) (region-beginning) (point-min)))
          (end (if (use-region-p) (region-end) (point-max))))
      (robby--request
       complete-prompt
       basic-prompt
       historyp
       options
       beg
       end
       (lambda (text beg end)
         (with-current-buffer buffer
           (message nil)
           (if (robby--preview-p arg)
               (robby--show-response-in-help-window text beg end)
             (funcall action text beg end output-buffer))
           (run-hooks 'robby-command-complete-hook)))))))

;;;###autoload (autoload 'robby-define-command "robby" "Define a custom robby command." nil t)
(cl-defmacro robby-define-command (name
                                   docstring
                                   &key
                                   historyp
                                   prompt
                                   action
                                   options
                                   output-buffer)
  "Define a new Robby command.

NAME is the command name, a symbol.
DOCSTRING is the documentation string for the new command.

Keyword parameters:

HISTORYP - include conversation history in OpenAI request if t.

PROMPT - Function or string.  If a function, command will call
with interactive prefix arg to obtain the prompt.  If a string,
grab prompt from region or entire buffer context if no region,
and prefix region text with PROMPT string to build prompt.

ACTION - function to invoke when request is complete.  The
function is passed the response text and the selected region, and
must be of the form `(TEXT BEG END)'.

OPTIONS - property list of options to pass to the OpenAI API. These
options are merged in with the customization options specified in
the api customization group, either `robby-chat-api' or
`robby-completions-api'.

OUTPUT-BUFFER is the output buffer to put response in for buffer
commands. Defaults to current buffer."
  `(defun ,name (arg)
     ,docstring
     (interactive "P")
     (robby--run-command
      :arg arg
      :historyp ,historyp
      :prompt ',prompt
      :action #',action
      :options ,options
      :output-buffer ,output-buffer)))

(provide 'robby-define-command)

;;; robby-define-command.el ends here
