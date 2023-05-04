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
      (funcall prompt)
    ;; String prompts come from custom commands. Force them to use
    ;; region, and prefix with supplied prompt:
    (let* ((region-text (robby--get-region-or-buffer-text))
           (prompt-with-prefix (format "%s\n%s" prompt region-text)))
      `(,prompt-with-prefix . ,prompt))))

;;;###autoload (autoload 'robby-define-command "robby" "Define a custom robby command." nil t)
(cl-defmacro robby-define-command (name
                                   docstring
                                   &key
                                   historyp
                                   prompt
                                   prompt-args
                                   action
                                   action-args
                                   api
                                   api-options)
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

API-OPTIONS - property list of options to pass to the OpenAI
API. These options are merged in with the customization options
specified in the api customization group, either `robby-chat-api'
or `robby-completions-api'."
  `(defun ,name (arg)
     ,docstring
     (interactive "P")
     (robby-run-command
      ;; :arg arg ;; TODO add back arg?
      :prompt ,prompt
      :prompt-args ,prompt-args
      :action ,action
      :historyp ,historyp
      :api ,api
      :api-options ,api-options)))

(provide 'robby-define-command)

;;; robby-define-command.el ends here
