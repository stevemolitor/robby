;;; robby-define-command.el  --- robby-define-command macro  -*- lexical-binding:t -*-

;;; Commentary:

;; Provides the robby-define-command macro, used to define custom robby commands

;;; Code:

(require 'robby-run-command)

(cl-defmacro robby-define-command (name
                                   docstring
                                   &key
                                   prompt
                                   prompt-args
                                   action
                                   action-args
                                   api-options
                                   grounding-fns
                                   no-op-pattern
                                   no-op-message
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

PROMPT-ARGS - plist of arguments to pass to the prompt function.

ACTION - function to invoke when request is complete.  The
function is passed the response text and the selected region, and
must be of the form (TEXT BEG END).

ACTION-ARGS - plist of arguments to pass to the action function.

API-OPTIONS - property list of options to pass to the OpenAI
API. These options are merged in with the customization options
specified in the api customization group, either `robby-chat-api'
or `robby-completions-api'.

GROUNDING-FNS - Format the response from OpenAI before returning
it. Only used if `NEVER-STREAM-P' is t.

NO-OP-PATTERN - If the response matches this regular expression,
do not perform the action. Useful with a prompt that tells OpenAI
to respond with a certain response if there is nothing to do. For
example with a prompt of \"Fix this code. Respond with \\='the code
is correct\\=' if the code is correct\", then a NO-OP-PATTERN of
\"code is correct\" will tell robby to not replace the region
when the pattern matches. Only use NO-OP-PATTERN when
NEVER-STREAM-P is t.

NO-OP-MESSAGE - Message to display when NO-OP-PATTERN matches. Optional.

HISTORYP - include conversation history in OpenAI request if t.

NEVER-STREAM-P - Stream response if t. If present this value
overrides the `robby-stream' customization variable."
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
      :grounding-fns ,grounding-fns
      :no-op-pattern ,no-op-pattern
      :no-op-message ,no-op-message
      :historyp ,historyp
      :never-stream-p ,never-stream-p)))

(provide 'robby-define-command)

;;; robby-define-command.el ends here
