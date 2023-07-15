;;; robby-define-command.el  --- macro to define robby custom commands  -*- lexical-binding:t -*-

;;; Commentary:

;; Provides `robby-define-command' function, used to define custom
;; Robby commands, and internal helper functions used by
;; `robby-define-command'.

(require 'cl-lib)
(require 'map)

(require 'robby-run-command)

;;; Code:

(defun robby--handle-prefix-args (arg prompt-args)
  "Mix in appropriate values into PROMPT-ARGS based on prefix ARG.

If there is prefix arg, never prompt for input in a command."
  (if arg
      (map-merge 'plist prompt-args '(:never-ask-p t))
    prompt-args))

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

NEVER-STREAM-P - Stream reponse if t. if present this value overrides
the `robby-stream' customization variable."
  `(defun ,name (arg)
     ,docstring
     (interactive "P")
     (robby-run-command
      :prompt ,prompt
      :prompt-args (robby--handle-prefix-args arg ,prompt-args)
      :action ,action
      :action-args ,action-args
      :api ,api
      :api-options ,api-options
      :historyp ,historyp
      :never-stream-p ,never-stream-p
      )))

(provide 'robby-define-command)

;;; robby-define-command.el ends here
