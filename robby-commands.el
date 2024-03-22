;;; robby-commands.el  --- robby autoloaded custom commands  -*- lexical-binding:t -*-

;;; Commentary:

;; Built in Robby commands.

;;; Code:

(require 'robby-define-command)
(require 'robby-prompts)
(require 'robby-actions)
(require 'robby-grounding-fns)
(require 'robby-spinner)


;;; Core Commands

;;;; robby-chat
;;;###autoload (autoload 'robby-chat "robby" "Query AI from minibuffer, respond in robby-chat-mode buffer." t)
(robby-define-command
 robby-chat
 "Query AI from minibuffer, respond in robby-chat-mode buffer."
 :prompt #'robby-get-prompt-from-minibuffer
 :action #'robby-respond-with-robby-chat
 :historyp t)

;;;; robby-chat-from-region
;;;###autoload (autoload 'robby-chat-from-region "robby" "Query AI from minibuffer, respond in robby-chat-mode buffer." t)
(robby-define-command
 robby-chat-from-region
 "Query AI from region, respond in robby-chat-mode buffer."
 :prompt #'robby-get-prompt-from-region
 :action #'robby-respond-with-robby-chat-without-prompt
 :historyp t)

;;;; robby-message
;;;###autoload (autoload 'robby-message "robby" "Query AI from minibuffer, respond with message." t)
(robby-define-command
 robby-message
 "Query AI from minibuffer, respond with message."
 :prompt #'robby-get-prompt-from-minibuffer
 :action #'robby-respond-with-message
 :grounding-fns #'robby-format-message-text
 :historyp t
 :never-stream-p t)

;;;; robby-prepend-region
;;;###autoload (autoload 'robby-prepend-region "robby" "Query AI from region or entire buffer if no selected region, prepend results to region or buffer." t)
(robby-define-command
 robby-prepend-region
 "Query AI from region or entire buffer if no selected region,
prepend results to region or buffer."
 :prompt #'robby-get-prompt-from-region
 :action #'robby-prepend-response-to-region
 :grounding-fns #'robby-extract-fenced-text-in-prog-modes
 :never-stream-p t)

;;;; robby-append-region
;;;###autoload (autoload 'robby-append-region "robby" "Query AI from region or entire buffer if no selected region, append results to region or buffer." t)
(robby-define-command
 robby-append-region
 "Query AI from region or entire buffer if no selected region,
append results to region or buffer."
 :prompt #'robby-get-prompt-from-region
 :action #'robby-append-response-to-region
 :grounding-fns #'robby-extract-fenced-text-in-prog-modes
 :never-stream-p t)

;;;; robby-replace-region
;;;###autoload (autoload 'robby-replace-region "robby" "Query AI from region or entire buffer if no selected region, replace region with response." t)
(robby-define-command
 robby-replace-region
 "Query AI from region or entire buffer if no selected region,
replace region with response.

If prefix arg is supplied, confirm changes in a diff buffer
before applying."
 :prompt #'robby-get-prompt-from-region
 :action #'robby-replace-region-with-response
 :grounding-fns #'robby-extract-fenced-text-in-prog-modes
 :never-stream-p t)

;;; Example Commands for Specific Tasks

;;;; robby-write-tests
;;;###autoload (autoload 'robby-write-tests "robby" "Write some tests for the code in the region, append to region." t)
(robby-define-command
 robby-write-tests
 "Write some tests for the code in the region, append to region."
 :historyp nil
 :prompt #'robby-get-prompt-from-region
 :prompt-args '(:prompt-prefix
                "Write some unit tests for the code delimited by triple backticks. Return the test code inside markdown a code fence delmited by triple backticks.\n ```"
                :prompt-suffix
                "```")
 :action #'robby-append-response-to-region
 :grounding-fns #'robby-extract-fenced-text)

;;;; robby-add-comment
;;;###autoload (autoload 'robby-add-comment "robby" "Add a comment for the code in the selected region or buffer." t)
(robby-define-command
 robby-add-comment
 "Add a comment for the code in the selected region or buffer."
 :historyp nil
 :never-stream-p t
 :prompt #'robby-get-prompt-from-region
 :prompt-args '(:prompt-prefix
                "The file extension for this code is \"%e\". First, determine the programming language based on the file extension. Then, write a documentation comment for the code delimted by triple backticks, formatted with the appropriate comment delimeters for the programming language, and based on the type of thing it is - code block, function definition, class definition, etc. Return the entire comment inside a markdown code fence, delimited by triple backticks. Do not return the original code in your response. Here is an example response for a Lisp function definition:

```
;; Adds two numbers, a and b
```

Here is the code: ```"
                :prompt-suffix "```")
 :action #'robby-prepend-response-to-region
 :grounding-fns #'robby-extract-fenced-text)

;;;; robby-fix-code
;;;###autoload (autoload 'robby-fix-code "robby" "Fix code in region." t)
(robby-define-command
 robby-fix-code
 "Fix code in the selected region.

Preview changes in a diff buffer when invoked with a prefix argument."
 :historyp nil
 :never-stream-p t
 :prompt #'robby-get-prompt-from-region
 :prompt-args '(:prompt-prefix
                "Fix the code delimited by triple backticks. Return the corrected code inside markdown a code fence, for example ```var x = 1;```. If the original code supplied was correct respond with 'the code is correct', but only if the original code was correct. For example, if the code is correct then respond with:

\"the code is correct\"

However if the code is NOT correct, respond with the fixed code and do NOT use the word \"correct\" in your response if the code is not correct. Never use the word \"correct\" unless the original code was correct. Here is the code to correct:\n```"
                :prompt-suffix "```")
 :action #'robby-replace-region-with-response
 :grounding-fns #'robby-extract-fenced-text
 :no-op-pattern (rx (or "the code is correct" "the original code is correct")))

;;;; robby-proof-read
;;;###autoload (autoload 'robby-proof-read "robby" "Proof read text." t)
(robby-define-command
 robby-proof-read
 "Proof read the text in the selected region.

Preview changes in a diff buffer when invoked with a prefix argument."
 :never-stream-p t
 :historyp nil
 :prompt #'robby-get-prompt-from-region
 :prompt-args '(:prompt-prefix
                "Please proof read the text delimited by triple backticks. Preserve the original formatting including line breaks or end of line characters Just respond with the corrected text only, nothing else.\n```"
                :prompt-suffix
                "```")
 :action #'robby-replace-region-with-response
 :grounding-fns #'robby-extract-fenced-text)

;;;; robby-describe-code
;;;###autoload (autoload 'robby-describe-code "robby" "Describe code in the selected region, show description in robby view window." t)
(robby-define-command
 robby-describe-code
 "Describe code in the selected region, show description in robby view window."
 :historyp nil
 :prompt #'robby-get-prompt-from-region
 :prompt-args '(:prompt-prefix
                "Describe this code:\n")
 :action #'robby-respond-with-robby-chat-without-prompt
 :action-args `(:response-buffer "*robby*")
 :api-options '(:max-tokens 2000))

;;;; robby-summarize
;;;###autoload (autoload 'robby-summarize "robby" "Summarize the text in the selected region or entire buffer if no selected region, show description in robby view window." t)
(robby-define-command
 robby-summarize
 "Summarize the text in the selected region or entire buffer if no
selected region, show description in robby view window."
 :historyp nil
 :prompt #'robby-get-prompt-from-region
 :prompt-args '(:prompt-prefix
                "Briefly summarize the text delimited by triple backticks.\n```"
                :prompt-suffix
                "```")
 :action #'robby-respond-with-robby-chat-without-prompt
 :api-options '(:max-tokens 2000))

(cl-defun robby-get-prompt-from-git-diff (&key prompt-prefix &allow-other-keys)
  "Get a prompt with staged changes in the current git repository.

Return a prompt for a git commit message based on the staged
changes in the current git repository.

PROMPT-PREFIX is a string to prepend to the prompt."
  (let* ((dir (locate-dominating-file default-directory ".git"))
         (diff (shell-command-to-string (format "cd %s && git diff --staged" dir))))
    (format "%s\n%s" prompt-prefix diff)))

;;;; robby-git-commit-message
;;;###autoload (autoload 'robby-git-commit-message "robby" "Generate git commit message title from staged changes." t)
(robby-define-command
 robby-git-commit-message
 "Generate git commit message title from staged changes."
 :prompt
 #'robby-get-prompt-from-git-diff
 :action
 #'robby-prepend-response-to-region
 :prompt-args
 '(:prompt-prefix "For the following git diff, provide a concise and precise commit title capturing the essence of the changes in less than 50 characters.\n")
 :grounding-fns #'robby-remove-quotes
 :never-stream-p t)

(provide 'robby-commands)

;;; robby-commands.el ends here
