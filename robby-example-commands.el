;;; robby-example-commands.el  --- Robby example commands  -*- lexical-binding:t -*-

;;; Commentary:

;; Example Robby commands defined using `define-robby-command'.

(require 'robby-run-command)
(require 'robby-prompts)
(require 'robby-actions)
(require 'robby-grounding-fns)

;;; Code:

;;;###autoload (autoload 'robby-write-tests "robby-commands" "Write some tests for the code in the region, append to region." t)
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

;;;###autoload (autoload 'robby-add-comment "robby-commands" "Add a comment for the code in the selected region or buffer." t)
(robby-define-command
 robby-add-comment
 "Add a comment for the code in the selected region or buffer."
 :historyp nil
 :never-stream-p t
 :prompt #'robby-get-prompt-from-region
 :prompt-args '(:prompt-prefix
                "The file extension for this code is \"%e\". First, determine the programming language based on the file extension. Then, write a documentation comment for the code delimted by triple backticks, formatted with the appropriate comment delimeters for the programming language, and based on the type of thing it is - code block, function definition, class definition, etc. Return the entire comment inside a markdown code fence, delimited by triple backticks. Here is the code: ```"
                :prompt-suffix "```")
 :action #'robby-prepend-response-to-region
 :grounding-fns #'(robby-extract-fenced-text robby-remove-trailing-end-of-line))

;;;###autoload (autoload 'robby-fix-code "robby-commands" "Fix code in region." t)
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
 :grounding-fns #'(robby-extract-fenced-text robby-remove-trailing-end-of-line)
 :no-op-pattern (rx (or "the code is correct" "the original code is correct")))

;;;###autoload (autoload 'robby-proof-read "robby-commands" "Proof read text." t)
(robby-define-command
 robby-proof-read
 "Proof read the text in the selected region.

Preview changes in a diff buffer when invoked with a prefix argument."
 :never-stream-p t
 :historyp nil
 :prompt #'robby-get-prompt-from-region
 :prompt-args '(:prompt-prefix
                "Please proof read the text delimited by triple backticks. Preserve the original formatting including line breaks or end of line characters.\n```"
                :prompt-suffix
                "```")
 :action #'robby-replace-region-with-response
 :grounding-fns #'robby-extract-fenced-text)

;;;###autoload (autoload 'robby-describe-code "robby-commands" "Describe code in the selected region, show description in robby view window." t)
(robby-define-command
 robby-describe-code
 "Describe code in the selected region, show description in robby view window."
 :historyp nil
 :never-stream-p t
 :prompt #'robby-get-prompt-from-region
 :prompt-args '(:prompt-prefix
                "Describe the code delimited by triple backticks.\n```"
                :prompt-suffix
                "```")
 :action #'robby-respond-with-robby-view
 :api-options '(:max-tokens 2000)
 :grounding-fns #'robby-extract-fenced-text)

;;;###autoload (autoload 'robby-summarize "robby-commands" "Summarize text." t)
(robby-define-command
 robby-summarize
 "Summarize the text in the selected region or entire buffer if no
selected region, show description in robby view window."
 :historyp nil
 :never-stream-p t
 :prompt #'robby-get-prompt-from-region
 :prompt-args '(:prompt-prefix
                "Briefly summarize the text delimited by triple backticks.\n```"
                :prompt-suffix
                "```")
 :action #'robby-respond-with-robby-view
 :api-options '(:max-tokens 2000)
 :grounding-fns #'robby-extract-fenced-text)

(provide 'robby-example-commands)

;; example-commands.el ends here
