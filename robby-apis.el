;;; robby-apis.el  --- api helper methods that dispatch on OpenAI API  -*- lexical-binding:t -*-

;;; Commentary:

;; Polymorphic API helper methods that dispatch on OpenAI API.

(require 'cl-generic)

(require 'robby-history)
(require 'robby-customization)

;;; Code:

;;; completions methods
(cl-defmethod robby--request-url ((api (eql 'completions)))
  "Return OpenAI completions API URL."
  "https://api.openai.com/v1/completions")

(cl-defmethod robby--request-input ((api (eql 'completions)) prompt historyp)
  "Return OpenAI completions API input data including PROMPT.
Also include prompt history if HISTORYP is true."
  (let* ((all-messages (if historyp
                           (append robby--history `(,prompt))
                         `(,prompt)))
         (prompt-with-history
          (string-join (flatten-list all-messages) "\n")))
    `((prompt . ,prompt-with-history))))

(cl-defmethod robby--parse-response ((api (eql 'completions)) data)
  "Parse OpenAI completions API from response DATA."
  (string-trim (or (assoc-default 'text (seq-first (assoc-default 'choices data))) "")))


;;; chat methods
(cl-defmethod robby--request-url ((api (eql 'chat)))
  "Return OpenAI chat API URL."
  "https://api.openai.com/v1/chat/completions")

(cl-defmethod robby--request-input ((api (eql 'chat)) prompt historyp)
  "Return OpenAI chat API input data including PROMPT.
Also include prompt history if HISTORYP is true."
  (let* ((system-message `((role . "system") (content . ,robby-chat-system-message)))
         (formatted-messages
          (if historyp
              (vconcat
               `(,system-message)
               (seq-reduce
                (lambda (vec history-elem)
                  (vconcat
                   vec
                   `(((role . "user") (content . ,(car history-elem)))
                     ((role . "assistant") (content . ,(cdr history-elem))))))
                robby--history
                '[])
               `(((role . "user") (content . ,prompt))))
            `[,system-message ((role . "user") (content . ,prompt))])))
    `((messages . ,formatted-messages))))

(cl-defmethod robby--parse-response ((api (eql 'chat)) data)
  "Parse OpenAI chat API from response DATA."
  (string-trim (or (assoc-default 'content (assoc-default 'message (seq-first (assoc-default 'choices data)))) "")))

(provide 'robby-apis)

;;; robby-apis.el ends here
