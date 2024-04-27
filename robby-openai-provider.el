;;; robby-openai-provider.el  --- robby openai provider  -*- lexical-binding:t -*-

;;; Code:

(require 'robby-provider)

(require 'cl-generic)

(robby-add-provider
 :symbol 'openai
 :name "OpenAI"
 :host "api.openai.com"
 :default-model "gpt-3.5-turbo"
 :models-path "/v1/models")

(cl-defmethod robby-provider-parse-models :around (data &context (robby-provider (eql 'openai)))
  (let ((models (cl-call-next-method data)))
    (seq-filter
     (lambda (name)
       (string-prefix-p "gpt" name))
     models)))

(provide 'robby-openai-provider)

;;; robby-openai-provider.el ends here
