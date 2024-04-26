;;; robby-mistralai-provider.el  --- robby mistralai provider  -*- lexical-binding:t -*-

;;; Code:

(require 'robby-provider)

(require 'cl-generic)

(robby-add-provider
 :symbol 'mistralai
 :name "Mistral AI"
 :host "api.mistral.ai"
 :default-model "mistral-small"
 :models-path "/v1/models")

;; example:
;; ((object . "error") (message . "Invalid model: gpt-4-turbo-preview") (type . "invalid_model") (param) (code . "1500"))
(cl-defmethod robby-provider-parse-error :around (data &context (robby-provider (eql 'mistralai)))
  "Parse mistralai error from response DATA."
  (let ((object (alist-get 'object data)))
    (when (and (stringp "error") (string= object "error"))
      (alist-get 'message data))))

(provide 'robby-mistralai-provider)

;;; robby-mistralai-provider.el ends here
