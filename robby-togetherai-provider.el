;;; robby-togetherai-provider.el  --- robby togetherai provider  -*- lexical-binding:t -*-

;;; Code:

(require 'robby-provider)

(require 'cl-generic)

(robby-add-provider
 :symbol 'togetherai
 :name "Together AI"
 :host "api.together.xyz"
 :default-model "togethercomputer/StripedHyena-Nous-7B"
 :models-path "/models/info?=")

(cl-defmethod robby-providers-parse-models (data &context (robby-provider (eql 'togetherai)))
  "Get models from response DATA for togetherai."
  (seq-map (lambda (elem) (alist-get 'name elem)) data))

(provide 'robby-togetherai-provider)

;;; robby-togetherai-provider.el ends here
