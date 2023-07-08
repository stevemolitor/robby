;;; robby-request.el  --- make robby OpenAI requests  -*- lexical-binding:t -*-

;;; Commentary:

;; Make HTTP requests to OpenAI API.

(require 'json)
(require 'request)

;;; Code:
(cl-defun robby--request (&key url payload success error)
  "Make HTTP request to OpenAI API.

Request is made to URL, JSON-encoding and posting payload.

On a successful request call the SUCCESS callback. The callback
must be of the form `(:data data)', where `data' is the parsed
JSON object in the response.

Call `error' on request error. The error callback must be of the
form `(:error-thrown error-thrown :data data :symbol-status)'.
"
  (request
    url
    :type "POST"
    :headers `(("Content-Type" . "application/json")
               ("Authorization" . ,(format "Bearer %s" (robby--get-api-key))))
    :data (json-encode payload)
    :parser 'json-read
    :success success
    :error error))

(provide 'robby-request)

;;; robby-request.el ends here
