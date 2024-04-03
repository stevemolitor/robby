;;; robby-providers.el  --- AI Provider Backends  -*- lexical-binding:t -*-

;;; Commentary:

;; Defines a generic function and implementions to customize robby for
;; various AI backends.

;;; Code:

;; declared in robby-customization.el
(defvar robby-provider)
(defvar robby-providers-settings)

(defvar robby--providers-settings
  '((openai
     . (:host "api.openai.com"
              :default-model "gpt-3.5-turbo"
              :api-base-path "/v1"
              :models-filter-re "\\`gpt"))
    (mistral
     . (:host "api.mistral.ai"
              :default-model "mistral-small-latest"
              :api-base-path "/v1"))
    (togetherai
     . (:host "api.together.xyz"
              :default-model "togethercomputer/StripedHyena-Nous-7B"
              :api-base-path "/v1")))
  "Association of AI providers and their settings.")

;;; Functions that can retreive settings from the provider settings
(defun robby--get-provider-settings ()
  "Return the settings of the current provider."
  (alist-get robby-provider robby--providers-settings))

(defun robby--providers-host ()
  "Return the host of the current provider."
  (plist-get (robby--get-provider-settings) :host))

(defun robby--providers-default-model ()
  "Return the default model to use with the the current provider."
  (plist-get (robby--get-provider-settings) :default-model))

(defun robby--providers-api-base-path ()
  "Return the host of the current provider."
  (plist-get (robby--get-provider-settings) :api-base-path))

(defun robby--providers-models-filter-re ()
  "Return regexp to filter models by for the current provider.

Nil means do not filter the models list. OpenAI returns models
that are not usable with the chat API, so we have to filter to
gpt*."
  (plist-get (robby--get-provider-settings) :models-filter-re))

;;; parse-error
;; a generic method to parse the error from the response when the curent value of robby-provider is 'mistral
(cl-defgeneric robby--providers-parse-error (data)
  "Get error from response DATA.

Different providers have different response formats for errors.")

(cl-defmethod robby--providers-parse-error (data)
  "Get error from response DATA."
  (cdr (assoc 'message (assoc 'error data))))

(cl-defmethod robby--providers-parse-error (data &context (robby-provider (eql 'mistral)))
  "Get error from response DATA.

This is a specific implementation when ROBBY-PROVIDER is the `mistral' provider."
  (let ((object (cdr (assoc 'object data))))
    (if (string= object "error")
        (cdr (assoc 'message data))
      nil)))

(provide 'robby-providers)

;;; robby-providers.el ends here
