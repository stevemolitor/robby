;;; robby-customization.el  --- robby customization options  -*- lexical-binding:t -*-

;;; Commentary:

;; Robby customization variables and groups.

;;; Code:

(require 'spinner)

(require 'robby-api-key)
(require 'robby-validation)
(require 'robby-utils)

;;; function to validate custom api options
(defun robby--validate-custom-api-option (name)
  "Validate that option NAME is within its allowed range of values.

Return an error message if the value is invalid, or nil if it is valid."
  (lambda (widget)
    (let* ((value (widget-value widget))
           (err-msg (robby--validate name value)))
      (when err-msg
        (widget-put widget :error (format "Invalid value for %s. %s" name err-msg))
        widget))))

;;; general settings
(defgroup robby nil
  "Simple AI Integration for Emacs."
  :group 'tools
  :tag "robby")

(defcustom robby-logging nil
  "Log to *robby-log* buffer if t."
  :type 'boolean
  :group 'robby)

(defcustom robby-max-history 2
  "Max number of previous prompt/response pairs in history."
  :type 'integer
  :group 'robby)

(defcustom robby-stream-p t
  "Stream responses from OpenAI if t."
  :type  'boolean
  :group 'robby)

(defcustom robby-use-curl t
  "If curl if availble to make HTTP requests to OpenAI.

Set to nil to always use Emacs\\=' built-in url.el library instead
of curl. If curl is not available robby will still fallback to
url.el even if `robby-use-curl' is t. Note that url.el does not
support streaming."
  :type 'boolean
  :group 'robby)

(defcustom robby-prompt-spec-fn #'robby-make-prompt-spec
  "Function that returns a prompt format spec.

The function takes two arguments, FILE-NAME and FILE-EXT.
FILE-NAME and FILE-EXT will be set to the base file name and file
extension of the file associated with the current buffer, or to
nil if the buffer has no associated file.

The function return an association list suitable for use with
`format-spec'."
  :type 'function
  :group 'robby)

(defcustom robby-spinner 'rotating-line
  "What kind of spinner to use to show progress."
  :type `(choice :tag "Spinner type"
                 ,@(mapcar (lambda (c) (list 'const (car c)))
                           spinner-types))
  :group 'robby)

(defcustom robby-spinner-lighter-format
  " robby %s"
  "Mode line lighter format for robby-spinner-mode.

It should include a `%s' placeholder for the spinner."
  :type 'string
  :group 'robby)

(defcustom robby-chat-system-message "You are an AI tool embedded within Emacs. Assist users with their tasks and provide information as needed. Do not engage in harmful or malicious behavior. Please provide helpful information. Answer concisely."
  "System message to use with OpenAI Chat API."
  :type 'string
  :group 'robby)

(defcustom robby-api-url "https://api.openai.com/v1/chat/completions"
  "URL to use for OpenAI API requests."
  :type 'string
  :group 'robby)

;;; chat api options
(defgroup robby-chat-api nil
  "Options to pass to the chat API."
  :group 'robby)

(defcustom robby-chat-model "gpt-3.5-turbo"
  "The model to use with the completions API."
  :type 'string
  :group 'robby-chat-api)

(defcustom robby-chat-max-tokens 2000
  "The maximum number of tokens to generate in the completion."
  :type '(choice integer (const nil))
  :group 'robby-chat-api)

(defcustom robby-chat-temperature nil
  "What sampling temperature to use, a number between 0.0 and 2.0.

Defaults to 1."
  :type `(choice (number :validate ,(robby--validate-custom-api-option 'chat-temperature)) (const nil))
  :group 'robby-chat-api)

(defcustom robby-chat-top-p nil
  "Chat API top-p value.

An alternative to sampling with temperature, called nucleus
sampling, where the model considers the results of the tokens
with top_p probability mass. So 0.1 means only the tokens
comprising the top 10% probability mass are considered. Valid
range is 0.0 to 2.0."
  :type `(choice (number :validate ,(robby--validate-custom-api-option 'chat-top-p)) (const nil))
  :group 'robby-chat-api)

(defcustom robby-chat-n nil
  "How many completions to generate for each prompt. Defaults to 1."
  :type '(choice number (const nil))
  :group 'robby-chat-api)

(defcustom robby-chat-stop nil
  "OpenAI stop sequence.

Up to 4 sequences where the API will stop generating further
tokens. The returned text will not contain the stop sequence."
  :type '(choice string (const nil)) ;; todo handle arrays of strings
  :group 'robby-chat-api)

(defcustom robby-chat-presence-penalty nil
  "OpenAI presence penalty.

Number between -2.0 and 2.0. Positive values penalize new tokens
based on whether they appear in the text so far, increasing the
model\\='s likelihood to talk about new topics."
  :type `(choice (number :validate ,(robby--validate-custom-api-option 'chat-presence-penalty)) (const nil))
  :group 'robby-chat-api)

(defcustom robby-chat-frequency-penalty nil
  "OpenAI API frequency penalty.

Number between -2.0 and 2.0. Positive values penalize new tokens
based on their existing frequency in the text so far, decreasing
the model\\='s likelihood to repeat the same line verbatim."
  :type `(choice (number :validate ,(robby--validate-custom-api-option 'chat-frequency-penalty)) (const nil))
  :group 'robby-chat-api)

(defcustom robby-chat-user nil
  "OpenAI API chat user.

A unique identifier representing your end-user, which can help
OpenAI to monitor and detect abuse."
  :type '(choice string (const nil))
  :group 'robby-chat-api)

(provide 'robby-customization)

;;; robby-customization.el ends here
