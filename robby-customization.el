;;; robby-customization.el  --- robby customization options  -*- lexical-binding:t -*-

;;; Commentary:

;; Robby customization variables and groups.

(require 'spinner)

(require 'robby-api-key)

;;; Code:
(declare-function robby-make-prompt-spec "robby-utils" (file-name file-ext))

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
  "The maximum number of previous prompt/response pairs to keep in
the conversation history."
  :type 'integer
  :group 'robby)

(defcustom robby-stream-p t
  "Stream responses from OpenAI if t"
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

(defcustom robby-confirm-whole-buffer-p t
  "If true, confirm before sending the entire buffer as the prompt."
  :type 'boolean
  :group 'robby)

(defcustom robby-prompt-spec-fn #'robby-make-prompt-spec
  "Function that returns a prompt format spec.

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

;;; chat api options
(defgroup robby-chat-api nil
  "Options to pass to the chat API."
  :group 'robby)

(defcustom robby-chat-model "gpt-3.5-turbo"
  "The model to use with the completions API."
  :type 'string
  :group 'robby-chat-api)

(defcustom robby-chat-suffix nil
  "The suffix that comes after a completion of inserted text."
  :type '(choice string (const nil))
  :group 'robby-chat-api)

(defcustom robby-chat-max-tokens 2000
  "The maximum number of tokens to generate in the completion."
  :type '(choice integer (const nil))
  :group 'robby-chat-api)

(defun robby--validate-temperature (widget)
  (let ((temp (widget-value widget)))
    (unless (and (>= temp 0) (<= temp 1))
      (widget-put widget :error (format "Invalid temperature %d, must be between 0 and 1 inclusive" temp))
      widget)))

(defcustom robby-chat-temperature nil
  "What sampling temperature to use. Defaults to 1."
  :type '(choice (number :validate robby--validate-temperature) (const nil))
  :group 'robby-chat-api)

(defcustom robby-chat-top-p nil
  "An alternative to sampling with temperature, called nucleus
sampling, where the model considers the results of the tokens
with top_p probability mass. So 0.1 means only the tokens
comprising the top 10% probability mass are considered."
  :type '(choice number (const nil))
  :group 'robby-chat-api)

(defcustom robby-chat-n nil
  "How many completions to generate for each prompt. Defaults to 1."
  :type '(choice number (const nil))
  :group 'robby-chat-api)

(defcustom robby-chat-stop nil
  "Up to 4 sequences where the API will stop generating further
tokens. The returned text will not contain the stop sequence."
  :type '(choice string (const nil)) ;; todo handle arrays of strings
  :group 'robby-chat-api)

(defcustom robby-chat-presence-penalty nil
  "Number between -2.0 and 2.0. Positive values penalize new tokens
based on whether they appear in the text so far, increasing the
model\\='s likelihood to talk about new topics."
  :type '(choice number (const nil))
  :group 'robby-chat-api)

(defcustom robby-chat-frequency-penalty nil
  "Number between -2.0 and 2.0. Positive values penalize new tokens
based on their existing frequency in the text so far, decreasing
the model\\='s likelihood to repeat the same line verbatim."
  :type '(choice number (const nil))
  :group 'robby-chat-api)

(defcustom robby-chat-user nil
  "A unique identifier representing your end-user, which can help
OpenAI to monitor and detect abuse."
  :type '(choice string (const nil))
  :group 'robby-chat-api)

(defcustom robby-chat-system-message "You are an AI tool embedded within Emacs. Assist users with their tasks and provide information as needed. Do not engage in harmful or malicious behavior. Please provide helpful information. Answer concisely."
  "System message used with Chat API"
  :type 'string
  :group 'robby)

(provide 'robby-customization)

;;; robby-customization.el ends here
