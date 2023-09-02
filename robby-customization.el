;;; robby-customization.el  --- robby customization options  -*- lexical-binding:t -*-

;;; Commentary:

;; Robby customization variables and groups.

;;; Code:

;;; general settings
(defcustom robby-openai-api-key #'robby--get-api-key-from-auth-source
  "OpenAI API key.

A string, or a function that returns the API key."
  :group 'robby
  :type '(choice
          (string :tag "OpenAI API key")
          (function :tag "Function that returns the OpenAI API key")))

(defgroup robby nil
  "Simple AI Integration for Emacs."
  :group 'tools
  :tag "robby")

(defcustom robby-logging nil
  "Log to *robby-log* buffer if t."
  :type 'boolean
  :group 'robby)

(defcustom robby-max-history 2
  "The maximum number of previous prompt / response pairs to keep in
the conversation history."
  :type 'integer
  :group 'robby)

(defcustom robby-stream-p t
  "Stream responses from OpenAI if t"
  :type  'boolean
  :group 'robby)

(defcustom robby-api "chat"
  "Which OpenAI model to use.

Use the customization interface or `custom-set-variables to
change this value, as it needs to clear out the history to avoid
incompatible history between the two apis."
  :type '(choice (const :tag "Completions" "completions")
                 (const :tag "Chat" "chat"))
  :group 'robby)

(defcustom robby-use-curl t
  "If curl if availble to make HTTP requests to OpenAI.

Set to nil to always use Emacs' built-in url.el library instead
of curl. If curl is not available robby will still fallback to
url.el even if `robby-use-curl' is t. Note that url.el does not
support streaming."
  :type 'boolean
  :group 'robby)

(defcustom robby-confirm-whole-buffer-p t
  "If true, confirm before sending the entire buffer as the prompt."
  :type 'boolean
  :group 'robby)

;;; completions api options
(defgroup robby-completions-api nil
  "Options to pass to the completions API."
  :group 'robby)

(defcustom robby-completions-model "text-davinci-003"
  "The model to use with the completions API."
  :type :string
  :group 'robby-completions-api)

(defcustom robby-completions-suffix nil
  "The suffix that comes after a completion of inserted text."
  :type '(choice string (const nil))
  :group 'robby-completions-api)

(defcustom robby-completions-max-tokens 300
  "The maximum number of tokens to generate in the completion."
  :type '(choice integer (const nil))
  :group 'robby-completions-api)

(defcustom robby-completions-temperature nil
  "What sampling temperature to use.  Defaults to 1."
  :type '(choice number (const nil))
  :group 'robby-completions-api)

(defcustom robby-completions-top-p nil
  "An alternative to sampling with temperature, called nucleus
sampling, where the model considers the results of the tokens
with top_p probability mass. So 0.1 means only the tokens
comprising the top 10% probability mass are considered."
  :type '(choice number (const nil))
  :group 'robby-completions-api)

(defcustom robby-completions-n nil
  "How many completions to generate for each prompt. Defaults to 1."
  :type '(choice integer (const nil))
  :group 'robby-completions-api)

(defcustom robby-completions-presence-penalty nil
  "Number between -2.0 and 2.0. Positive values penalize new tokens
based on whether they appear in the text so far, increasing the
model's likelihood to talk about new topics."
  :type '(choice number (const nil))
  :group 'robby-completions-api)

(defcustom robby-completions-frequency-penalty nil
  "Number between -2.0 and 2.0. Positive values penalize new tokens
based on their existing frequency in the text so far, decreasing
the model's likelihood to repeat the same line verbatim."
  :type  '(choice integer (const nil))
  :group 'robby-completions-api)

(defcustom robby-completions-best-of nil
  "Generates best_of completions server-side and returns the
\"best\" (the one with the highest log probability per
token). Results cannot be streamed."
  :type '(choice integer (const nil))
  :group 'robby-completions-api)

(defcustom robby-completions-logit-bias nil
  "Generates best_of completions server-side and returns the
\"best\" (the one with the highest log probability per
token). Results cannot be streamed."
  :type '(choice integer (const nil))
  :group 'robby-completions-api)

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

(defcustom robby-chat-max-tokens 300
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

(defcustom robby-chat-logprobs nil
  "Include the log probabilities on the logprobs most likely tokens,
as well the chosen tokens. For example, if logprobs is 5, the API
will return a list of the 5 most likely tokens. The API will
always return the logprob of the sampled token, so there may be
up to logprobs+1 elements in the response."
  :type '(choice integer (const nil))
  :group 'robby-chat-api)

(defcustom robby-chat-stop nil
  "Up to 4 sequences where the API will stop generating further
tokens. The returned text will not contain the stop sequence."
  :type '(choice string (const nil)) ;; todo handle arrays of strings
  :group 'robby-chat-api)

(defcustom robby-chat-presence-penalty nil
  "Number between -2.0 and 2.0. Positive values penalize new tokens
based on whether they appear in the text so far, increasing the
model's likelihood to talk about new topics."
  :type '(choice number (const nil))
  :group 'robby-chat-api)

(defcustom robby-chat-frequency-penalty nil
  "Number between -2.0 and 2.0. Positive values penalize new tokens
based on their existing frequency in the text so far, decreasing
the model's likelihood to repeat the same line verbatim."
  :type '(choice number (const nil))
  :group 'robby-chat-api)

(defcustom robby-chat-best-of nil
  "Generates best_of completions server-side and returns the
\"best\" (the one with the highest log probability per
token). Results cannot be streamed."
  :type '(choice number (const nil))
  :group 'robby-chat-api)

(defcustom robby-chat-user nil
  "A unique identifier representing your end-user, which can help
OpenAI to monitor and detect abuse."
  :type '(choice string (const nil))
  :group 'robby-chat-api)

(defcustom robby-chat-system-message "You are a large language model living in Emacs and a helpful assistant. Respond concisely."
  "System message used with Chat API"
  :type 'string
  :group 'robby)

(defcustom robby-models nil
  "The list of available Chat GPT models.

If nil robby will fetch the list from OpenAI the first time the
API options transient is opened in an Emacs session. You can
instead hard code the list of models to avoid that HTTP request.")

(provide 'robby-customization)

;;; robby-customization.el ends here
