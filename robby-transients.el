;;; robby-transients-bak.el  --- Robby transient menu defintions  -*- lexical-binding:t -*-

;;; Commentary:

;; Robby transient menu definitions

(require 'cus-edit)
(require 'seq)
(require 'transient)

(require 'robby-actions)
(require 'robby-customization)
(require 'robby-prompts)
(require 'robby-run-command)
(require 'robby-utils)
(require 'robby-view)

;;; Code:

;;; Scope struct
(cl-defstruct
    (robby--scope
     (:constructor robby--scope-default)
     (:constructor robby--scope-set-selected-api
                   (&key
                    scope api
                    &aux
                    (selected-api api)
                    (api-options (robby--scope-api-options scope))
                    (robby-value (robby--scope-robby-value scope))))
     (:constructor robby--scope-set-api-options
                   (&key
                    scope selected-api-options
                    &aux
                    (selected-api (robby--scope-selected-api scope))
                    (api-options
                     (map-merge 'plist
                                (robby--scope-api-options scope)
                                `(,(robby--scope-selected-api scope) ,selected-api-options)))
                    (robby-value (robby--scope-robby-value scope))))
     (:constructor robby--scope-set-robby-value
                   (&key
                    scope value
                    &aux
                    (selected-api (robby--scope-selected-api scope))
                    (api-options (robby--scope-api-options scope))
                    (robby-value value))))
  (selected-api (robby--string-to-sym robby-api) :read-only t)
  (api-options nil :read-only t)
  (robby-value nil :read-only t))

(cl-defmethod robby--scope-selected-api-options (scope)
  (plist-get (robby--scope-api-options scope) (robby--scope-selected-api scope)))

;;; Util Functions
(defun robby--custom-type (symbol)
  (let ((type (get symbol 'custom-type)))
    (if (sequencep type)
        (nth 1 type)
      type)))

(defun robby--transient-args-to-options (args)
  (seq-reduce
   (lambda (plist arg)
     (let* ((scope (oref transient-current-prefix scope))
            (api (robby--scope-selected-api scope))
            (api-str (robby--sym-to-string api))
            (parts (split-string arg "="))
            (key-str (car parts)) (key-sym (intern (format ":%s" key-str)))
            (raw-value (cadr parts))
            (custom-var (intern (format "robby-%s-%s" api-str key-str)))
            (custom-type (robby--custom-type custom-var))
            (value (if (or (eq custom-type 'integer) (eq custom-type 'number)) (string-to-number raw-value) raw-value)))
       (plist-put plist key-sym value)))
   args
   '()))

(defun robby--select-api (api)
  "Helper function to select an API in robby transient."
  (let* ((scope (or (oref transient-current-prefix scope) (robby--scope-default)))
         (new-scope (robby--scope-set-selected-api :scope scope :api api))
         (robby-value (transient-args 'robby)))
    (transient-setup transient-current-command nil nil
                     :scope new-scope
                     :value robby-value)))

(defun robby--transient-api-description (api)
  (let* ((scope (oref transient--prefix scope))
         (api-name (capitalize (robby--sym-to-string api)))
         (robby-api-sym (robby--string-to-sym robby-api))
         (selected-api (or (and scope (robby--scope-selected-api scope)) robby-api-sym)))
    (if (eq selected-api api)
        (propertize api-name 'face 'transient-enabled-suffix)
      api-name)))

(defun robby--api-options-defaults (api)
  "Get default api options values for API from current customization
values."
  (let* ((api-name (robby--sym-to-string api))
         (custom-variables
          (seq-filter
           (lambda (var) (not (null (symbol-value var))))
           (seq-map #'car (custom-group-members (intern-soft (format "robby-%s-api" api-name)) nil))))
         (regexp (format "^robby-%s-" api-name)))
    (seq-map
     (lambda (var)
       (let ((key (replace-regexp-in-string regexp "" (symbol-name var))))
         (format "%s=%s" key (symbol-value var))))
     custom-variables)))

(defun robby--get-transient-prompt ()
  (let* ((args (transient-args transient-current-command))
         (from-region-p (transient-arg-value "-fromregion" args))
         (prompt (transient-arg-value "prompt=" args)))
    (if from-region-p
        #'robby-get-prompt-fromregion
      prompt)))

(defun robby--run-transient-command (action)
  (let* ((scope (or (oref transient-current-prefix scope) (robby--scope-default)))
         (api (robby--scope-selected-api scope))
         (api-str (robby--sym-to-string api))
         (api-options (robby--scope-selected-api-options scope))
         (args (transient-args transient-current-command))
         (simple-prompt (transient-arg-value "prompt=" args))
         (prompt-prefix (transient-arg-value "prompt-prefix=" args))
         (prompt-suffix (transient-arg-value "prompt-suffix=" args))
         (prompt-buffer (transient-arg-value "prompt-buffer=" args))
         (prompt (or simple-prompt #'robby-get-prompt-from-region))
         (prompt-args (if simple-prompt
                          '()
                        `(:prompt-prefix ,prompt-prefix :prompt-suffix ,prompt-suffix :buffer ,prompt-buffer :never-ask-p t)))
         (response-buffer (transient-arg-value "response-buffer=" args))
         (action-args `(:response-buffer ,response-buffer)))
    (robby-run-command
     :prompt prompt
     :prompt-args prompt-args
     :action action
     :action-args action-args
     :api api-str
     :api-options api-options)))

;;; Readers
(defun robby--read-buffer (prompt initial-input history)
  "Select a buffer."
  (interactive)
  (read-buffer "Select buffer: "))

(defun robby--read-decimal (prompt initial-input history)
  "Read a decimal number."
  (interactive)
  (format "%s" (read-number prompt)))

;;; Action Suffixes
(transient-define-suffix
  robby--respond-with-message-suffix ()
  (interactive)
  (robby--run-transient-command #'robby-respond-with-message))

(transient-define-suffix
  robby--respond-with-robby-view-suffix ()
  (interactive)
  (robby--run-transient-command #'robby-respond-with-robby-view))

(transient-define-suffix
  robby--respond-in-conversation-suffix ()
  (interactive)
  (robby--run-transient-command #'robby-respond-in-conversation))

(transient-define-suffix
  robby--prefix-region-with-response-suffix ()
  (interactive)
  (robby--run-transient-command #'robby-prepend-response-to-region))

(transient-define-suffix
  robby--append-response-to-region-suffix ()
  (interactive)
  (robby--run-transient-command #'robby-append-response-to-region))

(transient-define-suffix
  robby--replace-region-with-response-suffix ()
  (interactive)
  (robby--run-transient-command #'robby-replace-region-with-response))

;;; API Related Suffixes
(transient-define-suffix
  robby--select-chat-suffix ()
  "Select the Chat API."
  :transient 'transient--do-exit
  (interactive)
  (robby--select-api :chat))

(transient-define-suffix
  robby--select-completions-suffix ()
  "Select the Completions API."
  :transient 'transient--do-exit
  (interactive)
  (robby--select-api :completions))

(transient-define-suffix
  robby--apply-api-options ()
  :transient 'transient--do-exit
  (interactive)
  (let* ((scope (oref transient-current-prefix scope))
         (robby-value (robby--scope-robby-value scope))
         (args (transient-args transient-current-command))
         (api-options (robby--transient-args-to-options args))
         (new-scope (robby--scope-set-api-options :scope scope :selected-api-options api-options)))
    (transient-set)
    (transient-setup 'robby nil nil
                     :scope new-scope
                     :value robby-value)))

(transient-define-suffix
  robby--setup-api-options ()
  "Call appropriate API options prefix for API in scope.

The initial transient value comes from either any previously
edited options for the API, or default API options from
customization values."
  (interactive)
  (let* ((scope (or (oref transient-current-prefix scope) (robby--scope-default)))
         (selected-api (robby--scope-selected-api scope))
         (api-options (robby--scope-selected-api-options scope))
         (value (or (and api-options (robby--plist-to-transient-args api-options))
                    (robby--api-options-defaults selected-api)))
         (transient-name (format "robby--%s-api-options" (robby--sym-to-string selected-api))))
    (transient-setup (intern transient-name) nil nil :scope scope :value value)))

;;; API Options Prefixes
(transient-define-prefix
  robby--chat-api-options ()
  "Chat API options."
  ["Chat API Options"
   ("m" "model" "model="  :always-read t :choices ("gpt-3.5-turbo" "gpt-4"))
   ("s" "suffix" "suffix=" :always-read t)
   ("t" "max tokens" "max-tokens=" :reader transient-read-number-N+ :always-read t)
   ("e" "temperature" "temperature=" :reader robby--read-decimal :always-read t)
   ("p" "top p" "top-p=" :reader robby--read-decimal :always-read t)
   ("n" "n" "n=" :reader transient-read-number-N+ :always-read t)
   ("l" "log probabilities" "logprobs=" :reader transient-read-number-N+ :always-read t)
   ("o" "stop" "stop=" :always-read t)
   ("r" "presence penalty" "presence-penalty=" :reader robby--read-decimal :always-read t)
   ("f" "frequency penalty" "frequency-penalty=" :reader robby--read-decimal :always-read t)
   ("b" "best of" "best-of=" :reader transient-read-number-N+ :always-read t)
   ("u" "user" "user=" :always-read t)
   ""
   ("a" "apply options" robby--apply-api-options)])

(transient-define-prefix
  robby--completions-api-options ()
  "Completions API options."
  ["Completions API Options"
   ("m" "model" "model="  :always-read t)
   ("s" "suffix" "suffix=" :always-read t)
   ("t" "max tokens" "max-tokens=" :reader transient-read-number-N+ :always-read t)
   ("e" "temperature" "temperature=" :reader robby--read-decimal :always-read t)
   ("p" "top p" "top-p=" :reader robby--read-decimal :always-read t)
   ("n" "n" "n=" :reader transient-read-number-N+ :always-read t)
   ("r" "presence penalty" "presence-penalty=" :reader robby--read-decimal :always-read t)
   ("f" "frequency penalty" "frequency-penalty=" :reader robby--read-decimal :always-read t)
   ("l" "logit bias" "logit-bias=" :reader robby--read-decimal :always-read t)
   ""
   ("a" "apply options" robby--apply-api-options)])

;;; Robby transient
(transient-define-prefix robby ()
  "Invoke OpenAI Chat API."
  :incompatible '(("prompt=" "prompt-prefix=")
                  ("prompt=" "prompt-suffix=")
                  ("prompt=" "prompt-buffer="))

  [:class transient-row "API"
          ("c" "Chat" robby--select-chat-suffix
           :description (lambda () (robby--transient-api-description :chat)))
          ("o" "Completions" robby--select-completions-suffix
           :description (lambda () (robby--transient-api-description :completions)))
          ("A" "API options" robby--setup-api-options :transient transient--do-replace)]
  [["Prompt"
    ("S" "simple prompt" "prompt=" :always-read t)
    ("p" "prompt prefix" "prompt-prefix=" :always-read t)
    ("s" "prompt suffix" "prompt-suffix=" :always-read t)
    ("b" "prompt buffer" "prompt-buffer=" :reader robby--read-buffer :level 5)]]
  [["Action"
    ("x" "prefix region with response" robby--prefix-region-with-response-suffix)
    ("a" "append response to region" robby--append-response-to-region-suffix)
    ("r" "replace region with response" robby--replace-region-with-response-suffix)
    ("f" "response buffer" "response-buffer=" :reader robby--read-buffer :level 5)]
   [""
    ("v" "respond in robby view buffer" robby--respond-with-robby-view-suffix)
    ("m" "respond with message" robby--respond-with-message-suffix)
    ("n" "start a conversation with AI" robby--respond-in-conversation-suffix)]])

(provide 'robby-transients)

;; robby-transients.el ends here
