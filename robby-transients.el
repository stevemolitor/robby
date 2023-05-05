;;; robby-transients-bak.el  --- Robby transient menu defintions  -*- lexical-binding:t -*-

;;; Commentary:

;; Robby transient menu definitions

(require 'cus-edit)
(require 'seq)
(require 'transient)

(require 'robby-customization)
(require 'robby-utils)

;;; Code:

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
            (api (plist-get scope :api))
            (parts (split-string arg "="))
            (key-str (car parts)) (key-sym (intern (format ":%s" key-str)))
            (raw-value (cadr parts))
            (custom-var (intern (format "robby-%s-%s" api key-str)))
            (custom-type (robby--custom-type custom-var))
            (value (if (or (eq custom-type 'integer) (eq custom-type 'number)) (string-to-number raw-value) raw-value)))
       (plist-put plist key-sym value)))
   args
   '()))

(defun robby--select-api (api)
  "Helper function to select an API in robby transient."
  (let* ((scope (oref transient-current-prefix scope))
         (prev-api (plist-get 'scope :api))
         (options (if (equal prev-api api) (plist-get scope :options) '()))
         (robby-value (transient-args 'robby)))
    (transient-setup transient-current-command nil nil
                     :scope `(:api ,api (:options ,options))
                     :value robby-value)))

(defun robby--transient-selected-api (scope)
  (or (plist-get scope ':api) robby-api))

(defun robby--transient-api-description (api)
  (let* ((scope (oref transient--prefix scope))
         (api-name (capitalize api)))
    (if (equal (robby--transient-selected-api scope) api)
        (propertize api-name 'face 'transient-enabled-suffix)
      api-name)))

(defun robby--advanced-options-defaults (api)
  "Get default advanced options values for API from current
customization values."
  (let ((custom-variables
         (seq-filter
          (lambda (var) (not (null (symbol-value var))))
          (seq-map #'car (custom-group-members (intern-soft (format "robby-%s-api" api)) nil))))
        (regexp (format "^robby-%s-" api)))
    (seq-map
     (lambda (var)
       (let ((key (replace-regexp-in-string regexp "" (symbol-name var))))
         (format "%s=%s" key (symbol-value var))))
     custom-variables)))

(defun robby--return-to-parent-transient ()
  (let* ((scope (oref transient-current-prefix scope))
         (options (plist-get scope :options))
         (robby-value (plist-get scope :robby-value)))
    (transient-setup 'robby nil nil
                     :scope `(:api "chat" :options options)
                     :value robby-value)))

;;; Readers
(defun robby--read-buffer (prompt initial-input history)
  "Select a buffer."
  (interactive)
  (read-buffer "Select buffer: "))

(defun robby--read-decimal (prompt initial-input history)
  "Read a decimal number."
  (interactive)
  (format "%s" (read-number prompt)))

;;; Suffixes
(transient-define-suffix robby--respond-with-message-suffix ()
  (interactive)
  (let* ((scope (or (oref transient-current-prefix scope) '()))
         (api (or (plist-get scope :api) robby-api))
         (api-options (plist-get scope :api-options))) ;; TODO rename :options to :api-options in scope
    (robby-run-command
     :prompt (transient-arg-value "prompt=" (transient-args transient-current-command))
     :action #'robby-respond-with-message
     :api api
     :api-options api-options)))

(transient-define-suffix robby--respond-in-help-window-suffix ()
  (interactive)
  (let* ((scope (or (oref transient-current-prefix scope) '()))
         (api (or (plist-get scope :api) robby-api))
         (api-options (plist-get scope :api-options))) ;; TODO rename :options to :api-options in scope
    (robby-run-command
     :prompt (transient-arg-value "prompt=" (transient-args transient-current-command))
     :action #'robby-respond-in-help-window
     :api api
     :api-options api-options)))

(transient-define-suffix robby--prefix-region-with-response-suffix ()
  (interactive)
  (let* ((scope (or (oref transient-current-prefix scope) '()))
         (api (or (plist-get scope :api) robby-api))
         (api-options (plist-get scope :api-options))) ;; TODO rename :options to :api-options in scope
    (robby-run-command
     :prompt #'robby-get-prompt-from-region
     :action #'robby-prepend-response-to-region
     :api api
     :api-options api-options)))

(transient-define-suffix robby--append-response-to-region-suffix ()
  (interactive)
  (let* ((scope (or (oref transient-current-prefix scope) '()))
         (api (or (plist-get scope :api) robby-api))
         (api-options (plist-get scope :api-options))) ;; TODO rename :options to :api-options in scope
    (robby-run-command
     :prompt #'robby-get-prompt-from-region
     :action #'robby-append-response-to-region
     :api api
     :api-options api-options)))

(transient-define-suffix robby--replace-region-with-response-suffix ()
  (interactive)
(let* ((scope (or (oref transient-current-prefix scope) '()))
         (api (or (plist-get scope :api) robby-api))
         (api-options (plist-get scope :api-options))) ;; TODO rename :options to :api-options in scope
    (robby-run-command
     :prompt #'robby-get-prompt-from-region
     :action #'robby-replace-region-with-response
     :api api
     :api-options api-options)))

(transient-define-suffix robby--select-completions-suffix ()
  "Select the Completions API."
  :transient 'transient--do-exit
  (interactive)
  (robby--select-api "completions"))

(transient-define-suffix robby--select-chat-suffix ()
  "Select the Chat API."
  :transient 'transient--do-exit
  (interactive)
  (robby--select-api "chat"))

(transient-define-suffix robby--apply-advanced-options ()
  :transient 'transient--do-exit
  (interactive)
  (let* ((scope (oref transient-current-prefix scope))
         (args (transient-args transient-current-command))
         (api-options (robby--transient-args-to-options args))
         (robby-value (plist-get scope :robby-value)))
    (transient-setup 'robby nil nil
                     :scope `(:api "chat" :api-options ,api-options)
                     :value robby-value)))

(transient-define-suffix robby--setup-advanced-options ()
  "Call appropriate advanced options prefix for API in scope."
  (interactive)
  (let* ((scope (oref transient-current-prefix scope))
         (api-options (plist-get scope :api-options))
         (selected-api (robby--transient-selected-api scope))
         (value (or (and api-options (robby--plist-to-transient-args api-options))
                    (robby--advanced-options-defaults selected-api)))
         (transient-name (format "robby--advanced-%s-options" selected-api)))
    (transient-setup (intern transient-name) nil nil
                     :scope `(:api ,selected-api :robby-value ,(transient-args 'robby))
                     :value value)))

;;; API Options Suffixes
(transient-define-prefix robby--advanced-chat-options ()
  "Advanced OpenAI API options."
  ["Advanced chat API Options"
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
   ("a" "apply options" robby--apply-advanced-options)])

(transient-define-prefix robby--advanced-completions-options ()
  "Advanced OpenAI API options."
  ["Advanced completions API Options"
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
   ("a" "apply options" robby--apply-completions-options)])

;;; Transient Commands
(transient-define-prefix robby ()
  "Invoke OpenAI Chat API."
  :incompatible '(("simple-prompt=" "from-region")
                  ("simple-prompt=" "prompt-buffer=")
                  ("simple-prompt=" "prompt-prefix=")
                  ("simple-prompt=" "prompt-suffix="))
  [:class transient-row "API"
          ("c" "Chat" robby--select-chat-suffix
           :description (lambda () (robby--transient-api-description "chat")))
          ("o" "Completions" robby--select-completions-suffix
           :description (lambda () (robby--transient-api-description "completions")))
          ("A" "advanced API options" robby--setup-advanced-options :transient transient--do-replace)]
  ["Prompt"
   ("s" "simple prompt" "prompt=" :always-read t)
   ("r" "from region" "from-region=")]
  [["Prompt From Region Options"
    ("pp" "prompt prefix" "prompt-prefix=" :always-read t)
    ("ps" "prompt suffix" "prompt-suffix=" :always-read t)
    ("pb" "prompt buffer" "prompt-buffer=" :always-read t :reader robby--read-buffer)]]
  ["Action"
   ("h" "respond in help window" robby--respond-in-help-window-suffix)
   ("m" "respond with message" robby--respond-with-message-suffix)
   ""
   ("x" "prefix region with response" robby--prefix-region-with-response-suffix)
   ("a" "append response to region" robby--append-response-to-region-suffix)
   ("r" "replace region with response" robby--replace-region-with-response-suffix)]
  [["Region Action Options"
    ("gb" "response buffer" "to-buffer=" :always-read t :reader robby--read-buffer)]])

(provide 'robby-transients)

;; robby-transients.el ends here
