;;; robby-transients.el  --- Robby transient menu definitions  -*- lexical-binding:t -*-

;;; Commentary:

;; Robby transient menu definitions

(require 'cl-lib)
(require 'cus-edit)
(require 'seq)
(require 'transient)

(require 'robby-actions)
(require 'robby-customization)
(require 'robby-models)
(require 'robby-prompts)
(require 'robby-run-command)
(require 'robby-utils)

;;; Code:

;;; Scope struct
(cl-defstruct
    (robby--scope
     (:constructor robby--scope-default)
     (:constructor robby--scope-set-api-options
                   (&key
                    scope options
                    &aux
                    (api-options options)
                    (robby-value (robby--scope-robby-value scope))))
     (:constructor robby--scope-set-robby-value
                   (&key
                    scope value
                    &aux
                    (api-options (robby--scope-api-options scope))
                    (robby-value value))))
  (api-options nil :read-only t)
  (robby-value nil :read-only t))

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
            (parts (split-string arg "="))
            (key-str (car parts)) (key-sym (intern (format ":%s" key-str)))
            (raw-value (cadr parts))
            (custom-var (intern (format "robby-chat-%s" key-str)))
            (custom-type (robby--custom-type custom-var))
            (value (if (or (eq custom-type 'integer) (eq custom-type 'number)) (string-to-number raw-value) raw-value)))
       (plist-put plist key-sym value)))
   args
   '()))

(defun robby--api-options-defaults ()
  "Get default api options values for API from current customization
values."
  (let* ((custom-variables
          (seq-filter
           (lambda (var) (not (null (symbol-value var))))
           (seq-map #'car (custom-group-members 'robby-chat-api nil))))
         (regexp "^robby-chat-"))
    (seq-map
     (lambda (var)
       (let ((key (replace-regexp-in-string regexp "" (symbol-name var))))
         (format "%s=%s" key (symbol-value var))))
     custom-variables)))

(defun robby--run-transient-command (action &optional arg view-buffer)
  (let* ((scope (or (oref transient-current-prefix scope) (robby--scope-default)))
         (api-options (robby--scope-api-options scope))
         (args (transient-args transient-current-command))
         (simple-prompt (transient-arg-value "prompt=" args))
         (prompt-prefix (transient-arg-value "prompt-prefix=" args))
         (prompt-suffix (transient-arg-value "prompt-suffix=" args))
         (prompt-buffer (transient-arg-value "prompt-buffer=" args))
         (prompt (or simple-prompt #'robby-get-prompt-from-region))
         (prompt-args (if simple-prompt
                          '()
                        `(:prompt-prefix ,prompt-prefix :prompt-suffix ,prompt-suffix :buffer ,prompt-buffer :never-ask-p t)))
         (response-buffer (or (transient-arg-value "response-buffer=" args) view-buffer)) 
         (action-args `(:response-buffer ,response-buffer))
         (historyp (transient-arg-value "historyp" args)))
    (robby-run-command
     :arg arg
     :prompt prompt
     :prompt-args prompt-args
     :action action
     :action-args action-args
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
  (robby--run-transient-command #'robby-respond-with-message t))

(transient-define-suffix
  robby--respond-with-robby-view-suffix ()
  (interactive)
  (robby--run-transient-command #'robby-respond-with-robby-view nil "*robby*"))

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
  (let* ((args (transient-args transient-current-command))
         (diff-preview (transient-arg-value "diff-preview" args)))
    (robby--run-transient-command #'robby-replace-region-with-response diff-preview)))

;;; API Related Suffixes
(transient-define-suffix
  robby--apply-api-options ()
  :transient 'transient--do-exit
  (interactive)
  (let* ((scope (oref transient-current-prefix scope))
         (robby-value (robby--scope-robby-value scope))
         (args (transient-args transient-current-command))
         (api-options (robby--transient-args-to-options args))
         (new-scope (robby--scope-set-api-options :scope scope :options api-options)))
    (transient-set)
    (transient-setup 'robby nil nil
                     :scope new-scope
                     :value robby-value)))

(defun robby--get-scope ()
  (or (oref transient-current-prefix scope) (robby--scope-default)))

(defun robby--get-api-options-transient-value (api-options)
  (or (and api-options (robby--plist-to-transient-args api-options))
      (robby--api-options-defaults)))

(transient-define-suffix
  robby--setup-api-options ()
  "Call appropriate API options prefix for API in scope.

The initial transient value comes from either any previously
edited options for the API, or default API options from
customization values."
  (interactive)
  (let* ((scope (robby--get-scope))
         (api-options (robby--scope-api-options scope))
         (value (robby--get-api-options-transient-value api-options)))
    (robby--update-models
     (lambda ()
       (transient-setup 'robby--chat-api-options nil nil :scope scope :value value)))))

(transient-define-suffix
  robby--reset-api-options ()
  "Reset current API options to their customization values."
  :transient 'transient--do-call
  (interactive)
  (transient-reset))

;;; Misc Suffixes
(transient-define-suffix
  robby--clear-history-suffix ()
  (interactive)
  (robby-clear-history))

;;; API Options Prefixes
(transient-define-prefix
  robby--chat-api-options ()
  "Chat API options."
  ["Chat API Options"
   ("m" "model" "model=" :always-read t :choices (lambda () (robby--models-for-api robby-models)))
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
   ("u" "user" "user=" :always-read t)]
  [[("a" "apply options" robby--apply-api-options)]
   [("z" "reset to customization values" robby--reset-api-options)]])

;;;###autoload (autoload 'robby-commands "robby-transients" "Display menu of custom robby commands." t)
(transient-define-prefix robby-example-commands ()
  "Display menu for executing example robby commands."
  ["Robby Example Commands"
   ("d" "describe code" robby-describe-code)
   ("f" "fix code" robby-fix-code)
   ("g" "generate commit message from staged changes" robby-git-commit-message)
   ("o" "add comments" robby-add-comment)
   ("t" "write tests" robby-write-tests)
   ("s" "summarize text" robby-summarize)
   ("x" "proof read text" robby-proof-read)])

;;; Robby transient
;;;###autoload (autoload 'robby "robby-transients" nil t)
(transient-define-prefix robby-builder ()
  "Build a robby AI command."
  :incompatible '(("prompt=" "prompt-prefix=")
                  ("prompt=" "prompt-suffix=")
                  ("prompt=" "prompt-buffer="))
  ["Prompt"
   ("i" "simple prompt" "prompt=" :always-read t :level 3)]
  ["Prompt from Region or Buffer Options"
   ("p" "prompt prefix" "prompt-prefix=" :always-read t :level 3)
   ("s" "prompt suffix" "prompt-suffix=" :always-read t :level 3)
   ("b" "prompt buffer" "prompt-buffer=" :reader robby--read-buffer :level 6)]
  [["Region Actions"
    ("x" "prefix region with response" robby--prefix-region-with-response-suffix :level 3)
    ("a" "append response to region" robby--append-response-to-region-suffix :level 3)
    ("g" "replace region with response" robby--replace-region-with-response-suffix :level 3)]
   ["Misc Actions"
    ("v" "respond in robby view buffer" robby--respond-with-robby-view-suffix :level 3)]]
  ["Region Action Options"
   ("f" "response buffer" "response-buffer=" :reader robby--read-buffer :level 6)
   ("d" "show diff preview before replacing region" "diff-preview" :reader robby--read-buffer :level 5)]
  ["Commands"
   ("u" "re-run last command" robby-run-last-command :level 4)
   ("c" "insert last command" robby-insert-last-command :level 4)]
  ["History" :description (lambda () (concat (propertize "History " 'face 'transient-heading) (propertize (format "(%d)" (length robby--history)) 'face 'transient-inactive-value)))
   ("h" "use history" "historyp" :level 5)
   ("l" "clear history" robby--clear-history-suffix :transient t :level 5)]
  ["API"
   ("A" "API options" robby--setup-api-options :transient transient--do-replace :level 5)])

(provide 'robby-transients)

;; robby-transients.el ends here

;;; robby-transients.el  --- Robby transient menu definitions  -*- lexical-binding:t -*-

;;; Commentary:

;; Robby transient menu definitions

(require 'cl-lib)
(require 'cus-edit)
(require 'seq)
(require 'transient)

(require 'robby-actions)
(require 'robby-customization)
(require 'robby-models)
(require 'robby-prompts)
(require 'robby-run-command)
(require 'robby-utils)

;;; Code:

;;; Scope struct
(cl-defstruct
    (robby--scope
     (:constructor robby--scope-default)
     (:constructor robby--scope-set-api-options
                   (&key
                    scope options
                    &aux
                    (api-options options)
                    (robby-value (robby--scope-robby-value scope))))
     (:constructor robby--scope-set-robby-value
                   (&key
                    scope value
                    &aux
                    (api-options (robby--scope-api-options scope))
                    (robby-value value))))
  (api-options nil :read-only t)
  (robby-value nil :read-only t))

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
            (parts (split-string arg "="))
            (key-str (car parts)) (key-sym (intern (format ":%s" key-str)))
            (raw-value (cadr parts))
            (custom-var (intern (format "robby-chat-%s" key-str)))
            (custom-type (robby--custom-type custom-var))
            (value (if (or (eq custom-type 'integer) (eq custom-type 'number)) (string-to-number raw-value) raw-value)))
       (plist-put plist key-sym value)))
   args
   '()))

(defun robby--api-options-defaults ()
  "Get default api options values for API from current customization
values."
  (let* ((custom-variables
          (seq-filter
           (lambda (var) (not (null (symbol-value var))))
           (seq-map #'car (custom-group-members 'robby-chat-api nil))))
         (regexp "^robby-chat-"))
    (seq-map
     (lambda (var)
       (let ((key (replace-regexp-in-string regexp "" (symbol-name var))))
         (format "%s=%s" key (symbol-value var))))
     custom-variables)))

(defun robby--run-transient-command (action &optional arg)
  (let* ((scope (or (oref transient-current-prefix scope) (robby--scope-default)))
         (api-options (robby--scope-api-options scope))
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
         (action-args `(:response-buffer ,response-buffer))
         (historyp (transient-arg-value "historyp" args)))
    (robby-run-command
     :arg arg
     :prompt prompt
     :prompt-args prompt-args
     :action action
     :action-args action-args
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
  (robby--run-transient-command #'robby-respond-with-message t))

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
  (let* ((args (transient-args transient-current-command))
         (diff-preview (transient-arg-value "diff-preview" args)))
    (robby--run-transient-command #'robby-replace-region-with-response diff-preview)))

;;; API Related Suffixes
(transient-define-suffix
  robby--apply-api-options ()
  :transient 'transient--do-exit
  (interactive)
  (let* ((scope (oref transient-current-prefix scope))
         (robby-value (robby--scope-robby-value scope))
         (args (transient-args transient-current-command))
         (api-options (robby--transient-args-to-options args))
         (new-scope (robby--scope-set-api-options :scope scope :options api-options)))
    (transient-set)
    (transient-setup 'robby nil nil
                     :scope new-scope
                     :value robby-value)))

(defun robby--get-scope ()
  (or (oref transient-current-prefix scope) (robby--scope-default)))

(defun robby--get-api-options-transient-value (api-options)
  (or (and api-options (robby--plist-to-transient-args api-options))
      (robby--api-options-defaults)))

(transient-define-suffix
  robby--setup-api-options ()
  "Call appropriate API options prefix for API in scope.

The initial transient value comes from either any previously
edited options for the API, or default API options from
customization values."
  (interactive)
  (let* ((scope (robby--get-scope))
         (api-options (robby--scope-api-options scope))
         (value (robby--get-api-options-transient-value api-options)))
    (robby--update-models
     (lambda ()
       (transient-setup 'robby--chat-api-options nil nil :scope scope :value value)))))

(transient-define-suffix
  robby--reset-api-options ()
  "Reset current API options to their customization values."
  :transient 'transient--do-call
  (interactive)
  (transient-reset))

;;; Misc Suffixes
(transient-define-suffix
  robby--clear-history-suffix ()
  (interactive)
  (robby-clear-history))

;;; API Options Prefixes
(transient-define-prefix
  robby--chat-api-options ()
  "Chat API options."
  ["Chat API Options"
   ("m" "model" "model=" :always-read t :choices (lambda () (robby--models-for-api robby-models)))
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
   ("u" "user" "user=" :always-read t)]
  [[("a" "apply options" robby--apply-api-options)]
   [("z" "reset to customization values" robby--reset-api-options)]])

;;;###autoload (autoload 'robby-commands "robby-transients" "Display menu of custom robby commands." t)
(transient-define-prefix robby-example-commands ()
  "Display menu for executing example robby commands."
  ["Robby Example Commands"
   ("d" "describe code" robby-describe-code)
   ("f" "fix code" robby-fix-code)
   ("g" "generate commit message from staged changes" robby-git-commit-message)
   ("o" "add comments" robby-add-comment)
   ("t" "write tests" robby-write-tests)
   ("s" "summarize text" robby-summarize)
   ("x" "proof read text" robby-proof-read)])

;;; Robby transient
;;;###autoload (autoload 'robby "robby-transients" nil t)
(transient-define-prefix robby-builder ()
  "Build a robby AI command."
  :incompatible '(("prompt=" "prompt-prefix=")
                  ("prompt=" "prompt-suffix=")
                  ("prompt=" "prompt-buffer="))
  ["Prompt"
   ("i" "simple prompt" "prompt=" :always-read t :level 3)]
  ["Prompt from Region or Buffer Options"
   ("p" "prompt prefix" "prompt-prefix=" :always-read t :level 3)
   ("s" "prompt suffix" "prompt-suffix=" :always-read t :level 3)
   ("b" "prompt buffer" "prompt-buffer=" :reader robby--read-buffer :level 6)]
  [["Region Actions"
    ("x" "prefix region with response" robby--prefix-region-with-response-suffix :level 3)
    ("a" "append response to region" robby--append-response-to-region-suffix :level 3)
    ("g" "replace region with response" robby--replace-region-with-response-suffix :level 3)]
   ["Misc Actions"
    ("v" "respond in robby view buffer" robby--respond-with-robby-view-suffix :level 3)]]
  ["Region Action Options"
   ("f" "response buffer" "response-buffer=" :reader robby--read-buffer :level 6)
   ("d" "show diff preview before replacing region" "diff-preview" :reader robby--read-buffer :level 5)]
  ["Commands"
   ("u" "re-run last command" robby-run-last-command :level 4)
   ("c" "insert last command" robby-insert-last-command :level 4)]
  ["History" :description (lambda () (concat (propertize "History " 'face 'transient-heading) (propertize (format "(%d)" (length robby--history)) 'face 'transient-inactive-value)))
   ("h" "use history" "historyp" :level 5)
   ("l" "clear history" robby--clear-history-suffix :transient t :level 5)]
  ["API"
   ("A" "API options" robby--setup-api-options :transient transient--do-replace :level 5)])

(provide 'robby-transients)

;; robby-transients.el ends here
