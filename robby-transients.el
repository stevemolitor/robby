;;; robby-transients.el  --- Robby transient menu definitions  -*- lexical-binding:t -*-

;;; Commentary:

;; Robby transient menu definitions

;;; Code:

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
(require 'robby-validation)

;;; Util Functions
(defun robby--custom-type (symbol)
  "Get the custom type of the custom variable indicated by SYMBOL."
  (let ((type (get symbol 'custom-type)))
    (if (sequencep type)
        (let ((nested-type (nth 1 type)))
          (if (sequencep nested-type)
              (car nested-type)
            nested-type))
      type)))

(defun robby--transient-args-to-options (args)
  "Convert ARGS to a plist of OpenAI API options."
  (seq-reduce
   (lambda (plist arg)
     (let* ((parts (split-string arg "="))
            (key-str (car parts)) (key-sym (intern (format ":%s" key-str)))
            (raw-value (cadr parts))
            (custom-var (intern (format "robby-chat-%s" key-str)))
            (custom-type (robby--custom-type custom-var))
            (value (if (or (eq custom-type 'integer) (eq custom-type 'number)) (string-to-number raw-value) raw-value)))
       (plist-put plist key-sym value)))
   args
   '()))

(defun robby--run-transient-command (action &optional arg)
  "Run ACTION with ARG and transient args."
  (let* ((args (transient-args transient-current-command))
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
     :arg arg
     :prompt prompt
     :prompt-args prompt-args
     :action action
     :action-args action-args)))

;;; Readers
(defun robby--read-buffer (_prompt _initial-input _history)
  "Select a buffer."
  (interactive)
  (read-buffer "Select buffer: "))

(defun robby--read-decimal (prompt _initial-input _history)
  "Read a decimal number from the minibuffer with PROMPT."
  (interactive)
  (format "%s" (read-number prompt)))

(defun robby--read-option-with-validation (option prompt initial-input history)
  "Read an OpenAI API option with validation.

OPTION is the option to validate against.

PROMPT is the prompt to display in the minibuffer when reading the option value.

INITIAL-INPUT is the initial value to display in the minibuffer.

HISTORY is the history list to use for the minibuffer."
  (save-match-data
    (cl-block nil
      (while t
        (let* ((str (read-from-minibuffer prompt initial-input nil nil history))
               (err-msg (robby--validate option str)))
          (if err-msg
              (progn
                (message err-msg)
                (sit-for 1))
            (cl-return str)))))))

(defun robby--read-temperature (prompt initial-input history)
  "Read temperature API option from the minibuffer.

PROMPT is the prompt to display in the minibuffer when reading the option value.

INITIAL-INPUT is the initial value to display in the minibuffer.

HISTORY is the history list to use for the minibuffer."
  (interactive)
  (robby--read-option-with-validation 'chat-temperature prompt initial-input history))

(defun robby--read-top-p (prompt initial-input history)
  "Read top-p API Option from the minibuffer.

PROMPT is the prompt to display in the minibuffer when reading the option value.

INITIAL-INPUT is the initial value to display in the minibuffer.

HISTORY is the history list to use for the minibuffer."
  (interactive)
  (robby--read-option-with-validation 'chat-top-p prompt initial-input history))

(defun robby--read-presence-penalty (prompt initial-input history)
  "Read presence penalty API option from the minibuffer.

PROMPT is the prompt to display in the minibuffer when reading the option value.

INITIAL-INPUT is the initial value to display in the minibuffer.

HISTORY is the history list to use for the minibuffer."
  (interactive)
  (robby--read-option-with-validation 'chat-presence-penalty prompt initial-input history))

(defun robby--read-frequency-penalty (prompt initial-input history)
  "Read frequency penalty API option from the minibuffer.

PROMPT is the prompt to display in the minibuffer when reading the option value.

INITIAL-INPUT is the initial value to display in the minibuffer.

HISTORY is the history list to use for the minibuffer."
  (interactive)
  (robby--read-option-with-validation 'chat-frequency-penalty prompt initial-input history))

;;; Action Suffixes
(transient-define-suffix
  robby--respond-with-robby-chat-suffix ()
  (interactive)
  (robby--run-transient-command #'robby-respond-with-robby-chat))

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

;;; Misc Suffixes
(transient-define-suffix
  robby--clear-history-suffix ()
  (interactive)
  (robby-clear-history))

;;; API Options Prefixes
(defvar robby--api-options
  '(robby-chat-max-tokens robby-chat-temperature robby-chat-top-p robby-chat-n robby-chat-stop robby-chat-presence-penalty robby-chat-frequency-penalty robby-chat-user)
  "API options available for customization in transient.

Only includes options that cannot be nil.")

(transient-define-suffix robby--apply-api-options ()
  (interactive)
  (seq-do (lambda (custom-var) (set custom-var nil)) robby--api-options)
  (let ((args (transient-args transient-current-command)))
    (seq-do
     (lambda (arg)
       (let* ((parts (split-string arg "="))
              (key-str (car parts))
              (raw-value (cadr parts))
              (custom-var (intern (format "robby-chat-%s" key-str)))
              (custom-type (robby--custom-type custom-var))
              (value (if (or (eq custom-type 'integer) (eq custom-type 'number)) (string-to-number raw-value) raw-value)))
         (set custom-var value)))
     args)))

(defun robby--init-api-options (obj)
  "Initialize API options for transient object OBJ."
  (oset obj value `(,@(robby--options-transient-value))))

(transient-define-prefix robby-api-options ()
  "Chat API option transient."
  :init-value 'robby--init-api-options
  ["Chat API Options"
   ("m" "model" "model=" :always-read t :choices ,(robby--get-models))
   ("t" "max tokens" "max-tokens=" :reader transient-read-number-N+ :always-read t)
   ("e" "temperature" "temperature=" :reader robby--read-temperature :always-read t)
   ("p" "top p" "top-p=" :reader robby--read-top-p :always-read t)
   ("n" "n" "n=" :reader transient-read-number-N+ :always-read t)
   ("o" "stop" "stop=" :always-read t)
   ("r" "presence penalty" "presence-penalty=" :reader robby--read-presence-penalty :always-read t)
   ("f" "frequency penalty" "frequency-penalty=" :reader robby--read-frequency-penalty :always-read t)
   ("u" "user" "user=" :always-read t)]
  [[("a" "apply options" robby--apply-api-options :transient transient--do-return)]
   [("x" "exit without applying options" ignore :transient transient--do-return :if (lambda () transient-current-command))]])

;;; Robby transient
;;;###autoload (autoload 'robby-builder "robby-transients" "Build a robby AI command." t)
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
    ("v" "respond in robby view buffer" robby--respond-with-robby-chat-suffix :level 3)]]
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
   ("A" "API options" robby-api-options :transient transient--do-recurse :level 5)])

;;;###autoload (autoload 'robby-example-commands "robby-transients" "Display menu for executing example robby commands." t)
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

(provide 'robby-transients)

;;; robby-transients.el ends here
