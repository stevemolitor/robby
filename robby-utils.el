;;; robby-utils.el  --- robby utility functions  -*- lexical-binding:t -*-

;;; Commentary:

;; Utility functions used internally by robby.

;;; Code:

(require 'cl-lib)
(require 'cus-edit)
(require 'map)
(require 'seq)

;;; string utils
(defun robby--format-message-text (text)
  "Replace % with %% in TEXT to avoid format string errors calling `message."
  (replace-regexp-in-string "%" "%%" text))

(defun robby--kebab-to-snake-case (string)
  "Transform STRING from kebab to snake case.
For example \"a-b-c\" becomes \"a_b_c\"."
  (replace-regexp-in-string "-" "_" string))

(defun robby--snake-to-space-case (string)
  "Transform STRING from snake case to string with spaces.
For example \"a_b_c\" becomes \"a b c\""
  (replace-regexp-in-string "_" " " string))

(defun robby--snake-to-kebob-case (string)
  "Transform STRING from snake case to kebob case.
For example \"a_b_c\" becomes \"a-b-c\""
  (replace-regexp-in-string "_" "-" string))

(defun robby--string-to-sym (string)
  "Convert STRING to a symbol."
  (intern (format ":%s" string)))

(defun robby--sym-to-string (sym)
  "Convert symbol SYM to a string."
  (replace-regexp-in-string "^:" "" (symbol-name sym)))

(defun robby--empty-p (thing)
  "Return t if THING is nil or an empty string. Otherwise return nil."
  (or (null thing) (string= thing "")))

(defun robby--decimal-p (str)
  "Return t if string STR is convertable to decimal number.
Otherwise return nil."
  (string-match-p
   (rx string-start
       (?  (or "+" "-"))
       (1+ digit)
       (0+ "." (1+ digit))
       string-end)
   str))

;;; property list utils
(defun robby--plist-to-alist (plist)
  "Convert PLIST to an association list (alist)."
  (cl-loop for (key value . rest) on plist by 'cddr
           collect
           (cons key value)))

(defun robby--plist-to-transient-args (plist)
  "Convert PLIST to transient args list."
  (let ((alist (robby--plist-to-alist plist)))
    (seq-map
     (lambda (assoc)
       (let ((key (replace-regexp-in-string "^:" "" (symbol-name (car assoc)) 1))
             (value (cdr assoc)))
         (format "%s=%s" key value)))
     alist)))

(defun robby--plist-keys (plist)
  "Get the keys from PLIST."
  (cl-loop for (key) on plist by 'cddr
           collect
           key))

(defun robby--to-stop-array-vals (options)
  "Tranform \"stop\" values in OPTIONS alist to lists.

We only support a single stop string, but the OpenAI API expects
an array."
  (seq-map (lambda (elem)
             (let* ((key (car elem))
                    (val (cdr elem))
                    (fixed-val (if (string= key "stop") `(,val) val)))
               `(,key . ,fixed-val)))
           options))

(defun robby--options-alist-for-api-request (options)
  "Get a list of options to pass to the OpenAI API.

Grabs OpenAI customization options for the chat API as and merges
them in with any specific options passed in OPTIONS. OPTIONS
overrides customization options. Return an alist of options to
pass, where the keys are strings."
  (seq-sort-by
   #'car #'string<
   (robby--to-stop-array-vals
    (map-merge
     'alist
     (seq-filter
      (lambda (elem) (not (null (cdr elem))))
      (robby--options-from-group 'robby-chat-api))
     (seq-map
      (lambda (assoc) (cons (robby--kebab-to-snake-case (replace-regexp-in-string "^:" "" (symbol-name (car assoc)))) (cdr assoc)))
      (robby--plist-to-alist options))))))

(defun robby--options-from-group (group)
  "Get list of options from a customization group GROUP."
  (seq-map
   (lambda (sym)
     (cons
      (robby--kebab-to-snake-case (robby--remove-api-prefix "chat" (symbol-name sym)))
      (symbol-value sym)))
   (seq-map
    #'car
    (seq-filter
     (lambda (elem)
       (eq (nth 1 elem) 'custom-variable))
     (custom-group-members group nil)))))

(defun robby--options-transient-value ()
  "Get API values from current customization values.

Get API options values from current customization values,
formatted for use as the initial value of a transient prefix."
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

;;; API utils
(defun robby--remove-api-prefix (api string)
  "Remove api prefix API from STRING.
For example, \"robby-chat-temperature\" becomes \"temperature\""
  (let ((regexp (format "^robby-%s-" api)))
    (replace-regexp-in-string regexp "" string)))

(defun robby--request-input (prompt historyp history system-message)
  "Return OpenAI chat API input data including PROMPT.

Also include prompt history if HISTORYP is true. HISTORY is the
history variable containing the conversation history list.

SYSTEM-MESSAGE is the system prompt."
  (let* ((system-message `((role . "system") (content . ,system-message)))
         (formatted-messages
          (if historyp
              (vconcat
               `(,system-message)
               (seq-reduce
                (lambda (vec history-elem)
                  (vconcat
                   vec
                   `(((role . "user") (content . ,(car history-elem)))
                     ((role . "assistant") (content . ,(cdr history-elem))))))
                history
                '[])
               `(((role . "user") (content . ,prompt))))
            `[,system-message ((role . "user") (content . ,prompt))])))
    `((messages . ,formatted-messages))))

(defun robby--chunk-content (chunk streamp)
  "Parse message text from chat API response JSON.

CHUNK is the response JSON from the OpenAI chat API.

STREAMP is true if the chunk is part of a streaming response."
  (let ((key (if streamp 'delta 'message)))
    (assoc-default 'content (assoc-default key (seq-first (assoc-default 'choices chunk))))))

(defun robby--gpt-models (all-models)
  "Return a list of GPT models from ALL-MODELS."
  (seq-filter
   (lambda (name) (string-prefix-p "gpt" name))
   all-models))

;;; robby--format-prompt
(defun robby--prompt-file-name-base ()
  "Return the base file, no directory no extension, for use in prompt specs."
  (if buffer-file-name
      (file-name-base buffer-file-name)
    ""))

(defun robby--prompt-file-ext ()
  "Return the file extension for use in prompt specs."
  (if buffer-file-name
      (file-name-extension buffer-file-name)
    ""))

(defun robby-make-prompt-spec (file-name file-ext)
  "Make a prompt format spec.

FILE-NAME and FILE-EXT will be set to the base file name and file
extension of the file associated with the current buffer, or to
nil if the buffer has no associated file.

Returns an association list suitable for use with `format-spec'."
  `((?e . ,file-ext)
    (?f . ,file-name)
    (?l . ,user-login-name)
    (?n . ,user-full-name)))

(defun robby--format-prompt (prompt &optional spec)
  "Format PROMPT string using `format-spec'.

Use the format spec returned by SPEC, or the spec returned by
`robby-prompt-spec-fn' if SPEC is nil."
  (let* ((file-name (robby--prompt-file-name-base))
         (file-ext (robby--prompt-file-ext))
         (prompt-spec (or
                       spec
                       (funcall robby-prompt-spec-fn file-name file-ext))))
    (format-spec prompt prompt-spec 'ignore)))

;;; grounding
(defun robby--ground-response (response grounding-fns)
  "Ground RESPONSE using GROUNDING-FNS."
  (if (seqp grounding-fns)
      (seq-reduce
       (lambda (resp fn) (funcall fn resp))
       grounding-fns
       response)
    (funcall grounding-fns response)))

(provide 'robby-utils)

;;; robby-utils.el ends here
