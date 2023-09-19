;;; robby-utils.el  --- robby utility functions -*- lexical-binding:t -*-

;;; Commentary:

;; Robby utility functions.

(require 'cl-macs)
(require 'cus-edit)
(require 'map)
(require 'seq)

(require 'robby-customization)

;;; Code:

;;; string utils
(defun robby--format-message-text (text)
  "Replace % with %% in TEXT to avoid format string errors calling `message."
  (replace-regexp-in-string "%" "%%" text))

(defun robby--kebab-to-snake-case (string)
  "Transform STRING from kebab to snake case.
For example \"a-b-c\" becomes a_b_c."
  (replace-regexp-in-string "-" "_" string))

(defun robby--snake-to-space-case (string)
  "Transform STRING from snake case to string with spaces.
For example \"a_b_c\" becomes \"a b c\""
  (replace-regexp-in-string "_" " " string))

(defun robby--string-to-sym (string)
  (intern (format ":%s" string)))

(defun robby--sym-to-string (sym)
  (replace-regexp-in-string "^:" "" (symbol-name sym)))

(defun robby--empty-p (thing)
  (or (null thing) (string= thing "")))

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

(defun robby--options-from-group ()
  "Get list of options from a Robby `robby-chat-api' customization group.

API specifies the customization group, for example `\"chat\"' or
`\"completions\"'.  Returns an association list of options."
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
     (custom-group-members 'robby-chat-api nil)))))

(defun robby--options (options)
  "Get a list of options to pass to the OpenAI API.

Grabs OpenAI customization options for the chat API as and merges
them in with any specific options passed in OPTIONS. OPTIONS
overrides customization options."
  (seq-sort-by
   #'car #'string<
   (map-merge
    'alist
    (seq-filter
     (lambda (elem) (not (null (cdr elem))))
     (robby--options-from-group))
    (seq-map
     (lambda (assoc) (cons (robby--kebab-to-snake-case (replace-regexp-in-string "^:" "" (symbol-name (car assoc)))) (cdr assoc)))
     (robby--plist-to-alist options)))))

;;; API utils
(defun robby--remove-api-prefix (api string)
  "Remove api prefix API from STRING.
For example, \"robby-chat-temperature\" becomes \"temperature\""
  (let ((regexp (format "^robby-%s-" api)))
    (replace-regexp-in-string regexp "" string)))

(defun robby--request-input (prompt historyp)
  "Return OpenAI chat API input data including PROMPT.
Also include prompt history if HISTORYP is true."
  (let* ((system-message `((role . "system") (content . ,robby-chat-system-message)))
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
                robby--history
                '[])
               `(((role . "user") (content . ,prompt))))
            `[,system-message ((role . "user") (content . ,prompt))])))
    `((messages . ,formatted-messages))))

(defun robby--chunk-content (chunk streamp)
  "Parse message text from chat API response JSON."
  (let ((key (if streamp 'delta 'message)))
    (assoc-default 'content (assoc-default key (seq-first (assoc-default 'choices chunk))))))

(defun robby--models-for-api (all-models)
  (seq-filter
   (lambda (name) (string-prefix-p "gpt" name))
   all-models))

;;; grounding
(defun robby--ground-response (response grounding-fns)
  (if (seqp grounding-fns)
      (seq-reduce
       (lambda (resp fn) (funcall fn resp))
       grounding-fns
       response)
    (funcall grounding-fns response)))

(provide 'robby-utils)

;;; robby-utils.el ends here
