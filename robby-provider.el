;;; robby-provider.el  --- Support Multiple AI Services  -*- lexical-binding:t -*-

;;; Commentary:

;;; Code:
(require 'cl-generic)

;; declared in robby-custom.el
(defvar robby-provider)

(defvar robby--provider-settings nil
  "alist of provider settings.")

;; TODO use this function for each provider upon initialization
(defun robby--add-provider-choice (symbol name)
  "Add a provider SYMBOL and NAME to the choices for the
`robby-provider' custom type."
  (let* ((new-choice `(const :tag ,name ,symbol))
         (old-choices (cdr (get 'provider 'custom-type)))
         (new-choices (cons new-choice old-choices))
         (new-type (cons 'choice new-choices)))
    (put 'provider 'custom-type new-type)))

(defun robby--get-provider-settings ()
  "Return the settings of the current provider."
  (alist-get robby-provider robby--provider-settings))

(defun robby--provider-host ()
  "Return the host of the current provider."
  (plist-get (robby--get-provider-settings) :host))

(defun robby--provider-default-model ()
  "Return the default model to use with the the current provider."
  (plist-get (robby--get-provider-settings) :default-model))

(defun robby--provider-api-base-path ()
  "Return the API base path of the current provider."
  (plist-get (robby--get-provider-settings) :api-base-path))

(defun robby--provider-models-path ()
  "Return the models path of the current provider."
  (plist-get (robby--get-provider-settings) :models-path))

(cl-defun robby-add-provider (&key symbol name host default-model api-base-path models-path)
  "Register a new robby provider."
  (let* ((settings `(:name ,name
                           :host ,host
                           :default-model ,default-model
                           :api-base-path ,(or api-base-path "/v1/chat/completions")
                           :models-path ,(or models-path "/v1/models"))))
    (push (cons symbol settings) robby--provider-settings)))

(cl-defmethod robby-provider-parse-error (data)
  "Get error from response DATA.

DATA is an alist of the JSON parsed response from the provider."
  ;; TODO
  nil
  ;; (cdr (assoc 'message (assoc 'error data)))
  )

(cl-defmethod robby-provider-parse-models (data)
  "Get models from response DATA."
  (message "base method data: %S" data)
  (seq-map (lambda (obj) (cdr (assoc 'id obj))) (cdr (assoc 'data data))))

(provide 'robby-provider)

;;; robby-provider.el ends here
