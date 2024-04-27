;;; robby-models.el  --- Update available models from OpenAI   -*- lexical-binding:t -*-

;;; Commentary:

;; Defines the `robby--get-models` function to fetch the list of available models from OpenAI.

;;; Code:

(require 'robby-api-key)
(require 'robby-request)
(require 'robby-customization)
(require 'robby-provider)

(defvar robby--models nil)

(defun robby--get-models ()
  "Get the list of available models from OpenAI.

Make  request to OpenAI API to get the list of available models."
  (if robby--models
      robby--models
    (let* ((inhibit-message t)
           (message-log-max nil)
           (url (robby--models-url))
           (url-request-method "GET")
           (url-request-extra-headers
            `(("Content-Type" . "application/json")
              ("Authorization" . ,(concat "Bearer " (robby--get-api-key)))))
           (inhibit-message t)
           (message-log-max nil))
      (with-current-buffer (url-retrieve-synchronously url)
        (robby--log (format "Models response: %s\n" (buffer-string)))
        (goto-char (point-min))
        (re-search-forward "^[[{]")
        (backward-char 1)
        (let* ((json-object-type 'alist)
               (resp (json-read))
               (err (robby-provider-parse-error resp)))
          (if err
              (error "Error fetching models: %S" err)
            (setq robby--models (robby-provider-parse-models resp))))))))

(provide 'robby-models)

;;; robby-models.el ends here
