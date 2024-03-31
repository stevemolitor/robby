;;; robby-models.el  --- Update available models from OpenAI   -*- lexical-binding:t -*-

;;; Commentary:

;; Defines the `robby--get-models` function to fetch the list of available models from OpenAI.

;;; Code:

(require 'robby-api-key)
(require 'robby-request)
(require 'robby-customization)

(defvar robby-models nil)

(defun robby--get-models ()
  "Get the list of available models from OpenAI.

Make  request to OpenAI API to get the list of available models."
  (if robby-models
      robby-models
    (let* ((inhibit-message t)
           (message-log-max nil)
           (url robby-api-url)
           (url-request-method "GET")
           (url-request-extra-headers
            `(("Content-Type" . "application/json")
              ("Authorization" . ,(concat "Bearer " (robby--get-api-key)))))
           (inhibit-message t)
           (message-log-max nil))
      (with-current-buffer (url-retrieve-synchronously url)
        (goto-char (point-min))
        (re-search-forward "^{")
        (backward-char 1)
        (let* ((json-object-type 'alist)
               (resp (json-read))
               (err (robby--request-parse-error-data resp)))
          (if err
              (error "Error fetching models: %S" err)
            (let* ((all-models (seq-map (lambda (obj) (cdr (assoc 'id obj))) (cdr (assoc 'data resp))))
                   (gpt-models (seq-filter (lambda (name) (string-prefix-p "gpt" name)) all-models)))
              (setq robby-models gpt-models))))))))

(provide 'robby-models)

;;; robby-models.el ends here
