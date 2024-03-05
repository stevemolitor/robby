;;; robby-models.el  --- Update available models from OpenAI   -*- lexical-binding:t -*-


(require 'robby-api-key)
(require 'robby-request)
(require 'robby-customization)

;;; Code:

(defvar robby-models nil)

(defun robby--get-models ()
  (if robby-models
      robby-models
    (let* ((inhibit-message t)
           (message-log-max nil)
           (url "https://api.openai.com/v1/models")
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
              (funcall on-error err)
            (let* ((all-models (seq-map (lambda (obj) (cdr (assoc 'id obj))) (cdr (assoc 'data resp))))
                   (gpt-models (seq-filter (lambda (name) (string-prefix-p "gpt" name)) all-models)))
              (setq robby-models gpt-models))))))))

(provide 'robby-models)

;; robby--get-models.el ends here
