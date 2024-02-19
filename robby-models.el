;;; robby-models.el  --- Update available models from OpenAI   -*- lexical-binding:t -*-

;;; Code:

(require 'robby-request)
(require 'robby-customization)

(defun robby--get-models (on-success on-error)
  (let* ((inhibit-message t)
         (message-log-max nil)
         (url "https://api.openai.com/v1/models")
         (url-request-method "GET")
         (url-request-extra-headers
          `(("Content-Type" . "application/json")
            ("Authorization" . ,(concat "Bearer " (robby--get-api-key)))))
         (inhibit-message t)
         (message-log-max nil))
    (url-retrieve
     url
     (lambda (_status)
       (goto-char (point-min))
       (re-search-forward "^{")
       (backward-char 1)
       (let* ((json-object-type 'alist)
              (resp (json-read))
              (err (robby--request-parse-error-data resp)))
         (if err
             (funcall on-error err)
           (let ((models
                  (seq-map (lambda (obj) (cdr (assoc 'id obj))) (cdr (assoc 'data resp)))))
             (funcall on-success models))))))))

(defvar robby-models nil)

(defun robby--update-models (callback)
  "Update models from OpenAI."
  (robby--get-models
   (lambda (models)
     (setq robby-models models)
     (funcall callback))
   (lambda (err)
     (error (format "Error fetching models from OpenAI: %s" err)))))

(provide 'robby-models)

;; robby--get-models.el ends here
