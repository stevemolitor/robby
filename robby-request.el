;;; robby-request.el  --- Make robby requests via curl or url-retrieve  -*- lexical-binding:t -*-

;;; Commentary:

(require 'cl-macs)
(require 'files)
(require 'json)
(require 'seq)

(require 'robby-apis)
(require 'robby-logging)

;;; API key 
(defun robby-get-api-key-from-auth-source ()
  "Get api key from auth source."
  (if-let ((secret (plist-get (car (auth-source-search
                                    :host "api.openai.com"
                                    :user "apikey"
                                    :require '(:secret)))
                              :secret)))
      (if (functionp secret)
          (encode-coding-string (funcall secret) 'utf-8)
        secret)
    (user-error "No `robby-api-key' found in auth source")))

(defun robby--get-api-key ()
  "Get api key from `robby-api-key'."
  (cond
   ((stringp robby-openai-api-key) robby-openai-api-key)
   ((functionp robby-openai-api-key) (funcall robby-openai-api-key))
   (t (error "`robby-openai-api-key` not set"))))

;;; shared utils
(defun robby--request-parse-error (err)
  (condition-case _err
      (cdr (assoc 'message (assoc 'error (json-read-from-string err))))
    (error nil)))

;;; curl
(defvar robby--curl-options
  '("--compressed"
    "--disable"
    "--silent"
    "-m 600"
    "-H" "Content-Type: application/json"))

(defun robby--curl-parse-chunk (remaining data)
  "Parse json documents in current buffer from DATA string.

Prepend REMAINING text incomplete JSON in last chunk Return
remaining incomplete text in this document.

Ignores `\"[DONE]\".

Returns a plist with remaining un-parsed text (if any) and a list
of parsed JSON objects: `(:remaining \"text\" :parsed '())'
"
  (with-temp-buffer
    (let ((new-remaining "")
          (parsed '())
          (done)
          (pos (point-min)))
      (insert remaining)
      (insert data)
      (goto-char pos)
      (while (and (not done) (not (looking-at " *\\[DONE\\]")))
        (condition-case err
            (progn
              (setq pos (point))
              (let* ((json-object-type 'alist)
                     (obj (json-read)))
                (setq parsed (cons obj parsed))))
          (error
           (setq done t)
           (setq new-remaining (buffer-substring pos (point-max))))))
      `(:remaining ,new-remaining :parsed ,(nreverse parsed)))))

(defconst robby--curl-unknown-error "Unexpected error making OpenAI request via curl" )

(defun robby--curl-parse-response (api string remaining streamp)
  (let* ((data (replace-regexp-in-string (rx bol "data:") "" string))
         (json (robby--curl-parse-chunk remaining data))
         (parsed (plist-get json :parsed))
         (text (string-join (seq-filter #'stringp (seq-map (lambda (chunk) (robby--chunk-content api chunk streamp)) parsed)))))
    (setq remaining (plist-get json :remaining))
    `(:text ,text :remaining ,(plist-get json :remaining))))

(cl-defun robby--curl (&key api payload on-text on-error streamp)
  (let* ((input-json (json-encode (append payload (if streamp '((stream . t)) nil))))
         (url (robby--request-url api))
         (curl-options (append robby--curl-options
                               `("-H" ,(format "Authorization: Bearer %s" (robby-get-api-key-from-auth-source))
                                 "-d" ,input-json)))
         (proc-buffer (if streamp nil (generate-new-buffer (format "*robby-request-%s*" (buffer-name)))))
         (proc (condition-case err
                   (apply #'start-process
                          "curl"
                          proc-buffer
                          "curl"
                          url
                          curl-options)
                 (error (funcall on-error err)))))
    (let ((remaining "")
          (text "")
          (errored nil))
      (when streamp
        (set-process-filter
         proc
         (lambda (_proc string)
           ;; TODO error parsing seems wrong here - it's not expecting a string. Validate.
           (let ((error-msg (robby--request-parse-error string)))
             (if error-msg
                 (progn
                   (setq errored t)
                   (funcall on-error error-msg))
               (let ((resp (robby--curl-parse-response api string remaining streamp)))
                 (setq remaining (plist-get resp :remaining))
                 (funcall on-text :text (plist-get resp :text) :completep nil)))))))
      (set-process-sentinel
       proc
       (lambda (_proc _status)
         (if streamp
             (if (not errored)
                 (funcall on-text :text text :completep t))
           (with-current-buffer proc-buffer
             (let* ((string (buffer-string))
                    (error-msg (robby--request-parse-error string)))
               (if error-msg
                   (funcall on-error error-msg)
                 (let ((resp (robby--curl-parse-response api string "" nil)))
                   (funcall on-text :text (plist-get resp :text) :completep t)))))))))
    proc))

;;; url-retrieve
(cl-defun robby--url-retrieve (&key api payload on-text on-error &allow-other-keys)
  (let* ((inhibit-message t)
         (message-log-max nil)
         (original-buffer (current-buffer))
         (url (robby--request-url api))
         (url-request-method "POST")
         (url-request-data
          (encode-coding-string (json-encode payload) 'utf-8))
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
              (err (robby--request-parse-error resp)))
         (if err
             (funcall on-error error-msg)
           ;; TODO maybe rename robby--chunk-content
           (let ((text (robby--chunk-content api resp nil)))
             (funcall on-text :text text :completep t))))))))

;;; robby--request
(defun robby--request-available-p ()
  (executable-find "curl"))

(cl-defun robby--request (&key api payload on-text on-error streamp)
  (if (and robby-use-curl (robby--request-available-p))
      (robby--curl :api api :payload payload :on-text on-text :on-error on-error :streamp streamp)
    (robby--url-retrieve :api api :payload payload :on-text on-text :on-error on-error)))

(provide 'robby-request)

;; robby-request.el ends here
