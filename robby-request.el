;;; robby-request.el  --- Make robby requests via curl or url-retrieve  -*- lexical-binding:t -*-

(require 'files)
(require 'json)
(require 'seq)
(require 'url-vars)

(require 'robby-provider) ; require first to make sure robby--provider-settings is defined

(require 'robby-api-key)
(require 'robby-customization)
(require 'robby-logging)
(require 'robby-utils)

;;; request util functions
(defun robby--request-parse-error-string (string)
  "Get error from JSON string ERR."
  (ignore-errors
    (robby-provider-parse-error (json-read-from-string string))))

(defun robby--request-get-error (string)
  "Get error from response STRING, or nil if no error.

If there is a response status and it is not 200, try to parse the
error message from the response and return that, otherwise return
a generic error message. Otherwise return nil (no error)."
  (let ((provider (robby--provider-name))
        (status (robby--parse-http-status string)))
    (if (and (numberp status) (not (eq status 200)))
        (let ((error-msg (robby--request-parse-error-string string)))
          (if error-msg
              (format "%s API returned error - '%s'" provider error-msg)
            (if (numberp status)
                (format "Unexpected response status %S from %s API request" status provider)
              (format "Unexpected response from %S API request: %S" provider string)))))))

;; TODO consider passing url to robby--request
(defun robby--chat-url ()
  "Get the chat API URL."
  (concat "https://" (robby--provider-host) (robby--provider-api-base-path)))

;;; curl
(defvar robby--curl-options
  '("--compressed"
    "--disable"
    "--silent"
    "-m 600"
    "-w HTTP STATUS: %{http_code}\n"
    "-H" "Content-Type: application/json"))

(defun robby--curl-parse-chunk (remaining data)
  "Parse json documents in current buffer from DATA string.

Prepend REMAINING text incomplete JSON in last chunk. Return
remaining incomplete text in this document.

Ignores \"[DONE]\".

Returns a plist with remaining un-parsed text (if any) and a list
of parsed JSON objects:

    (:remaining \"text\" :parsed \\='())"
  (with-temp-buffer
    (let ((new-remaining "")
          (parsed '())
          (done)
          (pos (point-min)))
      (insert remaining)
      (insert data)
      (goto-char pos)
      (while (and (not done) (not (looking-at " *\\[DONE\\]")))
        (condition-case _err
            (progn
              (setq pos (point))
              (let* ((json-object-type 'alist)
                     (obj (json-read)))
                (setq parsed (cons obj parsed))))
          (error
           (setq done t)
           (setq new-remaining (buffer-substring pos (point-max))))))
      `(:remaining ,new-remaining :parsed ,(nreverse parsed)))))

(defun robby--curl-parse-response (string remaining streamp)
  "Parse JSON curl response from data in STRING and REMAINING unparsed text.

STREAMP is non-nil if the response is a stream."
  (let* ((data (replace-regexp-in-string (rx bol "data:") "" string))
         (json (robby--curl-parse-chunk remaining data))
         (parsed (plist-get json :parsed))
         (text (string-join (seq-filter #'stringp (seq-map (lambda (chunk) (robby--chunk-content chunk streamp)) parsed)))))
    `(:text ,text :remaining ,(plist-get json :remaining))))

(cl-defun robby--curl (&key payload on-text on-error streamp)
  "Make a request to the OpenAI API using curl.

PAYLOAD is the request payload alist.

ON-TEXT is the callback for when a chunk of text is received.

ON-ERROR is the callback for when an error is received.

STREAMP is non-nil if the response is a stream."
  (let* ((input-json (json-encode (append payload (if streamp '((stream . t)) nil))))
         (curl-options (append robby--curl-options
                               `("-H" ,(format "Authorization: Bearer %s" (robby--get-api-key))
                                 "-d" ,input-json)))
         (proc-buffer (if streamp nil (generate-new-buffer (format "*robby-request-%s*" (buffer-name)))))
         (proc (condition-case err
                   (apply #'start-process
                          "curl"
                          proc-buffer
                          "curl"
                          (robby--chat-url)
                          curl-options)
                 (error (funcall on-error err)))))
    (let ((remaining "")
          (text "")
          (errored nil))
      (robby--log (format "# Curl request JSON payload:\n%s\n" input-json))
      (when streamp
        (set-process-filter
         proc
         (lambda (proc string)
           (robby--log (format "# Raw curl response chunk:\n%s\n" string))
           (condition-case err
               (let ((error-msg (robby--request-get-error string)))
                 (if error-msg
                     (progn
                       (setq errored t)
                       (funcall on-error error-msg))
                   (let ((resp (robby--curl-parse-response string remaining streamp)))
                     (setq remaining (plist-get resp :remaining))
                     (funcall on-text :text (plist-get resp :text) :completep nil))))
             (error
              (error "Robby: unexpected error processing curl response: %S" err)
              (kill-process proc))))))
      (set-process-sentinel
       proc
       (lambda (_proc _status)
         (if streamp
             (if (not errored)
                 (funcall on-text :text text :completep t))
           (with-current-buffer proc-buffer
             (let* ((string (buffer-string))
                    (error-msg (robby--request-get-error string)))
               (if error-msg
                   (funcall on-error error-msg)
                 (let ((resp (robby--curl-parse-response string "" nil)))
                   (funcall on-text :text (plist-get resp :text) :completep t)))))))))
    proc))

;;; url-retrieve
(cl-defun robby--url-retrieve (&key payload on-text on-error &allow-other-keys)
  "Make a request to the OpenAI API using `url-retrieve'.

Does not support streaming responses. Use `robby--curl' for that.

PAYLOAD is the request payload alist.

ON-TEXT is the callback for when a chunk of text is received.

ON-ERROR is the callback for when an error is received."
  (let* ((inhibit-message t)
         (message-log-max nil)
         (url-request-method "POST")
         (url-request-data
          (encode-coding-string (json-encode payload) 'utf-8))
         (url-request-extra-headers
          `(("Content-Type" . "application/json")
            ("Authorization:" . ,(concat "Bearer " (robby--get-api-key)))))
         (inhibit-message t)
         (message-log-max nil))
    (url-retrieve
     (robby--chat-url)
     (lambda (_status)
       (robby--log (format "# URL retrieve response buffer contents: %s" (buffer-substring-no-properties (point-min) (point-max))))
       (goto-char (point-min))
       (re-search-forward "^{")
       (backward-char 1)
       (let* ((json-object-type 'alist)
              (resp (json-read))
              (err (robby-provider-parse-error resp)))
         (if err
             (funcall on-error err)
           (let ((text (robby--chunk-content resp nil)))
             (funcall on-text :text text :completep t))))))))

;;; robby--request
(defun robby--request-available-p ()
  "Check if curl is available."
  (executable-find "curl"))

(cl-defun robby--request (&key payload on-text on-error streamp)
  "Make a request to the OpenAI API.

Use curl if available, otherwise use `url-retrieve'.

PAYLOAD is the request payload alist.

ON-TEXT is the callback for when a chunk of text is received.

ON-ERROR is the callback for when an error is received.

STREAMP is non-nil if the response is a stream."
  (if (and robby-use-curl (robby--request-available-p))
      (robby--curl :payload payload :on-text on-text :on-error on-error :streamp streamp)
    (robby--url-retrieve :payload payload :on-text on-text :on-error on-error)))

(provide 'robby-request)

;;; robby-request.el ends here
