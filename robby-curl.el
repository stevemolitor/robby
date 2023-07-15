;;; robby-curl.el  --- Make robby requests via curl  -*- lexical-binding:t -*-

;;; Commentary:

(require 'cl-macs)
(require 'json)
(require 'seq)

(require 'robby-api-key)
(require 'robby-logging)

;;; Code:
(defvar robby--curl-options
  '("--compressed"
    "--disable"
    "--silent"
    "-m 600"
    "-H" "Content-Type: application/json"))

(defun robby--chunk-content (chunk)
  (assoc-default 'content (assoc-default 'delta (seq-first (assoc-default 'choices chunk)))))

(defun robby--parse-chunk (remaining data)
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
          ((error)
           (setq done t)
           (setq new-remaining (buffer-substring pos (point-max))))))
      `(:remaining ,new-remaining :parsed ,(nreverse parsed)))))

(defconst robby--curl-unknown-error "Unexpected error making OpenAI request via curl" )

(defun robby--curl-parse-error (string)
  (condition-case _err
      (cdr (assoc 'message (assoc 'error (json-read-from-string string))))
    (error nil)))

(defun robby--curl-parse-response (string remaining)
  (let ((error-msg (robby--curl-parse-error string)))
    (if error-msg
        `(:error ,error-msg)
      (let* ((data (replace-regexp-in-string (rx bol "data:") "" string))
             (json (robby--parse-chunk remaining data))
             (parsed (plist-get json :parsed))
             (text (string-join (seq-filter #'stringp (seq-map #'robby--chunk-content parsed)))))
        (setq remaining (plist-get json :remaining))
        `(:text ,text :remaining ,(plist-get json :remaining))))))

(cl-defun robby--curl (&key payload on-text on-error never-stream-p)
  (let* ((input-obj (append '((stream . t)) payload))
         (input-json (json-encode input-obj))
         (curl-options (append robby--curl-options
                               `("-H" ,(format "Authorization: Bearer %s" (robby-get-api-key-from-auth-source))
                                 "-d" ,input-json)))
         (proc (condition-case err
                   (apply #'start-process
                          "curl"
                          "*robby-curl-process*"
                          "curl"
                          "https://api.openai.com/v1/chat/completions"
                          curl-options)
                 (error (funcall on-error err)))))
    (let ((remaining "")
          (text ""))
      (set-process-filter
       proc
       (lambda (_proc string)
         (robby--log (format "\n# raw data from curl: %s\n" string))
         (let ((error-msg (robby--curl-parse-error string)))
           (if error-msg
               (funcall on-error error-msg)
             (let ((resp (robby--curl-parse-response string remaining)))
               (setq remaining (plist-get resp :remaining))
               (funcall on-text :text (plist-get resp :text) :completep nil))))))
      (set-process-sentinel
       proc
       (lambda (_proc _string)
         (funcall on-text :text text :completep t))))
    proc))

(provide 'robby-curl)

;; robby-curl.el ends here
