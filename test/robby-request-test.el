;;; robby-request-test.el  --- test robby request functions  -*- lexical-binding:t -*-

(require 'ert)

(require 'robby-request)
(require 'robby-openai-provider)
(require 'robby-togetherai-provider)

;;; Code:

(defun robby--read-file-into-string (filepath)
  "Return filepath's file content."
  (with-temp-buffer
    (insert-file-contents filepath)
    (buffer-string)))

(ert-deftest robby--curl-parse-response--streaming-response ()
  (let ((parsed (robby--curl-parse-response (robby--read-file-into-string "./fixtures/streaming-response-complete.txt") "" t)))
    (should (equal (plist-get parsed :text) "Hello there!"))
    (should (equal (string-trim (plist-get parsed :remaining)) "[DONE]"))))

(ert-deftest robby--curl-parse-response--streaming-response-incomplete () ;
  (let ((part1 (robby--curl-parse-response (robby--read-file-into-string "./fixtures/streaming-response-incomplete-part-1.txt") "" t)))
    (should (equal (plist-get part1 :text) "Hello"))
    (should (equal (string-trim (plist-get part1 :remaining)) "{\"id\":\"chatcmpl-a\",\"object\":\"chat.completion.chu"))))

(ert-deftest robby--curl-parse-response--finish-streaming-response ()
  (let ((part2 (robby--curl-parse-response (robby--read-file-into-string "./fixtures/streaming-response-incomplete-part-2.txt") "{\"id\":\"chatcmpl-a\",\"object\":\"chat.completion.chu" t)))
    (should (equal (plist-get part2 :text) " there!"))))

(ert-deftest robby--request-get-error--no-error-on-200 ()
  (dolist (provider '(openai togetherai))
    (let ((robby-provider provider)
          (resp-string (robby--read-file-into-string "./fixtures/streaming-response-complete-status-200.txt")))
      (should (equal (robby--parse-http-status resp-string) 200))
      (should (null (robby--request-get-error resp-string))))))

(ert-deftest robby--request-get-error--openai-error ()
  (let ((robby-provider 'openai)
        (resp-string (robby--read-file-into-string "./fixtures/streaming-response-openai-model-not-found.txt")))
    (should (equal (robby--parse-http-status resp-string) 404))
    (should (equal (robby--request-get-error resp-string)
                   "OpenAI API error - 'The model `adfasdf` does not exist or you do not have access to it.'"))))

(ert-deftest robby--request-get-error--together-error ()
  (let ((robby-provider 'togetherai)
        (resp-string (robby--read-file-into-string "./fixtures/streaming-response-togetherai-model-not-found.txt")))
    (should (equal (robby--parse-http-status resp-string) 404))
    (should (equal (robby--request-get-error resp-string)
                   "Together AI API error - 'Unable to access model adfasdf. Please visit https://api.together.xyz to see the list of supported models or contact the owner to request access.'"))))

(ert-deftest robby--request-get-error--generic-error-when-400-no-error-message ()
  (dolist (provider '(openai togetherai))
    (let ((robby-provider provider)
          (resp-string (robby--read-file-into-string "./fixtures/streaming-response-complete-status-400.txt")))
      (should (equal (robby--parse-http-status resp-string) 400))
      (should (string-match-p
               "Unexpected response status 400 from .+ API request"
               (robby--request-get-error resp-string))))))

(provide 'robby-request-test)

;;; robby-request-test.el ends here
