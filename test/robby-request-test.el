;;; robby-request-test.el  --- test robby request functions  -*- lexical-binding:t -*-

(require 'ert)

(require 'robby-request)

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

(provide 'robby-request-test)

;;; robby-request-test.el ends here
