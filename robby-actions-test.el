;;; robby-actions-test.el  --- unit test for robby response handlers  -*- lexical-binding:t -*-

(require 'robby-actions)

;;; Code:

(ert-deftest robby-prepend-response-to-region ()
  (with-temp-buffer
    (insert "region text")
    (robby-prepend-response-to-region :text "AI response" :beg (point-min) :chars-processed 0)
    (should
     (equal (buffer-substring-no-properties (point-min) (point-max))
            "AI response\nregion text"))))

(ert-deftest robby-append-response-to-region ()
  (with-temp-buffer
    (insert "region text")
    (robby-append-response-to-region :text "AI response" :end (point-max) :chars-processed 0)
    (should
     (equal (buffer-substring-no-properties (point-min) (point-max))
            "region text\nAI response"))))

(ert-deftest robby-replace-region-with-response ()
  (with-temp-buffer
    (insert "region text")
    (robby-replace-region-with-response :text "AI response" :beg (point-min) :end (point-max) :chars-processed 0)
    (should
     (equal (buffer-substring-no-properties (point-min) (point-max))
            "AI response"))))

(provide 'robby-actions-test)

;;; robby-actions-test.el ends here
