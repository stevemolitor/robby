;;; robby-actions-test.el  --- unit test for robby response handlers  -*- lexical-binding:t -*-

(require 'robby-actions)

;;; Code:

(ert-deftest robby--prepend-response-to-region ()
  (with-temp-buffer
    (insert "region text")
    (robby--prepend-response-to-region "AI response" (point-min) (point-max) (current-buffer))
    (should
     (equal (buffer-substring-no-properties (point-min) (point-max))
            "AI response\nregion text"))))

(ert-deftest robby--append-response-after-region ()
  (with-temp-buffer
    (insert "region text")
    (robby--append-response-after-region "AI response" (point-min) (point-max) (current-buffer))
    (should
     (equal (buffer-substring-no-properties (point-min) (point-max))
            "region text\nAI response"))))

(ert-deftest robby--replace-region-with-response ()
  (with-temp-buffer
    (insert "region text")
    (robby--replace-region-with-response "AI response" (point-min) (point-max) (current-buffer))
    (should
     (equal (buffer-substring-no-properties (point-min) (point-max))
            "AI response"))))

(provide 'robby-actions-test)

;;; robby-actions-test.el ends here
