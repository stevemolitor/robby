;;; robby-grounding-fns-test.el  --- unit tests for robby grounding functions  -*- lexical-binding:t -*-

;;; Code:

(require 'ert)

(require 'robby-grounding-fns)

;;; robby-extract-code-block tests
(ert-deftest robby-extract-code-block--has-block ()
  (let ((response "Here is some example code:

```
(defun add (a b)
  (+ a b)
```
"))
    (should (equal (robby-extract-code-block response) "(defun add (a b)
  (+ a b)
"))))

(ert-deftest robby-extract-code-block--no-block ()
  (let ((response "
(defun add (a b)
  (+ a b)
"))
    (should (equal (robby-extract-code-block response) response))))

;;; robby-format-message-text tests
(ert-deftest robby-format-message-text ()
  (should (equal (robby-format-message-text "29%") "29%%")))

(provide 'robby-grounding-fns-test)

;; robby-grounding-fns-test.el ends here
