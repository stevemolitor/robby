;;; robby-grounding-fns-test.el  --- unit tests for robby grounding functions  -*- lexical-binding:t -*-

;;; Code:

(require 'ert)

(require 'robby-grounding-fns)

;;; robby-extract-fenced-text tests
(ert-deftest robby-extract-fenced-text--has-block ()
  (let ((response "Here is some example code:

```
(defun add (a b)
  (+ a b)
```
"))
    (should (equal (robby-extract-fenced-text response) "(defun add (a b)
  (+ a b)
"))))

(ert-deftest robby-extract-fenced-text--has-language ()
  (let ((response "Here is some example code:

```emacs-lisp
(defun add (a b)
  (+ a b)
```
"))
    (should (equal (robby-extract-fenced-text response) "(defun add (a b)
  (+ a b)
"))))

(ert-deftest robby-extract-fenced-text--no-block ()
  (let ((response "
(defun add (a b)
  (+ a b)
"))
    (should (equal (robby-extract-fenced-text response) response))))

;;; robby-format-message-text tests
(ert-deftest robby-format-message-text ()
  (should (equal (robby-format-message-text "29%") "29%%")))

;;; robby-newlines tests
(ert-deftest robby-remove-trailing-end-of-line ()
  (should (equal (robby-remove-trailing-end-of-line "Hello, \nworld!\n") "Hello, \nworld!"))
  (should (equal (robby-remove-trailing-end-of-line "Hello, world!\n\n") "Hello, world!"))
  (should (equal (robby-remove-trailing-end-of-line "No end of line character") "No end of line character"))
  (should (equal (robby-remove-trailing-end-of-line "") "")))

(provide 'robby-grounding-fns-test)

;; robby-grounding-fns-test.el ends here
