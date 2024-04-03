;;; robby--providers-test.el  --- tests for robby  provider functions and methods -*- lexical-binding:t -*-

(require 'ert)

(require 'robby-providers)

;;; robby--providers-parse-error tests
(ert-deftest robby--providers-parse-error-standard ()
  (let ((robby-provider 'openai))
    (should (equal (robby--providers-parse-error '((error . ((message . "whoops")))))
                   "whoops"))))

(ert-deftest robby--providers-parse-error-standard-no-error ()
  (let ((robby-provider 'openai))
    (should (eql (robby--providers-parse-error '((not-error . ((message . "whoops")))))
                   nil))))

(ert-deftest robby--providers-parse-error-mistral ()
  (let ((robby-provider 'mistral))
    (should (equal (robby--providers-parse-error '((object . "error") (message . "whoops")))
                   "whoops"))))

(ert-deftest robby--providers-parse-error-mistral-no-error ()
  (let ((robby-provider 'mistral))
    (should (eql (robby--providers-parse-error '((object . "not-error") (message . "whoops")))
                 nil))))
