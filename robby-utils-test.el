;;; robby-utils-test.el  --- tests for robby utility functions -*- lexical-binding:t -*-

(require 'robby-utils)

;;; Code:

(ert-deftest robby--format-message-text ()
  (should (equal (robby--format-message-text "29%") "29%%")))

(ert-deftest robby--plist-to-alist ()
  (should (equal (robby--plist-to-alist '(:a 1 :b 2))
                 '((:a . 1) (:b . 2)))))

(provide 'robby-utils-test)

;;; robby-utils-test.el ends here
