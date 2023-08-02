;;; robby-utils-test.el  --- tests for robby utility functions -*- lexical-binding:t -*-

(require 'robby-utils)

;;; Code:

(ert-deftest robby--format-message-text ()
  (should (equal (robby--format-message-text "29%") "29%%")))

(ert-deftest robby--kebab-to-snake-case ()
  (should (equal (robby--kebab-to-snake-case "a-b-c") "a_b_c")))

(ert-deftest robby--snake-to-space-case ()
  (should (equal (robby--snake-to-space-case "a_b_c") "a b c")))

(ert-deftest robby--plist-to-alist ()
  (should (equal (robby--plist-to-alist '(:a 1 :b 2))
                 '((:a . 1) (:b . 2)))))

(ert-deftest robby--plist-to-transient-args ()
  (should (equal (robby--plist-to-transient-args '(:a 1 :b "2"))
                 '("a=1" "b=2"))))

(provide 'robby-utils-test)

;;; robby-utils-test.el ends here
