;;; robby-utils-test.el  --- tests for robby utility functions -*- lexical-binding:t -*-

(require 'robby-utils)

;;; Code:

;;; string utils tests
(ert-deftest robby--kebab-to-snake-case ()
  (should (equal (robby--kebab-to-snake-case "a-b-c") "a_b_c")))

(ert-deftest robby--snake-to-space-case ()
  (should (equal (robby--snake-to-space-case "a_b_c") "a b c")))

;;; property list utils tests
(ert-deftest robby--plist-to-alist ()
  (should (equal (robby--plist-to-alist '(:a 1 :b 2))
                 '((:a . 1) (:b . 2)))))

(ert-deftest robby--plist-to-transient-args ()
  (should (equal (robby--plist-to-transient-args '(:a 1 :b "2"))
                 '("a=1" "b=2"))))

(ert-deftest robby--plist-keys ()
  (should (equal (robby--plist-keys '(:a 1 :b "2"))
                 '(:a :b))))

;;; robby--options test
(ert-deftest robby--options ()
  (let ((api "completions")
        (robby-completions-max-tokens 1)
        (robby-completions-model "text-davinci-003")
        (robby-completions-temperature 1.0))
    (should (equal (robby--options api '(:max-tokens 2))
                   '(("max_tokens" . 2)
                     ("model" . "text-davinci-003")
                     ("temperature" . 1.0))))))

(provide 'robby-utils-test)

;;; robby-utils-test.el ends here
