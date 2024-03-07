;;; robby-validation-test.el  --- test for robby validation functions  -*- lexical-binding:t -*-

(require 'robby-validation)

;;; Code:

(ert-deftest robby--validate-chat-temperature ()
  (should (equal (robby--validate 'chat-temperature "1.0") nil))
  (should (equal (robby--validate 'chat-temperature "0.0") nil))
  (should (equal (robby--validate 'chat-temperature "2.0") nil))
  (should (equal (robby--validate 'chat-temperature "") nil))
  (should (equal (robby--validate 'chat-temperature nil) nil))
  
  (should (equal (robby--validate 'chat-temperature "-0.1") "Please enter a number between 0.0 and 2.0."))
  (should (equal (robby--validate 'chat-temperature "2.1") "Please enter a number between 0.0 and 2.0."))

  (should (equal (robby--validate 'chat-temperature "asdf") "Please enter a number between 0.0 and 2.0."))
  (should (equal (robby--validate 'chat-temperature 'asdf) "Please enter a number between 0.0 and 2.0."))

  (should (equal (robby--validate 'chat-temperature 1.0) nil))
  (should (equal (robby--validate 'chat-temperature 0.0) nil))
  (should (equal (robby--validate 'chat-temperature 2.0) nil))
  (should (equal (robby--validate 'chat-temperature -0.1) "Please enter a number between 0.0 and 2.0."))
  (should (equal (robby--validate 'chat-temperature 2.1) "Please enter a number between 0.0 and 2.0.")))

(provide 'robby-validations-test)

;; robby-validations-test.el ends here
