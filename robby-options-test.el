;;; robby-options-test.el  ---  unit tests for robby-options functions  -*- lexical-binding:t -*-

(require 'robby-options)

;;; Code:

(ert-deftest robby--options ()
  (let ((robby-api "completions")
        (robby-completions-max-tokens 1)
        (robby-completions-model "text-davinci-003")
        (robby-completions-temperature 1.0))
    (should (equal (robby--options '(:max-tokens 2))
                   '(("max_tokens" . 2)
                     ("model" . "text-davinci-003")
                     ("temperature" . 1.0))))))

(provide 'robby-options-test)

;;; robby-options-test.el ends here
