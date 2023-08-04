;;; robby-history-test.el  ---  unit test for robby history functions  -*- lexical-binding:t -*-

(require 'robby-history)

;;; Code:

(ert-deftest robby--history-push ()
  (let ((robby--history nil))
    (robby--history-push "prompt" "response")
    (should (equal robby--history '(("prompt" . "response"))))
    (robby--history-push "another prompt" "another response")
    (should (equal robby--history '(("prompt" . "response")
                                     ("another prompt" . "another response"))))))

(provide 'robby-history-test)

;;; robby-history-test.el ends here
