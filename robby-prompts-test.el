;;; robby-prompts-test.el  --- unit tests for robby prompt functions  -*- lexical-binding:t -*-

(require 'robby-prompts)

;;; Code:

(ert-deftest robby--get-prompt-from-region--region-has-text ()
  (with-temp-buffer
    (insert "region text")
    (should (equal (robby--get-prompt-from-region nil) '("region text" . "region text")))))

(provide 'robby-prompts-test)

;;; robby-prompts-test.el ends here
