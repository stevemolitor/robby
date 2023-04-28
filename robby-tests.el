;;; robby-tests.el  --- robby unit test suite  -*- lexical-binding:t -*-

;;; Commentary:

;; Robby unit test suite. This suite does not include integration
;; tests that require an OpenAI API key, just unit tests we can run in
;; CI.

(require 'robby-history-test)
(require 'robby-actions-test)
(require 'robby-logging-test)
(require 'robby-options-test)
(require 'robby-apis-test)
(require 'robby-utils-test)

(provide 'robby-tests)

;;; robby-tests.el ends here
