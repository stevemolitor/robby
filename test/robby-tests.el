;;; robby-tests.el  --- robby unit test suite  -*- lexical-binding:t -*-

;;; Commentary:

;; Robby unit test suite. This suite does not include integration
;; tests that require an OpenAI API key, just unit tests we can run in
;; CI.

(require 'ert)

(require 'robby-actions-test)
(require 'robby-grounding-fns-test)
(require 'robby-history-test)
(require 'robby-logging-test)
(require 'robby-request-test)
(require 'robby-utils-test)

(provide 'robby-tests)

;;; robby-tests.el ends here
