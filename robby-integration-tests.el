;;; robby-integration-test.el  --- integration test for robby commands  -*- lexical-binding:t -*-

(require 'cl)
(require 'ert-async)
(require 'seq)

(require 'robby-commands)
(require 'robby-customization)
(require 'robby-history)
(require 'robby-run-command)

;;; Code:

;;; tests
(cl-defun robby--test-run-command (&key streamp done)
  (let ((robby-stream-p t)
        (cb (cl-function (lambda (&key text completep &allow-other-keys)
                           (if completep
                               (should (string-match-p "1865" text)))
                           (funcall done)))))
    (robby-run-command
     :prompt "What year did Abraham Lincoln die?"
     :action cb)))

(ert-deftest-async robby--integration-test-run-command-with-curl-streaming (done)
  (robby--test-run-command :streamp t :done done))

(ert-deftest-async robby--integration-test-run-command-with-curl-no-streaming (done)
  (robby--test-run-command :streamp nil :done done))

;;; suite
(defun robby-run-integration-tests ()
  (interactive)
  (setq ert-async-timeout 20)
  (ert "^robby--integration"))

(provide 'robby-integration-tests)

;;; robby-integration-tests.el ends here
