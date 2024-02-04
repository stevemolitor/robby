;;; robby-integration-test.el  --- integration test for robby commands  -*- lexical-binding:t -*-

(require 'cl)
(require 'ert)
(require 'ert-async)
(require 'seq)

(require 'robby-commands)
(require 'robby-customization)
(require 'robby-history)
(require 'robby-run-command)

;;; Code:

;;; tests
(cl-defun robby--test-run-command (&key use-curl streamp done)
  (let ((robby-stream-p streamp)
        (robby-use-curl use-curl)
        (cb (cl-function (lambda (&key text completep &allow-other-keys)
                           (if completep
                               (should (string-match-p "1865" text)))
                           ;; TODO done is called multiple times
                           (funcall done)))))
    (robby-run-command
     :prompt "What year did Abraham Lincoln die?"
     :action cb)))

(ert-deftest-async robby--integration-test-run-command-curl-streaming (done)
  (robby--test-run-command :use-curl t :streamp t :done done))

(ert-deftest-async robby--integration-test-run-command-no-curl-no-streaming (done)
  (robby--test-run-command :use-curl nil :streamp nil :done done))

;;; suite
(defun robby-run-integration-tests ()
  (interactive)
  (setq ert-async-timeout 20)
  (ert "^robby--integration"))

(provide 'robby-integration-tests)

;;; robby-integration-tests.el ends here
