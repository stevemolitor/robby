;;; robby-logging-test.el  --- unit tests for robby logging functions  -*- lexical-binding:t -*-

(require 'ert)

(require 'robby-logging)

;;; Code:

(ert-deftest robby--logging ()
  (let ((robby-logging t)
        (robby--log-buffer "*robby-test-log*"))
    (unwind-protect
        (progn
          (robby--log "log message")
          (with-current-buffer robby--log-buffer
            (should (equal (buffer-substring-no-properties (point-min) (point-max)) "log message")))
          (let ((robby-logging nil))
            (robby--log "another message")
            (with-current-buffer robby--log-buffer
              (should (equal (buffer-substring-no-properties (point-min) (point-max)) "log message")))))
      (kill-buffer robby--log-buffer))))

(provide 'robby-logging-test)

;;; robby-logging-test.el ends here
