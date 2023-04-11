;;; robby-test-utils.el  --- robby test util functions  -*- lexical-binding:t -*-

;;; Commentary:

;; Robby test util functions.

(require 'robby-customization)
(require 'robby-history)

;;; Code:

(defmacro robby--with-history (history &rest body)
  "Execute BODY with history set to HISTORY."
  `(let ((robby--history ,history))
     ,@body))

(provide 'robby-test-utils)

;;; robby-test-utils.el ends here
