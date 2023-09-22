;;; robby-utils-test.el  --- tests for robby utility functions -*- lexical-binding:t -*-

(require 'robby-utils)

;;; Code:

;;; string utils tests
(ert-deftest robby--kebab-to-snake-case ()
  (should (equal (robby--kebab-to-snake-case "a-b-c") "a_b_c")))

(ert-deftest robby--snake-to-space-case ()
  (should (equal (robby--snake-to-space-case "a_b_c") "a b c")))

;;; property list utils tests
(ert-deftest robby--plist-to-alist ()
  (should (equal (robby--plist-to-alist '(:a 1 :b 2))
                 '((:a . 1) (:b . 2)))))

(ert-deftest robby--plist-to-transient-args ()
  (should (equal (robby--plist-to-transient-args '(:a 1 :b "2"))
                 '("a=1" "b=2"))))

(ert-deftest robby--plist-keys ()
  (should (equal (robby--plist-keys '(:a 1 :b "2"))
                 '(:a :b))))

;;; robby--options test
(ert-deftest robby--options ()
  (let ((robby-chat-model "gpt-4")
        (robby-chat-max-tokens 100)
        (robby-chat-temperature 1.0))
    (should (equal (robby--options '(:max-tokens 2))
                   '(("max_tokens" . 2)
                     ("model" . "gpt-4")
                     ("temperature" . 1.0))))))

(ert-deftest robby--request-input--no-history ()
  (robby--with-history
   nil
   (let ((expected-json "{\"messages\":[{\"role\":\"user\",\"content\":\"hello\"}]}")
         (input (robby--request-input "hello" nil)))
     (should (equal input `((messages . [((role . "system") (content . ,robby-chat-system-message))
                                         ((role . "user") (content . "hello"))])))))))

(ert-deftest robby--request-input--with-history ()
  (robby--with-history
   '(("Who won the world series in 2020?" . "The Los Angeles Dodgers won the World Series in 2020."))
   (should (equal
            (robby--request-input "Where was it played?" t)
            `((messages .
                        [((role . "system") (content . ,robby-chat-system-message))
                         ((role . "user") (content . "Who won the world series in 2020?"))
                         ((role . "assistant") (content . "The Los Angeles Dodgers won the World Series in 2020."))
                         ((role . "user") (content . "Where was it played?"))
                         ]))))))

(ert-deftest robby--chunk-content--no-streaming ()
  (let ((resp '((choices . [((index . 0)
                             (message
                              (role . "assistant")
                              (content . "Hello! How can I assist you today?"))
                             (finish_reason . "stop"))]))))
    (should (equal
             (robby--chunk-content resp nil)
             "Hello! How can I assist you today?"))))

(ert-deftest robby--chunk-content--streaming ()
  (let ((resp '((choices . [((index . 0)
                             (delta
                              (role . "assistant")
                              (content . "Hello"))
                             (finish_reason . "stop"))]))))
    (should (equal
             (robby--chunk-content resp t)
             "Hello"))))

(ert-deftest robby--models-for-api ()
  (let ((all-models '("gpt-3.5-turbo" "gpt-4" "text-davinci-003" "text-davinci-002" "text-davinci-edit-001")))
    (should (equal (robby--models-for-api all-models) '("gpt-3.5-turbo" "gpt-4")))))

;;; prompt templates
(ert-deftest robby--format-prompt--with-prompt-spec-arg ()
  (should (equal
           (robby--format-prompt "file extension: %e" '((?e . "el")))
           "file extension: el")))

(ert-deftest robby--format-prompt--with-repeats ()
  (should (equal
           (robby--format-prompt "file extension: %e %e" '((?e . "el")))
           "file extension: el el")))

(ert-deftest robby--format-prompt--with-missing-args ()
  (should (equal
           (robby--format-prompt "file extension:" '((?e . "el")))
           "file extension:")))

;;; grounding
(ert-deftest robby--ground-response--chain-fns ()
  (cl-letf (((symbol-function 'upper) (lambda (resp) (upcase resp)))
            ((symbol-function 'twice) (lambda (resp) (concat resp resp))))
    (should (equal (robby--ground-response "response" '(upper twice))
                   "RESPONSERESPONSE"))))

(ert-deftest robby--ground-response--single-fn ()
  (cl-letf (((symbol-function 'upper) (lambda (resp) (upcase resp))))
    (should (equal (robby--ground-response "response" #'upper)
                   "RESPONSE"))))

(provide 'robby-utils-test)

;;; robby-utils-test.el ends here

