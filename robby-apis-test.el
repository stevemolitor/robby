;;; robby-apis-tests.el  --- robby api methods tests  -*- lexical-binding:t -*-

(require 'robby-apis)
(require 'robby-test-utils)

;;; Code:

(ert-deftest robby--request-input--no-history--completions-api ()
  (robby--with-history nil
                        (should (equal (robby--request-input 'completions "How big is the sun?" nil)
                                       '((prompt . "How big is the sun?"))))))

(ert-deftest robby--request-input---with-history--completions-api ()
  (robby--with-history
   '("Who won the world series in 2020?"
     "The Los Angeles Dodgers won the World Series in 2020.")
   (should (equal (robby--request-input 'completions "Where was it played?" t)
                  '((prompt . "Who won the world series in 2020?\nThe Los Angeles Dodgers won the World Series in 2020.\nWhere was it played?"))))))

(ert-deftest robby--parse-response--completions-api ()
  (should (equal
           (robby--parse-response 'completions '((choices . [((text . "text response"))])))
           "text response")))


;;; chat api
(ert-deftest robby--request-input--no-history--chat-api ()
  (robby--with-history
   nil
   (let ((expected-json "{\"messages\":[{\"role\":\"user\",\"content\":\"hello\"}]}")
         (input (robby--request-input 'chat "hello" nil)))
     (should (equal input '((messages . [((role . "user") (content . "hello"))]))))
     ;; (should (equal (json-encode input) expected-json))
     )))

(ert-deftest robby--request-input--with-history--chat-api ()
  (robby--with-history
   '(("Who won the world series in 2020?" . "The Los Angeles Dodgers won the World Series in 2020."))
   (should (equal
            (robby--request-input 'chat "Where was it played?" t)
            '((messages .
                        [((role . "user") (content . "Who won the world series in 2020?"))
                         ((role . "assistant") (content . "The Los Angeles Dodgers won the World Series in 2020."))
                         ((role . "user") (content . "Where was it played?"))
                         ]))))))

(ert-deftest robby--parse-response--chat-api ()
  (should (equal
           (robby--parse-response
            'chat
            '((choices . [((message (content . "chat response")))])))
           "chat response")))

(provide 'robby-apis-test)

;;; robby-apis-test.el ends here
