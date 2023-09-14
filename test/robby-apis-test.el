;;; robby-apis-tests.el  --- robby api methods tests  -*- lexical-binding:t -*-

(require 'robby-apis)
(require 'robby-test-utils)

;;; Code:

;;; completions api
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

(ert-deftest robby--models-for-api--completions-api ()
  (let ((all-models '("gpt-3.5-turbo" "gpt-4" "text-davinci-003" "text-davinci-002" "text-davinci-edit-001")))
    (should (equal (robby--models-for-api 'completions all-models) '("text-davinci-003" "text-davinci-002")))))

;;; chat api
(ert-deftest robby--request-input--no-history--chat-api ()
  (robby--with-history
   nil
   (let ((input (robby--request-input 'chat "hello" nil)))
     (should (equal input `((messages . [((role . "system") (content . ,robby-chat-system-message))
                                         ((role . "user") (content . "hello"))])))))))

(ert-deftest robby--request-input--with-history--chat-api ()
  (robby--with-history
   '(("Who won the world series in 2020?" . "The Los Angeles Dodgers won the World Series in 2020."))
   (should (equal
            (robby--request-input 'chat "Where was it played?" t)
            `((messages .
                        [((role . "system") (content . ,robby-chat-system-message))
                         ((role . "user") (content . "Who won the world series in 2020?"))
                         ((role . "assistant") (content . "The Los Angeles Dodgers won the World Series in 2020."))
                         ((role . "user") (content . "Where was it played?"))
                         ]))))))

(ert-deftest robby--models-for-api--chat-api ()
  (let ((all-models '("gpt-3.5-turbo" "gpt-4" "text-davinci-003" "text-davinci-002" "text-davinci-edit-001")))
    (should (equal (robby--models-for-api 'chat all-models) '("gpt-3.5-turbo" "gpt-4")))))

(ert-deftest robby--chunk-content--chat-api-no-streaming ()
  (let ((resp '((choices . [((index . 0)
                             (message
                              (role . "assistant")
                              (content . "Hello! How can I assist you today?"))
                             (finish_reason . "stop"))]))))
    (should (equal
             (robby--chunk-content 'chat resp nil)
             "Hello! How can I assist you today?"))))

(ert-deftest robby--chunk-content--chat-api-streaming ()
  (let ((resp '((choices . [((index . 0)
                             (delta
                              (role . "assistant")
                              (content . "Hello"))
                             (finish_reason . "stop"))]))))
    (should (equal
             (robby--chunk-content 'chat resp t)
             "Hello"))))

;;; images api
(ert-deftest robby--request-input--images-api ()
  (robby--with-history
   nil
   (let ((input (robby--request-input 'images "a white siamese cat" nil)))
     (should (equal
              input
              '((prompt . "a white siamese cat") (size . "1024x1024")))))))

(ert-deftest robby--chunk-content--images-api ()
            (let ((resp '((created . 1694693216) (data . [((url . "https://image-url"))]))))
              (should (equal
                       "https://image-url"
                       (robby--chunk-content 'images resp nil)))))

(provide 'robby-apis-test)

;;; robby-apis-test.el ends here
