(in-package #:wepay)

(setf drakma:*header-stream* *standard-output*)

;;; The basic under the hound function

(defun wepay-api-call (cmd &key parameters client-id client-secret account-id user-id access-token)
  (let ((more-headers ()))
      (when (or access-token (not client-secret))
        (push `("Authorization" . ,*wepay-access-token*) more-headers))
      (when client-id
        (push `("client_id" . ,*wepay-client-id*) parameters))
      (when client-secret
        (push `("client_secret" . ,*wepay-client-secret*) parameters))
      (when account-id
        (push `("account_id" . ,*wepay-account-id*) parameters))
      (when user-id
        (push `("user_id" . ,*wepay-user-id*) parameters))
    (multiple-value-bind (stream headers)
        (if parameters
            (drakma:http-request (concatenate 'string *wepay-server* cmd)
                                 :method :post
                                 :additional-headers more-headers
                                 :accept "application/json"
                                 :content-type "application/json"
                                 :content (json:encode-json-to-string parameters)
                                 :external-format-out :utf-8
                                 :external-format-in :utf-8
                                 :want-stream t)
            (drakma:http-request (concatenate 'string *wepay-server* cmd)
                                 :additional-headers more-headers
                                 :external-format-in :utf-8
                                 :want-stream t))
      (json:decode-json stream))))

;;; The fun stuff: reading and parsing the doc pages to generate the client interface functions

(eval-when (:compile-toplevel :load-toplevel :execute)
  (in-readtable :fare-quasiquote))

(defun extract-function-info (html)
  (match html
    (`((:div :id ,id :class "call")(:h2 ,func-name)(:p ,@doc-string) ,@rest)
      `(,(make-function func-name doc-string (mapcan 'extract-args rest))))
    (`(,tag ,@content) (mapcan 'extract-function-info content))
    (t )))

(defun extract-args (html)
  (match html
    (`((:table :class "arguments") ,thead (:tbody ,@args)) (mapcan 'extract-args args))
    (`(:tr ((:td :class "param") ,arg-name) (,td2 ,required) (,td3 ,type) (,td4 ,@description))
      `((,arg-name ,(string-equal required "yes") ,type ,description)))
    (t )))

(defun lisp-symbol (name &optional (package 'wepay))
  (intern (string-upcase (substitute #\- #\_ name)) package))

(defparameter *global-args* '("client_id" "client_secret" "account_id" "user_id" "access_token"))

(defun make-function (name doc-string args)
  (let ((global-args ())
        (required-args ())
        (optional-args ()))
    (loop for (arg required desription) in args do
       (if (find arg *global-args* :test #'string-equal)
           (push (lisp-symbol arg :keyword) global-args)
           (if required
               (push arg required-args)
               (push arg optional-args))))
    (setf required-args (nreverse required-args)
          optional-args (nreverse optional-args))
    `(defun ,(lisp-symbol (format nil "wepay~a" (substitute #\- #\/ name)))
         (,@(mapcar 'lisp-symbol required-args)
          ,@(when optional-args (cons '&key (mapcar 'lisp-symbol optional-args))))
       ,(string-trim "
 	" (format nil "~{~a~}" doc-string))
       (let ((parameters (list ,@(loop for arg in required-args collect `(cons ,arg ,(lisp-symbol arg))))))
         ,@(loop for arg in optional-args
                 for lisp-arg = (lisp-symbol arg)
              collect `(when ,lisp-arg (push (cons ,arg ,lisp-arg) parameters)))
         (wepay-api-call ,name :parameters parameters ,@(loop for arg in global-args collect arg collect t))))))

(defun make-wepay-api-from-html (cmd)
  (extract-function-info
   (html-parse:parse-html
    (drakma:http-request (concatenate 'string *wepay-ref-url* cmd) :external-format-in :utf-8 :want-stream t))))

;;; Let's generate all the API functions!

#+nil
(with-open-file (s (asdf:system-relative-pathname :cl-wepay "./wepay-api.lisp") :direction :output :external-format :utf8)
  (format s "(in-package wepay)~%~%;;; The WePay API~%~%")
  (loop for func-def in (mapcan 'make-wepay-api-from-html
                                '("app" "user" "account" "checkout" "preapproval" "withdrawal" "credit_card"
                                  "batch" "subscription" "subscription_plan" "subscription_charge"))
       for name = (second func-def)
     do (format s "~%~%;;; wepay API function ~a~%~%(export '~a)~%" name name)
       (pprint func-def s)))

