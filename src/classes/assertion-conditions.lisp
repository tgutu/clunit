(in-package :clunit)

(define-condition ASSERTION-CONDITION () ())


(define-condition ASSERTION-PASSED (ASSERTION-CONDITION) ())


(define-condition ASSERTION-ERROR (ASSERTION-CONDITION)
	((message :initarg :message :initform nil :documentation "This is a special case, if an unexpected condition is signalled outside an assertion test, store the condition description here.")))


(define-condition ASSERTION-FAILED (ASSERTION-CONDITION)
	((expression	:initarg :expression	:initform nil	:documentation "The expression that was tested.")
	 (forms			:initarg :forms			:initform nil	:documentation "Holds a list of FORM-VALUE pairs, see function EXPAND-FORMS for more detail.")
	 (expected		:initarg :expected		:initform nil	:documentation "The value the expression was expected to return.")
	 (returned		:initarg :returned		:initform nil	:documentation "The result that was returned from evaluating the expression.")))


(define-condition ASSERTION-FAIL-FORCED (ASSERTION-CONDITION)
	((format-string	:initarg :format-string		:initform ""	:documentation "The format string argument passed to FORMAT.")
	 (args			:initarg :args				:initform ""	:documentation "The format string arguments.")))
