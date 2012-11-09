(in-package :clunit)

(defclass suite () ())

(defclass clunit-report ()
	((errors :initform 0)
	 (passes :initform 0)
	 (failures :initform 0 )
	 (assertion-conditions :initform (list))))

(define-condition assertion-condition (error)
	((expression :initarg :expression :initform nil :documentation "The expression that was tested.")
	 (forms :initarg :forms :initform nil :documentation "Holds a list of FORM-VALUE pairs, see function EXPAND-FORMS for more detail.")
	 (expected :initarg :expected :initform nil :documentation "The value the expression was expected to return.")
	 (result :initarg :result :initform nil :documentation "The result that was returned from evaluating the expression.")
	 (suite :initarg :suite :initform nil :documentation "A list of test suite names in calling order.")
	 (test :initarg :test :initform nil :documentation "The name of the test in which the condition was signalled.")
	 (type :initarg :type :initform nil :documentation "Condition type keyword, one of :fail, :error or :pass. Unexpected conditions(errors) have a type of :error. All other assertions either pass or fail.")
	 (message :initarg :message :initform nil :documentation "This is a special case, if an unexpected condition is signalled outside an assertion test, store the condition description here.")))
