(in-package :clunit)


;; This function is called inside the EXECUTE-TEST method at runtime.
(defgeneric expand-fixture (suite body)
	(:documentation "Expands out a fixture body for the given test suite."))


;; By specializing methods of this generic function on different FORMAT values, the reporting formats
;; of the unit test results can be extended.
(defgeneric print-format (object format stream)
	(:documentation "Outputs the OBJECT report to STREAM in the given FORMAT. FORMAT can be :default, :tap or NIL."))


;;	Define the default method definitions which either just return nil or their default values.
;;	We will specialize these methods for different tests and suites.
(defmethod expand-fixture ((suite t) body) body)
