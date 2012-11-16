(in-package :clunit)

(defclass CLUNIT-TEST-REPORT ()
	((errors		:initform 0		:initarg :errors)
	 (passes		:initform 0		:initarg :passes)
	 (failures		:initform 0		:initarg :failures)
	 (test-reports	:initform (list))))

;; The CLUNIT-TEST-REPORT instance is used to store the report information for each executed test case.
