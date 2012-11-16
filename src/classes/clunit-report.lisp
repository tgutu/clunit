(in-package :clunit)

(defclass CLUNIT-REPORT ()
	((errors		:initform 0		:initarg :errors)
	 (passes		:initform 0		:initarg :passes)
	 (failures		:initform 0		:initarg :failures)
	 (test-reports	:initform (list))))

;; The CLUNIT-REPORT instance is used to store the aggregated reports of all executed test cases.

