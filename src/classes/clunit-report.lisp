(in-package :clunit)

(defclass CLUNIT-REPORT ()
	((errors		:initform 0			:initarg :errors)
	 (failed		:initform 0			:initarg :failed)
	 (passed		:initform 0			:initarg :passed)
	 (skipped		:initform 0			:initarg :skipped)
	 (test-reports	:initform (list)	:reader test-reports)))

;; The CLUNIT-REPORT instance is used to store the aggregated reports of all executed test cases.

