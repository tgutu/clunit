(in-package :clunit)

(defclass CLUNIT-TEST-REPORT ()
	((test-name				:initform nil		:initarg :test-name)
	 (passed-p				:initform t		:initarg :passed-p)
	 (suite-list			:initform (list)	:initarg :suite-list)
	 (assertion-conditions	:initform (list))))

;; The CLUNIT-TEST-REPORT instance is used to store the report information for each executed test case.

