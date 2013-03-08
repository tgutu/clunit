(in-package :clunit)

(defclass CLUNIT-TEST-REPORT ()
	((test-name				:initform nil		:initarg :test-name	:reader	test-report-name)	; The reader function is used in TEST-CASE-EXECUTION-ACTION (see utility functions).
	 (passed-p				:initform t			:initarg :passed-p	:reader	test-report-passed-p)
	 (skipped-p				:initform nil		:initarg :skipped-p)
	 (suite-list			:initform (list)	:initarg :suite-list)
	 (assertion-conditions	:initform (list))))

;; The CLUNIT-TEST-REPORT instance is used to store the report information for each executed test case.

