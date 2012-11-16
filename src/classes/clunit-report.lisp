(in-package :clunit)

(defclass CLUNIT-REPORT ()
	((test-name				:initform nil		:initarg :test-name)
	 (passed-p				:initform nil		:initarg :passed-p)
	 (suite-list			:initform (list)	:initarg :suite-list)
	 (assertion-conditions	:initform (list))))

;; The CLUNIT-REPORT instance is used to store the aggregated reports of all executed test cases.