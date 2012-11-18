(in-package :clunit)

;; TEST ANYTHING PROTOCOL (TAP)
(defvar *tap-test-num* 0 "Counter used in these printing functions")

(defmethod print-format ((clunit-report clunit-report) (format (eql :TAP)) stream)
	(pprint-logical-block (stream nil)
		(with-slots  (passed failed errors test-reports) clunit-report
			(let ((total (+ passed failed errors)) (*tap-test-num* 0))
				(unless (zerop total)
					(format stream "TAP version 13~:@_1..~A" total)
					(dolist (report test-reports)
						(format stream "~A" report)))))))


;; README!!!! 
;; The pretty printing in CLISP is really broken so we just try has hard as can to compensate.
(defmethod print-format ((report clunit-test-report) (format (eql :TAP)) stream)
	(with-slots (test-name assertion-conditions suite-list) report
		(dolist (condition assertion-conditions)
			(typecase condition
				(assertion-passed		#-clisp (format stream "~0I~:@_ok ~A" (incf *tap-test-num*))
										#+clisp (format stream "~-4I~:@_ok ~A" (incf *tap-test-num*)))

				(t						#-clisp (format stream "~0I~:@_not ok ~A~4I~:@_" (incf *tap-test-num*))
										#+clisp (format stream "~-4I~:@_not ok ~A~4I~:@_" (incf *tap-test-num*))
										(pprint-logical-block (stream nil :per-line-prefix "#  ")
											(format stream "Suite: ~S~{~^ -> ~S~}" (first suite-list) (rest suite-list))
											(format stream "~:@_Test: ~S~:@_~:W~:@_" test-name condition)))))))


(defmethod print-format ((condition assertion-error) (format (eql :TAP)) stream)
	(pprint-logical-block (stream nil)
		(with-slots (message) condition
			(format stream "~A" message))))


(defmethod print-format ((condition assertion-failed) (format (eql :TAP)) stream)
	(pprint-logical-block (stream nil)
		(with-slots (expression expected returned forms) condition
			(format stream "Expression: ~S~:@_Expected: ~A~:@_Returned: ~A~{~^~:@_~:[~A~;~S => ~S~]~}" expression expected returned forms))))


(defmethod print-format ((condition assertion-fail-forced) (format (eql :TAP)) stream)
	(pprint-logical-block (stream nil)
		(with-slots (format-string args) condition
			(format stream "~?" format-string args))))
