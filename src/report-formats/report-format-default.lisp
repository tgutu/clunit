(in-package :clunit)

;;; The special treatment I have to give to the non-standard formatting behaviour in clisp is really beginning to piss me off.
(defmethod print-format ((clunit-report clunit-report) (format (eql :default)) stream)
	(pprint-logical-block (stream nil)
		(with-slots (passed failed errors test-reports) clunit-report
			#-clisp (format stream "~:@_DETAILS:~:@_~4:I=========")
			#+clisp (format stream "~:@_DETAILS:~:@_~8I=========")
			(let (suite)
				(dolist (report test-reports)
					(unless (equal suite (slot-value report 'suite-list))
						(setf suite (slot-value report 'suite-list))
						#-clisp (format stream "~4I~:@_~:@_~8I~S~{~^ -> ~S~}: (Test Suite)" (first suite) (rest suite))
						#+clisp (format stream "~-4I~:@_~:@_~S~{~^ -> ~S~}: (Test Suite)~4I" (first suite) (rest suite)))
						(format stream "~:W" report)))
		
			#-clisp (format stream	"~:@_~I~:@_SUMMARY:~:@_========")
			#+clisp (format stream	"~:@_~-8I~:@_SUMMARY:~:@_========")
			(let ((total (+ passed failed errors)))
				(format stream	"~4I~:@_Executed ~D test function~:P.~:@_Tested ~D assertion~:P.~8I" (length test-reports) total)
				(unless (zerop total)
					(format stream	"~:@_Passed: ~D/~D (~5,1F%)" passed total (* 100 (/ passed total)))
					(format stream	"~:@_Failed: ~D/~D (~5,1F%)" failed total (* 100 (/ failed total)))
					(format stream	"~:@_Errors: ~D/~D (~5,1F%)" errors total (* 100 (/ errors total))))))))


(defmethod print-format ((report clunit-test-report) (format (eql :default)) stream)
	(with-slots (test-name assertion-conditions) report
		(dolist (condition assertion-conditions)
			(unless (typep condition 'assertion-passed)
				#-clisp (format stream "~:@_~S: ~<~:W~:>~:@_" test-name condition)
				#+clisp (format stream "~:@_~S: ~8I~:W~:@_" test-name condition)))))


(defmethod print-format ((condition assertion-error) (format (eql :default)) stream)
	(pprint-logical-block (stream nil)
		(with-slots (message) condition
			(format stream "~A" message))))


(defmethod print-format ((condition assertion-failed) (format (eql :default)) stream)
	(pprint-logical-block (stream nil)
		(with-slots (expression expected returned forms) condition
			(format stream "Expression: ~S~:@_Expected: ~A~:@_Returned: ~A~{~^~:@_~:[~A~;~S => ~S~]~}" expression expected returned forms))))


(defmethod print-format ((condition assertion-fail-forced) (format (eql :default)) stream)
	(pprint-logical-block (stream nil)
		(with-slots (format-string args) condition
			(format stream format-string args))))

