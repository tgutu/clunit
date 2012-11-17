(in-package :clunit)

;; README!!!! 
;; The pretty printing in CLISP is really broken so we just try has hard as can to compensate.
(defmethod print-format ((clunit-report clunit-report) (format (eql :default)) stream)
	(pprint-logical-block (stream nil)
		(with-slots (passed failed errors skipped test-reports) clunit-report
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
				#-clisp (format stream	"~4I~:@_Test functions:~8I~:@_Executed: ~D~:@_Skipped:  ~D~4I~:@_~:@_Tested ~D assertion~:P.~8I"		(- (length test-reports) skipped) skipped total)
				#+clisp (format stream	"~4I~:@_Test functions:~8I~:@_Executed: ~D~:@_Skipped:  ~D~-8I~:@_~:@_Tested ~D assertion~:P.~8I"	(- (length test-reports) skipped) skipped total)
				(unless (zerop total)
					(format stream	"~:@_Passed: ~D/~D (~5,1F%)" passed total (* 100 (/ passed total)))
					(format stream	"~:@_Failed: ~D/~D (~5,1F%)" failed total (* 100 (/ failed total)))
					(format stream	"~:@_Errors: ~D/~D (~5,1F%)" errors total (* 100 (/ errors total))))))))


(defmethod print-format ((report clunit-test-report) (format (eql :default)) stream)
	(with-slots (test-name assertion-conditions) report
		(dolist (condition assertion-conditions)
			(unless (typep condition 'assertion-passed)
				(format stream "~:@_~S: ~<~:W~:>~:@_" test-name condition)))))


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

