(in-package :clunit)

;;; The special treatment I have to give to the non-standard formatting behaviour in clisp is really beginning to piss me off.
(defmethod print-format ((clunit-report clunit-report) (format (eql :default)) stream)
	(pprint-logical-block (stream nil)
		(with-slots (passes failures errors assertion-conditions) clunit-report
			#-clisp (format stream "DETAILS:~:@_~4:I=========")
			#+clisp (format stream "DETAILS:~:@_~8I=========")
			(let (suite)
				(dolist (condition assertion-conditions)
					(unless (equal suite (slot-value condition 'suite))
						(setf suite (slot-value condition 'suite))
						#-clisp (format stream "~4I~:@_~:@_~8I~S~{~^ -> ~S~}: (Test Suite)" (first suite) (rest suite))
						#+clisp (format stream "~-4I~:@_~:@_~S~{~^ -> ~S~}: (Test Suite)~4I" (first suite) (rest suite)))
					(unless (eq :pass (slot-value condition 'type))
						(format stream "~:@_~A" condition))))
		
			#-clisp (format stream	"~:@_~I~:@_SUMMARY:~:@_========")
			#+clisp (format stream	"~:@_~-8I~:@_SUMMARY:~:@_========")
			(let ((total (+ passes failures errors)))
				(format stream	"~4I~:@_Tested ~D assertion~:P.~8I" total)
				(unless (zerop total)
					(format stream	"~:@_Passed: ~D/~D (~5,1F%)" passes total (* 100 (/ passes total)))
					(format stream	"~:@_Failed: ~D/~D (~5,1F%)" failures total (* 100 (/ failures total)))
					(format stream	"~:@_Errors: ~D/~D (~5,1F%)" errors total (* 100 (/ errors total))))))))

(defmethod print-format ((assertion-condition assertion-condition) (format (eql :default)) stream)
	(pprint-logical-block (stream nil)
		(with-slots (expression expected message result forms test type) assertion-condition
			(case type
				(:error	(format stream "~S: ~<~A~:>" test message))
				(t		#-clisp (format stream "~S: ~<Expression: ~S~:@_Expected: ~A~:@_Returned: ~A~{~^~:@_~:[~A~;~S => ~S~]~}~:>" test (list expression expected result forms))
						#+clisp (format stream "~S: ~8IExpression ~S~:@_Expected ~A~:@_Returned: ~A~{~^~:@_~:[~A~;~S => ~S~]~}" test expression expected result forms))))))


