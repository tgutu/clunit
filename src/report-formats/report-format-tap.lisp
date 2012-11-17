(in-package :clunit)

;; TEST ANYTHING PROTOCOL (TAP)
;; The special treatment I have to give to the non-standard formatting behaviour in clisp is really beginning to piss me off.
(defmethod print-format ((clunit-report clunit-report) (format (eql :tap)) stream)
	(pprint-logical-block (stream nil)
		(with-slots (passes failures errors assertion-conditions) clunit-report
			(let ((total (+ passes failures errors)) (test-num 0))
				(unless (zerop total)
					(format stream "TAP version 13~:@_1..~A" total)
					(dolist (condition assertion-conditions)
						(with-slots (type) condition
							(format stream "~:@_~:[not ok~;ok~] ~A ~:[~;# SKIP Error occured.~]" (eq :pass type) (incf test-num) (eq :error type))
							(unless (eq :pass type)
								#-clisp (format stream "~4I~:@_~A~0I" condition)
								#+clisp (format stream "~4I~:@_~A~-4I" condition)))))))))

(defmethod print-format ((assertion-condition assertion-condition) (format (eql :tap)) stream)
	(pprint-logical-block (stream nil :per-line-prefix "#  ")
		(with-slots (expression expected message result forms test suite type) assertion-condition
			#-clisp (format stream "Suite: ~S~{~^ -> ~S~}" (first suite) (rest suite))
			(case type
				(:error	(format stream "~:@_Test: ~S:~:@_~A" test message))
				(t		(format stream "~:@_Test: ~S~:@_Expression: ~S~:@_Expected: ~A~:@_Returned: ~A~{~^~:@_~:[~A~;~S => ~S~]~}" test expression expected result forms))))))
