;;;;	Copyright (c) 20012 Tapiwa Gutu
;;;;	
;;;;	Permission is hereby granted, free of charge, to any person obtaining 
;;;;	a copy of this software and associated documentation files (the "Software"), 
;;;;	to deal in the Software without restriction, including without limitation 
;;;;	the rights to use, copy, modify, merge, publish, distribute, sublicense, 
;;;;	and/or sell copies of the Software, and to permit persons to whom the 
;;;;	Software is furnished to do so, subject to the following conditions:
;;;;	
;;;;	The above copyright notice and this permission notice shall be included 
;;;;	in all copies or substantial portions of the Software.
;;;;	
;;;;	THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS 
;;;;	OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
;;;;	FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL 
;;;;	THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR 
;;;;	OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, 
;;;;	ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR 
;;;;	OTHER DEALINGS IN THE SOFTWARE.

(in-package :clunit)

(defun report-suite-progress (suite suite-list)
	(if *report-progress*
		(format *standard-output* "~%~%~VT~S: (Test Suite)" (* *tab-width* (1+ (length suite-list))) suite)))

(defun report-test-progress (test-name suite-list)
	(if *report-progress*
		(format *standard-output* "~%~VT~S: " (* *tab-width* (1+ (length suite-list))) test-name)))

(defun report-assertion-progress (type)
	(if *report-progress*
		(case type
			(:error	(princ #\E *standard-output*))
			(:fail	(princ #\F *standard-output*))
			(:pass	(princ #\. *standard-output*)))))

;; The special treatment I have to give to the non-standard formatting behaviour in clisp is really beginning to piss me off.
(defmethod print-object ((assertion-condition assertion-condition) stream)
	(if (member *clunit-report-format* '(:default :tap))
		(print-format assertion-condition *clunit-report-format* stream)
		(call-next-method)))

(defmethod print-object ((clunit-report clunit-report) stream)
	(if (member *clunit-report-format* '(:default :tap))
		(print-format clunit-report *clunit-report-format* stream)
		(call-next-method)))


;; DEFAULT reporting format.
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


;; TEST ANYTHING PROTOCOL (TAP)
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
