(in-package :clunit)

;; The special treatment I have to give to the non-standard formatting behaviour in clisp is really beginning to piss me off.
(defmethod print-object ((assertion-condition assertion-condition) stream)
	(if (member *clunit-report-format* '(:default :tap))
		(print-format assertion-condition *clunit-report-format* stream)
		(call-next-method)))

(defmethod print-object ((clunit-report clunit-report) stream)
	(if (member *clunit-report-format* '(:default :tap))
		(print-format clunit-report *clunit-report-format* stream)
		(call-next-method)))

(defmethod print-object ((clunit-test-report clunit-test-report) stream)
	(if (member *clunit-report-format* '(:default :tap))
		(print-format clunit-test-report *clunit-report-format* stream)
		(call-next-method)))
