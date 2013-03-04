(in-package :clunit)

(defun report-suite-progress (suite suite-list)
	(if *report-progress*
		(format *standard-output* "~%~%~VT~A: (Test Suite)" (* *tab-width* (1+ (length suite-list))) suite)))

(defun report-test-progress (test-name suite-list)
	(if *report-progress*
		(format *standard-output* "~%~VT~A: " (* *tab-width* (1+ (length suite-list))) test-name)))

(defun report-assertion-progress (type)
	(if *report-progress*
		(case type
			(:error	(princ #\E *standard-output*))
			(:fail	(princ #\F *standard-output*))
			(:pass	(princ #\. *standard-output*)))))