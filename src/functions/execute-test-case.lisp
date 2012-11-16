(in-package :clunit)


;; EXECUTE-TEST-CASE Algorithm:
;; 1. Create a CLUNIT-TEST-REPORT instance.
;; 2. Push it on the the TEST-REPORTS slot of *CLUNIT-REPORT*.
;; 3. If we have depencies check if *CLUNIT-REPORT* has a CLUNIT-TEST-REPORT for a test we depend on.
;;    a. If there is and the test failed, skip this test.
;;    b. If there isn't queue this test.
;; 4. After executing a test, check if any queued test cases can now be run or should be removed from the queue.
;;
(defun execute-test-case (test-case)
	(with-slots (test-function dependencies name) test-case
		(let ((*clunit-test-report* (make-instance 'clunit-test-report :test-name name :suite-list *suite-name*)))
			(push *clunit-test-report* (slot-value *clunit-report* 'test-reports))	
			;; Check for dependencies
			(funcall test-function)
			;; Check if any queued functions have had their dependency requirement met by this function having run.
			)))


(defun run-test (test &key use-debugger (report-progress t) stop-on-fail)
	"Executes a test case called TEST. If REPORT-PROGRESS is non-NIL, the test progress is reported. If USE-DEBUGGER is non-NIL, the debugger is invoked whenever an assertion fails.
If STOP-ON-FAIL is non-NIL, the rest of the unit test is cancelled when any assertion fails or an error occurs."
	(let ((*clunit-report* (make-instance 'clunit-report))
		  (*report-progress* report-progress)
		  (test-case (get-test-case test))
		  (*use-debugger* use-debugger)
		  (*stop-on-fail* stop-on-fail))

		(unless test-case
			(error "Test case ~S is not defined." test))

		(handler-bind ((error #'handle-condition)
						(warning #'muffle-warning))
			(restart-case
				(progn
					(if *report-progress*
						(format *standard-output* "~%PROGRESS:~%========="))
					(execute-test-case test-case))
				(cancel-unit-test () 
					:report (lambda (s) (format s "Cancel unit test execution."))
					nil)))
		*clunit-report*))

