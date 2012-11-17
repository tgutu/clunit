(in-package :clunit)


;; EXECUTE-TEST-CASE Algorithm:
;; 1. Create a CLUNIT-TEST-REPORT instance.
;; 2. Push it on to the the TEST-REPORTS slot of *CLUNIT-REPORT*.
;; 3. Process test case depencies:
;;    a. If a test we depend on failed or was skipped, skip this test.
;;    b. If a test we depend on hasn't run, queue this test.
;;
(defun execute-test-case (test-case)
	(with-slots (test-function name) test-case
		(let ((*clunit-test-report* (make-instance 'clunit-test-report :test-name name :suite-list *suite-name*)))
			(push *clunit-test-report* (slot-value *clunit-report* 'test-reports))
			(report-test-progress name *suite-name*)
			(ecase (test-case-execution-action test-case)
				(:run		(funcall test-function))

				(:skip		(if *report-progress*
								(format *standard-output* "[SKIPPED]"))
							(setf (slot-value *clunit-test-report* 'skipped-p) t))
					
				(:queue		(if *report-progress*
								(format *standard-output* "[QUEUED]"))
							(push *clunit-test-report* *queued-test-reports*))))))



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
					(setf *queued-tests* (list) *last-clunit-report* *clunit-report*)
					(execute-test-case test-case))
				(cancel-unit-test () 
					:report (lambda (s) (format s "Cancel unit test execution."))
					nil)))
		*clunit-report*))

