(in-package :clunit)


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

		(handler-bind ((error #'handle-error)
						(warning #'muffle-warning)
						(assertion-condition #'handle-assertion))
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

				(:skip		(skip-test-case))

				(:queue		(queue-test-case)
							(push *clunit-test-report* *queued-test-reports*))))))


(defun get-test-case (name)
	"Retrieves the TEST-CASE instance associated with the key NAME in the hash table *TEST-CASES*"
	(gethash name *test-case-hashtable*))


(defun (setf get-test-case) (new-test-case name)
	"Adds NEW-TEST-CASE in the hash table *TEST-CASES* under the key NAME."
	(setf (gethash name *test-case-hashtable*) new-test-case))


(defun delete-test-case (name)
	"Deletes the TEST-CASE instance associated with the key NAME in the hash table *TEST-CASES*"
	(remhash name *test-case-hashtable*))


(defun defined-test-p (test-name)
	"Returns T if a test case called TEST-NAME is defined, otherwise returns NIL."
	(if (get-test-case test-name) t nil))


(defun get-defined-tests ()
	"Returns a list of all defined test case names."
	(loop
		:for key :being :the :hash-key :of *test-case-hashtable*
		:collect key))


(defun queue-test-case ()
	(if *report-progress*
		(format *standard-output* "[QUEUED]")))


(defun skip-test-case ()
	(if *report-progress*
		(format *standard-output* "[SKIPPED]"))
	(incf (slot-value *clunit-report* 'skipped))
	(setf (slot-value *clunit-test-report* 'skipped-p) t))


(defun test-case-execution-action (test-case)
	"Determines the execution action for TEST-CASE. If test case has no dependencies or pending dependencies, then :RUN is returned.
If test case has another test case it depends on that failed or was skipped, then :SKIP is returned.
If test case depends on test cases that have not yet run or are also queued, then :QUEUE is returned."
	(unless (slot-value test-case 'dependencies)
		;; If test case has no dependencies return :RUN
		(return-from test-case-execution-action :run))

	(with-slots (dependencies) test-case
		(tagbody
				(dolist (test dependencies)
					(if (get-test-case test) ; Check if reference is stale.

						(let ((report (find test (slot-value *clunit-report* 'test-reports) :test #'eq :key #'clunit-test-report-name)))
							(cond
								((not report)								; If test report was not found it means test has not yet been run.
																			(go :queue))

								((member report *queued-test-reports*)		; Test we depend on is also queued.
																			(go :queue))
								((or (slot-value report 'skipped-p)
									 (not (slot-value report 'passed-p)))
																			(go :skip))))

						;; Delete stale reference. References become stale when a test case is undef'ed.
						(setf dependencies (delete test dependencies))))

			:run
				(return-from test-case-execution-action :run)

			:skip
				(return-from test-case-execution-action :skip)

			:queue
				(return-from test-case-execution-action :queue))))


(defun process-queued-tests ()
	;; In the worst case scenario we have a queued list of tests which have a linear dependency.
	;; I this scenario, a queued test depends on the test queued after it, so we have to re-process
	;; the queue at least N times, were N is the length of the queue.
	(let (processed-reports)
		(flet 
			((execute-test-report (test-report)
				(with-slots (test-name suite-list) test-report
					(let ((*clunit-test-report* test-report) (test-case (get-test-case test-name)))
						(case (test-case-execution-action test-case)
							(:run
									(report-test-progress test-name *suite-name*)
									(push test-report processed-reports)
									(let ((*suite-name* suite-list))
										(funcall (slot-value test-case 'test-function))))
							(:skip
									(report-test-progress test-name *suite-name*)
									(push test-report processed-reports)
									(skip-test-case)))))))
			(loop
				:repeat (length *queued-test-reports*)
				:do
					(unless *queued-test-reports*
						;; If all the tests we processed in a shorter run.
						(return-from process-queued-tests))

					(setf processed-reports (list))
					(mapc #'execute-test-report *queued-test-reports*)
					(setf *queued-test-reports* (nset-difference *queued-test-reports* processed-reports :test #'eq))))))

