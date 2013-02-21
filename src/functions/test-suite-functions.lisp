(in-package :clunit)


(defun run-suite (suite &key use-debugger (report-progress t) stop-on-fail)
	"Executes a test case called SUITE. If REPORT-PROGRESS is non-NIL, the test progress is reported. If USE-DEBUGGER is non-NIL, the debugger is invoked whenever an assertion fails.
If STOP-ON-FAIL is non-NIL, the rest of the unit test is cancelled when any assertion fails or an error occurs."
	(let ((*clunit-report* (make-instance 'clunit-report))
		  (*report-progress* report-progress)
		  (test-suite (get-test-suite suite))
		  (*use-debugger* use-debugger)
		  (*stop-on-fail* stop-on-fail))

		(unless test-suite
			(error "Test suite ~S is not defined." suite))

		(handler-bind ((error #'handle-error)
						(warning #'muffle-warning)
						(assertion-condition #'handle-assertion))
			(restart-case
				(progn
					(if *report-progress*
						(format *standard-output* "~%PROGRESS:~%========="))
					(setf *queued-test-reports* (list) *last-clunit-report* *clunit-report*)
					(execute-test-suite test-suite)
					(when *queued-test-reports*
						(if *report-progress*
							(format *standard-output* "~%~%QUEUED TESTS:~%============="))
						(process-queued-tests)))
				(cancel-unit-test () 
					:report (lambda (s) (format s "Cancel unit test execution."))
					nil)))
		*clunit-report*))


(defun execute-test-suite (test-suite)
	(with-slots (name test-cases child-suites) test-suite
		(report-suite-progress name *suite-name*)
		(restart-case
			(let (obj (*suite-name* (append *suite-name* (list name))))				; Extend the test suite call-chain list by appending the name of the test suite being called.
				
				;; Execute test cases.
				(dolist (test test-cases)
					(setf obj (get-test-case test))
					;; Check if reference is now stale, if it is delete it. References become stale when a test case or test suite was undef'ed.
					(if obj
						(execute-test-case obj)
						(setf test-cases (delete test test-cases))))

				;; Execute child suites.
				(dolist (suite child-suites)
					(setf obj (get-test-suite suite))
					(if obj
						(execute-test-suite obj)
						(setf child-suites (delete suite child-suites)))))

			(skip-suite ()
				:report (lambda (s) (format s "Skip test suite ~S." name))
				nil))))


(defun get-defined-suites ()
	"Returns a list of all defined test suite names."
	(loop
		:for key :being :the :hash-key :of *test-suite-hashtable*
		:collect key))


(defun get-test-suite (name)
	"Retrieves the TEST-SUITE instance associated with the key NAME in the hash table *TEST-SUITES*"
	(gethash name *test-suite-hashtable*))


(defun (setf get-test-suite) (new-test-suite name)
	"Adds NEW-TEST-SUITE in the hash table *TEST-SUITES* under the key NAME."
	(setf (gethash name *test-suite-hashtable*) new-test-suite))


(defun defined-suite-p (suite-name)
	"Returns T if a test suite called SUITE-NAME is defined, otherwise returns NIL."
	(if (get-test-suite suite-name) t nil))


(defun delete-test-suite (name)
	"Deletes the TEST-SUITE instance associated with the key NAME in the hash table *TEST-SUITES*"
	(remhash name *test-suite-hashtable*))


(defun get-child-tests (suite-name)
	"Returns a list of all test case names that are children of the suite called SUITE-NAME."
	(let ((suite (get-test-suite suite-name)))
		(if suite
			(slot-value suite 'test-cases))))


(defun get-child-suites (suite-name)
	"Returns a list of all test suite names that are children of the suite called SUITE-NAME."
	(let ((suite (get-test-suite suite-name)))
		(if suite
			(slot-value suite 'child-suites))))

