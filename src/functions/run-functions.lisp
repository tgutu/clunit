(in-package :clunit)

(defmethod execute-suite ((suite symbol))
	(restart-case
		(let ((*suite-name* (append *suite-name* (list suite))))														; Extend the test suite call-chain list by appending the name of the test suite being called.
			(report-suite-progress suite (rest *suite-name*))
			(mapc #'execute-test (get-suite-tests suite))																; Execute all the suite tests.
			(dolist (suite (get-suite-childsuites suite))																; Execute child suites.
				(execute-suite suite)))
		(skip-suite ()
			:report (lambda (s) (format s "Skip test suite ~S." suite))
			nil)))


(defun signal-assertion (type &key expression expected result forms)
	(error 'assertion-condition :type type :suite *suite-name* :test *test-name* :expression expression :expected expected :result result :forms forms))


(defun handle-condition (condition)
	"Records the result of assertion tests and records any errors that occur."
	(unless (typep condition 'assertion-condition)	
		;; If CONDITION is not of type ASSERTION-CONDITION then an error occured.
		;; We record the error as an ASSERTION-CONDITION with the TYPE slot set to :ERROR.
		(setf condition
			  (make-condition 'assertion-condition :type :error :message (princ-to-string condition) :test *test-name* :suite *suite-name*)))

	(let ((restart (or (find-restart 'skip-assertion) (find-restart 'skip-test))))
		(with-slots (passes failures errors assertion-conditions) *clunit-report*
			(with-slots (type) condition
				(setf assertion-conditions (nconc assertion-conditions (list condition)))
				(report-assertion-progress type)
				(case type
					(:fail	(incf failures))

					(:error	(incf errors))

					(:pass	(incf passes)
							(invoke-restart restart))))	; We do not invoke the debugger for successful assertions.

			(if *stop-on-fail*
				(invoke-restart 'cancel-unit-test))

			(if (and restart (not *use-debugger*))
				(invoke-restart restart)))))


(defun run-suite (suite &key use-debugger (report-progress t) stop-on-fail)
	"Executes a test suite called SUITE. REPORT-PROGRESS switches on progress reporting if set to true. The debugger is invoked whenever an assertion fails if USE-DEBUGGER is set to true."
	(let ((*clunit-report* (make-instance 'clunit-report)) (*use-debugger* use-debugger) (*report-progress* report-progress) (*stop-on-fail* stop-on-fail))
		(handler-bind ((error #'handle-condition)(warning #'muffle-warning))
			(restart-case
				(progn
					(if *report-progress*
						(format *standard-output* "~%PROGRESS:~%========="))
					(execute-suite suite))
				(cancel-unit-test () 
					:report (lambda (s) (format s "Cancel unit test execution."))
					nil)))
		*clunit-report*))

