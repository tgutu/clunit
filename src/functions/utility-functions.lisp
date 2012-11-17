(in-package :clunit)


;; TEST-CASE Utility functions
(defun get-test-case (name)
	"Retrieves the TEST-CASE instance associated with the key NAME in the hash table *TEST-CASES*"
	(gethash name *test-case-hashtable*))

(defun (setf get-test-case) (new-test-case name)
	"Adds NEW-TEST-CASE in the hash table *TEST-CASES* under the key NAME."
	(setf (gethash name *test-case-hashtable*) new-test-case))

(defun delete-test-case (name)
	"Deletes the TEST-CASE instance associated with the key NAME in the hash table *TEST-CASES*"
	(remhash name *test-case-hashtable*))

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
									(if *report-progress*
										(format *standard-output* "[SKIPPED]"))
									(push test-report processed-reports)
									(setf (slot-value *clunit-test-report* 'skipped-p) t)))))))
			(loop
				:repeat (length *queued-test-reports*)
				:do
					(unless *queued-test-reports*
						;; If all the tests we processed in a shorter run.
						(return-from process-queued-tests))

					(setf processed-reports (list))
					(mapc #'execute-test-report *queued-test-reports*)
					(setf *queued-test-reports* (nset-difference *queued-test-reports* processed-reports :test #'eq))))))



;; TEST-SUITE Utility functions.
(defun get-test-suite (name)
	"Retrieves the TEST-SUITE instance associated with the key NAME in the hash table *TEST-SUITES*"
	(gethash name *test-suite-hashtable*))

(defun (setf get-test-suite) (new-test-suite name)
	"Adds NEW-TEST-SUITE in the hash table *TEST-SUITES* under the key NAME."
	(setf (gethash name *test-suite-hashtable*) new-test-suite))

(defun delete-test-suite (name)
	"Deletes the TEST-SUITE instance associated with the key NAME in the hash table *TEST-SUITES*"
	(remhash name *test-suite-hashtable*))



;; SIGNAL-ASSERTION
(defun signal-assertion (type &key expression expected result forms)
	(error 'assertion-condition :type type :suite *suite-name* :test *test-name* :expression expression :expected expected :result result :forms forms))



;; HANDLE-CONDITION
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
