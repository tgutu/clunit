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
