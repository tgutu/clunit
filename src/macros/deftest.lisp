(in-package :clunit)

;; The DEFTEST macro has three possible forms:
;;
;; (deftest name () . body)
;;
;; (deftest name (suite1 suite2 ... suiteN) . body)
;;
;; (deftest name ((suite1 suite2 ... suiteN) (test1 test2 ... testN)) . body)
;;
;; The first form defines a test case which is not associated with any test suite or has any dependencies.
;; The second form defines a test case which is associated with test suites: suite1 ... suiteN
;; The third form defines a test case associated with zero or more test suites and depends on zero or more tests.
;;
;; DEFTEST Algorithm:
;; 1. Establish the DEFTEST form used.
;; 2. If test is associated with 1 or more test suites make sure all test suites are defined.
;; 3. If test depends on 1 or more test cases, emit a warning for any test case not defined.
;;    Remember if test does not exist at runtime the reference is removed. Whereas test suites
;;    have to be defined before we can refer to them.
;; 4. Add reference in each the test suite's TEST-CASES slot.
;; 5. Create a named lambda test function.
;; 6. Create new test suite instance and add it to lookup table.
;;
(defmacro deftest (name declarations &body body)
	"Defines a test case called NAME. DECLARATIONS identifies the test suites this test case is associated with as well as any other test cases that it depends on.
The test case body is revaluated on each run, so any redefinition of macros and inline functions will be automatically visible without having to redefine the test."
	(with-gensyms (parent-suites test-dependencies test-function)
		`(let ((,parent-suites ',declarations) ,test-dependencies ,test-function)

			(unless (listp ,parent-suites)
				(error "In (deftest name declarations . body), DECLARATION should be a list not ~S." ,parent-suites))

			;; Check if declaration is of the form ((suite1 ... suiteN) (test1 ... testN))
			(when (listp (first ,parent-suites))
				;; Set PARENT-SUITES and TEST-DEPENDENCIES to their respective values in the  ((suite1 ... suiteN) (test1 ... testN))  declaration form.
				(setf ,test-dependencies (second ,parent-suites))
				(setf ,parent-suites (first ,parent-suites)))

			(unless (every #'symbolp ,parent-suites)
				(error "In ~S, every name should be a symbol." ,parent-suites))

			(unless (every #'symbolp ,test-dependencies)
				(error "In ~S, every name should be a symbol." ,test-dependencies))

			;; Emit warnings for all dependencies on test cases that have not yet been defined.
			(loop
				:for test :in ,test-dependencies
				:do (unless (get-test-case test)
						(warn "Defining test case ~S which has a dependency on undefined test case ~S." ',name test)))

			;; Make sure that all the parents are test suites.
			(loop
				:for parent :in ,parent-suites
				:do (unless (get-test-suite parent)
						(error "Trying to add test case ~S reference to test suite, but test suite ~S is not defined." ',name parent)))

			;; Add test case reference to each of its parent's TEST-CASES slot.
			(loop
				:for parent :in ,parent-suites
				:do	(pushnew ',name (slot-value (get-test-suite parent) 'test-cases)))


			(setf ,test-function
					(lambda ()
						(block ,name
							(with-test-restart
								(let ((*test-name* ',name) (body '(progn ,@body)))
									(report-test-progress ',name *suite-name*)
									(when *suite-name*																	; If test was not called by any test suite, then do not attempt to expand out any fixtures.
										(dolist (suite (reverse *suite-name*) body)										; However, if the test is being executed in a context with one or more test suites,
											(setf body (expand-fixture suite body))))									; expand out the fixtures starting with the most specific
									(eval body))))))
			
			;; Create new test case instance and add it to lookup table.
			(setf (get-test-case ',name) (make-instance 'clunit-test-case :name ',name :dependencies ,test-dependencies :test-function ,test-function)))))


;; UNDEFTEST Algorithm:
;; 1. Check if test case is defined, if its not throw an error.
;; 2. Undefine test
;;
;; N.B. Remember we do not bother tracing the suites that contain a reference to this test case because they will update their references when they realise that it no longer exists.
;;
(defmacro undeftest (name)
	`(progn
		(unless (symbolp ',name)
				(error "In (undeftest name), NAME should be a symbol not ~S." ',name))

		(unless (get-test-case ',name)
			(error "In (undeftest name), test case ~S is not defined. " ',name))

		(delete-test-case ',name)))

