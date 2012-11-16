(in-package :clunit)

;; This function is used in DEFSUITE and DEFTEST to avoid circularity when defining new classes.
(defun remove-superclasses (class-list)																																	
	"REMOVE-SUPERCLASSES removes any class in CLASS-LIST that is a super class of another class in the list."
	(flet ((superclass-p (superclass)
				(dolist (class class-list)
					(cond
						;; If CLASS and SUPERCLASS are the same symbol do nothing.
						((eq class superclass)		nil)								
						
						;; If CLASS is a subclass of SUPERCLASS return T.
						((typep (make-instance class) superclass)
										(return-from superclass-p t))))))
		(remove-if #'superclass-p class-list)))


#|
	The method EXPAND-FIXTURE now specializes on the test suite name instead of the class.
	Consider the hierarchy shown below

	(defsuite NumberSuite ())

	(defsuite FloatSuite (NumberSuite))

	(defsuite IntegerSuite (NumberSuite))

	(defsuite BooleanSuite (FloatSuite IntegerSuite))

	(deffixture IntegerSuite (@body)
		(let ((x 0) (y 1) (z 2))
			@body))

	(deffixture FloatSuite (@body)
	    (let ((x 0.0) (y 1.0) (z 2.0))
			@body))


	(deftest test-bool1 (BooleanSuite)
		(assert-true  (< x y z))
		(assert-true  (= x y z) x y z))

	Because BooleanSuite does not have a fixture but is a subclass of both FloatSuite and IntegerSuite
	When TESTBOOL1 is called inside the IntegerSuite the call
	(expand-fixture (make-instance 'BooleanSuite) body) actually end up calling the fixture expander
	for the FloatSuite because FloatSuite is more specific than Integer suite!
	So we will stick to only using the suite name and that will solve the problem.
|#
(defmacro deffixture (suite (plug) &body body)
	"A fixture defines a code template that is wrapped around the code of each test case and test suite that are executed by test suite SUITE at runtime.. The test case body is plugged into the template at the position identified by PLUG.
Fixtures are expanded at runtime, so the fixture that will wrap around a test depends on the test suite call stack."
	`(handler-bind ((warning #'muffle-warning))
		;; Test that fixture is being defined for a SUITE subclass.
		(unless (typep (make-instance ',suite) 'suite)
				(error "~A is not a test suite." ',suite))
		(defmethod expand-fixture ((suite (eql ',suite)) body)
			(subst body ',plug '(progn ,@body)))))


(defmacro deftest (test-name parent-suites &body body)
	"Defines a test case called TEST-NAME. If PARENT-SUITES is non-NIL, a reference to the test is added to each test suite, otherwise its a suiteless test.
The test case body is revaluated on each run, so any redefinition of macros and inline functions will be automatically visible without having to redefine the test."
	`(handler-bind ((warning #'muffle-warning))
		,@(loop
			:for suite
			:in (remove-duplicates parent-suites)
			:collect `(unless (typep (make-instance ',suite) 'suite)										; Test that each parent is a subclass of SUITE and signal an error if its not.
						(error "~A is not a test suite." ',suite))
			:collect `(defmethod get-suite-tests ((suite (eql ',suite)))									; Add the test we are defining to the child list of each of its parent suites
						',(remove-duplicates (list* test-name (get-suite-tests suite)))))					; by redefining the value returned by the parents' GET-SUITE-TESTS method.
			
		(defmethod get-test-parents ((suite (eql ',test-name))) ',(remove-duplicates parent-suites))

		(defmethod execute-test ((test (eql ',test-name)))													; Define a specialized EXECUTE-TEST method for the new test.
			(with-test-restart
				(handler-bind ((warning #'muffle-warning))													
					(let ((*test-name* test) (body '(progn ,@body)))
						(report-test-progress ',test-name *suite-name*)
						(when *suite-name*																	; If test was not called by any test suite, then do not attempt to expand out any fixtures.
							(dolist (suite (reverse *suite-name*) body)										; However, if the test is being executed in a context with one or more test suites,
								(setf body (expand-fixture suite body))))									; expand out the fixtures starting with the most specific
						(eval body)))))
			',test-name))


(defmacro undeftest (name)
	`(handler-bind ((warning #'muffle-warning))
		,@(loop
			:for suite
			:in (get-test-parents name)
			:collect `(defmethod get-suite-tests ((suite (eql ',suite)))											; We do a similar thing as above for a test. We redefine the test's parents
						',(remove name (get-suite-tests suite))))													; GET-SUITE-TESTS methods.
		(remove-method #'execute-test (find-method #'execute-test '() '((eql ,name))))))




