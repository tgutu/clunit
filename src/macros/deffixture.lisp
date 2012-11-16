(in-package :clunit)

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
		(unless (get-test-suite ',suite)
				(error "~A is not a test suite." ',suite))
		(defmethod expand-fixture ((suite (eql ',suite)) body)
			(subst body ',plug '(progn ,@body)))))

