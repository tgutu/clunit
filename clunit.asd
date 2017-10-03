(in-package :cl-user)

(asdf:defsystem :clunit
	:version "0.2.3"
	:author "Tapiwa Gutu <tgutu@ml.sun.ac.za>"
	:licence "BSD"
	:description "CLUnit is a Common Lisp unit testing framework."
	:long-description 
"CLUnit is a Common Lisp unit testing framework. It is designed to be easy to use so that you can quickly start testing.
CLUnit provides a rich set of features aimed at improving your unit testing experience:

*	Multiple inheritance for test suites allows you to group tests into hierarchies.
*	Composes the test results of each test run into a single report.
*	Allows redefinition of inline functions and macros without having to redefine your tests.
*	Supports composable test suite fixtures.
*	Allows for an interactive testing process which gives you access to the test environment.
*	Provides visual feedback of the unit test progress.
*	Extensible test reporting. Builtin support for default reporting and TAP output."

	:components 
		((:module "src"
		  :serial t
		  :components  ((:file "package")
						(:file "specials")
						 					
						(:module "classes" 
							:components
								((:file "assertion-conditions")
								 (:file "clunit-report")
								 (:file "clunit-test-case")
								 (:file "clunit-test-suite")
								 (:file "clunit-test-report")))
						 								 
						 (:module "functions" 
							:components
								((:file "assertion-functions")
								 (:file "generic-functions")
								 (:file "test-case-functions")
								 (:file "test-suite-functions")
								 (:file "rerun-failed-tests")
								 (:file "progress-report-functions")))

						(:module "report-formats" 
							:components
								((:file "report-format-default")
								 (:file "report-format-tap")
								 (:file "print-object")))

						(:module "macros"
							:serial t
							:components
								((:file "utility-macros")
								 (:file "assertion-macros")
								 (:file "deffixture")
								 (:file "defsuite")
								 (:file "deftest")))))))
