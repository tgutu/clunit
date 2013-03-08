(in-package :cl-user)

(cl:defpackage :clunit
	(:use :cl)
	(:export
			;; Standard functions and macros
			:*clunit-report-format*
			:*clunit-equality-test*

			:clunit-report :clunit-test-report

			:deftest :defsuite :deffixture
			:undefsuite :undeftest :undeffixture

			:run-test :run-suite
			:rerun-failed-tests

			:assert-fail
			:assert-expands
			:assert-condition
			:assert-false :assert-true
			:assert-eq :assert-eql :assert-equal :assert-equalp :assert-equality :assert-equality*
			
			;; Programmatic functions and macros
			:defined-test-p			:defined-suite-p
			:get-child-tests 		:get-child-suites
			:get-defined-tests 		:get-defined-suites
			:clunit-test-reports	:clunit-test-report-name	:clunit-test-report-passed-p))