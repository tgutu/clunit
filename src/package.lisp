(in-package :cl-user)

(cl:defpackage :clunit
	(:use :cl)
	(:export
			:clunit-report :clunit-test-report
			:*clunit-report-format*
			:deftest :defsuite :deffixture
			:undefsuite :undeftest
			:run-test :run-suite :rerun-failed-tests
			:assert-false :assert-true
			:assert-condition :assert-expands
			:assert-eq :assert-eql :assert-equal :assert-equalp))