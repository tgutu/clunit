(in-package :cl-user)

(cl:defpackage :clunit
	(:use :cl)
	(:export
			:clunit-report
			:*clunit-report-format*
			:deftest :defsuite :deffixture
			:undefsuite :undeftest
			:run-test :run-suite
			:assert-false :assert-true
			:assert-condition :assert-expands
			:assert-eq :assert-eql :assert-equal :assert-equalp))