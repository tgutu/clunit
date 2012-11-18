(in-package :cl-user)

(asdf:defsystem :clunit
	:version "0.2.0"
	:author "Tapiwa Gutu"
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
