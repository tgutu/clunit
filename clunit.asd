(in-package :cl-user)

(asdf:defsystem :clunit
	:version "0.1.0"
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
								((:file "generic-functions")
								 (:file "execute-test-case")
								 (:file "execute-test-suite")
								 (:file "report-functions")
								 (:file "utility-functions")))

						(:module "macros"
							:serial t
							:components
								((:file "utility-macros")
								 (:file "assertion-macros")
								 (:file "deffixture")
								 (:file "defsuite")
								 (:file "deftest")))))))
