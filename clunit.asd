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
								((:file "classes")
								 (:file "clunit-test-case")
								 (:file "clunit-test-suite")
								 (:file "clunit-test-report")
								 (:file "clunit-report")))
						 								 
						 (:module "functions" 
							:components
								((:file "generic-functions")
								 (:file "report-functions")
								 (:file "run-functions")
								 (:file "utility-functions")))

						(:module "macros"
							:serial t
							:components
								((:file "utility-macros")
								 (:file "assertion-macros")
								 (:file "defmacros")
								 (:file "defsuite")
								 (:file "deftest")))))))
