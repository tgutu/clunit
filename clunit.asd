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
								((:file "classes")))
						 								 
						 (:module "functions" 
							:components
								((:file "generic-functions")
								 (:file "report-functions")
								 (:file "run-functions")))

						(:module "macros"
							:serial t
							:components
								((:file "utility-macros")
								 (:file "assertion-macros")
								 (:file "defmacros")))))))
