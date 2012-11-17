(in-package :clunit)

(defun assertion-expander (&key result test result-expression report-expression expected forms)
	"Expands an assertion macro call."
	`(with-assert-restart
		(let ((,result ,result-expression))
			(if ,test
				(signal-assertion :pass)
				(signal-assertion :fail
					:returned ,result
					:expected ',expected
					:expression ',report-expression
					:forms (list ,@(form-expander forms)))))))

(defun form-expander (forms)
	;; FORM-EXPANDER manipulates the list of forms provided to an assertion form, e.g. (defmacro assert-false (expression &rest forms) . body)
	;; The members of the forms list are printed out when an assertion test fails. The example below, shows the debug output when an assertion form fails.
	;; 
	;; (let ((x 1) (y 2) (z 3))
	;;		(assert-true (= x y z) x y "Comment: This is meant to fail." z))				; forms = '(x y "Comment: This is meant to fail." z)
	;;
	;;	======== Debug output ===========
	;;	Expression: (= x y z)
	;;	Expected: T
	;;	Returned: NIL
	;;	x => 1
	;;  y => 2
	;;  Comment: This is meant to fail.
	;;	z => 3
	;;	==================================
	;;	As you can see, the reporting is somehow able to differentiate between the symbols x, y, z and the string comment.
	;;	This is achieved by expanding '(x y "Comment..." z) => (T 'x x T 'y y NIL "Comment..." T 'z z)
	;;	The T or NIL symbol tells the reporting function whether to report the next two values as a pair or not.
	;;	I went at great lengths to explain this because WHAT the function does is straight forward from the code, but WHY it does it
	;;	isn't too obvious unless someone tells you :o)
	(loop
		:for form  :in forms
		:if (typep form 'string)
			:collect nil :and :collect form
		:else
			:collect t :and :collect `',form :and :collect form))


(defmacro assert-true (expression &body forms)
	"Evaluates EXPRESSION as an assertion, an assertion passes if it returns any non-NIL value. FORMS and their values are printed if the test fails.
Remember in Common Lisp any non-NIL value is true, if you want a strict binary assertion test use (assert-eq t expression) instead."
	(with-gensyms (result)
		(assertion-expander :result result :test result :result-expression expression :report-expression expression :expected t :forms forms)))


(defmacro assert-false (expression &body forms)
	"Evaluates EXPRESSION as an assertion, an assertion passes if it returns false. FORMS and their values are printed if the test fails."
	(with-gensyms (result)
		(assertion-expander :result result :test `(not ,result) :result-expression expression :report-expression expression :expected nil :forms forms)))


;; Equality assertion macros.
(defmacro assert-eq (value expression &body forms)
	"Evaluates EXPRESSION as an assertion, an assertion passes if (EQ VALUE EXPRESSION) returns true. FORMS and their values are printed if the test fails."
	(with-gensyms (result)
		(assertion-expander :result result :test `(eq ,value ,result) :result-expression expression  :report-expression `(eq ,value ,expression) :expected value :forms forms)))


(defmacro assert-eql (value expression &body forms)
	"Evaluates EXPRESSION as an assertion, an assertion passes if (EQL VALUE EXPRESSION) returns true. FORMS and their values are printed if the test fails."
	(with-gensyms (result)
		(assertion-expander :result result :test `(eql ,value ,result) :result-expression expression  :report-expression `(eql ,value ,expression) :expected value :forms forms)))


(defmacro assert-equal (value expression &body forms)
	"Evaluates EXPRESSION as an assertion, an assertion passes if (EQUAL VALUE EXPRESSION) returns true. FORMS and their values are printed if the test fails."
	(with-gensyms (result)
		(assertion-expander :result result :test `(equal ,value ,result) :result-expression expression  :report-expression `(equal ,value ,expression) :expected value :forms forms)))


(defmacro assert-equalp (value expression &body forms)
	"Evaluates EXPRESSION as an assertion, an assertion passes if (EQUALP VALUE EXPRESSION) returns true. FORMS and their values are printed if the test fails."
	(with-gensyms (result)
		(assertion-expander :result result :test `(equalp ,value ,result) :result-expression expression  :report-expression `(equalp ,value ,expression) :expected value :forms forms)))


;; MACROEXPAND-1 assertion macro
(defmacro assert-expands (&environment env expansion expression &body forms)
	"Evaluates EXPRESSION as an assertion, an assertion passes if (EQUALP EXPANSION (MACROEXPAND-1 EXPRESSION)) returns true. FORMS and their values are printed if the test fails."
	(with-gensyms (result)
		(assertion-expander :result result :test `(equalp ,result ',expansion) :result-expression `(macroexpand-1 ',expression ,env)  :report-expression `(macroexpand-1 ',expression) :expected expansion :forms forms)))


;; Condition assertion macro.
(defmacro assert-condition (condition expression &body forms)
	"Evaluates EXPRESSION as an assertion, an assertion passes if EXPRESSION signals CONDITION. FORMS and their values are printed if the test fails."
	`(with-assert-restart
		(handler-case 
			(progn
				,expression
				(signal-assertion :fail :expression ',expression :expected ',condition :forms (list ,@(form-expander forms))))
			
			(,condition ()
					(signal-assertion :pass))
			(condition (c)
					(signal-assertion :fail :expression ',expression :returned (type-of c) :expected ',condition :forms (list ,@(form-expander forms)))))))


