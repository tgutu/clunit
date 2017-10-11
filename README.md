## CLUnit

CLUnit is a Common Lisp unit testing framework. It is designed to be easy to use so that you can quickly start testing.

Author: Tapiwa Gutu

CLUnit provides a rich set of features aimed at improving your unit testing experience:
+ Multiple inheritance for test suites allows you to group tests into hierarchies.
+ Composes the test results of each test run into a single report.
+ Allows redefinition of inline functions and macros without having to redefine your tests.
+ Supports composable test suite fixtures.
+ Allows for an interactive testing process which gives you access to the test environment.
+ Provides visual feedback of the unit test progress.
+ Extensible test reporting. Builtin support for default reporting and [TAP][2] output.
+ Released under MIT license

Check out the comprehensive [CLUnit Tutorial][1].

# Example

```cl
(ql:quickload "clunit")
(use-package :clunit)

;; Test suite for all number operation tests.
(defsuite NumberSuite ())
  
;; Test suite for floating point operations
(defsuite FloatSuite (NumberSuite))
  
(defsuite IntegerSuite (NumberSuite))
    
;; Define a test called TEST-INT1
(deftest test-int1 (IntegerSuite)
    (assert-true  (= 1 1))
    (assert-equalp 4 (+ 2 2)))

;; Define a test called TEST-FLOAT1
(deftest test-float1 (FloatSuite)
    (assert-true (= 1.0 -1.0))
    (assert-equalp 4.0 (+ 2.0 2.0)))

(print (run-suite 'NumberSuite))

```

which produces the output:

```
PROGRESS:
=========

    NUMBERSUITE: (Test Suite)

        INTEGERSUITE: (Test Suite)
            TEST-INT1: ..

        FLOATSUITE: (Test Suite)
            TEST-FLOAT1: F.

FAILURE DETAILS:
================

    NUMBERSUITE -> FLOATSUITE: (Test Suite)
        TEST-FLOAT1: Expression: (= 1.0 -1.0)
                     Expected: T
                     Returned: NIL


SUMMARY:
========
    Test functions:
        Executed: 2
        Skipped:  0

    Tested 4 assertions.
        Passed: 3/4 ( 75.0%)
        Failed: 1/4 ( 25.0%) 
```

(if you are entering forms in the REPL, the ``print`` form is not usually needed).

# Tests and assertions

Each test, like ``test-int1`` in the above example, can contain a number of assertions, given in the table below:

| Assertion                                  | Description                                                      |
| ------------------------------------------ | ---------------------------------------------------------------- |
| ``assert-true EXPRESSION``                 | Passes if the expression ``EXPRESSION`` is not ``NIL``           |
| ``assert-false EXPRESSION``                | Passes if ``EXPRESSION`` is ``NIL``                              |
| ``assert-eq VALUE EXPRESSION``             | Passes if ``(EQ VALUE EXPRESSION)`` returns true                 |
| ``assert-eql VALUE EXPRESSION``            | Passes if ``(EQL VALUE EXPRESSION)`` returns true                |
| ``assert-equal VALUE EXPRESSION``          | Passes if ``(EQUAL VALUE EXPRESSION)`` returns true              |
| ``assert-equalp VALUE EXPRESSION``         | Passes if ``(EQUALP VALUE EXPRESSION)`` returns true             |
| ``assert-equality TEST VALUE EXPRESSION``  | Passes if ``(FUNCALL TEST VALUE EXPRESSION)`` returns true       |
| ``assert-equality* VALUE EXPRESSION``      | Passes if  ``(FUNCALL \*clunit-equality-test\* VALUE EXPRESSION)`` returns true. By default *clunit-equality-test* is ``EQUALP`` |
| ``assert-expands EXPANSION EXPRESSION``    | Tests macro expansion, passes if ``(EQUALP EXPANSION (MACROEXPAND-1 EXPRESSION))`` is true        |
| ``assert-condition CONDITION EXPRESSION``  | Passes if ``EXPRESSION`` signals ``CONDITION``                   |
| ``assert-fails FORMAT-STRING``             | Force test to fail, giving a format string for the message       |


All of these tests take optional forms, which are evaluated and printed if the test fails.
These can be used to provide test diagnostics or documentation. For example

```cl

(deftest test-suiteless ()
    (let ((a 1) (b 2) (c 3))
        (assert-true (= a b c) "This assertion is meant to fail." a b c )))

(run-test 'test-suiteless :report-progress nil)
```
produces the output:

```
FAILURE DETAILS:
================
    TEST-SUITELESS: Expression: (= A B C)
                    Expected: T
                    Returned: NIL
                    This assertion is meant to fail.
                    A => 1
                    B => 2
                    C => 3

SUMMARY:
========
    Test functions:
        Executed: 1
        Skipped:  0

    Tested 1 assertion.
        Passed: 0/1 (  0.0%)
        Failed: 1/1 (100.0%)
        Errors: 0/1 (  0.0%)
```

(if not in a REPL, you may have to print the return value of ``run-test``).

[1]: http://tgutu.github.com/clunit  "CLUnit"
[2]: http://en.wikipedia.org/wiki/Test_Anything_Protocol "Test Anything Protocol"
