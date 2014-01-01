;;; function.el --- Test for function signature of go-mode.el

;; Copyright (C) 2013 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'ert)
(require 'go-eldoc)

;;
;; Tests for Signature String
;;

(ert-deftest one-argument-no-retval ()
  "Function has one argument and no return value"
  (with-go-temp-buffer
    "
package main
func foo(arg int) {
}
"
    (forward-cursor-on "arg")
    (let ((got (go-eldoc--documentation-function))
          (expected "foo: (arg int) "))
      (should (string= got expected)))))

(ert-deftest one-argument-one-retval ()
  "Function has one argument and one return value"
  (with-go-temp-buffer
    "
package main
func foo(arg int) int {
}
"
    (forward-cursor-on "arg")
    (let ((got (go-eldoc--documentation-function))
          (expected "foo: (arg int) int"))
      (should (string= got expected)))))

(ert-deftest one-argument-one-retval-has-name ()
  "Function has one argument and one return value has name"
  (with-go-temp-buffer
    "
package main
func foo(arg int) (ret int) {
}
"
    (forward-cursor-on "arg")
    (let ((got (go-eldoc--documentation-function))
          (expected "foo: (arg int) (ret int)"))
      (should (string= got expected)))))

(ert-deftest one-argument-contains-underscore ()
  "Function has one argument contains underscore(#8)"
  (with-go-temp-buffer
    "
package main
func foo(arg_name int) (ret int) {
}
"
    (forward-cursor-on "arg_name")
    (let ((got (go-eldoc--documentation-function))
          (expected "foo: (arg_name int) (ret int)"))
      (should (string= got expected)))))

(ert-deftest one-argument-multiple-retvals ()
  "Function has one argument and multiple return values"
  (with-go-temp-buffer
    "
package main
func foo(arg int) (int, int) {
}
"
    (forward-cursor-on "arg")
    (let ((got (go-eldoc--documentation-function))
          (expected "foo: (arg int) (int, int)"))
      (should (string= got expected)))))

(ert-deftest multiple-arguments-one-retval ()
  "Function has multiple arguments and one return value"
  (with-go-temp-buffer
    "
package main
func foo(arg1 int, arg2 int) int {
}
"
    (forward-cursor-on "arg1")
    (let ((got (go-eldoc--documentation-function))
          (expected "foo: (arg1 int, arg2 int) int"))
      (should (string= got expected)))))

(ert-deftest multiple-arguments-multiple-retval ()
  "Function has multiple arguments and multiple return values"
  (with-go-temp-buffer
    "
package main
func foo(arg1 int, arg2 string) (int, string) {
}
"
    (forward-cursor-on "arg1")
    (let ((got (go-eldoc--documentation-function))
          (expected "foo: (arg1 int, arg2 string) (int, string)"))
      (should (string= got expected)))))

(ert-deftest multiple-arguments-multiple-retval-have-names ()
  "Function has multiple arguments and multiple return values have names"
  (with-go-temp-buffer
    "
package main
func foo(arg1 int, arg2 string) (ret1 int, ret2 string) {
}
"
    (forward-cursor-on "arg1")
    (let ((got (go-eldoc--documentation-function))
          (expected "foo: (arg1 int, arg2 string) (ret1 int, ret2 string)"))
      (should (string= got expected)))))

(ert-deftest multiple-arguments-omit-type ()
  "Function has multiple argumes and multple return values but type is omitted"
  (with-go-temp-buffer
    "
package main
func foo(arg1, arg2 string) (ret1, ret2 int) {
}
"
    (forward-cursor-on "arg1")
    (let ((got (go-eldoc--documentation-function))
          (expected "foo: (arg1, arg2 string) (ret1, ret2 int)"))
      (should (string= got expected)))))

(ert-deftest channel-argument ()
  "Function has channel argument"
  (with-go-temp-buffer
    "
package main
func foo(ch <-chan string) int {
}
"
    (forward-cursor-on "ch")
    (let ((got (go-eldoc--documentation-function))
          (expected "foo: (ch <-chan string) int"))
      (should (string= got expected)))))

(ert-deftest channel-argument-and-channel-retval ()
  "Function has one argument and one return value"
  (with-go-temp-buffer
    "
package main
func foo(ch <-chan string) chan<- bool {
}
"
    (forward-cursor-on "ch")
    (let ((got (go-eldoc--documentation-function))
          (expected "foo: (ch <-chan string) (chan<- bool)"))
      (should (string= got expected)))))

(ert-deftest list-argument ()
  "Function has list argument"
  (with-go-temp-buffer
    "
package main
func foo(arg []int) int {
}
"
    (forward-cursor-on "arg")
    (let ((got (go-eldoc--documentation-function))
          (expected "foo: (arg []int) int"))
      (should (string= got expected)))))

(ert-deftest list-return-value ()
  "Function has channel argument"
  (with-go-temp-buffer
    "
package main
func foo(arg int) []int {
}
"
    (forward-cursor-on "arg")
    (let ((got (go-eldoc--documentation-function))
          (expected "foo: (arg int) []int"))
      (should (string= got expected)))))

(ert-deftest interface-argument ()
  "Function has interface argument"
  (with-go-temp-buffer
    "
package main
func foo(arg interface{}) int {
}
"
    (forward-cursor-on "arg")
    (let ((got (go-eldoc--documentation-function))
          (expected "foo: (arg interface{}) int"))
      (should (string= got expected)))))

(ert-deftest interface-return-value ()
  "Function has interface return value"
  (with-go-temp-buffer
    "
package main
func foo(arg int) interface{} {
}
"
    (forward-cursor-on "arg")
    (let ((got (go-eldoc--documentation-function))
          (expected "foo: (arg int) interface{}"))
      (should (string= got expected)))))

(ert-deftest interface-return-value ()
  "Function has interface return value"
  (with-go-temp-buffer
    "
package main
func foo(arg int) interface{} {
}
"
    (forward-cursor-on "arg")
    (let ((got (go-eldoc--documentation-function))
          (expected "foo: (arg int) interface{}"))
      (should (string= got expected)))))

(ert-deftest list-of-channel-argument ()
  "Function has list of channels argument"
  (with-go-temp-buffer
    "
package main
func foo(ch [2]string) int {
}
"
    (forward-cursor-on "ch")
    (let ((got (go-eldoc--documentation-function))
          (expected "foo: (ch [2]string) int"))
      (should (string= got expected)))))

(ert-deftest list-of-interface-argument ()
  "Function has list of channels argument"
  (with-go-temp-buffer
    "
package main
func foo(arg [999]interface{}) int {
}
"
    (forward-cursor-on "arg")
    (let ((got (go-eldoc--documentation-function))
          (expected "foo: (arg [999]interface{}) int"))
      (should (string= got expected)))))

(ert-deftest slice-of-channel-argument ()
  "Function has slice of channels argument"
  (with-go-temp-buffer
    "
package main
func foo(ch []string) int {
}
"
    (forward-cursor-on "ch")
    (let ((got (go-eldoc--documentation-function))
          (expected "foo: (ch []string) int"))
      (should (string= got expected)))))

(ert-deftest slice-of-interface-argument ()
  "Function has slice of channels argument"
  (with-go-temp-buffer
    "
package main
func foo(arg []interface{}) int {
}
"
    (forward-cursor-on "arg")
    (let ((got (go-eldoc--documentation-function))
          (expected "foo: (arg []interface{}) int"))
      (should (string= got expected)))))

(ert-deftest type-only-argument ()
  "Function has type only argument"
  (with-go-temp-buffer
    "
package main
import \"time\"
type test_interface interface{
    test_mf(arg time.Duration)
}

func main() {
	var ti test_interface
	ti.test_mf( )
}
"
    (forward-cursor-on "main")
    (forward-cursor-on "main")
    (forward-cursor-on "( )")
    (forward-char 1)

    (let ((got (go-eldoc--documentation-function))
          (expected "test_mf: (arg time.Duration) "))
      (should (string= got expected)))))

(ert-deftest type-only-argument-complex ()
  "Function has type only argument which is complex(#10)"
  (with-go-temp-buffer
    "
package main
import \"time\"
type test_interface interface{
    test_ms([]time.Duration)
}

func main() {
	var ti test_interface
	ti.test_ms( )
}
"
    (forward-cursor-on "main")
    (forward-cursor-on "main")
    (forward-cursor-on "( )")
    (forward-char 1)

    (let ((got (go-eldoc--documentation-function))
          (expected "test_ms: ([]time.Duration) "))
      (should (string= got expected)))))

;;; function.el end here
