;;; taco-tests.el --- Tests for urgrep -*- lexical-binding: t -*-

;; Copyright (C) 2021 Jim Porter

;; Author: Jim Porter
;; Keywords: tests

;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.

;; You should have received a copy of the GNU General Public License along with
;; this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Tests for taco

;;; Code:

(require 'ert)

(defmacro with-fake-files (files &rest body)
  (declare (indent 1))
  `(let ((default-directory "/path/to/project/"))
     (cl-letf (((symbol-function #'file-exists-p)
              (lambda (path) (member (file-relative-name path) ,files))))
     ,@body)))

(ert-deftest taco-tests-bfg9000-fresh ()
  (with-fake-files '("build.bfg")
    (should (equal (taco-compile-command "build" nil)
                   "9k build/ && cd build/ && ninja"))
    (should (equal (taco-compile-command "build" t) "9k build/"))
    (should (equal (taco-get-builddir nil "build") "/path/to/project/build/"))))

(ert-deftest taco-tests-bfg9000-rebuild ()
  (with-fake-files '("build.bfg" "build/build.ninja")
    (should (equal (taco-compile-command "build" nil) "cd build/ && ninja"))
    (should (equal (taco-compile-command "build" t) "cd build/ && ninja"))
    (should (equal (taco-get-builddir nil "build") "/path/to/project/build/"))))

(ert-deftest taco-tests-autotools-fresh ()
  (with-fake-files '("configure")
    (should (equal (taco-compile-command "build" nil)
                   "mkdir build/ && cd build/ && ../configure && make"))
    (should (equal (taco-compile-command "build" t)
                   "mkdir build/ && cd build/ && ../configure"))
    (should (equal (taco-get-builddir nil "build") "/path/to/project/build/"))))

(ert-deftest taco-tests-autotools-automake ()
  (with-fake-files '("configure.ac")
    (should (equal (taco-compile-command "build" nil)
                   (concat "automake && mkdir build/ && cd build/ && "
                           "../configure && make")))
    (should (equal (taco-compile-command "build" t)
                   "automake"))
    (should (equal (taco-get-builddir nil "build") "/path/to/project/build/"))))

(ert-deftest taco-tests-autotools-rebuild ()
  (with-fake-files '("configure" "build/Makefile")
    (should (equal (taco-compile-command "build" nil)
                   "cd build/ && make"))
    (should (equal (taco-compile-command "build" t)
                   "cd build/ && make"))
    (should (equal (taco-get-builddir nil "build") "/path/to/project/build/"))))

(ert-deftest taco-tests-make ()
  (with-fake-files '("Makefile")
    (should (equal (taco-compile-command "build" nil)
                   "make"))
    (should (equal (taco-compile-command "build" t) "make"))
    (should (equal (taco-get-builddir nil "build") "/path/to/project/"))))

(ert-deftest taco-tests-make ()
  (with-fake-files '("build.ninja")
    (should (equal (taco-compile-command "build" nil)
                   "ninja"))
    (should (equal (taco-compile-command "build" t) "ninja"))
    (should (equal (taco-get-builddir nil "build") "/path/to/project/"))))

(ert-deftest taco-tests-absolute-builddir ()
  (with-fake-files '("build.bfg")
    (should (equal (taco-compile-command "/path/to/build" nil)
                   "9k ../build/ && cd ../build/ && ninja"))
    (should (equal (taco-compile-command "/path/to/build" t) "9k ../build/"))
    (should (equal (taco-get-builddir nil "/path/to/build")
                   "/path/to/build/"))))

;; In practice, this case would almost certainly fail (shell commands aren't
;; likely to be aware of TRAMP paths), but we test it to be sure our path
;; handling is robust.
(ert-deftest taco-tests-remote-builddir ()
  (let* ((builddir "/ssh:host:/path/to/build/")
         (escaped-builddir (taco--maybe-shell-quote-argument builddir)))
    (with-fake-files '("build.bfg")
      (should (equal (taco-compile-command builddir nil)
                     (format "9k %1$s && cd %1$s && ninja" escaped-builddir)))
      (should (equal (taco-compile-command builddir t)
                     (format "9k %s" escaped-builddir)))
      (should (equal (taco-get-builddir nil builddir)
                     "/ssh:host:/path/to/build/")))))
