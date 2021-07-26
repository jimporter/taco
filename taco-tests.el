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
  "Pretend FILES exist within BODY."
  (declare (indent 1))
  `(let* ((files ,files)
          (project-directory "/path/to/project/")
          (default-directory project-directory))
     (cl-letf (((symbol-function #'file-exists-p)
                (lambda (path)
                  (member (file-relative-name path project-directory) files))))
       ,@body)))

(defmacro with-fake-project (project &rest body)
  "Pretend the active project is PROJECT within BODY."
  (declare (indent 1))
  `(let ((project ,project))
     (cl-letf (((symbol-function #'project-current)
                (lambda (&optional maybe-prompt _directory)
                  (or project (when maybe-prompt '(transient . ""))))))
       ,@body)))

(ert-deftest taco-tests-get-builddir-out-of-tree-builder ()
  (with-fake-files '("build.bfg")
    ;; `taco-get-builddir'
    (should (equal (taco-get-builddir default-directory)
                   "/path/to/project/build/"))
    (should (equal (taco-get-builddir "") "/path/to/project/build/"))
    (let ((default-directory "/path/to/elsewhere/"))
      (should (equal (taco-get-builddir "/path/to/project/")
                     "/path/to/project/build/")))
    (should (equal (taco-get-builddir default-directory "builddir")
                   "/path/to/project/builddir/"))
    ;; `taco-project-get-builddir'
    (should (equal (taco-project-get-builddir '(vc . "/path/to/project/"))
                   "/path/to/project/build/"))
    (should (equal (taco-project-get-builddir '(vc . "/path/to/project/")
                                              "builddir")
                   "/path/to/project/builddir/"))
    (should (equal (taco-project-get-builddir '(transient . ""))
                   "/path/to/project/build/"))
    (should-error (taco-project-get-builddir nil))
    (let ((default-directory "/path/to/project/sub/"))
      (should (equal (taco-project-get-builddir '(vc . "/path/to/project/"))
                     "/path/to/project/build/")))))

(ert-deftest taco-tests-get-builddir-in-tree-builder ()
  (with-fake-files '("Makefile")
    ;; `taco-get-builddir'
    (should (equal (taco-get-builddir default-directory) "/path/to/project/"))
    (should (equal (taco-get-builddir "") "/path/to/project/"))
    (let ((default-directory "/path/to/elsewhere/"))
      (should (equal (taco-get-builddir "/path/to/project/")
                     "/path/to/project/")))
    (should (equal (taco-get-builddir default-directory "builddir")
                   "/path/to/project/"))
    ;; `taco-project-get-builddir'
    (should (equal (taco-project-get-builddir '(vc . "/path/to/project/"))
                   "/path/to/project/"))
    (should (equal (taco-project-get-builddir '(vc . "/path/to/project/")
                                              "builddir")
                   "/path/to/project/"))
    (should (equal (taco-project-get-builddir '(transient . ""))
                   "/path/to/project/"))
    (should-error (taco-project-get-builddir nil))
    (let ((default-directory "/path/to/project/sub/"))
      (should (equal (taco-project-get-builddir '(vc . "/path/to/project/"))
                     "/path/to/project/")))))

(ert-deftest taco-tests-get-builddir-no-builder ()
  (with-fake-files nil
    ;; `taco-get-builddir'
    (should (equal (taco-get-builddir default-directory) "/path/to/project/"))
    (should (equal (taco-get-builddir "") "/path/to/project/"))
    (let ((default-directory "/path/to/elsewhere/"))
      (should (equal (taco-get-builddir "/path/to/project/")
                     "/path/to/project/"))
      (should (equal (taco-get-builddir "../project")
                     "/path/to/project/")))
    (should (equal (taco-get-builddir default-directory "builddir")
                   "/path/to/project/"))
    ;; `taco-project-get-builddir'
    (should (equal (taco-project-get-builddir '(vc . "/path/to/project/"))
                   "/path/to/project/"))
    (should (equal (taco-project-get-builddir '(vc . "/path/to/project/")
                                              "builddir")
                   "/path/to/project/"))
    (should (equal (taco-project-get-builddir '(transient . ""))
                   "/path/to/project/"))
    (should-error (taco-project-get-builddir nil))
    (let ((default-directory "/path/to/project/sub/"))
      (should (equal (taco-project-get-builddir '(vc . "/path/to/project/"))
                     "/path/to/project/")))))

(ert-deftest taco-tests-compile-command ()
  (with-fake-files '("build.bfg")
    (should (equal (taco-compile-command default-directory "build")
                   "9k build/ && cd build/ && ninja"))
    (should (equal (taco-compile-command "" "build")
                   "9k build/ && cd build/ && ninja"))
    (should (equal (taco-compile-command default-directory "build" :one-step t)
                   "9k build/"))
    (should (equal (taco-compile-command default-directory "build"
                                         :trailing-space t)
                   "9k build/ && cd build/ && ninja "))
    (should (equal (taco-compile-command default-directory "build" :one-step t
                                         :trailing-space t)
                   "9k build/ "))
    (let ((default-directory "/path/to/elsewhere/"))
      (should (equal (taco-compile-command "/path/to/project/" "build")
                     "9k build/ && cd build/ && ninja"))
      (should (equal (taco-compile-command "../project" "build")
                     "9k build/ && cd build/ && ninja")))))

(ert-deftest taco-tests-compile ()
  (cl-letf (((symbol-function #'compile)
             (lambda (x) (interactive (list compile-command)) x)))
    (with-fake-files '("build.bfg")
      ;; `taco-compile'
      (should (equal (taco-compile "/path/to/project/")
                     "9k build/ && cd build/ && ninja"))
      (should (equal (taco-compile "/path/to/project/" "builddir")
                     "9k builddir/ && cd builddir/ && ninja"))
      (should (equal (call-interactively #'taco-compile)
                     "9k build/ && cd build/ && ninja "))
      ;; `taco-project-compile'
      (should (equal (taco-project-compile `(vc . ,default-directory))
                     "9k build/ && cd build/ && ninja"))
      (should (equal (taco-project-compile `(vc . ,default-directory)
                                           "builddir")
                     "9k builddir/ && cd builddir/ && ninja"))
      (should (equal (taco-project-compile '(transient . ""))
                     "9k build/ && cd build/ && ninja"))
      (should-error (taco-project-compile nil))
      (with-fake-project `(vc . ,default-directory)
        (should (equal (call-interactively #'taco-project-compile)
                       "9k build/ && cd build/ && ninja ")))
      (with-fake-project '(transient . "")
        (should (equal (call-interactively #'taco-project-compile)
                       "9k build/ && cd build/ && ninja ")))
      (with-fake-project nil
        (should (equal (call-interactively #'taco-project-compile)
                       "9k build/ && cd build/ && ninja ")))
        )))

(ert-deftest taco-tests-bfg9000-fresh ()
  (with-fake-files '("build.bfg")
    (should (equal (taco-compile-command default-directory "build")
                   "9k build/ && cd build/ && ninja"))
    (should (equal (taco-compile-command default-directory "build" :one-step t)
                   "9k build/"))
    (should (equal (taco-get-builddir default-directory "build")
                   "/path/to/project/build/"))))

(ert-deftest taco-tests-bfg9000-rebuild ()
  (with-fake-files '("build.bfg" "build/build.ninja")
    (should (equal (taco-compile-command default-directory "build")
                   "cd build/ && ninja"))
    (should (equal (taco-compile-command default-directory "build" :one-step t)
                   "cd build/ && ninja"))
    (should (equal (taco-get-builddir default-directory "build")
                   "/path/to/project/build/"))))

(ert-deftest taco-tests-cmake-fresh ()
  (with-fake-files '("CMakeLists.txt")
    (should (equal (taco-compile-command default-directory "build")
                   "cmake -G Unix\\ Makefiles build/ && cd build/ && make"))
    (should (equal (taco-compile-command default-directory "build" :one-step t)
                   "cmake -G Unix\\ Makefiles build/"))
    (should (equal (taco-get-builddir default-directory "build")
                   "/path/to/project/build/"))))

(ert-deftest taco-tests-cmake-rebuild ()
  (with-fake-files '("CMakeLists.txt" "build/Makefile")
    (should (equal (taco-compile-command default-directory "build")
                   "cd build/ && make"))
    (should (equal (taco-compile-command default-directory "build" :one-step t)
                   "cd build/ && make"))
    (should (equal (taco-get-builddir default-directory "build")
                   "/path/to/project/build/"))))

(ert-deftest taco-tests-autotools-fresh ()
  (with-fake-files '("configure")
    (should (equal (taco-compile-command default-directory "build")
                   "mkdir build/ && cd build/ && ../configure && make"))
    (should (equal (taco-compile-command default-directory "build" :one-step t)
                   "mkdir build/ && cd build/ && ../configure"))
    (should (equal (taco-get-builddir default-directory "build")
                   "/path/to/project/build/"))))

(ert-deftest taco-tests-autotools-automake ()
  (with-fake-files '("configure.ac")
    (should (equal (taco-compile-command default-directory "build")
                   (concat "autoreconf && mkdir build/ && cd build/ && "
                           "../configure && make")))
    (should (equal (taco-compile-command default-directory "build" :one-step t)
                   "autoreconf"))
    (should (equal (taco-get-builddir default-directory "build")
                   "/path/to/project/build/"))))

(ert-deftest taco-tests-autotools-rebuild ()
  (with-fake-files '("configure" "build/Makefile")
    (should (equal (taco-compile-command default-directory "build")
                   "cd build/ && make"))
    (should (equal (taco-compile-command default-directory "build" :one-step t)
                   "cd build/ && make"))
    (should (equal (taco-get-builddir default-directory "build")
                   "/path/to/project/build/"))))

(ert-deftest taco-tests-make ()
  (with-fake-files '("Makefile")
    (should (equal (taco-compile-command "build")
                   "make"))
    (should (equal (taco-compile-command "build" :one-step t) "make"))
    (should (equal (taco-get-builddir nil "build") "/path/to/project/"))))

(ert-deftest taco-tests-make ()
  (with-fake-files '("build.ninja")
    (should (equal (taco-compile-command default-directory "build")
                   "ninja"))
    (should (equal (taco-compile-command default-directory "build" :one-step t)
                   "ninja"))
    (should (equal (taco-get-builddir default-directory "build")
                   "/path/to/project/"))))

(ert-deftest taco-tests-absolute-builddir ()
  (with-fake-files '("build.bfg")
    (should (equal (taco-compile-command default-directory "/path/to/build")
                   "9k ../build/ && cd ../build/ && ninja"))
    (should (equal (taco-compile-command default-directory "/path/to/build"
                                         :one-step t)
                   "9k ../build/"))
    (should (equal (taco-get-builddir default-directory "/path/to/build")
                   "/path/to/build/"))))

;; In practice, this case would almost certainly fail (shell commands aren't
;; likely to be aware of TRAMP paths), but we test it to be sure our path
;; handling is robust.
(ert-deftest taco-tests-remote-builddir ()
  (let* ((builddir "/ssh:host:/path/to/build/")
         (escaped-builddir (taco--maybe-shell-quote-argument builddir)))
    (with-fake-files '("build.bfg")
      (should (equal (taco-compile-command default-directory builddir)
                     (format "9k %1$s && cd %1$s && ninja" escaped-builddir)))
      (should (equal (taco-compile-command default-directory builddir
                                           :one-step t)
                     (format "9k %s" escaped-builddir)))
      (should (equal (taco-get-builddir default-directory builddir)
                     "/ssh:host:/path/to/build/")))))

;;; taco-tests.el ends here
