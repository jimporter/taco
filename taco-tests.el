;;; taco-tests.el --- Tests for taco -*- lexical-binding: t -*-

;; Copyright (C) 2021-2026 Jim Porter

;; Author: Jim Porter
;; URL: https://sr.ht/~jimporter/taco/
;; Keywords: tests

;; This file is NOT part of GNU Emacs.

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
(require 'taco)

(defvar comp-warn-primitives)

(defmacro with-fake-files (files &rest body)
  "Pretend FILES exist within BODY."
  (declare (indent 1))
  `(cl-letf* ((files ,files)
              (project-directory "/path/to/project/")
              (comp-warn-primitives
               (remove 'file-exists-p (bound-and-true-p comp-warn-primitives)))
              ((symbol-function #'file-exists-p)
               (lambda (path)
                 (member (file-relative-name path project-directory) files)))
              (default-directory project-directory))
       ,@body))

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
    (should (equal (taco-project-get-builddir '(transient . ""))
                   "/path/to/project/build/"))
    (should (equal (taco-project-get-builddir '(transient . "") "builddir")
                   "/path/to/project/builddir/"))
    (let ((default-directory "/path/to/elsewhere/"))
      (should (equal (taco-project-get-builddir
                      '(transient . "/path/to/project/"))
                     "/path/to/project/build/")))
    (let ((default-directory "/path/to/project/sub/"))
      (should (equal (taco-project-get-builddir
                      '(transient . "/path/to/project/"))
                     "/path/to/project/build/")))
    (should-error (taco-project-get-builddir nil))))

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
    (should (equal (taco-project-get-builddir '(transient . ""))
                   "/path/to/project/"))
    (should (equal (taco-project-get-builddir '(transient . "") "builddir")
                   "/path/to/project/"))
    (let ((default-directory "/path/to/elsewhere/"))
      (should (equal (taco-project-get-builddir
                      '(transient . "/path/to/project/"))
                     "/path/to/project/")))
    (let ((default-directory "/path/to/project/sub/"))
      (should (equal (taco-project-get-builddir
                      '(transient . "/path/to/project/"))
                     "/path/to/project/")))
    (should-error (taco-project-get-builddir nil))))

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
    (should (equal (taco-project-get-builddir '(transient . ""))
                   "/path/to/project/"))
    (should (equal (taco-project-get-builddir '(transient . "") "builddir")
                   "/path/to/project/"))
    (let ((default-directory "/path/to/elsewhere/"))
      (should (equal (taco-project-get-builddir
                      '(transient . "/path/to/project/"))
                     "/path/to/project/")))
    (let ((default-directory "/path/to/project/sub/"))
      (should (equal (taco-project-get-builddir
                      '(transient . "/path/to/project/"))
                     "/path/to/project/")))
    (should-error (taco-project-get-builddir nil))))

(ert-deftest taco-tests-compile-command ()
  (let ((taco-num-jobs 4))
    (with-fake-files '("build.bfg")
      (should (equal (taco-compile-command default-directory "build")
                     "9k build/ && cd build/ && ninja -j4"))
      (should (equal (taco-compile-command "" "build")
                     "9k build/ && cd build/ && ninja -j4"))
      (should (equal (taco-compile-command default-directory "build"
                                           :one-step t)
                     "9k build/"))
      (should (equal (taco-compile-command default-directory "build"
                                           :trailing-space t)
                     "9k build/ && cd build/ && ninja -j4 "))
      (should (equal (taco-compile-command default-directory "build"
                                           :one-step t :trailing-space t)
                     "9k build/ "))
      (let ((default-directory "/path/to/elsewhere/"))
        (should (equal (taco-compile-command "/path/to/project/" "build")
                       "9k build/ && cd build/ && ninja -j4"))
        (should (equal (taco-compile-command "../project" "build")
                       "9k build/ && cd build/ && ninja -j4"))))
    (with-fake-files nil
      (should (equal (taco-compile-command default-directory "build") ""))
      (should (equal (taco-compile-command default-directory "build"
                                           :trailing-space t)
                     "")))))

(ert-deftest taco-tests-compile ()
  (cl-letf ((taco-num-jobs 4)
            ((symbol-function #'compile)
             (lambda (x) (interactive (list compile-command)) x)))
    (with-fake-files '("build.bfg")
      ;; `taco-compile'
      (should (equal (taco-compile "/path/to/project/")
                     "9k build/ && cd build/ && ninja -j4"))
      (should (equal (taco-compile "/path/to/project/" "builddir")
                     "9k builddir/ && cd builddir/ && ninja -j4"))
      (should (equal (call-interactively #'taco-compile)
                     "9k build/ && cd build/ && ninja -j4 "))
      ;; `taco-project-compile'
      (should (equal (taco-project-compile '(transient . ""))
                     "9k build/ && cd build/ && ninja -j4"))
      (should (equal (taco-project-compile '(transient . "") "builddir")
                     "9k builddir/ && cd builddir/ && ninja -j4"))
      (should-error (taco-project-compile nil))
      (with-fake-project '(transient . "")
        (should (equal (call-interactively #'taco-project-compile)
                       "9k build/ && cd build/ && ninja -j4 ")))
      (with-fake-project nil
        (should (equal (call-interactively #'taco-project-compile)
                       "9k build/ && cd build/ && ninja -j4 "))))))

(ert-deftest taco-tests-bfg9000-fresh ()
  (let ((taco-num-jobs 4))
    (with-fake-files '("build.bfg")
      (should (equal (taco-compile-command default-directory "build")
                     "9k build/ && cd build/ && ninja -j4"))
      (should (equal (taco-compile-command default-directory "build"
                                           :one-step t)
                     "9k build/"))
      (let ((taco-extra-arguments-alist '((bfg9000 "--extra")
                                          (ninja "--extra-build"))))
        (should (equal (taco-compile-command default-directory "build")
                       (concat "9k --extra build/ && cd build/ && "
                               "ninja -j4 --extra-build"))))
      (should (equal (taco-get-builddir default-directory "build")
                     "/path/to/project/build/")))))

(ert-deftest taco-tests-bfg9000-rebuild ()
  (let ((taco-num-jobs 4))
    (with-fake-files '("build.bfg" "build/build.ninja")
      (should (equal (taco-compile-command default-directory "build")
                     "cd build/ && ninja -j4"))
      (should (equal (taco-compile-command default-directory "build"
                                           :one-step t)
                     "cd build/ && ninja -j4"))
      (should (equal (taco-get-builddir default-directory "build")
                     "/path/to/project/build/")))))

(ert-deftest taco-tests-cmake-fresh ()
  (let ((taco-num-jobs 4))
    (with-fake-files '("CMakeLists.txt")
      (should (equal (taco-compile-command default-directory "build")
                     (concat "cmake build/ && cd build/ && "
                             "cmake --build ./ -j4")))
      (should (equal (taco-compile-command default-directory "build"
                                           :one-step t)
                     "cmake build/"))
      (let ((taco-extra-arguments-alist '((cmake "--extra")
                                          (cmake-build "--extra-build"))))
        (should (equal (taco-compile-command default-directory "build")
                       (concat "cmake --extra build/ && cd build/ && "
                               "cmake --build ./ -j4 --extra-build"))))
      (should (equal (taco-get-builddir default-directory "build")
                     "/path/to/project/build/")))))

(ert-deftest taco-tests-cmake-rebuild ()
  (let ((taco-num-jobs 4))
    (with-fake-files '("CMakeLists.txt" "build/CMakeCache.txt")
      (should (equal (taco-compile-command default-directory "build")
                     "cd build/ && cmake --build ./ -j4"))
      (should (equal (taco-compile-command default-directory "build"
                                           :one-step t)
                     "cd build/ && cmake --build ./ -j4"))
      (should (equal (taco-get-builddir default-directory "build")
                     "/path/to/project/build/")))))

(ert-deftest taco-tests-cmake-prefer-cmake-build ()
  (let ((taco-num-jobs 4))
    (with-fake-files '("CMakeLists.txt" "build/Makefile" "build/CMakeCache.txt")
      (should (equal (taco-compile-command default-directory "build")
                     "cd build/ && cmake --build ./ -j4"))
      (should (equal (taco-compile-command default-directory "build"
                                           :one-step t)
                     "cd build/ && cmake --build ./ -j4")))))

(ert-deftest taco-tests-autotools-fresh ()
  (let ((taco-num-jobs 4))
    (with-fake-files '("configure")
      (should (equal (taco-compile-command default-directory "build")
                     "mkdir build/ && cd build/ && ../configure && make -j4"))
      (should (equal (taco-compile-command default-directory "build"
                                           :one-step t)
                     "mkdir build/ && cd build/ && ../configure"))
      (let ((taco-extra-arguments-alist '((configure "--extra")
                                          (make "--extra-build"))))
        (should (equal (taco-compile-command default-directory "build")
                       (concat "mkdir build/ && cd build/ && "
                               "../configure --extra && "
                               "make -j4 --extra-build"))))
      (should (equal (taco-get-builddir default-directory "build")
                     "/path/to/project/build/")))))

(ert-deftest taco-tests-autotools-automake ()
  (let ((taco-num-jobs 4))
    (with-fake-files '("configure.ac")
      (should (equal (taco-compile-command default-directory "build")
                     (concat "autoreconf && mkdir build/ && cd build/ && "
                             "../configure && make -j4")))
      (should (equal (taco-compile-command default-directory "build"
                                           :one-step t)
                     "autoreconf"))
      (should (equal (taco-get-builddir default-directory "build")
                     "/path/to/project/build/")))))

(ert-deftest taco-tests-autotools-rebuild ()
  (let ((taco-num-jobs 4))
    (with-fake-files '("configure" "build/Makefile")
      (should (equal (taco-compile-command default-directory "build")
                     "cd build/ && make -j4"))
      (should (equal (taco-compile-command default-directory "build"
                                           :one-step t)
                     "cd build/ && make -j4"))
      (should (equal (taco-get-builddir default-directory "build")
                     "/path/to/project/build/")))))

(ert-deftest taco-tests-make ()
  (let ((taco-num-jobs 4))
    (with-fake-files '("Makefile")
      (should (equal (taco-compile-command default-directory "build")
                     "make -j4"))
      (should (equal (taco-compile-command default-directory "build"
                                           :one-step t)
                     "make -j4"))
      (should (equal (taco-get-builddir default-directory "build")
                     "/path/to/project/")))))

(ert-deftest taco-tests-make-nil-jobs ()
  (let ((taco-num-jobs nil))
    (with-fake-files '("Makefile")
      (should (equal (taco-compile-command default-directory "build")
                     "make"))
      (should (equal (taco-compile-command default-directory "build"
                                           :one-step t)
                     "make"))
      (should (equal (taco-get-builddir default-directory "build")
                     "/path/to/project/")))))

(ert-deftest taco-tests-ninja ()
  (let ((taco-num-jobs 4))
    (with-fake-files '("build.ninja")
      (should (equal (taco-compile-command default-directory "build")
                     "ninja -j4"))
      (should (equal (taco-compile-command default-directory "build"
                                           :one-step t)
                     "ninja -j4"))
      (should (equal (taco-get-builddir default-directory "build")
                     "/path/to/project/")))))

(ert-deftest taco-tests-absolute-builddir ()
  (let ((taco-num-jobs 4))
    (with-fake-files '("build.bfg")
      (should (equal (taco-compile-command default-directory "/path/to/build")
                     "9k ../build/ && cd ../build/ && ninja -j4"))
      (should (equal (taco-compile-command default-directory "/path/to/build"
                                           :one-step t)
                     "9k ../build/"))
      (should (equal (taco-get-builddir default-directory "/path/to/build")
                     "/path/to/build/")))))

;; In practice, this case would almost certainly fail (shell commands aren't
;; likely to be aware of TRAMP paths), but we test it to be sure our path
;; handling is robust.
(ert-deftest taco-tests-remote-builddir ()
  (let* ((builddir "/ssh:host:/path/to/build/")
         (escaped-builddir (taco--maybe-shell-quote-argument builddir))
         (taco-num-jobs 4))
    (with-fake-files '("build.bfg")
      (should (equal (taco-compile-command default-directory builddir)
                     (format "9k %1$s && cd %1$s && ninja -j4"
                             escaped-builddir)))
      (should (equal (taco-compile-command default-directory builddir
                                           :one-step t)
                     (format "9k %s" escaped-builddir)))
      (should (equal (taco-get-builddir default-directory builddir)
                     "/ssh:host:/path/to/build/")))))

;;; taco-tests.el ends here
