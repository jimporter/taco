;;; taco.el --- Tool-aware compilation commands -*- lexical-binding: t -*-

;; Copyright (C) 2021 Jim Porter

;; Author: Jim Porter
;; URL: https://github.com/jimporter/taco
;; Version: 0.1-dev
;; Keywords:
;; Package-Requires: ((emacs "27.1") (project "0.2.0"))

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

;; Inspect a project's files to provide intelligent guesses for the default
;; compilation commands.

;;; Code:

(require 'cl-lib)
(require 'compile)
(require 'project)
(require 'subr-x)

(defgroup taco nil
  "Provide intelligent compilation defaults based on a project's build files."
  :group 'tools
  :group 'processes)

(defcustom taco-builddir-name "build"
  "The default directory to use for builds, relative to the source directory."
  :type 'string)

(defcustom taco-one-step nil
  "If non-nil, only provide a default command for a single step at a time."
  :type 'boolean)


;; Taco tools

(defvar taco-tools
  '((ninja
     (build-step . build)
     (project-file . "build.ninja")
     (working-directory builddir)
     (command "ninja"))
    (make
     (build-step . build)
     (project-file . "Makefile")
     (working-directory builddir)
     (command "make"))
    (bfg9000
     (build-step . configure)
     (project-file . "build.bfg")
     (working-directory srcdir)
     (command "9k" builddir)
     (next-step build ninja))
    (configure
     (build-step . configure)
     (project-file . "configure")
     (working-directory builddir t)
     (command project-file)
     (next-step build make))
    (automake
     (build-step . preconfigure)
     (project-file . "configure.ac")
     (working-directory srcdir)
     (command "autoreconf")
     (next-step configure configure)))
  "An alist of known tools to use when compiling projects.")

;; XXX: It would be nice to let a tool have multiple project files.
(defun taco--project-file (tool directory)
  "Get the path to the project file for TOOL in DIRECTORY."
  (let ((project-file (alist-get 'project-file (cdr tool))))
    (expand-file-name project-file directory)))

(defun taco--find-tool (directory &optional step guess)
  "Find the best compilation tool in DIRECTORY for STEP.
If the tool's project file is found in DIRECTORY, return that tool.
Otherwise, if GUESS is non-nil, use that as the tool's key."
  (or (cl-dolist (tool taco-tools)
        (let* ((build-step (alist-get 'build-step (cdr tool)))
               (project-file (taco--project-file tool directory)))
          (when (and (or (not step) (eq step build-step))
                     (file-exists-p project-file))
            (cl-return (list tool project-file nil)))))
      (if-let* ((guess)
                (tool (assq guess taco-tools))
                (project-file (taco--project-file tool directory)))
        (list tool project-file t))))

(defun taco--maybe-shell-quote-argument (argument)
  "Quote ARGUMENT if needed for passing to an inferior shell.
This works as `shell-quote-argument', but avoids quoting unnecessarily
for MS shells."
  (if (and (or (eq system-type 'ms-dos)
               (and (eq system-type 'windows-nt) (w32-shell-dos-semantics)))
           (not (string-match "[^-0-9a-zA-Z_./=]" argument)))
      argument
    (shell-quote-argument argument)))


;; User-facing Taco functions/commands

(eval-when-compile
  (if (fboundp 'project-root)
      (defsubst taco--project-root (project) (project-root project))
    (defsubst taco--project-root (project) (car (project-roots project)))))

;;;###autoload
(defun taco-get-builddir (srcdir &optional builddir-name)
  "Get the build directory associated with SRCDIR.
BUILDDIR-NAME, if non-nil, represents the path fragment to append to
the source directory to make the build dir.  If nil, use the default
value specified in `taco-builddir-name'."
  (let* ((srcdir (file-name-as-directory (expand-file-name srcdir)))
         (builddir-name (or builddir-name taco-builddir-name))
         (builddir srcdir)
         (next-step))
    (cl-loop
     (pcase-let ((`(,tool ,_project-file ,_guessed-tool)
                  (apply #'taco--find-tool builddir next-step)))
       (unless tool (cl-return))
       ;; Get the next step information; if it's a build step, then set the
       ;; builddir up.  This covers the usual configure-then-build process where
       ;; the actual build files end up in a builddir.
       (setq next-step (alist-get 'next-step (cdr tool)))
       (when (eq (car next-step) 'build)
         (setq builddir (expand-file-name (file-name-as-directory builddir-name)
                                          srcdir))
         (cl-return))
       ;; If there's no next step, we're done.
       (unless next-step (cl-return))))
    builddir))

;;;###autoload
(defun taco-project-get-builddir (project &optional builddir-name)
  "Get the build directory associated with PROJECT.
BUILDDIR-NAME, if non-nil, represents the path fragment to append to
the source directory to make the build dir.  If nil, use the default
value specified in `taco-builddir-name'."
  (taco-get-builddir (taco--project-root project) builddir-name))

;;;###autoload
(cl-defun taco-compile-command (srcdir builddir-name &key one-step
                                       trailing-space)
  "Return the default compile command for the code in SRCDIR.
BUILDDIR-NAME represents the path fragment to append to the source
directory to make the build dir.

ONE-STEP, if non-nil, results in only a single command being returned;
otherwise, all (known) commands to build the code will be returned.

TRAILING-SPACE, if non-nil, appends an extra space character to the
end of the command in order to make it easier to add additional
arguments."
  (let* ((srcdir (file-name-as-directory (expand-file-name srcdir)))
         (builddir srcdir)
         (cwd srcdir)
         (next-step)
         (commands))
    (cl-loop
     (pcase-let ((`(,tool ,project-file ,guessed-tool)
                  (apply #'taco--find-tool builddir next-step)))
       (unless tool (cl-return))
       ;; If previous steps have never been executed, we had to guess the tool
       ;; to use for this step.  For the non-guessed case, we can remove the
       ;; previous commands, since they've been run already.
       (if guessed-tool
           (when one-step (cl-return))
         (setq commands nil
               cwd srcdir))
       ;; Get the next step information; if it's a build step, then set the
       ;; builddir up.  This covers the usual configure-then-build process where
       ;; the actual build files end up in a builddir.
       (setq next-step (alist-get 'next-step (cdr tool)))
       (when (eq (car next-step) 'build)
         (setq builddir (expand-file-name (file-name-as-directory builddir-name)
                                          srcdir)))
       ;; Generate the commands to be executed for this step.
       (pcase-let* ((next-dir (alist-get 'working-directory (cdr tool)))
                    (`(,next-dir ,make-next-dir) next-dir)
                    (next-dir (pcase next-dir
                                ('srcdir srcdir)
                                ('builddir builddir)))
                    (arguments (alist-get 'command (cdr tool))))
         ;; Change directory if necessary.
         (unless (string= cwd next-dir)
           (let ((directory (taco--maybe-shell-quote-argument
                             (file-relative-name next-dir cwd))))
             (when make-next-dir
               (push (format "mkdir %s" directory) commands))
             (push (format "cd %s" directory) commands))
           (setq cwd next-dir))
         ;; Fill in command argument placeholders.
         (dolist (i `((project-file . ,(file-relative-name project-file cwd))
                      (srcdir       . ,(file-relative-name srcdir cwd))
                      (builddir     . ,(file-relative-name builddir cwd))))
           (setq arguments (cl-substitute (cdr i) (car i) arguments)))
         (push (mapconcat #'taco--maybe-shell-quote-argument arguments " ")
               commands))
       ;; If there's no next step, we're done.
       (unless next-step (cl-return))))
    (concat (mapconcat #'identity (reverse commands) " && ")
            (when trailing-space " "))))

(defun taco--compile (directory builddir-name one-step interactive)
  "Invoke `compile' in DIRECTORY with the default commands.
BUILDDIR-NAME, if non-nil, represents the path fragment to append to
the source directory to make the build dir.

ONE-STEP, if non-nil, results in only a single command being returned;
otherwise, all (known) commands to build the code will be returned.

If INTERACTIVE is non-nil, call `compile' interactively."
  (let* ((default-directory directory)
         (compile-command (taco-compile-command directory builddir-name
                           :one-step one-step :trailing-space interactive)))
    (if interactive
        (call-interactively #'compile)
      (compile compile-command))))

(defun taco--read-build-directory (arg)
  "Get the build directory name for the compilation commands.
If ARG is non-nil, prompt the user for the directory; otherwise, use
the default value in `taco-builddir-name'."
  (if arg
      (read-directory-name "Build directory name: "
                           (expand-file-name taco-builddir-name))
    taco-builddir-name))

;;;###autoload
(defun taco-compile (directory &optional builddir-name interactive)
  "Compile the code in DIRECTORY using the default command.
BUILDDIR-NAME, if non-nil, represents the path fragment to append to
the source directory to make the build dir.  If nil, use the default
value specified in `taco-builddir-name'.  INTERACTIVE, if non-nil,
specifies that `compile' should be called interactively."
  (interactive
   (list default-directory
         (taco--read-build-directory current-prefix-arg)
         t))
  (taco--compile directory (or builddir-name taco-builddir-name) taco-one-step
                 interactive))

;;;###autoload
(defun taco-project-compile (project &optional builddir-name interactive)
  "Compile the code in PROJECT using the default command.
BUILDDIR-NAME, if non-nil, represents the path fragment to append to
the source directory to make the build dir.  If nil, use the default
value specified in `taco-builddir-name'.  INTERACTIVE, if non-nil,
specifies that `compile' should be called interactively."
  (interactive
   (list (project-current t)
         (taco--read-build-directory current-prefix-arg)
         t))
  (let ((directory (expand-file-name (taco--project-root project))))
    (taco--compile directory (or builddir-name taco-builddir-name) taco-one-step
                   interactive)))

(provide 'taco)
;;; taco.el ends here
