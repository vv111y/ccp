;;; ccp.el --- Cookie Cutter Projects   -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Willy Rempel

;; Author: First Last <willy.rempel@gmail.com>
;; URL: https://example.com/ccp.el
;; Version: 0.1-pre
;; Package-Requires: ((emacs "25.2"))
;; Keywords: convenience, tools,

;; This file is not part of GNU Emacs.

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

;; Auto templates for code projects, focusing on elisp and machine learning tools

;;;; Installation

;;;;; MELPA

;; If you installed from MELPA, you're done.

;;;;; Manual

;; Install these required packages:

;; + foo
;; + bar

;; Then put this file in your load-path, and put this in your init
;; file:

;; (require 'ccp)

;;;; Usage

;; Run one of these commands:

;; `ccp-command': Frobnicate the flange.

;;;; Tips

;; + You can customize settings in the `ccp' group.

;;;; Credits

;; This package would not have been possible without the following
;; packages: foo[1], which showed me how to bifurcate, and bar[2],
;; which takes care of flanges.
;;
;;  [1] https://example.com/foo.el
;;  [2] https://example.com/bar.el

;;; Code:

;;;; Requirements

(require 'ido)
(require 's)
(require 'f)

;;;; Variables

(defvar ccp-root-dir nil "ccp directory.")

(defvar ccp-forms-dir nil "ccp directory to store pre-filled forms (defined project templates).")

(cl-defstruct (ccp-spec (:constructor ccp-spec-create)
                        (:copier nil))
  "A single or group project specification. Certain specification values
maybe a specification group itself, thus we have recursive and heirarchical specifications.
name - name of spec
key - hotkey (used by hydra)
type - primitive types, spec-group, etc
description - brief description of spec
default - default value if any
vals - list of allowed values, if applicable
selection - slot used to store user selection/input
action - function associated with this spec. Applied to the project when building.
parent - any parent spec
required - list of children specs that are necessary for this spec
optional - list of optional children specs
Required slots are: name, key, and type. The rest are optional."
  name key type description default vals selection action parent required optional)

;; (defvar ccp-default-project nil "User defined list of default components for projects.")
;; project meta templates - predefined, partly filled ccp projects
;; also templates - predefined material from other sources added 

;;;; Commands


;;;; Spec: manage

;; TODO 1st. make spec code
;;;###autoload
(defmacro cpp-define-spec (short-name &keys)
  "Define a spec. Short-name is the atomic reference to this spec used by ccp. "
  ;; hydra entry
  ;; ui form
  ;; data struct
  )


;;;###autoload
(defun ccp-new-project ()
  ;; This should be a single work to uniquely identify this spec for a particular project. The rest of the keyed arguments are used for the spec struct.
  (interactive)
  )

;;;###autoload
(defun cpp-add-spec (spec child)
  "Add CHILD specification to parent SPEC. CHILD is appended to the end of the OPTIONAL list."
  ())

;;;###autoload
(defun cpp-remove-spec (spec child)
  )



;;;; UI
;;;;; UI: Selections

;; (ccp-select-name)
;; (ccp-select-category)
;; (ccp-select-type)

;;;;; Hydras

(defhydra ccp-main-hydra (:foreign-keys warn :exit t :hint nil)
  "
Main menu

_r_: Run Pandoc               _o_: Options
_p_: Convert to pdf           _I_: Input format
_v_: View output file         _O_: Output format
_V_: View output buffer       _s_: Settings files
_S_: View current settings    _e_: Example lists
_L_: View log buffer

"

  ("q" nil "Quit"))

(defhydra pandoc-main-hydra (:foreign-keys warn :exit t :hint nil)
  ("r" pandoc-run-pandoc)
  ("p" pandoc-convert-to-pdf)
  ("v" pandoc-view-output)
  ("V" pandoc-view-output-buffer)
  ("S" pandoc-view-settings)
  ("L" pandoc-view-log)
  ("o" pandoc-options-hydra/body)
  ("I" pandoc-input-format-hydra/body)
  ("O" pandoc-output-format-hydra/body)
  ("s" pandoc-settings-file-hydra/body)
  ("e" pandoc-@-hydra/body)
  ("q" nil "Quit"))

(defhydra pandoc-options-hydra (:foreign-keys warn :exit t :hint nil)
  "
Options menu

_f_: Files
_r_: Reader options
_w_: General writer options
_s_: Options for specific writers
_c_: Citations
_m_: Math rendering

"
  ("f" pandoc-file-hydra/body)
  ("r" pandoc-reader-options-hydra/body)
  ("w" pandoc-writer-options-hydra/body)
  ("s" pandoc-specific-options-hydra/body)
  ("c" pandoc-citations-hydra/body)
  ("m" pandoc-math-hydra/body)
  ("q" nil "Quit")
  ("b" pandoc-main-hydra/body "Back"))

(define-pandoc-hydra pandoc-reader-options-hydra (:foreign-keys warn :hint nil)
  (concat "Reader options"
          "\n\n"
          (mapconcat #'car pandoc--reader-hydra-list "\n")
          "\n\n")
  (mapcar #'cdr pandoc--reader-hydra-list)
  ("q" nil "Quit")
  ("b" pandoc-options-hydra/body "Back" :exit t))

;;;;; selection fns

(defun ccp--string-input (prompt)
  "Get a string input from the user, make sure it is alphanumeric and
not empty."
  (when (stringp prompt)
    (let ((str (read-string prompt nil t)))
      (unless (or (s-blank-str-p str)
                  (s-matches? "[^a-zA-Z1-9]" str))
        str))))

;;;; Project: Do
;;;;; File utils
(defun ccp--make-dir ()
  "Create project directory in correct location and cd into it."
  )

(defun ccp--merge-files (filename files)
          (with-temp-file filename
            (insert (ccp--concat-files files))))

(defun ccp--concat-files (files)
  (mapconcat #'(lambda (f)
                 (concat "#####  " (file-name-base f) " #####"
                         "\n" (f-read-text f) "\n\n"))
             files "\n"))

;;;;; External Templates Specs

;; (ccp-insert-templates)

;;;###autoload
(defun ccp-new-project ()
  "Open new form buffer. Filling out the form defines the new project. Once satisfied run ccp-make-project to build the initial, empty project."
  (interactive)
  (when (bound-and-true-p cpp-current-project)
    (push cpp-current-project cpp-open-projects))
  (setq cpp-current-project (cpp-new-base)))

;;;###autoload
(defun ccp-save-project-form (arg)
  "If the selections define a common project that you work on, you can save these selections and load them into the form later. Only the project name will not be saved, as that is the sole identifier for all projects."
  (interactive (list (read-file-name "Save project template: " ccp-forms-dir)))
  )

;;;###autoload
(defun ccp-load-project-form (arg)
  "Load a previously saved filled-out form."
  (interactive (list (read-file-name "Load project template: " ccp-forms-dir)))
  )

;;;###autoload
(defun ccp-make-project (arg)
  "Make the new, empty, project. Writes the new directory and all intial files that were selected."
  (interactive)
  )

;;;###autoload
(defun ccp-new-category (arg)
  "Define a new category "
  (interactive))


;;; Footer
;;;; Load ccp maybe?

(defun ccp-load ()
  "Init ccp default dir and get initial templates and necessary files."
  (if-let ((root-dir (bound-and-true-p ccp-root-dir)))
      (message "ccp root directory: %s" root-dir)
    (message "ccp root directory not found. Please specify variable and download necessary templates according to documentation.")
    (setq ccp-root-dir (file-name-directory (symbol-file 'ccp-root-dir)))))

(ccp-load)

(provide 'ccp)

;;; ccp.el ends here
