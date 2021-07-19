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

;; (require 'foo)
(require 'ido)
(require 's)
(require 'f)

;;;; Customization

;; (defgroup ccp nil
;;   "Settings for `ccp'."
;;   :link '(url-link "https://example.com/ccp.el"))

;; (defcustom ccp-something nil
;;   "This setting does something."
;;   :type 'something)

;;;; Variables

(defvar ccp-root-dir nil "ccp directory.")
(setq ccp-root-dir (file-name-directory (symbol-file 'ccp-root-dir)))

(defvar ccp-forms-dir nil "ccp directory to store pre-filled forms (defined project templates).")

(defvar ccp-gitignore-repo nil "gitignore templates directory.")
(defvar ccp-gitignores-table nil "gitignore map filename to full path.")


;;;;; Keymaps

;; This technique makes it easier and less verbose to define keymaps
;; that have many bindings.

;; (defvar ccp-map
;;   ;; This makes it easy and much less verbose to define keys
;;   (let ((map (make-sparse-keymap "ccp map"))
;;         (maps (list
;;                ;; Mappings go here, e.g.:
;;                "RET" #'ccp-RET-command
;;                [remap search-forward] #'ccp-search-forward
;;                )))
;;     (cl-loop for (key fn) on maps by #'cddr
;;              do (progn
;;                   (when (stringp key)
;;                     (setq key (kbd key)))
;;                   (define-key map key fn)))
;;     map))

;;;; Commands

;; make selections
;; (ccp-select-name)
;; (ccp-select-category)
;; (ccp-select-type)
;; build
;; (ccp-setup-git)
;; (ccp-insert-templates)
;; (ccp-init-commit)

;;;###autoload
(defun ccp-new-project (args)
  "Open new form buffer. Filling out the form defines the new project. Once satisfied run ccp-make-project to build the initial, empty project."
  (interactive)
  )

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
  "Make the new, empty, project. Writes the new directory and all intial files that were selected. initializes git and makes first commit of all said files."
  (interactive)
  )

;;;###autoload
(defun ccp-new-category (arg)
  "Define a new category "
  (interactive))


;;;###autoload
(defun ccp-select-gitignore ()
  (interactive)
  (setq ccp-select-gitignores
        (completing-read-multiple
         "add gitignore templates: " (hash-table-keys ccp-gitignores-table))))

;;;###autoload
(defun ccp-update-gitignofre-templates ()
  "Update list of gitignore templates from the gitignore repo."
  (interactive)
  (when-let* ((gidir (concat ccp-root-dir "gitignore"))
              (f-dir? gidir)
              (gifiles (directory-files-recursively gidir ".gitignore" nil t)))
    (setq ccp-gitignores-table (make-hash-table :test 'equal))
    (mapc #'(lambda (el) (puthash (f-filename el) el ccp-gitignores-table))
          gifiles)))

;;;;; Project classes

(defclass base-project ()
  ((name :initarg :name
         :initform ""
         :type string
         :documentation "Name of the project.")
   (license :initarg :license
            :initform ""
            :type string
            :documentation "Project license.")
   (category :initarg :category
             :initform ""
             :type string
             :documentation "Primary category for the project. Determines filesystem directory it goes under.")
   (plabels :initarg :labels
            :initform ()
            :type list
            :documentation "Additional labels that describe the project type.")
   (language :initarg :language
             :initform ""
             :type string
             :documentation "Primary language of the project.")
   (languages :initarg :languages
              :initform ()
              :type list
              :documentation "Other languages used in the project.")
   (builder :initarg :builder
            :initform ""
            :type string
            :documentation "Build tool (if any) used for the project.")
   (packaging :initarg :packaging
              :initform ""
              :type string
              :documentation "Package system (if any) used for the project dependencies.")
   (gitignore :initarg :gitingore
              :initform ()
              :type list
              :documentation "List of gitignore template files used to create final gitignore file.")
   (readme :initarg :readme
           :initform ""
           :type string
           :documentation "Template to use for the readme, as a string.")
   (sourcefile :initarg :sourcefile
               :initform ""
               :type string
               :documentation "Template to use for new, empty, generic source file.")
   (tests :initarg :tests
          :initform ""
          :type string
          :documentation "Template for the testing facilities for the projects.")
   (path :initarg :path
         :initform ""
         :type string
         :documentation "Filesystem path for the project.")
   :documentation "Base project class that defines common attributes for any software project. Most of these are optional, except for: name, category, language, path (auto-generated, it is not required to specify.)"))

(defclass multi-project (base-project)
  ((projects :initarg :projects
             :initform ()
             :type list
             :documentation "List of projects that belong to the main project."))
  :documentation "Base class for a multi-project. Only adds a list of child projects to the base-project class.")

(defclass )

;; (defclass docker-project (base-project)
;;   (()))

(defclass python-project (base-project)
  ((cookiecutter :initarg :cookiecutter
                 :initform ""
                 :type string
                 :documentation "Name of the cookiecutter template to use (if any).")
   (requirements :initarg :requirements
                 :type list
                 :documentation "List of packages (if any) for the requirements file.")
   (env-filename :initarg :env-filename
                 :initform ""
                 :type string
                 :documentation "Filename for the environments file (if it will be used).")
   (env-packages :initarg :env-packages
                 :initform ()
                 :type list
                 :documentation "List of packages for the environments file (if any).")
   (env-channels :initarg :env-channels
                 :initform ()
                 :type list
                 :documentation "List of environment channels to poll (if used).")))

;; (defclass elisp-project (base-project))

;; (defclass datascience-project (python-project))

;; (defclass ml-project (python-project))

;;;; Functions
;;;;; Public
;;;;; Private

;;;; Make project
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

;;;; Selections
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
