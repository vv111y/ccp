;;; workfile.el --- scratch                          -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Will Rempel

;; Author: Will Rempel <Will@WillMBA.local>
;; Keywords: internal,


;;; Project classes

;; (defclass base-project ()
;;   ((name :initarg :name
;;          :initform ""
;;          :type string
;;          :documentation "Name of the project.")
;;    (license :initarg :license
;;             :initform ""
;;             :type string
;;             :documentation "Project license.")
;;    (category :initarg :category
;;              :initform ""
;;              :type string
;;              :documentation "Primary category for the project. Determines filesystem directory it goes under.")
;;    (plabels :initarg :labels
;;             :initform ()
;;             :type list
;;             :documentation "Additional labels that describe the project type.")
;;    (language :initarg :language
;;              :initform ""
;;              :type string
;;              :documentation "Primary language of the project.")
;;    (languages :initarg :languages
;;               :initform ()
;;               :type list
;;               :documentation "Other languages used in the project.")
;;    (builder :initarg :builder
;;             :initform ""
;;             :type string
;;             :documentation "Build tool (if any) used for the project.")
;;    (packaging :initarg :packaging
;;               :initform ""
;;               :type string
;;               :documentation "Package system (if any) used for the project dependencies.")
;;    (gitignore :initarg :gitingore
;;               :initform ()
;;               :type list
;;               :documentation "List of gitignore template files used to create final gitignore file.")
;;    (readme :initarg :readme
;;            :initform ""
;;            :type string
;;            :documentation "Template to use for the readme, as a string.")
;;    (sourcefile :initarg :sourcefile
;;                :initform ""
;;                :type string
;;                :documentation "Template to use for new, empty, generic source file.")
;;    (tests :initarg :tests
;;           :initform ""
;;           :type string
;;           :documentation "Template for the testing facilities for the projects.")
;;    (path :initarg :path
;;          :initform ""
;;          :type string
;;          :documentation "Filesystem path for the project.")
;;    :documentation "Base project class that defines common attributes for any software project. Most of these are optional, except for: name, category, language, path (auto-generated, it is not required to specify.)"))

;; (defclass multi-project (base-project)
;;   ((projects :initarg :projects
;;              :initform ()
;;              :type list
;;              :documentation "List of projects that belong to the main project."))
;;   :documentation "Base class for a multi-project. Only adds a list of child projects to the base-project class.")

;; TODO no? perhaps these should be structs instead
;; (defclass docker-project (base-project)
;;   (()))

;; (defclass python-project (base-project)
;;   ((cookiecutter :initarg :cookiecutter
;;                  :initform ""
;;                  :type string
;;                  :documentation "Name of the cookiecutter template to use (if any).")
;;    (requirements :initarg :requirements
;;                  :type list
;;                  :documentation "List of packages (if any) for the requirements file.")
;;    (env-filename :initarg :env-filename
;;                  :initform ""
;;                  :type string
;;                  :documentation "Filename for the environments file (if it will be used).")
;;    (env-packages :initarg :env-packages
;;                  :initform ()
;;                  :type list
;;                  :documentation "List of packages for the environments file (if any).")
;;    (env-channels :initarg :env-channels
;;                  :initform ()
;;                  :type list
;;                  :documentation "List of environment channels to poll (if used).")))

;; (defclass elisp-project (base-project))

;; (defclass datascience-project (python-project))

;; (defclass ml-project (python-project))

;;; [2021-07-22 Thu]

(symbolp (nth 0 (ccp-spec-children cpp-current-project)))
(ccp-spec-parent cpp-current-project)
(setf (ccp-spec-parent cpp-current-project) 'root)
(push 'boo (nreverse (ccp-spec-children cpp-current-project)))
(setq vv '(1 2 4 5 6 8 9))
(setq vv (-insert-at most-positive-fixnum 'boo vv))

;;; types

(type-of )
(let ((b '()))
  (type-of b))

(cond (()))
(listp nil)
(pus nil "bee")

;;; hydra
(define-pandoc-hydra ccp-test-hydra nil 
"
Main menu

_r_: Run Pandoc               _o_: Options
_p_: Convert to pdf           _I_: Input format"

'(("r" nil "qeet"))
  ;; (lambda () ('("r" nil "qeet")))
  ("q" nil "Quit")
  ("b" pandoc-options-hydra/body "Back" :exit t)
  )
(ccp-test-hydra/body)
