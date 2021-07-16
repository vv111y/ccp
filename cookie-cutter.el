;;; org-projects.el --- Cookie cutter for code projects   -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Willy Rempel

;; Author: First Last <willy.rempel@gmail.com>
;; URL: https://example.com/org-projects.el
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

;; (require 'org-projects)

;;;; Usage

;; Run one of these commands:

;; `org-projects-command': Frobnicate the flange.

;;;; Tips

;; + You can customize settings in the `org-projects' group.

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

;;;; Customization

;; (defgroup org-projects nil
;;   "Settings for `org-projects'."
;;   :link '(url-link "https://example.com/org-projects.el"))

;; (defcustom org-projects-something nil
;;   "This setting does something."
;;   :type 'something)

;;;; Variables

;; (defvar org-projects-var nil
;;   "A variable.")

;;;;; Keymaps

;; This technique makes it easier and less verbose to define keymaps
;; that have many bindings.

;; (defvar org-projects-map
;;   ;; This makes it easy and much less verbose to define keys
;;   (let ((map (make-sparse-keymap "org-projects map"))
;;         (maps (list
;;                ;; Mappings go here, e.g.:
;;                "RET" #'org-projects-RET-command
;;                [remap search-forward] #'org-projects-search-forward
;;                )))
;;     (cl-loop for (key fn) on maps by #'cddr
;;              do (progn
;;                   (when (stringp key)
;;                     (setq key (kbd key)))
;;                   (define-key map key fn)))
;;     map))

;;;; Commands

;;;###autoload
(defun org-projects-command (args)
  ""
  (interactive)
  ;; make selections
  (org-projects-select-name)
  (org-projects-select-category)
  (org-projects-select-type)
  ;; build
  (org-projects-setup-git)
  (org-projects-insert-templates)
  (org-projects-init-commit)
  )

(require 'emr)
;;;; Functions

;;;;; Public


;;;;; Selections
;;;;; Hydras

(defhydra org-projects-main-hydra (:foreign-keys warn :exit t :hint nil)
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

(defun org-projects--string-input (prompt)
  "Get a string input from the user, make sure it is alphanumeric and
not empty."
  (when (stringp prompt)
    (let ((str (read-string prompt nil t)))
      (unless (or (s-blank-str-p str)
                  (s-matches? "[^a-zA-Z1-9]" str))
        str))))

;;;; Footer

(provide 'org-projects)

;;; org-projects.el ends here
