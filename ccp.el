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

;; (defvar ccp-gitignore-repo nil "gitignore templates directory.")
;; (defvar ccp-gitignores-table nil "gitignore map filename to full path.")


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

;;;###autoload
(defun ccp-update-templates ()
  (interactive)
  (when-let* ((gidir (concat ccp-root-dir "gitignore"))
              (f-dir? gidir)
              (gifiles (directory-files-recursively gidir ".gitignore" nil t)))
    (setq ccp-gitignores-table (make-hash-table :test 'equal))
    (mapc #'(lambda (el) (puthash (f-filename el) el ccp-gitignores-table))
          gifiles)))

;;;###autoload
(defun ccp-new-project (args)
  ""
  (interactive)
  ;; make selections
  (ccp-select-name)
  (ccp-select-category)
  (ccp-select-type)
  ;; build
  (ccp-setup-git)
  (ccp-insert-templates)
  (ccp-init-commit)
  )

(defun ccp-select-gitignore ()
  (interactive)
  (helm :buffer "ccp gitignore"
        :sources
        (helm-build-sync-source "ccp-gitignores"
          :candidates (lambda () (let ((li (hash-table-keys ccp-gitignores-table)))
                                   (cons helm-input li)))
          :resume t
          :action
          (lambda (c)
            (ccp--merge-files "test.txt"
                              (mapcar (lambda (el) (gethash el ccp-gitignores-table))
                                      (helm-marked-candidates))))))

;;;; Functions

;;;;; Public


;;;;; Private

        ;; (defun ccp--read-)

        (defun ccp--merge-files (filename files)
          (with-temp-file filename
            (insert (ccp--concat-files files)))))

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

;;;; Footer

(provide 'ccp)

;;; ccp.el ends here
