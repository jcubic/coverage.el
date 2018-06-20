;;; jest-coverage.el - display code coverage
;;
;; Filename: jest-coverage.el
;; Description: Display code coverage from jest javascript framework
;; Author: (Jakub Jankiewicz) https://jcubic.pl/me
;; Copyright (C) 2018, Jakub Jankiewicz
;; Created: Wed Jun 20 22:16:41 CEST 2018
;; Version: 0.1
;; Package-Requires: (json highlight)
;;
;;    This program is free software: you can redistribute it and/or modify
;;    it under the terms of the GNU General Public License as published by
;;    the Free Software Foundation, either version 3 of the License, or
;;    (at your option) any later version.
;;
;;    This program is distributed in the hope that it will be useful,
;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;    GNU General Public License for more details.
;;
;;    You should have received a copy of the GNU General Public License
;;    along with this program.  If not, see <http://www.gnu.org/licenses/>

(require 'json)
(require 'highlight)

(make-variable-buffer-local
   (defvar jc/statements nil "variable that contain previous coverage"))

(define-minor-mode jest-coverage-mode
  "Show code coverage from jest json file for git controled repo."
  :lighter " cov"
  (if jest-coverage-mode
      (jc/mark-buffer)
    (jc/clear-buffer)))

(defface jest-coverage-covered
    '((t (:background "dark green")))
  "background color for covered lines"
  :group 'coverage-minor-mode)

(defface jest-coverage-not-covered
    '((t (:background "dark red")))
  "background color for not covered lines"
  :group
  'coverage-minor-mode)

(defun jc/root-git-repo ()
  (interactive)
  (replace-regexp-in-string "\n" "" (shell-command-to-string "git rev-parse --show-toplevel")))


(defun jc/line-pos-at-line (line)
  (interactive)
  (save-excursion
    (goto-line line)
    (line-beginning-position)))

(defun jc/clear-buffer ()
  (interactive)
  (save-excursion
    (end-of-buffer)
    (setq jc/statements nil)
    (hlt-unhighlight-region 0 (point))))

(defun jc/mark-buffer ()
  (interactive)
  (let* ((dir (jc/root-git-repo))
         (json-object-type 'hash-table)
         (json-array-type 'list)
         (json-key-type 'string)
         (json (json-read-file (concat dir "/coverage/coverage-final.json")))
         (filename (buffer-file-name (current-buffer)))
         (coverage (gethash filename json))
         (statments (gethash "statementMap" coverage)))
    (save-excursion
      (let ((coverage-list (gethash "s" coverage))
            (covered 0)
            (not-covered 0))
        (maphash (lambda (key value)
                   (if (not (and jc/statements (= (gethash key jc/statements) value)))
                       (let* ((statment (gethash key statments))
                              (start (gethash "start" statment))
                              (end (gethash "end" statment))
                              (start-line-pos (jc/line-pos-at-line (gethash "line" start)))
                              (start-pos (+ start-line-pos (gethash "column" start)))
                              (end-line-pos (jc/line-pos-at-line (gethash "line" start)))
                              (end-pos (+ end-line-pos (gethash "column" end)))
                              (face (if (= value 0)
                                        'jest-coverage-not-covered
                                      'jest-coverage-covered)))
                         (hlt-highlight-region start-pos end-pos face)))
                   (if (= value 0)
                       (setq not-covered (+ 1 not-covered))
                     (setq covered (+ 1 covered))))
                 coverage-list)
        (message "%3.2f%% coverage" (* (/ (float covered) (+ covered not-covered)) 100))
        (setq jc/statements coverage-list)))))

(provide 'jest-coverage)
