;;; coverage.el - display code coverage (supports phpunit and JavaScript jest)
;;
;; Filename: coverage.el
;; Description: Display code coverage from jest javascript framework or phpunit
;; Author: (Jakub T. Jankiewicz) https://jcubic.pl/me
;; Copyright (C) 2018-2019, Jakub Jankiewicz
;; Created: Wed Jun 20 22:16:41 CEST 2018
;; Version: 0.2
;; Package-Requires: (json highlight xml)
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
(require 'xml)

(make-variable-buffer-local
   (defvar jc/statements nil "variable that contain previous statement coverage"))

(make-variable-buffer-local
   (defvar jc/branches nil "variable that contain previous branch coverage"))

(define-minor-mode coverage-mode
  "Show code coverage from jest json file for git controled repo."
  :lighter " cov"
  (if coverage-mode
      (jc/mark-buffer)
    (jc/clear-buffer)))


(defface jc/covered
    '((t :background "dark green"))
  "background color for covered lines"
  :group 'coverage-minor-mode)

(defface jc/not-covered
    '((t :background "dark red"))
  "background color for not covered lines"
  :group
  'coverage-minor-mode)

(defun jc/shell-line (command)
  (replace-regexp-in-string "\n" "" (shell-command-to-string command)))

(defun jc/root-git-repo ()
  (interactive)
  (jc/shell-line "git rev-parse --show-toplevel"))

(defun jc/real-filename (filename)
  (jc/shell-line (concat "readlink -f " filename)))

(defun jc/line-pos-at-line (line)
  (interactive)
  (save-excursion
    (goto-line line)
    (line-beginning-position)))

(defun jc/end-pos-at-line (line)
  (interactive)
  (save-excursion
    (goto-line line)
    (line-end-position)))

(defun jc/clear-buffer ()
  (interactive)
  (save-excursion
    (end-of-buffer)
    (setq jc/statements nil)
    (hlt-unhighlight-region 0 (point))))


(defun jc/get-coverage (coverage-fname filename)
  (let* ((xml (jc/parse-xml coverage-fname))
         (coverage (assq 'coverage xml))
         (pkg (cadr (xml-node-children coverage)))
         (files (xml-node-children (cadr (xml-node-children pkg))))
         (result))
    (dolist (file files result)
      (if (and (listp file) (string= filename (xml-get-attribute file 'name)))
          (setq result file)))))

(defun jc/mark-buffer-php ()
  (interactive)
  (let* ((dir (jc/root-git-repo))
         (filename (jc/real-filename (buffer-file-name)))
         (coverage-fname (concat dir "/build/logs/clover.xml"))
         (coverage (jc/get-coverage coverage-fname filename)))
    (save-excursion
      (let ((covered 0)
            (not-covered 0))
        (dolist (line (xml-get-children coverage 'line))
          (if (string= (xml-get-attribute line 'type) "stmt")
              (let* ((num (string-to-number (xml-get-attribute line 'num)))
                     (count (string-to-number (xml-get-attribute line 'count)))
                     (start-pos (jc/line-pos-at-line num))
                     (end-pos (jc/end-pos-at-line num))
                     (face (if (= count 0)
                               'jc/not-covered
                             'jc/covered)))
                (hlt-highlight-region start-pos end-pos face)
                (if (= count 0)
                    (setq not-covered (+ 1 not-covered))
                  (setq covered (+ 1 covered))))))
        (message "%3.2f%% coverage" (* (/ (float covered) (+ covered not-covered)) 100))))))

(defun jc/mark-buffer ()
  (let ((ext (file-name-extension (buffer-file-name))))
    (cond ((string= "php" ext) (jc/mark-buffer-php))
          ((or (string= "js" ext) (string= "ts" ext)) (jc/mark-buffer-jest))
          (t (throw 'jest "invalid filename")))))

(defun jc/parse-xml (fname)
  (with-temp-buffer
    (insert-file-contents fname)
    (xml-parse-region (point-min) (point-max))))


(defun jc/mark-buffer-jest ()
  (interactive)
  (message "jc/mark-buffer-jest")
  (let ((coverage (jc/get-jest-coverage)))
    (if (not (hash-table-p coverage))
        (message "file coverage not found")
      (let ((statments (gethash "statementMap" coverage)))
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
                                  (end-line (gethash "line" end))
                                  (end-line-pos (jc/line-pos-at-line end-line))
                                  (end-column (gethash "column" end))
                                  (end-pos (if end-column
                                               (+ end-line-pos end-column)
                                             (jc/line-pos-at-line (+ end-line 1))))
                                  (face (if (= value 0)
                                            'jc/not-covered
                                          'jc/covered)))
                             (message "[%s:%s] %s:%s -> [%s:%s] %s:%s"
                                      (gethash "line" start)
                                      (gethash "column" start)
                                      start-line-pos start-pos
                                      (gethash "line" end)
                                      (gethash "column" end)
                                      end-line-pos end-pos)
                             (hlt-highlight-region start-pos end-pos face)))
                       (if (= value 0)
                           (setq not-covered (+ 1 not-covered))
                         (setq covered (+ 1 covered))))
                     coverage-list)
            (message "%3.2f%% coverage" (* (/ (float covered) (+ covered not-covered)) 100))
            (setq jc/statements coverage-list)))))))

(defun jc/get-jest-coverage ()
  (interactive)
  (let* ((dir (jc/root-git-repo))
         (json-object-type 'hash-table)
         (json-array-type 'list)
         (json-key-type 'string)
         (coverage-fname (concat dir "/coverage/coverage-final.json")))
    (if (not (file-exists-p coverage-fname))
        (message "file coverage not found")
      (let* ((json (json-read-file coverage-fname))
             (filename (jc/real-filename (buffer-file-name (current-buffer))))
             (coverage (gethash filename json)))
        coverage))))

(defun jc/mark-buffer-branch-jest ()
  (interactive)
  (let ((coverage (jc/get-jest-coverage)))
    (if (not (hash-table-p coverage))
        (message "No coverage found for this file")
      (let ((branch (gethash "branchMap" coverage)))
        (save-excursion
          (let ((coverage-list (gethash "b" coverage))
                (covered 0)
                (not-covered 0))
            (maphash (lambda (key value)
                       (if (not (and jc/statements (= (gethash key jc/statements) value)))
                           (let* ((statment (gethash key statments))
                                  (start (gethash "start" statment))
                                  (end (gethash "end" statment))
                                  (start-line-pos (jc/line-pos-at-line (gethash "line" start)))
                                  (start-pos (+ start-line-pos (gethash "column" start)))
                                  (end-line-pos (jc/line-pos-at-line (gethash "line" end)))
                                  (end-pos (+ end-line-pos (gethash "column" end)))
                                  (face (if (= value 0)
                                            'jc/not-covered
                                          'jc/covered)))
                             (hlt-highlight-region start-pos end-pos face)))
                       (if (= value 0)
                           (setq not-covered (+ 1 not-covered))
                         (setq covered (+ 1 covered))))
                     coverage-list)
            (message "%3.2f%% coverage" (* (/ (float covered) (+ covered not-covered)) 100))
            (setq jc/statements coverage-list)))))))

(provide 'coverage)
