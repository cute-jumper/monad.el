;;; monad.el --- Elisp implementation of monads      -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Junpeng Qiu

;; Author: Junpeng Qiu <qjpchmail@gmail.com>
;; Keywords: extensions

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

;;

;;; Code:

(require 'cl-lib)

;; ----------- ;;
;; Maybe monad ;;
;; ----------- ;;
(defun Just (x) (list 'Just x))

(defvar Nothing '(Nothing))

(defalias 'maybe-return 'Just)

(defun maybe-bind (m f)
  (pcase m
    (`(Just ,x) (funcall f x))
    (_ Nothing)))

(defun maybe-then (m1 m2)
  (maybe-bind m1 (lambda (_) m2)))

(defun maybe-join (m)
  (maybe-bind m 'identity))

(maybe-join (Just (Just 4)))

(defmacro monad-dispatch (suffix type &rest args)
  (declare (indent 2))
  (let ((func-name (intern (format "%s-%s" type suffix))))
    `(,func-name ,@args)))

(defmacro monad-do (type &rest body)
  (declare (indent 1))
  (let* ((reverse-body (nreverse body))
         (macro-body `(monad-dispatch return ,type
                        ,(pop reverse-body)))
         curr-sexp)
    (while (setq curr-sexp (pop reverse-body))
      (setq macro-body
            `(monad-dispatch bind ,type
               ,@(cdr curr-sexp)
               (lambda (,(car curr-sexp)) ,macro-body))))
    macro-body))

;; ----- ;;
;; tests ;;
;; ----- ;;
(monad-do maybe
  (x (Just 3))
  (y (Just 4))
  (_ Nothing)
  (+ x y))

(maybe-bind '(Just 2) (lambda (x) (Just (+ x 1))))

(provide 'monad)
;;; monad.el ends here
