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

(defmacro monad-dispatch (suffix type)
  `(intern (format "%s-%s" ,type ,suffix)))

(defun monad-normalize-return (sexp return-func)
  (if (consp sexp)
      (if (eq (car sexp) 'return)
          `(funcall ',return-func ,@(cdr sexp))
        (mapcar
         (lambda (s)
           (monad-normalize-return s return-func))
         sexp))
    sexp))

(defmacro monad-do (type &rest body)
  (declare (indent 1))
  (let* ((reverse-body (nreverse body))
         (return-func  (monad-dispatch 'return type))
         (bind-func (monad-dispatch 'bind type))
         (macro-body (monad-normalize-return
                      (pop reverse-body) return-func))
         curr-sexp)
    (while (setq curr-sexp (pop reverse-body))
      (when (eq (length curr-sexp) 1)
        (setq curr-sexp (cons '_ curr-sexp)))
      (setq macro-body
            `(funcall ',bind-func
                      ,(monad-normalize-return (cadr curr-sexp) return-func)
                      (lambda (,(car curr-sexp)) ,macro-body))))
    macro-body))

;; ----------- ;;
;; Maybe monad ;;
;; ----------- ;;
(defun Just (x) (cons 'Just x))

(defvar Nothing '(Nothing))

(defalias 'Maybe-return 'Just)

(defun Maybe-bind (m f)
  (pcase m
    (`(Just . ,x) (funcall f x))
    (_ Nothing)))

(defun Maybe-then (m1 m2)
  (Maybe-bind m1 (lambda (_) m2)))

(defun Maybe-join (m)
  (Maybe-bind m 'identity))

;; ----- ;;
;; tests ;;
;; ----- ;;
(monad-do Maybe
  (x (Just 3))
  (y (Just 4))
  ((return 10))
  (return (+ x y)))

(monad-do Maybe
  (x (save-excursion
       (return 4)))
  (y (if (= x 4)
         (return 5)
       (return -4)))
  (return (+ x y)))

(Maybe-bind (Just 2) (lambda (x) (Just (+ x 1))))

(Maybe-join (Just (Just 4)))

;; ---------- ;;
;; List monad ;;
;; ---------- ;;
(defalias 'List-return 'list)

;; ----------- ;;
;; State monad ;;
;; ----------- ;;

(defun State (f)
  (cons 'State f))

(defun State-run (state &rest args)
  (apply (cdr state) args))

(defun State-return (x)
  (State (lambda (s) (list x s))))

(defun State-bind (m f)
  (State (lambda (s)
           (let ((pair (State-run m s)))
             (State-run (funcall f (car pair)) (nth 1 pair))))))

(defun State-get ()
  (State (lambda (s) (list s s))))

(defun State-put (s)
  (State (lambda (_) (list nil s))))

;; ----- ;;
;; tests ;;
;; ----- ;;
(defun stack-pop ()
  (State (lambda (s) (list (car s) (cdr s)))))

(defun stack-push (a)
  (State (lambda (s) (list nil (cons a s)))))

(State-run
 (monad-do State
   (x (State-get))
   (y (stack-pop))
   (z (stack-pop))
   ((if (= (length x) 3)
        (State-put '(200))
      (State-put '(100))))
   ((stack-push y))
   (_ (stack-push z))
   (return x))
 '(8 9 10))

(State-run (State-bind (State (lambda (s) (list 0 (+ 10 s))))
                       (lambda (a) (State (lambda (s) (list a (* 11 s)))))) 1)

(provide 'monad)
;;; monad.el ends here
