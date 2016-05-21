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

(defvar Nothing 'Nothing)

(defalias 'Maybe-return 'Just)

(defun Maybe-bind (m f)
  (pcase m
    (`(Just . ,x) (funcall f x))
    (_ Nothing)))

(defun Maybe-then (m1 m2)
  (Maybe-bind m1 (lambda (_) m2)))

(defun Maybe-join (m)
  (Maybe-bind m 'identity))

(defmacro Maybe<- (&rest body)
  `(let ((res (progn
                ,@body)))
     (if res
         (Just res)
       Nothing)))

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
         Nothing
       (return -4)))
  (return (+ x y)))

(monad-do Maybe
  (beg (Maybe<-
        (save-excursion
          (goto-char (point-min))
          (re-search-forward ";;; Commentary:$" nil t))))
  (end (Maybe<-
        (save-excursion
          (goto-char (point-min))
          (re-search-forward ";;; Code:$" nil t))))
  (return (cons beg end)))

(Maybe-bind (Just 2) (lambda (x) (Just (+ x 1))))

(Maybe-join (Just (Just 4)))

;; ---------- ;;
;; List monad ;;
;; ---------- ;;
(defalias 'List 'list)

(defalias 'List-return 'list)

(defun List-bind (m f)
  (apply #'append (mapcar f m)))

;; ----- ;;
;; tests ;;
;; ----- ;;

(monad-do List
  (x (List 2 3 4))
  (y (List 5 6 7))
  (return (cons x y)))

;; ------------ ;;
;; Reader monad ;;
;; ------------ ;;

(defalias 'Reader-run 'funcall)

(defun Reader-return (x)
  (lambda (_) x))

(defun Reader-bind (m f)
  (lambda (x) (Reader-run (funcall f (Reader-run m x)) x)))

;; ----- ;;
;; tests ;;
;; ----- ;;

(Reader-run
 (monad-do Reader
   (x (apply-partially '* 2))
   (y (apply-partially '+ 10))
   (return (+ x y)))
 3)

;; ------------ ;;
;; Writer monad ;;
;; ------------ ;;

(defun Writer (a w)
  (cons 'Writer (cons a w)))

(defalias 'Writer-run 'cdr)

(defun Writer-return (x)
  (Writer x nil))

;; TODO: more monoid type
(defun Monoid-append (x y)
  (cond
   ((stringp x) (concat x y))
   ((listp x) (append x y))))

(defun Writer-bind (m f)
  (let* ((res1 (Writer-run m))
         (res2 (Writer-run (funcall f (car res1)))))
    (Writer (car res2) (Monoid-append (cdr res1) (cdr res2)))))

;; ----- ;;
;; tests ;;
;; ----- ;;

(defun log-number (x)
  (Writer x (list (format "Got number: %s" x))))

(monad-do Writer
  (x (log-number 3))
  (y (log-number 5))
  (return (* x y)))

;; ----------- ;;
;; State monad ;;
;; ----------- ;;

(defun State (f)
  (cons 'State f))

(defun State-run (state &rest args)
  (apply (cdr state) args))

(defun State-return (x)
  (State (lambda (s) (cons x s))))

(defun State-bind (m f)
  (State (lambda (s)
           (let ((pair (State-run m s)))
             (State-run (funcall f (car pair)) (cdr pair))))))

(defun State-get ()
  (State (lambda (s) (cons s s))))

(defun State-put (s)
  (State (lambda (_) (cons nil s))))

;; ----- ;;
;; tests ;;
;; ----- ;;
(defun stack-pop ()
  (State #'identity))

(defun stack-push (a)
  (State (lambda (s) (cons nil (cons a s)))))

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

(State-run (State-bind (State (lambda (s) (cons 0 (+ 10 s))))
                       (lambda (a) (State (lambda (s) (cons a (* 11 s)))))) 1)

(provide 'monad)
;;; monad.el ends here
