(defpackage :a-lexer
  (:use :cl))

(in-package :a-lexer)

(defun pure (i)
  (lambda (inp) (list (cons i inp))))

(defun zero ()
  (lambda (inp) (declare (ignore inp)) '()))

(defun item ()
  (lambda (inp)
    (if inp
        (list (cons (first inp) (rest inp)))
        nil)))

(defun bind (p f)
  (labels ((applicator (v-and-inp)
             (let ((v (car v-and-inp))
                   (inp (cdr v-and-inp)))
               (funcall (funcall f v) inp))))
    (lambda (inp)
      (mapcan #'applicator (funcall p inp)))))

(defun sat (p)
  (bind (item)
        (lambda (x)
          (if (funcall p x) (pure x) (zero)))))

(defun char? (c)
  (sat (lambda (x) (equal x c))))

(defun digit? ()
  (sat #'digit-char-p))

(defun letter? ()
  (sat #'alpha-char-p))

(defun plus (p q)
  (lambda (inp)
    (append (funcall p inp) (funcall q inp))))

(defun alphanum? ()
  (plus (digit?) (letter?)))

(defun or? (a b)
  (lambda (inp)
    (let ((r (or (car (funcall a inp))
                 (car (funcall b inp)))))
      (if r (list r) nil))))

(defun many? (p)
  (labels ((at-least-one ()
             (bind p
                   (lambda (x)
                     (bind (many? p)
                           (lambda (xs)
                             (pure (cons x xs))))))))
    (or?
     (at-least-one)
     (pure '()))))



(defun run-parser (parser inp)
  (funcall parser (coerce inp 'list)))
