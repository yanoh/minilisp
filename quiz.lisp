(defun last (lst)
  (if (cdr lst)
    (last (cdr lst))
    (car lst)))

(defmacro eval-list (lst)
  (if (cdr lst)
    (list 'cons (car lst) (list 'eval-list (cdr lst)))
    (list 'list (car lst))))

(defmacro progn (lst)
  (list 'last (list 'eval-list lst)))

(defun caar (lst)
  (car (car lst)))

(defun cadar (lst)
  (car (cdr (car lst))))

(defun define-all (vars)
  (if vars
    (cons (list 'define
                (caar vars)
                (cadar vars))
          (define-all (cdr vars)))))

(defun append (head tail)
  (if head
    (cons (car head)
          (append (cdr head) tail))
    tail))

(defmacro let (vars exprs)
  (list 'progn
        (append (define-all vars)
                exprs)))

(defun mapc (func lst)
  (if (cdr lst)
    (progn ((func (car lst))
            (mapc func (cdr lst))
            lst))
    (progn ((func (car lst))
            lst))))

(defun mapcar (func lst)
  (if lst
    (cons (func (car lst)) (mapcar func (cdr lst)))))

(defun select (func lst)
  (if (cdr lst)
    (if (func (car lst))
      (cons (car lst) (select func (cdr lst)))
      (select func (cdr lst)))
    (if (func (car lst))
      (cons (car lst) ())
      ())))

(defun not (x)
  (if x () t))

(defun sum (lst)
  (if (cdr lst)
    (+ (car lst) (sum (cdr lst)))
    (car lst)))

(defun kondo1 (lst)
  (mapcar (lambda (n)
            (sum (select (lambda (m) (not (= n m))) seq)))
          seq))

(defun kondo2 (lst)
  (let ((s (sum seq)))
    ((mapcar (lambda (n) (+ s (negate n))) seq))))

(define seq '(1 5 8 2 4))

(println seq)
(println (kondo1 seq))
(println (kondo2 seq))

(exit)
