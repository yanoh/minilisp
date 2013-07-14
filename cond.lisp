(defun not (x)
  (if x
    ()
    t))

(defun and (lhs rhs)
  (if lhs
    (if rhs
      t)))

(defun or (lhs rhs)
  (if lhs
    t
    (if rhs
      t)))

(defun lt (lhs rhs)
  (prim-lt lhs rhs))

(defun gt (lhs rhs)
  (lt rhs lhs))

(defun eq (lhs rhs)
  (and (not (lt lhs rhs))
       (not (gt lhs rhs))))

(defun ne (lhs rhs)
  (not (eq lhs rhs)))

(defun le (lhs rhs)
  (or (lt lhs rhs)
      (eq lhs rhs)))

(defun ge (lhs rhs)
  (or (gt lhs rhs)
      (eq lhs rhs)))

(defun length (lst)
  (if lst
    (+ 1 (length (cdr lst)))
    0))

(defun nthcdr (n lst)
  (if (eq n 0)
    lst
    (nthcdr (+ n (negate 1)) (cdr lst))))

(defun nthcar (n lst)
  (car (nthcdr n lst)))

(defun last (lst)
  (nthcar (+ (length lst) (negate 1)) lst))

(defmacro eval (lst)
  (list 'if 't lst))

(defmacro progn (lst)
  (if (eq 1 (length lst))
    (eval (car lst))
    (list 'last
          (list 'list
                (eval (car lst))
                (list 'progn (cdr lst))))))

(defmacro when (x lst)
  (list 'if x (list 'progn lst)))


(println 'when-macro-test)
(when (eq 1 1) ((println 'hello) (println 'world)))

(defmacro cond (lst)
  (list 'if
        (list 'and (car lst) (ge (length lst) 2))
        (list 'progn (car (cdr lst)))
        (list 'if
              (gt (length lst) 2)
              (list 'cond (cdr (cdr lst))))))

(println 'cond-macro-test)
(cond ((= 1 0)
        ((println 'case-1)
         (println 'hello)
         (println 'world))
       (= 1 1)
        ((println 'case-2)
         (println 'hello)
         (println 'world))
       t
        ((println 'default)
         (println 'hello)
         (println 'world))))

(exit)
