(define random-seq
  '(15 63 82 57 38  4 88 74 91 14 22 66 21 81 16 99 19 84 54 30
    26 49 43 87 37 67 24 76 78 13  7 97 50 60  6 36 89 34 86 58
    32 48 17 62 23 55 33  3  0 35  8 94 45  2 18  1 98 64 39 47
    68 73 52 59 29  5 69 25 92 31 42 96 90 79 85 93 44 12 20 70
    65 46 40 28 56 75  9 83 41 53 10 51 95 61 11 72 27 71 77 80))


(defun not (x) (if x () t))
(defun and (lhs rhs) (if lhs (if rhs t)))
(defun or (lhs rhs) (if lhs t (if rhs t)))

(defun lt (lhs rhs) (prim-lt lhs rhs))
(defun gt (lhs rhs) (lt rhs lhs))

(defun eq (lhs rhs) (and (not (lt lhs rhs)) (not (gt lhs rhs))))
(defun ne (lhs rhs) (not (eq lhs rhs)))

(defun le (lhs rhs) (or (lt lhs rhs) (eq lhs rhs)))
(defun ge (lhs rhs) (or (gt lhs rhs) (eq lhs rhs)))


(defun append (lhs rhs)
  (if lhs (cons (car lhs) (append (cdr lhs) rhs)) rhs))

(defun select (n lst comp)
  (if lst
    (append
      (if (comp n (car lst)) (list (car lst)))
      (select n (cdr lst) comp))))

(defun qsort (lst)
  (if lst
    (append
      (append
        (qsort (select (car lst) (cdr lst)
                       (lambda (lhs rhs) (gt lhs rhs))))
        (list (car lst)))
      (qsort (select (car lst) (cdr lst)
                     (lambda (lhs rhs) (le lhs rhs)))))))


(println (qsort random-seq))
(exit)
