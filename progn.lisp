(defun last (lst)
  (if (cdr lst)
    (last (cdr lst))
    (car lst)))

(defmacro eval-list (lst)
  (if lst
    (list 'cons (car lst) (list 'eval-list (cdr lst)))))

(defmacro progn (lst)
  (list 'last (list 'eval-list lst)))


(println 'progn-macro-test-1)
(progn ((println 'hello)
        (println 'world)))

(println 'progn-macro-test-2)
(println (progn ((println 'hello)
                 (println 'world)
                 (+ 1 1))))

(exit)
