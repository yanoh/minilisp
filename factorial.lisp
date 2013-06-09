(defun mul (l r) (if (= r 1) l (+ l (mul l (- r 1)))))
(defun fact (n) (if (= n 1) 1 (mul n (fact (- n 1)))))
(println (fact 5))
