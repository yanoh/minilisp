(defun mul (l r) (if (= r 1) l (+ l (mul l (+ r (negate 1))))))
(defun fact (n) (if (= n 1) 1 (mul n (fact (+ n (negate 1))))))
(println (fact 5))
(exit)
