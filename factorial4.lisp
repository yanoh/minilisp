(defun mul (l r) (if (= r 1) l (+ l (mul l (+ r (negate 1))))))
(defun fact (n) (if (= n 1) 1 (mul n (fact (+ n (negate 1))))))

(defun times (n func) (if (= n 0) t (car (list (times (+ n (negate 1)) func) (func (+ n (negate 1)))))))
(times 1000 (lambda (n) (println (list n (fact 5)))))

(exit)
