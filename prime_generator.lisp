(defun factor-out (number divisor)
  (do ((e 0 (1+ e))
       (r number (/ r divisor)))
      ((/= (mod r divisor) 0) (values r e))))
 
(defun mult-mod (x y modulus) (mod (* x y) modulus))
 
(defun expt-mod (base exponent modulus)
  (labels ((expt-mod-iter (b e p)
             (cond ((= e 0) p)
                   ((evenp e)
                    (expt-mod-iter (mult-mod b b modulus)
                                   (/ e 2)
                                   p))
                   (t
                    (expt-mod-iter b
                                   (1- e)
                                   (mult-mod b p modulus))))))
    (expt-mod-iter base exponent 1)))
 
(defun random-in-range (lower upper)
  (+ lower (random (+ (- upper lower) 1))))
 
(defun miller-rabin-test (n k)
  (cond ((= n 1)   nil)
        ((< n 4)     t)
        ((evenp n) nil)
        (t
         (multiple-value-bind (d s) (factor-out (- n 1) 2)
           (labels ((strong-liar? (a)
                      (let ((x (expt-mod a d n)))
                        (or (= x 1)
                            (loop repeat s
                                  for y = x then (mult-mod y y n)
                                  thereis (= y (- n 1)))))))
             (loop repeat k
                   always (strong-liar? (random-in-range 2 (- n 2)))))))))

(dotimes (cases (read))
    (let ((m (read))
          (n (read)))
        (do ((i m (+ i 1)))
            ((> i n))
            (if (miller-rabin-test i 20) (format t "~a~%" i)))))
