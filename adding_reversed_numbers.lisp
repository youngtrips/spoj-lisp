(defun rev (n)
  (let ((ret 0))
    (loop
      (when (<= n 0) (return))
      (setf ret (+ (* ret 10) (mod n 10)))
      (setf n (truncate (/ n 10))))
    ret))

(dotimes (cases (read))
  (let ((m (read))
        (n (read)))
    (format t "~a~%" (rev (+ (rev m) (rev n))))))
