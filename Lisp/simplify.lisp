(defun simplify(expr)
    (cond
      ((eql '+ (car expr))
       (cond
       ((and (numberp (second expr)) (numberp (third expr))) (+ (second expr) (third expr)))
       ((and (numberp (second expr)) (zerop (second expr))) (simplify (third expr)))
       ((and (numberp (third expr)) (zerop (third expr))) (simplify (second expr)))
        )
       )
      ((eql '* (car expr))
       (cond
       ((and (numberp (second expr)) (zerop (second expr))) 0)
       ((and (numberp (third expr)) (zerop (third expr))) 0)
       ((and (numberp (second expr)) (= 1 (second expr))) (simplify (third expr)) )
       ((and (numberp (third expr)) (= 1 (third expr))) (simplify (second expr)) )
        )
       )
      )
  )

(print (simplify '(* 0 2)))
(print (simplify '(+ 0 2)))
(print (simplify '(* 1 (+ 0 2))))
(print (simplify '(* (+ 3 4) 1)))
