(defun derive(expr args)
  (if (or (null expr) (null args))
    nil
    (if (not (symbolp args))
      nil
      (cond
	((numberp expr) 0)
	((symbolp expr) (if (eql expr args) 1 0))
	((listp expr) 
	 (cond ((eql '+ (car expr)) (list '+ (derive (car(cdr expr)) args) (derive (car(cdr(cdr expr)))args))) 
	 ((eql '- (car expr)) (list '- (derive (car(cdr expr)) args) (derive (car(cdr(cdr expr)))args))) 
	 ((eql '* (car expr)) (list '* (derive (car(cdr expr)) args) (derive (car(cdr(cdr expr)))args))) 
	 ((eql '/ (car expr)) (list '/ (derive (car(cdr expr)) args) (derive (car(cdr(cdr expr)))args))) 
	 )
      )
     )
    )
   )
 )

(print (derive 'x 'x))
(print (derive 'y 'x))
(print (derive 1 'x))
(print (derive '(+ x y) 'x))
(print (derive '(* x y) 'x))
(print (derive '(- x y) 'x))
(print (derive '(/ x y) 'x))
