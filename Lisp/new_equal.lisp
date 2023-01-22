(defun newequal(a b)
  (cond
    ((and (atom a) (atom b))
      (eql a b)
      )
    ((and (listp a) (listp b))
      (if (newequal (car a) (car b))
        (newequal (cdr a) (cdr b))
        nil
      )
     )
    )
  )

(print (newequal (list 1 2) (list 1 2)))
(print (newequal (list 1 3) (list 1 2)))
