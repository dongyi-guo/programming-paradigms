(defun myequal(a b)
  (cond
    ((and (null a) (null b))
     T
     )
    ((or (null a) (null b))
     nil
     )
    ((and (listp a) (listp b))
     (if (myequal (car a) (car b))
       (myequal (cdr a) (cdr b))
       nil
       )
     )
    (T
      (eql a b)
      )
   )
  )


(print (myequal (list 1 3) (list 1 2)))
(print (myequal (list 1 2) (list 1 2)))

