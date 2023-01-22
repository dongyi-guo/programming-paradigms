;;;; COMP3109 Assignment 1
;;;; Name: Jelly DongYi Guo 
;;;; SID: 470033070 
;;;; UniKey: dguo0006

;;; This file is conducted and tested on GNU clisp v2.49 <http://clisp.cons.org/>
;;; How to use:
;;; $ clisp -i matrix.lisp test.lisp
;;; For individual function usage:
;;; $ clisp -i matrix.lisp

;;; This file contains main functions of matrice manipulation and documentaries of explaination
;;; For testcases and examples, see ./test.lisp

;;; Task 1
;; Task 1 contains 4 functions, respectively 1 main function and 3 helper functions for a complete recursive usage

;; Function chechelem(a) takes one single element in (will never be nil)
;; Checking if element is in scope of the matrix type
(defun checkelem(a)
    ;; Explicitly deal with parameter based on its type
    (cond
        ;; single element is whether symbolic or numeric, then T
        ((numberp a) T)
        ((symbolp a) T)
        ;; or it's a function (+) or (*) with 2 numeric parameters, then T
        ((listp a)
            (if (= 3 (list-length a)) 
                (if (or (eql '+ (car a)) (eql '* (car a)))
                T nil
                )
            )
        )
        ;; for a single instance, there will be no more fit in E
        (T nil)
    )
)
;; Function checkvl(l) determines whether all the element in single list are in the scope of the language or not
(defun checkvl(l)
    ;; if the list has only one element, then refer to checkelem(a), simply check whether is the element is in scope or not
    (if (null (cdr l))
        (checkelem (car l))
        ;; if the list has more than one element,
        ;; then refer ot checkelem(a), check the head element of the list is in the scope or not, if yes
        ;; then recursively use the function to rest of the list
        (if (checkelem (car l))
            (checkvl (cdr l))
            ;; if there is an element is not in scope of the language, we fail
            nil
        )
    )
)
;; Function checklength(m) determines whether the matrix is in a good form
;; which is, all sub-lists nested is in a same length
(defun checklength(m)
    ;; Sorry for the reverse thinking
    ;; if there IS more than one element in the matrix, in this case, row number of the matrix is larger than 1
    (if (null (null (cdr m)))
        ;; then recursively go through the first 2 columns, check whether they have the same length
        (if (= (list-length (car m)) (list-length (car (cdr m))))
            ;; if yes, then check whether the head list is fully in scope of the language
            (if (checkvl (car m))
                ;; if yes again, recursively use the function on the rest columns
                (checklength (cdr m))
                ;; if head list is not in scope, we fail
                nil
            )
            ;; if the first 2 columns are having unequal length, we fail
            nil
        )
        ;; if the matrix has only one nested list, in the other word, a 1-row matrix
        ;; simply check the nested column (list) is fully in the scope of language
        (checkvl (car m))
    )
)

;; Function m-check(a) takes the matrix, generally kicks all the non-required possibility of parameters
;; which means any parameter which is not a double list
;; then call the helper functions which are applied stackly already, which can finish the job automatically
(defun m-check(m)
    ;; first thing first, our parameter cannot be null
    (if (null m) nil
        ;; As a matrix, form of which must be a list of list, any other type of parameters will be rejected
        (if (null (listp m)) nil
        ;; Just in case, nested list cannot be empty because smallest matrix accepted can only be 1 * 1
            (if (null (car m)) nil
                (if (null (listp (car m))) nil
                    (checklength m)
)))))

;;; Task 2
;; Function m-shape(m) takes matrix in, firstly check the form of matrix, then return the dimensions of the matrix
(defun m-shape(m)
    ;; if matrix is not well formed, we fail
    (if (not (m-check m)) nil
    ;; output the dimensions by list (row column), which is retrieved from the list length of outer list and head nested list
        (list (list-length m) (list-length (car m)))
))

;;; Task 3
;; Function m-elem(m r c) takes a matrix, a row_idx and a col_idx to identify the wanted element
(defun m-elem(m r c)
    ;; if matrix is not well formed, we fail
    (if (not (m-check m)) nil)
    ;; if row index or column index are invalid, we fail
    (if (or (< r 1) (< c 1)) nil)
    ;; General ideas:
    ;; We approach our target by "ripping the head off" theory if row and column index is not one
    ;; if the row index is not 1, then recursively pass the function with rest of the outer list
    ;; if the row index is 1, then YES! the head sub-list is the column we want
    ;; if the column index is not 1, then recursively pass the function with rest of the nested list, as a list inside a list
    ;; if the column index is 1, then YYEESS!! the head element is what we want
    ;; Any situation besides finding a correct element with a correct coordinator will be failed unfortunately:(
    (cond
        ;; if the matrix has only one row
        ((= 1 r)
            ;; then we start to locate the element in the specific column
            (cond
                ;; if the matrix has only one column
                ((= 1 c)
                    ;; Yay! the first element is indeed what we need
                    (car (car m))
                )
                ;; if the matrix has more than one column
                ((> c 1)
                    ;; we get rid off the first element of head sublist and pass the rest into the function recursively
                    (m-elem (list (cdr (car m))) r (- c 1))
                )
            )
        )
        ;; if the matrix has more than one row
        ((> r 1)
            ;; we get rid off the first column(nested list) and pass the rest into the function recursively
            (m-elem (cdr m) (- r 1) c)
        )
        ;; No other conditon or failed operation above accepted :(
        (T nil)
    )
)

;;; Task 4
;; Function m-add(m n) takes 2 complied matrices, if they are in a good form,
;; it returns the results of matrix of adding those 2 matrices
(defun m-add(m n)
    ;; check whether m and n are in good shape or not, if not story ends
    (if (or (not (m-check m)) (not (m-check n))) nil
    ;; check whether m and n are in same structure or not, if not story ends
    (if (or (not (= (car (m-shape m)) (car (m-shape n)))) (not (= (car (cdr (m-shape m))) (car (cdr (m-shape n)))))) nil
    ;; we use function mapcar here twice to get every element from both matrices in same coordinators,
    ;; and re-construct them into a expression of one element of the results
    (mapcar
    (lambda (x y) (mapcar (lambda (a b) (list '+ a b)) x y))
    m n)
)))

;;; Task 5
;; Helper function rotate(m) helps on rotating the matrix in order to have the same dimension as the other one
;; This function rotates the matrix in anti-clockwise order
;; [[1,2,3]                  [[1,4,7]
;;  [4,5,6]  -> rotate() ->   [2,5,8]
;;  [7,8,9]]                  [3,6,9]]
;; While target matrix is rectangle, it will have the same shape as the other matrix,
;; and easy list-constructing can be done with a double mapcar()
(defun rotate(m)
    ;; check whether m and n are in good shape or not, if not story ends
    (if (not (m-check m)) nil
    ;; Application of mapcar allows us to retrieve every element in the same position in every column of the matrix
    ;; and list() is applied to construct them into a list entry
    (apply #'mapcar #'list m)
))
;; Function m-mul(m1 m2) takes 2 complied matrices, if they are in a good form
;; Do the matrix multiplication if conditions met
(defun m-mul(m1 m2)
    ;; check whether m and n are in good shape or not, if not story ends
    (if (or (not (m-check m1)) (not (m-check m2))) nil
    (cond
    ;; Matrices multiplication requires one of the row is equal to the other's column
    ;; that is, whether the first parameter's row, or second's row respectively to second's column, or the first's column
     ;; if first matrix's row == second's column
     ((= (car (m-shape m1)) (cadr (m-shape m2)))
     ;; pass the first matrix and rotated second matrix, in order to have the same shapes
     ;; and use the double mapcar() to conduct every element into a (*) expression
     ;; then conduct every (+) expression on column of the resulting matrix
        (mapcar (lambda (x y) (cons '+ (mapcar (lambda (a b) (list '* a b)) x y))) m1 (rotate m2))
     )
     ;; if second matrix's row == first's column
     ((= (car (m-shape m2)) (cadr (m-shape m1)))
     ;; pass the second matrix and rotated first matrix, in order to have the same shapes
     ;; and use the double mapcar() to conduct every element into a (*) expression
     ;; then conduct every (+) expression on column of the resulting matrix
        (mapcar (lambda (x y) (cons '+ (mapcar (lambda (a b) (list '* a b)) x y))) (rotate m1) m2)
     )
     ;; Any other situation like condition of multiplication is not met, will be failed
     (T nil)
    )
))


;;; Task 6
;; Function m-scalar-mul(a m) takes an symbolic a and a matrix, if matrix is in a good form
;; Do the scalar multiplication
(defun m-scalar-mul(a m)
    ;; check whether m and n are in good shape or not, if not story ends
    (if (not (m-check m)) nil
    ;; checj whether our scalar is valid, if not then story ends as well
    (if (or (null a) (not (numberp a))) nil
    ;; Use the double mapcar() to make every element in the matrix scalar-multiplied with a
    (mapcar (lambda (x) (mapcar (lambda (p) (list '* p a)) x)) m)
)))

;;; Task 7
;; Helper function - helps on single element simplification based on rules
;; Referred to Week 4's tutorial exercise 4
;; This function aims to perform the rules on every single element
(defun simplify(expr)
    (cond
    ; Rules for (+)
      ((eql '+ (car expr))
       (cond
       ; (+ <c1> <c2>) -> (+ c1 c2)
       ((and (numberp (second expr)) (numberp (third expr))) (+ (second expr) (third expr)))
       ; (+ <e> 0) -> <e>
       ((and (symbolp (second expr)) (and (numberp (third expr)) (zerop (third expr)))) (second expr))
       ; (+ 0 <e>) -> <e>
       ((and (symbolp (third expr)) (and (numberp (second expr)) (zerop (second expr)))) (third expr))
       ; (+ <v> <v>) -> (* 2 <v>)
       ((eql (second expr) (third expr)) (list '* '2 (second expr)))
       ; (+ (* <c1> <v>) (* <c2> <v>)) -> (* (+ c1 c2) <v>)
       ((and (listp (second expr)) (listp (third expr)))
        (if (and (eql '* (car (second expr))) (eql '* (car (third expr))))
            (if (and (numberp (second (second expr))) (numberp (second (third expr))))
                (if (and (and (symbolp (third (second expr))) (symbolp (third (third expr))))(eql (third (second expr)) (third (third expr))))
                (list '* (+ (second (second expr)) (second (third expr))) (third (second expr)))))))
        ; if element cannot be proceed to be simplified anymore, return the original element
        (T expr)      
    ))
    ; Rules for (*)
      ((eql '* (car expr))
       (cond
       ; (* <c1> <c2>) -> (* c1 c2)
       ((and (numberp (second expr)) (numberp (third expr))) (* (second expr) (third expr)))
       ; (* 1 <e>) -> <e>
       ((and (and (numberp (second expr)) (= 1 (second expr))) (symbolp (third expr))) (third expr))
       ; (* <e> 1) -> <e>
       ((and (and (numberp (third expr)) (= 1 (third expr))) (symbolp (second expr))) (second expr))
       ; (* (+ <e1> <e2>) <e3>) -> (+ (* <e1> <e3>) (* <e2> <e3>))
       ((and (listp (second expr)) (eql '+ (car (second expr))))(list '+ (list '* (second (second expr)) (third expr)) (list '* (third (second expr)) (third expr))))
       ; (* <e1> (+ <e2> <e3>)) -> (+ (* <e1> <e2>) (* <e1> <e3>))
       ((and (listp (third expr)) (eql '+ (car (third expr))))(list '+ (list '* (second (third expr)) (second expr)) (list '* (third (third expr)) (second expr))))
        ; if element cannot be proceed to be simplified anymore, return the original element
        (T expr)
        )
       )
        ;if expression cannot be proceed to be simplified anymore, return the original expression
       (T expr)
      )
  )
;; Function m-simplify(m) takes the matrix
(defun m-simplify(m)
    (if (not (m-check m)) nil
    (mapcar (lambda (x) (mapcar (lambda (y) (simplify y)) x))m)
))

;;;; Not accomplished
;; Task 5
;; if the expected matrix result is not square (for instance consequence of 4*1 and 1*2)
;; current m-mul(m n) will not be able to do a complete calculation by losing 2 rows
;; My thought is while I found the two matrices is not generating a square result
;; Duplicate the one with less length on its dimensions till it reached the same length as the other one

;; Task 7
;; It is quite easy to tell -- current m-simplify(m) cannot evaluate the expressions that generated by itself
;; if the element is able to be simplified
;; Thoughts of mine here were actually there can be another token parameter passed in both simplify() and the m-simplify()
;; while every element (instance of list of list) is not a list (not expression)
;; then the token will actually pass something so we acknowledge there will be no more simplification needed

;;;; But deadline is deadline, have to leave and pay my best respects :....(