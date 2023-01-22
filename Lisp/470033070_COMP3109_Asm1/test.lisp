;;;; COMP3109 Assignment 1 (Testcases)
;;;; Name: Jelly DongYi Guo 
;;;; SID: 470033070 
;;;; UniKey: dguo0006

;;; This file is conducted and tested on GNU clisp v2.49 <http://clisp.cons.org/>
;;; How to use:
;;; $ clisp -i matrix.lisp test.lisp
;;; For individual function usage:
;;; $ clisp -i matrix.lisp

;; This file contains testcases for every function in matrix.lisp, categorized by Tasks
;; For function description, see ./matrix.lisp
;; Testcases are written in range of the function features which is put down in description
;; E.g.: checkelem(a) is never expecting NILs, testcases with NILs on checkelem(a) will not be applied

;; Testcase functions will finish all the 5 testcases for each function at once
;; Successful individual testcases will have outputs
;; Successful function testcases will have outputs

;; Testcases
; pre-loaded elements
(setq A '((a b)(c d)(e f)))
(setq B '((1 2 3)(4 5 6)))
(setq C'((a b)(c d)))
(setq D '((1 2)(3 4)))
(setq P '((1 2 3)))
(setq Z '((a b) (c k) (e f)))
(setq E '((a b)(c d)(e f)))
(setq F '((1 2 3)(4 5 6)))
(setq G '((a b)(c d)))
(setq H '((1 2)(3 4)))
(setq ll '(+ (* 2 v) (* 3 v)))
(setq ee '(* v (+ a b)))
(setq pp '(* (+ a b) v))
(setq M '(((+ 1 2) (+ a 4)) ((* 1 p)(+ 0 q))))
(setq N '(((+ 0 a) (+ a a)) ((* 1 p)(+ 0 q))))
(setq Q (list (list ee pp)))
(setq X (list (list ll '(+ a a))))
(setq Y '(((+ 2 1)(* 3 3)) ((* 1 a)(* p 1))))
; Divide line
(defun divideline() (print "-----------------------------------"))

(print "Test starts!")
(divideline)
;; Task 1

; checkelem(a):
; (checkelem 1) -> T
; (checkelem 'a) -> T 
; (checkelem '(+ 1 2)) -> T
; (checkelem '(* a b)) -> T
; (checkelem '(/ 2 1)) -> nil
(defun testcelem()
    (if (eql (checkelem 1) T)
        (if (eql (checkelem 'a) T)
            (if (eql (checkelem '(+ 1 2)) T)
                (if (eql (checkelem '(* a b)) T)
                    (if (eql (checkelem '(/ 2 1)) nil)
                    (progn
                        (print "checkelem testcase 1 succeeded")
                        (print "checkelem testcase 2 succeeded")
                        (print "checkelem testcase 3 succeeded")
                        (print "checkelem testcase 4 succeeded")
                        (print "checkelem testcase 5 succeeded")
                        (print "checkelem(a) succeeded"))
                    ))))))
(testcelem)
(divideline)
; checkvl(l)
; (checkvl '(1)) -> T
; (checkvl '(1 2 a b)) -> T 
; (checkvl '((+ 1 2) (- 1 3))) -> nil
; (checkvl '((/ 9 9))) -> nil
; (checkvl '(k k k k k k)) -> T
(defun testvl()
    (if (eql (checkvl '(1)) T)
        (if (eql (checkvl '(1 2 a b)) T)
            (if (eql (checkvl '((+ 1 2) (- 1 3))) nil)
                (if (eql (checkvl '((/ 9 9))) nil)
                    (if (eql (checkvl '(k k k k k k)) T)
                    (progn
                        (print "checkvl testcase 1 succeeded")
                        (print "checkvl testcase 2 succeeded")
                        (print "checkvl testcase 3 succeeded")
                        (print "checkvl testcase 4 succeeded")
                        (print "checkvl testcase 5 succeeded")
                        (print "checkvl(l) succeeded"))
                    ))))))
(testvl)
(divideline)
; checklength(m)
; (checklength '((1))) -> T
; (checklength '((1 x y)(2 a b))) -> T
; (checklength '((1 x y)(a b))) -> nil 
; (checklength '((1) (+ 2 4) (+ 1 1))) -> T
; (checklength '((1) (- 2 4) (+ 1 1))) -> nil
(defun testlength()
    (if (eql (checklength '((1))) T)
        (if (eql (checklength '((1 x y)(2 a b))) T)
            (if (eql (checklength '((1 x y)(a b))) nil)
                (if (eql (checklength '((1) ((+ 2 4)) ((+ 1 1)))) T)
                    (if (eql (checklength '((1) (- 2 4) (+ 1 1))) nil)
                    (progn
                        (print "checklength testcase 1 succeeded")
                        (print "checklength testcase 2 succeeded")
                        (print "checklength testcase 3 succeeded")
                        (print "checklength testcase 4 succeeded")
                        (print "checklength testcase 5 succeeded")
                        (print "checklength(m) succeeded"))
                    ))))))
(testlength)
(divideline)
; m-check(m)
; (m-check nil) -> nil
; (m-check '(nil)) -> nil
; (m-check '(((+ 1 1)))) -> T
; (m-check '((1 x y) (2 a b))) -> T
; (m-check '((p) ((+ 1 2)))) -> T 
(defun testcheck()
    (if (eql (m-check nil) nil)
        (if (eql (m-check '(nil)) nil)
            (if (eql (m-check '(((+ 1 1)))) T)
                (if (eql (m-check '((1 x y) (2 a b))) T)
                    (if (eql (m-check '((p) ((+ 1 2)))) T)
                    (progn
                        (print "m-check testcase 1 succeeded")
                        (print "m-check testcase 2 succeeded")
                        (print "m-check testcase 3 succeeded")
                        (print "m-check testcase 4 succeeded")
                        (print "m-check testcase 5 succeeded")
                        (print "m-check(m) succeeded"))
                    ))))))
(testcheck)
(divideline)
;; Task 2
; m-shape(m)
; (m-shape '((1 x y) (2 a b))) -> (2 3)
; (m-shape '((1 x y) (2 a))) -> nil
; (m-shape nil) -> nil
; (m-shape '((1))) -> (1 1)
; (m-shape '(())) -> nil
(defun testshape()
    (if (equal (m-shape '((1 x y) (2 a b))) (list 2 3))
        (if (eql (m-shape '((1 x y) (2 a))) nil)
            (if (eql (m-shape nil) nil)
                (if (equal (m-shape '((1))) (list 1 1))
                    (if (eql (m-shape '(())) nil)
                    (progn
                        (print "m-shape testcase 1 succeeded")
                        (print "m-shape testcase 2 succeeded")
                        (print "m-shape testcase 3 succeeded")
                        (print "m-shape testcase 4 succeeded")
                        (print "m-shape testcase 5 succeeded")
                        (print "m-shape(m) succeeded"))
                    ))))))
(testshape)
(divideline)
;; Task 3
; m-elem(m)
; (m-elem '((1 x y) (2 a b)) 2 2) -> a
; (m-elem '((1 2) (3 4)) 0 1) -> nil
; (m-elem '((1 2) (3 4)) 1 0) -> nil
; (m-elem '((1 2) (3 4)) 4 4) -> nil
; (m-elem '((+ 1 1)) 1 1) -> (+ 1 1)
(defun testelem()
    (if (equal (m-elem '((1 x y) (2 a b)) 2 2) 'a)
        (if (eql (m-elem '((1 2) (3 4)) 0 1) nil)
            (if (eql (m-elem '((1 2) (3 4)) 1 0) nil)
                (if (eql (m-elem '((1 2) (3 4)) 4 4) nil)
                    (if (equal (m-elem '(((+ 1 1))) 1 1) (list '+ 1 1))
                    (progn
                        (print "m-elem testcase 1 succeeded")
                        (print "m-elem testcase 2 succeeded")
                        (print "m-elem testcase 3 succeeded")
                        (print "m-elem testcase 4 succeeded")
                        (print "m-elem testcase 5 succeeded")
                        (print "m-elem(m) succeeded"))
                    ))))))
(testelem)
(divideline)
;; Task 4
; m-add(m n)
; (m-add '((1 2)) '((2 3))) -> ((+ 1 2)(+ 2 3))
; (m-add '((a b)) '((m n))) -> ((+ a m)(+ b n))
; (m-add '((+ 1 2) x y) '(2 3 (* p q))) -> nil
; (m-add '((1 2) (3 1)) '((1 3))) -> nil
; (m-add nil nil) -> nil
(defun testadd()
    (if (equal (m-add '((1 2)) '((2 3))) '(((+ 1 2) (+ 2 3))))
        (if (equal (m-add '((a b)) '((m n))) '(((+ A M) (+ B N))))
            (if (eql (m-add '(x y) '(2)) nil)
                (if (eql (m-add '((1 2) (3 1)) '((1 3))) nil)
                    (if (eql (m-add nil nil) nil)
                    (progn
                        (print "m-add testcase 1 succeeded")
                        (print "m-add testcase 2 succeeded")
                        (print "m-add testcase 3 succeeded")
                        (print "m-add testcase 4 succeeded")
                        (print "m-add testcase 5 succeeded")
                        (print "m-add(m n) succeeded"))
                    ))))))
(testadd)
(divideline)
;; Task 5
; rotate(m)
; (rotate '((a b)(c d)(e f))) -> ((a c e)(b d f))
; (rotate '((a b c)(k e f))) -> ((a k)(b e)(c f))
; (rotate '((1 x y)(2 b))) -> nil
; (rotate '((1 3)(2 4))) -> ((1 2)(3 4))
; (rotate nil) -> nil
(defun testrotate()
    (if (equal (rotate '((a b)(c d)(e f))) '((A C E) (B D F)))
        (if (equal (rotate '((a b c)(k e f))) '((A K) (B E) (C F)))
            (if (eql (rotate '((1 x y)(2 b))) nil)
                (if (equal (rotate '((1 3)(2 4))) '((1 2) (3 4)))
                    (if (eql (rotate nil) nil)
                    (progn
                        (print "rotate testcase 1 succeeded")
                        (print "rotate testcase 2 succeeded")
                        (print "rotate testcase 3 succeeded")
                        (print "rotate testcase 4 succeeded")
                        (print "rotate testcase 5 succeeded")
                        (print "rotate(m) succeeded"))
                    ))))))
(testrotate)
(divideline)
; m-mul(m1 m2)
; (m-mul A B) -> ((+ (* A 1) (* B 4)) (+ (* C 2) (* D 5)) (+ (* E 3) (* F 6)))
; (m-mul B A) -> ((+ (* 1 A) (* 2 C) (* 3 E)) (+ (* 4 B) (* 5 D) (* 6 F)))
; (m-mul C D) -> ((+ (* A 1) (* B 3)) (+ (* C 2) (* D 4)))
; (m-mul P Q) -> ((+ (* 1 A)) (+ (* 2 C)) (+ (* 3 E)))
; (m-mul '((1)) D) -> nil
(defun testmul()
    (if (equal (m-mul A B) '((+ (* A 1) (* B 4)) (+ (* C 2) (* D 5)) (+ (* E 3) (* F 6))))
        (if (equal (m-mul B A) '((+ (* 1 A) (* 2 C) (* 3 E)) (+ (* 4 B) (* 5 D) (* 6 F))))
            (if (equal (m-mul C D) '((+ (* A 1) (* B 3)) (+ (* C 2) (* D 4))))
                (if (equal (m-mul P Z) '((+ (* 1 A)) (+ (* 2 C)) (+ (* 3 E))))
                    (if (eql (m-mul '((1)) D) nil)
                    (progn
                        (print "m-mul testcase 1 succeeded")
                        (print "m-mul testcase 2 succeeded")
                        (print "m-mul testcase 3 succeeded")
                        (print "m-mul testcase 4 succeeded")
                        (print "m-mul testcase 5 succeeded")
                        (print "m-mul(m1 m2) succeeded"))
                    ))))))
(testmul)
(divideline)
;; Task 6
; m-scalar-mul(a m)
; (m-scalar-mul 0 E) -> (((* A 0) (* B 0)) ((* C 0) (* D 0)) ((* E 0) (* F 0)))
; (m-scalar-mul nil E) -> nil
; (m-scalar-mul a E) -> nil
; (m-scalar-mul 9 E) -> (((* A 9) (* B 9)) ((* C 9) (* D 9)) ((* E 9) (* F 9)))
; (m-scalar-mul '() E) -> (((* A 3) (* B 3)) ((* C 3) (* D 3)) ((* E 3) (* F 3)))
(defun testsmul()
    (if (equal (m-scalar-mul 0 E) '(((* A 0) (* B 0)) ((* C 0) (* D 0)) ((* E 0) (* F 0))))
        (if (eql (m-scalar-mul nil E) nil)
            (if (eql (m-scalar-mul a E) nil)
                (if (equal (m-scalar-mul 9 E) '(((* A 9) (* B 9)) ((* C 9) (* D 9)) ((* E 9) (* F 9))))
                    (if (equal (m-scalar-mul (+ 1 2) E) '(((* A 3) (* B 3)) ((* C 3) (* D 3)) ((* E 3) (* F 3))))
                    (progn
                        (print "m-scalar-mul testcase 1 succeeded")
                        (print "m-scalar-mul testcase 2 succeeded")
                        (print "m-scalar-mul testcase 3 succeeded")
                        (print "m-scalar-mul testcase 4 succeeded")             
                        (print "m-scalar-mul testcase 5 succeeded")
                        (print "m-scalar-mul(a m) succeeded"))
                    ))))))
(testsmul)
(divideline)
;;Task 7
; simplify(a)
; (simplify '(+ 1 2)) -> 3
; (simplify '(+ 0 9)) -> 9
; (simplify '(+ a a)) -> (* 2 A)
; (simplify ll) -> (* 5 V)
; (simplify '(* 3 4)) -> 12
; (simplify '(* 1 e)) -> E
; (simplify '(* e 1)) -> E
; (simplify ee) -> (+(* a v)(* b v))
; (simplify pp) -> (+(* a v)(* b v))
(defun testasim()
    (if (equal (simplify '(+ 1 2)) 3)
        (if (eql (simplify '(+ 0 9)) 9)
            (if (equal (simplify '(+ a a)) '(* 2 A))
                (if (equal (simplify ll) '(* 5 V))
                    (if (equal (simplify '(* 3 4)) 12)
                        (if (eq (simplify '(* 1 e)) 'E)
                            (if (eq (simplify '(* e 1)) 'E)
                                (if (equal (simplify ee) '(+ (* A V) (* B V)))
                                    (if (equal (simplify pp) '(+ (* A V) (* B V)))
                                    (progn
                                        (print "simplify testcase 1 succeeded")
                                        (print "simplify testcase 2 succeeded")
                                        (print "simplify testcase 3 succeeded")
                                        (print "simplify testcase 4 succeeded")
                                        (print "simplify testcase 5 succeeded")
                                        (print "simplify testcase 6 succeeded")
                                        (print "simplify testcase 7 succeeded")
                                        (print "simplify testcase 8 succeeded")
                                        (print "simplify testcase 9 succeeded")
                                        (print "simplify(a) succeeded"))
                    ))))))))))
(testasim)
(divideline)
; m-simplify(m)
; (m-simplify M) -> ((3 (+ A 4))(P Q))
; (m-simplify N) -> ((A (* 2 A))(P Q))
; (m-simplify Q) -> (( (+ (* A V) (* B V)) (+ (* A V)(* B V))))
; (m-simplify X) -> (((* 5 V)(* 2 A)))
; (m-simplify Y) -> ((3 9)(A P))
(defun testmsim()
    (if (equal (m-simplify M) '((3 (+ A 4)) (P Q)))
        (if (equal (m-simplify N) '((A (* 2 A)) (P Q)))
            (if (equal (m-simplify Q) '(((+ (* A V) (* B V)) (+ (* A V) (* B V)))))
                (if (equal (m-simplify X) '(((* 5 V) (* 2 A))))
                    (if (equal (m-simplify Y) '((3 9) (A P)))
                    (progn
                        (print "m-simplify testcase 1 succeeded")
                        (print "m-simplify testcase 2 succeeded")
                        (print "m-simplify testcase 3 succeeded")
                        (print "m-simplify testcase 4 succeeded")
                        (print "m-simplify testcase 5 succeeded")
                        (print "m-simplify(m) succeeded"))
))))))
(testmsim)
(divideline)