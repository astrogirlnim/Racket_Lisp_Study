#lang racket

(provide hours
	 bin2dec
	 dec2bin
	 hex2dec
	 dec2hex
	 my-remove
	 my-reverse
	 sorted?
	 inflate
	 iterate
	 add5
	 collatz
	 compound
	 power-set
	 primes
	 prime-factors
	 all-factors
	 )

; Please do not modify the lines above this one.

; ********************************************************
; Name: Nataly Moreno-Martinez
; Email address: natybegins@gmail.com
; ********************************************************

; This file may be opened in DrRacket.  Lines beginning with
; semicolons are comments.

; If you are asked to write a procedure, please make sure it has the
; specified name, and the specified number and order of arguments.
; The names of the formal arguments need not be the same as in the
; problem specification.

; For each problem, the intended inputs to your procedures are
; specified (for example, "positive integers") and your procedures
; need not do anything reasonable for other possible inputs.

; You may write auxiliary procedures in addition to the requested
; one(s) -- for each of your auxiliary procedures, please include a
; comment explaining what it does, and giving an example or two.

; You may also use procedures you have written elsewhere in this
; assignment or previous assignments.  They only need to be defined
; once somewhere within this file.

; Please use the predicate equal? to test equality of values that may
; not be numbers.  To test equality of numbers, you can use =.

; Reading: Chapters 3 and 4 of the Racket Guide.


;; Concepts include the following:

; defining functions 
; lambda expressions
; recursion: top-level, i.e., cdr recursion 
; list selectors: car, cdr, first, rest
; list constructors: cons, list, append
; list functions: apply, reverse, remove-duplicates, sort, assq, assoc
; various math functions, e.g., quotient, remainder, expt
; defining local variables with let
; functions as arguments
; functions with optional arguments

; ********************************************************
; ** problem 0 **

; Replace the number 0 in the definition below to indicate the number
; of hours you spent doing this assignment Decimal numbers (eg, 6.237)
; are fine.  Exclude time spent reading.

(define hours 13)

; ********************************************************
; ** problem 1 **
; 
; Write two procedures which convert binary numbers to decimal and vice versa

; (bin2dec '(1 0 0))

; that takes a list of binary digits and returns the
; corresponding decimal number

; (dec2bin n)

; that takes a positive integer as input, and returns 
; a list of binary digits
; The following identity should hold:

;; (bin2dec (dec2bin n)) == n


; Examples
; (bin2dec '(1 0 1 0)) => 10
; (bin2dec '(1 1 1 1 1)) => 31
; (bin2dec '(1)) => 1
; (bin2dec '()) => 0
; (bin2dec '(1 0 0 0 0 0 0 0)) => 128
;
; (dec2bin 23) => '(1 0 1 1 1)
; (dec2bin 2) => '(1 0)
; (dec2bin 128) => '(1 0 0 0 0 0 0 0)
; (dec2bin 127) => '(1 1 1 1 1 1 1)

; ********************************************************

(define (bin2dec num)
  (foldl (lambda (element base)
           (+ (* base 2) element))
         0
         num))

(define (dec2bin n)
  (let-values ([(q r) (quotient/remainder n 2)])
    (if (= q 0)
        r
        (flatten (list (dec2bin q) r)))))

; ********************************************************
; ** problem 2 ** 

; Write two procedures which convert hexadecimal numbers (base 16) to
; decimal and vice versa

; (hex2dec n)

; that takes a list of hexadecimal (base 16) digits and returns the
; corresponding decimal number.  Note that hexadecimal digits may be
; upper or lower case. Your procedure should handle both.

; (dec2hex n)

; that takes a positive integer as input, and returns a list of
; hexadecimal digits.  Your procedure may output either upper or lower
; case hexadecimal digitsl.  The test procedures below expect only
; upper case, but the grading program will accept either.

; The following identity should hold:

;; (hex2dec (dec2hex n)) == n

; Examples

; (hex2dec '(A)) => 10
; (hex2dec '(F F)) => 255
; (hex2dec '(1 0 0)) => 256
; (hex2dec '(d e a d b e e f)) => 3735928559

; (dec2hex 255) => '(F F)
; (dec2hex 10) => '(A)
; (dec2hex 256) => '(1 0 0)
; (dec2hex 3735928559) => '(D E A D B E E F)

; Hint: the racket assq or assoc function might be useful for a simple
; table lookup to convert hex digits to integers and vice versa
; ********************************************************

; Case insensitive hexadecimal to decimal digit converter
; Converts 0 - F to 0 - 15 by indexing a list
(define (find_index element)
  (let ([digit (index-of '(0 1 2 3 4 5 6 7 8 9 A B C D E F) element)])
    (if digit
       digit
       (index-of '(0 1 2 3 4 5 6 7 8 9 a b c d e f) element))))

(define (hex2dec num)
  (foldl (lambda (element base)
           (let ([digit (find_index element)])
              (+ (* base 16) digit)))
        0
        num))


(define (dec2hex n)
  (let*-values ([(q r) (quotient/remainder n 16)]
                ; convert remainder r to hexadecimal digit dhex
               [(dhex) (list-ref '(0 1 2 3 4 5 6 7 8 9 A B C D E F) r)]) 
    (if (= q 0)
        (list dhex)
        (flatten (list (dec2hex q) dhex)))))

; ********************************************************
; ** problem 3 ** 

;; racket has built in functions: remove and reverse

;; Write a procedure my-remove which duplicates its behavior

;; (my-remove v lst [proc]) → list?
;;   v : any/c
;;   lst : list?

;; Returns a list that is like lst, omitting the first element of lst for
;; which the comparison procedure proc (which must accept two arguments)
;; returns #t (as evaluated for v and an element in the list)

;; Write a procedure my-reverse which duplicates the behavior of reverse

;; (my-reverse lst) → list?
;;  lst : list?

;; Returns a list that has the same elements as lst, but in reverse order.

;; Note: you will get no credit for a trivial solution such as:
;;  (define my-remove remove)
;;  (define my-reverse reverse)
;; I am nonplussed to have to say this, but the ULAs thought it was necessary.
;; Sheesh.

;; Examples:

;; (my-remove 2 '(1 2 3 2 4)) => '(1 3 2 4)
;; (my-remove 2 '(1 2 3 2 4) =) => '(1 3 2 4)
;; (my-remove '(2) '((1) (2) (3))) => '((1) (3))
;; (my-remove "2" '("1" "2" "3")) => '("1" "3")
;; (my-remove #\c '(#\a #\b #\c)) => '(#\a #\b)
;; (my-remove 2 '(1 2 3 4) <) => '(1 2 4)
;; (my-remove 2 '(1 2 3 4) >) => '(2 3 4)

;; (my-reverse '(1 2 3 4)) => '(4 3 2 1)
;; (my-reverse '(1)) => '(1)
;; (my-reverse '()) => '()
;; (my-reverse '((1) (2) (3))) => '((3) (2) (1)

; ********************************************************
 
; (Replace this comment with your procedure(s).)

(define (my-remove value lst [proc equal?])
  (cond
    ([null? lst]
     lst)
    ([proc value (car lst)]
     (rest lst))
    (else
     (cons (car lst) (my-remove value (rest lst) proc)))))

(define (my-reverse lst)
  (if (null? lst)
      '()
      (append (my-reverse (cdr lst)) (list (car lst)))))
; ********************************************************
; ** problem 4 ** 

; Write a procedure

; (sorted? lst . compare?)

; which takes a lst and returns true if the top-level items are sorted
; according to the given comparison operator, compare?.  compare? is
; optional (as indicated by the "." in the definition.)  The default
; comparison operator is <= (less than or equal for numbers)

;; Here are some examples, first with compare? given.

;;  (sorted? '(1 2 3 4) <) => #t
;;  (sorted? '(1 2 3 4) >) => #f
;;  (sorted? '(1 2 3 4 4) <) => #f
;;  (sorted? '(1 1 1 1) =) => #t
;;  (sorted? '(1 1 1 1) <) => #f
;;  (sorted? '(1 1 1 1) <=) => #t
;;  (sorted? '("a" "b" "c") string<=?) => #t
;;  (sorted? '((1) (1 2) (1 2 3) (1 2 3 4)) (lambda (x y) (<= (length x) (length y)))) => #t
;;  (sorted? '((1) (1 2) (1 2 3) (1 2 3 4) (1)) (lambda (x y) (<= (length x) (length y)))) => #f

;; Examples using default comparison operator: <=

;;  (sorted? '(1 2 3 4)) => #t
;;  (sorted? '(1 2 3 4 4 4)) => #t
;;  (sorted? '(1 2 3 4 3 2 1)) => #f
;;  


(define (sorted? lst . compare?)
  (let ((func (if (null? compare?)
                  <=
                  (car compare?))))
    (if (or (null? lst)
            (equal? (length lst) 1))
        #t
        (if (func (car lst) (second lst))
            (sorted? (cdr lst) func)
            #f))))

; ********************************************************
; ** problem 5 ** 

; Write a procedure

; (inflate lst)

;; which returns a list with all the top level numeric values in the
;; original list incremented by 1.  Non-numeric values are unchanged.

; Examples

;; (inflate '(1 2 3)) => '(2 3 4)
;; (inflate '(1)) => '(2)
;; (inflate '()) => '()
;; (inflate '(a b c 2 3 4)) => '(a b c 3 4 5)
;; (inflate '((1) (2) (3))) => '((1) (2) (3))


; ********************************************************
 
; (Replace this comment with your procedure(s).)

(define (inflate lst)
    (if (null? lst)
     '()
     (map (lambda (i)
            (if(integer? i)
               (add1 i)
               i))
          lst)))

; ********************************************************
; ** problem 6 **

; Write a procedure

; (iterate start proc n)

; which executes the function proc n times, beginning with the argument
; start, and using the result of the previous function as the argument
; for the next call.  It returns a list of all the results.

(define (add5 x) (+ x 5))
; (iterate 2 add5 10) => '(7 12 17 22 27 32 37 42 47 52)

; (iterate 0 (lambda (x) (+ x 1)) 3) => '(1 2 3)
; (iterate 1 (lambda (n) (* n 2)) 10) => '(2 4 8 16 32 64 128 256 512 1024)
; (iterate 1 (lambda (x) (* x -2)) 10) => '(-2 4 -8 16 -32 64 -128 256 -512 1024)
; (iterate 10 (lambda (n) (- n 1)) 10) => '(9 8 7 6 5 4 3 2 1 0)
; (iterate 3 (lambda (n) (+ n 2)) 10) => '(5 7 9 11 13 15 17 19 21 23)

(define (collatz n)
  (if (= (modulo n 2) 0) (/ n 2)
      (+ 1 (* n 3))))

; (iterate 100 collatz 25) => '(50 25 76 38 19 58 29 88 44 22 11 34 17 52 26 13 40 20 10 5 16 8 4 2 1)
; ********************************************************
 
; (Replace this comment with your procedure(s).)

(define (iterate start proc n)
  (let ([current_value (proc start)])
    (if (= n 1)
        current_value
        (flatten (append (list current_value) (iterate current_value proc (sub1 n)))))))

; ********************************************************
; ** problem 7 **

; Write a procedure

; (compound start proc test)

; which executes the function proc until test is true, beginning with
; the argument start, and using the result of the previous function as
; the argument for the next call.  It returns a list of all the
; results.  Thus, compound is pretty much like iterate above, except
; that instead of executing a given number of times, it conditionally
; executes until the given test is satisfied.

; To see how this might matter, consider the last example of iterate:

; (iterate 100 collatz 25) => '(50 25 76 38 19 58 29 88 44 22 11 34 17 52 26 13 40 20 10 5 16 8 4 2 1)

; Normally, a collatz series should stop when it reaches 1.  However,
; look what happens:

; (iterate 100 collatz 26) => 
; '(50 25 76 38 19 58 29 88 44 22 11 34 17 52 26 13 40 20 10 5 16 8 4 2 1 4)

; We can solve this problem with compound:

; (compound 100 collatz (lambda (x) (= x 1)))
; '(50 25 76 38 19 58 29 88 44 22 11 34 17 52 26 13 40 20 10 5 16 8 4 2 1)
; (compound 200 collatz (lambda (x) (= x 1)))
; '(100 50 25 76 38 19 58 29 88 44 22 11 34 17 52 26 13 40 20 10 5 16 8 4 2 1)
; (compound 256 collatz (lambda (x) (= x 1)))
; '(128 64 32 16 8 4 2 1)

; More examples:

; (compound 10 (lambda (n) (- n 1)) (lambda (n) (<= n 0))) => '(9 8 7 6 5 4 3 2 1 0)
; (compound 0 add5 (lambda (x) (> x 50))) => '(5 10 15 20 25 30 35 40 45 50 55)
; (compound 0 add5 (lambda (x) (>= x 50))) => '(5 10 15 20 25 30 35 40 45 50)
; (compound 2 (lambda (n) (* n 2)) (lambda (x) (>= x 50))) => '(4 8 16 32 64)

; ********************************************************
 
; (Replace this comment with your procedure(s).)

(define (compound start proc test)
  (let ([current_value (proc start)])
    (if (test current_value)
        current_value
        (flatten (append (list current_value)
                         (compound current_value proc test))))))


; ********************************************************
; ** problem 8 
; Write 

; (power-set lst)

; which treats the lst as a set and returns a list of all possible
; subsets.  Both top-level and element-wise order don't matter.  The
; test code below depends on order, but the grading program does not.

; Examples:

; Note: the following five cases demonstrate the recursive,
; set-building pattern of the algorithm.

; (power-set ‘()) => ‘(())
; (power-set ‘(1)) => ‘(() (1))
; (power-set ‘(1 2)) => ‘(() (1) (2) (1 2))
; (power-set ‘(1 2 3)) => ‘(() (1) (2) (1 2) (3) (1 3) (2 3) (1 2 3))
; (power-set ‘(1 2 3 4)) => ’(() (1) (2) (1 2) (3) (1 3) (2 3) (1 2 3) (4) (1 4) (2 4) (1 2 4) (3 4) (1 3 4) (2 3 4) (1 2 3 4))

; (define toppings '(onion peppers bacon sausage mushroom))
; (power-set toppings)
; '(() (mushroom) (sausage) (sausage mushroom) (bacon) (bacon mushroom)
; (bacon sausage) (bacon sausage mushroom) (peppers) (peppers mushroom)
; (peppers sausage) (peppers sausage mushroom) (peppers bacon) (peppers
; bacon mushroom) (peppers bacon sausage) (peppers bacon sausage
; mushroom) (onion) (onion mushroom) (onion sausage) (onion sausage
; mushroom) (onion bacon) (onion bacon mushroom) (onion bacon sausage)
; (onion bacon sausage mushroom) (onion peppers) (onion peppers
; mushroom) (onion peppers sausage) (onion peppers sausage mushroom)
; (onion peppers bacon) (onion peppers bacon mushroom) (onion peppers
; bacon sausage) (onion peppers bacon sausage mushroom))

; (Replace this comment with your procedures.)

(define (power-set lst)
  (cond
    ;if list is null, return listed null
    ([null? lst]
     (list '()))

    ;generate the subset list 
    ;append the subset list to the overall remainder list
    (else
     (let([recursive_remainder (power-set (cdr lst))])   
       (append recursive_remainder 
               (map (lambda (i)
                      (cons (car lst) i)) 
                    recursive_remainder))))))


; ********************************************************
; ** problem 9 
; 

; OK.  Now we are going to put power-set to use.  First, we give you a
; function to generate all the prime factors of a given positive
; integer.  (Last year I had the students write this function.)

(define (primes n)
  (define (sift list p)
    (filter (lambda (n)
              (not (zero? (modulo n p))))
            list))
  (define (iter nums primes)
    (let ((p (car nums)))
      (if (> (* p p) n)
          (append (reverse primes) nums)
          (iter (sift (cdr nums) p) (cons p primes)))))
  (iter (cdr (build-list n add1)) '()))

(define (divides? p q)
  (zero? (modulo q p)))

(define (prime-factors n)
  (let loop ((primes (primes n)))
    (cond ((memq n primes) (list n))
          ((divides? (car primes) n)
           (cons (car primes) (prime-factors (/ n (car primes)))))
          (else (loop (cdr primes))))))


; Use prime-factors to write the following procedure:

; (all-factors n) which generates a sorted list of all the factors of
; the positive integer n, without duplicates.

;; Note: racket has a remove-duplicates function

;; Hint: the factors of a positive number can be obtained from the
;; power set of the number's prime factors.

; Examples:

; (all-factors 20) => '(1 2 4 5 10 20)
; (all-factors 32) => '(1 2 4 8 16 32)
; (all-factors 97) => '(1 97)
; (all-factors 1000) => '(1 2 4 5 8 10 20 25 40 50 100 125 200 250 500 1000)
; (all-factors 30030) => '(1 2 3 5 6 7 10 11 13 14 15 21 22 26 30 33
;; 35 39 42 55 65 66 70 77 78 91 105 110 130 143 154 165 182 195 210 231
;; 273 286 330 385 390 429 455 462 546 715 770 858 910 1001 1155 1365
;; 1430 2002 2145 2310 2730 3003 4290 5005 6006 10010 15015 30030)


; (Replace this comment with your procedure(s).)

; Generate all factors  of n from a list of unique power-sets
; multiply a power-set together to get a factor
(define (find_factors factor_multiples)
  (map (lambda (i)
           (foldl * 1 i)) 
         factor_multiples))

(define (all-factors n)
  (let* ([factor_multiples
          (remove-duplicates (power-set (prime-factors n)))]
         [factors (find_factors factor_multiples)])
    (sort factors <)))
    

; ********************************************************
; ********  testing, testing. 1, 2, 3 ....
; ********************************************************

(define *testing-flag* #t)

(define (oldtest name got expected)
  (cond (*testing-flag*
	 (let* ((expected (if (procedure? expected)
			      (and (expected got) 'OK-TEST)
			      expected))
		(prefix (if (equal? got expected)
			    'OK
			    'X)))
	   (list 'testing name prefix 'got: got 'expected: expected)))))
	
(define (test name got expected)
  (cond (*testing-flag*
	 (let* ((OK (if (procedure? expected)
		    	(expected got)
			(equal? got expected)))
		(prefix (if OK
			    'OK
			    'X)))
	   (list 'testing name prefix 'got: got 'expected: expected)))))

(test 'hours hours (lambda (x) (> x 0)))
	
(test 'bin2dec (bin2dec '(1 0 1 0)) 10)
(test 'bin2dec (bin2dec '(1 1 1 1 1)) 31)
(test 'bin2dec (bin2dec '(1)) 1)
(test 'bin2dec (bin2dec '())  0)
(test 'bin2dec (bin2dec '(1 0 0 0 0 0 0 0)) 128)


(test 'dec2bin (dec2bin 10) '(1 0 1 0))
(test 'dec2bin (dec2bin 31) '(1 1 1 1 1))
(test 'dec2bin (dec2bin 23) '(1 0 1 1 1))
(test 'dec2bin (dec2bin 2) '(1 0))
(test 'dec2bin (dec2bin 128) '(1 0 0 0 0 0 0 0))
(test 'dec2bin (dec2bin 127) '(1 1 1 1 1 1 1))


(test 'hex2dec (hex2dec '(A)) 10)
(test 'hex2dec (hex2dec '(F F)) 255)
(test 'hex2dec (hex2dec '(1 0 0)) 256)
(test 'hex2dec (hex2dec '(d e a d b e e f)) 3735928559)

(test 'dec2hex (dec2hex 10) '(A))
(test 'dec2hex (dec2hex 255) '(F F))
(test 'dec2hex (dec2hex 256) '(1 0 0))
(test 'dec2hex (dec2hex 3735928559) '(D E A D B E E F))

(test 'my-remove (my-remove 2 '(1 2 3 2 4)) '(1 3 2 4))
(test 'my-remove (my-remove 2 '(1 2 3 2 4) =) '(1 3 2 4))
(test 'my-remove (my-remove '(2) '((1) (2) (3))) '((1) (3)))
(test 'my-remove (my-remove "2" '("1" "2" "3")) '("1" "3"))
(test 'my-remove (my-remove #\c '(#\a #\b #\c)) '(#\a #\b))
(test 'my-remove (my-remove 2 '(1 2 3 4) <) '(1 2 4))
(test 'my-remove (my-remove 2 '(1 2 3 4) >) '(2 3 4))

(test 'my-reverse (my-reverse '(1 2 3 4)) '(4 3 2 1))
(test 'my-reverse (my-reverse '(1)) '(1))
(test 'my-reverse (my-reverse '()) '())
(test 'my-reverse (my-reverse '((1) (2) (3))) '((3) (2) (1)))


(test 'sorted? (sorted? '(1 2 3 4) <) #t)
(test 'sorted? (sorted? '(1 2 3 4) >) #f)
(test 'sorted? (sorted? '(1 2 3 4 4) <) #f)
(test 'sorted? (sorted? '(1 1 1 1) =) #t)
(test 'sorted? (sorted? '(1 1 1 1) <) #f)
(test 'sorted? (sorted? '(1 1 1 1) <=) #t)
(test 'sorted? (sorted? '("a" "b" "c") string<=?) #t)
(test 'sorted? (sorted? '((1) (1 2) (1 2 3) (1 2 3 4)) (lambda (x y) (<= (length x) (length y)))) #t)
(test 'sorted? (sorted? '((1) (1 2) (1 2 3) (1 2 3 4) (1)) (lambda (x y) (<= (length x) (length y)))) #f)

(test 'sorted? (sorted? '(1 2 3 4)) #t)
(test 'sorted? (sorted? '(1 2 3 4 4 4)) #t)
(test 'sorted? (sorted? '(1 2 3 4 3 2 1)) #f)

(test 'inflate (inflate '(1 2 3)) '(2 3 4))
(test 'inflate (inflate '(1)) '(2))
(test 'inflate (inflate '()) '())
(test 'inflate (inflate '(a b c 2 3 4)) '(a b c 3 4 5))
(test 'inflate (inflate '((1) (2) (3))) '((1) (2) (3)))


(test 'iterate (iterate 2 add5 10) '(7 12 17 22 27 32 37 42 47 52))
(test 'iterate (iterate 0 (lambda (x) (+ x 1)) 3) '(1 2 3))
(test 'iterate (iterate 1 (lambda (n) (* n 2)) 10) '(2 4 8 16 32 64 128 256 512 1024))
(test 'iterate (iterate 1 (lambda (x) (* x -2)) 10) '(-2 4 -8 16 -32 64 -128 256 -512 1024))
(test 'iterate (iterate 10 (lambda (n) (- n 1)) 10) '(9 8 7 6 5 4 3 2 1 0))
(test 'iterate (iterate 3 (lambda (n) (+ n 2)) 10) '(5 7 9 11 13 15 17 19 21 23))
(test 'iterate (iterate 100 collatz 25) '(50 25 76 38 19 58 29 88 44 22 11 34 17 52 26 13 40 20 10 5 16 8 4 2 1))


(test 'compound (compound 100 collatz (lambda (x) (= x 1))) '(50 25 76 38 19 58 29 88 44 22 11 34 17 52 26 13 40 20 10 5 16 8 4 2 1))
(test 'compound (compound 200 collatz (lambda (x) (= x 1)))  '(100 50 25 76 38 19 58 29 88 44 22 11 34 17 52 26 13 40 20 10 5 16 8 4 2 1))
(test 'compound (compound 256 collatz (lambda (x) (= x 1)))  '(128 64 32 16 8 4 2 1))

(test 'compound (compound 10 (lambda (n) (- n 1)) (lambda (n) (<= n 0))) '(9 8 7 6 5 4 3 2 1 0))
(test 'compound (compound 0 add5 (lambda (x) (> x 50))) '(5 10 15 20 25 30 35 40 45 50 55))
(test 'compound (compound 0 add5 (lambda (x) (>= x 50))) '(5 10 15 20 25 30 35 40 45 50))
(test 'compound (compound 2 (lambda (n) (* n 2)) (lambda (x) (>= x 50))) '(4 8 16 32 64))

(test 'power-set (power-set '()) '(()))
(test 'power-set (power-set '(1)) '(() (1)))
(test 'power-set (power-set '(1 2)) '(() (2) (1) (1 2)))
(test 'power-set (power-set '(1 2 3)) '(() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3)))

(test 'all-factors (all-factors 20) '(1 2 4 5 10 20))
(test 'all-factors (all-factors 32) '(1 2 4 8 16 32))
(test 'all-factors (all-factors 97) '(1 97))
(test 'all-factors (all-factors 1000) '(1 2 4 5 8 10 20 25 40 50 100 125 200 250 500 1000))
(test 'all-factors (all-factors 30030) '(1 2 3 5 6 7 10 11 13 14 15 21 22 26 30 33 35 39 42 55 65 66 70 77 78 91 105 110 130 143 154 165 182 195 210 231 273 286 330 385 390 429 455 462 546 715 770 858 910 1001 1155 1365 1430 2002 2145 2310 2730 3003 4290 5005 6006 10010 15015 30030))



