#lang racket

(provide hours
         entry entry? entry-key entry-value
         ram-read ram-write diff-rams
         extract bits->int int->bits int->bits-width
         conf conf? conf-cpu conf-ram
         diff-configs incr-pc do-load do-store
         do-add do-sub
         do-input do-output
         do-jump do-skipzero do-skippos do-skiperr
         do-loadi do-storei do-shift
         do-and do-xor
         next-config
         init-config symbol-table assemble
         simulate encrypt-prog reverse-prog
         power-prog)

;************************************************************
; Name: Nataly Moreno-Martinez
; Email address: natybegins@gmail.com
;************************************************************

; Computer science topics: TC-201 assembler and simulator,
; assembly language programs for encrypt, reverse, and power.

; You may write auxiliary procedure(s) in addition to
; the one(s) specified in the problem.  Please include
; a comment for each one explaining its input and results.

;************************************************************
; ** problem 0 ** 
(define hours 16)

; ********************************************************
; ** problem 00 ** 
; Below is a UNIX transcript with one command replaced by XXXX

(define transcript "

bash-4.4$ pwd
/home/accts/sbs5/cs201/www/Fall_2019/lectures/test
bash-4.4$ ls
file  file2
bash-4.4$ xxxx
file: ASCII text
bash-4.4$ cat file
hello world
")

; define xxxx below to be the correct UNIX command.

(define xxxx "file file")


;************************************************************

; A table is a list of entries, where each entry has two fields: key and value.
; The constructor for entries is entry, the type predicate is entry?, and the
; two selectors are entry-key and entry-value.

(struct entry (key value) #:transparent)

; Random access memory (RAM)

; We represent the contents of a memory register as
; a list of 16 bits, each either 0 or 1.
; The contents of the RAM are represented as a list giving
; the contents of memory register 0, memory register 1,
; and so on, up to some address n, where n is at most 4095.
; Those memory registers whose contents are not explicitly
; listed are assumed to contain 16 zeroes.

; Examples of RAMs.

(define ram-ex1
  '((0 0 0 1  0 0 0 0  0 0 0 0  0 0 1 1)
    (0 0 1 0  0 0 0 0  0 0 0 0  0 1 0 0)
    (0 0 0 0  0 0 0 0  0 0 0 0  0 0 0 0)
    (0 0 0 0  0 0 0 0  0 0 0 0  1 0 1 0)
    (0 0 0 0  0 0 0 0  0 0 0 0  0 0 0 0)))
 
(define ram-ex2
  '((0 0 0 1  0 0 0 0  0 0 0 0  0 0 1 1)
    (0 0 1 0  0 0 0 0  0 0 0 0  0 1 0 1)
    (0 0 0 0  0 0 0 0  0 0 0 0  0 0 0 0)
    (0 0 0 0  0 0 0 0  0 0 1 0  0 1 0 0)))

;************************************************************
; ** problem 1 ** 
; Write three procedures

; (ram-read address ram)
; (ram-write address contents ram)
; (diff-rams ram1 ram2)

; (ram-read address ram)
; takes a memory address and a ram
; and returns a list of 16 bits giving the contents
; of the memory register in ram with the given address.

; (diff-rams ram1 ram2)
; takes two RAMs and returns a list indicating the memory registers 
; which have different contents in the two RAMs.
; The format of the list is a list of triples giving
; a memory address, the contents of that memory register
; in ram1, and the contents of that memory register
; in ram2.  The addresses should be in increasing order.

; (ram-write address contents ram)
; takes a memory address (address), a list of 16 bits (contents) and a ram,
; and returns a ram representing the result of copying the contents 
; into the memory register of ram specified by the memory address.

; Examples

;> (ram-read 0 ram-ex1)
;'(0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1)

;> (ram-read 6 ram-ex2)
;'(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)

;> (diff-rams ram-ex1 ram-ex2)
;'((1 (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0) (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 1))
;  (3 (0 0 0 0 0 0 0 0 0 0 0 0 1 0 1 0) (0 0 0 0 0 0 0 0 0 0 1 0 0 1 0 0)))

;> (diff-rams '() '((0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
;'()

;> (diff-rams ram-ex1 (ram-write 2 '(0 0 0 0 1 1 1 1 0 0 0 0 1 1 1 1) ram-ex1))
;'((2 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (0 0 0 0 1 1 1 1 0 0 0 0 1 1 1 1)))

;> (diff-rams ram-ex2 (ram-write 5 '(1 1 0 0 1 1 0 0 1 1 0 0 1 1 0 0) ram-ex2))
;'((5 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (1 1 0 0 1 1 0 0 1 1 0 0 1 1 0 0)))

;> (diff-rams ram-ex1 (ram-write 1 '(0 1 1 1 0 0 0 0 0 0 0 0 0 0 1 1) ram-ex1))
;'((1 (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0) (0 1 1 1 0 0 0 0 0 0 0 0 0 0 1 1)))

;************************************************************

(define (ram-read address ram)
  (cond
    [(> address 4095)
     "address out of range"]
    [(> address (length ram))
     '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)]
    [else
     (list-ref ram address)]))

(define (ram-write address contents ram)
  (let ([content (if (null? contents)
                      '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                      contents)])
      (if (= address 0)
          (cons content (if (null? ram)
                            '()
                            (cdr ram)))
          (if (null? ram)
              (cons '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                    (ram-write (sub1 address) content '()))
              (cons (car ram)
                    (ram-write (sub1 address) content (cdr ram)))))))

(define (diff-rams ram1 ram2)

  ; recursive helper function to list differences
  ; it goes through each ram list and updates n,
  ; where n corresponds to the address.
  ; The default ram address (in the case that one ram is
  ; is longer than the other) is 16 bits of 0's.
  (define (diff-rams-recursive ram1 ram2 n)
    (let ([default '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)])
     (cond
      [(and (null? ram1)
            (null? ram2))
       '()]
      [(null? ram1)
       (if (equal? (car ram2) default)
           (diff-rams-recursive ram1 (cdr ram2) (add1 n))
           (cons (list n default (car ram2))
              (diff-rams-recursive ram1 (cdr ram2) (add1 n))))]
      [(null? ram2)
       (if (equal? (car ram1) default)
           (diff-rams-recursive (cdr ram1) ram2 (add1 n))
           (cons (list n (car ram1) default)
             (diff-rams-recursive (cdr ram1) ram2 (add1 n))))]
      [(equal? (car ram1) (car ram2))
       (diff-rams-recursive (cdr ram1) (cdr ram2) (add1 n))]
      [else
       (cons (list n (car ram1) (car ram2))
             (diff-rams-recursive (cdr ram1) (cdr ram2) (add1 n)))])))
  
  ;run the recursive function
  (diff-rams-recursive ram1 ram2 0))

;************************************************************
; ** problem 2 **
; Write four procedures:

; (extract i j lst)
; (bits->int lst) 
; (int->bits n)
; (int->bits-width n w)

; (extract i j lst) 
; takes nonnegative integers i and j and a list lst
; and returns the list of elements of lst indexed i through j.
; You may assume i and j are at least 0 and less than the
; length of the list, and i is less than or equal to j.
; As in list-ref, list elements are indexed starting with 0.

; (bits->int lst) takes a list of bits lst
; and returns the value of the nonnegative number 
; represented in binary by those digits.

; (int->bits n) takes a nonnegative integer n
; and returns the list of bits representing n in 
; unsigned binary.
; Note that for 0 the answer is (0) but for
; all other numbers the answer starts with 1.

; (int->bits-width n w) takes a nonnegative integer n
; and returns a list of w bits representing n in 
; unsigned binary.
; If n cannot be correctly represented in binary using
; w bits, the string "field too small" should be returned.

; Examples

;> (extract 1 3 '(a b c d e))
;'(b c d)

;> (extract 4 4 '(a b c d e))
;'(e)

;> (bits->int '(0))
;0

;> (bits->int '(0 0 0 1 1 0))
;6

;> (int->bits 0)
;'(0)

;> (int->bits 6)
;'(1 1 0)

;> (int->bits-width 14 8)
;'(0 0 0 0 1 1 1 0)

;> (int->bits-width 14 3)
;"field too small"

;************************************************************

(define (extract i j lst)
  (if (= i j)
      (list (list-ref lst j))
      (cons (list-ref lst i) (extract (add1 i) j lst))))

(define (bits->int lst)
  (foldl (lambda (element base)
           (+ (* base 2) element))
         0
         lst))

(define (int->bits n)
  (let-values ([(q r) (quotient/remainder n 2)])
    (if (= q 0)
        (list r)
        (append (int->bits q) (list r)))))

(define (int->bits-width n w)
  (cond
    [(= w (length(int->bits n)))
     (int->bits n)]
    [(> w (length(int->bits n)))
     (cons 0 (int->bits-width n (sub1 w)))]
    [(< w (length(int->bits n)))
     "field too small"]))

;************************************************************
; Next we develop a simulator for the TC-201

; For the TC-201 Central Processing Unit (CPU), the contents of the registers 
; are represented by a table with entries giving the contents of the CPU 
; registers ** in this order **.

; the accumulator (acc)
; the program counter (pc)
; the run flag (rf)
; the arithmetic error bit (aeb)

; Each entry is a list containing 
; a symbol (one of 'acc, 'pc, 'rf, 'aeb)
; a list of bits of the correct length,
; namely, 16 bits for the acc, 12 bit for
; the pc, and 1 bit each for rf and aeb.

; Examples

(define cpu-ex1 
  (list
   (entry 'acc '(0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1))
   (entry 'pc '(0 0 0 0 0 0 0 0 0 1 1 1))
   (entry 'rf '(1))
   (entry 'aeb '(0))))

(define cpu-ex2 
  (list
   (entry 'acc '(1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1))
   (entry 'pc '(0 0 0 0 0 0 0 0 0 1 1 1))
   (entry 'rf '(1))
   (entry 'aeb '(1))))

; A configuration of the TC-201 is a struct with two fields:
; (1) the contents of the CPU registers, in the above format,
; (2) the contents of the RAM, in the format of problem 1.

(struct conf (cpu ram) #:transparent)

; Note that the constructor is conf, the type-predicate
; is conf?, and the selectors are conf-cpu, conf-ram.

; Examples

(define config-ex1 (conf cpu-ex1 ram-ex1))
(define config-ex2 (conf cpu-ex2 ram-ex2))

;************************************************************
; ** problem 3 *
; Write four procedures

; (diff-configs config1 config2)
; (incr-pc n config)
; (do-load address config)
; (do-store address config)

; (diff-configs config1 config2)
; takes two configurations and returns a list showing where they differ, 
; as a list of triples, giving the name (or address) of the
; register, the contents in config1 and the contents in config2.  
; The order should be CPU registers first (in order: acc, pc, rf, aeb) 
; and then memory registers in increasing order of addresses.

; (incr-pc n config)
; takes a nonnegative integer n and a TC-201 configuration config
; and returns the TC-201 configuration that is obtained by adding n 
; to the value of pc.  Note that the sum should be taken modulo 4096.  
; (Racket has a modulo procedure.)

; (do-load address config)
; takes a memory address and a TC-201 configuration, and returns the TC-201 
; configuration that is obtained by copying the contents
; of the given memory address into the accumulator.
; The values of all other registers (including the pc) are unchanged.

; (do-store address config)
; takes a memory address and a TC-201 configuration, and returns the TC-201 
; configuration that is obtained by copying the contents of the accumulator 
; into the given memory address.
; The values of all other registers (including the pc) are unchanged.

; Examples

;> (diff-configs config-ex1 config-ex2)
;'((acc (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1) (1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1))
;  (aeb (0) (1))
;  (1 (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0) (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 1))
;  (3 (0 0 0 0 0 0 0 0 0 0 0 0 1 0 1 0) (0 0 0 0 0 0 0 0 0 0 1 0 0 1 0 0)))

; The first result is shown in full -- you may produce an equivalent
; configuration.  Subsequent results are shown using diff-configs.

;> (incr-pc 1 config-ex1)
;(conf
; (list
;  (entry 'acc '(0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1))
;  (entry 'pc '(0 0 0 0 0 0 0 0 1 0 0 0))
;  (entry 'rf '(1))
;  (entry 'aeb '(0)))
; '((0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1)
;   (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0)
;   (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;   (0 0 0 0 0 0 0 0 0 0 0 0 1 0 1 0)
;   (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))

;> (diff-configs config-ex2 (incr-pc 4090 config-ex2))
;'((pc (0 0 0 0 0 0 0 0 0 1 1 1) (0 0 0 0 0 0 0 0 0 0 0 1)))

;> (diff-configs config-ex1 (do-load 1 config-ex1))
;'((acc (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1) (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0)))

;> (diff-configs config-ex2 (do-load 12 config-ex2))
;'((acc (1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1) (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))

;> (diff-configs config-ex1 (do-store 5 config-ex1))
;'((5 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1)))

;>  (diff-configs config-ex2 (do-store 0 config-ex2))
;'((0 (0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1) (1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1)))

;************************************************************

; Diff-cpus recursively lists differences between input cpus
; cpu1 and cpu2
(define (diff-cpus cpu1 cpu2)
  (cond
    [(null? cpu1)
     '()]
    [(not (list? cpu1))
     (if (equal? cpu1 cpu2)
         '()
         (list (entry-key cpu1)
               (entry-value cpu1)
               (entry-value cpu2)))]
    [(not (equal? (car cpu1)
                  (car cpu2)))
     (cons  (list (entry-key (car cpu1))
                  (entry-value (car cpu1))
                  (entry-value (car cpu2)))
            (diff-cpus (cdr cpu1) (cdr cpu2)))]
    [else
      (diff-cpus (cdr cpu1) (cdr cpu2))]))

(define (diff-configs config1 config2)
  (append (diff-cpus (conf-cpu config1) (conf-cpu config2))
          (diff-rams (conf-ram config1) (conf-ram config2))))

(define (incr-pc n config)
  ; convert pc to new-pc number, then output
  ; new conf
  (let* ([pc-n (bits->int (entry-value (second (conf-cpu config))))]
         [total (modulo (+ n pc-n) 4096)]
         [new-pc (int->bits-width-sm total 12)])
    (conf (list
           (car (conf-cpu config))
           (entry 'pc new-pc)
           (third (conf-cpu config))
           (entry 'aeb '(0)))
          (conf-ram config))))

(define (do-load address config)
  (let ([load-content (ram-read address (conf-ram config))])
    (conf
     (append
      (list (entry 'acc load-content))
      (cdr (conf-cpu config)))
     (conf-ram config))))

(define (do-store address config)
  (let ([store-content (entry-value (car (conf-cpu config)))])
    (conf
     (conf-cpu config)
     (ram-write address store-content (conf-ram config)))))
	   
;************************************************************
; ** problem 4 **
; Write two procedures

; (do-add address config)
; (do-sub address config)

; (do-add address config)
; takes a memory address and a TC-201 configuration
; and returns a TC-201 configuration in which
; the contents of the memory register addressed has
; been added to the contents of the accumulator.

; (do-sub address config) is similar, except that the
; contents of the memory register addressed has
; been subtracted from the contents of the accumulator.

; Note that if the result is zero, the answer should
; be +0, not -0.

; If the result can be correctly represented in 16-bit sign/magnitude
; then the arithmetic error bit (aeb) should also be set to 0.

; If the result cannot be correctly represented in 16-bit sign/magnitude
; then the arithmetic error bit (aeb) should also be set to 1.
; In this case, the result in the accumulator should be 
; 16 zeroes, representing +0.

; The contents of registers other than the accumulator and the
; arithmetic error bit should be unchanged.

; Examples

;> (diff-configs config-ex1 (do-add 3 config-ex1))
;'((acc (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1) (0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 1)))

;> (diff-configs config-ex2 (do-add 3 config-ex2))
;'((acc (1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1) (0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 1))
;  (aeb (1) (0)))

;> (diff-configs config-ex1 (do-sub 3 config-ex1))
;'((acc (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1) (0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 1)))

;> (diff-configs config-ex2 (do-sub 3 config-ex2))
;'((acc (1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1) (1 0 0 0 0 0 0 0 0 0 1 0 0 1 1 1))
;  (aeb (1) (0)))

;>  (let ((config (conf cpu-ex1 '((0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1))))) (diff-configs config (do-add 0 config)))
;'((acc (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1) (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
;  (aeb (0) (1)))

;************************************************************

; Sign-magnitude conversion of bits to integers
(define (bits->int-sm n)
  (let ([sign (car n)]
        [remainder (cdr n)])
    (if (= sign 1)
        (- 0 (bits->int remainder))
        (bits->int remainder))))

; int->bits-width but with sign magnitude taken into account
(define (int->bits-width-sm n w)
  (if (< n 0)
      (let ([result (int->bits-width (- 0 n) w)])
        (if (zero? (car result))
            (cons 1 (cdr result))
            "field too small"))
      (if (equal? (car (int->bits-width n w)) 1)
          "field too small"
          (int->bits-width n w))))

(define (do-add address config)
  ;first, calculate the new acc by adding the acc and address together
  (let ([new-acc (int->bits-width-sm
                  (+ (bits->int-sm (entry-value (car (conf-cpu config))))
                     (bits->int-sm (ram-read address (conf-ram config))))
                  16)])
    ;then, output new config
    (if (equal? new-acc "field too small")
        (conf (list
               (entry 'acc (int->bits-width 0 16))
               (second (conf-cpu config))
               (third (conf-cpu config))
               (entry 'aeb '(1)))
              (conf-ram config))
        (conf (list
               (entry 'acc new-acc)
               (second (conf-cpu config))
               (third (conf-cpu config))
               (entry 'aeb '(0)))
              (conf-ram config)))))

(define (do-sub address config)
  (let* ([total (- (bits->int-sm (entry-value (car (conf-cpu config))))
                   (bits->int-sm (ram-read address (conf-ram config))))])
    (cond
      [(< total 0)
       ; if the bit representation of the subtraction is < 16 bits
       ; long and negative, make it a 16 bit number representation and
       ; print config. Else, return acc = 0 and aeb = 1.
       (if (< (length (int->bits (- 0 total)))
              16)
           (let ([new-acc (cons 1
                                (int->bits-width (- 0 total) 15))])
             (conf
              (list
               (entry 'acc new-acc)
               (second (conf-cpu config))
               (third (conf-cpu config))
               (entry 'aeb '(0)))
              (conf-ram config)))
           (conf
              (list
               (entry 'acc (int->bits-width 0 16))
               (second (conf-cpu config))
               (third (conf-cpu config))
               (entry 'aeb '(1)))
              (conf-ram config)))]
      ;if the length is greater than 16, provide error
      [(> (length(int->bits total)) 16)
       (conf
        (list
         (entry 'acc (int->bits-width 0 16))
         (second (conf-cpu config))
         (third (conf-cpu config))
         (entry 'aeb '(1)))
        (conf-ram config))]
      [else
       (conf
        (list
         (entry 'acc (int->bits-width total 16))
         (second (conf-cpu config))
         (third (conf-cpu config))
         (entry 'aeb '(0)))
        (conf-ram config))])))

;************************************************************
; ** problem 5 ** 
; Write two procedures

; (do-input config)
; (do-output config)

; Each takes a TC-201 configuration and performs the appropriate action 
; (reading a number from the user or writing a number out to the user)
; and *returns* the resulting TC-201 configuration.

; For input, the new configuration has the value read in the 
; accumulator, and all other registers unchanged.
; To read in a value, you may use the following
; let construct:
; (let ((value (begin (display "input = ") (read)))) ...)

; To ensure the number typed by the user is in the correct range, 
; you may take its remainder on division by 2^(15).

; For output, the new configuration is returned *unchanged*.  
; If the integer value from the accumulator is in 
; value-from-accumulator, then the output to the user can be 
; produced by:

; (display "output = ")
; (display value-from-accumulator)
; (newline)

; Examples (these show how the interaction looks)

; The lines input = .. and output = .. show the interaction between 
; TC-201 and user.  The TC-201 configuration shows the value
; returned by the procedure.

;> (diff-configs config-ex1 (do-input config-ex1))
;input = 22
;'((acc (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1) (0 0 0 0 0 0 0 0 0 0 0 1 0 1 1 0)))

;> (diff-configs config-ex1 (do-output config-ex1))
;output = 15
;'()

;************************************************************
(define (do-input config)
  (let ([value (begin (display "input = ")
                      (read))])
    ;write value to the accumulator
    (conf
     (cons
      (entry 'acc (int->bits-width-sm value 16))
      (cdr (conf-cpu config)))
     (conf-ram config))))

(define (do-output config)
  (let ([value-from-accumulator (bits->int-sm (entry-value (second (conf-cpu config))))])
    (display "output = ")
    (display value-from-accumulator)
    (newline)
    config))

;************************************************************
; ** problem 6 **
; Write four procedures

; (do-jump address config)
; (do-skipzero config)
; (do-skippos config)
; (do-skiperr config)


; (do-jump address config)
; takes a memory address and a TC-201 configuration, and
; returns a TC-201 configuration in which the program counter
; (pc) is set to the given address.  All other registers are
; unaffected.

; (do-skipzero config)
; takes a TC-201 configuration config and returns
; a TC-201 configuration in which the program counter (pc)
; is increased by 2 if the accumulator contains +0 or -0,
; and is increased by 1 otherwise.  All registers other than
; the pc are unaffected.

; (do-skippos config)
; takes a TC-201 configuration config and returns
; a TC-201 configuration in which the program counter (pc)
; is increased by 2 if the accumulator contains a nonzero
; positive number, and is increased by 1 otherwise.  
; All registers other than the pc are unaffected.

; (do-skiperr config)
; takes a TC-201 configuration config and returns
; a TC-201 configuration in which the program counter (pc)
; is increased by 2 if the arithmetic error bit contains 1
; and is increased by 1 if the arithmetic error bit contains 0.
; In either case, in the new configuration, the arithmetic
; error bit is set to 0.
; All registers other than the pc and the aeb are unaffected.

; Examples

;> (diff-configs config-ex1 (do-jump 5 config-ex1))
;'((pc (0 0 0 0 0 0 0 0 0 1 1 1) (0 0 0 0 0 0 0 0 0 1 0 1)))

;> (diff-configs config-ex2 (do-skipzero config-ex2))
;'((pc (0 0 0 0 0 0 0 0 0 1 1 1) (0 0 0 0 0 0 0 0 1 0 0 0)))

;> (diff-configs config-ex1 (do-skippos config-ex1))
;'((pc (0 0 0 0 0 0 0 0 0 1 1 1) (0 0 0 0 0 0 0 0 1 0 0 1)))

;> (diff-configs config-ex2 (do-skiperr config-ex2))
;'((pc (0 0 0 0 0 0 0 0 0 1 1 1) (0 0 0 0 0 0 0 0 1 0 0 1)) (aeb (1) (0)))

;************************************************************

(define (do-jump address config)
  (cond
    [(< address 0)
     "nonreal address"]
    [(null? address)
     "usage: (do-jump < address number > < config >)"]
    [else
     (let ([ad (int->bits-width address 12)])
       (conf
        (list
         (car (conf-cpu config))
         (entry 'pc ad)
         (third (conf-cpu config))
         (fourth (conf-cpu config)))
        (conf-ram config)))]))

; returns the entry of a cpu field in config
(define (field-generator field config)
  (let ([cpu (conf-cpu config)])
    (cond
      [(equal? field 'acc)
       (first cpu)]
      [(equal? field 'pc)
       (second cpu)]
      [(equal? field 'rf)
       (third cpu)]
      [(equal? field 'aeb)
       (last cpu)])))

(define (do-skipzero config)
  (let ([pc (field-generator 'pc config)])
    (cond
      ; positive 0
      [(= (bits->int (entry-value pc)) 0)
       (incr-pc 2 config)]
      ; negative 0
      [(and (= (car (entry-value pc)) 1)
            (= (bits->int (cdr (entry-value pc))) 0))
       (let ([new-config (conf
                          (list
                           (car (conf-cpu config))
                           (entry 'pc (int->bits-width 2 12))
                           (third (conf-cpu config))
                           (fourth (conf-cpu config)))
                          (conf-ram config))])
         (new-config))]
      [else
       (incr-pc 1 config)])))

(define (do-skippos config)
  (let ([acc-value (entry-value (field-generator 'acc config))])
   (cond
    [(and (= (car acc-value) 0)
          (> (bits->int (cdr acc-value)) 0))
     (incr-pc 2 config)]
    [else
     (incr-pc 1 config)])))

(define (do-skiperr config)
   (let ([aeb-value (entry-value (field-generator 'aeb config))])
   (if (= (car aeb-value) 1)
       (incr-pc 2 config)
       (incr-pc 1 config))))
           
;************************************************************
; ** problem 7 **
; Write three procedures

; (do-loadi address config)
; (do-storei address config)
; (do-shift address config)

; (do-loadi address config)
; takes a memory address and a TC-201 configuration and returns a TC-201 
; configuration that reflects the result of doing a "load indirect" from the
; given memory address to the accumulator.
; That is, the low-order 12 bits of the contents of the memory register 
; addressed by address are extracted and used as the memory address
; from which the contents are loaded into the accumulator.
; All other registers are unaffected.

; (do-storei address config)
; takes a memory address and a TC-201 configuration and returns a TC-201 
; configuration that reflects the result of doing a "store indirect" to the
; given memory address from the accumulator.
; That is, the low-order 12 bits of the contents of the memory register 
; addressed by address are extracted and used as the memory address
; to which the contents of the accumulator are copied.
; All other registers are unaffected.

; (do-shift address config)
; takes a memory address and a TC-201 configuration and returns a TC-201 
; configuration that reflects the result of doing a shift of accumulator
; left or right by the number of bits given in the specified memory address.
; A positive number shifts the accumulator to the left.
; A negative number shifts the accumulator to the right.


; This example is useful for loadi and storei testing.

(define ram-ex3
  '((0 0 0 0  0 0 0 0  0 0 0 0  0 1 1 0)
    (1 1 1 1  0 0 0 0  0 0 0 0  0 1 0 1)
    (0 0 0 0  0 0 0 0  0 0 0 0  0 1 0 0)
    (0 0 0 0  0 0 0 0  0 0 0 0  1 0 0 0)
    (0 0 0 0  1 1 1 1  0 0 0 0  1 1 1 1)
    (0 1 0 1  0 1 0 1  0 1 0 1  0 1 0 1)
    (1 0 0 0  0 0 0 0  0 0 0 0  0 0 1 0)))

(define config-ex3 (conf cpu-ex1 ram-ex3))

; Examples

;> (diff-configs config-ex3 (do-loadi 1 config-ex3))
;'((acc (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1) (0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1)))

;> (diff-configs config-ex3 (do-loadi 2 config-ex3))
;'((acc (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1) (0 0 0 0 1 1 1 1 0 0 0 0 1 1 1 1)))

;> (diff-configs config-ex3 (do-storei 1 config-ex3))
;'((5 (0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1) (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1)))

;> (diff-configs config-ex3 (do-storei 2 config-ex3))
;'((4 (0 0 0 0 1 1 1 1 0 0 0 0 1 1 1 1) (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1)))

;> (diff-configs config-ex3 (do-shift 2 config-ex3))
; '((acc (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1) (0 0 0 0 0 0 0 0 1 1 1 1 0 0 0 0)))

;> (diff-configs config-ex3 (do-shift 3 config-ex3))
;'((acc (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1) (0 0 0 0 1 1 1 1 0 0 0 0 0 0 0 0)))

;> (diff-configs config-ex3 (do-shift 6 config-ex3))
;'((acc (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1) (0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1)))
;************************************************************

(define (do-loadi address config)
  (let* ([mem-add (list-tail (ram-read address (conf-ram config)) 4)]
         [n (bits->int mem-add)])
    (do-load n config)))

(define (do-storei address config)
  (let* ([mem-add (list-tail (ram-read address (conf-ram config)) 4)]
         [n (bits->int mem-add)])
    (do-store n config)))

; returns the first n elements of a list
(define list-head
  (lambda (lst n)
    (if (= 0 n)                    
        '()                     
        (cons (car lst)             
              (list-head (cdr lst)     
                         (sub1 n)))))) 

(define (do-shift address config)
  (let* ([mem-shift (bits->int-sm (ram-read address (conf-ram config)))]
         [acc (entry-value (field-generator 'acc config))])
    (cond
      [(< mem-shift 0)
       (conf
        (cons
         (entry 'acc
                (list-head
                 (append (int->bits-width 0 (- 0 mem-shift)) acc)
                 16))
         (cdr (conf-cpu config)))
        (conf-ram config))]
      [else
       (conf
        (cons
         (entry 'acc
                (list-tail
                 (append acc (int->bits-width 0 mem-shift))
                 mem-shift))
         (cdr (conf-cpu config)))
        (conf-ram config))])))

;************************************************************
; ** problem 8 **
; Write two procedures

; (do-and address config)
; (do-xor address config)

; (do-and address config)
; takes a memory address and a TC-201 configuration and returns a
; TC-201 configuration that reflects the result of doing and of the
; contents of the given memory address and the accumulator.  The
; result is stored in the accumulator.  All other registers are
; unaffected.


; (do-xor address config)
; takes a memory address and a TC-201 configuration and returns a
; TC-201 configuration that reflects the result of doing an exclusive
; or of the contents of the given memory address and the accumulator.
; The result is stored in the accumulator.
; All other registers are unaffected.

; Examples:

;> (diff-configs config-ex2 (do-and 1 config-ex2))
;'((acc (1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1) (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)))

;> (diff-configs config-ex3 (do-and 1 config-ex3))
;'((acc (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1) (0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 1)))

;> (diff-configs config-ex3 (do-xor 1 config-ex3))
;'((acc (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1) (1 1 1 1 0 0 0 0 0 0 0 0 1 0 1 0)))

;> (diff-configs config-ex3 (do-xor 5 config-ex3))
; '((acc (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1) (0 1 0 1 0 1 0 1 0 1 0 1 1 0 1 0)))
;************************************************************

(define (do-and address config)
  (let* ([acc (entry-value (field-generator 'acc config))]
         [memory (ram-read address (conf-ram config))]
         [and-results (map (lambda(i v)
                             (if (false? (and
                                          (equal? i v)
                                          (= i 1)))
                                 0
                                 1))
                           acc
                           memory)])
    (conf
     (cons
      (entry 'acc and-results)
      (cdr (conf-cpu config)))
     (conf-ram config))))

(define (do-xor address config)
  (let* ([acc (entry-value (field-generator 'acc config))]
         [memory (ram-read address (conf-ram config))]
         [xor-results (map (lambda(i v)
                             (if (false? (equal? i v))
                                 1
                                 0))
                           acc
                           memory)])
    (conf
     (cons
      (entry 'acc xor-results)
      (cdr (conf-cpu config)))
     (conf-ram config))))

;************************************************************
; ** problem 9 **
; Write one procedure

; (next-config config)

; that takes a TC-201 configuration and returns the next TC-201 configuration,
; after one iteration of the fetch/execute cycle.

; If the run flag (rf) is 0, then the configuration config is returned unchanged,
; because the machine is halted.

; The instructions that should be implemented are:

; halt, load, store, add, sub, input, output, jump
; skipzero, skippos, skiperr, loadi, storei, shift, and, xor.

; These are opcodes 0000 through 1111, respectively.

; For a halt instruction, in the returned configuration the run flag is 0 and all
; other registers are unchanged.

; Otherwise, the program counter (pc) contains a memory address, and the TC-201 
; instruction at that location is fetched and executed, and the resulting 
; configuration is returned.  Note that all instructions result in a configuration
; being returned, even input and output.

; This example is useful for testing next-config.

(define cpu-ex4
  (list
   (entry 'acc '(0 0 0 0  0 0 0 0  0 0 0 0  0 0 0 0))
   (entry 'pc  '(0 0 0 0  0 0 0 0  0 0 0 0))
   (entry 'rf '(1))
   (entry 'aeb '(0))))

(define ram-ex4
  '((0 0 0 1  0 0 0 0  0 0 0 0  0 0 1 1)
    (0 0 1 0  0 0 0 0  0 0 0 0  0 1 0 0)
    (0 0 0 0  0 0 0 0  0 0 0 0  0 0 0 0)
    (0 0 0 0  0 0 0 0  0 0 0 1  0 1 0 1)
    (1 1 1 1  1 1 1 1  0 0 0 0  0 0 0 0)))

(define config-ex4 (conf cpu-ex4 ram-ex4))

; Examples
; (Your configurations may be equivalent.)

;> (next-config config-ex4)
;(conf
; (list
;  (entry 'acc '(0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1))
;  (entry 'pc '(0 0 0 0 0 0 0 0 0 0 0 1))
;  (entry 'rf '(1))
;  (entry 'aeb '(0)))
; '((0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1)
;   (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0)
;   (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;   (0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1)
;   (1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0)))

;> (next-config (next-config config-ex4))
;(conf
; (list
;  (entry 'acc '(0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1))
;  (entry 'pc '(0 0 0 0 0 0 0 0 0 0 1 0))
;  (entry 'rf '(1))
;  (entry 'aeb '(0)))
; '((0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1)
;   (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0)
;   (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;   (0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1)
;   (0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1)))

;> (next-config (next-config (next-config config-ex4)))
;(conf
; (list
;  (entry 'acc '(0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1))
;  (entry 'pc '(0 0 0 0 0 0 0 0 0 0 1 0))
;  (entry 'rf '(0))
;  (entry 'aeb '(0)))
; '((0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1)
;   (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0)
;   (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;   (0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1)
;   (0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1)))

;************************************************************

(define (next-config config)
  (let* ([operations (list do-load do-store do-add do-sub do-input do-output
                          do-jump do-skipzero do-skippos do-skiperr
                          do-loadi do-storei do-shift do-and do-xor)]
         ;operations which only take in config as input
         [single-input-operations (list do-input do-output
                                        do-skipzero 
                                        do-skippos
                                        do-skiperr)]
         [operation (ram-read (bits->int (entry-value
                                        (field-generator 'pc config)))
                            (conf-ram config))]
         [oppcode (list-head operation 4)]
         [oppaddress (list-tail operation 4)])
   (cond
    [(= (car (entry-value (field-generator 'rf config))) 0)
     config]
    [(equal? '(0 0 0 0) oppcode)
     (conf
      (list
       (car (conf-cpu config))
       (second (conf-cpu config))
       (entry 'rf '(0))
       (last (conf-cpu config)))
      (conf-ram config))]
    [else
     (if (false? (member (list-ref operations (- (bits->int oppcode) 1))
                         single-input-operations))
      (incr-pc 1
              ((list-ref operations (- (bits->int oppcode) 1))
               (bits->int oppaddress)
               config))
      (incr-pc 1
              ((list-ref operations (- (bits->int oppcode) 1))
               config)))])))

;************************************************************
; ** problem 10 **
; Write three procedures

; (init-config lst)
; (symbol-table prog)
; (assemble prog)

; (init-config lst)
; takes a list lst of 16 bit patterns, and returns a TC-201 configuration 
; in which those patterns are loaded into RAM starting with address 0, 
; and the CPU registers are initialized so that the accumulator has
; value +0, the program counter has address 0, the run flag has 
; value 1, and the arithmetic error bit has value 0.

; (symbol-table prog)
; takes a TC-201 assembly language program prog (in the format specified below) 
; and returns a table of entries in which the key is a symbol that is a label 
; in prog and the value is the corresponding memory address for that
; instruction or data value (when the program is loaded into memory starting 
; at address 0.)  

; The addresses in the table should be in increasing order.

; (assemble prog)
; translates a TC-201 assembly language program prog 
; into a list of 16-bit patterns to be loaded into the TC-201 memory.

; The symbolic opcodes are: halt, load, store, add, sub, input, output
; jump, skipzero, skippos, skiperr, loadi, storei, shift, and, xor.

; There is also a data statement.

; An assembly language program is a list of "lines", where
; each line is a list of two or three elements representing
; an instruction or a data statement.  If the line has
; three elements, the first one is a symbolic label that
; should appear in the symbol table for the program.
; The remaining two elements (or the only two elements,
; if the line has just two elements) are either a symbol
; representing an opcode and an address, or the symbol 'data
; and a data value.  The address field of an instruction may
; be a number in the range 0 to 4095 inclusive, or a symbolic
; label, in which case the address is the numeric value of the
; label in the symbol table.  The value field of a data statement
; may be a number in the range -32767 to 32767 inclusive, or
; a symbolic label, in which case the value used is the numeric
; value of the label in the symbol table.

; You may assume that numeric addresses and data values will
; be in the correct ranges.

; Note that even instructions like halt, input, and skipzero, which
; ignore their address fields, must have an address specified.
; (A typical choice is 0 for the address fields of such instructions.)

; Example TC-201 assembly language programs

; a program with only instructions, numeric addresses, and no labels

(define prog1
  '((load 3)
    (store 4)
    (halt 0)))


; a program with only data statements, three labels, and both numeric
; and symbolic data values

(define prog2
  '((x data 1)
    (y data -2)
    (z data y)))

; a version of the program we wrote in lecture to sum up
; a zero-terminated sequence of numbers, output the sum, and halt.

(define prog3
  '((start  load constant-zero)
    (        store sum)
    (next    input 0)
    (        skipzero 0)
    (        jump add-num)
    (        load sum)
    (        output 0)
    (        halt 0)
    (add-num add sum)
    (        store sum)
    (        jump next)
    (sum     data 0)
    (constant-zero data 0)))

; Examples of init-config, symbol-table and assemble

;> (init-config ram-ex2)
;(conf
; (list (entry 'acc '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)) 
;       (entry 'pc '(0 0 0 0 0 0 0 0 0 0 0 0)) 
;       (entry 'rf '(1)) 
;       (entry 'aeb '(0)))
; '((0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1)
;   (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 1)
;   (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;   (0 0 0 0 0 0 0 0 0 0 1 0 0 1 0 0)))

;> (symbol-table prog1)
;'()

;> (symbol-table prog2)
;(list (entry 'x 0) (entry 'y 1) (entry 'z 2))

;> (symbol-table prog3)
;(list
; (entry 'start 0)
; (entry 'next 2)
; (entry 'add-num 8)
; (entry 'sum 11)
; (entry 'constant-zero 12))

;> (assemble prog1)
;'((0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1)
;  (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0)
;  (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))

;> (assemble prog2)
;'((0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)
;  (1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0)
;  (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1))

;> (assemble prog3)
;'((0 0 0 1 0 0 0 0 0 0 0 0 1 1 0 0)
;  (0 0 1 0 0 0 0 0 0 0 0 0 1 0 1 1)
;  (0 1 0 1 0 0 0 0 0 0 0 0 0 0 0 0)
;  (1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;  (0 1 1 1 0 0 0 0 0 0 0 0 1 0 0 0)
;  (0 0 0 1 0 0 0 0 0 0 0 0 1 0 1 1)
;  (0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0)
;  (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;  (0 0 1 1 0 0 0 0 0 0 0 0 1 0 1 1)
;  (0 0 1 0 0 0 0 0 0 0 0 0 1 0 1 1)
;  (0 1 1 1 0 0 0 0 0 0 0 0 0 0 1 0)
;  (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;  (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))

;************************************************************

; initial configuration construction

(define (init-config lst)
  (conf
   (list
    (entry 'acc (int->bits-width 0 16))
    (entry 'pc (int->bits-width 0 12))
    (entry 'rf '(1))
    (entry 'aeb '(0)))
   lst))

;list of oppcodes
(define oppcodes '(halt load store add sub input output
                        jump skipzero skippos skiperr loadi
                        storei shift and xor))

; symbol table construction
(define (symbol-table prog)
  (define (symbol-table-recursive prog n)
    (cond
      [(null? prog)
       '()]
      [(= (length(car prog)) 3)
       (cons
        (entry (caar prog) n)
        (symbol-table-recursive (cdr prog) (add1 n)))]
      [else
       (symbol-table-recursive (cdr prog) (add1 n))]))
  (symbol-table-recursive prog 0))



; assemble program
(define (assemble prog)
  ; assemble i recursively attaches the result of
  ; assembling prog value n to total assemble list,
  ; where n goes from 1 to length prog
 (define (assemble-i prog n)
   (let ([current-prog (if (= n (length prog))
                           '()
                           (list-ref prog n))]
         [symbol-tab (symbol-table prog)])
     (cond
       [(null? current-prog)
        '()]
       ; if the current prog's length is 2,
       ; check if a data or protocol and evaluate
       [(= (length current-prog) 2)
        (cond
          ; if data, evaluate
          [(and (equal? (car current-prog) 'data)
                (number? (second current-prog)))
           (cons
            (int->bits-width-sm (second current-prog) 16)
            (assemble-i prog (add1 n)))]
          [(and (equal? (car current-prog) 'data)
                (symbol? (second current-prog)))
           (cons
            (int->bits-width-sm
             (entry-value (search-table symbol-tab (second current-prog)))
             16)
            (assemble-i prog (add1 n)))]
          ;if protocol, evaluate
          [else
           (if (number? (last current-prog))
               (cons
                (append
                 (entry-value (search-table opcode-table (car current-prog)))
                 (int->bits-width-sm (last current-prog) 12))
                (assemble-i prog (add1 n)))
               (cons
                (append
                 (entry-value (search-table opcode-table (car current-prog)))
                 (int->bits-width-sm
                  (entry-value (search-table symbol-tab (second current-prog)))
                  12))
                (assemble-i prog (add1 n))))])]
       ;if length = 3, evaluate the symbol
       [else
        (let ([current-prog (cdr current-prog)])
            (cond
          ; if data, evaluate
              [(and (equal? (car current-prog) 'data)
                    (number? (second current-prog)))
               (cons
                (int->bits-width-sm (second current-prog) 16)
                (assemble-i prog (add1 n)))]
              [(and (equal? (car current-prog) 'data)
                    (symbol? (second current-prog)))
               (cons
                (int->bits-width-sm
                 (entry-value (search-table symbol-tab (second current-prog)))
                 16)
                (assemble-i prog (add1 n)))]
              ;if protocol, evaluate
              [else
               (if (number? (last current-prog))
                   (cons
                    (append
                     (entry-value (search-table opcode-table (car current-prog)))
                     (int->bits-width-sm (last current-prog) 12))
                    (assemble-i prog (add1 n)))
                   (cons
                    (append
                     (entry-value (search-table opcode-table (car current-prog)))
                     (int->bits-width-sm
                      (entry-value (search-table symbol-tab (second current-prog)))
                      12))
                    (assemble-i prog (add1 n))))]))])))
  (assemble-i prog 0))

; function to search an table and return the entry
(define (search-table table symbol)
  (cond
    [(equal? symbol (entry-key (car table)))
      (car table)]
    [(null? table)
     '()]
    [else
     (search-table (cdr table) symbol)]))
 
; table of symbolic opcodes

(define opcode-table
  (list
   (entry 'halt '(0 0 0 0))
   (entry 'load '(0 0 0 1))
   (entry 'store '(0 0 1 0))
   (entry 'add '(0 0 1 1))
   (entry 'sub '(0 1 0 0))
   (entry 'input '(0 1 0 1))
   (entry 'output '(0 1 1 0))
   (entry 'jump '(0 1 1 1))
   (entry 'skipzero '(1 0 0 0))
   (entry 'skippos '(1 0 0 1))
   (entry 'skiperr '(1 0 1 0))
   (entry 'loadi '(1 0 1 1))
   (entry 'storei '(1 1 0 0))	
   (entry 'shift '(1 1 0 1))
   (entry 'and '(1 1 1 0))
   (entry 'xor '(1 1 1 1))))

;************************************************************
; ** problem 11 **
; Write one procedure and one program for the TC-201

; (simulate n config)
; encrypt-prog

; (simulate n config)
; simulates the TC-201 computer from the configuration config until
; either it halts (the run flag is 0) or n iterations of the fetch/execute
; cycle have been performed, whichever is first.
; The result returned should be a list of the successive configurations 
; of the TC-201 starting with the config.

; You are strongly advised to use your simulate procedure to help you test 
; your implementation of the instructions more extensively than the test cases 
; in the assignment.

; encrypt-prog
; reads in a positive integer from the user, which is the encryption
; key.  Then it loops, reading in a positive integer and outputting
; that integer xor'd with the key.  The loop continues until the user
; enters a non-positive integer.

; Examples 
; (This program stops after executing 3 instructions, returning
; 4 configurations, including the initial one.)

;> (simulate 5 config-ex4)
;(list
; (conf
;  (list
;   (entry 'acc '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
;   (entry 'pc '(0 0 0 0 0 0 0 0 0 0 0 0))
;   (entry 'rf '(1))
;   (entry 'aeb '(0)))
;  '((0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1)
;    (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0)
;    (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;    (0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1)
;    (1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0)))
; (conf
;  (list
;   (entry 'acc '(0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1))
;   (entry 'pc '(0 0 0 0 0 0 0 0 0 0 0 1))
;   (entry 'rf '(1))
;   (entry 'aeb '(0)))
;  '((0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1)
;    (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0)
;    (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;    (0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1)
;    (1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0)))
; (conf
;  (list
;   (entry 'acc '(0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1))
;   (entry 'pc '(0 0 0 0 0 0 0 0 0 0 1 0))
;   (entry 'rf '(1))
;   (entry 'aeb '(0)))
;  '((0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1)
;    (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0)
;    (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;    (0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1)
;    (0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1)))
; (conf
;  (list
;   (entry 'acc '(0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1))
;   (entry 'pc '(0 0 0 0 0 0 0 0 0 0 1 0))
;   (entry 'rf '(0))
;   (entry 'aeb '(0)))
;  '((0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1)
;    (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0)
;    (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;    (0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1)
;    (0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1))))

; Examples of run-time behavior of encrypt-prog interacting 
; with user.  We "capture" the returned list of configurations 
; by assigning it to be the value of the symbol results.

;; below the key is 13 (1101).  Note that encrypt is also decrypt.
; > (define results (simulate 100 (init-config (assemble encrypt-prog))))
; input = 13
; input = 8
; output = 5
; input = 15
; output = 2
; input = 2
; output = 15
; input = 5
; output = 8
; input = 0

; > (define results (simulate 100 (init-config (assemble encrypt-prog))))
; input = 511
; input = 78
; output = 433
; input = 999
; output = 536
; input = 536
; output = 999
; input = 433
; output = 78
; input = 0


;************************************************************

(define (simulate n config)
  (cond
    [(equal? (entry-value (field-generator 'rf config)) '(0))
     (list config)]
    [(= n 0)
     '()]
    [else
     (append (list config) (simulate (sub1 n) (next-config config)))]))

;computes xor of two positive numbers
(define (xor-positive key1 key2)
  (if (and (> key1 0)
           (> key2 0))
      (bits->int-sm
       (map (lambda(i v)
              (if (false? (equal? i v))
                  1
                  0))
            (int->bits-width key1 16)
            (int->bits-width key2 16)))
      '()))

;displays xor of 2 positive inputs
(define (xor-display-and-continue key1 key2)
  (display "output =")
  (display  (xor-positive key1 key2))
  (newline)
  ;return as a data list to be read for assemble
  (cons
   (list 'data (xor-positive key1 key2))
   (encrypt-loop key1)))

; loop portion of encrypt-prog
(define (encrypt-loop key1)
  (let* ([key2-new (begin (display "input = ")
                          (read))])
    (if (> key2-new 0)
        (xor-display-and-continue key1 key2-new)
        '())))
    

(define encrypt-prog
  (let ([key1 (begin (display "input = ")
                      (read))])
      (if (> key1 0)
          (let ([key2 (begin (display "input = ")
                      (read))])
            (xor-display-and-continue key1 key2)
            '())
          '())))

;************************************************************
; ** problem 12 ** 
; Write one program for the TC-201

; reverse-prog

; that reads in a zero-terminated sequence of numbers from
; the user, prints them out in reverse, and halts.
; The terminating 0 is not printed out.
; You need not worry about running out of memory.

; Examples

; Example of run-time behavior of reverse-prog interacting with user.
; We "capture" the sequence of configurations returned
; by simulate by making it the value of the symbol results.

;> (define results (simulate 100 (init-config (assemble reverse-prog))))
;input = 13
;input = -44
;input = 16
;input = 0
;output = 16
;output = -44
;output = 13
;************************************************************

;loops the reverse-prog call until the input is zero,
    ; then returns the list of numbers
(define (reverse-prog-recursive lst)
  (let ([input (begin (display "input = ")
                     (read))])
    (if (not (= input 0))
        (reverse-prog-recursive (if (null? lst)
                                    (list input)
                                    (cons input lst)))
        lst)))

(define reverse-prog
  (let ([reversed-input (reverse-prog-recursive '())])
    (if (list? reversed-input)
        (let ([input reversed-input])
         (map
          (lambda(i)
            (display "output =")
            (display i)
            (newline))
          input)
         (map
          (lambda(i)
            (list 'data i))
          input))
        (let ([input reversed-input])
          (display "output =")
          (display input)
          (newline)
          (list (list 'data input))))))

; ********************************************************
; ** problem 13 **
; Write another program for the TC-201

; power-prog

; that reads in a positive integer and an exponent for 2
; and prints out the integer multiplied by 2 to the power of the given exponent

; Examples

; Example of run-time behavior of power-prog interacting with user.

; > (define results (simulate 100 (init-config (assemble power-prog))))
; input = 20 
; input = -2
; output = 5
; > (define results (simulate 100 (init-config (assemble power-prog))))
; input = 15
; input = 3
; output = 120



(define power-prog
  (let ([input (begin (display "input = ")
                      (read))])
    (if (> input 0)
        (let* ([power-2 (begin (display "input = ")
                              (read))]
               [result (* input (expt 2 power-2))])
          (display "output = ")
          (display result)
          (newline)
          (list (list 'data result)))
        '())))

;*********************************************************
;Write a program to read in two numbers from the user, print out the maximum
; of the two, and halt.
(define maximum-n
  '((start input 0)
    (store x)
    (input 0)
    (store y)
    (sub x)
    (skippos 0)
    (jump ygreater)
    (xgreater load x)
    (output 0)
    (halt 0)
    (ygreater load y)
    (output 0)
    (halt 0)
    (x data 0)
    (y data 0)))

(define even-or-odd
  '((input 0)
    (xor constantone)
    (skippzero)
    (jump printodd)
    (jump printeven)
    (printeven load zero)
    (output 0)
    (halt 0)
    (printodd load constantone)
    (output 0)
    (halt 0)
    (zero data 0)
    (constantone data 1)))

(define odd-even-prog
'((        input 0)
  (        shift offset)
  (        shift negoff)
  (        output 0)
  (        halt 0)
  (offset data 15)
  (negoff data -15)
  ))










; ********************************************************
; ********  testing, testing. 1, 2, 3 ....
; ********************************************************

(define *testing-flag* #t)
(define error display)  ;; turn off error messages

(define (testold name got expected)
  (cond (*testing-flag*
         (let* ((expected (if (procedure? expected)
                              (and (expected got) 'OK-TEST)
                              expected))
                (prefix (if (equal? got expected)
                            '***OK***
                            'X)))
           (list 'testing name prefix 'got: got 'expected: expected)))))

(define (test name got expected)
  (cond (*testing-flag*
         (let* ((OK (if (procedure? expected)
                        (expected got)
                        (equal? got expected)))
                (prefix (if OK
                            '***OK***
                            '***X***)))
           (list 'testing name prefix 'got: got 'expected: expected)))))

(test 'hours hours (lambda (x) (> x 0)))

(test 'ram-read (ram-read 0 ram-ex1) '(0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1))

(test 'ram-read (ram-read 6 ram-ex2) '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))

(test 'diff-rams (diff-rams ram-ex1 ram-ex2) '((1 (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0) (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 1)) (3 (0 0 0 0 0 0 0 0 0 0 0 0 1 0 1 0) (0 0 0 0 0 0 0 0 0 0 1 0 0 1 0 0))))

(test 'diff-rams (diff-rams '() '((0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))) '())

(test 'diff-rams (diff-rams ram-ex1 (ram-write 2 '(0 0 0 0 1 1 1 1 0 0 0 0 1 1 1 1) ram-ex1)) '((2 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (0 0 0 0 1 1 1 1 0 0 0 0 1 1 1 1))))

(test 'diff-rams (diff-rams ram-ex2 (ram-write 5 '(1 1 0 0 1 1 0 0 1 1 0 0 1 1 0 0) ram-ex2)) '((5 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (1 1 0 0 1 1 0 0 1 1 0 0 1 1 0 0))))

(test 'diff-rams (diff-rams ram-ex1 (ram-write 1 '(0 1 1 1 0 0 0 0 0 0 0 0 0 0 1 1) ram-ex1)) '((1 (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0) (0 1 1 1 0 0 0 0 0 0 0 0 0 0 1 1))))


(test 'extract (extract 1 3 '(a b c d e)) '(b c d))

(test 'extract (extract 4 4 '(a b c d e)) '(e))

(test 'bits->int (bits->int '(0)) 0)

(test 'bits->int (bits->int '(0 0 0 1 1 0)) 6)

(test 'int->bits (int->bits 0) '(0))

(test 'int->bits (int->bits 6) '(1 1 0))

(test 'int->bits-width (int->bits-width 14 8) '(0 0 0 0 1 1 1 0))

(test 'int->bits-width (int->bits-width 14 3) "field too small")

(test 'diff-configs (diff-configs config-ex1 config-ex2) '((acc (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1) (1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1))
                                                           (aeb (0) (1))
                                                           (1 (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0) (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 1))
                                                           (3 (0 0 0 0 0 0 0 0 0 0 0 0 1 0 1 0) (0 0 0 0 0 0 0 0 0 0 1 0 0 1 0 0))))

(test 'incr-pc (incr-pc 1 config-ex1)
      (conf
       (list
        (entry 'acc '(0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1))
        (entry 'pc '(0 0 0 0 0 0 0 0 1 0 0 0))
        (entry 'rf '(1))
        (entry 'aeb '(0)))
       '((0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1)
         (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0)
         (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
         (0 0 0 0 0 0 0 0 0 0 0 0 1 0 1 0)
         (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))))

(test 'diff-configs (diff-configs config-ex2 (incr-pc 4090 config-ex2))
      '((pc (0 0 0 0 0 0 0 0 0 1 1 1) (0 0 0 0 0 0 0 0 0 0 0 1))))

(test 'load-store (diff-configs config-ex1 (do-load 1 config-ex1))
      '((acc (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1) (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0))))

(test 'load-store (diff-configs config-ex2 (do-load 12 config-ex2))
      '((acc (1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1) (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))))

(test 'load-store (diff-configs config-ex1 (do-store 5 config-ex1))
      '((5 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1))))

(test 'load-store  (diff-configs config-ex2 (do-store 0 config-ex2))
      '((0 (0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1) (1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1))))


(test 'add-sub (diff-configs config-ex1 (do-add 3 config-ex1))
      '((acc (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1) (0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 1))))

(test 'add-sub (diff-configs config-ex2 (do-add 3 config-ex2))
      '((acc (1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1) (0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 1))
        (aeb (1) (0))))

(test 'add-sub (diff-configs config-ex1 (do-sub 3 config-ex1))
      '((acc (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1) (0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 1))))

(test 'add-sub (diff-configs config-ex2 (do-sub 3 config-ex2))
      '((acc (1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1) (1 0 0 0 0 0 0 0 0 0 1 0 0 1 1 1))
        (aeb (1) (0))))

(test 'add-sub  (let ((config (conf cpu-ex1 '((0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1))))) (diff-configs config (do-add 0 config)))
      '((acc (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1) (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
        (aeb (0) (1))))


(test 'skip-jump (diff-configs config-ex1 (do-jump 5 config-ex1))
      '((pc (0 0 0 0 0 0 0 0 0 1 1 1) (0 0 0 0 0 0 0 0 0 1 0 1))))

(test 'skip-jump (diff-configs config-ex2 (do-skipzero config-ex2))
      '((pc (0 0 0 0 0 0 0 0 0 1 1 1) (0 0 0 0 0 0 0 0 1 0 0 0))))

(test 'skip-jump (diff-configs config-ex1 (do-skippos config-ex1))
      '((pc (0 0 0 0 0 0 0 0 0 1 1 1) (0 0 0 0 0 0 0 0 1 0 0 1))))

(test 'skip-jump (diff-configs config-ex2 (do-skiperr config-ex2))
      '((pc (0 0 0 0 0 0 0 0 0 1 1 1) (0 0 0 0 0 0 0 0 1 0 0 1)) (aeb (1) (0))))


(test 'loadi-storei (diff-configs config-ex3 (do-loadi 1 config-ex3))
      '((acc (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1) (0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1))))

(test 'loadi-storei (diff-configs config-ex3 (do-loadi 2 config-ex3))
      '((acc (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1) (0 0 0 0 1 1 1 1 0 0 0 0 1 1 1 1))))

(test 'loadi-storei (diff-configs config-ex3 (do-storei 1 config-ex3))
      '((5 (0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1) (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1))))

(test 'loadi-storei (diff-configs config-ex3 (do-storei 2 config-ex3))
      '((4 (0 0 0 0 1 1 1 1 0 0 0 0 1 1 1 1) (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1))))

(test 'shift  (diff-configs config-ex3 (do-shift 2 config-ex3))
      '((acc (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1) (0 0 0 0 0 0 0 0 1 1 1 1 0 0 0 0))))

(test 'shift (diff-configs config-ex3 (do-shift 3 config-ex3))
      '((acc (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1) (0 0 0 0 1 1 1 1 0 0 0 0 0 0 0 0))))

(test 'shift (diff-configs config-ex3 (do-shift 6 config-ex3))
      '((acc (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1) (0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1))))

(test 'and (diff-configs config-ex2 (do-and 1 config-ex2))
      '((acc (1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1) (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1))))

(test 'and (diff-configs config-ex3 (do-and 1 config-ex3))
      '((acc (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1) (0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 1))))

(test 'xor (diff-configs config-ex3 (do-xor 1 config-ex3))
      '((acc (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1) (1 1 1 1 0 0 0 0 0 0 0 0 1 0 1 0))))

(test 'xor (diff-configs config-ex3 (do-xor 5 config-ex3))
      '((acc (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1) (0 1 0 1 0 1 0 1 0 1 0 1 1 0 1 0))))


(test 'next-config (next-config config-ex4)
      (conf
       (list
        (entry 'acc '(0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1))
        (entry 'pc '(0 0 0 0 0 0 0 0 0 0 0 1))
        (entry 'rf '(1))
        (entry 'aeb '(0)))
       '((0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1)
         (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0)
         (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
         (0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1)
         (1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0))))

(test 'next-config (next-config (next-config config-ex4))
      (conf
       (list
        (entry 'acc '(0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1))
        (entry 'pc '(0 0 0 0 0 0 0 0 0 0 1 0))
        (entry 'rf '(1))
        (entry 'aeb '(0)))
       '((0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1)
         (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0)
         (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
         (0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1)
         (0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1))))

(test 'next-config (next-config (next-config (next-config config-ex4)))
      (conf
       (list
        (entry 'acc '(0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1))
        (entry 'pc '(0 0 0 0 0 0 0 0 0 0 1 0))
        (entry 'rf '(0))
        (entry 'aeb '(0)))
       '((0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1)
         (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0)
         (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
         (0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1)
         (0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1))))



(test 'init-config (init-config ram-ex2)
      (conf
       (list (entry 'acc '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)) 
             (entry 'pc '(0 0 0 0 0 0 0 0 0 0 0 0)) 
             (entry 'rf '(1)) 
             (entry 'aeb '(0)))
       '((0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1)
         (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 1)
         (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
         (0 0 0 0 0 0 0 0 0 0 1 0 0 1 0 0))))

(test 'symbol-table (symbol-table prog1)
      '())

(test 'symbol-table (symbol-table prog2)
      (list (entry 'x 0) (entry 'y 1) (entry 'z 2)))

(test 'symbol-table (symbol-table prog3)
      (list
       (entry 'start 0)
       (entry 'next 2)
       (entry 'add-num 8)
       (entry 'sum 11)
       (entry 'constant-zero 12)))

(test 'assemble (assemble prog1)
      '((0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1)
        (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0)
        (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))

(test 'assemble (assemble prog2)
      '((0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)
        (1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0)
        (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)))

(test 'assemble (assemble prog3)
      '((0 0 0 1 0 0 0 0 0 0 0 0 1 1 0 0)
        (0 0 1 0 0 0 0 0 0 0 0 0 1 0 1 1)
        (0 1 0 1 0 0 0 0 0 0 0 0 0 0 0 0)
        (1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
        (0 1 1 1 0 0 0 0 0 0 0 0 1 0 0 0)
        (0 0 0 1 0 0 0 0 0 0 0 0 1 0 1 1)
        (0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0)
        (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
        (0 0 1 1 0 0 0 0 0 0 0 0 1 0 1 1)
        (0 0 1 0 0 0 0 0 0 0 0 0 1 0 1 1)
        (0 1 1 1 0 0 0 0 0 0 0 0 0 0 1 0)
        (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
        (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))

(test 'simulate (simulate 5 config-ex4)
      (list
       (conf
        (list
         (entry 'acc '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
         (entry 'pc '(0 0 0 0 0 0 0 0 0 0 0 0))
         (entry 'rf '(1))
         (entry 'aeb '(0)))
        '((0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1)
          (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0)
          (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
          (0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1)
          (1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0)))
       (conf
        (list
         (entry 'acc '(0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1))
         (entry 'pc '(0 0 0 0 0 0 0 0 0 0 0 1))
         (entry 'rf '(1))
         (entry 'aeb '(0)))
        '((0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1)
          (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0)
          (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
          (0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1)
          (1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0)))
       (conf
        (list
         (entry 'acc '(0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1))
         (entry 'pc '(0 0 0 0 0 0 0 0 0 0 1 0))
         (entry 'rf '(1))
         (entry 'aeb '(0)))
        '((0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1)
          (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0)
          (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
          (0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1)
          (0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1)))
       (conf
        (list
         (entry 'acc '(0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1))
         (entry 'pc '(0 0 0 0 0 0 0 0 0 0 1 0))
         (entry 'rf '(0))
         (entry 'aeb '(0)))
        '((0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1)
          (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0)
          (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
          (0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1)
          (0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1)))))



;********************** end of hw6.scm **********************
