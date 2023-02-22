#lang racket
(define tm-sort
  (list
   ;goes to the right end of the input
   (ins 'q1 0 'q1 0 'R)
   (ins 'q1 1 'q1 1 'R)
   (ins 'q1 'b 'q2 'b 'L)

   ;places an x if it sees a 0, B if it sees a 1
   (ins 'q2 0 'q3 'x 'R)
   (ins 'q2 1 'q2 'B 'L)
   (ins 'q2 'B 'q2 'B 'L)
   (ins 'q2 'x 'q2 'x 'L)
   (ins 'q2 'b 'q5 'b 'R)

   ;places a 0 at the right end
   (ins 'q3 0 'q3 0 'R)
   (ins 'q3 1 'q3 1 'R)
   (ins 'q3 'B 'q3 'B 'R)
   (ins 'q3 'x 'q3 'x 'R)
   (ins 'q3 'b 'q4 0 'L)

   ;returns to the first x from the right, to place 0
   (ins 'q4 'x 'q2 'x 'L)
   (ins 'q4 'B 'q4 'B 'L)
   (ins 'q4 0 'q4 0 'L)
   (ins 'q4 1 'q4 1 'L)

   ;replaces Bs with xs and places a 1 at the end
   (ins 'q5 'B 'q6 'x 'R)
   (ins 'q5 'x 'q5 'x 'R)
   (ins 'q5 0 'q7 0 'R)
   (ins 'q5 1 'q7 1 'R)

   (ins 'q6 0 'q6 0 'R)
   (ins 'q6 1 'q6 1 'R)
   (ins 'q6 'B 'q6 'B 'R)
   (ins 'q6 'x 'q6 'x 'R)
   (ins 'q6 'b 'q4 1 'L)

   ;replaces xs with blanks
   (ins 'q7 0 'q7 0 'L)
   (ins 'q7 1 'q7 1 'L)
   (ins 'q7 'x 'q7 'b 'L)
   (ins 'q7 'b 'q8 'b 'R)

   ;returns the head to the right
   (ins 'q8 'b 'q8 'b 'R)))