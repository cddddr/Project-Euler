#!r6rs

(library
 (combinations)
 (export
  combinations combinations-all
  permutations permutations-for-each)
 (import
  (rnrs)
  (srfi :26))

 (define (combinations-all xs)
   (if (null? xs)
       (list)
       (let ([x (car xs)] [ys-cmbs (combinations-all (cdr xs))])
	 (append (list (list x))
		 ys-cmbs
		 (map (cut cons x <>) ys-cmbs)))))

 (define (combinations xs n)
   (define (cmbs xs xs:len n)
     (if (zero? n)
	 (list (list))
	 (let ([m (- n 1)])
	   (let recur ([x (car xs)] [ys (cdr xs)] [ys:len (- xs:len 1)] [result (list)])
	     (if (= ys:len m)
		 (cons (cons x ys) result)
		 (let ([ys-cmbs (map (cut cons x <>) (cmbs ys ys:len m))])
		   (recur (car ys) (cdr ys) (- ys:len 1) (append ys-cmbs result))))))))
   (let ([xs:len (length xs)])
     (if (or (< n 0) (< xs:len n))
	 (list)
	 (reverse (cmbs xs xs:len n)))))

 ;; 順序めちゃくちゃ
 (define (permutations xs)
   (if (null? xs)
       (list (list))
       (let recur ([lefts (list)] [rights xs] [result (list)])
	 (if (null? rights)
	     result
	     (let* ([c (car rights)]
		    [result:sub (permutations (append lefts (cdr rights)))])
	       (recur (cons c lefts)
		      (cdr rights)
		      (append result (map (cut cons c <>) result:sub))))))))

 ;; remove でずぼらしているので xs に重複があるとまずい
 (define (permutations-for-each f xs)
   (let recur ([xs xs] [sy (list)])
     (if (null? xs)
	 (f (reverse sy))
	 (for-each (lambda (x) (recur (remove x xs) (cons x sy))) xs))))

)