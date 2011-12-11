#!r6rs

(library
 (pelib primes)
 (export
  get-primes
  lazy-primes
  prime-stream)
 (import
  (rnrs)
  (srfi :41)
  (srfi :42)
  (pelib lazy-pairs))

 (define true 1)
 (define false 0)
 (define N (/ #e1e6 2))
 (define M (- N 1))

 (define sieved 1011)
 (define v (make-bytevector N))

 (define (get-primes lim)
   (define (odd-prime-table->primes table i primes)
     (if (zero? i)
	 primes
	 (odd-prime-table->primes
	  table (- i 1) (if (vector-ref table i)
			    (cons (+ 1 (* i 2)) primes)
			    primes))))
   (define (get-odd-prime-table limit)
     (let* ([size (+ 1 (div limit 2))]
	    [v (make-vector size #t)]
	    [sieve-limit (div (- (exact (floor (sqrt limit))) 1) 2)])
       (define (draw-multiples inc start)
	 (if (< start size)
	     (begin
	       (vector-set! v start #f)
	       (draw-multiples inc (+ start inc)))))
       (let sieve ([i 1])
	 (if (< sieve-limit i) v
	     (let ([x (+ 1 (* i 2))])
	       (if (vector-ref v i)
		   (draw-multiples x (div (- (* x x) 1) 2)))
	       (sieve (+ i 1)))))))
   (let* ([limit (if (even? lim) (- lim 1) lim)]
	  [table (get-odd-prime-table limit)])
     (cons 2 (odd-prime-table->primes table (div limit 2) (list)))))

 (define (->odd x)
   (if (even? x) (+ x 1) x))

 (define (part-sieve!)
   (let* ([start:physical (+ sieved 2)]
	  [end:physical (+ start:physical (* M 2))]
	  [sieve-limit (exact (floor (sqrt end:physical)))])
     (define (start:virtual p)
       (let ([q (exact (ceiling (/ start:physical p)))])
	 (/ (- (* p (->odd q)) start:physical) 2)))
     (bytevector-fill! v true)
     (let sieve! ([p 3] [qs (cddr lazy-primes)])
       (when (<= p sieve-limit)
	 (do-ec (:range i (start:virtual p) N p)
	   (bytevector-u8-set! v i false))
	 (sieve! (car qs) (cdr qs))))
     (set! sieved end:physical)
     (let collect-primes ([i:virtual 0] [n 0] [primes (list)])
       (if (< M i:virtual)
	   (values n (reverse primes))
	   (if (= false (bytevector-u8-ref v i:virtual))
	       (collect-primes (+ i:virtual 1) n primes)
	       (collect-primes (+ i:virtual 1) (+ n 1) (cons (+ start:physical (* i:virtual 2)) primes)))))))

 (define lazy-primes
   (letrec ([next-primes
	     (lambda ()
	       (let-values ([(n primes) (part-sieve!)])
		 (if (zero? n)
		     (next-primes)
		     (begin (pairs->lazy-pairs! primes (next-primes)) primes))))]
	    [primes (get-primes (- sieved 2))])
     (pairs->lazy-pairs! primes (next-primes))
     primes))

 (define prime-stream
   (stream-let recur ([ps lazy-primes])
     (stream-cons (car ps) (recur (lcdr! ps)))))

)
