#!r6rs

(library
 (pelib primes)
 (export
  get-primes
  lazy-primes
  prime-stream)
 (import
  (rnrs)
  (only (srfi :1) drop iota circular-list)
  (srfi :26)
  (srfi :41)
  (srfi :42)
  (pelib lazy-pairs))

 #|
 ## wheel factorization で使う定数・ユーティリティ群
 |#

 (define true 1)
 (define false 0)
 (define presieves
   (list 2 3 5))
 (define npresieves
   (length presieves))
 (define first-prime 7)
 (define circumference
   (apply * presieves))
 (define coprimes
   (filter (lambda (x) (= 1 (gcd x circumference)))
	   (iota circumference)))
 (define ncoprimes
   (length coprimes))
 (define steps
   (let ([steps (map - (append (cdr coprimes) (list (+ circumference 1)))
		       coprimes)])
     (apply circular-list steps)))
 (define integer->index
   (let ([conversion-table (make-vector circumference)])
     (vector-set! conversion-table 0 -1)
     (do-ec (:range i ncoprimes)
	    (:let coprime (list-ref coprimes i))
	    (:range j coprime (min circumference (+ coprime (list-ref steps i))))
       (vector-set! conversion-table j i))
     (lambda (x)
       (let-values ([(q r) (div-and-mod x circumference)])
	 (+ (* ncoprimes q)
	    (vector-ref conversion-table r))))))
 (define (index->integer i)
   (let-values ([(q r) (div-and-mod i ncoprimes)])
     (+ (* circumference q)
	(list-ref coprimes r))))
 (define (integer->steps x)
   (drop steps (mod (integer->index x) ncoprimes)))

 (define (count-primes v)
   (let ([i-max (- (bytevector-length v) 1)])
     (let count ([i 0] [nprimes 0])
       (if (< i-max i)
	   nprimes
	   (count (+ i 1) (+ nprimes (bytevector-u8-ref v i)))))))

 ;; フラグテーブル v の v[0] が n, steps で始まるものだとして素数を収集する
 (define (collect-primes v n steps)
   (let ([size (bytevector-length v)])
     (let collect ([i 0] [n n] [steps steps] [primes (list)])
       (if (<= size i)
	   (reverse primes)
	   (if (= true (bytevector-u8-ref v i))
	       (collect (+ i 1) (+ n (car steps)) (cdr steps) (cons n primes))
	       (collect (+ i 1) (+ n (car steps)) (cdr steps) primes))))))

 (define (eratosthenes n)
   (let* ([size (+ 1 (integer->index n))]
	  [v (make-bytevector size true)]
	  [root (exact (floor (sqrt (index->integer (- size 1)))))])
     (bytevector-u8-set! v 0 false)
     (let sieve! ([i 0] [p 1] [steps steps])

       (define (sieve-pq-group! q)
	 (let ([composite-step (* p ncoprimes)])
	   (let draw! ([i (integer->index (* p q))])
	     (when (< i size)
	       (bytevector-u8-set! v i false)
	       (draw! (+ i composite-step))))))

       (when (<= p root)
	 (when (= true (bytevector-u8-ref v i))
	   (let sieve-composites! ([q p] [steps steps] [nsieves 0])
	     (when (< nsieves ncoprimes)
	       (sieve-pq-group! q)
	       (sieve-composites! (+ q (car steps)) (cdr steps) (+ nsieves 1)))))
	 (sieve! (+ i 1) (+ p (car steps)) (cdr steps))))

     (append presieves (collect-primes v 1 steps))))

 (define (get-primes n)
   (if (< n first-prime)
       (filter (lambda (x) (<= x n)) presieves)
       (eratosthenes n)))

 #|
 ## 区間篩で使う定数及び変数・ユーティリティ群
 |#

 (define segment-size (* circumference (div #e1e6 circumference)))
 (define table-size (+ 1 (integer->index segment-size)))
 (define sieve-start (+ 1 (* circumference (div #e1e4 circumference))))
 (define sieve-end (+ sieve-start segment-size -2))
 (define table-start (integer->index sieve-start))

 (define (compute-first-index a0 d lower-limit)
   (if (<= lower-limit a0)
       a0
       (+ a0 (* d (div (- lower-limit a0 (- d) 1) d)))))

 (define (segment-sieve!)
   (let ([v (make-bytevector table-size true)]
	 [sieve-limit (exact (floor (sqrt sieve-end)))]
	 [primes (drop lazy-primes npresieves)])
     (let sieve! ([p (car primes)] [primes (cdr primes)])

       (define (sieve-pq-group! q)
	 (let ([composite-step (* p ncoprimes)])
	   (let draw! ([i (- (compute-first-index (integer->index (* p q)) composite-step table-start) table-start)])
	     (when (< i table-size)
	       (bytevector-u8-set! v i false)
	       (draw! (+ i composite-step))))))

       (when (<= p sieve-limit)
	 (let sieve-composites! ([q p] [steps (integer->steps p)] [nsieves 0])
	   (when (< nsieves ncoprimes)
	     (sieve-pq-group! q)
	     (sieve-composites! (+ q (car steps)) (cdr steps) (+ nsieves 1))))
	 (sieve! (car primes) (cdr primes))))

     (let ([found-primes (collect-primes v sieve-start steps)])
       (set! sieve-start (+ sieve-start segment-size))
       (set! sieve-end (+ sieve-end segment-size))
       (set! table-start (integer->index sieve-start))
       found-primes)))

 (define lazy-primes
   (letrec ([next-primes
	     (lambda ()
	       (let ([found-primes (segment-sieve!)])
		 (if (null? found-primes)
		     (next-primes)
		     (begin
		       (pairs->lazy-pairs! found-primes (next-primes))
		       found-primes))))]
	    [primes (get-primes (- sieve-start 2))])
     (pairs->lazy-pairs! primes (next-primes))
     primes))

 (define prime-stream
   (stream-let recur ([ps lazy-primes])
     (stream-cons (car ps) (recur (lcdr! ps)))))

)

