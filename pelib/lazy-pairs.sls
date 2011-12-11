#!r6rs

(library
 (pelib lazy-pairs)
 (export
  lcons pairs->lazy-pairs! lcdr!)
 (import
  (rnrs)
  (rnrs mutable-pairs (6))
  (only (srfi :1) last-pair))

 (define promises (make-eq-hashtable))

 (define-syntax lcons
   (syntax-rules ()
     [(_ left right:expr)
      (let ([pair (list left)])
	(hashtable-set! promises pair (lambda () right:expr))
	pair)]))

 (define-syntax pairs->lazy-pairs!
   (syntax-rules ()
     [(_ xs expr)
      (if (null? xs)
	  (assertion-violation 'pairs->lazy-pairs! "pair required" xs)
	  (let ([pair (last-pair xs)])
	    (if (hashtable-ref promises pair #f)
		(assertion-violation 'pairs->lazy-pairs! "already registered" xs)
		(hashtable-set! promises pair (lambda () expr)))))]))

 (define (lcdr! lazy-pair)
   (define (unknown-lazy-pair)
     (assertion-violation 'lcdr! "unknown lazy-pair" lazy-pair))
   (if (pair? (cdr lazy-pair))
       (cdr lazy-pair)
       (let ([right ((hashtable-ref promises lazy-pair unknown-lazy-pair))])
	 (set-cdr! lazy-pair right)
	 (hashtable-delete! promises lazy-pair)
	 right)))

)
