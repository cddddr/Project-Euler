#!r6rs

(library
 (anaphoric)
 (export
  aif awhen aunless aand alambda)
 (import
  (rnrs))

 (define-syntax aif
   (lambda (x)
     (syntax-case x ()
       [(k test exprs ...)
	(<= 1 (length #'(exprs ...)) 2)
	(with-syntax ([it (datum->syntax #'k 'it)])
	  #'(let ([it test])
	      (if it exprs ...)))]
       [(k . else)
	#'(syntax-violation 'k "expected 2 or 3 expressions" '(k . else))])))

 (define-syntax awhen
   (lambda (x)
     (syntax-case x ()
       [(k test a . b)
	(with-syntax ([it (datum->syntax #'k 'it)])
	  #'(let ([it test])
	      (if it (begin a . b))))]
       [(k . else)
	#'(syntax-violation 'k "invalid syntax" '(k . else))])))

 (define-syntax aunless
   (lambda (x)
     (syntax-case x ()
       [(k test a . b)
	(with-syntax ([it (datum->syntax #'k 'it)])
	  #'(let ([it test])
	      (if (not it) (begin a . b))))]
       [(k . else)
	#'(syntax-violation 'k "invalid syntax" '(k . else))])))

 (define-syntax aand
   (lambda (x)
     (syntax-case x ()
       [(k) #'#t]
       [(k a) #'a]
       [(k a . b)
	(with-syntax ([it (datum->syntax #'k 'it)])
	  #'(let ([it a])
	      (if it (k . b) #f)))])))

 (define-syntax alambda
   (lambda (x)
     (syntax-case x ()
       [(k args body . rest)
	(with-syntax ([self (datum->syntax #'k 'self)])
	  #'(letrec ([self (lambda args body . rest)])
	      self))]
       [(k . else)
	#'(syntax-violation 'k "invalid syntax" '(k . else))])))

)