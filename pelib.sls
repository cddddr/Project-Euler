#!r6rs

;; Project Euler 用ライブラリ

(library
 (pelib)
 (export
  ;; Utilities
  print println pa$ dot$ memoize
  combinations combinations-all permutations permutations-for-each
  float->string factors->string
  single? unique unique* inject
  bitwise-reverse >> <<
  ;; Math
  inc dec divisible? perfect-square?
  place-of fold-digits integer->digits digits->integer
  mod-expt coprime? coprimes?
  factor factors factors-flat euler-phi divisors sigma0 sigma1
  euclid mod-inv chinese-remainder-theorem
  factorial binomial
  get-primes sqrt->cf
  ;; Primality Test
  prime? prime?:fermat-test prime?:miller-rabin-test
  ;; Streams, Lazy Pairs
  prime-stream lazy-primes
  lcons pairs->lazy-pairs! lcdr!
  ;; Syntaxes
  let1 define-memoized lambda-memoized
  aif awhen aunless aand alambda
  inc! dec!
  ^ ^a ^b ^c ^d ^e ^f ^g ^h ^i ^j ^k ^l ^m ^n ^o ^p ^q ^r ^s ^t ^u ^v ^w ^x ^y ^z ^_)
 (import
  (rnrs)
  (rename (rnrs)
	  (lambda ^)
	  (bitwise-arithmetic-shift-right >>)
	  (bitwise-arithmetic-shift-left <<))
  (only (srfi :1) car+cdr last xcons make-list iota append-map circular-list)
  (srfi :26)
  (srfi :27)
  (anaphoric)
  (combinations)
  (pelib lazy-pairs)
  (pelib primes))

 (define-syntax let1
   (syntax-rules ()
     [(_ var init . exprs)
      (let ([var init]) . exprs)]
     [(k . else)
      (syntax-violation 'k "invalid syntax" '(k . else))]))

 (define-syntax make-short-lambda
   (lambda (stx)
     (define (make stx)
       (string->symbol (string-append "^" (symbol->string (syntax->datum stx)))))
     (syntax-case stx ()
       [(k name)
	(with-syntax ([alias (datum->syntax #'k (make #'name))])
	  #'(define-syntax alias
	      (lambda (stx)
		(syntax-case stx ()
		  [(k . exprs)
		   (with-syntax ([id (datum->syntax #'k (syntax->datum #'name))])
		     #'(lambda (id) . exprs))]))))]
       [(k name . rest-names)
	#'(begin (k name)
		 (k . rest-names))])))
 (make-short-lambda a b c d e f g h i j k l m n o p q r s t u v w x y z _)

 ;; メモ化されたクロージャを作る
 (define-syntax lambda-memoized
   (syntax-rules ()
     [(_ args expr . rest-exprs)
      (let ([f (lambda args expr . rest-exprs)])
	(set! f (memoize f))
	f)]
     [(k . else)
      (syntax-violation 'k "invalid syntax" '(k . else))]))

 ;; メモ化された手続きを定義する
 (define-syntax define-memoized
   (syntax-rules ()
     [(_ (proc . args) exprs . rest-exprs)
      (define proc (lambda-memoized args exprs . rest-exprs))]
     [(k . else)
      (syntax-violation 'k "invalid syntax" '(k . else))]))

 (define-syntax inc!
   (syntax-rules ()
     [(_ x) (begin (set! x (+ x 1)) x)]
     [(_ x y) (begin (set! x (+ x y)) x)]
     [(k . else) (syntax-violation 'k "invalid syntax" '(k . else))]))

 (define-syntax dec!
   (syntax-rules ()
     [(_ x) (begin (set! x (- x 1)) x)]
     [(_ x y) (begin (set! x (- x y)) x)]
     [(k . else) (syntax-violation 'k "invalid syntax" '(k . else))]))

 (define (print . xs)
   (for-each display xs))

 (define (println . xs)
   (for-each display xs)
   (newline))

 ;; Gauche の pa$
 (define (pa$ proc . args)
   (lambda additional-args (apply proc (append args additional-args))))

 ;; Gauche の .$ / compose
 (define (dot$ . procs)
   (if (null? procs)
       values
       (let recur ([f (car procs)] [procs (cdr procs)])
	 (if (null? procs)
	     f
	     (recur (lambda args (call-with-values (lambda () (apply (car procs) args)) f))
		    (cdr procs))))))

 ;; 手続きをメモ化して返す
 (define (memoize proc)
   (let ([cache-table (make-hashtable equal-hash equal?)]
	 [not-memoized '(not-memoized)])
     (lambda args
       (let ([cache (hashtable-ref cache-table args not-memoized)])
	 (if (eq? cache not-memoized)
	    (let ([result (apply proc args)])
	      (hashtable-set! cache-table args result)
	      result)
	    cache)))))

 ;; 長さ 1 のリストであるかどうか
 (define (single? xs)
   (and (pair? xs)
	(null? (cdr xs))))

 ;; 速い delete-duplicates
 (define (unique* xs < =)
   (if (null? xs)
       (list)
       (let ([xs (list-sort < xs)])
	 (let recur ([src (cdr xs)] [prev (car xs)] [dst (list (car xs))])
	   (if (null? src)
	       dst
	       (let ([x (car src)])
		 (if (= x prev)
		     (recur (cdr src) prev dst)
		     (recur (cdr src) x (cons x dst)))))))))

 ;; 数値をまとめたリストからユニークなものを取り出す
 (define (unique xs)
   (unique* xs > =))

 ;; ys の要素同士の間に x を挿入する
 (define (inject x ys)
   (if (null? ys)
       (list)
       (cons (car ys) (append-map (cut list x <>) (cdr ys)))))

 ;; x 小数点以下第 n 位に rounding した数値に対応する文字列を返す
 (define (float->string float n rounding)
   (let ([x (expt 10 n)])
     (number->string (/ (rounding (* float x)) x))))

 ;; (factors->string '((2 . 3) (5 . 4))) => "2^3 * 5^4"
 (define (factors->string facts)
   (define (convert pair)
     (string-append (number->string (car pair)) "^" (number->string (cdr pair))))
   (apply string-append (inject " * " (map convert facts))))

 (define (inc x) (+ x 1))

 (define (dec x) (- x 1))

 (define (divisible? x y)
   (zero? (mod x y)))

 (define (perfect-square? x)
   (let ([root (exact (floor (sqrt x)))])
     (= x (* root root))))

 ;; x の桁数を返す
 (define (place-of x)
   (fold-digits (lambda (n _) (+ n 1)) 0 x))

 ;; 桁毎に右から左へ畳み込む
 (define (fold-digits f acc x)
   (if (< x 10)
       (f acc x)
       (fold-digits f (f acc (mod x 10)) (div x 10))))

 ;; 非負整数 x を順序を保ったまま桁毎に分解する
 (define (integer->digits x)
   (fold-digits xcons (list) x))

 ;; 分解された桁を数に戻す
 ;; x == (digits->integer (integer->digits x))
 (define (digits->integer digits)
   (fold-left (lambda (acc x) (+ x (* 10 acc))) 0 digits))

 ;; bit の並びを反転させる
 (define (bitwise-reverse ei)
   (bitwise-reverse-bit-field ei 0 (bitwise-length ei)))

 ;; 右向きバイナリ法による冪剰余
 (define (mod-expt x e m)
   (let ([x (mod x m)])
     (let recur ([v 1] [k (bitwise-length e)] [e (bitwise-reverse e)])
       (if (zero? k)
	   v
	   (let ([v (mod (* v v) m)])
	     (if (odd? e)
		 (recur (mod (* v x) m) (- k 1) (>> e 1))
		 (recur v (- k 1) (>> e 1))))))))

 ;; フェルマーテストにより確率的に素数判定を行う
 (define (prime?:fermat-test n a)
   (if (= 1 (mod-expt a (- n 1) n))
       'probable-prime
       #f))

 ;; ミラー-ラビン素数判定法により確率的に素数判定を行う
 (define (prime?:miller-rabin-test n a)
   (let* ([n-1 (- n 1)]
	  [s (bitwise-first-bit-set n-1)]
	  [d (>> n-1 s)]
	  [x (mod-expt a d n)])
     (if (= x 1)
	 'probable-prime
	 (let recur ([r 0] [x x])
	   (cond
	    [(= r s) #f]
	    [(= x n-1) 'probable-prime]
	    [else (recur (+ r 1) (mod (* x x) n))])))))

 ;; prime? のみしか使わないけど prime? の中へ入れると遅くなるので
 (define small-primes
   '(2 3 5 7 11 13 17 19 23 29 31 37 41
     43 47 53 59 61 67 71 73 79 83 89 97))
 (define border 101)
 (define border^2 (* border border))

 ;; 試し割りと決定的／確率的なミラーラビンによる素数判定を行う
 ;; 戻り値は #f, #t, 'probable-prime
 (define (prime? x)
   (cond
    [(< x border) (and (memv x small-primes) #t)]
    [(< x border^2) (if (exists (cut divisible? x <>) small-primes) #f #t)]
    [(< x 3825123056546413051)
     (let ([as (cond
		[(< x 1373653) '(2 3)]
		[(< x 9080191) '(31 73)]
		[(< x 4759123141) '(2 7 61)]
		[(< x 2152302898747) '(2 3 5 7 11)]
		[(< x 3474749660383) '(2 3 5 7 11 13)]
		[(< x 341550071728321) '(2 3 5 7 11 13 17)]
		[else '(2 3 5 7 11 13 17 19 23)])])
       (for-all (cut prime?:miller-rabin-test x <>) as))]
    [else (and (for-all (cut prime?:miller-rabin-test x <>)
			'(2 3 5 7 11 13 17 19 23 29))
	       'probable-prime)]))

 ;; x と y が互いに (ﾟдﾟ) であるかどうか
 (define (coprime? x y)
   (= 1 (gcd x y)))

 ;; 全ての組について互いに (ﾟдﾟ) であるかどうか
 (define (coprimes? . args)
   (for-all (cut apply coprime? <>) (combinations args 2)))

 ;; 素因数分解 / ポラード・ロー
 (define (factor n)
   (define (f x)
     (+ 1 (mod (* x x) n)))
   (define (rho)
     (let ([start (+ 1 (random-integer n))])
       (let recur ([x (f start)] [y (f (f start))])
	 (let ([d (gcd n (abs (- x y)))])
	   (cond
	    [(= d 1) (recur (f x) (f (f y)))]
	    [(= d n) (rho)]
	    [else (unique (append (factor d) (factor (/ n d))))])))))
   (cond
    [(prime? n)   (list n)]
    [(<= n 10000) (factor:naive n)]
    [else         (rho)]))

 ;; 素因数分解 / 素数列による試し割り
 (define (factor:naive x)
   (define (f x p ps)
     (cond
      [(<= x 1) (list)]
      [(prime? x) (list x)]
      [(< 0 (mod x p)) (f x (car ps) (lcdr! ps))]
      [else (let loop ([x (div x p)])
	      (if (divisible? x p)
		  (loop (div x p))
		  (cons p (f x (car ps) (lcdr! ps)))))]))
   (f x 2 (lcdr! lazy-primes)))

 ;; x の素因数とその個数をペアにしてそれらのリストを返す
 (define (factors x)
   (let recur ([x x] [pfs (factor x)])
     (if (= x 1)
	 (list)
	 (let recur/count ([p (car pfs)] [x x] [n 0])
	   (if (divisible? x p)
	       (recur/count p (/ x p) (+ n 1))
	       (cons (cons p n) (recur x (cdr pfs))))))))

 ;; (factors-flat 100) => '(2 2 5 5)
 (define (factors-flat x)
   (append-map (lambda (pair) (make-list (cdr pair) (car pair)))
	       (factors x)))

 ;; 約数を列挙し昇順にソートして返す 
 (define (divisors x)
   (define (kons divs fact)
     (let-values ([(p n) (car+cdr fact)])
       (append divs (append-map (lambda (p^e) (map (cut * p^e <>) divs))
				(map (cut expt p <>) (iota n 1))))))
   (list-sort < (fold-left kons (list 1) (factors x))))

 ;; 約数の個数を求める
 (define (sigma0 x)
   (apply * (map (dot$ inc cdr) (factors x))))

 ;; 約数の和を求める
 (define (sigma1 x)
   (define (compute fact)
     (let-values ([(p n) (car+cdr fact)])
       (/ (- (expt p (+ n 1)) 1)
	  (- p 1))))
   (apply * (map compute (factors x))))

 ;; オイラーのトーティエント関数
 (define (euler-phi x)
   (apply * x (map (lambda (p) (- 1 (/ 1 p)))
		   (factor x))))

 ;; 拡張されたユークリッドの互除法により
 ;; x,y が与えられた時 ax + by = c を満たす a,b,c を求める
 ;; (euclid x y) => (values a b c)
 (define (euclid x y)
   (define (euclid-main x y)
     (let-values ([(q r) (div-and-mod x y)])
       (if (<= r 1)
	   (values 1 (- q) r)
	   (let-values ([(a b c) (euclid y (mod x y))])
	     (values b (+ a (* b q -1)) c)))))
   (if (> x y)
       (euclid-main x y)
       (let-values ([(a b c) (euclid-main y x)]) (values b a c))))

 ;; a,m が互いに素である時、 ax ≡ r (mod m) における乗法の逆元を求める
 ;; ax ≡ r (mod m) => x ≡ inv (mod m) => return inv
 (define (mod-inv a r m)
   (let-values ([(x _ inv) (euclid a m)])
     (if (zero? inv) #f
	 (mod (* x r) m))))

 ;; m1 ... mn が互いに素である時
 ;; x ≡ a1 (mod m1) ... x ≡ an (mod mn)
 ;; 上記を全て満たす x ≡ a (mod m) を返す
 ;; arg: (cons m1 a1) ... (cons mn an)
 ;; ret: (cons m a)
 (define (chinese-remainder-theorem exp . rest-exps)
   (define (crt exp1 exp2)
     (let ([m1 (car exp1)] [a1 (cdr exp1)]
	   [m2 (car exp2)] [a2 (cdr exp2)])
       (let ([m1m2 (* m1 m2)])
	 (cons m1m2
	       (mod (+ (* m1 (mod-inv m1 a2 m2))
		       (* m2 (mod-inv m2 a1 m1)))
		    m1m2)))))
   (fold-left crt exp rest-exps))

 ;; x!
 (define (factorial x)
   (apply * (iota x 1)))

 ;; nCk
 (define (binomial n k)
   (/ (factorial n) (factorial (- n k)) (factorial k)))

 ;; 平方根の連分数展開を行う
 ;; 例えば sqrt(13) => [3,(1,1,1,1,6)] なら
 ;; (sqrt->cf 13) => (cons 3 (circular-list 1 1 1 1 6)) を返す
 (define (sqrt->cf x)
   (let ([a0 (exact (floor (sqrt x)))])
     (define (next n d)
       (let* ([nk (/ (+ x (* d (- d))) n)]
	      [ak (div (- a0 d) nk)]
	      [dk (- (- d) (* ak nk))])
	 (values ak nk dk)))
     (define (expand n1 d1 nk dk)
       (let-values ([(ak+1 nk+1 dk+1) (next nk dk)])
	 (if (and (= n1 nk+1) (= d1 dk+1))
	     (list)
	     (cons ak+1 (expand n1 d1 nk+1 dk+1)))))
     (let-values ([(a1 n1 d1) (next 1 (- a0))])
       (cons a0 (apply circular-list a1 (expand n1 d1 n1 d1))))))

)

;; EOF