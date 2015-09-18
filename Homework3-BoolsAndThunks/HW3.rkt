#lang plai-typed
(require plai-typed/s-exp-match)

(define-type Value
  [numV (n : number)]
  [boolV (b : boolean)]
  [thunkV (t : ExprC)
          (env : Env)] ;(add an env)
  [closV (arg : symbol)
         (body : ExprC)
         (env : Env)])

(define-type ExprC
  [numC (n : number)]
  [boolC (b : boolean)]
  [idC (s : symbol)]
  [plusC (l : ExprC) 
         (r : ExprC)]
  [multC (l : ExprC)
         (r : ExprC)]
  [letC (n : symbol) 
        (rhs : ExprC)
        (body : ExprC)]
  [lamC (n : symbol)
        (body : ExprC)]
  [equalC (lhs : ExprC)
          (rhs : ExprC)]
  [delayC (v : ExprC)]
  [forceC (v : ExprC)]
  [ifC (tst : ExprC)
       (t : ExprC)
       (f : ExprC)]
  [appC (fun : ExprC)
        (arg : ExprC)])

(define-type Binding
  [bind (name : symbol)
        (val : Value)])

(define-type-alias Env (listof Binding))

(define mt-env empty)
(define extend-env cons)

(module+ test
  (print-only-errors true))

;; parse ----------------------------------------
(define (parse [s : s-expression]) : ExprC
  (cond
    [(s-exp-match? `NUMBER s) (numC (s-exp->number s))]    
    [(s-exp-match? `true s) (boolC true)]
    [(s-exp-match? `false s) (boolC false)]
    [(s-exp-match? `SYMBOL s) (idC (s-exp->symbol s))]
    [(s-exp-match? '{+ ANY ANY} s)
     (plusC (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(s-exp-match? '{* ANY ANY} s)
     (multC (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(s-exp-match? '{let {[SYMBOL ANY]} ANY} s)
     (let ([bs (s-exp->list (first
                             (s-exp->list (second
                                           (s-exp->list s)))))])
       (letC (s-exp->symbol (first bs))
             (parse (second bs))
             (parse (third (s-exp->list s)))))]
    [(s-exp-match? '{lambda {SYMBOL} ANY} s)
     (lamC (s-exp->symbol (first (s-exp->list 
                                  (second (s-exp->list s)))))
           (parse (third (s-exp->list s))))]
    [(s-exp-match? '{= ANY ANY} s)
     (equalC (parse (second (s-exp->list s)))
              (parse (third (s-exp->list s))))]
    [(s-exp-match? '{delay ANY} s)
                   (delayC (parse (second (s-exp->list s))))]
    [(s-exp-match? '{force ANY} s)
                   (forceC (parse (second (s-exp->list s))))]   
    [(s-exp-match? '{if ANY ANY ANY} s)
     (ifC (parse (second (s-exp->list s)))
          (parse (third (s-exp->list s)))
          (parse (fourth (s-exp->list s))))]
    
    [(s-exp-match? '{ANY ANY} s)
     (appC (parse (first (s-exp->list s)))
           (parse (second (s-exp->list s))))]
    [else (error 'parse "invalid input")]))

(module+ test
  (test (parse '2)
        (numC 2))
  (test (parse `x) ; note: backquote instead of normal quote
        (idC 'x))
  (test (parse '{+ 2 1})
        (plusC (numC 2) (numC 1)))
  (test (parse '{* 3 4})
        (multC (numC 3) (numC 4)))
  (test (parse '{+ {* 3 4} 8})
        (plusC (multC (numC 3) (numC 4))
               (numC 8)))
  (test (parse '{let {[x {+ 1 2}]}
                  y})
        (letC 'x (plusC (numC 1) (numC 2))
              (idC 'y)))
  (test (parse '{lambda {x} 9})
        (lamC 'x (numC 9)))
  (test (parse '{double 9})
        (appC (idC 'double) (numC 9)))
  (test/exn (parse '{{+ 1 2}})
            "invalid input")
  (test (parse '{= 8 8})
        (equalC (numC 8) (numC 8)))
  (test (parse '{= {+ 4 4} 8})
        (equalC (plusC (numC 4) (numC 4)) (numC 8)))
  
  (test (parse '{if {= 8 8} 0 1})
        (ifC  (equalC (numC 8) (numC 8)) (numC 0) (numC 1)))
  (test (parse '{if true 8 9})
       (ifC (boolC true) (numC 8) (numC 9)))
  (test (parse '{delay {+ 1 1}})
        (delayC (plusC (numC 1) (numC 1))))
  (test (parse '{force {+ 1 1}})
        (forceC (plusC (numC 1) (numC 1))))

  (test (parse '{delay {+ 1 {lambda {x} x}}})
        (delayC (plusC (numC 1) (lamC 'x (idC 'x)))))
  
  (test (parse '{force {delay {+ 1 {lambda {x} x}}}})
        (forceC (delayC (plusC (numC 1) (lamC 'x (idC 'x))))))
  )

;; interp ----------------------------------------
(define (interp [a : ExprC] [env : Env]) : Value
  (type-case ExprC a   
    [numC (n) (numV n)]
    [boolC (b) (boolV b)]
    [idC (s) (lookup s env)]
    [plusC (l r) (num+ (interp l env) (interp r env))]
    [multC (l r) (num* (interp l env) (interp r env))]
    [letC (n rhs body)
          (interp body
                  (extend-env
                   (bind n (interp rhs env))
                   env))]
    [lamC (n body)
          (closV n body env)]
    [equalC (lhs rhs)
            (num-eq (interp lhs env) (interp rhs env))]
    
    [ifC (t l r) 
         (type-case Value (interp t env)
           [boolV (v)
                  (cond [v (interp l env)]
                        [else (interp r env)])]
           [else (error 'interp "not a boolean")])
         ]
    [delayC (body) (thunkV body env)] 
    [forceC (thunk-expr) (type-case Value (interp thunk-expr env)
                  [thunkV (v e) (interp v e)]
            [else (error 'interp "not a thunk")])]
            ;(type-case Value (interp v env)
             ; [thunkV (t) (interp t env)]
              ;[else (error 'interp "not a thunk")])]
    ;[forceC (body)
     ;       (type-case Value body
      ;        [thunkV (v) (interp v env)]
       ;       [else (error 'interp "not a thunk")])]
    
    [appC (fun arg) (type-case Value (interp fun env)
                      [closV (n body c-env)
                             (interp body
                                     (extend-env
                                      (bind n
                                            (interp arg env))
                                      c-env))]
                      [else (error 'interp "not a function")])]))
(module+ test
  (test (interp (parse '2) mt-env)
        (numV 2))
  (test/exn (interp (parse `x) mt-env)
            "free variable")
  (test (interp (parse `x) 
                (extend-env (bind 'x (numV 9)) mt-env))
        (numV 9))
  (test (interp (parse '{+ 2 1}) mt-env)
        (numV 3))
  (test (interp (parse '{* 2 1}) mt-env)
        (numV 2))
  (test (interp (parse '{+ {* 2 3} {+ 5 8}})
                mt-env)
        (numV 19))
  (test (interp (parse '{lambda {x} {+ x x}})
                mt-env)
        (closV 'x (plusC (idC 'x) (idC 'x)) mt-env))
  (test (interp (parse '{let {[x 5]}
                          {+ x x}})
                mt-env)
        (numV 10))
  (test (interp (parse '{let {[x 5]}
                          {let {[x {+ 1 x}]}
                            {+ x x}}})
                mt-env)
        (numV 12))
  (test (interp (parse '{let {[x 5]}
                          {let {[y 6]}
                            x}})
                mt-env)
        (numV 5))
  (test (interp (parse '{{lambda {x} {+ x x}} 8})
                mt-env)
        (numV 16))

  (test/exn (interp (parse '{1 2}) mt-env)
            "not a function")
  (test/exn (interp (parse '{+ 1 {lambda {x} x}}) mt-env)
            "not a number")
  (test/exn (interp (parse '{let {[bad {lambda {x} {+ x y}}]}
                              {let {[y 5]}
                                {bad 2}}})
                    mt-env)
            "free variable")
  (test (interp (parse '{= 8 8}) mt-env)
        (boolV true))        
  (test (interp (parse '{= {+ 4 4} 8}) mt-env)
                (boolV true))
  (test (interp (parse '{= {+ 4 3} 8}) mt-env)
                (boolV false))
  
  (test (interp (parse '{if {= 2 {+ 1 1}} 7 8})
                mt-env)
        (interp (parse '7)
                mt-env))
  
  (test (interp (parse '{if false {+ 1 {lambda {x} x}} 9})
                mt-env)
        (interp (parse '9)
                mt-env))
  
  (test (interp (parse '{if true 10 {+ 1 {lambda {x} x}}})
                mt-env)
        (interp (parse '10)
                mt-env))
  
  (test/exn (interp (parse '{if 1 2 3})
                    mt-env)
            "not a boolean")

  ;;1
  (test (interp (parse '{delay {+ 1 {lambda {x} x}}}) mt-env)
        (thunkV (plusC (numC 1) (lamC 'x (idC 'x))) mt-env))
  
  ;;2
  (test/exn (interp (parse '{force 1})
                    mt-env)
            "not a thunk")
  ;;3
  (test (interp (parse '{force {if {= 8 8} {delay 7} {delay 9}}})
                mt-env)
        (interp (parse '7)
                mt-env))
  ;;4
  (test (interp (parse '{let {[d {let {[y 8]}
                                   {delay {+ y 7}}}]}
                          {let {[y 9]}
                            {force d}}})
                mt-env)
        (interp (parse '15)
                mt-env))
  
  #;
  (time (interp (parse '{let {[x2 {lambda {n} {+ n n}}]}
                          {let {[x4 {lambda {n} {x2 {x2 n}}}]}
                            {let {[x16 {lambda {n} {x4 {x4 n}}}]}
                              {let {[x256 {lambda {n} {x16 {x16 n}}}]}
                                {let {[x65536 {lambda {n} {x256 {x256 n}}}]}
                                  {x65536 1}}}}}})
                mt-env)))

;;num=--------------------------------------------------
(define (num-eq [l : Value] [r : Value]) : Value
  (cond
   [(and (numV? l) (numV? r))
    (boolV (= (numV-n l) (numV-n r)))]
   [else
    (error 'interp "not a number")])
  )

(module+ test
  (test (num-eq (numV 8) (numV 8)) (boolV true))
  (test (num-eq (numV 8) (numV 9)) (boolV false))
  (test/exn (num-eq (boolV true) (numV 9)) "not a number")
  )

;; num+ and num* ----------------------------------------
(define (num-op [op : (number number -> number)] [l : Value] [r : Value]) : Value
  (cond
   [(and (numV? l) (numV? r))
    (numV (op (numV-n l) (numV-n r)))]
   [else
    (error 'interp "not a number")]))
(define (num+ [l : Value] [r : Value]) : Value
  (num-op + l r))
(define (num* [l : Value] [r : Value]) : Value
  (num-op * l r))
(define (num= [l : Value] [r : Value]) : Value
  (num-eq l r))

(module+ test
  (test (num+ (numV 1) (numV 2))
        (numV 3))
  (test (num* (numV 2) (numV 3))
        (numV 6))
  (test (num= (numV 2) (numV 2))
        (boolV true))
  )

;; lookup ----------------------------------------
(define (lookup [n : symbol] [env : Env]) : Value
  (cond
   [(empty? env) (error 'lookup "free variable")]
   [else (cond
          [(symbol=? n (bind-name (first env)))
           (bind-val (first env))]
          [else (lookup n (rest env))])]))

(module+ test
  (test/exn (lookup 'x mt-env)
            "free variable")
  (test (lookup 'x (extend-env (bind 'x (numV 8)) mt-env))
        (numV 8))
  (test (lookup 'x (extend-env
                    (bind 'x (numV 9))
                    (extend-env (bind 'x (numV 8)) mt-env)))
        (numV 9))
  (test (lookup 'y (extend-env
                    (bind 'x (numV 9))
                    (extend-env (bind 'y (numV 8)) mt-env)))
        (numV 8)))
  