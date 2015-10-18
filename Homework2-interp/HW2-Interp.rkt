#lang plai-typed
(require plai-typed/s-exp-match)

(define-type ExprC
  [numC (n : number)]
  [idC (s : symbol)]
  [plusC (l : ExprC) 
         (r : ExprC)]
  [multC (l : ExprC)
         (r : ExprC)]
  [maxC (l : ExprC)
        (r : ExprC)]
  [appC (s : symbol)
        (args : (listof ExprC))]  
  [ifZeroC (exp : ExprC)
           (ifTrue : ExprC)
           (ifFalse : ExprC)]
  [letQC (q : ExprC) (body : ExprC)]
  [unletC (q : symbol) (body : ExprC)]
  [letC (n : symbol) 
        (rhs : ExprC)
        (body : ExprC)]
  
  )

(define-type FunDefC
  [fdC (name : symbol) 
       (arg : (listof symbol))
       (body : ExprC)])

(define-type Binding
  [bind (name : symbol)
        (val : number)])

(define-type-alias Env (listof Binding))

(define mt-env empty)
(define extend-env cons)

(module+ test
  (print-only-errors true))

;; parse ----------------------------------------
(define (parse [s : s-expression]) : ExprC
  (cond
    [(s-exp-match? `NUMBER s) (numC (s-exp->number s))]
    [(s-exp-match? `SYMBOL s) (idC (s-exp->symbol s))]
    [(s-exp-match? '{+ ANY ANY} s)
     (plusC (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(s-exp-match? '{* ANY ANY} s)
     (multC (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(s-exp-match? '{max ANY ANY} s)
     (maxC (parse (second (s-exp->list s)))
           (parse (third (s-exp->list s))))]
    
    [(s-exp-match? '{ifZero ANY ANY ANY} s)
     (ifZeroC (parse (second (s-exp->list s)))
              (parse (third (s-exp->list s)))
              (parse (fourth (s-exp->list s))))]
    [(s-exp-match? '{let-q ANY ANY} s)
     (letQC (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    
    [(s-exp-match? '{unlet SYMBOL ANY} s)
     (unletC (s-exp->symbol (second (s-exp->list s)))
             (parse (third (s-exp->list s))))]
    
    [(s-exp-match? '{let {[SYMBOL ANY]} ANY} s)
     (let ([bs (s-exp->list (first
                             (s-exp->list (second
                                           (s-exp->list s)))))])
       (letC (s-exp->symbol (first bs))
             (parse (second bs))
             (parse (third (s-exp->list s)))))]
    [(s-exp-match? '{SYMBOL ANY ...} s)
     
     (appC (s-exp->symbol (first (s-exp->list s)))
           (map parse (rest (s-exp->list s))))]
    
    [else (error 'parse "invalid input")]))

(define (parse-fundef [s : s-expression]) : FunDefC
  (cond
    [(s-exp-match?'{define {SYMBOL SYMBOL ...} ANY} s)
     (fdC (s-exp->symbol (first (s-exp->list (second (s-exp->list s)))))
          (map (lambda (arg) (s-exp->symbol arg)) (rest (s-exp->list (second (s-exp->list s)))))
          (parse (third (s-exp->list s))))]
    [else (error 'parse-fundef "invalid input")]))

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
  (test (parse '{double 9})
        (appC 'double (list (numC 9))))
  
  (test (parse '{double 9 8})
        (appC 'double (list (numC 9) (numC 8))))
  (test (parse '{double 9 8 10})
        (appC 'double (list (numC 9) (numC 8) (numC 10))))
  
  (test (parse '{let {[x {+ 1 2}]}
                  y})
        (letC 'x (plusC (numC 1) (numC 2))
              (idC 'y)))
  (test (parse '{ifZero x y z})
        (ifZeroC (idC 'x) (idC 'y) (idC 'z)))
  (test (parse '{let-q x y})
        (letQC (idC 'x) (idC 'y)))
  
  (test (parse '{unlet x y})
        (unletC 'x (idC 'y)))
  (test (parse '{unlet x {+ 2 1}})
        (unletC 'x (plusC (numC 2) (numC 1))))
  
  (test/exn (parse '{{+ 1 2}})
            "invalid input")
  
  (test (parse-fundef '{define {double x} {+ x x}})
        (fdC 'double (list 'x) (plusC (idC 'x) (idC 'x))))
  
  
  (test/exn (parse-fundef '{def {f x} x})
            "invalid input")
  
  (define double-def
    (parse-fundef '{define {double x} {+ x x}}))
  (define quadruple-def
    (parse-fundef '{define {quadruple x} {double {double x}}})))


;; get-fundef ----------------------------------------
(define (get-fundef [s : symbol] [fds : (listof FunDefC)]) : FunDefC
  (cond
    [(empty? fds) (error 'get-fundef "undefined function")]
    [(cons? fds) (if (eq? s (fdC-name (first fds)))
                     (first fds)
                     (get-fundef s (rest fds)))]))

(module+ test
  (test (get-fundef 'double (list double-def))
        double-def)
  (test (get-fundef 'double (list double-def quadruple-def))
        double-def)
  (test (get-fundef 'double (list quadruple-def double-def))
        double-def)
  (test (get-fundef 'quadruple (list quadruple-def double-def))
        quadruple-def)
  (test/exn (get-fundef 'double empty)
            "undefined function"))

;; lookup ----------------------------------------
(define (lookup [n : symbol] [env : Env]) : number
  (cond
    [(empty? env) (error 'lookup "free variable")]
    [else (cond
            [(symbol=? n (bind-name (first env)))
             (bind-val (first env))]
            [else (lookup n (rest env))])]))

(module+ test
  (test/exn (lookup 'x mt-env)
            "free variable")
  (test (lookup 'x (extend-env (bind 'x 8) mt-env))
        8)
  (test (lookup 'x (extend-env
                    (bind 'x 9)
                    (extend-env (bind 'x 8) mt-env)))
        9)
  (test (lookup 'y (extend-env
                    (bind 'x 9)
                    (extend-env (bind 'y 8) mt-env)))
        8))


(define (unbind [x : symbol] [prev-env : Env] [rest-env : Env]) : Env
  (cond
    [(empty? rest-env) prev-env]
    [(symbol=? x (bind-name(first rest-env)))
     (append prev-env (rest rest-env))]
    
    [(not (empty? rest-env))
     (unbind x
             (append prev-env (list (first rest-env)))
             (rest rest-env))]
    )
  )
(module+ test
  (test (unbind 't mt-env (list(bind 'z 3) (bind 'y 2) (bind 'x 1)))  (list(bind 'z 3) (bind 'y 2) (bind 'x 1)))
  (test (unbind 'y mt-env (list(bind 'z 3) (bind 'y 2) (bind 'x 1)))  (list(bind 'z 3) (bind 'x 1)))
  (test (unbind 'x mt-env (list(bind 'z 3) (bind 'y 2) (bind 'x 1)))  (list(bind 'z 3) (bind 'y 2)))
  (test (unbind 'z mt-env (list(bind 'z 3) (bind 'y 2) (bind 'x 1)))  (list(bind 'y 2) (bind 'x 1)))
  (test (unbind 'y mt-env mt-env) mt-env)
  )

(define (repeating-args? [current : symbol][args : (listof symbol)])
  (cond [(empty? args) #f]
        [(eq? current (first args)) #t]
        [else (repeating-args? (first args) (rest args))]))

(define (contain-duplicates? [lst : (listof symbol)])
  (cond
    [(empty? lst) #f]
    [(repeating-args? (first lst) (rest lst)) #t]
    [else #f])
  )


;; interp ----------------------------------------
(define (interp [a : ExprC] [env : Env] [fds : (listof FunDefC)]) : number
  (type-case ExprC a
    [numC (n) n]
    [idC (s) (lookup s env)]
    [plusC (l r) (+ (interp l env fds) (interp r env fds))]
    [multC (l r) (* (interp l env fds) (interp r env fds))]
    [maxC (l r) (if(> (interp l env fds) (interp r env fds)) (interp l env fds) (interp r env fds))]   
    
    [appC (s args) (local [(define fd (get-fundef s fds))]
                                                     (interp (fdC-body fd)
                                                             (foldl extend-env 
                                                                    (map2 bind (fdC-arg fd) (map (lambda (a) (interp a env fds)) args)) mt-env)
                                                             fds))]
    [ifZeroC (tst thn els) (if (= (interp tst env fds) 0)
                               (interp thn env fds)
                               (interp els env fds))]
    [letQC (q body)
           (interp body
                   (extend-env (bind 'q (interp q env fds))
                               env)
                   fds) ]
    
    [unletC (n body)
            (interp body                  
                    (unbind n mt-env env)
                    fds) ]
    
    [letC (n rhs body)
          (interp body
                  (extend-env 
                   (bind n (interp rhs env fds))
                   env)
                  fds)]))

(module+ test
  (test (interp (parse '2) mt-env empty)
        2)
  (test/exn (interp (parse `x) mt-env empty)
            "free variable")
  (test (interp (parse `x) 
                (extend-env (bind 'x 9) mt-env)
                empty)
        9)
  (test (interp (parse '{+ 2 1}) mt-env empty)
        3)
  (test (interp (parse '{* 2 1}) mt-env empty)
        2)
  (test (interp (parse '{+ {* 2 3} {+ 5 8}})
                mt-env
                empty)
        19)
  (test (interp (parse '{ifZero 0 1 2}) mt-env empty)
        1)
  (test/exn (interp (parse '{ifZero x y z}) mt-env empty)
            "free variable")
  (test (interp (parse '{ifZero x y z})
                (extend-env (bind 'x 5) (extend-env (bind 'z 10) mt-env))
                empty)
        10)
  
  (test (interp (parse '{double 8})
                mt-env
                (list double-def))
        16)
  (test (interp (parse '{quadruple 8})
                mt-env
                (list double-def quadruple-def))
        32)
  (test (interp (parse '{let {[x 5]}
                          {+ x x}})
                mt-env
                empty)
        10)
  (test (interp (parse '{let {[x 5]}
                          {let {[x {+ 1 x}]}
                            {+ x x}}})
                mt-env
                empty)
        12)
  (test (interp (parse '{let {[x 5]}
                          {let {[y 6]}
                            x}})
                mt-env
                empty)
        5)
  (test (interp (parse '{let-q 5 {+ 7 q}}) mt-env empty)
        12)
  (test/exn (interp (parse '{let {[y 5]}
                              {bad 2}})
                    mt-env
                    (list (parse-fundef '{define {bad x} {+ x y}})))
            "free variable"))

;;max-------------------------------------------------------------
(test (interp (parse '{max 1 2})
              mt-env
              (list))
      2)
(test (interp (parse '{max {+ 4 5} {+ 2 3}})
              mt-env
              (list))
      9)

;;unlet-------------------------------------------------------------
(module+ test
  (test/exn (interp (parse '{let {[x 1]}
                              {unlet x
                                     x}})
                    mt-env
                    (list))
            "free variable")
  (test (interp (parse '{let {[x 1]}
                          {+ {unlet x 1} x}})
                mt-env
                (list))
        2)
  (test (interp (parse '{let {[x 1]}
                          {let {[x 2]}
                            {+ x {unlet x x}}}})
                mt-env
                (list))
        3)
  (test (interp (parse '{let {[x 1]}
                          {let {[x 2]}
                            {let {[z 3]}
                              {+ x {unlet x {+ x z}}}}}})
                mt-env
                (list))
        6)
  (test (interp (parse '{f 2})
                mt-env
                (list (parse-fundef '{define {f z}
                                       {let {[z 8]}
                                         {unlet z
                                                z}}})))
        2)
  )

; Part 3 — Functions that Accept Multiple Arguments -----------------------------------------------------------
;(module+ test
(test (interp (parse '{f 1 2})
              mt-env
              (list (parse-fundef '{define {f x y} {+ x y}})))
      3)

(test/exn (interp (parse '{f 1 2})
                  mt-env
                  (list (parse-fundef '{define {f x x} {+ x y}})))
          "bad syntax")

(test (interp (parse '{f})
              mt-env
              (list (parse-fundef '{define {f} {+ 1 2}})))
      3)
(test (interp (parse '{f 1})
              mt-env
              (list (parse-fundef '{define {f x} {+ x x}})))
      2)
(test (interp (parse '{+ {f} {f}})
              mt-env
              (list (parse-fundef '{define {f} 5})))
      10)
(test/exn (interp (parse '{f 1})
                  mt-env
                  (list (parse-fundef '{define {f x y} {+ x y}})))
          "wrong arity")

(test (interp (parse '{f 1 2 3})
              mt-env
              (list (parse-fundef '{define {f x y z} {+ {+ x y} z}})))
      6)

(test (interp (parse '{f 1 3})
              mt-env
              (list (parse-fundef '{define {f x y} {* x y}})))
      3)

(test (interp (parse '{f 1 3})
              mt-env
              (list (parse-fundef '{define {f x y} {max x y}})))
      3)

(test (interp (parse '{f 1 3})
              mt-env
              (list (parse-fundef '{define {f x y} {max x y}})))
      3)

(test (interp (parse '{f 5 3 8 2})
              mt-env
              (list (parse-fundef '{define {f w x y z} {max {max w x} {max y z}}})))
      8)
;)

