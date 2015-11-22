#lang plai-typed
(require plai-typed/s-exp-match)

(define-type Value
  [litV (n : number)]
  [closV (arg : symbol)
         (body : ExprC)
         (env : Env)])

(define-type ExprC
  [litC (n : number)]
  [idC (s : symbol)]
  [plusC (l : ExprC) 
         (r : ExprC)]
  [multC (l : ExprC)
         (r : ExprC)]
  [lamC (n : symbol)
        (body : ExprC)]
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
(define parse : (s-expression -> ExprC)
  (lambda (s)
    (cond
     [(s-exp-match? `NUMBER s) (litC (s-exp->number s))]
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
        (appC (lamC (s-exp->symbol (first bs))
                    (parse (third (s-exp->list s))))
              (parse (second bs))))]
     [(s-exp-match? '{lambda {SYMBOL} ANY} s)
      (lamC (s-exp->symbol (first (s-exp->list 
                                   (second (s-exp->list s)))))
            (parse (third (s-exp->list s))))]
     [(s-exp-match? '{ANY ANY} s)
      (appC (parse (first (s-exp->list s)))
            (parse (second (s-exp->list s))))]
     [else (error 'parse "invalid input")])))

(define (parse/num [s : s-expression]) : ExprC
  (parse s))

(module+ test
  (test (parse/num '2)
        (litC 2))
  (test (parse/num `x) ; note: backquote instead of normal quote
        (idC 'x))
  (test (parse/num '{+ 2 1})
        (plusC (litC 2) (litC 1)))
  (test (parse/num '{* 3 4})
        (multC (litC 3) (litC 4)))
  (test (parse/num '{+ {* 3 4} 8})
        (plusC (multC (litC 3) (litC 4))
               (litC 8)))
  (test (parse/num '{let {[x {+ 1 2}]}
                      y})
        (appC (lamC 'x (idC 'y))
              (plusC (litC 1) (litC 2))))
  (test (parse/num '{lambda {x} 9})
        (lamC 'x (litC 9)))
  (test (parse/num '{double 9})
        (appC (idC 'double) (litC 9)))
  (test/exn (parse/num '{{+ 1 2}})
            "invalid input")
  (test/exn (parse/num '"a")
            "invalid input"))

;; interp ----------------------------------------
(define interp : (ExprC Env -> Value)
  (lambda (a env)
    (type-case ExprC a
      [litC (n) (litV n)]
      [idC (s) (lookup s env)]
      [plusC (l r) (num+ (interp l env) (interp r env))]
      [multC (l r) (num* (interp l env) (interp r env))]
      [lamC (n body)
            (closV n body env)]
      [appC (fun arg) (type-case Value (interp fun env)
                        [closV (n body c-env)
                               (interp body
                                       (extend-env
                                        (bind n
                                              (interp arg env))
                                        c-env))]
                        [else (error 'interp "not a function")])])))

(define (interp/num [a : ExprC] [env : Env]) : Value
  (interp a env))

(module+ test
  (test (interp/num (parse/num '2) mt-env)
        (litV 2))
  (test/exn (interp/num (parse/num `x) mt-env)
            "free variable")
  (test (interp/num (parse/num `x) 
                    (extend-env (bind 'x (litV 9)) mt-env))
        (litV 9))
  (test (interp/num (parse/num '{+ 2 1}) mt-env)
        (litV 3))
  (test (interp/num (parse/num '{* 2 1}) mt-env)
        (litV 2))
  (test (interp/num (parse/num '{+ {* 2 3} {+ 5 8}})
                    mt-env)
        (litV 19))
  (test (interp/num (parse/num '{lambda {x} {+ x x}})
                    mt-env)
        (closV 'x (plusC (idC 'x) (idC 'x)) mt-env))
  (test (interp/num (parse/num '{let {[x 5]}
                                  {+ x x}})
                    mt-env)
        (litV 10))
  (test (interp/num (parse/num '{let {[x 5]}
                                  {let {[x {+ 1 x}]}
                                    {+ x x}}})
                    mt-env)
        (litV 12))
  (test (interp/num (parse/num '{let {[x 5]}
                                  {let {[y 6]}
                                    x}})
                    mt-env)
        (litV 5))
  (test (interp/num (parse/num '{{lambda {x} {+ x x}} 8})
                    mt-env)
        (litV 16))

  (test/exn (interp/num (parse/num '{1 2}) mt-env)
            "not a function")
  (test/exn (interp/num (parse/num '{+ 1 {lambda {x} x}}) mt-env)
            "not a literal")
  (test/exn (interp/num (parse/num '{let {[bad {lambda {x} {+ x y}}]}
                                      {let {[y 5]}
                                        {bad 2}}})
                        mt-env)
            "free variable"))

;; num+ and num* ----------------------------------------
(define num-op : ((number number -> number)
                  Value
                  Value
                  -> Value)
  (lambda (op l r)
    (cond
     [(and (litV? l) (litV? r))
      (litV (op (litV-n l) (litV-n r)))]
     [else
      (error 'interp "not a literal")])))

(define (num+ [l : Value] [r : Value]) : Value
  (num-op + l r))
(define (num* [l : Value] [r : Value]) : Value
  (num-op * l r))

(module+ test
  (test (num+ (litV 1) (litV 2))
        (litV 3))
  (test (num* (litV 2) (litV 3))
        (litV 6)))

;; lookup ----------------------------------------
(define lookup : (symbol Env -> Value)
  (lambda (n env)
    (cond
     [(empty? env) (error 'lookup "free variable")]
     [else (cond
            [(symbol=? n (bind-name (first env)))
             (bind-val (first env))]
            [else (lookup n (rest env))])])))

(module+ test
  (test/exn (lookup 'x mt-env)
            "free variable")
  (test (lookup 'x (extend-env (bind 'x (litV 8)) mt-env))
        (litV 8))
  (test (lookup 'x (extend-env
                    (bind 'x (litV 9))
                    (extend-env (bind 'x (litV 8)) mt-env)))
        (litV 9))
  (test (lookup 'y (extend-env
                    (bind 'x (litV 9))
                    (extend-env (bind 'y (litV 8)) mt-env)))
        (litV 8)))