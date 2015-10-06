#lang plai-typed
(require plai-typed/s-exp-match)

(define-type Value
  [numV (n : number)]
  [closV (arg : symbol)
         (body : ExprC)
         (env : Env)]
  [consV (lst : (listof ExprC))])

(define-type Thunk
  [delay (body : ExprC)
         (env : Env)
         (done : (boxof (optionof Value)))])

(define-type ExprC
  [numC (n : number)]
  [idC (s : symbol)]
  [plusC (l : ExprC) 
         (r : ExprC)]
  [multC (l : ExprC)
         (r : ExprC)]
  [lamC (n : symbol)
        (body : ExprC)]
  [appC (fun : ExprC)
        (arg : ExprC)]
  [if0C (test : ExprC)
        (t : ExprC)
        (f : ExprC)]
  [consC (rhs : ExprC)
         (lhs : ExprC)]
  [firstC (e : ExprC)]
  [restC (e : ExprC)])

(define-type Binding
  [bind (name : symbol)
        (val : Thunk)])

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
    [(s-exp-match? '{if0 ANY ANY ANY} s)
     (if0C (parse (second (s-exp->list s)))
          (parse (third (s-exp->list s)))
          (parse (fourth (s-exp->list s))))]
    [(s-exp-match? '{cons ANY ANY} s)
     (consC (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(s-exp-match? '{first ANY} s)
     (firstC (parse (second (s-exp->list s))))]            

    [(s-exp-match? '{rest ANY} s)
     (restC (parse (second (s-exp->list s))))]            

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
        (appC (lamC 'x (idC 'y))
              (plusC (numC 1) (numC 2))))
  (test (parse '{lambda {x} 9})
        (lamC 'x (numC 9)))
  (test (parse '{double 9})
        (appC (idC 'double) (numC 9)))
  (test/exn (parse '{{+ 1 2}})
            "invalid input"))

;; interp ----------------------------------------
(define (interp [a : ExprC] [env : Env]) : Value
  (type-case ExprC a
    [numC (n) (numV n)]
    [idC (s) (force (lookup s env))]
    [plusC (l r) (num+ (interp l env)
                       (interp r env))]
    [multC (l r) (num* (interp l env)
                       (interp r env))]
    [lamC (n body)
          (closV n body env)]
    
    [if0C (test t f)
          (type-case Value (interp test env)
            [numV (n)
                  (if (equal? n 0) (interp t env) (interp f env))]
            [else (error 'interp "not a number")])]
    [consC (lhs rhs)
           (consV (cons lhs (list rhs)))]
    [firstC (e)
            (interp (first
                     (type-case Value (interp e env)
                       [consV (l) l]
                       [else (error 'interp "not a cons")]))
                    env)]
    
    [restC (e)
           (type-case Value (interp e env)
             [consV (l)
                    (if (= (length (rest l)) 1)                    
                        (interp (first (rest l)) env)
                        (consV (rest l)))]
             [else (error 'interp "not a cons")])]
               
    [appC (fun arg) (type-case Value (interp fun env)
                      [closV (n body c-env)
                             (interp body
                                     (extend-env
                                      (bind n (delay arg env (box (none))))
                                      c-env))]
                      [else (error 'interp "not a function")])]))

(define (interp-expr (a : ExprC)) : s-expression   
  (type-case Value (interp a mt-env)
    [numV (n) (number->s-exp n)]
    [closV (arg bod env) `function]
    [consV (l) `cons]))

(module+ test

  
  ;;14
  ;(test (interp-expr (parse '{first {cons {+ 1 {lambda {y} y}}
   ;                                      4}}))
    ;    '4) 
  
  ;;Begin of lazy tests ____________________________________________________________

  (test (interp {parse '{{lambda {x} 0}
                         {1 2}}}
                mt-env) (numV 0))
  
  ;1
  (test (interp-expr (parse '10))
        '10)
  ;2
  (test (interp-expr (parse '{+ 10 17}))
        '27)
  ;3
  (test (interp-expr (parse '{* 10 7}))
        '70)
  ;4
  (test (interp-expr (parse '{{lambda {x} {+ x 12}}
                              {+ 1 17}}))
        '30)
  ;5
  (test (interp-expr (parse '{let {[x 0]}
                               {let {[f {lambda {y} {+ x y}}]}
                                 {+ {f 1}
                                    {let {[x 3]}
                                      {f 2}}}}}))
        '3)
  ;6
  (test (interp-expr (parse '{if0 0 1 2}))
        '1)
  ;7
  (test (interp-expr (parse '{if0 1 1 2}))
        '2)
  ;8
  (test (interp-expr (parse '{cons 1 2}))
        `cons)
  ;9
  (test (interp-expr (parse '{first {cons 1 2}}))
        '1)
  ;10
  (test (interp-expr (parse '{rest {cons 1 2}}))
        '2)
  ;11
  ;; Lazy evaluation:
  (test (interp-expr (parse '{{lambda {x} 0}
                              {+ 1 {lambda {y} y}}}))
        '0)
  ;;12
  (test (interp-expr (parse '{let {[x {+ 1 {lambda {y} y}}]}
                               0}))
        '0)
  ;;13
  (test (interp-expr (parse '{first {cons 3
                                          {+ 1 {lambda {y} y}}}}))
        '3)
  ;;14
  (test (interp-expr (parse '{rest {cons {+ 1 {lambda {y} y}}
                                         4}}))
        '4)
  ;;15
  (test (interp-expr (parse '{first {cons 5
                                          ;; Infinite loop:
                                          {{lambda {x} {x x}}
                                           {lambda {x} {x x}}}}}))
        '5)
  ;;16
  (test (interp-expr 
         (parse 
          '{let {[mkrec
                  ;; This is call-by-name mkrec
                  ;;  (simpler than call-by-value):
                  {lambda {body-proc}
                    {let {[fX {lambda {fX}
                                {body-proc {fX fX}}}]}
                      {fX fX}}}]}
              {let {[fib
                     {mkrec
                      {lambda {fib}
                        ;; Fib:
                        {lambda {n}
                          {if0 n
                               1
                               {if0 {+ n -1}
                                    1
                                    {+ {fib {+ n -1}}
                                       {fib {+ n -2}}}}}}}}]}
                ;; Call fib on 4:
                {fib 4}}}))
        '5)
  ;;17
  (test (interp-expr 
         (parse 
          '{let {[mkrec
                  ;; This is call-by-name mkrec
                  ;;  (simpler than call-by-value):
                  {lambda {body-proc}
                    {let {[fX {lambda {fX}
                                {body-proc {fX fX}}}]}
                      {fX fX}}}]}
             {let {[nats-from
                    {mkrec
                     {lambda {nats-from}
                       ;; nats-from:
                       {lambda {n}
                         {cons n {nats-from {+ n 1}}}}}}]}
               {let {[list-ref
                      {mkrec
                       {lambda {list-ref}
                         ;; list-ref:
                         {lambda {n}
                           {lambda {l}
                             {if0 n
                                  {first l}
                                  {{list-ref {+ n -1}} {rest l}}}}}}}]}
                 ;; Call list-ref on infinite list:
                 {{list-ref 4} {nats-from 2}}}}}))
        '6)

  ;;Begin of lazy tests ____________________________________________________________
  
  (test (interp (parse '2) mt-env)
        (numV 2))
  (test/exn (interp (parse `x) mt-env)
            "free variable")
  (test (interp (parse `x) 
                (extend-env (bind 'x (delay (numC 9) mt-env (box (none)))) mt-env))
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

  (test (interp (parse '{{lambda {x} 5} {1 2}})
                 mt-env)
        (numV 5))

  (test/exn (interp (parse '{1 2}) mt-env)
            "not a function")
  (test/exn (interp (parse '{+ 1 {lambda {x} x}}) mt-env)
            "not a number")
  (test/exn (interp (parse '{let {[bad {lambda {x} {+ x y}}]}
                              {let {[y 5]}
                                {bad 2}}})
                    mt-env)
            "free variable")

  #;
  (time (interp (parse '{let {[x2 {lambda {n} {+ n n}}]}
                          {let {[x4 {lambda {n} {x2 {x2 n}}}]}
                            {let {[x16 {lambda {n} {x4 {x4 n}}}]}
                              {let {[x256 {lambda {n} {x16 {x16 n}}}]}
                                {let {[x65536 {lambda {n} {x256 {x256 n}}}]}
                                  {x65536 1}}}}}})
                mt-env)))

;; force ----------------------------------------

(define (force [t : Thunk]) : Value
  (type-case Thunk t
    [delay (b e d) (type-case (optionof Value) (unbox d)
                     [none ()
                           (let ([v (interp b e)])
                             (begin
                               (set-box! d (some v))
                               v))]
                     [some (v) v])]))

(module+ test
  (test (force (delay (numC 8) mt-env (box (none))))
        (numV 8))
  (test (let ([v (delay (numC 8) mt-env (box (none)))])
          (begin
            (force v)
            (force v)))
        (numV 8))
  (test (force (delay (numC 8) mt-env (box (some (numV 9)))))
        (numV 9))
  (test (force (delay (idC 'x)
                      (extend-env (bind 'x (delay (numC 9) mt-env (box (none))))
                                  mt-env)
                      (box (none))))
        (numV 9)))

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

(module+ test
  (test (num+ (numV 1) (numV 2))
        (numV 3))
  (test (num* (numV 2) (numV 3))
        (numV 6)))

;; lookup ----------------------------------------
(define (lookup [n : symbol] [env : Env]) : Thunk
  (cond
   [(empty? env) (error 'lookup "free variable")]
   [else (cond
          [(symbol=? n (bind-name (first env)))
           (bind-val (first env))]
          [else (lookup n (rest env))])]))

(module+ test
  (test/exn (lookup 'x mt-env)
            "free variable")
  (test (lookup 'x (extend-env (bind 'x (delay (numC 8) mt-env (box (none)))) mt-env))
        (delay (numC 8) mt-env (box (none))))
  (test (lookup 'x (extend-env
                    (bind 'x (delay (numC 9) mt-env (box (none))))
                    (extend-env (bind 'x (delay (numC 8) mt-env (box (none)))) mt-env)))
        (delay (numC 9) mt-env (box (none))))
  (test (lookup 'y (extend-env
                    (bind 'x (delay (numC 9) mt-env (box (none))))
                    (extend-env (bind 'y (delay (numC 8) mt-env (box (none)))) mt-env)))
        (delay (numC 8) mt-env (box (none)))))