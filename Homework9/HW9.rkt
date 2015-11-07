#lang plai-typed
(require plai-typed/s-exp-match)

;NOTE: When we have an expression typecheck it!
(define-type Value
  [numV (n : number)]
  [boolV (b : boolean)]
  [closV (arg : symbol)
         (body : ExprC)
         (env : Env)])

(define-type ExprC
  [numC (n : number)]
  [idC (s : symbol)]
  [plusC (l : ExprC) 
         (r : ExprC)]
  [multC (l : ExprC)
         (r : ExprC)]
  [lamC (n : symbol)
        (arg-type : Type)
        (body : ExprC)]
  [appC (fun : ExprC)
        (arg : ExprC)]
  [boolC (t : boolean)]
  [equalC (lhs : ExprC)
          (rhs : ExprC)]
  [ifC (tst : ExprC)
       (then : ExprC)
       (els : ExprC)])
  

(define-type Type
  [numT]
  [boolT]
  [arrowT (arg : Type)
          (result : Type)])

(define-type Binding
  [bind (name : symbol)
        (val : Value)])

(define-type-alias Env (listof Binding))

(define-type TypeBinding
  [tbind (name : symbol)
         (type : Type)])

(define-type-alias TypeEnv (listof TypeBinding))

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
    [(s-exp-match? '{= ANY ANY} s)
     (equalC (parse (second (s-exp->list s)))
              (parse (third (s-exp->list s))))]
    [(s-exp-match? '{let {[SYMBOL : ANY ANY]} ANY} s)
     (let ([bs (s-exp->list (first
                             (s-exp->list (second
                                           (s-exp->list s)))))])
       (appC (lamC (s-exp->symbol (first bs))
                   (parse-type (third bs))
                   (parse (third (s-exp->list s))))
             (parse (fourth bs))))]
    [(s-exp-match? '{lambda {[SYMBOL : ANY]} ANY} s)
     (let ([arg (s-exp->list
                 (first (s-exp->list 
                         (second (s-exp->list s)))))])
       (lamC (s-exp->symbol (first arg))
             (parse-type (third arg))
             (parse (third (s-exp->list s)))))]
    [(s-exp-match? '{if ANY ANY ANY} s)
     (ifC (parse (second (s-exp->list s)))
          (parse (third (s-exp->list s)))
          (parse (fourth (s-exp->list s))))]    
    [(s-exp-match? '{ANY ANY} s)
     (appC (parse (first (s-exp->list s)))
           (parse (second (s-exp->list s))))]
    [else (error 'parse "invalid input")]))

(define (parse-type [s : s-expression]) : Type
  (cond
   [(s-exp-match? `num s) 
    (numT)]
   [(s-exp-match? `bool s)
    (boolT)]   
   [(s-exp-match? `(ANY -> ANY) s)
    (arrowT (parse-type (first (s-exp->list s)))
            (parse-type (third (s-exp->list s))))]
   [else (error 'parse-type "invalid input")]))

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
  (test (parse '{let {[x : num {+ 1 2}]}
                  y})
        (appC (lamC 'x (numT) (idC 'y))
              (plusC (numC 1) (numC 2))))
  (test (parse '{lambda {[x : num]} 9})
        (lamC 'x (numT) (numC 9)))
  (test (parse '{double 9})
        (appC (idC 'double) (numC 9)))
  (test/exn (parse '{{+ 1 2}})
            "invalid input")

  (test (parse-type `num)
        (numT))
  (test (parse-type `bool)
        (boolT))
  (test (parse-type `(num -> bool))
        (arrowT (numT) (boolT)))
  (test/exn (parse-type '1)
            "invalid input"))

;; interp ----------------------------------------
(define (interp [a : ExprC] [env : Env]) : Value
  (type-case ExprC a
    [numC (n) (numV n)]
    [boolC (b) (boolV b)]
    [idC (s) (lookup s env)]
    [plusC (l r) (num+ (interp l env) (interp r env))]
    [multC (l r) (num* (interp l env) (interp r env))]
    [equalC (lhs rhs)
            (num-eq (interp lhs env) (interp rhs env))]
    [ifC (t l r) 
         (if (equal? #t (boolV-b (interp t env)))
                     (interp l env)
                     (interp r env))]         
    [lamC (n t body)
          (closV n body env)]
    [appC (fun arg) (type-case Value (interp fun env)
                      [closV (n body c-env)
                             (interp body
                                     (extend-env
                                      (bind n
                                            (interp arg env))
                                      c-env))]
                      [else (error 'interp "not a function")])]))

(module+ test

  (test (interp (parse '{= 10 10}) mt-env)
        (boolV true))
  ;;----------------------------------------------------
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
  (test (interp (parse '{lambda {[x : num]} {+ x x}})
                mt-env)
        (closV 'x (plusC (idC 'x) (idC 'x)) mt-env))
  (test (interp (parse '{let {[x : num 5]}
                          {+ x x}})
                mt-env)
        (numV 10))
  (test (interp (parse '{let {[x : num 5]}
                          {let {[x : num {+ 1 x}]}
                            {+ x x}}})
                mt-env)
        (numV 12))
  (test (interp (parse '{let {[x : num 5]}
                          {let {[y : num 6]}
                            x}})
                mt-env)
        (numV 5))
  (test (interp (parse '{{lambda {[x : num]} {+ x x}} 8})
                mt-env)
        (numV 16))

  (test/exn (interp (parse '{1 2}) mt-env)
            "not a function")
  (test/exn (interp (parse '{+ 1 {lambda {[x : num]} x}}) mt-env)
            "not a number")
  (test/exn (interp (parse '{let {[bad : (num -> num) {lambda {[x : num]} {+ x y}}]}
                              {let {[y : num 5]}
                                {bad 2}}})
                    mt-env)
            "free variable"))

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

;; Implement for booleans as well (true and false)
;;num=--------------------------------------------------
(define (num-eq [l : Value] [r : Value]) : Value  
  ;(cond
   ;;[(and (numV? l) (numV? r))
    (boolV (= (numV-n l) (numV-n r)))
   ;[else
    ;(error 'interp "not a number")])
  )

(module+ test
  (test (num+ (numV 1) (numV 2))
        (numV 3))
  (test (num* (numV 2) (numV 3))
        (numV 6)))

;; lookup ----------------------------------------
(define (make-lookup [name-of : ('a -> symbol)] [val-of : ('a -> 'b)])
  (lambda ([name : symbol] [vals : (listof 'a)]) : 'b
    (cond
     [(empty? vals)
      (error 'find "free variable")]
     [else (if (equal? name (name-of (first vals)))
               (val-of (first vals))
               ((make-lookup name-of val-of) name (rest vals)))])))

(define lookup
  (make-lookup bind-name bind-val))

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

;; typecheck ----------------------------------------
(define (typecheck [a : ExprC] [tenv : TypeEnv])
  (type-case ExprC a
    [numC (n) (numT)]
    [boolC (t) (boolT)]    
    [plusC (l r) (typecheck-nums l r tenv)]
    [multC (l r) (typecheck-nums l r tenv)]
    [equalC (l r) (typecheck-nums-bools l r tenv)]
    [ifC (tst thn els)
         (type-case Type (typecheck-bool tst tenv)
           [boolT ()
                  (local [(define t (typecheck thn tenv))
                          (define e (typecheck els tenv))]
                    (if (equal? t e)
                        t
                        (type-error t "no type")))]
           [else (type-error tst "bool")])]
    [idC (n) (type-lookup n tenv)]
    [lamC (n arg-type body)
          (arrowT arg-type
                  (typecheck body 
                             (extend-env (tbind n arg-type)
                                         tenv)))]
    [appC (fun arg)
          (type-case Type (typecheck fun tenv)
            [arrowT (arg-type result-type)
                    (if (equal? arg-type
                                (typecheck arg tenv))
                        result-type
                        (type-error arg
                                    (to-string arg-type)))]
            [else (type-error fun "function")])]))

(define (typecheck-bool tst tenv)
  (type-case Type (typecheck tst tenv)
    [boolT () (boolT)]
    [else (type-error tst "bool")]))

(define (typecheck-nums l r tenv)
  (type-case Type (typecheck l tenv)
    [numT ()
          (type-case Type (typecheck r tenv)
            [numT () (numT)]
            [else (type-error r "num")])]
    [else (type-error l "num")]))

(define (typecheck-nums-bools l r tenv)
  (type-case Type (typecheck l tenv)
    [numT ()
          (type-case Type (typecheck r tenv)
            [numT () (boolT)]
            [else (type-error r "num")])]
    [boolT ()
           (type-case Type (typecheck r tenv)
             [boolT () (boolT)]
             [else (type-error r "bool")])]
    [else (type-error l "num")]))


(define (type-error a msg)
  (error 'typecheck (string-append
                     "no type: "
                     (string-append
                      (to-string a)
                      (string-append " not "
                                     msg)))))

(define type-lookup
  (make-lookup tbind-name tbind-type))

(module+ test

  #|;;Part 2 — Pairs ---------------------------------------
  (test (interp (parse '{cons 10 8})
                mt-env)
        ;; Your constructor might be different than consV:
        (consV (numV 10) (numV 8)))
  
  (test (interp (parse '{first {cons 10 8}})
                mt-env)
        (numV 10))
  
  (test (interp (parse '{rest {cons 10 8}})
                mt-env)
        (numV 8))
  
  (test (typecheck (parse '{cons 10 8})
                   mt-env)
        ;; Your constructor might be different than crossT:
        (crossT (numT) (numT)))
  
  (test (typecheck (parse '{first {cons 10 8}})
                   mt-env)
        (numT))
  
  (test (typecheck (parse '{+ 1 {rest {cons 10 8}}})
                   mt-env)
        (numT))
  
  (test (typecheck (parse '{lambda {[x : (num * bool)]}
                             {first x}})
                   mt-env)
        ;; Your constructor might be different than crossT:
        (arrowT (crossT (numT) (boolT)) (numT)))
  
  (test (typecheck (parse '{{lambda {[x : (num * bool)]}
                              {first x}}
                            {cons 1 false}})
                   mt-env)
        (numT))
  
  (test (typecheck (parse '{{lambda {[x : (num * bool)]}
                              {rest x}}
                            {cons 1 false}})
                   mt-env)
        (boolT))
  
  (test/exn (typecheck (parse '{first 10})
                       mt-env)
            "no type")
  
  (test/exn (typecheck (parse '{+ 1 {first {cons false 8}}})
                       mt-env)
            "no type")
  
  (test/exn (typecheck (parse '{lambda {[x : (num * bool)]}
                                 {if {first x}
                                     1
                                     2}})
                       mt-env)
            "no type")
|#
  ;;Part 1 — true, false, =, and if----------------------
  
  
  (test (interp (parse '{if {= 13 {if {= 1 {+ -1 2}}
                                      12
                                      13}}
                            4
                            5})
                mt-env)
         (numV 5))
  
  (test/exn (typecheck (parse '{if true 1 false}) mt-env)
        "no type")
  
  (test (typecheck (parse '{= 13 {if {= 1 {+ -1 2}}
                                     12
                                     13}})
                   mt-env)
         (boolT))
  
  (test/exn (typecheck (parse '{+ 1 {if true true false}})
                       mt-env)
            "no type")
  ;;-----------------------------------------------------
  (test (typecheck (parse '{= 10 10}) mt-env)
        (boolT))
  (test (typecheck (parse `true) mt-env)
        (boolT))
  (test (typecheck (parse `false) mt-env)
        (boolT))
  ;;-----------------------------------------------------
  (test (typecheck (parse '10) mt-env)
        (numT))
  (test (typecheck (parse '{+ 10 17}) mt-env)
        (numT))
  (test (typecheck (parse '{* 10 17}) mt-env)
        (numT))
  (test (typecheck (parse '{lambda {[x : num]} 12}) mt-env)
        (arrowT (numT) (numT)))
  (test (typecheck (parse '{lambda {[x : num]} {lambda {[y : bool]} x}}) mt-env)
        (arrowT (numT) (arrowT (boolT)  (numT))))

  (test (typecheck (parse '{{lambda {[x : num]} 12}
                            {+ 1 17}})
                   mt-env)
        (numT))

  (test (typecheck (parse '{let {[x : num 4]}
                             {let {[f : (num -> num)
                                      {lambda {[y : num]} {+ x y}}]}
                               {f x}}})
                   mt-env)
        (numT))

  (test/exn (typecheck (parse '{1 2})
                       mt-env)
            "no type")
  (test/exn (typecheck (parse '{{lambda {[x : bool]} x} 2})
                       mt-env)
            "no type")
  (test/exn (typecheck (parse '{+ 1 {lambda {[x : num]} x}})
                       mt-env)
            "no type")
  (test/exn (typecheck (parse '{* {lambda {[x : num]} x} 1})
                       mt-env)
            "no type"))