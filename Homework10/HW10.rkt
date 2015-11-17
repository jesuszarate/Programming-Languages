#lang plai-typed
(require plai-typed/s-exp-match)
 
(define-type Value
  [numV (n : number)]
  [closV (arg : symbol)
         (body : ExprC)
         (env : Env)]
  [boolV (b : boolean)]
  [listV (elems : (listof Value))])

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
  [emptyC]
  [consC (l : ExprC)
         (r : ExprC)]
  [firstC (a : ExprC)]
  [restC (a : ExprC)]
  [boolC (t : boolean)])

(define-type Type
  [numT]
  [boolT]
  [arrowT (arg : Type)
          (result : Type)]
  [varT (is : (boxof (optionof Type)))]  
  [listofT (elem : Type)])

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
    [(s-exp-match? `empty s) (emptyC)]
    [(s-exp-match? `NUMBER s) (numC (s-exp->number s))]
    [(s-exp-match? `true s) (boolC #t)]
    [(s-exp-match? `false s) (boolC #f)]
    [(s-exp-match? `SYMBOL s) (idC (s-exp->symbol s))]    
    [(s-exp-match? '{+ ANY ANY} s)
     (plusC (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(s-exp-match? '{* ANY ANY} s)
     (multC (parse (second (s-exp->list s)))
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

(define (parse-type [s : s-expression]) : Type
  (cond
   [(s-exp-match? `num s) 
    (numT)]
   [(s-exp-match? `bool s)
    (boolT)]
   [(s-exp-match? `(ANY -> ANY) s)
    (arrowT (parse-type (first (s-exp->list s)))
            (parse-type (third (s-exp->list s))))]
   [(s-exp-match? `(listof ANY) s)
    (listofT (parse-type (second (s-exp->list s))))]
   [(s-exp-match? `? s) 
    (varT (box (none)))]
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
  (test (parse `empty)
        (emptyC))
  (test (parse '{cons 1 2})
        (consC (numC 1) (numC 2)))
  (test (parse '{first 1})
        (firstC (numC 1)))
  (test (parse '{rest 1})
        (restC (numC 1)))
  (test/exn (parse '{{+ 1 2}})
            "invalid input")

  (test (parse-type `num)
        (numT))
  (test (parse-type `bool)
        (boolT))
  (test (parse-type `(num -> bool))
        (arrowT (numT) (boolT)))
  (test (parse-type `?)
        (varT (box (none))))
  (test (parse-type `(listof num))
        (listofT (numT)))
  (test/exn (parse-type '1)
            "invalid input"))

;; interp ----------------------------------------
(define (interp [a : ExprC] [env : Env]) : Value
  (type-case ExprC a
    [numC (n) (numV n)]
    [idC (s) (lookup s env)]
    [plusC (l r) (num+ (interp l env) (interp r env))]
    [multC (l r) (num* (interp l env) (interp r env))]
    [lamC (n t body)
          (closV n body env)]
    [appC (fun arg) (type-case Value (interp fun env)
                      [closV (n body c-env)
                             (interp body
                                     (extend-env
                                      (bind n
                                            (interp arg env))
                                      c-env))]
                      [else (error 'interp "not a function")])]
    [emptyC () (listV empty)]
    [consC (l r) (let ([v-l (interp l env)]
                       [v-r (interp r env)])
                   ;(listV (cons v-l (listV-elems v-r))))]
                   (type-case Value v-r
                     [listV (elems) (listV (cons v-l elems))]
                     [else (error 'interp "not a list")]))]
    
    [firstC (a)
            (local ([define elems (listV-elems (interp a env))])
              (if (empty? elems)
                  (error 'interp "list is empty")
                  (first elems)))]
    [restC (a)
           (local [(define elems (listV-elems (interp a env)))]
             (if (empty? elems)
                 (error 'interp "list is empty")
                 (listV (rest elems))))]
    [boolC (b) (boolV b)]))
  
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
  (test (interp (parse `empty)
                mt-env)
        (listV empty))
  (test (interp (parse '{cons 1 empty})
                mt-env)
        (listV (list (numV 1))))
  (test (interp (parse '{first {cons 1 empty}})
                mt-env)
        (numV 1))
  (test (interp (parse '{rest {cons 1 empty}})
                mt-env)
        (listV empty))
  (test/exn (interp (parse '{cons 1 2})
                    mt-env)
            "not a list")
  ;(test/exn (interp (parse '{first 1})
   ;                 mt-env)
    ;        "not a list")
  ;(test/exn (interp (parse '{rest 1})
   ;                 mt-env)
    ;        "not a list")
  (test/exn (interp (parse '{first empty})
                    mt-env)
            "list is empty")
  (test/exn (interp (parse '{rest empty})
                    mt-env)
            "list is empty")

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
    [plusC (l r) (typecheck-nums l r tenv)]
    [multC (l r) (typecheck-nums l r tenv)]
    [idC (n) (type-lookup n tenv)]
    [lamC (n arg-type body)
          (arrowT arg-type
                  (typecheck body 
                             (extend-env (tbind n arg-type)
                                         tenv)))]
    [appC (fun arg)
          (local [(define result-type (varT (box (none))))]
            (begin
              (unify! (arrowT (typecheck arg tenv)
                              result-type)
                      (typecheck fun tenv)
                      fun)
              result-type))]
    ;; These are all wrong:
    [emptyC ()
            (listofT (varT (box (none))))]
    [consC (l r)
           (let ([v-l (typecheck l tenv)]
                 [v-r (typecheck r tenv)])
             (begin
               (unify! (listofT v-l)  v-r l)
               v-r))]
    
    [firstC (a) (local [(define result-type (varT (box (none))))]                  
                    (begin
                      (unify! (listofT result-type)
                              (typecheck a tenv)
                              a)
                      result-type))]
    
    [restC (a) (local [(define result-type (varT (box (none))))]
                 (begin
                   (unify! (listofT result-type)
                           (typecheck a tenv)
                           a)
                   (listofT result-type)))]
    [boolC (b) (boolT)]))

(define (typecheck-nums l r tenv)
  (begin
    (unify! (typecheck l tenv)
            (numT)
            l)
    (unify! (typecheck r tenv)
            (numT)
            r)
    (numT)))

(define type-lookup
  (make-lookup tbind-name tbind-type))

(module+ test
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

  (test (resolve (typecheck (parse '{{lambda {[x : num]} 12}
                                     {+ 1 17}})
                            mt-env))
        (numT))

  (test (resolve (typecheck (parse '{let {[x : num 4]}
                                      {let {[f : (num -> num)
                                               {lambda {[y : num]} {+ x y}}]}
                                        {f x}}})
                            mt-env))
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

;; unify! ----------------------------------------
(define (unify! [t1 : Type] [t2 : Type] [expr : ExprC])
  (type-case Type t1
    [varT (is1)
          (type-case (optionof Type) (unbox is1)
            [some (t3) (unify! t3 t2 expr)]
            [none ()
                  (local [(define t3 (resolve t2))]
                    (if (eq? t1 t3)
                        (values)
                        (if (occurs? t1 t3)
                            (type-error expr t1 t3)
                            (begin
                              (set-box! is1 (some t3))
                              (values)))))])]    
    [else
     (type-case Type t2
       [varT (is2) (unify! t2 t1 expr)]
       [numT () (type-case Type t1
                  [numT () (values)]
                  [else (type-error expr t1 t2)])]
       [boolT () (type-case Type t1
                   [boolT () (values)]
                   [else (type-error expr t1 t2)])]
       [arrowT (a2 b2) (type-case Type t1
                         [arrowT (a1 b1)
                                 (begin
                                   (unify! a1 a2 expr)
                                   (unify! b1 b2 expr))]
                         [else (type-error expr t1 t2)])]       
       [listofT (e2)
                (type-case Type t1                      
                       [listofT (e1)
                             (unify! e1 e2 expr)]
                       [else (type-error expr t1 t2)])
                ])]))       

(define (resolve [t : Type]) : Type
  (type-case Type t
    [varT (is)
          (type-case (optionof Type) (unbox is)
            [none () t]
            [some (t2) (resolve t2)])]
    [else t]))

(define (occurs? [r : Type] [t : Type]) : boolean
  (type-case Type t
    [numT () false]
    [boolT () false]
    [arrowT (a b)
            (or (occurs? r a)
                (occurs? r b))]
    [varT (is) (or (eq? r t) ; eq? checks for the same box
                   (type-case (optionof Type) (unbox is)
                     [none () false]
                     [some (t2) (occurs? r t2)]))]
    [listofT (e)             
             (occurs? r e)]))    
             

(define (type-error [a : ExprC] [t1 : Type] [t2 : Type])
  (error 'typecheck (string-append
                     "no type: "
                     (string-append
                      (to-string a)
                      (string-append
                       " type "
                       (string-append
                        (to-string t1)
                        (string-append
                         " vs. "
                         (to-string t2))))))))

(define (run-prog [s : s-expression]) : s-expression
  (local [(define p (parse s))]
    (type-case Type (typecheck p mt-env)
      [numT () (run-interp p)]
      [boolT () (run-interp p)]
      [arrowT (arg result) (run-interp p)]            
      [varT (is) (run-interp p)]
      [listofT (elem) (run-interp p)]
      )))

(define (run-interp [e : ExprC]) : s-expression
  (type-case Value (interp e mt-env)
    [numV (n) (number->s-exp n)]
    [closV (arg bod env) `function]
    [listV (elems) `list]
    [boolV (b) (boolean->s-exp b)]))

(module+ test 

  (test (run-prog '1)
        '1)
  
  (test (run-prog `true)
        '#t)
  (test (run-prog `false)
        '#f)
  
  (test (run-prog `empty)
        `list)
  
  (test (run-prog '{cons 1 empty})
        `list)
  (test (run-prog '{cons empty empty})
        `list)
  (test/exn (run-prog '{cons 1 {cons empty empty}})
            "no type")
  
  (test/exn (run-prog '{first 1})
            "no type")
  (test/exn (run-prog '{rest 1})
            "no type")
  
  (test/exn (run-prog '{first empty})
            "list is empty")
  (test/exn (run-prog '{rest empty})
            "list is empty")
  
  (test (run-prog '{let {[f : ?
                            {lambda {[x : ?]} {first x}}]}
                     {+ {f {cons 1 empty}} 3}})
        '4)
  (test (run-prog '{let {[f : ?
                            {lambda {[x : ?]}
                              {lambda {[y : ?]}
                                {cons x y}}}]}
                     {first {rest {{f 1} {cons 2 empty}}}}})
        '2)
  
  (test/exn (run-prog '{lambda {[x : ?]}
                         {cons x x}})
            "no type")
  
  (test/exn (run-prog '{let {[f : ? {lambda {[x : ?]} x}]}
                         {cons {f 1} {f empty}}})
            "no type")
  (test/exn (run-prog '{lambda {[x : ?]}
                         {cons x 1}})
            "no type")
  
  (test (run-prog '{{lambda {[x : ?]} x} 2})
            '2)
  
  (test (run-prog '{let {[f : ?
                            {lambda {[x : ?]} {first x}}]}
                     {f {cons 1 empty}}})
        '1)
  (test (run-prog '{let {[f : ?
                            {lambda {[x : ?]} {first x}}]}
                     {f {cons 1 empty}}})
        '1)
  
  (test (run-prog '{lambda {[x : ?]}
                         {cons 1 x}})
            `function)  

  (test (run-prog '{let {[f : ?
                            {lambda {[x : ?]} {cons 1 x}}]}
                     {f {cons 2 empty}}})
        `list)
  
  (test (run-prog '{cons 1 {cons 2 empty}} )
        `list)
  
  (test (run-prog '{first {cons 1 empty}})
        '1)
  
  (test (run-prog '{first {cons 1 empty}})
        '1)
   (test (run-prog '{+ 1 {first {cons 1 empty}}})
        '2)
  (test (run-prog '1)
        '1)
  
  (test (run-prog `empty)
        `list)
  
  (test (run-prog '{cons 1 empty})
        `list)
  (test (run-prog '{cons empty empty})
        `list)
  (test/exn (run-prog '{cons 1 {cons empty empty}})
            "no type")
  
  (test/exn (run-prog '{first 1})
            "no type")
  (test/exn (run-prog '{rest 1})
            "no type")
  
  (test/exn (run-prog '{first empty})
            "list is empty")
  (test/exn (run-prog '{rest empty})
            "list is empty")
  
  (test (run-prog '{let {[f : ?
                            {lambda {[x : ?]} {first x}}]}
                     {+ {f {cons 1 empty}} 3}})
        '4)
  (test (run-prog '{let {[f : ?
                            {lambda {[x : ?]}
                              {lambda {[y : ?]}
                                {cons x y}}}]}
                     {first {rest {{f 1} {cons 2 empty}}}}})
        '2)
  
  (test/exn (run-prog '{lambda {[x : ?]}
                         {cons x x}})
            "no type")
  
  (test/exn (run-prog '{let {[f : ? {lambda {[x : ?]} x}]}
                         {cons {f 1} {f empty}}})
            "no type")

;;_____________________________________________  
  (define a-type-var (varT (box (none))))
  (define an-expr (numC 0))
  
  (test (unify! (numT) (numT) an-expr)
        (values))
  (test (unify! (boolT) (boolT) an-expr)
        (values))
  (test (unify! (arrowT (numT) (boolT)) (arrowT (numT) (boolT)) an-expr)
        (values))
  (test (unify! (varT (box (some (boolT)))) (boolT) an-expr)
        (values))
  (test (unify! (boolT) (varT (box (some (boolT)))) an-expr)
        (values))
  (test (unify! a-type-var a-type-var an-expr)
        (values))
  (test (unify! a-type-var (varT (box (some a-type-var))) an-expr)
        (values))
  
  (test (let ([t (varT (box (none)))])
          (begin
            (unify! t (boolT) an-expr)
            (unify! t (boolT) an-expr)))
        (values))
  
  (test/exn (unify! (numT) (boolT) an-expr)
            "no type")
  (test/exn (unify! (numT) (arrowT (numT) (boolT)) an-expr)
            "no type")
  (test/exn (unify! (arrowT (numT) (numT)) (arrowT (numT) (boolT)) an-expr)
            "no type")
  (test/exn (let ([t (varT (box (none)))])
              (begin
                (unify! t (boolT) an-expr)
                (unify! t (numT) an-expr)))
            "no type")
  (test/exn (unify! a-type-var (arrowT a-type-var (boolT)) an-expr)
            "no type")
  (test/exn (unify! a-type-var (arrowT (boolT) a-type-var) an-expr)
            "no type")
  
  (test (resolve a-type-var)
        a-type-var)
  (test (resolve (varT (box (some (numT)))))
        (numT))
  
  (test (occurs? a-type-var a-type-var)
        #t)
  (test (occurs? a-type-var (varT (box (none))))
        #f)
  (test (occurs? a-type-var (varT (box (some a-type-var))))
        #t)
  (test (occurs? a-type-var (numT))
        #f)
  (test (occurs? a-type-var (boolT))
        #f)
  (test (occurs? a-type-var (arrowT a-type-var (numT)))
        #t)
  (test (occurs? a-type-var (arrowT (numT) a-type-var))
        #t))