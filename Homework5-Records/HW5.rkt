#lang plai-typed
(require plai-typed/s-exp-match)

(define-type-alias Location number)

(define-type Value
  [numV (n : number)]
  [errorV (s : string)]
  [closV (arg : symbol)
         (body : ExprC)
         (env : Env)]
  [recV (d : ExprC)
        (ns : (listof symbol))
        (vs : (listof Location))
        (env : Env)
        (sto : Store)]
  )

(define-type ExprC
  [numC (n : number)]
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
  [appC (fun : ExprC)
        (arg : ExprC)]
  [setC (var : symbol)
        (val : ExprC)]
  [set2C (n : ExprC)
         (var : symbol)
         (val : ExprC)]
  [beginC (l : ExprC)
          (r : ExprC)]
  
  ;;Start Record___________________
  [recordC (d : ExprC)
           (ns : (listof symbol))
           (vs : (listof ExprC))]           
  ;;End Record_____________________
  
  ;;Start Record/handle___________________  
  [errorC (err : string)]
  ;;End Record/handle_____________________
  
  [getC (rec : ExprC)
        (n : symbol)]
  )


(define-type Binding
  [bind (name : symbol)
        (location : Location)])

(define-type-alias Env (listof Binding))

(define mt-env empty)
(define extend-env cons)

(define-type Storage
  [cell (location : Location) 
        (val : Value)])

(define-type-alias Store (listof Storage))
(define mt-store empty)
(define override-store cons)

(define-type Result
  [v*s (v : Value) (s : Store)])

(define-type Result-list-sto
  [l*s (l : (listof Value)) (s : Store)])

(define-type Result-loclist-sto
  [loc*s (l : (listof Location)) (s : Store)])

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
       (letC (s-exp->symbol (first bs))
             (parse (second bs))
             (parse (third (s-exp->list s)))))]
    [(s-exp-match? '{lambda {SYMBOL} ANY} s)
     (lamC (s-exp->symbol (first (s-exp->list 
                                  (second (s-exp->list s)))))
           (parse (third (s-exp->list s))))]
    
    
    ;;Start Record ___________________________________________________________
    [(s-exp-match? '{record {SYMBOL ANY} ...} s)     
     (recordC (errorC "no such field")
              (map (lambda (l) (s-exp->symbol (first (s-exp->list l))));
                   (rest (s-exp->list s)))
              (map (lambda (l) (parse (second (s-exp->list l))))
                   (rest (s-exp->list s))))]
    [(s-exp-match? '{get ANY SYMBOL} s)
     (getC (parse (second (s-exp->list s)))
           (s-exp->symbol (third (s-exp->list s))))]
    ;;End Record ______________________________________________________________
    
    
    ;;Start Record/Handle ___________________________________________________________
    
    [(s-exp-match? '{record/handle ANY {SYMBOL ANY} ...} s)
     (recordC (parse (second (s-exp->list s)))
              (map (lambda (l)                            
                     (s-exp->symbol (first (s-exp->list l))))                            
                   (rest (rest (s-exp->list s))))
              (map (lambda (l)
                     (parse (second (s-exp->list l))))
                   (rest (rest (s-exp->list s)))))]
    
    [(s-exp-match? '{error STRING} s)
     (errorC (s-exp->string (second (s-exp->list s))))]
    
    ;;Start Record/Handle ___________________________________________________________
    
    
    [(s-exp-match? '{set! SYMBOL ANY} s)
     (setC (s-exp->symbol (second (s-exp->list s)))
           (parse (third (s-exp->list s))))]
    
    
    ;;Start Set ___________________________________________________________
    [(s-exp-match? '{set ANY SYMBOL ANY} s)
     (set2C (parse (second (s-exp->list s)))
            (s-exp->symbol (third (s-exp->list s)))
            (parse (fourth (s-exp->list s))))]
    ;;End Set _____________________________________________________________   
    
    
    
    [(s-exp-match? '{begin ANY ANY} s)
     (beginC (parse (second (s-exp->list s)))
             (parse (third (s-exp->list s))))]
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
  (test (parse '{set! b 0})
        (setC 'b (numC 0)))
  (test (parse '{begin 1 2})
        (beginC (numC 1) (numC 2)))
  (test/exn (parse '{{+ 1 2}})
            "invalid input"))

;; with form ----------------------------------------
(define-syntax-rule
  (with [(v-id sto-id) call]
        body)
  (type-case Result call
    [v*s (v-id sto-id) body]))

;; interp ----------------------------------------
(define (interp [a : ExprC] [env : Env] [sto : Store]) : Result
  (type-case ExprC a
    [numC (n) (v*s (numV n) sto)]
    [idC (s) (v*s (fetch (lookup s env) sto)
                  sto)]
    [plusC (l r)
           (with [(v-l sto-l) (interp l env sto)]
                 (with [(v-r sto-r) (interp r env sto-l)]
                       (v*s (num+ v-l v-r) sto-r)))]
    [multC (l r)
           (with [(v-l sto-l) (interp l env sto)]
                 (with [(v-r sto-r) (interp r env sto-l)]
                       (v*s (num* v-l v-r) sto-r)))]
    [letC (n rhs body)
          (with [(v-rhs sto-rhs) (interp rhs env sto)]
                (let ([l (new-loc sto-rhs)])
                  (interp body
                          (extend-env (bind n l)
                                      env)
                          (override-store (cell l v-rhs)
                                          sto-rhs))))]
    [lamC (n body)
          (v*s (closV n body env) sto)]
    
    ;;Start Record -------------------------------------------------------
    [recordC (d ns vs)
             (type-case Result-loclist-sto (create-vs-locations vs env sto)
               [loc*s (ll ss)                   
                      (with [(v-d s-d) (interp d env sto)]
                            (let ([n-sto (append sto ss)])
                              (v*s (recV d ns ll env n-sto) n-sto)))])]    
    
    [getC (a n)
          (with [(v-a sto-a) (interp a env sto)]
                (type-case Value v-a
                  [recV (d ns vs n-env r-sto) 
                            (with [(v-d sto-d) (interp d n-env sto-a)] ; Check if the d exists first before we interpret.                                  
                                  (v*s (find-w-error v-d n ns vs r-sto) sto-d))]
                  [else (error 'interp "no such field")]))]         
    ;;End Record -------------------------------------------------------
    
    
    ;;Start Record/Handle _______________________________________________________    
    [errorC (e) (v*s (errorV e) sto)]
    ;;End Record/Handle _________________________________________________________
    
    [appC (fun arg)
          (with [(v-f sto-f) (interp fun env sto)]
                (with [(v-a sto-a) (interp arg env sto-f)]
                      (type-case Value v-f
                        [closV (n body c-env)
                               (let ([l (new-loc sto-a)])
                                 (interp body
                                         (extend-env (bind n l)
                                                     c-env)
                                         (override-store (cell l v-a)
                                                         sto-a)))]
                        [else (error 'interp "not a function")])))]
    [setC (var val)
          (let ([l (lookup var env)])
            (with [(v-v sto-v) (interp val env sto)]
                  (v*s v-v
                       (override-store (cell l v-v)
                                       sto-v))))]
    ;;Start set___________________________________________________
    
    
    [set2C (n var val)                      
           (with [(v-n sto-n) (interp n env sto)]
                 (let ([l (get-location v-n sto-n)])
                   (type-case Value v-n                     
                     [recV (d ns ls env-n r-sto)                           
                           (with [(v-val sto-val) (interp val env-n sto-n)]
                                 (let ([rec-sto (replace-rec var v-val ns ls env-n r-sto)])
                                   (let ([newc (cell l (recV d ns ls env-n rec-sto))])                                     
                                     (v*s v-val (override-store newc sto-val)))))]
                     [else  (error 'interp "not a record")])))]
    
    
    ;;End set_____________________________________________________
    
    [beginC (l r)
            (with [(v-l sto-l) (interp l env sto)]
                  (interp r env sto-l))]))

(define (interp-expr (a : ExprC)) : s-expression 
  (with [(v-l sto-l) (interp a mt-env mt-store)]
        (type-case Value v-l
          [numV (n) (number->s-exp n)]
          [closV (arg bod env) `function]
          [recV (d ns vs env sto) `record]          
          [errorV (e) (error 'interp-expr e)])))

(define (get-location [e : Value] [sto : Store]) : Location
  (cond
    [(empty? sto) (error 'get-location "free expression")]
    [else (cond
            [(eq? e (cell-val (first sto)))
             (cell-location (first sto))]
            [else (get-location e (rest sto))])]))  

(module+ test

  ;(test (interp-expr (parse '{quote {let {{z 1}}
   ;                                   {begin {record {x {set! z 2}}} z}}}))
    ;    '2)
  

  (test/exn (get-location (numV 1) mt-env) "free expression")
  (test/exn (interp-expr (parse '{get 5 x})) "no such field")
  (test/exn (interp-expr (parse '{let {[r 1]} {set r x 5}})) "not a record")
  ;; Start Tests for Record/handle ________________________________________
  ;#|
  (test (interp-expr (parse '{let {[r {record/handle 5 {x 1}}]}
                               {get r x}}))
        '1)
  (test/exn (interp-expr (parse '{let {[r {record {x 1}}]}
                               {+ r 1}}))
        "not a number")

  (test/exn (interp-expr (parse '{let {[r {record {x 1}}]}
                               {r 1}}))
        "not a function")
  
  (test (interp-expr (parse '{let {[r {record/handle 5 {x 1}}]}
                               {get r y}}))
        '5)
  
  (test/exn (interp-expr (parse '{let {[r {record/handle {error "ouch"} {x 1}}]}
                                   {get r y}}))
            "ouch")
  
  (test (interp-expr (parse '{let {[r {record/handle {error "ouch"} {x 1}}]}
                               {get r x}}))
        '1)
  ;|#
  ;; End Tests for Record/handle ________________________________________
  
  ;; Start Tests for Mutating Records ________________________________________
  ;#|
  (test (interp-expr (parse '{let {[r {record {x 1}}]}
                               {get r x}}))
        '1)
  
  (test (interp-expr (parse '{let {[r {record {x 1}}]}
                               {begin
                                 {set r x 5}                                 
                                 {get r x}}}))
        '5)
  
  (test (interp-expr (parse '{let {[r1 {record {x 1}}]}
                               {let {[r2 r1]}
                                 {begin
                                   {set r1 x 2}
                                   {get r2 x}}}}))
        '2)
  
  (test (interp-expr (parse '{let {[r {record {x 13} {y 22}}]}                               
                               {begin                               
                                 {set r x 5}
                                 {begin
                                   {set r x 13}
                                   {get r x}}}}))
        '13)
  (test (interp-expr (parse '{let {[r {record {x 1}}]}
                               {get r x}}))
        '1)
  
  (test (interp-expr (parse '{let {[r {record {x 1}}]}
                               {begin
                                 {set r x 5}
                                 {get r x}}}))
        '5)
  
  (test (interp-expr (parse '{let {[g {lambda {r} {get r a}}]}
                               {let {[s {lambda {r} {lambda {v} {set r b v}}}]}
                                 {let {[r1 {record {a 0} {b 2}}]}
                                   {let {[r2 {record {a 3} {b 4}}]}
                                     {+ {get r1 b}
                                        {begin
                                          {{s r1} {g r2}}
                                          {+ {begin
                                               {{s r2} {g r1}}
                                               {get r1 b}}
                                             {get r2 b}}}}}}}}))
        '5)
  
  ;; Start Tests for Mutating Records ________________________________________
  
  ;; Start Tests for Records with Store ______________________________________
  (test (interp-expr (parse '{let {[x 0]}
                               {let {[r {record {a x}}]}
                                 {begin
                                   {set! x 1}
                                   {get r a}}}}))
        '0)
  
  (test (interp-expr (parse '{+ 1 4}))
        '5)
  
  (test (interp-expr (parse '{record {a 10} {b {+ 1 2}}}))
        `record)

  (test (interp-expr (parse '{lambda {a} {+ 1 2}})) `function)
  
  (test (interp-expr (parse '{get {record {a 10} {b {+ 1 0}}} b}))
        '1)
  
  
  (test/exn (interp-expr (parse '{get {record {a 10}} b}))
            "no such field")
  (test (interp-expr (parse '{get {record {r {record {z 0}}}} r}))
        `record)
  (test (interp-expr (parse '{get {get {record {r {record {z 0}}}} r} z}))
        '0)
  ;; End Tests for Records with Store _______________________________________
  
  
  (test (interp (parse '2) mt-env mt-store)
        (v*s (numV 2) 
             mt-store))
  (test/exn (interp (parse `x) mt-env mt-store)
            "free variable")
  (test (interp (parse `x) 
                (extend-env (bind 'x 1) mt-env)
                (override-store (cell 1 (numV 9))
                                mt-store))
        (v*s (numV 9)
             (override-store (cell 1 (numV 9))
                             mt-store)))
  (test (interp (parse '{+ 2 1}) mt-env mt-store)
        (v*s (numV 3)
             mt-store))
  (test (interp (parse '{* 2 1}) mt-env mt-store)
        (v*s (numV 2)
             mt-store))
  (test (interp (parse '{+ {* 2 3} {+ 5 8}})
                mt-env
                mt-store)
        (v*s (numV 19)
             mt-store))
  (test (interp (parse '{lambda {x} {+ x x}})
                mt-env
                mt-store)
        (v*s (closV 'x (plusC (idC 'x) (idC 'x)) mt-env)
             mt-store))
  (test (interp (parse '{let {[x 5]}
                          {+ x x}})
                mt-env
                mt-store)
        (v*s (numV 10)
             (override-store (cell 1 (numV 5))
                             mt-store)))
  (test (interp (parse '{let {[x 5]}
                          {let {[x {+ 1 x}]}
                            {+ x x}}})
                mt-env
                mt-store)
        (v*s (numV 12)
             (override-store (cell 2 (numV 6))
                             (override-store (cell 1 (numV 5))
                                             mt-store))))
  (test (interp (parse '{let {[x 5]}
                          {let {[y 6]}
                            x}})
                mt-env
                mt-store)
        (v*s (numV 5)
             (override-store (cell 2 (numV 6))
                             (override-store (cell 1 (numV 5))
                                             mt-store))))
  (test (interp (parse '{{lambda {x} {+ x x}} 8})
                mt-env
                mt-store)
        (v*s (numV 16)
             (override-store (cell 1 (numV 8))
                             mt-store)))
  (test (interp (parse '{begin 1 2})
                mt-env
                mt-store)
        (v*s (numV 2)
             mt-store))
  (test (interp (parse '{let {[x 5]}
                          {begin
                            {set! x 6}
                            x}})
                mt-env
                mt-store)
        (v*s (numV 6)
             (override-store (cell 1 (numV 6))
                             (override-store (cell 1 (numV 5))
                                             mt-store))))
  
  (test/exn (interp (parse '{1 2}) mt-env mt-store)
            "not a function")
  (test/exn (interp (parse '{+ 1 {lambda {x} x}}) mt-env mt-store)
            "not a number")
  (test/exn (interp (parse '{let {[bad {lambda {x} {+ x y}}]}
                              {let {[y 5]}
                                {bad 2}}})
                    mt-env
                    mt-store)
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
(define (lookup [n : symbol] [env : Env]) : Location
  (cond
    [(empty? env) (error 'lookup "free variable")]
    [else (cond
            [(symbol=? n (bind-name (first env)))
             (bind-location (first env))]
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

;; store operations ----------------------------------------

(define (new-loc [sto : Store]) : Location
  (+ 1 (max-address sto)))

(define (max-address [sto : Store]) : Location
  (cond
    [(empty? sto) 0]
    [else (max (cell-location (first sto))
               (max-address (rest sto)))]))

(define (fetch [l : Location] [sto : Store]) : Value
  (cond
    [(empty? sto) (error 'interp "unallocated location")]
    [else (if (equal? l (cell-location (first sto)))
              (cell-val (first sto))
              (fetch l (rest sto)))]))

(module+ test
  (test (max-address mt-store)
        0)
  (test (max-address (override-store (cell 2 (numV 9))
                                     mt-store))
        2)
  
  (test (fetch 2 (override-store (cell 2 (numV 9))
                                 mt-store))
        (numV 9))
  (test (fetch 2 (override-store (cell 2 (numV 10))
                                 (override-store (cell 2 (numV 9))
                                                 mt-store)))
        (numV 10))
  (test (fetch 3 (override-store (cell 2 (numV 10))
                                 (override-store (cell 3 (numV 9))
                                                 mt-store)))
        (numV 9))
  (test/exn (fetch 2 mt-store)
            "unallocated location"))
;; find & update ----------------------------------------

;; Takes a name and two parallel lists, returning an item from the
;; second list where the name matches the item from the first list.
(define (find [n : symbol] [ns : (listof symbol)] [vs : (listof Value)])
  : Value
  (cond
    [(empty? ns) (error 'interp "no such field")]
    [else (if (symbol=? n (first ns))
              (first vs)
              (find n (rest ns) (rest vs)))]))

(define (find-w-error [err : Value][n : symbol] [ns : (listof symbol)] [vs : (listof Location)] [sto : Store])
  : Value
  (cond
    [(empty? ns) err]                                 
    [else (if (symbol=? n (first ns))
              (fetch (first vs) sto)
              (find-w-error err n (rest ns) (rest vs) sto))])  
  )




;; Takes a name n, value v, and two parallel lists, returning a list
;; like the second of the given lists, but with v in place
;; where n matches the item from the first list.
(define (update [n : symbol] [v : Value]
                [ns : (listof symbol)] [vs : (listof Value)])
  : (listof Value)
  (cond
    [(empty? ns) (error 'interp "no such field")]
    [else (if (symbol=? n (first ns))
              (cons v (rest vs))
              (cons (first vs) 
                    (update n v (rest ns) (rest vs))))]))

(module+ test
  (test (find 'a (list 'a 'b) (list (numV 1) (numV 2)))
        (numV 1))
  (test (find 'b (list 'a 'b) (list (numV 1) (numV 2)))
        (numV 2))
  (test/exn (find 'a empty empty)
            "no such field")
  
  (test (update 'a (numV 0) (list 'a 'b) (list (numV 1) (numV 2)))
        (list (numV 0) (numV 2)))
  (test (update 'b (numV 0) (list 'a 'b) (list (numV 1) (numV 2)))
        (list (numV 1) (numV 0)))
  (test/exn (update 'a (numV 0) empty empty)
            "no such field"))

(define (get-cell-location [s : symbol] [ns : (listof symbol)] [ls : (listof Location)]) : Location
  (cond
    [(empty? ns) (error 'get-cell-location "no such field")]
    [else (cond
            [(equal? s (first ns))
             (first ls)]
            [else (get-cell-location s (rest ns) (rest ls))])]))

(define (remove-cell [l : Location] [sto : Store]) : (listof Storage)
  (cond
    [(empty? sto) (error 'remove-cell "unknown location")]
    [else (if (equal? l (cell-location (first sto)))
              (rest sto)
              (cons (first sto) (remove-cell l (rest sto))))])
  )

(module+ test
  (test/exn (get-cell-location 't (list 'r 's) (list 1 2)) "no such field")
  (test (remove-cell 2 (override-store (cell 1 (numV 9))
                                       (override-store (cell 2 (numV 5))
                                                       (override-store (cell 3 (numV 6)) mt-env))))
        (override-store (cell 1 (numV 9))                                       
                        (override-store (cell 3 (numV 6)) mt-env)))
  (test (remove-cell 1 (override-store (cell 1 (numV 9))
                                       (override-store (cell 2 (numV 5))
                                                       (override-store (cell 3 (numV 6)) mt-env))))
        (override-store (cell 2 (numV 5))                                       
                        (override-store (cell 3 (numV 6)) mt-env)))
  (test/exn (remove-cell 5 (override-store (cell 1 (numV 9))
                                           (override-store (cell 2 (numV 5))
                                                           (override-store (cell 3 (numV 6)) mt-env))))
            "unknown location")
  )

; don't need l so far or d
(define (replace-rec [n : symbol] [val : Value] [ns : (listof symbol)] [ls : (listof Location)] [env : Env] [sto : Store]) : Store
  (let ([l (get-cell-location n ns ls)])
    (let ([new-sto (remove-cell l sto)])
      (override-store (cell l val) new-sto))))

(define (remove-val [var : symbol] [ns : (listof symbol)] [vs : (listof Value)]) : (listof Value) 
  (cond
    [(empty? ns) empty ]
    [else (if (equal? var (first ns))
              (rest vs)
              (cons (first vs) (remove-val var (rest ns) (rest vs))))])
  )

(define (remove-var [var : symbol] [ns : (listof symbol)]) : (listof symbol) 
  (cond
    [(empty? ns) empty ]
    [else (if (equal? var (first ns))
              (rest ns)
              (cons (first ns) (remove-var var (rest ns))))])
  )

(define (create-vs-locations [vs : (listof ExprC)] [env : Env] [sto : Store]) : Result-loclist-sto
  (let ([vals (map (lambda (x)
                     (with [(v-x s-x) (interp x env sto)]
                           v-x
                           )) vs)])                               
    (let ([n-sto (create-store vals sto)])
      (loc*s (map (lambda (x) (cell-location x)) n-sto) n-sto)))
  )

(define (create-store [vs : (listof Value)] [sto : Store]) : Store
  (cond
    [(empty? vs) mt-store]
    [else
     (let ([l (new-loc sto)])
       (let ([newStore (override-store (cell l (first vs)) sto)])
         (override-store (cell l (first vs)) (create-store (rest vs) newStore))))])
  )


(module+ test
  (test (create-store (list (numV 1) (numV 2)) mt-store)
        (override-store (cell 1 (numV 1)) (override-store (cell 2 (numV 2)) mt-store)))
  
  (test (remove-val 'c (list 'a 'b 'c 'd) (list (numV 1) (numV 2) (numV 3) (numV 4)))
        (list (numV 1) (numV 2) (numV 4)))
  (test (remove-val 'z (list 'a 'b 'c 'd) (list (numV 1) (numV 2) (numV 3) (numV 4)))
        (list (numV 1) (numV 2) (numV 3)(numV 4)))
  
  (test (remove-var 'c (list 'a 'b 'c 'd))
        (list 'a 'b 'd))
  (test (remove-var 'z (list 'a 'b 'c 'd))
        (list 'a 'b 'c 'd))
  )


















