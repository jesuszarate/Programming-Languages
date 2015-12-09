#lang plai-typed
(require plai-typed/s-exp-match)

;; Garbage collection for run-time memory

(define-type ExprC
  [numC (n : number)]
  [plusC (lhs : ExprC)
        (rhs : ExprC)]
  [multC (lhs : ExprC)
        (rhs : ExprC)]
  [idC (name : symbol)]
  [lamC (n : symbol)
        (body : ExprC)]
  [appC (fun-expr : ExprC)
        (arg-expr : ExprC)]
  [if0C (tst : ExprC)
        (thn : ExprC)
        (els : ExprC)]
  [boxC (arg : ExprC)]
  [unboxC (arg : ExprC)]
  )

#|
    (define-type ExprD
  8   [numD (n : number)]
  9   [plusD (lhs : ExprD)
             (rhs : ExprD)]
  10  [multD (lhs : ExprD)
             (rhs : ExprD)]
  11  [atD (pos : number)]
  12  [lamD (body : ExprD)]
  13  [appD (fun-expr : ExprD)
            (arg-expr : ExprD)]
  14  [if0D (tst : ExprD)
            (thn : ExprD)
            (els : ExprD)])
  102 [boxD (arg : ExprD)])
  202 [unboxD (arg : ExprD)])
|#

#|
    (define-type Value
  15  [numV (n : number)]
  16  [closV (body : ExprD)
             (env : Env)])
  103 [boxV (body : Location)]

|#

;; Location
(define-type-alias Location number)

;; Storage
;(define v-reg 0)
(define-type Storage
  [cell (location : Location) (val : number)])

(define-type-alias Store (listof Storage))
(define mt-store empty)
(define override-store cons)

(define mt-env empty)
(define extend-env cons)

(define-type BindingC
  [bindC (name : symbol)])

(define-type-alias EnvC (listof BindingC))

#|
    (define-type Cont
  0   [doneK]
  1   [addSecondK (r : ExprD)
                  (env : Env)
                  (k : Cont)]
  2   [doAddK (v1 : Value)
              (k : Cont)]
  3   [multSecondK (r : ExprD)
                   (env : Env)
                   (k : Cont)]
  4   [doMultK (v1 : Value)
               (k : Cont)]
  5   [appArgK (arg-expr : ExprD)
               (env : Env)
               (k : Cont)]
  6   [doAppK (fun-val : Value)
              (k : Cont)]
  7   [doIf0K (then-expr : ExprD)
              (else-expr : ExprD)
              (env : Env)
              (k : Cont)])
  100 [boxK               
              (k : Cont)])

  200 [unboxK              
              (k : Cont)])

|#

#|
  17 cons for env
  18 moved
|#

(module+ test
  (print-only-errors true))

;; ----------------------------------------
;; Allocator for code, which is never freed;
;; use `code-ref` instead of `ref` to refer to code

(define code-memory (make-vector 2048 0))
(define code-ptr 0)

(define (code-incptr n)
  (begin
    (set! code-ptr (+ code-ptr n))
    (- code-ptr n)))

(define (code-malloc1 tag a)
  (begin
    (vector-set! code-memory code-ptr tag)
    (vector-set! code-memory (+ code-ptr 1) a)
    (code-incptr 2)))

(define (code-malloc2 tag a b)
  (begin
    (vector-set! code-memory code-ptr tag)
    (vector-set! code-memory (+ code-ptr 1) a)
    (vector-set! code-memory (+ code-ptr 2) b)
    (code-incptr 3)))

(define (code-malloc3 tag a b c)
  (begin
    (vector-set! code-memory code-ptr tag)
    (vector-set! code-memory (+ code-ptr 1) a)
    (vector-set! code-memory (+ code-ptr 2) b)
    (vector-set! code-memory (+ code-ptr 3) c)
    (code-incptr 4)))

(define (code-ref n d)
  (vector-ref code-memory (+ n d)))

;; ----------------------------------------

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
   [(s-exp-match? '{lambda {SYMBOL} ANY} s)
    (lamC (s-exp->symbol (first (s-exp->list 
                                 (second (s-exp->list s)))))
          (parse (third (s-exp->list s))))]
   [(s-exp-match? '{box ANY} s)
     (boxC (parse (second (s-exp->list s))))]
   [(s-exp-match? '{unbox ANY} s)
     (unboxC (parse (second (s-exp->list s))))]
   [(s-exp-match? '{ANY ANY} s)
    (appC (parse (first (s-exp->list s)))
          (parse (second (s-exp->list s))))]
   [(s-exp-match? '{if0 ANY ANY ANY} s)
    (if0C (parse (second (s-exp->list s)))
          (parse (third (s-exp->list s)))
          (parse (fourth (s-exp->list s))))]   
   [else (error 'parse "invalid input")]))

(module+ test
  (test (parse '3) (numC 3))
  (test (parse `x) (idC 'x))
  (test (parse '{+ 1 2}) (plusC (numC 1) (numC 2)))
  (test (parse '{* 1 2}) (multC (numC 1) (numC 2)))
  (test (parse '{lambda {x} x}) (lamC 'x (idC 'x)))
  (test (parse '{1 2}) (appC (numC 1) (numC 2)))
  (test (parse '{if0 0 1 2}) (if0C (numC 0) (numC 1) (numC 2)))
  (test/exn (parse '{}) "invalid input"))

;; ----------------------------------------

(define (compile a-fae env)
  (type-case ExprC a-fae
    [numC (n) (code-malloc1 8 n)]
    [plusC (l r) (code-malloc2 9 (compile l env) (compile r env))]
    [multC (l r) (code-malloc2 10 (compile l env) (compile r env))]
    [idC (name) (code-malloc1 11 (locate name env))]
    [lamC (n body-expr) 
          (code-malloc1 12 (compile body-expr (extend-env
                                               (bindC n)
                                               env)))]
    [appC (fun-expr arg-expr)
          (code-malloc2 13 
                        (compile fun-expr env)
                        (compile arg-expr env))]
    [if0C (test-expr then-expr else-expr)
          (code-malloc3 14
                        (compile test-expr env)
                        (compile then-expr env)
                        (compile else-expr env))]
    
    ;; Boxes
    [boxC (arg) (code-malloc1 102 (compile arg env))]

    [unboxC (arg) (code-malloc1 202 (compile arg env))]
  ))

(define (locate name env)
  (cond
   [(empty? env) (error 'locate "free variable")]
   [else (if (symbol=? name (bindC-name (first env)))
             0
             (+ 1 (locate name (rest env))))]))

;; ----------------------------------------
;; Memory allocator with a 2-space collector
;;  for run-time allocation

(define MEMORY-SIZE 128)

(define space1 (make-vector MEMORY-SIZE 0))
(define space2 (make-vector MEMORY-SIZE 0))

(define memory space1)
(define ptr 0)

(define empty-memory (make-vector 0 0))
(define from-memory empty-memory)

(define result-reg 0)

(define (incptr n)
  ;; Increment the allocation pointer, and
  ;;  if there's not enough room for the next
  ;;  allocation, then collect garbage
  (begin
    (set! ptr (+ ptr n))
    (if (>= (+ ptr 5) MEMORY-SIZE)
        (begin
          (set! result-reg (- ptr n))
          (if (eq? from-memory empty-memory)
              (gc)
              ;; Ran out of space while GCing
              ;;  => GCing didn't reclaim anything,
              ;;     so we're really out of space
              (error 'malloc "out of memory")))
        (- ptr n))))

(define (malloc1 tag a)
  (begin
    (vector-set! memory ptr tag)
    (vector-set! memory (+ ptr 1) a)
    (incptr 2)))

(define (malloc2 tag a b)
  (begin
    (vector-set! memory ptr tag)
    (vector-set! memory (+ ptr 1) a)
    (vector-set! memory (+ ptr 2) b)
    (incptr 3)))

(define (malloc3 tag a b c)
  (begin
    (vector-set! memory ptr tag)
    (vector-set! memory (+ ptr 1) a)
    (vector-set! memory (+ ptr 2) b)
    (vector-set! memory (+ ptr 3) c)
    (incptr 4)))

(define (malloc4 tag a b c d)
  (begin
    (vector-set! memory ptr tag)
    (vector-set! memory (+ ptr 1) a)
    (vector-set! memory (+ ptr 2) b)
    (vector-set! memory (+ ptr 3) c)
    (vector-set! memory (+ ptr 4) d)
    (incptr 5)))

(define (ref n d)
  (vector-ref memory (+ n d)))


;; Pointer in to space; objects before the
;;  pointer are "black", and object after are "gray"
(define updated-ptr 0)

(define (gc)
  (begin
    (display "GCing\n")
    ;; Swap to and from space:
    (set! from-memory memory)
    (if (eq? memory space1)
        (set! memory space2)
        (set! memory space1))
    (set! ptr 0)
    ;; Update registers to start:
    (set! v-reg (move v-reg))
    (set! env-reg (move env-reg))
    (set! k-reg (move k-reg))
    (set! result-reg (move result-reg))
    (set! updated-ptr 0)
    ;; Loop until there are no gray objects:
    (update)))

(define (update)
  (if (= updated-ptr ptr)
      ;; No more gray objects:
      (begin
        (set! from-memory empty-memory)
        result-reg)
      ;; updated-ptr points to first gray object:
      (case (ref updated-ptr 0)
        [(100 103 200)
         (begin
           (move! 1)           
           (done 1))]
        [(0 15)
         ;; Record has just an integer
         (done 1)]
        [(1 3 5)
         (begin
           ;; Record has two run-time pointers
           ;;  in slots 2 and 3 (and an integer in 1)
           (move! 2)
           (move! 3)
           (done 3))]
        [(2 4 6 17)
         (begin
           ;; Etc.
           (move! 1)
           (move! 2)
           (done 2))]
        [(16)
         (begin
           (move! 2)
           (done 2))]
        [(7)
         (begin
           (move! 3)
           (move! 4)
           (done 4))])))

(define (done n)
  (begin
    (set! updated-ptr (+ updated-ptr (+ n 1)))
    
    (update)))


;; move! : number -> void
;;  Updates pointer at updated-ptr+n, moving the
;;  target as necessary:
(define (move! n)
  (vector-set! memory (+ updated-ptr n)
               (move (vector-ref memory (+ updated-ptr n)))))

;; move : number -> number
;;  If n refers to a white record, copy it to to-space and
;;   insert a forwarding pointer, so now it's gray
;; If n refers to a gray/black record, return the forwarding
;;   pointer.
(define (move n)
  (if (= 18 (vector-ref from-memory n))
      ;; Gray/black:
      (vector-ref from-memory (+ n 1))
      ;; White:
      (begin
        (case (vector-ref from-memory n)
          [(0 15 103 100 200)
           ;; size 1
           (begin
             (malloc1 (vector-ref from-memory n)
                      (vector-ref from-memory (+ n 1)))
             (vector-set! from-memory (+ n 1) (- ptr 2)))]
          [(2 4 6 16 17)
           ;; size 2
           (begin
             (malloc2 (vector-ref from-memory n)
                      (vector-ref from-memory (+ n 1))
                      (vector-ref from-memory (+ n 2)))
             (vector-set! from-memory (+ n 1) (- ptr 3)))]
          [(1 3 5)
           ;; size 3
           (begin
             (malloc3 (vector-ref from-memory n)
                      (vector-ref from-memory (+ n 1))
                      (vector-ref from-memory (+ n 2))
                      (vector-ref from-memory (+ n 3)))
             (vector-set! from-memory (+ n 1) (- ptr 4)))]
          [(7)
           ;; size 4
           (begin
             (malloc4 (vector-ref from-memory n)
                      (vector-ref from-memory (+ n 1))
                      (vector-ref from-memory (+ n 2))
                      (vector-ref from-memory (+ n 3))
                      (vector-ref from-memory (+ n 4)))
             (vector-set! from-memory (+ n 1) (- ptr 5)))])
        ;; Change to gray:
        (vector-set! from-memory n 18)
        ;; Return forwarding porter (that we just installed):
        (vector-ref from-memory (+ n 1)))))

;; ----------------------------------------

(define expr-reg 0) ; ExprC
(define env-reg 0)  ; Env

;; interp : ExprC Env Cont -> Value
(define (interp)
  (case (code-ref expr-reg 0)
    [(8) ; num
     (begin
       (set! v-reg (malloc1 15 (code-ref expr-reg 1)))
       (continue))]
    [(9) ; add
     (begin
       (set! k-reg (malloc3 1
                            (code-ref expr-reg 2)
                            env-reg 
                            k-reg))
       (set! expr-reg (code-ref expr-reg 1))
       (interp))]
    [(10) ; mult
     (begin
       (set! k-reg (malloc3 3
                            (code-ref expr-reg 2)
                            env-reg k-reg))
       (set! expr-reg (code-ref expr-reg 1))
       (interp))]
    [(11) ; id
     (begin
       (set! env2-reg env-reg)
       (set! v-reg (code-ref expr-reg 1))
       (env-ref))]
    [(12) ; lam
     (begin
       (set! v-reg (malloc2 16 (code-ref expr-reg 1) env-reg))
       (continue))]
    [(13) ; app
     (begin
       (set! k-reg (malloc3 5
                            (code-ref expr-reg 2)
                            env-reg k-reg))
       (set! expr-reg (code-ref expr-reg 1))
       (interp))]
    [(14) ; if0
     (begin
       (set! k-reg (malloc4 7
                            (code-ref expr-reg 2)
                            (code-ref expr-reg 3)
                            env-reg k-reg))
       (set! expr-reg (code-ref expr-reg 1))
       (interp))]
    [(102) ; "box"
     (begin
       (set! k-reg (malloc1 100 k-reg))       
       (set! expr-reg (code-ref expr-reg 1))
       (interp))]
    
    [(202) ; "unbox"
     (begin       
       (set! k-reg (malloc1 200 k-reg))       
       (set! expr-reg (code-ref expr-reg 1))
       (interp))]))

(define k-reg 0) ; Cont
(define v-reg 0) ; Value

;; continue : Cont Value -> Value
(define (continue)
  (case (ref k-reg 0)
    [(0) ; mtk
     v-reg]
    [(1) ; addSecondK
     (begin
       (set! expr-reg (ref k-reg 1))
       (set! env-reg (ref k-reg 2))
       (set! k-reg (malloc2 2 v-reg (ref k-reg 3)))
       (interp))]
    [(2) ; doAddK
     (begin
       (set! v-reg (num+ (ref k-reg 1) v-reg))
       (set! k-reg (ref k-reg 2))
       (continue))]
    [(3) ; multSecondK
     (begin
       (set! expr-reg (ref k-reg 1))
       (set! env-reg (ref k-reg 2))
       (set! k-reg (malloc2 4 v-reg (ref k-reg 3)))
       (interp))]
    [(4) ; doMultK
     (begin
       (set! v-reg (num* (ref k-reg 1) v-reg))
       (set! k-reg (ref k-reg 2))
       (continue))]
    [(5) ; appArgK
     (begin
       (set! expr-reg (ref k-reg 1))
       (set! env-reg (ref k-reg 2))
       (set! k-reg (malloc2 6 v-reg (ref k-reg 3)))
       (interp))]
    [(6) ; doAppK
     (begin
       (set! expr-reg (ref (ref k-reg 1) 1))
       (set! env-reg (malloc2 17
                              v-reg
                              (ref (ref k-reg 1) 2)))
       (set! k-reg (ref k-reg 2))
       (interp))]
    [(7) ; doIfK
     (begin
       (if (numzero? v-reg)
           (set! expr-reg (ref k-reg 1))
           (set! expr-reg (ref k-reg 2)))
       (set! env-reg (ref k-reg 3))
       (set! k-reg (ref k-reg 4))
       (interp))]
    [(100) ; doboxK 
     (begin       
       (set! k-reg (ref k-reg 1))              
       (set! v-reg (malloc1 103 v-reg))
       (continue))]
    [(200) ; dounboxK
     (begin
       (set! k-reg (ref k-reg 1))
       (set! v-reg (ref v-reg 1))
       (continue))]))

;; TODO: make sure x and y are numbers not boxes
;; num-op : (number number -> number) -> (Value Value -> Value)
(define (num-op op)
  (lambda (x y)
    (malloc1 15 (op (ref x 1) (ref y 1)))))

(define num+ (num-op +))
(define num* (num-op *))

;; numzero? : ExprC-Value -> boolean
(define (numzero? x)
  (zero? (ref x 1)))

(define env2-reg 0)

(define (env-ref)
  (if (zero? v-reg)
      (begin
        (set! v-reg (ref env2-reg 1))
        (continue))
      (begin
        (set! env2-reg (ref env2-reg 2))
        (set! v-reg (- v-reg 1))
        (env-ref))))

;; ----------------------------------------

(define (init-k) (malloc1 0 0))
(define (interpx a env k)
  (begin
    (set! expr-reg a)
    (set! env-reg env)
    (set! k-reg k)
    (interp)))
(define (numV x) (malloc1 15 x))
(define empty-env (malloc1 0 0))

(define (vtest a b)
  (test (ref a 1) (ref b 1)))

(define (reset!)
  (begin
    (set! code-ptr 0)
    (set! ptr 0)
    (set! v-reg 0)
    (set! expr-reg 0)
    (set! k-reg 0)
    (set! env-reg 0)
    (set! result-reg 0)
    (set! from-memory empty-memory)
    (void)))

(module+ test
  (reset!)
  (vtest (interpx (compile
                   (parse '{unbox {unbox {box {box 3}}}})
                   mt-env)
                  empty-env
                  (init-k))
         (numV 3))  (reset!)
  
  (reset!)
  (vtest (interpx (compile
                   (parse
                    '{{lambda {mkrec}
                        {{{lambda {chain}
                            {lambda {unchain}
                              ;; Make a chain of boxes, then traverse
                              ;; them:
                              {{unchain 13} {chain 13}}}}
                          ;; Create recursive chain function:
                          {mkrec
                           {lambda {chain}
                             {lambda {n}
                               {if0 n
                                    1
                                    {box {chain {+ n -1}}}}}}}}
                         ;; Create recursive unchain function:
                         {mkrec
                          {lambda {unchain}
                            {lambda {n}
                              {lambda {b}
                                {if0 n
                                     b
                                     {unbox {{unchain {+ n -1}} b}}}}}}}}}
                      ;; mkrec:
                      {lambda {body-proc}
                        {{lambda {fX}
                           {fX fX}}
                         {lambda {fX}
                           {body-proc {lambda {x} {{fX fX} x}}}}}}})
                   mt-env)
                  empty-env
                  (init-k))
         (numV 1))
  (reset!)
  #|
  (vtest (interpx (compile (parse '{+ 99 77}) mt-env)
                  empty-env
                  (init-k))
         (numV 16))
  |#
  ;; unbox -------------------------------------------
  ;#|
  (vtest (interpx (compile (parse '{unbox {box 10}})
                           mt-env)
                  empty-env
                  (init-k))
         (numV 10))
  (reset!)
  ;|#
  ;; box -------------------------------------------           
  ;(vtest (interpx (compile (parse '{box 10})
   ;                        mt-env)
    ;              empty-env
     ;             (init-k))
   ;      (boxV 0))
  ;(reset!)
  ;;------------------------------------------------  
  (vtest (interpx (compile (parse '10) mt-env)
                  empty-env
                  (init-k))
         (numV 10))
  (reset!)
  (vtest (interpx (compile (parse '{+ 10 7}) mt-env)
                  empty-env
                  (init-k))
         (numV 17))
  (reset!)
  (vtest (interpx (compile (parse '{* 10 7}) mt-env)
                  empty-env
                  (init-k))
         (numV 70))
  (reset!)
  (vtest (interpx (compile
                   (parse '{{lambda {x} {+ x 12}}
                            {+ 1 17}})
                   mt-env)
                  empty-env
                  (init-k))
         (numV 30))
  (reset!)
  (vtest (interpx (compile (parse `x)
                           (extend-env (bindC 'x) mt-env))
                  (malloc2 17 (numV 10) empty-env)
                  (init-k))
         (numV 10))
  (reset!)
  (vtest (interpx (compile (parse `{{lambda {x} {+ x 12}}
                                    {+ 1 17}})
                           mt-env)
                  empty-env
                  (init-k))
         (numV 30))
  (reset!)
  (vtest (interpx (compile (parse '{{lambda {x}
                                      {{lambda {f}
                                         {+ {f 1}
                                            {{lambda {x}
                                               {f 2}}
                                             3}}}
                                       {lambda {y} {+ x y}}}}
                                    0})
                           mt-env)
                  empty-env
                  (init-k))
         (numV 3))
  (reset!)
  (vtest (interpx (compile (parse '{if0 0 1 2})
                           mt-env)
                  empty-env
                  (init-k))
         (numV 1))
  (reset!)
  (vtest (interpx (compile (parse '{if0 1 1 2})
                           mt-env)
                  empty-env
                  (init-k))
         (numV 2))
  (reset!)
  (vtest (interpx (compile
                   (parse 
                    '{{lambda {mkrec}
                        {{lambda {fib}
                           ;; Call fib on 4:
                           {fib 4}}
                         ;; Create recursive fib:
                         {mkrec
                          {lambda {fib}
                            ;; Fib:
                            {lambda {n}
                              {if0 n
                                   1
                                   {if0 {+ n -1}
                                        1
                                        {+ {fib {+ n -1}}
                                           {fib {+ n -2}}}}}}}}}}
                      ;; mkrec:
                      {lambda {body-proc}
                        {{lambda {fX}
                           {fX fX}}
                         {lambda {fX}
                           {body-proc {lambda {x} {{fX fX} x}}}}}}})
                   mt-env)
                  empty-env
                  (init-k))
         (numV 5))  
  (reset!)
  (test/exn (interpx (compile
                      (parse
                       '{{lambda {x} {{x x} x}}
                         {lambda {x} {{x x} x}}})
                      mt-env)
                     empty-env
                     (init-k))
            "out of memory")

  (test/exn (compile (parse `x) mt-env)
            "free variable"))