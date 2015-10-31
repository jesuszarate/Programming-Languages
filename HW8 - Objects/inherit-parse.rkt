#lang plai-typed
(require plai-typed/s-exp-match
         "class.rkt"
         "inherit.rkt")

(module+ test
  (print-only-errors true))


;; ----------------------------------------

(define (parse-class [s : s-expression]) : ClassI
  (cond
   [(s-exp-match? `{class SYMBOL extends SYMBOL {ANY ...} ANY ...} s)
    (classI (s-exp->symbol (second (s-exp->list s)))
            (s-exp->symbol (fourth (s-exp->list s)))
            (map parse-field
                 (s-exp->list (fourth (rest (s-exp->list s)))))
            (map parse-method 
                 (rest (rest (rest (rest (rest (s-exp->list s))))))))]
   [else (error 'parse-class "invalid input")]))

(define (parse-field [s : s-expression]) : symbol
  (cond
   [(s-exp-match? `SYMBOL s)
    (s-exp->symbol s)]
   [else (error 'parse-field "invalid input")]))

(define (parse-method [s : s-expression]) : MethodI
  (cond
   [(s-exp-match? `{SYMBOL ANY} s)
    (methodI (s-exp->symbol (first (s-exp->list s)))
             (parse (second (s-exp->list s))))]
   [else (error 'parse-method "invalid input")]))

(define (parse [s : s-expression]) : ExprI
  (cond
   [(s-exp-match? `NUMBER s) (numI (s-exp->number s))]
   [(s-exp-match? `arg s) (argI)]
   [(s-exp-match? `this s) (thisI)]
   [(s-exp-match? '{+ ANY ANY} s)
    (plusI (parse (second (s-exp->list s)))
           (parse (third (s-exp->list s))))]
   [(s-exp-match? '{* ANY ANY} s)
    (multI (parse (second (s-exp->list s)))
           (parse (third (s-exp->list s))))]
   [(s-exp-match? '{new SYMBOL ANY ...} s)
    (newI (s-exp->symbol (second (s-exp->list s)))
          (map parse (rest (rest (s-exp->list s)))))]
   [(s-exp-match? '{get ANY SYMBOL} s)
    (getI (parse (second (s-exp->list s)))
          (s-exp->symbol (third (s-exp->list s))))]
   [(s-exp-match? '{send ANY SYMBOL ANY} s)
    (sendI (parse (second (s-exp->list s)))
           (s-exp->symbol (third (s-exp->list s)))
           (parse (fourth (s-exp->list s))))]
   [(s-exp-match? '{super SYMBOL ANY} s)
    (superI (s-exp->symbol (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
   
   [(s-exp-match? '{select ANY ANY} s)
    (selectI (parse (second (s-exp->list s)))
             (parse (third (s-exp->list s))))]
   [(s-exp-match? '{instanceof ANY SYMBOL} s)
    (instanceofI (parse (second (s-exp->list s)))
                 (s-exp->symbol (third (s-exp->list s))))]
   [else (error 'parse "invalid input")]))

(module+ test  
  (test (parse '0)
        (numI 0))
  (test (parse `arg)
        (argI))
  (test (parse `this)
        (thisI))
  (test (parse '{+ 1 2})
        (plusI (numI 1) (numI 2)))
  (test (parse '{* 1 2})
        (multI (numI 1) (numI 2)))
  (test (parse '{new posn 1 2})
        (newI 'posn (list (numI 1) (numI 2))))
  (test (parse '{get 1 x})
        (getI (numI 1) 'x))
  (test (parse '{send 1 m 2})
        (sendI (numI 1) 'm (numI 2)))
  (test (parse '{super m 1})
        (superI 'm (numI 1)))
  (test/exn (parse `x)
            "invalid input")

  (test (parse-field `x)
        'x)
  (test/exn (parse-field '{x 1})
            "invalid input")

  (test (parse-method `{m this})
        (methodI 'm (thisI)))
  (test/exn (parse-method `{m 1 2})
            "invalid input")
  
  (test (parse-class '{class posn3D extends posn
                             {x y z}
                             {m1 arg}
                             {m2 this}})
        (classI 'posn3D 'posn
                (list 'x 'y 'z)
                (list (methodI 'm1 (argI))
                      (methodI 'm2 (thisI)))))
  (test/exn (parse-class '{class})
            "invalid input"))

;; ----------------------------------------

(define (interp-prog [classes : (listof s-expression)] [a : s-expression]) : s-expression
  (let ([v (interp-i (parse a)
                     (map parse-class classes))])
    (type-case Value v
      [numV (n) (number->s-exp n)]
      [objV (class-name field-vals) `object])))






(module+ test

  (test (interp-prog (list)
                     '{new object})
        `object)
;;___________________________________________
  ;1
  (test (interp-prog (list '{class fish extends object
                                   {size color}})
                     '{instanceof {new fish 1 2} fish})
        '0)
  ;2 
  (test (interp-prog (list '{class fish extends object
                                   {size color}})
                     '{instanceof {new fish 1 2} fish})
        '0)
  ;3
  (test (interp-prog (list '{class fish extends object
                                   {size color}})
                     '{instanceof {new fish 1 2} object})
        '0)
  ;4
  (test/exn (interp-prog empty            
                         '{instanceof {+ 1 1} object})
            "not an object")
  ;5
  (test (interp-prog (list '{class fish extends object
                                   {size color}}
                           '{class shark extends fish
                                   {teeth}})
                     '{instanceof {new shark 1 2 3} fish})
        '0)
  ;6
  (test (interp-prog (list '{class fish extends object
                                   {size color}}
                           '{class mamal extends object
                                   {legs}}
                           '{class shark extends fish
                                   {teeth}})                           
                     '{instanceof {new shark 1 2 3} fish})
        '0)
   
  ;; Part 5 — More Extra Credit: Objects as Numbers ----------------------------------------------
#|
  (test (interp-prog (list
                      '{class zero extends object
                              {}
                              {plus arg}
                              {mult this}
                              {select {send arg zero 0}}})
                     '{+ 7 {* 8 {new zero}}})
        '7)
  (test (interp-prog (list
                      '{class infinity extends object
                              {}
                              {plus this}
                              {mult this}
                              {select {send arg nonzero 0}}})
                     '{+ 7 {new infinity}})
        `object)
  (test (interp-prog (list
                      '{class infinity extends object
                              {}
                              {plus this}
                              {mult this}
                              {select {send arg nonzero 0}}}
                      '{class snowball extends object
                                   {size}
                                   {zero this}
                                   {nonzero {new snowball {+ 1 {get this size}}}}})
                     '{get {select {new infinity} {new snowball 3}} size})
        '4)
  
  ;; Part 4 — Extra Credit: Numbers as Objects ----------------------------------------------

  (test (interp-prog (list)
                     '{instanceof 8 object})
        '0)
  (test (interp-prog (list)
                     '{send 8 plus 9})
        '17)
  (test (interp-prog (list)
                     '{send 8 mult 9})
        '72)
  (test (interp-prog (list '{class snowball extends object
                              {size}
                              {zero this}
                              {nonzero {new snowball {+ 1 {get this size}}}}})
                     '{get {send 8 select {new snowball 10}} size})
        '11)
  
|#
  ;;----------------------------------------------
  
  ;;----------------------------------------------
  (test (interp-prog (list '{class snowball extends object
                              {size}
                              {zero this}
                              {nonzero {new snowball {+ 1 {get this size}}}}})
                     '{get {select 0 {new snowball 1}} size})
        '1)
  (test (interp-prog (list '{class snowball extends object
                                   {size}
                                   {zero this}
                                   {nonzero {new snowball {+ 1 {get this size}}}}})
                     '{get {select {+ 1 2} {new snowball 1}} size})
        '2)
  ;;----------------------------------------------
  (test (interp-prog
         (list
          '{class empty extends object
                  {}})
         '{new empty})
        `object)

 (test (interp-prog 
        (list
         '{class posn extends object
                 {x y}
                 {mdist {+ {get this x} {get this y}}}
                 {addDist {+ {send arg mdist 0}
                             {send this mdist 0}}}}
         
         '{class posn3D extends posn
                 {z}
                 {mdist {+ {get this z} 
                           {super mdist arg}}}})
        
        '{send {new posn3D 5 3 1} addDist {new posn 2 7}})
       '18))
