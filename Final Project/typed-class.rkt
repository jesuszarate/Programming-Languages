#lang plai-typed

(require "class.rkt"
         "inherit.rkt")

(define-type ClassT
  [classT (name : symbol)
          (super-name : symbol)
          (fields : (listof FieldT))
          (methods : (listof MethodT))])

(define-type FieldT
  [fieldT (name : symbol)
          (type : Type)])

(define-type MethodT
  [methodT (name : symbol)
           (arg-type : Type)
           (result-type : Type)
           (body-expr : ExprI)])

(define-type Type
  [numT]
  [nullT]
  [objT (class-name : symbol)])

(module+ test
  (print-only-errors true))

;; ----------------------------------------

(define find-classT
  (make-find classT-name))

(define find-fieldT
  (make-find fieldT-name))

(define find-methodT
  (make-find methodT-name))

(define (type-error a msg)
  (error 'typecheck (string-append
                     "no type: "
                     (string-append
                      (to-string a)
                      (string-append " not "
                                     msg)))))

(define (get-all-field-types class-name t-classes)
  (if (equal? class-name 'object)
      empty        
      (type-case ClassT (find-classT class-name t-classes)
        [classT (name super-name fields methods)
                (append 
                 (get-all-field-types super-name t-classes)
                 (map fieldT-type fields))])))

;; ----------------------------------------

(define (make-find-in-tree find-in-list extract)
  (lambda (name t-class t-classes)
    (local [(define items (extract t-class))
            (define super-name 
              (classT-super-name t-class))]
      (if (equal? super-name 'object)
          (find-in-list name items)
          (try (find-in-list name items)
               (lambda ()
                 ((make-find-in-tree find-in-list extract)
                  name 
                  (find-classT super-name t-classes)
                  t-classes)))))))

(define find-field-in-tree
  (make-find-in-tree find-fieldT classT-fields))

(define find-method-in-tree
  (make-find-in-tree find-methodT classT-methods))

;; ----------------------------------------

(define (is-subclass? name1 name2 t-classes)
  (cond
    [(equal? name1 name2) true]
    [(equal? name1 'object) false]
    [else
     (type-case ClassT (find-classT name1 t-classes)
       [classT (name super-name fields methods)
               (is-subclass? super-name name2 t-classes)])]))

(define (subclass name1 name2 t-classes)
  (cond
    [(equal? name1 name2) (objT name1)]
    [(equal? name1 'object) (objT name1)]
    [else
     (type-case ClassT (find-classT name1 t-classes)
       [classT (name super-name1 fields methods)
               (type-case ClassT (find-classT name2 t-classes)
                 [classT (name super-name2 fields methods)
                         (subclass super-name1 super-name2 t-classes)])])]))

(define (w-subclass t1 t2 t-classes)
  (type-case Type t1
    [objT (name1)
          (type-case Type t2
            [objT (name2)
                  (subclass name1 name2 t-classes)]
            [else (error 'w-subclass "Error line 107")])]
    [else (cond [(equal? t1 t2) t1])]))

(define (is-subtype? t1 t2 t-classes)
  (type-case Type t1
    [objT (name1)
          (type-case Type t2
            [objT (name2)
                  (is-subclass? name1 name2 t-classes)]
            [else false])]
    [else (equal? t1 t2)]))

;(define (super-subtype t1 t2 t-classes)
;  (type-case Type t1
;    [objT (name1)
;          (type-case Type t2
;            [objT (name2)
;                  (type-case Type t1
;                    [objT (name1) (is-subclass? name1 name2 t-classes)]
;                    [else (error 'super-subtype "Error 107")])]
;            [else false])]
;    [else (equal? t1 t2)]))

(module+ test
  (define a-t-class (classT 'a 'object empty empty))
  (define b-t-class (classT 'b 'a empty empty))
  (define c-t-class (classT 'c 'a empty empty))
  (define d-t-class (classT 'd 'c empty empty))
  (define e-t-class (classT 'e 'c empty empty))

  ;;if0 generic
  ;;subclass --------------------------------------------
  (test (subclass 'object 'object empty)
        (objT 'object))
  (test (subclass 'b 'c (list a-t-class b-t-class c-t-class))
        (objT 'a))
  (test (subclass 'c 'b (list a-t-class b-t-class c-t-class))
        (objT 'a))
  (test (subclass 'd 'e (list a-t-class
                              b-t-class
                              c-t-class
                              d-t-class
                              e-t-class))
        (objT 'c))
  #|
  (test (subclass 'c 'd (list a-t-class
                              b-t-class
                              c-t-class
                              d-t-class
                              e-t-class))
        (objT'c))
  |#
  ;;----------------------------------------------------
  (test (is-subclass? 'object 'object empty)
        true)
  (test (is-subclass? 'a 'b (list a-t-class b-t-class))
        false)
  (test (is-subclass? 'b 'a (list a-t-class b-t-class))
        true)

  (test (is-subtype? (numT) (numT) empty)
        true)
  (test (is-subtype? (numT) (objT 'object) empty)
        false)
  (test (is-subtype? (objT 'object) (numT) empty)
        false)
  (test (is-subtype? (objT 'a) (objT 'b) (list a-t-class b-t-class))
        false)
  (test (is-subtype? (objT 'b) (objT 'a) (list a-t-class b-t-class))
        true))

;; ----------------------------------------

(define typecheck-expr : (ExprI (listof ClassT) Type Type -> Type)
  (lambda (expr t-classes arg-type this-type)
    (local [(define (recur expr)
              (typecheck-expr expr t-classes arg-type this-type))
            (define (typecheck-nums l r)
              (type-case Type (recur l)
                [numT ()
                      (type-case Type (recur r)
                        [numT () (numT)]
                        [else (type-error r "num")])]
                [else (type-error l "num")]))]
      (type-case ExprI expr
        [numI (n) (numT)]
        [plusI (l r) (typecheck-nums l r)]
        [multI (l r) (typecheck-nums l r)]
        [argI ()
              (if (equal? arg-type (nullT))
                  (type-error arg-type "no null")
                  arg-type)]
        [thisI () 
               (if (equal? this-type (objT 'bad))
                   (type-error this-type "bad obj")
                   this-type)]
        [newI (class-name exprs)
              (local [(define arg-types (map recur exprs))
                      (define field-types
                        (get-all-field-types class-name t-classes))]
                (if (and (= (length arg-types) (length field-types))
                         (foldl (lambda (b r) (and r b))                                  
                                true
                                (map2 (lambda (t1 t2)
                                        (type-case Type t1 ; nulls
                                          [objT (class-name) (is-subtype? t1 t2 t-classes)]
                                          [numT () (is-subtype? t1 t2 t-classes)]
                                          [nullT ()
                                           (type-case Type t2
                                             [objT (class-name) #t]                                             
                                             [else (type-error t2 "num")])]
                                           ))
                                      arg-types
                                      field-types)))
                    (objT class-name)
                    (type-error expr "field type mismatch")))]
        [getI (obj-expr field-name)
              (type-case Type (recur obj-expr)
                [objT (class-name)
                      (local [(define t-class
                                (find-classT class-name t-classes))
                              (define field
                                (find-field-in-tree field-name
                                                    t-class
                                                    t-classes))]
                        (type-case FieldT field
                          [fieldT (name type) type]))]
                [else (type-error obj-expr "object")])]
        [sendI (obj-expr method-name arg-expr)
               (local [(define obj-type (recur obj-expr))
                       (define arg-type (recur arg-expr))]
                 (type-case Type obj-type
                   [objT (class-name)
                         (typecheck-send class-name method-name
                                         arg-expr arg-type
                                         t-classes)]
                   [else
                    (type-error obj-expr "object")]))]
        [superI (method-name arg-expr)
                (local [(define arg-type (recur arg-expr))
                        (define this-class
                          (find-classT (objT-class-name this-type)
                                       t-classes))]
                  (typecheck-send (classT-super-name this-class)
                                  method-name
                                  arg-expr arg-type
                                  t-classes))]
        ;; My implemetations------------------------------------
        ;*
        [instanceofI (obj-expr class-name)
                     (type-case Type (recur obj-expr)
                       [objT (c-name) (numT)]
                       [else (type-error obj-expr "object")])]
        ;***
        [if0I (tst thn els)
              (if (is-subtype? (recur tst) (numT) empty)
                  (local [(define t-thn (recur thn))
                          (define t-els (recur els))]
                    [cond
                      [(is-subtype? t-thn t-els t-classes) t-els]
                      [(is-subtype? t-els t-thn t-classes) t-thn]
                      [else (w-subclass t-thn t-els t-classes)]
                      ])                      
                  (type-error tst "num"))]
        ;*
        [nullI () (nullT)]
        ;**
        [castI (class-name obj-expr) 
               (local ([define obj-t (recur obj-expr)])
                 (if (is-subtype? (objT class-name) obj-t t-classes)
                     obj-t
                     (type-error obj-expr (to-string class-name))))]
        ))))
              
(define (typecheck-send [class-name : symbol]
                        [method-name : symbol]
                        [arg-expr : ExprI]
                        [arg-type : Type]
                        [t-classes : (listof ClassT)])
  (type-case MethodT (find-method-in-tree
                      method-name
                      (find-classT class-name t-classes)
                      t-classes)
    [methodT (name arg-type-m result-type body-expr)
             (if (is-subtype? arg-type arg-type-m t-classes)
                 result-type
                 (type-error arg-expr (to-string arg-type-m)))]))

(define (typecheck-method [method : MethodT]
                          [this-type : Type]
                          [t-classes : (listof ClassT)]) : ()
  (type-case MethodT method
    [methodT (name arg-type result-type body-expr)
             (if (is-subtype? (typecheck-expr body-expr t-classes
                                              arg-type this-type)
                              result-type
                              t-classes)
                 (values)
                 (type-error body-expr (to-string result-type)))]))

(define (check-override [method : MethodT]
                        [this-class : ClassT]
                        [t-classes : (listof ClassT)])
  (local [(define super-name 
            (classT-super-name this-class))
          (define super-method
            (try
             ;; Look for method in superclass:
             (find-method-in-tree (methodT-name method)
                                  (find-classT super-name t-classes)
                                  t-classes)
             ;; no such method in superclass:
             (lambda () method)))]
    (if (and (equal? (methodT-arg-type method)
                     (methodT-arg-type super-method))
             (equal? (methodT-result-type method)
                     (methodT-result-type super-method)))
        (values)
        (error 'typecheck (string-append
                           "bad override of "
                           (to-string (methodT-name method)))))))

(define (typecheck-class [t-class : ClassT] [t-classes : (listof ClassT)])
  (type-case ClassT t-class
    [classT (name super-name fields methods)
            (map (lambda (m)
                   (begin
                     (typecheck-method m (objT name) t-classes)
                     (check-override m t-class t-classes)))
                 methods)]))

(define (typecheck [a : ExprI] [t-classes : (listof ClassT)]) : Type
  (begin
    (map (lambda (t-class)
           (typecheck-class t-class t-classes))
         t-classes)    
    (typecheck-expr a t-classes (nullT) (objT 'bad))))
;(typecheck-expr a t-classes (numT) (objT 'bad))))
;    (local [(define return-type (typecheck-expr a t-classes (nullT) (objT 'bad)))]
;      (if (or (equal? return-type (nullT))
;              (equal? return-type (objT 'bad)))
;          (type-error a "no type")
;          return-type))))

;; ----------------------------------------

(module+ test
  (define posn-t-class
    (classT 'posn 'object
            (list (fieldT 'x (numT)) (fieldT 'y (numT)))
            (list (methodT 'mdist (numT) (numT) 
                           (plusI (getI (thisI) 'x) (getI (thisI) 'y)))
                  (methodT 'addDist (objT 'posn) (numT)
                           (plusI (sendI (thisI) 'mdist (numI 0))
                                  (sendI (argI) 'mdist (numI 0)))))))

  (define posn3D-t-class 
    (classT 'posn3D 'posn
            (list (fieldT 'z (numT)))
            (list (methodT 'mdist (numT) (numT)
                           (plusI (getI (thisI) 'z) 
                                  (superI 'mdist (argI)))))))
  (define posn3D-2-t-class 
    (classT 'posn3D-2 'posn
            (list (fieldT 'z (numT)))
            (list (methodT 'mdist (numT) (numT)
                           (plusI (getI (thisI) 'z) 
                                  (superI 'mdist (argI)))))))

  (define posn4D-t-class 
    (classT 'posn4D 'posn
            (list (fieldT 'z (numT)) (fieldT 'ntn (objT 'nothing)))
            (list (methodT 'mdist (numT) (numT)
                           (plusI (getI (thisI) 'z) 
                                  (superI 'mdist (argI)))))))
  (define posn4D-2-t-class
    (classT 'posn4D-2 'posn
            (list (fieldT 'z (numT)) (fieldT 'ntn (objT 'nothing)))
            (list (methodT 'mdist (numT) (numT)
                           (plusI (getI (thisI) 'z) 
                                  (superI 'mdist (argI)))))))

  (define square-t-class 
    (classT 'square 'object
            (list (fieldT 'topleft (objT 'posn)))
            (list)))
  
  (define nothing-t-class
    (classT 'nothing 'object
            (list (fieldT 'p (objT 'nothing)))
            empty))

  (define nothing2-t-class
    (classT 'nothing2 'object
            (list (fieldT 'x (numT)) (fieldT 'y (numT)))
            (list (methodT 'mdist (numT) (numT) 
                           (plusI (getI (nullI) 'x) (getI (thisI) 'y))))))

  ;(define nothing2D-t-class
   ; (classT 'nothing2D 'nothing
    ;        (list (fieldT 'p (objT 'posn)))
     ;       (list (methodT 'mdist (numT) (numT)
      ;                     (plusI (getI (thisI) 'z) 
       ;                           (superI 'mdist (argI)))))))

  (define (typecheck-nothing a)
    (typecheck a
               (list nothing2-t-class)))
  
  (define (typecheck-posn a)
    (typecheck a
               (list posn-t-class
                     posn3D-t-class posn4D-t-class
                     square-t-class nothing-t-class
                     posn3D-2-t-class posn4D-2-t-class)))

  (define nothing-null (nullI))
  (define nothing2 (newI 'nothing2 (list (numI 1))))  
  (define nothing (newI 'nothing empty))
  (define posn27 (newI 'posn (list (numI 2) (numI 7))))
  (define posn531 (newI 'posn3D (list (numI 5) (numI 3) (numI 1))))
  (define squarePosn (newI 'square (list posn27)))
  (define posn3D-2 (newI 'posn3D-2 (list (numI 5) (numI 3) (numI 1))))
  (define posn4D-2 (newI 'posn4D-2 (list (numI 5) (numI 3) (numI 1) (nullI))))
  
  ;;this & arg----------------------------------------------------
  (test/exn (typecheck (argI) empty)
      "no type")
  (test/exn (typecheck (thisI) empty)
      "no type")
  ;(test (typecheck-posn (newI 'posn (list (numI 1) (argI))))
  ;   (numT))
  ;(test (typecheck-posn (if0I (numI 0) (thisI) (numI 2)))
  ;      (numT))

  ;(test (typecheck-posn (sendI posn27 'mdist (getI (argI) 'x)))
   ;     (numT))

  ;;cast ----------------------------------------------------    
  (test (typecheck-posn (castI 'posn posn27))
        (objT 'posn))
  
  (test/exn (typecheck-posn (castI 'posn nothing))
        "no type")
    
  ;;null ----------------------------------------------------
  (test (typecheck (nullI) empty)
      (nullT))
  
  (test/exn (typecheck-posn (sendI (nullI) 'mdist (numI 0)))
        "no type")
  
  (test (typecheck-posn (getI posn27 'x))                       
        (numT))
  
  (test/exn (typecheck-posn (getI nothing-null 'x))                       
        "no type")
  
  (test/exn (typecheck-nothing (newI 'nothing2 (list (numI 2) (numI 1))))
        "no type")      
  
  (test (typecheck-posn (newI 'nothing (list (newI 'nothing (list (nullI))))))
        (objT 'nothing))

  (test (typecheck-posn (newI 'nothing (list nothing-null)))
        (objT 'nothing))
  
  (test/exn (typecheck-posn (newI 'posn (list (numI 2) (nullI))))
        "no type")
  
  (test (typecheck-posn (newI 'posn4D (list (numI 5) (numI 3) (numI 1) (nullI))))
        (objT 'posn4D))
  
  (test (typecheck-posn (newI 'posn4D (list (numI 5) (numI 3) (numI 1) (newI 'nothing (list (nullI))))))
        (objT 'posn4D))
  
  ; What should happen if one is not a subtype of the other
  ;;if0 ----------------------------------------------------
  (test (typecheck-posn (if0I (numI 0)
                              posn3D-2
                              posn4D-2))
        (objT 'posn))
  (test (typecheck-posn (if0I (numI 0)
                              posn4D-2
                              posn3D-2))
        (objT 'posn))
  (test (typecheck-posn (if0I (numI 0)
                              squarePosn
                              posn27))
        (objT 'object))
  (test (typecheck-posn (if0I (numI 0)
                              (newI 'posn (list (numI 2) (numI 7)))
                              (newI 'posn3D (list (numI 5) (numI 3) (numI 3)))))
        (objT 'posn))
  
  (test (typecheck-posn (if0I (numI 0)
                              (newI 'posn3D (list (numI 5) (numI 3) (numI 3)))
                              (newI 'posn (list (numI 2) (numI 7)))))
        (objT 'posn))  
  
  (test (typecheck (if0I (numI 0) (numI 1) (numI 2)) empty)
        (numT))
  
  
  (test/exn (typecheck (if0I (newI 'object empty) (numI 1) (numI 2)) empty)
        "no type")
  
  ;;instanceof ----------------------------------------------------
  
  (test (typecheck-posn (instanceofI posn27 'posn))
      (numT))
  
  (test/exn (typecheck-posn (instanceofI (numI 3) 'posn))
      "no type")
  
  ;;---------------------------------------------------------------
  
  (test (typecheck-posn (sendI posn27 'mdist (numI 0)))
        (numT))
  (test (typecheck-posn (sendI posn531 'mdist (numI 0)))
        (numT))  
  (test (typecheck-posn (sendI posn531 'addDist posn27))
        (numT))  
  (test (typecheck-posn (sendI posn27 'addDist posn531))
        (numT))

  (test (typecheck-posn (newI 'square (list (newI 'posn (list (numI 0) (numI 1))))))
        (objT 'square))
  (test (typecheck-posn (newI 'square (list (newI 'posn3D (list (numI 0) (numI 1) (numI 3))))))
        (objT 'square))
  
  (test (typecheck (multI (numI 1) (numI 2))
                   empty)
        (numT))

  (test/exn (typecheck-posn (sendI (numI 10) 'mdist (numI 0)))
            "no type")
  (test/exn (typecheck-posn (sendI posn27 'mdist posn27))
            "no type")
  (test/exn (typecheck (plusI (numI 1) (newI 'object empty))
                       empty)
            "no type")
  (test/exn (typecheck (plusI (newI 'object empty) (numI 1))
                       empty)
            "no type")
  (test/exn (typecheck (plusI (numI 1) (newI 'object (list (numI 1))))
                       empty)
            "no type")
  (test/exn (typecheck (getI (numI 1) 'x)
                       empty)
            "no type")
  (test/exn (typecheck (numI 10)
                       (list posn-t-class 
                             (classT 'other 'posn
                                     (list)
                                     (list (methodT 'mdist 
                                                    (objT 'object) (numT)
                                                    (numI 10))))))
            "bad override")
  (test/exn (typecheck-method (methodT 'm (numT) (objT 'object) (numI 0)) (objT 'object) empty)
            "no type")
  (test/exn (typecheck (numI 0)
                       (list square-t-class
                             (classT 'cube 'square
                                     empty
                                     (list
                                      (methodT 'm (numT) (numT)
                                               ;; No such method in superclass:
                                               (superI 'm (numI 0)))))))
            "not found"))

;; ----------------------------------------

(define strip-types : (ClassT -> ClassI)
  (lambda (t-class)
    (type-case ClassT t-class
      [classT (name super-name fields methods)
              (classI
               name 
               super-name
               (map fieldT-name fields)
               (map (lambda (m)
                      (type-case MethodT m
                        [methodT (name arg-type result-type body-expr)
                                 (methodI name body-expr)]))
                    methods))])))

(define interp-not-t : (ExprI (listof ClassT) -> Value)
  (lambda (a t-classes)       
      (interp-i a
              (map strip-types t-classes))))

(define interp-t : (ExprI (listof ClassT) -> Value)
  (lambda (a t-classes)
    (begin
      (typecheck a t-classes)
      (interp-i a
              (map strip-types t-classes)))))

(module+ test
  (define (interp-t-posn a)
    (interp-t a
              (list posn-t-class posn3D-t-class)))
  
  (test (interp-t-posn (sendI posn27 'mdist (numI 0)))
        (numV 9))  
  (test (interp-t-posn (sendI posn531 'mdist (numI 0)))
        (numV 9))
  (test (interp-t-posn (sendI posn531 'addDist posn27))
        (numV 18))
  (test (interp-t-posn (sendI posn27 'addDist posn531))
        (numV 18)))