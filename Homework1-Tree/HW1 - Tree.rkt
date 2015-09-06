#lang plai-typed

#|
(define (sum (t : Tree)) 
  (type-case Tree t
    [leaf (val) ... ]
    [node (val left right) ... ])
  )
|#

(define-type Tree
    [leaf (val : number)]
    [node (val : number)
          (left : Tree)
          (right : Tree)])

(define (sum (t : Tree)) : number
  (type-case Tree t
    [leaf (val) val ]
    [node (val left right) (+ val (+ (sum left) (sum right))) ])
  )
(module+ test
(print-only-errors true)
(test (sum (node 5 (leaf 6) (leaf 7))) 18)
(test (sum (node 50 (leaf 50) (leaf 700))) 800)
(test (sum (node -5 (leaf 0) (leaf 5))) 0)
(test (sum (leaf 5)) 5)
(test (sum (leaf -5)) -5)
(test (sum (leaf 0)) 0))


(define (negate (t : Tree)) : Tree
  (type-case Tree t
    [leaf (val) (leaf (- 0 val)) ]
    [node (val left right) (node (- 0 val) (negate left) (negate right)) ])
  )
(test (negate (node 5 (leaf 6) (leaf 7))) (node -5 (leaf -6) (leaf -7)))
(test  (node -5 (leaf -6) (leaf -7)) (negate (node 5 (leaf 6) (leaf 7))))
(test (negate (leaf 7)) (leaf -7))
(test (negate (leaf 0)) (leaf 0))

(define (contains? (t : Tree) (n : number)) : boolean
  (type-case Tree t
    [leaf (val) (= val n) ]
    [node (val left right) (cond
                             [(or (or (contains? left n) (contains? right n)) (contains? (leaf val) n)) true]
                             [else false]
                             )])
  )

(test (contains? (node 5 (leaf 6) (leaf 7)) 6) #t)
(test (contains? (node 5 (leaf 6) (leaf 7)) 8) #f)
(test (contains? (leaf 7) 8) #f)
(test (contains? (leaf 7) 7) #t)


(define (bigger-leaves? [t : Tree] [n : number]) : boolean
  (type-case Tree t
    [leaf (val) (> val n ) ]
    [node (val left right) (and (bigger-leaves? left (+ val n)) (bigger-leaves? right (+ val n)))  ])
  )

(define ( big-leaves? [t : Tree]) : boolean
 (bigger-leaves? t 0)
)

(test (big-leaves? (leaf 8)) #t)
(test (big-leaves? (leaf -8)) #f)
(test (big-leaves? (node 5 (leaf 6) (leaf 7))) #t)
(test (big-leaves? (node -1 (leaf 0) (leaf 1))) #t)
(test (big-leaves? (node 5 (node 2 (leaf 8) (leaf 9)) (leaf 7))) #t)
(test (big-leaves? (node 5 (node 2 (leaf 8) (leaf 6)) (leaf 7))) #f)
(test (big-leaves? (node 5 (leaf 0) (leaf 6))) #f)

(define (getRight (t : Tree)) : number
  (type-case Tree t
    [leaf (val) val ]
    [node (val left right) (getRight right) ])
  )

(define (is-sorted? (t : Tree) (previous : number)) : boolean
  (type-case Tree t
    [leaf (val)  (< val previous)]
    [node (val left right)
          (cond
           [(is-sorted? left val)
            (is-sorted? (leaf val) (getRight right))]
           [else  #f])])                       
  )


(test (is-sorted? (node 2 (leaf 1) (leaf 3)) 0) #t)
(test (is-sorted? (node 2 (leaf 1) (node 4 (leaf 3) (leaf 5))) 0) #t)


(define (sorted? (t : Tree)) : boolean
  (is-sorted? t 0)
)
(test (sorted? (node 2 (leaf 1) (leaf 3))) #t)
(test (sorted? (node 2 (leaf 1) (node 4 (leaf 3) (leaf 5)))) #t)
(test (sorted? (node 1 (leaf 2) (leaf 3))) #f)
(test (sorted? (node 1 (leaf 2) (node 3 (leaf 4) (leaf 5)))) #f)
