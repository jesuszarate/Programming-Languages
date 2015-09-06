#lang plai-typed


;; To the Third Power
(define (3rd-power x)
  (local[(define (squared n)
           (* n n))]
    (* (squared x) x)))

(test (3rd-power 3) 27)
(test (3rd-power 4) 64)
(test (3rd-power 5) 125)
(test (3rd-power 8) 512)
(test (3rd-power 10) 1000)
(test (3rd-power 17) 4913)

;; To the Fourth Power
(define (4th-power x)
  (local [(define (squared n)
            (* n n))]
    (* (squared x) (squared x))))

;; To the 8th Power
(define (8th-power x)
  (* (4th-power x) (4th-power x) )
  )
;; To the 16th Power
(define (16th-power x)
  (* (8th-power x) (8th-power x) ))

;; To the 32nd Power
(define (32nd-power x)
  (* (16th-power x) (16th-power x) ))
(test (32nd-power 2) 4294967296)

(define (10th-power x)
  (local [(define (squared n)
            (* n n))]
    (* (squared x) (8th-power x))))

;;Part 2 - To the 42nd Power
(define (42nd-power x)
  (* (32nd-power x) (10th-power x)))
(test (42nd-power 3) 109418989131512359209)
(test (42nd-power 4) 19342813113834066795298816)
(test (42nd-power 5) 227373675443232059478759765625)
(test (42nd-power 8) 85070591730234615865843651857942052864)
(test (42nd-power 10) 1000000000000000000000000000000000000000000)
(test (42nd-power 17) 4773695331839566234818968439734627784374274207965089)

;;Part 3 — plural
(define (plural [x : string]) : string
  (local [
          (define (reverseX n)
            (list->string (reverse (string->list n))))
          (define (firstLetter n)
            (first (string->list (reverseX n))))]
    (cond
      [(equal? x "") ""]
      [ (equal? (firstLetter x) #\y)
       (string-append (list->string(reverse(rest (string->list (reverseX x))))) "ies")]       
      [else
       (string-append x "s")
       ]
      ))
  )

(test (plural "cry") "cries")
(test (plural "activity") "activities")
(test (plural "accuracy") "accuracies")
(test (plural "blackberry") "blackberries")
(test (plural "curry") "curries")
(test (plural "Hello") "Hellos")
(test (plural "who") "whos")
(test (plural "kid") "kids")
(test (plural "computer") "computers")
(test (plural "") "")

;;Part 4 — energy-usage
(define-type Light
  [bulb (watts : number)
        (technology : symbol)]
  [candle (inches : number)])

(define (energy-usage [l : Light]) : number
  (type-case Light l
    [bulb (w t)
          (/ (* w 24) 1000)
          ]
    [candle (i) 0])            
  )

(test (energy-usage (bulb 100.0 'halogen)) 2.4)
(test (energy-usage (bulb 300.0 'halogen)) 7.2)
(test (energy-usage (bulb 400.0 'halogen)) 9.6)
(test (energy-usage (bulb 500.0 'halogen)) 12)
(test (energy-usage (bulb 0.0 'halogen)) 0.0)
(test (energy-usage (bulb -100.0 'halogen)) -2.4)
(test (energy-usage (bulb -1.0 'halogen)) -0.024)
(test (energy-usage (bulb 10000000000000000000000.0 'halogen)) 2.4e+20)
(test (energy-usage (candle 10.0)) 0)
(test (energy-usage (candle 20.0)) 0)
(test (energy-usage (candle 30.0)) 0)

;;Part 5 - use-for-one-hour

(define (use-for-one-hour [l : Light]) : Light
  (type-case Light l
    [bulb (w t)
          (bulb w t)
          ]        
    [candle (i)
            (cond [(< i 1)
                   (candle i)]
                  [else
                   (candle (- i 1))])
            ]            
    )
  )

(test (use-for-one-hour (bulb 100.0 'halogen)) (bulb 100.0 'halogen))
(test (use-for-one-hour (bulb 1.0 'halogen)) (bulb 1.0 'halogen))
(test (use-for-one-hour (bulb -100.0 'halogen)) (bulb -100.0 'halogen))
(test (use-for-one-hour (candle 10.0)) (candle 9.0 ))
(test (use-for-one-hour (candle 100.0)) (candle 99.0 ))
(test (use-for-one-hour (candle 9920.0)) (candle 9919.0 ))
(test (use-for-one-hour (candle 0.0)) (candle 0.0 ))
(test (use-for-one-hour (candle -1.0)) (candle -1.0 ))


