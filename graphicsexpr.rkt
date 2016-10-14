#lang racket
;;Graphics Expression version 1.0 release
(module grexpr racket)
(require pict)
(require pict/shadow)
(provide H CENTER-H CENTER-V BGCOLOR P CENTER SHADOW V FIX-CIRCLE FIX-FILLED-CIRCLE)

(provide (except-out (all-from-out racket)
                     #%module-begin
                     ))
(provide (rename-out [module-begin #%module-begin]))

(define-syntax !config
  (syntax-rules ()
    [(_ (a b) ...) (begin (define a (make-parameter b)) ...
                          (provide a) ...)]))

(define-syntax define-container
  (syntax-rules ()
    [(_ (cname w h) expr)
  (define cname
  (lambda (e)
    (let* ([child (lift-text e)]
           [child-w (pict-width child)]
           [child-h (pict-height child)]
           [w (+ (distance-left) (distance-right) child-w)]
           [h (+ (distance-top) (distance-bottom) child-h)]
           )
      (pin-over expr
                (distance-left) (distance-top)
                child
                ))))]))


(define-syntax define-fix-container
  (syntax-rules ()
    [(_ (cname w h) expr)
     (define cname (lambda (w h e)
       (let* ([child (lift-text e)]
           [child-w (pict-width child)]
           [child-h (pict-height child)]
           [expect-w (+ (distance-left) (distance-right) child-w)]
           [expect-h (+ (distance-top) (distance-bottom) child-h)]
           )
         (if (and (<= expect-w w) (<= expect-h h))
          (pin-over expr
                (distance-left) (distance-top)
                child
                )
          (pin-over expr                      
                (distance-left) (distance-top)
                (scale-to-fit child (check-positive (- w (distance-left) (distance-right)))
                                  (check-positive (- h (distance-top) (distance-bottom)))
                ))))))]))

(define-syntax define-all
  (syntax-rules ()
    [(_ (cname cname2 w h) expr) (begin (define-fix-container (cname2 w h) expr)
                                 (define-container (cname w h) expr)
                                 (provide cname cname2)
                                 )]))

(!config (text-color "black") (text-style null)
         (text-size 14)(color "black")(border-width 2)
         (border-color "black")
         (distance-top 10)(distance-bottom 10)
         (distance-left 15)(distance-right 15) (padding 6)
         (draw-border? #t) (radius -0.25)
         
         )
(define-syntax !module-begin
  (syntax-rules (define define-syntax)
    [(_ (define x ...) y ...) (begin (define x ...)
                                     (!module-begin y ...))]
    [(_ (define-syntax x ...) y ...) (begin (define x ...)
                                     (!module-begin y ...))]
    [(_ x y ...) (begin (lift-text x)
                        (!module-begin y ...))]
    [(_) (void)]))
     

(define-syntax module-begin
  (syntax-rules ()
    [(_ a ...) (#%module-begin
                (!module-begin a ...))]))

(define-syntax-rule (check-positive expr)
  (let ([result expr])
    (if (<= result 0) (error "there is no enough space.")
        result)))
        

(define lift-text
  (lambda (shape)
    (match shape
      [(? string? x) (colorize  (text x (text-style) (text-size) 0) (text-color))]
      [else shape]
      )))

(define-syntax let*-child
  (syntax-rules (:)
    [(_ (e : child child-w child-h) ((a b) ...) body ...)
        (let* ([child (lift-text e)]
               [child-w (pict-width child)]
               [child-h (pict-height child)]
               (a b) ...)
          body ...)]))


;;;'containers

(define-all (RECT FIX-RECT w h)
 (rectangle w h #:border-color (border-color) #:border-width (border-width)))


(define-all (ROUNDED-RECT FIX-ROUNDED-RECT w h)
   (rounded-rectangle w h (radius) #:border-color (border-color) #:border-width (border-width)
                      ))

(define-all (CIRCLE FIX-CIRCLE1 w h)
  (circle (max w h) #:border-color (border-color) #:border-width (border-width)))

(define (FIX-CIRCLE w e)
  (FIX-CIRCLE1 w w e))

(define-all (FILLED-CIRCLE FIX-FILLED-CIRCLE1 w h)
  (filled-ellipse (max w h) (max w h) #:draw-border? (draw-border?) #:border-color (border-color)
                    #:border-width (border-width)
                    #:color (color)))

(define (FIX-FILLED-CIRCLE w e)
  (FIX-FILLED-CIRCLE1 w w e))

(define-all (FILLED-RECT FIX-FILLED-RECT w h)
  (filled-rectangle w h #:draw-border? (draw-border?) #:border-color (border-color)
                    #:border-width (border-width)
                    #:color (color)))

(define-all (FILLED-ROUNDED-RECT FIX-FILLED-ROUNDED-RECT w h)
  (filled-rounded-rectangle w h (radius) #:draw-border? (draw-border?) #:border-color (border-color)
                    #:border-width (border-width)
                    #:color (color)))


;;'aligners


(define V
  (lambda elist
    (if (null? elist) (blank 0)
        (let* ([childs (map lift-text elist)]
               [w (apply max (map pict-width childs))]
               [h (+ (apply + (map pict-height childs)) (* (padding) (- (length elist) 1)))]
               [helper h])
          (foldr (lambda (a b)
                   (set! helper (- helper (pict-height a)))
                   (begin0 (pin-over b 0 helper a)
                           (set! helper (- helper (padding)))))
                 (blank w h) childs)))))

(define H
  (lambda elist
    (if (null? elist) (blank 0)
        (let* ([childs (map lift-text elist)]
               [h (apply max (map pict-height childs))]
               [w (+ (apply + (map pict-width childs)) (* (padding) (- (length elist) 1)))]
               [helper w])
          (foldr (lambda (a b)
                   (set! helper (- helper (pict-width a)))
                   (begin0 (pin-over b helper 0 a)
                           (set! helper (- helper (padding)))))
                 (blank w h) childs)))))


(define CENTER-V
  (lambda (d e)
    (let*-child (e : child child-w child-h)
                ([expect-distance (/ (check-positive (- d child-h)) 2)]
                 [w child-w]
                 [h d])
                (pin-over (blank w h)
                          0 expect-distance
                          child))))

(define CENTER-H
  (lambda (d e)
    (let*-child (e : child child-w child-h)
                ([expect-distance (/ (check-positive (- d child-w)) 2)]
                 [w d]
                 [h child-h])
                (pin-over (blank w h)
                          expect-distance 0
                          child))))

(define CENTER
  (lambda (x e)
    (CENTER-H x (CENTER-V x e))))

;;High-Order Combinators

(define BGCOLOR
  (lambda (c e)
    (let*-child (e : child child-w child-h)
                ()
                (pin-over (filled-rectangle
                           child-w child-h #:draw-border? #f
                           #:color c
                           )
                          0 0 child))))

(define split-text
  (lambda (t1 a)
    (if (string? t1)
        (append (map (lambda (x) (list->string (list x)))
             (string->list t1)) a)
        (cons t1 a)
        )))

(define P
  (lambda (d . elist)
    (let loop ([before (blank 0)]
               [accumlator (blank 0)]
               [elist (foldr split-text '() elist)])
      (if (null? elist)
          (vl-append before accumlator)
        (let*-child ((car elist) : child _1 _2) ()
          (let ([accr (hbl-append accumlator child)])
          (if (> (pict-width accr) d)
              (if (> (pict-width child) d)
                  (check-positive -1)
                  (loop (vl-append before accr)
                        (blank 0)
                        (cdr elist)))
              (loop before
                    accr
                    (cdr elist)))))))))

(define SHADOW
  (lambda (sh radius p)
    (shadow p radius #:color (text-color) #:shadow-color sh)))

