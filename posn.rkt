(module posn racket
  (provide (struct-out posn)
           posn=?
           posn/plus
           posn-list/plus
           posn/minus
           posn/in-bounds?
           distance
           posn/rotate
           posn-list/rotate
           posn/identity)
  
  (require lens/applicable)
  
  ;; ...........................................................................................posns
  (struct/lens posn (x y) #:transparent)

  ;; (posn=? posn posn) -> boolean
  (define (posn=? p1 p2)
    (and (= (posn-x p1) (posn-x p2))
         (= (posn-y p1) (posn-y p2))))

  ;; (posn/plus posn posn) -> posn 
  (define (posn/plus p1 p2)
    (posn (+ (posn-x p1) (posn-x p2))
          (+ (posn-y p1) (posn-y p2))))

  ;; (posn-list/plus lop posn) -> lop
  (define (posn-list/plus lop p)
    (map (λ (a-posn) (posn/plus a-posn p)) lop))

  ;; (posn/minus posn posn) -> posn
  (define (posn/minus p1 p2)
    (posn (+ (posn-x p1) (* -1 (posn-x p2)))
          (+ (posn-y p1) (* -1 (posn-y p2)))))


  ;; (posn/in-bounds? posn number number number number boolean)
  (define (posn/in-bounds? p x-min x-max y-min y-max inclusive?)
    (define x (posn-x p)) 
    (define y (posn-y p))
    (if inclusive?
        (and (>= x x-min) (<= x x-max)
             (>= y y-min) (<= y y-max))
        (and (> x x-min) (< x x-max)
             (> y y-min) (< y y-max))))

  ;; (posn/rotate posn posn)
  (define (distance p1 p2)
    (define dx (- (posn-x p1) (posn-x p2)))
    (define dy (- (posn-y p1) (posn-y p2)))
    (sqrt (- (* dx dx) (* dy dy))))

  ;; (posn/rotate posn deg)
  (define (posn/rotate p deg)
    (posn (round (- (* (posn-x p) (cos (degrees->radians deg)))
                    (* (posn-y p) (sin (degrees->radians deg)))))
          (round (+ (* (posn-x p) (sin (degrees->radians deg)))
                    (* (posn-y p) (cos (degrees->radians deg)))))))

  ;; (posn-list/rotate list-of-posns degree)
  (define (posn-list/rotate lop deg)
    (map (λ (p) (posn/rotate p deg)) lop))

  ;; (posn/identity posn)
  (define (posn/identity p)
    (posn (* -1 (posn-x p))
          (* -1 (posn-y p))))
  )