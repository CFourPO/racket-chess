#lang racket
  
(require racket/gui lens 2htdp/universe)
(struct/lens game [white black turn state] #:transparent)
(struct/lens piece [type loc moves has-moved?] #:transparent)

;; constants

(define board-size 8)
(define square-size 60)
(define board-width (* square-size board-size))

(define board%
  (class object%
    (super-new)
    
    (define diagonal-moves '((1 1) (1 -1) (-1 1) (-1 -1))) ;; TODO: use Chris's posn library
    (define straight-moves '((1 0) (0 1) (-1 0) (0 -1)))
    (define knight-moves '((2 1) (2 -1) (-2 1) (-2 -1) (1 2) (1 -2) (-1 2) (-1 -2)))
  
    ;; sets all POTENTIALLY possible moves for a piece
    (define (get-moves type-of-piece)
      (cond [(eq? type-of-piece 'king) (append diagonal-moves straight-moves)]))

    (define white-king (piece 'king '(4 7) (get-moves 'king) #f))
    (define black-king (piece 'king '(4 0) (get-moves 'king) #f))

    (define starting-game (game (list black-king) (list white-king) 'white empty))

    ;; board render constants
   
    (define transparent (make-object color% 0 0 0 0))
    (define light-square (make-object color% 200 200 200))
    (define dark-square (make-object color% 50 50 50))
    (define hover-color (make-object color% 150 150 255 0.35))
    (define press-color (make-object color% 150 150 255 0.7))
    

    ;; board render helpers
    (define (scalev v) (* square-size v))
    (define (get-x position) (modulo position board-size))
    (define (get-y position) (inexact->exact (floor (/ position board-size))))
    (define (get-xy position) (cons (get-x position) (get-y position)))

    (define (xy->position x y)
      (cons
       (inexact->exact (floor (/ x square-size)))
       (inexact->exact (floor (/ y square-size)))))

    (define (decide-square-color position)
      (if (equal? (modulo (+ (car position) (cdr position)) 2) 0) light-square dark-square))

    ;; draws a square on a dc
    (define (draw-square dc position color)
      (send dc set-pen (make-object pen% transparent 0))
      (send dc set-brush (make-object brush% color 'solid))
      (send dc draw-rectangle (scalev (car position)) (scalev (cdr position)) square-size square-size))

    ;; highlights a square and returns the new bitmap
    (define (highlight-square board-bmdc position color)
      (draw-square board-bmdc position color)
      (send board-bmdc get-bitmap))

    ;; creates a bitmap of an empty board 
    (define (draw-board)
      (define (draw-squares counter board-bmdc)
        (cond [(equal? counter (sqr board-size)) (send board-bmdc get-bitmap)]
              [else
               (let ([position (get-xy counter)])
                 (draw-square board-bmdc position (decide-square-color position))
                 (draw-squares (add1 counter) board-bmdc))]))
      (draw-squares 0 (make-object bitmap-dc% (make-object bitmap% board-width board-width))))

    (define empty-bm (draw-board))
    (define current-bm empty-bm)

    ;; creates a new bmdc context from the empty board so that we can draw on the board
    (define (get-board-bmdc)
      (define new-bmdc (make-object bitmap-dc% (make-object bitmap% board-width board-width)))
      (send new-bmdc draw-bitmap empty-bm 0 0)
      new-bmdc)
  
    (define/public (handle-mouse mouse-event)
      (let* ([event-type (send mouse-event get-event-type)]
             [mouse-x (send mouse-event get-x)]
             [mouse-y (send mouse-event get-y)]
             [position (xy->position mouse-x mouse-y)])
        (cond
          [(equal? event-type 'leave) (set! current-bm empty-bm)]
          [(equal? event-type 'left-down) (set! current-bm (highlight-square (get-board-bmdc) position press-color))]
          [(or (equal? event-type 'motion) (equal? event-type 'left-up)) (set! current-bm (highlight-square (get-board-bmdc) position hover-color))])))

    ;; the canvas calls this to update it's bitmap
    (define/public (get-bitmap) current-bm)))

(define current-board (new board%)) ;; this could be passed in as an init parameter to board-canvas...

;; the canvas with on overridden event function... really these two classes could probably just be combined or organized better
(define board-canvas%
  (class canvas%
    (inherit get-width get-height refresh)
    
    (define/override (on-event event)
      (if (is-a? event mouse-event%) (send current-board handle-mouse event) (println "not a mouse event"))
      (refresh))

    (define/private (my-paint-callback self dc)
      (let ([bm (send current-board get-bitmap)])
        (send dc draw-bitmap bm 0 0)
        (send canvas min-width (send bm get-width))
        (send canvas min-height (send bm get-height))))
    (super-new [paint-callback (λ (c dc) (my-paint-callback c dc))])))

;; creates a window with the canvas then shows it (we could wrap this in a function...) 
(define frame (new frame%
                   [label "Chess"]
                   [min-width board-width]
                   [min-height board-width]))
(define canvas (new board-canvas% [parent frame]))
(send frame fullscreen #f)
(send frame show #t)
