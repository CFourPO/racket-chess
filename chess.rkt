#lang racket
  
(require racket/gui lens "./posn.rkt")
(struct/lens game [pieces turn state] #:transparent)
(struct/lens piece [type color loc moves has-moved?] #:transparent)

;; constants

(define board-size 8)
(define square-size 60)
(define board-width (* square-size board-size))

(define board%
  (class object%
    (super-new)
    
    (define diagonal-moves (list (posn 1 1) (posn 1 -1) (posn -1 1) (posn -1 -1)))
    (define straight-moves (list (posn 1 0) (posn 0 1) (posn -1 0) (posn 0 -1)))
    (define knight-moves   (list (posn 2 1) (posn 2 -1) (posn -2 1) (posn -2 -1) (posn 1 2) (posn 1 -2) (posn -1 2) (posn -1 -2)))
  
    ;; sets all POTENTIALLY possible moves for a piece
    ;; (define (get-moves type-of-piece location)
    ;;   (cond [(eq? type-of-piece 'king) (append diagonal-moves straight-moves)]
    ;;         [(eq? type-of-piece 'rook ())

    (define white-king (piece 'king 'white (posn 4 7) empty #f))
    (define black-king (piece 'king 'black (posn 4 0) empty #f))
    (define white-rook (piece 'rook 'white (posn 0 7) empty #f))

    (define starting-game (game (list black-king white-king white-rook) 'white empty))
    (define current-game starting-game)

    ;; get piece at location or return false
    (define (get-piece position)
      (findf (λ (p) (posn=? (piece-loc p) position)) (game-pieces current-game)))

    (let (build-moves loc directions repeat) 

    ;; gets all the possible moves for a piece except the ones off the board (still includes moves passing through other pieces etc)
    (define (get-all-possible-moves p)
      (let ([type (piece-type p)]
            [loc (piece-loc p)])
        (filter (λ (pc) (posn/in-bounds? (piece-loc pc) 0 7 0 7 #true))
         (cond [(eq? 'king type) (append diagonal-moves straight-moves)]

    ;; board render constants
    (define transparent (make-object color% 0 0 0 0))
    (define light-square (make-object color% 190 190 190))
    (define dark-square (make-object color% 50 100 50))
    (define white-color (make-object color% "white"))
    (define black-color (make-object color% "black"))
    (define hover-color (make-object color% 150 150 255 0.35))
    (define press-color (make-object color% 150 150 255 0.7))

    ;; board render helpers
    (define (scalev v) (* square-size v))
    (define (get-x position) (modulo position board-size))
    (define (get-y position) (inexact->exact (floor (/ position board-size))))
    (define (get-xy position) (posn (get-x position) (get-y position)))

    (define (xy->position x y)
      (posn
       (inexact->exact (floor (/ x square-size)))
       (inexact->exact (floor (/ y square-size)))))

    (define (decide-square-color position)
      (if (equal? (modulo (+ (posn-x position) (posn-y position)) 2) 0) light-square dark-square))

    ;; draws a square on a dc
    (define (draw-square dc position color)
      (send dc set-pen (make-object pen% transparent 0))
      (send dc set-brush (make-object brush% color 'solid))
      (send dc draw-rectangle (scalev (posn-x position)) (scalev (posn-y position)) square-size square-size))

    ;; highlights a square and returns the new bitmap
    (define (highlight-square board-bmdc position color)
      (send board-bmdc set-pen (make-object pen% transparent 0))
      (draw-square board-bmdc position color))

    (define (write-text dc text position color)
      (send dc set-text-foreground color)
      (send dc draw-text text (scalev (posn-x position)) (scalev (posn-y position))))
      
    (define (draw-pieces pieces board-bdmc)
      (define (draw-piece p)
        (write-text
         board-bdmc
         (symbol->string (piece-type p))
         (piece-loc p)
         (if (eq? 'white (piece-color p)) white-color black-color)))
      (map draw-piece pieces))

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
    
    ;; creates a new bmdc context from the empty board so that we can draw on the board
    (define (get-board-bmdc)
      (define new-bmdc (make-object bitmap-dc% (make-object bitmap% board-width board-width)))
      (send new-bmdc draw-bitmap empty-bm 0 0)
      new-bmdc)

    ;; this is the final rendereded object, it gets update on mouse input, and rendeded by
    (define current-bm (let ([bmdc (get-board-bmdc)])
                         (draw-pieces (game-pieces current-game) bmdc)
                         (send bmdc get-bitmap)))

    ;; on mouse left-down handler eventually we will do some state logic here
    (define (press-square bmdc position)
      (highlight-square bmdc position press-color))

    (define (select-square bmdc position)
      (let ([piece (get-piece position)])
        (cond [piece (lens-set game-state-lens current-game (list 'selected piece))])))
        
    ;; this currently handles mouse left-up and motion handling, eventually this will be split
    (define (hover-square bmdc position)
      (highlight-square bmdc position hover-color))
      
    (define/public (handle-mouse mouse-event)
      (let* ([event-type (send mouse-event get-event-type)]
             [mouse-x (send mouse-event get-x)]
             [mouse-y (send mouse-event get-y)]
             [position (xy->position mouse-x mouse-y)]
             [bmdc (get-board-bmdc)])
        (cond
          ;[(equal? event-type 'leave) (basic-board bmdc)] ;; TODO eventually we will probably need to handle this, right now it's fine
          [(equal? event-type 'left-up) (select-square bmdc position)]
          [(equal? event-type 'left-down) (press-square bmdc position)]
          [(or (equal? event-type 'motion) (equal? event-type 'left-up)) (hover-square bmdc position)])
        (draw-pieces (game-pieces current-game) bmdc)
        (set! current-bm (send bmdc get-bitmap))))

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
