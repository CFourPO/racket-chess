#lang racket
  
(require racket/gui lens "./posn.rkt")
(struct/lens game [pieces turn state] #:transparent)
(struct/lens state [type data] #:transparent)
(struct/lens piece [type color position movement has-moved?] #:transparent)
(struct/lens movement [directions repeatable] #:transparent)

;; helper lenses
(define (piece-movement-directions p) (lens-view (lens-compose movement-directions-lens piece-movement-lens) p))
(define (piece-movement-repeatable p) (lens-view (lens-compose movement-repeatable-lens piece-movement-lens) p))

(define (game-state-type g) (lens-view (lens-compose state-type-lens game-state-lens) g))
(define (game-state-data g) (lens-view (lens-compose state-data-lens game-state-lens) g))

;; constants
(define board-size 8)
(define square-size 60)
(define board-width (* square-size board-size))

(define chess-game%
  (class object%
    (super-new)

    ;; types of moves
    (define diagonal-moves (list (posn 1 1) (posn 1 -1) (posn -1 1) (posn -1 -1)))
    (define straight-moves (list (posn 1 0) (posn 0 1) (posn -1 0) (posn 0 -1)))
    (define knight-moves   (list (posn 2 1) (posn 2 -1) (posn -2 1) (posn -2 -1) (posn 1 2) (posn 1 -2) (posn -1 2) (posn -1 -2)))

    ;; each pieces movement pattern
    (define rook-movement (movement straight-moves #true))
    (define king-movement (movement (append straight-moves diagonal-moves) #false))
    (define knight-movement (movement knight-moves #false))
    (define bishop-movement (movement diagonal-moves #true))
    (define queen-movement (movement (append straight-moves diagonal-moves) #true))

    ;; starting pieces
    (define starting-pieces
      (list
       ;; black pieces
       (piece 'rook 'black (posn 0 0) rook-movement #false)
       (piece 'knight 'black (posn 1 0) knight-movement #false)
       (piece 'bishop 'black (posn 2 0) bishop-movement #false)
       (piece 'queen 'black (posn 3 0) queen-movement #false)
       (piece 'king 'black (posn 4 0) king-movement #false)
       (piece 'bishop 'black (posn 5 0) bishop-movement #false)
       (piece 'knight 'black (posn 6 0) knight-movement #false)
       (piece 'rook 'black (posn 7 0) rook-movement #false)
       ;; white pieces
       (piece 'rook 'white (posn 0 7) rook-movement #false)
       (piece 'knight 'white (posn 1 7) knight-movement #false)
       (piece 'bishop 'white (posn 2 7) bishop-movement #false)
       (piece 'queen 'white (posn 3 7) queen-movement #false)
       (piece 'king 'white (posn 4 7) king-movement #false)
       (piece 'bishop 'white (posn 5 7) bishop-movement #false)
       (piece 'knight 'white (posn 6 7) knight-movement #false)
       (piece 'rook 'white (posn 7 7) rook-movement #false)))

    ;; starting game
    (define starting-game (game starting-pieces 'white (state 'waiting empty)))

    ;; the current game (will be mutated based on user input)
    (define current-game starting-game)

    ;; board helper functions
    (define (other-color color)
      (if (eq? 'white color) 'black 'white))

    (define (get-pieces color board)
      (filter (λ (p) (eq? color (piece-color p))) (game-pieces board)))
    
    ;; get piece at location or return false
    (define (get-piece position board)
      (findf (λ (p) (posn=? (piece-position p) position)) (game-pieces board)))

    ;; returns the color of the piece we collided with or false
    (define (collision position board)
      (let ([p (get-piece position board)])
        (if p (piece-color p) #false)))

    ;; is the posn on the board?
    (define (in-bounds? position board)
      (posn/in-bounds? position 0 7 0 7 #true))

    ;; a piece can move to a location if it's in bounds and not occupied by a piece of the same color
    (define (can-move-here? p position board)
      (and (in-bounds? position board) (not (eq? (collision position board) (piece-color p)))))

    (define (get-repeat-movement p board)
      (define (repeat-direction position direction)
        (let ([next-position (posn/plus position direction)])
          (cond [(not (can-move-here? p next-position board)) empty]
                [(collision next-position board) (cons next-position empty)]
                [else (cons next-position (repeat-direction next-position direction))])))
      (append-map (λ (d) (repeat-direction (piece-position p) d)) (piece-movement-directions p)))

    (define (get-non-repeat-movement p board)
      (filter (λ (position) (can-move-here? p position board))
              (posn-list/plus (piece-movement-directions p) (piece-position p))))

    (define (get-king color board)
      (findf (λ (p) (and (eq? 'king (piece-type p)) (eq? color (piece-color p)))) (game-pieces board)))

    (define (move-piece p position board)
      (let ([new-pieces
             (filter-not (λ (p2)
                           (or
                            (eq? (piece-position p2) (piece-position p))
                            (eq? (piece-position p2) position))) (game-pieces board))]
            [new-piece (lens-set piece-position-lens p position)])
        (lens-set game-pieces-lens board (append new-pieces (list new-piece)))))

    ;; is the king who's turn it is in check?
    (define (in-check? board)
      (let ([king (get-king (game-turn board) board)]
            [other-pieces (get-pieces (other-color (game-turn board)) board)])
        (member (piece-position king) (append-map (λ (p) (potential-moves p board)) other-pieces))))
    
    (define (potential-moves p board)
      (if (piece-movement-repeatable p)
          (get-repeat-movement p board)
          (get-non-repeat-movement p board)))
      
    (define (all-possible-moves p board)
      (filter-not
       (λ (move)
         (in-check? (move-piece p move board)))
       (potential-moves p board)))

    (define (select-piece p position)
      (lens-set game-state-lens current-game (state 'selected (list p (all-possible-moves p current-game)))))

    (define (flip-turn board)
      (lens-set game-turn-lens board (other-color (game-turn board))))
       
  
    (define (select-square position)
      (let ([p (get-piece position current-game)]
            [state-type (game-state-type current-game)]
            [state-data (game-state-data current-game)])
        (cond [(and p (eq? (game-turn current-game) (piece-color p))) (set! current-game (select-piece p position))]
              [(eq? state-type 'selected)
               (cond [(member position (second state-data)) (set! current-game (move-piece (first state-data) position (flip-turn current-game)))
                                                            (set! current-game
                                 (lens-set game-state-lens current-game (state 'waiting empty)))]
                     [else (set! current-game
                                 (lens-set game-state-lens current-game (state 'waiting empty)))])])))


             

    
               
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
         (piece-position p)
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
          [(equal? event-type 'left-up) (select-square position)]
          [(equal? event-type 'left-down) (press-square bmdc position)]
          [(or (equal? event-type 'motion) (equal? event-type 'left-up)) (hover-square bmdc position)])
        (draw-pieces (game-pieces current-game) bmdc)
        (set! current-bm (send bmdc get-bitmap))))

    ;; the canvas calls this to update it's bitmap
    (define/public (get-bitmap) current-bm)))

(define current-chess-game (new chess-game%)) ;; this could be passed in as an init parameter to board-canvas...

;; the canvas with on overridden event function... really these two classes could probably just be combined or organized better
(define board-canvas%
  (class canvas%
    (inherit get-width get-height refresh)
    
    (define/override (on-event event)
      (if (is-a? event mouse-event%) (send current-chess-game handle-mouse event) (println "not a mouse event"))
      (refresh))

    (define/private (my-paint-callback self dc)
      (let ([bm (send current-chess-game get-bitmap)])
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
