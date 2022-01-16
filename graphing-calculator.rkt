;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname _graphing) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
(require spd/tags)
(require racket/format)

;; ---------------
;; User Constants
;; Change these!
;; ---------------

;; The equation f(x). Change all you want.
(define (f x)
  (sin x) ; y = (10sin(x))/x
  ) 

;; Width impacts performance -
;; rectangles-per-pixel * width rectangles generated on screen at max
(define WIDTH 800)
(define HEIGHT 800)

;; The larger this is, the better the graph looks
;; also impacts performance quite a bit.
(define RECTANGLES-PER-PIXEL 5)

(define AXIS-THICKNESS 3)
(define GRAPH-THICKNESS 2)

(define AXIS-COLOR "black")
(define GRAPH-COLOR "blue")

;; How much to MULTIPLY/DIVIDE the SCALE FACTOR
;; each time we zoom in/out?
;; Higher number = faster zoom
(define ZOOM-FACTOR 1.5)

;; The image to place at the center of the screen
;; Set to empty-image to hide
(define CENTER-IMAGE (circle 5 "solid" "red"))

;; Text size and color, and decimals for coordinates
;; Set text size to 0 to hide
(define TEXT-SIZE 12)
(define TEXT-COLOR "red")
(define TEXT-DECIMALS 2) ; Max is 6


;; ---------------
;; Internal Constants
;; ---------------

(define CTR-X (/ WIDTH 2))
(define CTR-Y (/ HEIGHT 2))

(define Y-AXIS (rectangle AXIS-THICKNESS HEIGHT "solid" AXIS-COLOR))
(define X-AXIS (rectangle WIDTH AXIS-THICKNESS "solid" AXIS-COLOR))

(define GRAPH-SEGMENT (square GRAPH-THICKNESS "solid" GRAPH-COLOR))

(define MTS (rectangle WIDTH HEIGHT "solid" "white"))


;; -------------
;; Data Definitions
;; -------------

(@htdd Graph)
(define-struct graph (cx cy s))
;; Graph is (make-graph Number Number Number)
;; For origin coord of center-x center-y on screen, and scale factor
;; Scale factor must be greater than 1, or else zooming in will zoom out
;; interp. a graph, where scale factor is pixels per unit is window height/scale
;;         e.g. (make-graph (0 0 4) would make graph with origin at center
;;         of window, at 25 pixels per unit (window height 100)
(define G0 (make-graph 0 0 10))
(define G1 (make-graph -25 -25 10)) ; Most of the window is quadrant 1
(define G2 (make-graph 25 -25 10)) ; Most of window is quadrant 2 etc etc
(define G3 (make-graph 25 25 10))
(define G4 (make-graph -25 25 10))

(define (fn-for-graph x y s)
  (... (graph-cx x)
       (graph-cy y)
       (graph-s s)))

;;Pretend a data definition for Natural with add1 is also here

;; ------------
;; Functions
;; Check expects not possible due to length of result
;; ------------

(@htdf main)
(@signature Graph -> Graph)
;; Creates the graph

(define (main g)
  (big-bang g
    (to-draw render) 
    (on-key zoom)
    (on-mouse translate)))


(@htdf render)
(@signature Graph -> Image)
;; Renders graph to the screen by placing picture of (f x) on picture of axes

(@template Graph Natural encapsulated)

(define (render g)
  
  (local [
          
          (define scale (graph-s g))
          (define add-amount (/ 1 RECTANGLES-PER-PIXEL))
          (define (number->decimal-string n d)
            (~r (/ (round (* n (expt 10 d))) (expt 10 d))))
          (define center-text
            (string-append "(" (number->decimal-string
                                (- (graph-cx g)) TEXT-DECIMALS)
                           ", " (number->decimal-string
                                 (- (graph-cy g)) TEXT-DECIMALS)
                           ")"))
          
          (define (place-center g)
            (place-image (above CENTER-IMAGE
                                (text center-text TEXT-SIZE TEXT-COLOR))
                         CTR-X
                         (+ CTR-Y (/ TEXT-SIZE 2))
                         (render-rectangles g 0)))
          
          (define (render-rectangles g x)
            (local [(define image-y
                      (y->coord (+
                                 (f (-
                                     (coord->x x scale)
                                     (graph-cx g)))
                                 (graph-cy g))
                                scale))]
              (cond [(> x WIDTH) (place-axes g)]
                    [(or (not (real? image-y)) (> image-y HEIGHT) (< image-y 0))
                     (render-rectangles g (+ x add-amount))]
                    [else (place-image
                           GRAPH-SEGMENT
                           x image-y
                           (render-rectangles g (+ x add-amount)))])))
          
          (define (place-axes g)
            (place-image
             Y-AXIS
             (x->coord (graph-cx g) scale)
             CTR-Y
             (place-image
              X-AXIS
              CTR-X
              (y->coord (graph-cy g) scale)
              MTS)))

          ]
    
    
    (place-center g)))

  
  

(@htdf x->coord)
(@signature Number Number -> Number)
;; Changes graph x-coord to screen x-coord according scale factor

(check-expect (x->coord 0 10) CTR-X) ; Center of screen

(@template Number)
  
(define (x->coord n s)
  (+ (* n s) CTR-X))



(@htdf coord->x)
(@signature Number Number -> Number)
;; Changes x-coord on screen to x-coord on graph, with division by 0 protection

(check-expect (coord->x CTR-X 10) 0.00001)

(@template Number)

(define (coord->x n s)
  (local [(define try (/ (- n CTR-X) s))]
    (if (= try 0)
        (+ try 0.00001)
        try)))



(@htdf y->coord)
(@signature Number Number -> Number)
;; Changes y-coord on graph to y-coord on screen according to scale factor

(check-expect (y->coord 0 10) CTR-Y) ; Center of screen

(@template Number)

(define (y->coord n s)
  (+ (* (- n) s) CTR-Y))



(@htdf coord->y)
(@signature Number Number -> Number)
;; Changes y-coord on screen to y-coord on graph according to scale factor

(check-expect (coord->y CTR-Y 10) 0) ; Center of screen

(@template Number)

(define (coord->y n s)
  (- (/ (- n CTR-Y) s)))



(@htdf zoom)
(@signature Graph KeyEvent -> Graph)
;; Zooms graph on center x y by scale factor

(check-expect (zoom G0 "wheel-up") (make-graph 0 0 (* 10 ZOOM-FACTOR)))
(check-expect (zoom G0 " ") G0)

(@template Graph KeyEvent)

(define (zoom g ke)
  (cond [(key=? ke "wheel-up")
         (make-graph (graph-cx g) (graph-cy g) (* (graph-s g) ZOOM-FACTOR))]
        [(key=? ke "wheel-down")
         (make-graph (graph-cx g) (graph-cy g) (/ (graph-s g) ZOOM-FACTOR))]
        [else g]))


(@htdf translate)
(@signature Graph Integer Integer MouseEvent -> Graph)
;; Translates click location to center of screen

(check-expect (translate G1
                         (x->coord (graph-cx G1) (graph-s G1))
                         (y->coord (graph-cy G1) (graph-s G1))
                         "button-down") (make-graph 0.00001 0 (graph-s G0)))
(check-expect (translate G1 CTR-X CTR-Y "button-up") G1)

(@template Graph MouseEvent)

(define (translate g x y me)
  (cond [(mouse=? me "button-down")
         (make-graph (- (graph-cx g)
                        (- (coord->x x (graph-s g))
                           (coord->x CTR-X (graph-s g))))
                     (- (graph-cy g)
                        (- (coord->y y (graph-s g))
                           (coord->y CTR-Y (graph-s g))))
                     (graph-s g))]
        [else g]))

(main G0)