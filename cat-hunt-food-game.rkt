;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname cat-hunt-food-game) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(require 2htdp/image)
(require 2htdp/universe)

(define Image Any)
; interp. an image shown to the user

(define Speed Number)
; interp. speed of cat in pixels per tick.

(define Location Any)
; A Location is a structure: (make-posn Number Number)
; interp. x and y coordinate of a location on screen.

(define TickPeriod Number)
; interp. the current tick number, periods between 0 and 27

(define Time Number)
; interp. seconds since the game started

(define PosnX Number)
; interp. X value of a (X / Y) Position coordinate

(define PosnY Number)
;interp. Y Value of a (X / Y) Position coordinate

(define VectorLength Number)
; interp. the length of a vector

(define Orientation Number)
; interp. the orientation of the cat -> in which direction she looks in degree

(define-struct world-state (speed catPosition foodPosition worldCounter secSinceStart gamePaused?))

(define WorldStateSig (signature (WorldStateOf Speed Location Location TickPeriod Time Boolean)))
; interp. current speed of cat, current location of cat, current location of food

(define MouseEvent (signature (enum "button-down"
                                    "button-up"
                                    "drag"
                                    "move"
                                    "enter"
                                    "leave")))
; interp. mouse events, e.g., mouse movements or mouse clicks

; ---------------------------- Konstanten -------------------------------------------

; image sources:
; all pictures were genereated by OpenAI's DALL·E specifically for this game between 24.11-26.11.23

(define CATimg
  (bitmap "images/DALL·E_2023-11-24_16.25.06-Calm-Cat.png"))

(define FOODimg
  (bitmap "images/DALL·E_2023-11-24_16.31.02-food.png"))

(define RAGE_CATimg
  (bitmap "images/DALL·E_2023-11-25_12.25.31-Rage-Cat.png"))

(define U_WONimg
  (bitmap "images/DALL·E_2023-11-26_15.32.30-Sad-End.png"))

(define CAT_WONimg
  (bitmap "images/DALL·E_2023-11-26_15.41.03-Happy-End.png"))

(define FURY_WONimg
  (bitmap "images/DALL·E_2023-11-25_12.31.12-Rage-End.png"))

(define WIDTH
  800)

(define HEIGHT
  800)

(define catSpeed
  90)

; world ticks with a rate of 28ticks per second
(define WORLD-TICKS
  28)

(define SPEED-UP-INTERVALL
  5)

(define RAGE-MODE-TIME
  20)

(define SPEED-UP-SPEED
  25)

(define (userWon? worldInstnc)
  (= (world-state-secSinceStart worldInstnc) 30))

(define (TIME-TO-RAGE worldInstnc)
  (>= (world-state-secSinceStart worldInstnc) (+ RAGE-MODE-TIME 1)))

(define RAGE-SPEED
  550)

(define MTCN
  (empty-scene WIDTH HEIGHT))

; +++++++++++++++++++++++ helper functions ++++++++++++++++++++++++++++++++++++++++++++++++++++

; -------------------------------- Canvas draw --------------------------------

(: canvas (WorldStateSig -> Image))
; white canvas with cat and food
(define (canvas worldInstnc)
  (overlay/align "right" "top"
                 (text (string-append (number->string (world-state-secSinceStart worldInstnc)) " sec") 20 "black")
                 (place-image (rotate (rotateCalc worldInstnc) CATimg)
                              (posn-x (world-state-catPosition worldInstnc))
                              (posn-y (world-state-catPosition worldInstnc))
                              (place-image FOODimg
                                           (posn-x (world-state-foodPosition worldInstnc))
                                           (posn-y (world-state-foodPosition worldInstnc))
                                           MTCN))))

(: angryCanvas (WorldStateSig -> Image))
; special canvas when cat gets Angry
(define (angryCanvas worldInstnc)
  (overlay/align "right" "top"
                 (text (string-append (number->string (world-state-secSinceStart worldInstnc)) " sec") 20 "red")
                 (place-image (rotate (rotateCalc worldInstnc) RAGE_CATimg)
                              (posn-x (world-state-catPosition worldInstnc))
                              (posn-y (world-state-catPosition worldInstnc))
                              (place-image FOODimg
                                           (posn-x (world-state-foodPosition worldInstnc))
                                           (posn-y (world-state-foodPosition worldInstnc))
                                           (rectangle WIDTH HEIGHT "solid" "black")))))

; -------------------------- worldCounter modulator ---------------------------------

(: worldCounterModulator (WorldStateSig -> TickPeriod))
; sums ticks until they reached 27, then sets worldCounter back to 0
(define (worldCounterModulator worldInstnc)
  (if (= (world-state-worldCounter worldInstnc) (- WORLD-TICKS 1))
      0
      (+ (world-state-worldCounter worldInstnc) 1)))

(: seconds (WorldStateSig -> Time))
; calculates the seconds since the game started, based on worldCounter
(define (seconds worldInstnc)
  (if (= (world-state-worldCounter worldInstnc) 0)
      (+ (world-state-secSinceStart worldInstnc) 1)
      (world-state-secSinceStart worldInstnc)))

; ------------------------------ speed modulator ------------------------------------

(: speedBuff (WorldStateSig -> Speed))
; gives the cat all 5 seconds a speed buff -> at second 21, the cat gets enraged with high Speed
(define (speedBuff worldInstnc)
  (cond
    [(and (= (modulo (world-state-secSinceStart worldInstnc) SPEED-UP-INTERVALL) 0)
          (< (world-state-secSinceStart worldInstnc) RAGE-MODE-TIME))
     (+ (world-state-speed worldInstnc) SPEED-UP-SPEED)]
    [(= (world-state-secSinceStart worldInstnc) RAGE-MODE-TIME)
     RAGE-SPEED]
    [else (world-state-speed worldInstnc)]))

; -------------------------- berechnung zur neuen Katzen-position -------------------

(: newCatPosition (WorldStateSig -> Location))
; returns posn of cat after moving in direct direction to cat with the speed of world-state-speed
(define (newCatPosition worldInstnc)
  (make-posn (+ (posn-x (world-state-catPosition worldInstnc))
                (* (/ (unitValueX worldInstnc) 28) (world-state-speed worldInstnc)))
             (+ (posn-y (world-state-catPosition worldInstnc))
                (* (/ (unitValueY worldInstnc) 28) (world-state-speed worldInstnc)))))

(: calcXdirection (WorldStateSig -> Number))
; calculates diffrence between cat X and food X
(define (calcXdirection worldInstnc)
  (- (posn-x (world-state-foodPosition worldInstnc))
     (posn-x (world-state-catPosition worldInstnc))))

(: calcYdirection (WorldStateSig -> Number))
; calculates diffrence between cat Y and food Y
(define (calcYdirection worldInstnc)
  (- (posn-y (world-state-foodPosition worldInstnc))
     (posn-y (world-state-catPosition worldInstnc))))

(: vectorLength (WorldStateSig -> VectorLength))
; calculates the length of the vector cat -> food
(define (vectorLength worldInstnc)
  (sqrt (+ (expt (calcXdirection worldInstnc) 2) (expt (calcYdirection worldInstnc) 2))))

(: unitValueX (WorldStateSig -> Number))
; returns unit values for Xs
(define (unitValueX worldInstnc)
  (/ (calcXdirection worldInstnc) (vectorLength worldInstnc)))

(: unitValueY (WorldStateSig -> Number))
; returns unitValue for Ys
(define (unitValueY worldInstnc)
  (/ (calcYdirection worldInstnc) (vectorLength worldInstnc)))

(: rotateCalc (WorldStateSig -> Orientation))
; totation of cat depends on vectordirection. calculated via arctan -> Winkel = arctan(x, y) * 180 / pi
(define (rotateCalc worldInstnc)
  (+ (* (atan (unitValueX worldInstnc) (unitValueY worldInstnc)) (/ 180 pi)) 180))

; ---------------------------- berechnung der Vektorlänge ----------------------------

(: vectorLengthDirectionX1 (PosnX PosnY PosnX PosnY -> VectorLength))
; Calculates the length of a point (x/y) to another (x2/y2)
(define (vectorLengthDirectionX1 x1 y1 x2 y2)
  (sqrt (+ (expt (- x1 x2) 2) (expt (- y1 y2) 2))))

; ----------------------------- Cursor Checks ----------------------------------

(: cursorOnCat? (PosnX PosnY WorldStateSig -> Boolean))
; checks rather cursor is on the Cat or not
(define (cursorOnCat? posX posY worldInstnc)
  (> (/ (image-width CATimg) 2)
     (vectorLengthDirectionX1 (posn-x (world-state-catPosition worldInstnc))
                              (posn-y (world-state-catPosition worldInstnc))
                              posX
                              posY)))

(: cursorOnFood? (PosnX PosnY WorldStateSig -> Boolean))
; checks rather cursor is on the Food or not
(define (cursorOnFood? posX posY worldInstnc)
  (> (/ (image-width FOODimg) 2)
     (vectorLengthDirectionX1 (posn-x (world-state-foodPosition worldInstnc))
                              (posn-y (world-state-foodPosition worldInstnc))
                              posX
                              posY)))


; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(: INITIAL-STATE WorldStateSig)
; An initial state we can start the game with
(define INITIAL-STATE
  (make-world-state catSpeed
                    (make-posn (/ WIDTH 2) (- HEIGHT (/ HEIGHT 9)))
                    (make-posn (/ WIDTH 2) (+ 0 (/ HEIGHT 5)))
                    0
                    0
                    #f))

                       
(: draw (WorldStateSig -> Image))
; Draw the cat and its food at their location
; conditions checks rather game is paused and tehrefore print "game Paused" on screen if true.
(define (draw worldInstnc)
  (cond
    [(and (TIME-TO-RAGE worldInstnc)
          (world-state-gamePaused? worldInstnc))
     (overlay/align "middle" "middle"
                    (text "game Paused" 50 "red")
                    (angryCanvas worldInstnc))]
    
    [(TIME-TO-RAGE worldInstnc)
     (angryCanvas worldInstnc)]
    
    [(world-state-gamePaused? worldInstnc)
     (overlay/align "middle" "middle"
                    (text "game Paused" 50 "blue")
                    (canvas worldInstnc))]
    
    [else 
     (canvas worldInstnc)]))
  
         
(: tick (WorldStateSig -> WorldStateSig))
; Move the cat towards its food
(define (tick worldInstnc)
  ; The `if` statement ensures that the speedBuff is applied only exactly during its defined
  ; period. If the speedBuff were called in every tick, it would
  ; result in the buff being 28 times larger, as each time 28 values are added
  ; to the speed in the background, but it is only triggered every 5 seconds.
  (if (world-state-gamePaused? worldInstnc)
      (make-world-state 0
                        (world-state-catPosition worldInstnc)
                        (world-state-foodPosition worldInstnc)
                        26
                        (world-state-secSinceStart worldInstnc)
                        (world-state-gamePaused? worldInstnc))
      (make-world-state (if (= (world-state-worldCounter worldInstnc) 0)
                            (speedBuff worldInstnc)
                            (world-state-speed worldInstnc))
                        (newCatPosition worldInstnc)
                        (world-state-foodPosition worldInstnc)
                        (worldCounterModulator worldInstnc)
                        (seconds worldInstnc)
                        (world-state-gamePaused? worldInstnc))))

(: mouse (WorldStateSig Number Number MouseEvent -> WorldStateSig))
; Update the position of the food when the mouse moves
(define (mouse worldInstnc posX posY mouseEvent)
  (cond
    ; drag and drop the food on the canvas
    [(and (string=? mouseEvent "drag")
          (cursorOnFood? posX posY worldInstnc))
     (make-world-state (world-state-speed worldInstnc)
                       (world-state-catPosition worldInstnc)
                       (make-posn posX posY)
                       (world-state-worldCounter worldInstnc)
                       (world-state-secSinceStart worldInstnc)
                       #f)]
    ; when click on cat -> speed down by 1/3px per sec
    ; when click on canvas -> speed up by 1/3px per sec
    [(string=? mouseEvent "button-down")
     (cond
       [; when lcick on cat:
        (cursorOnCat? posX posY worldInstnc)
        ; if statement makes sure, cat speed won't go lower than 1/3 of max speed (and therefore
        ; preventing the user to set cat speed to 0 or even negative)
        (make-world-state (if (> (world-state-speed worldInstnc) (/ catSpeed 3))
                              (- (world-state-speed worldInstnc) (/ catSpeed 3))
                              (world-state-speed worldInstnc))
                          (world-state-catPosition worldInstnc)
                          (world-state-foodPosition worldInstnc)
                          (world-state-worldCounter worldInstnc)
                          (world-state-secSinceStart worldInstnc)
                          (world-state-gamePaused? worldInstnc))]
       [; when click on canvas:
        (and
         (not (cursorOnCat? posX posY worldInstnc))
         (not (cursorOnFood? posX posY worldInstnc)))
        (make-world-state (+ (world-state-speed worldInstnc) (/ catSpeed 3))
                          (world-state-catPosition worldInstnc)
                          (world-state-foodPosition worldInstnc)
                          (world-state-worldCounter worldInstnc)
                          (world-state-secSinceStart worldInstnc)
                          (world-state-gamePaused? worldInstnc))]
       [else worldInstnc])]
    ; freeze game if mouse leaves the canvas
    [(string=? mouseEvent "leave")
     (make-world-state (world-state-speed worldInstnc)
                       (world-state-catPosition worldInstnc)
                       (world-state-foodPosition worldInstnc)
                       (world-state-worldCounter worldInstnc)
                       (world-state-secSinceStart worldInstnc)
                       #t)]
    ; continue if mouse enters canvas again
    [(string=? mouseEvent "enter")
     (make-world-state (if (TIME-TO-RAGE worldInstnc)
                           RAGE-SPEED
                           (+ catSpeed (* (quotient (world-state-secSinceStart worldInstnc) SPEED-UP-INTERVALL)
                                          SPEED-UP-SPEED)))
                       (world-state-catPosition worldInstnc)
                       (world-state-foodPosition worldInstnc)
                       (world-state-worldCounter worldInstnc)
                       (world-state-secSinceStart worldInstnc)
                       #f)]    
    [else worldInstnc]))

; --------- game end ---------------

; if vector cat -> food reaches a certain lenght, cat wins

(define WINdistance
  (+ (/ (image-width CATimg) 3) (/ (image-width FOODimg) 3)))

; checks rather cat reached the winning distance
(define (catWon? worldInstnc)
  (or (< (vectorLength worldInstnc) WINdistance)
      (userWon? worldInstnc)))

; defines how the stop screen looks like
(define (endScreen worldInstnc)
  (cond
    [(userWon? worldInstnc)
     (overlay (above (text "You won!" 40 "black")
                     U_WONimg)
              MTCN)]
    [(TIME-TO-RAGE worldInstnc)
     (overlay (above (text "Cat won!" 40 "red")
                     FURY_WONimg)
              (rectangle WIDTH HEIGHT "solid" "black"))]
    [else 
     (overlay (above (text "Cat won!" 40 "blue")
                     CAT_WONimg)
              MTCN)]))

; ----------------------------------
  

(: cat-game (WorldStateSig -> WorldStateSig))
; Starts the game
(define (cat-game initial-state)
  (big-bang initial-state
    [to-draw draw]
    [on-tick tick]
    [on-mouse mouse]
    [stop-when catWon? endScreen]))



; --------- START GAME -------
(cat-game INITIAL-STATE)
; -------------------------
