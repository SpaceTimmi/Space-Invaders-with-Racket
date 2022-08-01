;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname space-invaders-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders


;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 2)
(define MISSILE-SPEED 10)
(define ANGLE 4.5)

(define HIT-RANGE 10)

(define INVADE-RATE 100)

(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))

(define MISSILE (ellipse 5 15 "solid" "red"))



;; Data Definitions:

(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))



(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))



(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 12))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -10))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right


#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))


(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))



(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))
(define G4 (make-game (list I1 I2) (list M1 M2) T2))


;;!!!
;; The random function for invaders produces too much invaders
;; fix the shot mechanic
;; add a stop-when function to main

;; World State - Space invaders

(define (main ws)
  (big-bang ws
    (on-tick update-state)    ;; Ws -> Ws 
    (to-draw display-state)   ;; Ws -> Image
    (on-key  move-tank)))     ;; Ws (KeyPress) -> Ws


;; Ws -> Ws
;; on tick update the state of the missiles, invaders and tank
                         
;; Main function
(define (update-state ws)
  (make-game (update-invaders (game-invaders ws) (game-missiles ws))
             (update-missile  (game-missiles ws))
             (update-tank     (game-tank ws))))

;; Helper functions
(define (update-invaders loi lom)
  (cond [(empty? loi) (random-invader empty)]
        [else (if (missile-hit-invader? (first loi) lom)
                  (update-invaders (rest loi) lom)
                  (cons (move-invader (first loi))
                        (update-invaders (rest loi) lom)))]))

;; (helper function for update-invaders)           
(define (move-invader invader)
  (cond [(> (invader-x invader) WIDTH) (make-invader (- (invader-x invader) (invader-dx invader))
                                                     (+ (invader-y invader) ANGLE)  ; Hits right wall
                                                     -10)]
        [(< (invader-x invader) 0)     (make-invader (+ (invader-x invader) (invader-dx invader))
                                                     (+ (invader-y invader) ANGLE)  ; Hits left wall
                                                     +10)]
        [else
         (make-invader (+ (invader-x invader) (invader-dx invader))
                       (+ (invader-y invader) ANGLE)
                       (invader-dx invader))]))


;; checks if any missiles colides with an invader then returns true or false
;; !!! (This is bugged)
(define (missile-hit-invader? invader lom)
  (cond [(empty? lom) false]
        [else (if (or (= (+ HIT-RANGE (invader-y invader)) (missile-y (first lom)))
                      (= (invader-y invader) (missile-y (first lom))))
                  true
                  (missile-hit-invader? invader (rest lom)))]))

;; Decides if a new invader should pop on-screen
;; if random produces 1 a new invader will appear; if random produces 0 a new invader will not be added to the ListOfInvaders
;; (helper function for update-invaders)
(define (random-invader e)
  (local [(define rand-inv (random 100))]
    (if (= rand-inv 0)
        (cons (make-invader (random WIDTH)  1 12) empty)
        empty)))


(define (update-missile lom)
  (cond [(empty? lom) empty]
        [else
         (if (> (missile-y (first lom)) 0)
             (cons (make-missile (missile-x (first lom)) (- (missile-y (first lom)) MISSILE-SPEED))
                   (update-missile (rest lom)))
             (update-missile (rest lom)))]))


(define (update-tank t)
  (local [(define new-pos (+ (tank-x t) (tank-dir t)))]
    (cond [(> new-pos WIDTH) (make-tank (+ (tank-x t) (- TANK-SPEED)) -1)]
          [(< new-pos 0)     (make-tank (+ (tank-x t)    TANK-SPEED)   1)]
          [else
           (make-tank (+ (tank-x t) (* TANK-SPEED (tank-dir t)))
                      (tank-dir t))])))



;; Ws -> Image
;; display the missiles, invaders and tank on-screen
(check-expect (display-state G0)
              (place-image TANK
                           (tank-x (game-tank G0))
                           (- HEIGHT TANK-HEIGHT/2)
                           BACKGROUND))
(check-expect (display-state G1)
              (place-image TANK
                           (tank-x (game-tank G1))
                           (- HEIGHT TANK-HEIGHT/2)
                           BACKGROUND))
(check-expect (display-state G2)
              (place-image TANK
                           (tank-x (game-tank G2))
                           (- HEIGHT TANK-HEIGHT/2)
                           (place-image MISSILE
                                        (missile-x (first (game-missiles G2)))
                                        (missile-y (first (game-missiles G2)))
                                        (place-image INVADER
                                                     (invader-x (first (game-invaders G2)))
                                                     (invader-y (first (game-invaders G2)))
                                                     BACKGROUND))))
              

;; Main Function
(define (display-state ws)
  (place-image TANK
               (tank-x (game-tank ws))
               (- HEIGHT TANK-HEIGHT/2)
               (display-invader (game-invaders ws) (game-missiles ws))))

;; Helper Functions
(define (display-invader invader m)
  (cond [(empty? invader) (display-missile m)]
        [else
         (place-image INVADER
                      (invader-x (first invader))
                      (invader-y (first invader))
                      (display-invader (rest invader) m))]))

(define (display-missile m)
  (cond [(empty? m) BACKGROUND]
        [else
         (place-image MISSILE
                      (missile-x (first m))
                      (missile-y (first m))
                      (display-missile (rest m)))]))



;; Ws (KeyPress) -> Ws
;; update the postion of the tank when the left or right arrow keys is pressed, and fire missile when spacebar is pressed.
; (define G4 (make-game (list I1 I2) (list M1 M2) T2))
; T1 Right
; T2 Left

(check-expect (move-tank G1 "left")
              (make-game empty
                         empty
                         (make-tank (tank-x      (game-tank G1))
                                    (- (tank-dir (game-tank G1))))))
(check-expect (move-tank G1 "right")
              (make-game empty
                         empty
                         T1))
(check-expect (move-tank G4 "right")
              (make-game (game-invaders G4)
                         (game-missiles G4)
                         (make-tank (tank-x (game-tank G4))
                                    (- (tank-dir (game-tank G4))))))
(check-expect (move-tank G4 " ")
              (make-game (game-invaders G4)
                         (append
                          (game-missiles G4)
                          (list (make-missile (tank-x (game-tank G4))
                                              (- HEIGHT TANK-HEIGHT/2))))
                         (game-tank G4)))



;; Main Function

(define (move-tank ws ke)
  (cond [(or (and (= (tank-dir (game-tank ws))  1) (key=? ke "left"))    ;; Tank was going right and key equals left
             (and (= (tank-dir (game-tank ws)) -1) (key=? ke "right")))  ;; Tank was going left and key equals right 
         (make-game (game-invaders ws)
                    (game-missiles ws)
                    (make-tank (tank-x (game-tank ws))
                               (- (tank-dir (game-tank ws)))))]
        [(key=? ke " ") (make-game (game-invaders ws)
                                   (add-missile (game-missiles ws) (game-tank ws)) ;; When spacebar is pressed
                                   (game-tank ws))]
        [else ws]))

;; Helper function
(define (add-missile lom tank)
  (cond [(empty? lom ) (cons (make-missile (tank-x tank)
                                           (- HEIGHT TANK-HEIGHT/2)) empty)]
        [else
         (cons (first lom)
               (add-missile (rest lom) tank))]))