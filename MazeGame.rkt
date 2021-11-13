#lang slideshow
(require 2htdp/image)
(define (transform max pos)
  (+ (- max pos) 1))

(define (memqLite item collection)
  (cond ((null? collection) false)
        ((and (= (car item) (caar collection)) (= (cadr item) (cadar collection))) true)
        (else (memqLite item (cdr collection)))))

(define (Maze base maxX maxY colorWall colorOther walls pacMan puckX puckY)
  (define (row initialX y)
    (define x (transform maxX initialX))
    (if (= x 10)
        (if (memqLite (list x y) walls)
           (colorize base colorWall)
           (if (and (= x puckX) (= y puckY))
               pacMan
               (if (= y 10)
                   (colorize base "red")
                   (colorize base colorOther))))
        (if (memqLite (list x y) walls)
           (hc-append (colorize base colorWall) (row (- initialX 1) y))
           (if (and (= x puckX) (= y puckY))
               (hc-append pacMan (row (- initialX 1) y))
               (hc-append (colorize base colorOther) (row (- initialX 1) y))))
        )
    )
  (if (= maxY 1)
      (row maxX maxY)
      (vc-append (row maxX maxY) (Maze base maxX (- maxY 1) colorWall colorOther walls pacMan puckX puckY))
      )
  )

(define wall (list (list 1 2) (list 2 2) (list 3 2) (list 4 2) (list 4 3) (list 4 4) (list 6 2) (list 7 2) (list 8 2) (list 9 2) (list 9 3) (list 8 3)
(list 7 3) (list 6 3) (list 6 4) (list 6 5) (list 6 6) (list 5 6) (list 4 6) (list 3 6) (list 2 6) (list 2 5) (list 8 5) (list 8 6) (list 8 7) (list 2 8)
(list 2 9) (list 3 9) (list 4 9) (list 5 9) (list 6 9) (list 7 9) (list 8 9) (list 9 9) (list 10 9) (list 10 8) (list 10 7) (list 10 6) (list 10 5)))

(define pacMan (rotate 30 (wedge 15 300 "solid" "gold")))
(define baseShape (filled-rounded-rectangle 30 30 #:border-color "black" #:border-width 2))
(define (drawBoard x y)
 (Maze baseShape 10 10 "brown" "lightblue" wall pacMan x y))

(define (displayMenu posX posY)
  (printf "\nChoose what you want to learn\n")
  (printf "1. Up\n")
  (printf "2. Down\n")
  (printf "3. Left\n")
  (printf "4. Right\n")

  (define (direction c posX posY)
    (cond [(= c 1) (list posX (+ posY 1))]
          [(= c 2) (list posX (- posY 1))]
          [(= c 3) (list (- posX 1) posY)]
          [(= c 4) (list (+ posX 1) posY)]
          [(= c 5) (exit)]
          [else (printf "Invalid choice.\n") (direction (read) posX posY)]))
  (direction (read) posX posY))

(define (checkValid curX curY maxX maxY)
  (cond( (and (and (and (> curX 0) (<= curX maxX)) (and (> curY 0) (<= curY maxY))) (not (memqLite (list curX curY) wall))) true)
        (else false))
  )

(define bombs (list (list 2 4) (list 5 8)))

(define (Game posX posY)
  (cond
    ((and (= posX 10) (= posY 10)) 1)
    ((memqLite (list posX posY) bombs) 2)
    (else
     (print (drawBoard posX posY))
     (define newLoc (displayMenu posX posY))
     (define newX (car newLoc))
     (define newY (car (cdr newLoc)))
     (if (checkValid newX newY 10 10)
         (Game newX newY)
         (Game posX posY))
    )
  )
)
(display "Start GL HF\n")
(define value (Game 1 1))
(cond
  ((= value 1) (drawBoard 10 10) (display "Victory Royale!!\n"))
  ((= value 2) (display "KEKW!! You died to a landmine\nFan-Doby Dozy If I do say so myself\n"))
  (false)
    )
(display "GG EZ")
