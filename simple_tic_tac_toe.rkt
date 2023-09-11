;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Assignment 4 (Avery R and Nick G)|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)
(require racket/list)

(define-struct world-state (turn board difficult done))
;;a world-state is a (make-world-state boolean listOfNatural[0, 1, 2] Natural Natural[0, 1, 2, 3])
;;interp. Turn is a boolean that represents whose turn it is:
;;                          true is player's, false is computer's turn
;;        Board is a list of Numbers where
;;                           0 represents a blank space,
;;                           1 represents an X,
;;                           and 2 represents an O.
;;        Difficult represents the difficulty from a sale of 0-2.
;;        Done represents the state of the game (has anyone won yet?) where
;;                           0 represents that the game is not done,
;;                           1 represents the player's win,
;;                           2 represents the opponent's win,
;;                           and 3 represents a draw.

;;CONSTANTS
(define SIZE 900) ;;<<< Change this constant to scale the entire game!
(define MTS (empty-scene SIZE SIZE))
(define LN-SIZE (floor (* SIZE .04)))
(define LN (make-pen "goldenrod" LN-SIZE "solid" "round" "round"))
(define MARGIN (floor (* SIZE 0.05)))
(define pos1 (* SIZE (/ 1 3)))
(define pos2 (* SIZE (/ 2 3)))
(define TEXT-SIZE (floor (* SIZE .25)))
(define image-placement (list (/ SIZE 6) (/ SIZE 2) (* SIZE (/ 5 6))))
(define START (make-world-state true (list 0 0 0 0 0 0 0 0 0) 0 0))
;;^^^^ use this to start the game!
(define general-posn-list
  (list (make-posn (/ 1 6) (/ 1 6))
        (make-posn (/ 3 6) (/ 1 6))
        (make-posn (/ 5 6) (/ 1 6))
        (make-posn (/ 1 6) (/ 3 6)) ;;we'll use this list later in the render
        (make-posn (/ 3 6) (/ 3 6)) ;;function, which will scale everything up/down
        (make-posn (/ 5 6) (/ 3 6)) ;;according to SIZE (these are general position terms
        (make-posn (/ 1 6) (/ 5 6)) ;;If the SIZE was to be set to 1)
        (make-posn (/ 3 6) (/ 5 6))
        (make-posn (/ 5 6) (/ 5 6))))
(define win-lists
  (list
   (list 0 1 2) ;;Horizontal win
   (list 3 4 5) ;;Horizontal win
   (list 6 7 8) ;;Horizontal win
   (list 0 3 6) ;;Vertical win
   (list 1 4 7) ;;Vertical win
   (list 2 5 8) ;;Vertical win
   (list 0 4 8) ;;Diagonal win
   (list 2 4 6))) ;;Diagonal win
(define gameboard
  (add-line
   (add-line
    (add-line
     (add-line
      MTS
      MARGIN pos2 (- SIZE MARGIN) pos2 LN)
     MARGIN pos1 (- SIZE MARGIN) pos1 LN)
    pos2 MARGIN pos2 (- SIZE MARGIN) LN)
   pos1 MARGIN pos1 (- SIZE MARGIN) LN))

;;Main function
;;signature: World-State -> World-State
;;purpose: To initialize the world state
(define (main ws)
  (big-bang ws
    (on-tick check-conditions)
    (on-mouse mouse-handle)
    (on-key key-handle)
    (to-draw render)))

;;check-conditions function
;;signature: World-State -> World-State
;;purpose: Checks to see if its the opponents turn (and takes its turn) and
;;         also checks to see if the win conditions have been met.
(define (check-conditions ws)
  (local
    ;;ListOfNatural Natural Natural Natural -> ListOfNatural
    [(define (win-list board1 n1 n2 n3)
       (list (list-ref board1 n1)
             (list-ref board1 n2)
             (list-ref board1 n3)))
     ;;ListOfNatural -> Boolean
     (define (check-win lon)
       (or (andmap (lambda (n) (= n 1)) lon)
           (andmap (lambda (n) (= n 2)) lon)))
     ;;listOfNatural listOfNatural -> (listOf (listOfNatural))
     (define (create-win-lists board1 lon)
       (win-list board1 (first lon) (second lon) (third lon)))
     ;;ListOfNatural -> Boolean
     (define (has-won board1)
       (ormap check-win
              (map (lambda (l) (create-win-lists board1 l)) win-lists)))
     ;;ListOfNatural -> Natural
     (define (opponent-move board1)
       (local
         [(define hard (world-state-difficult ws))
          ;;ListOfNatural -> Natural
          (define (get-rand-index bd)
            (local
              [(define rand (random 8))]
              (if (valid-space bd rand)
                  rand
                  (get-rand-index bd))))
          ;;listOfNatural Natural Natural -> Natural
          (define (what-move bd index pl)
            (cond
              [(or (< 8 index) (= hard 0))
               (get-rand-index bd)]
              [(and (= hard 2) (= pl 2) (= index 8))
               (if (and (valid-space bd index)
                        (has-won (list-set bd index pl)))
                   index
                   (what-move bd 0 1))]
              [else
               (if (and (valid-space bd index)
                        (has-won (list-set bd index pl)))
                   index
                   (what-move bd (+ index 1) pl))]))]
         ;;Trampoline
         (what-move board1 0 2)))
     ;;Boolean -> Natural
     (define (who-wins turn)
       (if turn
           2
           1))
     ;;World-State -> Boolean
     (define (has-draw ws)
       (and (equal? (world-state-done ws) 0)
                (andmap (lambda (n) (or (equal? n 1)
                                        (equal? n 2)))
                        (world-state-board ws))))]
    ;;Trampoline
    (cond
      [(and (has-won (world-state-board ws))
            (equal? (world-state-done ws) 0))
       (make-world-state
        (world-state-turn ws)
        (world-state-board ws)
        (world-state-difficult ws)
        (who-wins (world-state-turn ws)))]
      [(has-draw ws)
       (make-world-state
        (world-state-turn ws)
        (world-state-board ws)
        (world-state-difficult ws)
        3)]
      [(and (not (world-state-turn ws))
            (equal? (world-state-done ws) 0))
       (take-turn ws (opponent-move (world-state-board ws)))]
      [else ws])))

;;render function
;;Signature: World-State -> Image
;;Purpose: Takes the world-state and creates an updated gameboard based off of the input world-state
(define (render ws)
  (local
    ;;Natural -> Image
    [(define (get-piece num)
       (cond
         [(equal? num 1) (piece-img "X" "red")]
         [(equal? num 2) (piece-img "O" "blue")]
         [else
          (square 0 "solid" "white")]))
     ;;ListOfNatural -> ListOfImage
     (define (make-image-list board1)
       (map get-piece board1))
     ;;Scale up the previous general-posn-list to size
     (define pos-list
       (map (lambda (p) (make-posn
                         (* (posn-x p) SIZE)
                         (* (posn-y p) SIZE)))
            general-posn-list))
     ;;String -> Image
     (define (win-img string)
       (text string (floor (* (/ 1 2) TEXT-SIZE)) "black"))
     ;;World-State -> Image
     (define (win-text ws)
       (cond
         [(equal? (world-state-done ws) 1)
          (win-img "Player Wins!")]
         [(equal? (world-state-done ws) 2)
          (win-img "Opponent Wins!")]
         [(equal? (world-state-done ws) 3)
          (win-img "Draw!")]
         [else (win-img "")]))
     ;;String String -> Image
     (define (piece-img piece color)
       (text piece TEXT-SIZE color))]
    ;;Trampoline
    (overlay (win-text ws) (place-images (make-image-list (world-state-board ws))
                                         pos-list gameboard))))

;;mouse-handle function
;;Signature: World-State Natural Natural -> World-State
;;Purpose: Update the World-State according to the the mouse click
(define (mouse-handle ws x y ME)
  (local
    ;;Natural Natural -> Natural
    [(define (what-space x y)
       (cond
         [(<= x (* SIZE (/ 1 3)))
          (what-row y 0)]
         [(>= x (* SIZE (/ 2 3)))
          (what-row y 2)]
         [else
          (what-row y 1)]))
     ;;Natural Natural -> Natural
     (define (what-row y column)
       (cond
         [(<= y (* SIZE (/ 1 3)))
          (+ 0 column)]
         [(>= y (* SIZE (/ 2 3)))
          (+ 6 column)]
         [else
          (+ 3 column)]))]
    ;;Trampoline
    (cond
      [(and (equal? ME "button-down")
            (equal? (world-state-done ws) 0)
            (world-state-turn ws)
            (valid-space (world-state-board ws) (what-space x y)))
       (take-turn ws (what-space x y))]
      [else ws])))

;;valid-space function
;;signature: ListOfNatural Natural -> Boolean
;;purpose: to see if a space on the board is valid to place a new piece.
(define (valid-space board1 index)
  (zero? (list-ref board1 index)))

;;take-turn function
;;signature: World-State Natural -> World-State
;;purpose: to produce the next world state depending on if a game-piece is to be
;;         placed and whose turn it is.
(define (take-turn ws index)
  (local
    ;;ListOfNatural Natural -> ListOfNatural
    [(define (update-board board1 index)
       (cond
         [(valid-space board1 index)
          (if
           (world-state-turn ws)
           (list-set board1 index 1)
           (list-set board1 index 2))]
         [else board1]))]
    ;;Trampoline
    (make-world-state
     (not (world-state-turn ws))
     (update-board (world-state-board ws) index)
     (world-state-difficult ws)
     (world-state-done ws))))
    
;;key-handle function
;;Signature: World-State -> World-State
;;Purpose: Update the world-state according to the entered key
(define (key-handle ws KE)
  ;;Defining the valid key presses
  (local
    [(define keyList (list "0" "1" "2"))]
    ;;Trampoline
    (cond
      [(ormap (lambda (k) (equal? k KE)) keyList)
       (make-world-state
        (world-state-turn ws)
        (world-state-board ws)
        (string->number KE)
        (world-state-done ws))]
      [else ws])))

;;this starts the program
(main START)