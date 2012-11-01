#lang racket

(define init-board
(list
(cons 0 'X) (cons 1 2) (cons 2 'X) (cons 3 2) (cons 4 'X) (cons 5 2) (cons 6 'X) (cons 7 2)
(cons 8 2) (cons 9 'X) (cons 10 2) (cons 11 'X) (cons 12 2) (cons 13 'X) (cons 14 2) (cons 15 'X)
(cons 16 'X) (cons 17 2) (cons 18 'X) (cons 19 2) (cons 20 'X) (cons 21 2) (cons 22 'X) (cons 23 2)
(cons 24 0) (cons 25 'X) (cons 26 0) (cons 27 'X) (cons 28 0) (cons 29 'X) (cons 30 0) (cons 31 'X)
(cons 32 'X) (cons 33 0) (cons 34 'X) (cons 35 0) (cons 36 'X) (cons 37 0) (cons 38 'X) (cons 39 0)
(cons 40 1) (cons 41 'X) (cons 42 1) (cons 43 'X) (cons 44 1) (cons 45 'X) (cons 46 1) (cons 47 'X)
(cons 48 'X) (cons 49 0) (cons 50 'X) (cons 51 1) (cons 52 'X) (cons 53 1) (cons 54 'X) (cons 55 1)
(cons 56 1) (cons 57 'X) (cons 58 1) (cons 59 'X) (cons 60 1) (cons 61 'X) (cons 62 1) (cons 63 'X)))

; TODO: Make more efficient (e.g. list-ref is called several times more than is minimal)
; Fix duplicate validation messages, e.g. Valid move: (40 26)\nValid move: (40 26)
(define (valid-move-except-range? board move player oponent_player)
  (let ([own_piece       (move_own_piece?          board move player)]
        [no_collision    (move_no_collision?       board move)]
        [right-direction (move-right-direction?    board move player)])
    (cond
      [(and own_piece right-direction no_collision)
                             (begin (fprintf (current-output-port) "Valid move: ~a\n"                       move) #t)]
      [(not own_piece)       (begin (fprintf (current-output-port) "Invalid move: piece not your own: ~a\n" move) #f)]
      [(not right-direction) (begin (fprintf (current-output-port) "Invalid move: wrong direction: ~a\n"    move) #f)]
      [(not no_collision)    (begin (fprintf (current-output-port) "Invalid move: collision: ~a\n"          move) #f)]
      [else                  (begin (fprintf (current-output-port) "Invalid move: ????? Debug!: ~a\n"       move) (read) #f)])))

(define (move_own_piece? board move player)
  (let ((piece (cdr (list-ref board (car move)))))
    (or (eqv? player piece)
        (eqv? (* 10 player) piece))))

(define (move_in_immediate_range? board move)
  (member (cadr move)
          (immediately-surrounding-tiles board (car move))))

; Get a list of potential end-tiles from a starting tile
; May go off the board: this is caught with "move-own-piece" validation
; E.g. (end-tiles init-board 24 1 1) => '(15 17 31 33),
; even though tiles 15 and 31 wrap the board on the other side.
; Due to the layout of the board, however, these wraparound tiles
; are forbidden tiles (X), and so there is no danger of landing on them.

; row-diff: The vertical row difference between start tile and potential end tile
; tiles-leftright: The horizontal tile difference between start tile and potential end tile
(define (end-tiles board tile_idx row-diff tiles-leftright)
  (let [(tiles-updown (* row-diff 8))]
    (filter (lambda (x) (and (>= x 0) (< x 64)))
            (list (- (- tile_idx tiles-updown) tiles-leftright)
                  (+ (- tile_idx tiles-updown) tiles-leftright)
                  (- (+ tile_idx tiles-updown) tiles-leftright)
                  (+ (+ tile_idx tiles-updown) tiles-leftright)))))

(define (immediately-surrounding-tiles board tile_idx)
  (end-tiles board tile_idx 1 1))

(define (jumpable-tiles board tile_idx)
  (end-tiles board tile_idx 2 2))

(define (oponent-in-jumpable-tile? board move)
  (member (cadr move)
          (jumpable-tiles board (car move))))

(define (end_tile_unoccupied? board move)
  (eqv? (cdr (list-ref board (cadr move))) 0))
  
(define (move_in_jumpable_range? board move oponent_player_number)
  (let* ([start_idx (car move)]
         [end_idx   (cadr move)]
         [oponent-jumpable?       (oponent-in-jumpable-tile? board move)]
         [oponent_player_between? (oponent_player_between_start_and_end? board move oponent_player_number)]
         [unoccupied?             (end_tile_unoccupied? board move)])
    (cond
      [(and oponent-jumpable? oponent_player_between? unoccupied?)
       (begin (fprintf (current-output-port) "Valid move: ~a\n" move) #t)]
      [(not oponent-jumpable?)
       (begin (fprintf (current-output-port) "Invalid move: oponent not in jumpable tile: ~a\n" move) #f)]
      [(not oponent_player_between?)
       (begin (fprintf (current-output-port) "Invalid move: oponent not between start and end: ~a\n" move) #f)]
      [(not unoccupied?)
       (begin (fprintf (current-output-port) "Invalid move: end tile occupied: ~a\n" move) #f)]
      ; This has happened, while trying to get capture working (10.20.12):
      [else
       (begin (fprintf (current-output-port) "Invalid move: I don't know what happened. Debug!: ~a\n" move) #f)])))

; TODO: Break up this large function into several smaller functions
(define (oponent_player_between_start_and_end? board move oponent_player_number)
  (let* [(start_idx (car move))
         (end_idx   (cadr move))
         (tile-diff (- start_idx end_idx))
         (possible-opponent-tile (cond 
                                   [(eq? tile-diff  18) (- start_idx 9)]
                                   [(eq? tile-diff  14) (- start_idx 7)]
                                   [(eq? tile-diff -14) (+ start_idx 7)]
                                   [(eq? tile-diff -18) (+ start_idx 9)]
                                   [else -1]))]
    (cond [(or (< possible-opponent-tile 0)
               (>= possible-opponent-tile (length board)))
           #f]
          [else
           (or 
            (eq? oponent_player_number
                 (cdr (list-ref board possible-opponent-tile)))
            (eq? (* oponent_player_number 10)
                 (cdr (list-ref board possible-opponent-tile))))])))

(define (move_no_collision? board move)
  (eqv? (cdr (list-ref board (cadr move))) 0))

(define (move-right-direction? board move player)
  (or (crowned-pre-move? board move) (valid-direction? move player)))

(define (oponent_player_piece_location move)
  (let* ((start_idx (car move))
         (end_idx   (cadr move))
         (tile-diff (- start_idx end_idx)))
    (cond 
      [(eq? tile-diff  18) (- start_idx 9)]    ; 2 tiles up and 2 to the left
      [(eq? tile-diff  14) (- start_idx 7)]    ; 2 tiles up and 2 to the right
      [(eq? tile-diff -14) (+ start_idx 7)]    ; 2 tiles down and 2 to the left
      [(eq? tile-diff -18) (+ start_idx 9)]))) ; 2 tiles down and 2 to the right

(define (valid-direction? move player)
  (let ([start_idx (car move)]
        [end_idx   (cadr move)])
    (cond
      [(eq? player 1) (> start_idx end_idx)]
      [(eq? player 2) (< start_idx end_idx)]
      [else #f])))

; Check if a piece can be crowned after its move has been made
; A piece can be crowned if has reached the side of the board
; opposite of its starting side

(define (crownable? board move player)
  (and (not (crowned-post-move? board move))
       (in-crown-spot? move player)))

(define (in-crown-spot? move player)
  (cond
    [(eq? player 1) (member (cadr move) '(0 1 2 3 4 5 6 7))]         ; Top
    [(eq? player 2) (member (cadr move) '(56 57 58 59 60 61 62 63))] ; Bottom
    [else #f]))

(define (crowned-pre-move? board move)
  (let ((piece (cdr (list-ref board (car move)))))
    (or (eq? piece 10) (eq? piece 20))))

(define (crowned-post-move? board move)
  (let ((piece (cdr (list-ref board (cadr move)))))
    (or (eq? piece 10) (eq? piece 20))))

(define (crown board move player)
  (replace-in-list board (cadr move) (* player 10)))

(define (replace-in-list lst pos replacement)
  (if (eq? pos 0)
      (append
       (list (cons pos replacement))
       (drop lst pos))
      (append
       (take lst pos)
       (list (cons pos replacement))
       (drop lst (+ pos 1)))))

(define (swap-start-end board move)
  (let* ((start_idx (car move))
        (end_idx    (cadr move))
        (start_fill (cdr (list-ref board start_idx)))
        (end_fill   (cdr (list-ref board end_idx))))
    (replace-in-list
     (replace-in-list board start_idx end_fill) ; becomes new board
     end_idx start_fill))) 

(define (player_move board move-fn player oponent_player)
  (let [(move (move-fn board))]
    (if (valid-move-except-range? board move player oponent_player)
        (let ((new-board
               
               (cond
                 ; Simple/immediate move possible
                 [(move_in_immediate_range? board move)
                  ; Try to crown the moved piece
                  ; TODO: DRY wrt try-crown
                  (try-crown (player-move-immediate board move player oponent_player)
                             move player oponent_player)]

                 ; Jump move possible
                 [(move_in_jumpable_range? board move oponent_player)
                  ; Try to chain together jump moves if possible
                  (try-jump-chain board move move-fn player oponent_player)]
                 
                 ; Else move not in range
                 [else (begin
                         (fprintf (current-output-port) "Invalid move: out of range: ~a\n" move)
                         (player_move board move-fn player oponent_player))])))

          ; Return the new board, or re-try if the move is invalid
          new-board)

        ; Else move is not valid because either it is someone else's piece,
        ; results in a collision, or is in the wrong direction.
        ; If invalid, try another move.
        (player_move board move-fn player oponent_player))))

(define (player-move-jump board move player oponent_player)
  (let*
      [(captured-piece-pos
        (oponent_player_piece_location move))
       
       ; Move your piece and remove the piece that was captured
       (jump-board
        
        (replace-in-list (swap-start-end board move)
                         captured-piece-pos
                         0))]
    
    (print-board jump-board)
    
    (fprintf (current-output-port)
             "CAPTURE! Player ~a moved from ~a to ~a and captured one of player ~a's pieces at ~a\n"
             player (car move) (cadr move) oponent_player captured-piece-pos)
    (fprintf (current-output-port) "Player 1 count: ~a\n" (+ (numpieces jump-board 1) (numpieces jump-board 10)))
    (fprintf (current-output-port) "Player 2 count: ~a\n" (+ (numpieces jump-board 2) (numpieces jump-board 20)))
    
    ; Pause to be able to see the above messages
    ; (read)
    
    ; Let the new board be the board in its post-jump state
    jump-board))

(define (player-move-immediate board move player oponent_player)
  (let ((immediate-board
         (swap-start-end board move)))
  
    (print-board immediate-board)
    (fprintf (current-output-port) "Valid immediate move: ~a\n" move)
    ; Pause to be able to see the above messages
    ; (read)
    
    ; Move your piece
    immediate-board))

(define (try-crown board move player oponent_player)
  ; Check to see if the moved piece can be crowned. If so, crown it,
  ; and return the board. Otherwise return the board without crowning the piece.
  (if (crownable? board move player)
      (let ((crowned-board (crown board move player)))
        (print-board crowned-board)
        (fprintf (current-output-port) "Crowning move: ~a\n" move)
        (fprintf (current-output-port) "Crowned! Player ~a's piece at ~a was crowned.\n" player (cadr move))
        (fprintf (current-output-port) "Player 1 count: ~a\n" (+ (numpieces crowned-board 1) (numpieces crowned-board 10)))
        (fprintf (current-output-port) "Player 2 count: ~a\n" (+ (numpieces crowned-board 2) (numpieces crowned-board 20)))
        ; Pause to be able to see the above messages
        ; (read)
        crowned-board)
      board))

(define (rejump board move-fn player oponent-player)
  (begin
    (print-board board)
    (fprintf (current-output-port) "Player ~a: Move: " player)
    (let [(move (move-fn board))]
      (if (and (valid-move-except-range? board move player oponent-player)
               (move_in_jumpable_range? board move oponent-player))
          (let [(new-board
                 (try-jump-chain board move move-fn player oponent-player))]
            (print-board new-board)
            new-board)
          (rejump board move-fn player oponent-player)))))

(define (try-jump-chain board move move-fn player oponent-player)
  (let [(jump-board
         (try-crown (player-move-jump board move player oponent-player)
                    move player oponent-player))]
    (if (any-jump-moves-available? jump-board (cadr move) player oponent-player)
        (rejump jump-board move-fn player oponent-player)
        jump-board)))

; TODO: Fix redundant print expressions called by map on move_in_jumpable_range?
(define (any-jump-moves-available? board piece-idx player oponent-player)
  (< 0 (length
        (filter (lambda (x) x)
                (map (lambda (end-idx)
                       (and  (move_in_jumpable_range? board (list piece-idx end-idx) oponent-player)
                             (move-right-direction? board (list piece-idx end-idx) player)))
                     (jumpable-tiles board piece-idx))))))

(define (human_player_move board player oponent_player)
  (begin
    (fprintf (current-output-port) "Player ~a: Please enter your move. Format: (start end)\n" player)
    (fprintf (current-output-port) "Move: ")
    (player_move board human_move player oponent_player)))

(define (human_move board)
   (read))

(define (AI_player_move board player openent_player)
    (player_move board AI_solve player openent_player))

(define (AI_solve board) (AI_strategy board 'random))

(define (AI_strategy board strategy)
  (cond
    [(eq? strategy 'random)
     (list
      (list-ref valid-tiles (random (length valid-tiles)))
      (list-ref valid-tiles (random (length valid-tiles))))]))
     
(define valid-tiles
  '(01 03 05 07
    08 10 12 14
    17 19 21 23
    24 26 28 30
    33 35 37 39
    40 42 44 46
    49 51 53 55
    56 58 60 62))

(define (player_move_types board)
  (begin
    (fprintf (current-output-port) "Select mode: [1] player vs cpu. [2] player vs player. [3] cpu vs cpu\n")
    (fprintf (current-output-port) "Mode: ")
    (let [(players_mode (read))]
      (print-board board)
      (cond
        [(eqv? players_mode 1) (cons human_player_move AI_player_move)]
        [(eqv? players_mode 2) (cons human_player_move human_player_move)]
        [(eqv? players_mode 3) (cons AI_player_move AI_player_move)]))))

(define (play_game board player_moves)
  (if (gameover? board)
      (fprintf (current-output-port) "Player ~a wins\n" (winner board))
      (play_game
       (let [(new-board ((car player_moves) board 1 2))]
         (if (gameover? new-board)
             new-board
             ((cdr player_moves) new-board 2 1)))
       player_moves)))

(define (gameover? board)
  (not (eqv? (winner board) 0)))

(define (winner board)
    (cond [(eqv? 0 (+ (numpieces board 1) (numpieces board 10))) 2]
          [(eqv? 0 (+ (numpieces board 2) (numpieces board 20))) 1]
          [else 0]))
  
(define (numpieces board player_number)
  (count (lambda (n) (eqv? (cdr n) player_number)) board))

(define (get-row board n)
  (filter
   (lambda (x) (and (>= (car x) (* n 8)) (< (car x) (* (+ n 1) 8))))
   board))

(define (print-board board)
  (map (lambda (n)
         (fprintf (current-output-port) "~a\n" (get-row board n)))
       '(0 1 2 3 4 5 6 7)))

(let [(board init-board)]
  (play_game board (player_move_types board)))


;; BEGIN TESTS SECTION

(require test-engine/racket-tests)

(define indices
  (list
   0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31
   32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61))

; Player 1 can move one of his own pieces
(check-expect (move_own_piece? init-board '(40 33) 1)
              #t)

; Player one can't move from a forbidden tile (marked by an X)
(check-expect (move_own_piece? init-board '(41 33) 1)
              #f)

; Player one can't move from an empty tile
(check-expect (move_own_piece? init-board '(24 33) 1)
              #f)

; Player 1 can't move one of player 2's pieces
(check-expect (move_own_piece? init-board '(17 33) 1)
              #f)

; Player 2 CAN move one his own pieces
(check-expect (move_own_piece? init-board '(17 33) 2)
              #t)

(check-expect
 (end-tiles init-board 44 1 1)
 (list 35 37 51 53))

(check-expect
 (end-tiles init-board 44 2 2)
 (list 26 30 58 62))

(check-expect
 (immediately-surrounding-tiles init-board 44)
 (list 35 37 51 53))

; Tiles 35, 37, 51, and 53 are in the immediate range of tile 44
(check-expect
 (not (false?
       (map (lambda (imm)
              (move_in_immediate_range? init-board (list 44 imm)))
            (immediately-surrounding-tiles init-board 44))))
 #t)

; All other tiles are not in the immediate range of tile 44
(check-expect
 (length
  (filter (lambda (x) x) ; Filter only true
          (map (lambda (non-imm)
                 (move_in_immediate_range? init-board (list 44 non-imm)))
               ; Leave only tiles other than the ones immediately surrounding 44
               (filter (lambda (tile) (not (member tile (immediately-surrounding-tiles init-board 44))))
                       indices))))
 0)

(check-expect
 (jumpable-tiles init-board 44)
 (list 26 30 58 62))

(check-expect
 (replace-in-list (list (cons 0 10) (cons 1 11) (cons 2 22) (cons 3 33) (cons 4 44)) 2 0)
 (list (cons 0 10) (cons 1 11) (cons 2 0) (cons 3 33) (cons 4 44)))

(check-expect
 (swap-start-end init-board '(40 33))
 (list
(cons 0 'X) (cons 1 2) (cons 2 'X) (cons 3 2) (cons 4 'X) (cons 5 2) (cons 6 'X) (cons 7 2)
(cons 8 2) (cons 9 'X) (cons 10 2) (cons 11 'X) (cons 12 2) (cons 13 'X) (cons 14 2) (cons 15 'X)
(cons 16 'X) (cons 17 2) (cons 18 'X) (cons 19 2) (cons 20 'X) (cons 21 2) (cons 22 'X) (cons 23 2)
(cons 24 0) (cons 25 'X) (cons 26 0) (cons 27 'X) (cons 28 0) (cons 29 'X) (cons 30 0) (cons 31 'X)
(cons 32 'X) (cons 33 1) (cons 34 'X) (cons 35 0) (cons 36 'X) (cons 37 0) (cons 38 'X) (cons 39 0)
(cons 40 0) (cons 41 'X) (cons 42 1) (cons 43 'X) (cons 44 1) (cons 45 'X) (cons 46 1) (cons 47 'X)
(cons 48 'X) (cons 49 1) (cons 50 'X) (cons 51 1) (cons 52 'X) (cons 53 1) (cons 54 'X) (cons 55 1)
(cons 56 1) (cons 57 'X) (cons 58 1) (cons 59 'X) (cons 60 1) (cons 61 'X) (cons 62 1) (cons 63 'X)))

; Uncomment the below expression to perform tests
;(test)