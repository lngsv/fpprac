#lang racket
(require racket/class
         racket/set
         lazy/force
         racket/promise
         racket/serialize
         compatibility/mlist
         (only-in racket/list shuffle argmax))


(define seeds-per-house 6)

; класс, описывающий игральную доску
(define kalah-board%
  (let ((n seeds-per-house))
    (class object%
      (super-new)

      (init-field
       [u (mlist n n n n n n 0)]
       [d (mlist n n n n n n 0)]
      )
    )
  )
)

(define kalah%
  (class object%
    (super-new)

    (init-field board my-row op-row)

    ; TODO: придумать норм представление;
    ; возможно, имеет смысл "поворачивать" доску так,
    ; чтобы ряд играющего был нижним;
    ; надо как-то выделить калахи
    (define/public (show-state)
      (display "u:")
      (mfor-each
       (lambda (x)
         (printf " ~a" x)
       )
       (get-field u board)
      )
      (printf "\n")
      (display "d:")
      (mfor-each
       (lambda (x)
         (printf " ~a" x)
       )
       (get-field d board)
      )
      (printf "\n")
    )


    
    ; функция "делающая ход"
    ; pos - выбранная лунка (0..5)
    (define/public (change-state pos)
      ; seeds - количество камней в выбранной для хода лунке
      (let ((seeds (mlist-ref my-row pos)))
        ; считаем, сколько будет камней в каждой лунке:
        ; в реальной игре игрок бы взял камни в выбранной лунке
        ; и клал бы камни по одному в каждую лункц против часовой стрелки
        ; получилось бы какое-то количество полных кругов и один неполный
        (begin
          ; обрабатывает my-row
          (let ((new-my-row
          (list->mlist (build-list
           7
           (lambda (x)
             (let ((shifted (modulo (- x pos) 13)))
               (+ (quotient seeds 13) ; камни, которые положатся полных кругах
                  (if (<= 1 shifted (remainder seeds 13)) 1 0) ; камни, которые положатся на последнем, неполном круге
                  (if (= x pos) 0 (mlist-ref my-row x)) ; то, что было в лунке
               )
             )
           )
          ))))
          (set-mcar! my-row (mcar new-my-row)) (set-mcdr! my-row (mcdr new-my-row)))
          
          ; обрабатываем op-row
          (let ((new-op-row
          (mappend (list->mlist (build-list
           7
           (lambda (x)
             (if (= x 6) (mlist-ref op-row 6) ; пропускаем калах противника
             (let* ((y (+ x 7)) (shifted (modulo (- y pos) 13)))
               (+ (quotient seeds 13) ; камни, которые положатся полных кругах
                  (if (<= 1 shifted (remainder seeds 13)) 1 0) ; камни, которые положатся на последнем, неполном круге
                  (mlist-ref op-row x) ; то, что было в лунке
               )
             ))))
           )
          )))
          (set-mcar! op-row (mcar new-op-row)) (set-mcdr! op-row (mcdr new-op-row)))

          ; захват камней противника
          (let ((last-house (+ pos (remainder seeds 13))))
            (cond ((and
                 (< last-house 6) ; последний камень положили в свою лунку
                 (= (mlist-ref my-row last-house) 1) ; последняя лунка была пустая
                 (> (mlist-ref op-row (- 5 last-house)) 0) ; противоположная лунка непустая
                )
                (begin
                  (set-mcar! (mlist-tail my-row 6) (+ (mlist-ref my-row 6)
                                                      (mlist-ref op-row (- 5 last-house))
                                                      1))
                  (set-mcar! (mlist-tail my-row last-house) 0)
                  (set-mcar! (mlist-tail op-row (- 5 last-house)) 0)
                )
            ))
          )
          
          ; возвращение ответа
          ; 'game-over - один из рядов опустел - заканчиваем игру
          ; 'my-turn - последний камень попал в калах - повтор хода
          ; 'op-turn - в любом другом случае передаем ход противнику
          (let ((my-sum (- (foldl + 0 (mlist->list my-row)) (mlist-ref my-row 6)))
                (op-sum (- (foldl + 0 (mlist->list op-row)) (mlist-ref op-row 6))))
            (cond
              ((or (= my-sum 0) (= op-sum 0))
               (begin (set-mcar! my-row 0) (set-mcdr! my-row (mlist 0 0 0 0 0 (+ (mlist-ref my-row 6) my-sum)))
                      (set-mcar! op-row 0) (set-mcdr! op-row (mlist 0 0 0 0 0 (+ (mlist-ref op-row 6) op-sum)))
                      'game-over))
              ((= (+ pos (remainder seeds 13)) 6) 'my-turn)
              (else 'op-turn)
            )
          )
        )
      )
    )

    
  )
)

(define interactive-player%
  (class kalah%
    (super-new)

    (init-field name)
    (field [op 'undefined])
    (inherit show-state)
    (inherit change-state)
    (inherit-field my-row)
    (inherit-field op-row)
    
    (define/public (my-win?)
      (if (>
           (mlist-ref my-row 6)
           (mlist-ref op-row 6)
          ) #t #f
      )
    )

    (define/public (op-win?)
      (if (<
           (mlist-ref my-row 6)
           (mlist-ref op-row 6)
          ) #t #f
      )
    )

    (define/public (draw?)
      (if (=
           (mlist-ref my-row 6)
           (mlist-ref op-row 6)
          ) #t #f
      )
    )

    ; просит пользователя ввести число, пока подается "плохой" val
    (define (improve-input val)
      (cond
        [(not (and (number? val) (<= 1 val 6)))
         (begin
           (displayln "Введите число от 1 до 6:")
           (improve-input (read)))]
        [(= (mlist-ref my-row (sub1 val)) 0)
         (begin
           (displayln "Выберите непустую лунку:")
           (improve-input (read)))]
        [else val]
      )
    )
    
    (define/public (make-move)
      (begin
        (show-state)
        (printf "Ход ~a\n" name)
        (let* ((val (read)) (ans (change-state (sub1 (improve-input val)))))
          (case ans
            [(my-turn) (begin (printf "Последний камень попал в калах! Снова ход ~a!\n" name)
                               (make-move))]
            [(op-turn) (send op make-move)]
            [(game-over)
             (begin (show-state) (printf "Игра окончена! ~a\n" 
                          (cond [(my-win?) (~a "Победа " name "!" #:separator "")]
                                [(op-win?) (~a "Победа " (get-field name op) "!" #:separator "")]
                                [else "Ничья!"])))]
            [else (printf "INVALID: ~a\n" ans)]
          )
        )
      )
    )
  )
)

(define new-board (new kalah-board%))
(define capture-check-board (new kalah-board% [u (mlist 0 3 0 5 0 0 7)] [d (mlist 0 1 0 0 0 0 8)]))
(define pre-draw-board (new kalah-board% [u (mlist 0 1 2 3 4 0 1)] [d (mlist 0 0 0 0 0 1 10)]))

(define init-board new-board)
(define A (new interactive-player% [name "A"] [board init-board] [my-row (get-field u init-board)] [op-row (get-field d init-board)]))
(define B (new interactive-player% [name "B"] [board init-board] [my-row (get-field d init-board)] [op-row (get-field u init-board)]))
(set-field! op A B)
(set-field! op B A)
(send B make-move)
