#lang racket
(require racket/class
         compatibility/mlist
         racket/draw
         pict
         pict/color)


(define DEFAULT_STYLE (make-object font% 14 'modern)) ; шрифт по умолчанию
(define STORE_STYLE (cons 'bold DEFAULT_STYLE)) ; шрифт для калахов
(define MESSAGE_STYLE (cons 'italic DEFAULT_STYLE)) ; шрифт для сообщений

(define seeds-per-house 6) ; сложность игры, изначальное количество камней в лунке

; класс, описывающий ряд лунок
(define kalah-row%
  (let ((n seeds-per-house))
    (class object%
      (super-new)
      (init-field [lst (mlist n n n n n n 0)]) ; последнее значение - калах
    )
  )
)

; класс, описывающий игральную доску
(define kalah-board%
  (class object%
    (super-new)

    (init-field [op-row (new kalah-row%)] ; верхний ряд
                [my-row (new kalah-row%)] ; нижний ряд
    )

    (define STORE_COLOR "darkorange") ; цвет калахов
    (define HOUSE_COLOR "olivedrab") ; цвет лунок

    
    ; выводит значение x в нужном формате
    ; текущий формат: padding=3
    ; color - цвет выводимого текста
    ; style - шрифт выводимого текста
    (define (print-colorized-int x color style)
      (print (colorize
              (text (cond [(<= 0 x 9) (~a "  " x)]
                          [else (~a " " x)]) style)
              color
      ))
    )


    ; выводит состояние доски
    ; ряд играющего игрока - нижний
    (define/public (show-state)
      (printf "\n")
      (let ((rev (mreverse (get-field lst op-row))))
        (print-colorized-int (mcar rev) STORE_COLOR STORE_STYLE)
        (mfor-each
         (lambda (x)
           (print-colorized-int x HOUSE_COLOR DEFAULT_STYLE)
         )
         (mcdr rev)
        )
      )
      (printf "\n")
      (print (text "   " DEFAULT_STYLE))
      (mfor-each
       (lambda (x i)
         (if (= i 6)
             (print-colorized-int x STORE_COLOR STORE_STYLE)
             (print-colorized-int x HOUSE_COLOR DEFAULT_STYLE)
         )
       )
       (get-field lst my-row)
       (list->mlist (build-list 7 values))
      )
      (printf "\n")
      (printf "\n")
    )
  )
)

(define kalah%
  (class object%
    (super-new)
    
    ; функция "делающая ход"
    ; pos - выбранная лунка (0..5)
    ; S - исходное состояние
    (define/public (change-state pos S)
      (let* ((my-row (get-field lst (get-field my-row S)))
             (op-row (get-field lst (get-field op-row S)))
             (seeds (mlist-ref my-row pos)) ; количество камней в выбранной для хода лунке
             (res-S (new kalah-board%))
             (res-my-row (get-field lst (get-field my-row res-S)))
             (res-op-row (get-field lst (get-field op-row res-S)))) 
        ; считаем, сколько будет камней в каждой лунке:
        ; в реальной игре игрок бы взял камни в выбранной лунке
        ; и клал бы камни по одному в каждую лункц против часовой стрелки
        ; получилось бы какое-то количество полных кругов и один неполный
        (begin
          ; обрабатываем my-row
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
          (begin (set-mcar! res-my-row (mcar new-my-row)) (set-mcdr! res-my-row (mcdr new-my-row))))
          
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
             ))
           )
          )))
          ))
          (begin (set-mcar! res-op-row (mcar new-op-row)) (set-mcdr! res-op-row (mcdr new-op-row))))

          ; захват камней противника
          (let ((last-house (+ pos (remainder seeds 13))))
            (cond ((and
                    (< last-house 6) ; последний камень положили в свою лунку
                    (= (mlist-ref res-my-row last-house) 1) ; последняя лунка была пустая
                    (> (mlist-ref res-op-row (- 5 last-house)) 0) ; противоположная лунка непустая
                   )
                   (set-mcar! (mlist-tail res-my-row 6) (+ (mlist-ref res-my-row 6)
                                                           (mlist-ref res-op-row (- 5 last-house))
                                                           1))
                   (set-mcar! (mlist-tail res-my-row last-house) 0)
                   (set-mcar! (mlist-tail res-op-row (- 5 last-house)) 0)
            ))
          )
          
          ; возвращение ответа: статус + новое состояние
          ; статусы:
          ;   'game-over - один из рядов опустел - заканчиваем игру
          ;   'my-turn - последний камень попал в калах - повтор хода
          ;   'op-turn - в любом другом случае передаем ход противнику
          (let ((my-sum (- (foldl + 0 (mlist->list res-my-row)) (mlist-ref res-my-row 6)))
                (op-sum (- (foldl + 0 (mlist->list res-op-row)) (mlist-ref res-op-row 6))))
            (cond
              ((or (= my-sum 0) (= op-sum 0))
               (set-mcar! res-my-row 0) (set-mcdr! res-my-row (mlist 0 0 0 0 0 (+ (mlist-ref res-my-row 6) my-sum)))
               (set-mcar! res-op-row 0) (set-mcdr! res-op-row (mlist 0 0 0 0 0 (+ (mlist-ref res-op-row 6) op-sum)))
               (values 'game-over res-S))
              ((= (+ pos (remainder seeds 13)) 6) (values 'my-turn res-S))
              (else (values 'op-turn res-S))
            )
          )
        )
      )
    )

    
  )
)

; класс-игрок
(define player%
  (class kalah%
    (super-new)

    (init-field name ; имя игрока
                my-row ; ряд игрока
                op-row ; ряд противника
    )
    (field [op 'undefined] ; класс - игрок-противник
           [board (new kalah-board% [my-row my-row] [op-row op-row])] ; доска
    ) 
    (inherit change-state)

    ; проверка собственного выигрыша
    (define/public (my-win?)
      (if (>
           (mlist-ref (get-field lst my-row) 6)
           (mlist-ref (get-field lst op-row) 6)
          ) #t #f
      )
    )

    ; проверка выигрыша противника 
    (define/public (op-win?)
      (if (<
           (mlist-ref (get-field lst my-row) 6)
           (mlist-ref (get-field lst op-row) 6)
          ) #t #f
      )
    )

    ; проверка ничьи
    (define/public (draw?)
      (if (=
           (mlist-ref (get-field lst my-row) 6)
           (mlist-ref (get-field lst op-row) 6)
          ) #t #f
      )
    )
  )
)

; класс-игрок, управляемый с клавиатуры
(define interactive-player%
  (class player%
    (super-new)

    (inherit-field name op board my-row op-row)
    (inherit change-state my-win? op-win? draw?)
    

    ; просит пользователя ввести число, пока подается "плохой" val
    ; завершает игру, если введено 'quit
    (define (process-input val)
      (cond
        [(eq? val 'quit) (kill-thread (current-thread))]
        [(not (and (number? val) (<= 1 val 6)))
         (println (text "Введите число от 1 до 6:" MESSAGE_STYLE))
         (process-input (read))]
        [(= (mlist-ref (get-field lst my-row) (sub1 val)) 0)
         (println (text "Выберите непустую лунку:" MESSAGE_STYLE))
         (process-input (read))]
        [else val]
      )
    )

    ; ход игрока
    ; изменения состояния доски и передача хода или окончание игры
    (define/public (make-move)
      (send board show-state)
      (println (text (~a "Ход " name) MESSAGE_STYLE))
      (let* ((val (read)))
        (define-values (ans new-S) (change-state (sub1 (process-input val)) board))
        (set-field! lst my-row (get-field lst (get-field my-row new-S)))
        (set-field! lst op-row (get-field lst (get-field op-row new-S)))
        (case ans
          [(my-turn) (begin (println (text (~a "Последний камень попал в калах! Снова ход " name "!") MESSAGE_STYLE))
                            (make-move))]
          [(op-turn) (send op make-move)]
          [(game-over)
           (begin (send board show-state) (print (text "Игра окончена! " MESSAGE_STYLE))
                  (println (text (cond [(my-win?) (~a "Победа " name "!")]
                                       [(op-win?) (~a "Победа " (get-field name op) "!")]
                                       [else "Ничья!"]) MESSAGE_STYLE)))]
          [else (error 'make-move "invalid status: ~a\n" ans)]
        )
      )
    )
  )
)


; доски в разных состояниях
(define new-board (new kalah-board%)) ; начальное состояние
(define capture-check-board (new kalah-board% [op-row (new kalah-row% [lst (mlist 0 3 0 5 0 0 7)])] [my-row (new kalah-row% [lst (mlist 0 1 0 0 0 1 8)])])) ; демонстрация захвата
(define pre-win-board (new kalah-board% [op-row (new kalah-row% [lst (mlist 0 3 0 5 0 0 7)])] [my-row (new kalah-row% [lst (mlist 0 0 0 0 0 1 8)])])) ; демонстрация выигрыша
(define pre-draw-board (new kalah-board% [op-row (new kalah-row% [lst (mlist 0 1 2 3 4 0 1)])] [my-row (new kalah-row% [lst (mlist 0 0 0 0 0 1 10)])])) ; демонстрация ничьей

; задание запускаемой игры
; init-board - изначальное состояние доски (new-board | apture-check-board | pre-win-board | pre-draw-board)
; player-A-class, player-B-class - название класса создаваемых игроков (interactive)
(define (start-game init-board player-A-class player-B-class)
  (define A (case player-A-class
    [(interactive) 
     (new interactive-player% [name "A"] [my-row (get-field my-row init-board)] [op-row (get-field op-row init-board)])]
    [else (error 'start-game "INVALID player-A-class")]
  ))
  (define B (case player-B-class
    [(interactive)
     (new interactive-player% [name "B"] [my-row (get-field op-row init-board)] [op-row (get-field my-row init-board)])]
    [else (error 'start-game "INVALID player-B-class")]
  ))
  (set-field! op A B)
  (set-field! op B A)
  (thread-wait (thread (lambda () (send A make-move)))) ; создаем нить, чтобы по (exit) интерпретатор "не замораживался"
)

; Примеры запуска игры
; (start-game new-board 'interactive 'interactive) ; начальное состояние
; (start-game capture-check-board 'interactive 'interactive) ; демонстрация захвата
; (start-game pre-win-board 'interactive 'interactive) ; демонстрация выигрыша
; (start-game pre-draw-board 'interactive 'interactive) ; демонстрация ничьей