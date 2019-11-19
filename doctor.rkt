; заготовка "Доктора". Август 2019
#lang scheme/base
; В учебных целях используется базовая версия Scheme

; основная функция, запускающая "Доктора"
; Упражнение 5. Прием оканчивается либо по стоп-слову (stop-word),
; либо если обслужено максимальное количество пациентов (max-pat)
(define (visit-doctor stop-word max-pat)
  (let patient-changer ((pat-cnt 0))
    (if (= pat-cnt max-pat)
        (println '(time to go home))
        (let ((name (ask-patient-name)))
          (if (equal? name stop-word)
              (println '(time to go home))
              (begin
                (printf "Hello, ~a!\n" name)
                (println '(what seems to be the trouble?))
                (doctor-driver-loop name)
                (patient-changer (add1 pat-cnt))
              )
          )
        )
    )
  )
)

; Упражнение 5. Спрашивает имя пациента и возвращает его
(define (ask-patient-name)
  (begin
    (println '(next!))
    (println '(who are you?))
    (print '**)
    (car (read))
  ) 
)

; цикл диалога Доктора с пациентом
; параметр name -- имя пациента
; Упражнение 4. В user-responses сохраняются все реплики пациента, они передаются в reply
(define (doctor-driver-loop name)
  (let inner-loop ((user-responses '()))
    (print '**) ; доктор ждёт ввода реплики пациента, приглашением к которому является **
    (let ((user-response (read)))
      (cond 
        ((equal? user-response '(goodbye)) ; реплика '(goodbye) служит для выхода из цикла
         (printf "Goodbye, ~a!\n" name)
         (println '(see you next week)))
        (else
         (let ((new-responses (cons user-response user-responses)))
           (println (reply new-responses)) ; иначе Доктор генерирует ответ, печатает его и продолжает цикл
           (inner-loop new-responses)
         )
        )
      )
    )
  )
)

; генерация ответной реплики по user-response -- реплике от пользователя
; Упражнение 4. Добавлен 3й способ, применим ко всем репликам, кроме первой
; Упражнение 6. Добавлен 4й способ, применим только к репликам, содержащим ключевые слова
; Упражнение 7. Функция выбирает стратегию с помощью новой структуры данных
(define (reply user-responses)
  (let ((strategy (pick-random-with-weight (filter (lambda (el) ((list-ref el 2) user-responses)) strategy-info))))
    ((car strategy) user-responses)
  )
)
			
; 1й способ генерации ответной реплики -- замена лица в реплике пользователя и приписывание к результату нового начала
(define (qualifier-answer user-responses)
  (append (pick-random '((you seem to think that)
                         (you feel that)
                         (why do you believe that)
                         (why do you say that)
                         (it sounds like) ; Упражнение 1.
                         (i think that)
                         (the problem is that))
          )
          (change-person (car user-responses))
  )
)

; случайный выбор одного из элементов списка lst
(define (pick-random lst)
  (list-ref lst (random (length lst)))
)

; замена лица во фразе			
(define (change-person phrase)
  (many-replace-with-map '((am are)
                           (are am)
                           (i you)
                           (me you)
                           (mine yours)
                           (my your)
                           (myself yourself)
                           (you i)
                           (your my)
                           (yours mine)
                           (yourself myself))
                         phrase)
)
  
; осуществление всех замен в списке lst по ассоциативному списку replacement-pairs
(define (many-replace replacement-pairs lst)
  (cond
    ((null? lst) lst)
    (else
     (let ((pat-rep (assoc (car lst) replacement-pairs))) ; Доктор ищет первый элемент списка в ассоциативном списке замен
       (cons
        (if pat-rep
            (cadr pat-rep) ; если поиск был удачен, то в начало ответа Доктор пишет замену
            (car lst) ; иначе в начале ответа помещается начало списка без изменений
        )
        (many-replace replacement-pairs (cdr lst)) ; рекурсивно производятся замены в хвосте списка
       )
     )
    )
  )
)

; Упражнение 2. Эта же функция с хвостовой рекурсией
(define (many-replace-iter replacement-pairs lst)
  (let loop ((lst lst) (res '()))
    (if (null? lst)
        (reverse res)
        (let ((pat-rep (assoc (car lst) replacement-pairs))) 
          (loop (cdr lst)
                (cons
                 (if pat-rep
                     (cadr pat-rep)
                     (car lst) 
                 )
                 res
                )
          )
        )
    )
  )
)

; Упражнение 3. Эта же функция, реализованная через map
(define (many-replace-with-map replacement-pairs lst)
  (map
   (lambda (key)
     (let ((pat-rep (assoc key replacement-pairs)))
       (if pat-rep
           (cadr pat-rep)
           key
       )
     )
   )
   lst
  )
)

; 2й способ генерации ответной реплики -- случайный выбор одной из заготовленных фраз, не связанных с репликой пользователя
(define (hedge user-responses)
  (pick-random '((please go on)
                 (many people have the same sorts of feelings)
                 (many of my patients have told me the same thing)
                 (please continue)
                 (the reason for it may be found in your childhood) ; Упражнение 1.
                 (why do you think so?)
                 (you remind me my first patient)
                )
  )
)

; 3й способ генерации ответной реплики -- возвращаемся к случайной фразе пользователя, сказанной ранее
(define (reminder user-responses)
  (append '(earlier you said that)
          (change-person (pick-random (cdr user-responses)))
  )
)

; Упражнение 6. -----------------------------------------------------------------------------------------

; Структура с ключевыми словами и ответными репликами для 4ой стратегии
(define keys-answers-map
  '( 
    ( ; начало данных 1й группы
     (depressed suicide exams university) ; список ключевых слов 1й группы
     ( ; список шаблонов для составления ответных реплик 1й группы 
      (when you feel depressed, go out for ice cream)
      (depression is a disease that can be treated)
     )
    ) ; завершение данных 1й группы
    ( ; начало данных 2й группы ...
     (mother father parents brother sister uncle aunt grandma grandpa)
     (
      (tell me more about your * , i want to know all about your *)
      (why do you feel that way about your * ?)
     )
    )
    (
     (university scheme lections)
     (
      (your education is important)
      (how many time do you spend to learning ?)
     )
    )
    (
     (husband wife love girlfriend boyfriend)
     (
      (* is always a problem)
      (relax ! take it easy !)
     )
    )
    (
     (son daughter kid child kids children)
     (
      (give your * more freedom)
      (you should accept it)
     )
    )
  )
)

; список ключевых слов
(define keyword-lst
  (foldl (lambda (x acc) (append (car x) acc)) '() keys-answers-map)
)

; проверяет, является ли word ключевым словом
(define (keyword? word)
  (not (boolean? (member word keyword-lst)))
)

; возвращает объединенный список шаблонов для ключевого слова keyword
(define (get-answers-by-keyword keyword)
  (foldl
   (lambda (x acc)
     (cond ((member keyword (car x)) (append (cadr x) acc))
           (else acc)
     )
   )
   '() keys-answers-map
  )
)

; проверяет, есть ли в реплике ключевые слова
(define (has-keywords? response)
  (ormap keyword? response)
)

; 4й способ генерации ответной реплики -- выявляем ключевые слова в реплике
(define (keyword-answer user-responses)
  (let ((keyword (pick-random (filter keyword? (car user-responses)))))
    (many-replace-with-map (list (list '* keyword)) (pick-random (get-answers-by-keyword keyword)))
  )
)

; ----------------------------------------------------------------------------------------- Упражнение 6.

; Упражнение 7. -----------------------------------------------------------------------------------------

; Структура с информацией о стратегиях генерации ответа
(define strategy-info
  (list
    (list hedge 1 (lambda (user-responses) #t))
    (list qualifier-answer 2 (lambda (user-responses) #t))
    (list reminder 3 (lambda (user-responses) (not (null? (cdr user-responses)))))
    (list keyword-answer 4 (lambda (user-responses) (has-keywords? (car user-responses))))
  )
)

; случайный выбор одного из элементов списка lst
(define (pick-random-with-weight lst)
  (let ((sum (foldl (lambda (x acc) (+ (cadr x) acc)) 0 lst)))
    (let loop ((sum sum) (lst lst) (rand-val (random sum)))
      (cond ((< rand-val (cadar lst)) (car lst))
            (else (loop (- sum (cadar lst)) (cdr lst) rand-val))
      )
    )
  )
)

; ----------------------------------------------------------------------------------------- Упражнение 7.

