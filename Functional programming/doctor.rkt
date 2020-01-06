; заготовка "Доктора". Август 2019
; Шаблон Виктора Малышко

#lang scheme/base 


; структура шаблонов для ключевых слов
(define structure
  '( 
  (
    (depressed suicide exams university)
    ( 
	  (when you feel depressed, go out for ice cream)
          (depression is a disease that can be treated)
          (i can help you in mental problems)
          (speaking about * will make everything easier)
	)
  ) 
  ( 
    (mother father parents brother sister uncle ant grandma grandpa)
    (
	  (tell me more about your * , i want to know all about your *)
          (why do you feel that way about your * ?)
          (yeah. i have interesting stories about my * too)
          (all this family stuff is always funny)
          )
  )
  (
    (university school lections)
	(
	  (your education is important)
	  (how many time do you spend to learning ?)
          (i never liked to go to *)
          (oh i remember the time i was a student...)
	)
  )
  (
    (girlfriend boyfriend love wife husband)
	(
	  (how much time are you together?)
	  (it is always interesting to listen about relationships)
          (there was a time it was difficult for me to talk about my *)
	)
  )
  (
    (work job colleague office corporation)
	(
	  (ooh, where do you work)
	  (tell me some facts about your *)
          (what is your profession?)
	)
  )
    ))

; список всех ключевых слов
(define keylist
   (let loop ((left structure) (result '()))
     (cond ((null? left) result)
           (else (loop (cdr left) (append result  (filter (lambda(elem) (not (member elem result))) (caar left)))))))
)


; основная функция, запускающая "Доктора"
; параметр name -- имя пациента
(define (visit-doctor stopword max_clients)
  (let loop ((clients_left max_clients)) 
          (cond ((= clients_left 0)
           (print '(i am worked out)))
          (else      
           (let ((name (ask-patient-name))) 
             (cond ((equal? name stopword)
                    (print '(i need to sleep)))
                   (else
                    (printf "Hello, ~a!\n" name)
                    (print '(what seems to be the trouble?))
                    (doctor-driver-loop name '())
                    (loop (- clients_left 1)))))))
  )
)

(define (ask-patient-name)
  (print '(Next!))
  (newline)
  (print '(Who are you?))
  (newline)
  (car (read))
)
  
; цикл диалога Доктора с пациентом
; параметр name -- имя пациента
(define (doctor-driver-loop name previous)
    (newline)
    (print '**) ; доктор ждёт ввода реплики пациента, приглашением к которому является **
    (let ((user-response (read)))
      (cond ((equal? user-response '(goodbye)) ; реплика '(goodbye) служит для выхода из цикла
             (printf "Goodbye, ~a!\n" name)
             (print '(see you next week))
             (newline)
             (newline))
             
            (else (print (reply user-response previous strategies)) ; иначе Доктор генерирует ответ, печатает его и продолжает цикл 
                  (doctor-driver-loop name (cons user-response previous))
             )
       )
      )
)

; Обобщённый reply
; генерация ответной реплики (user-response -- реплика от пользователя, previous - список прошлых реплик, strategies - список стратегий)
(define (reply user-response previous strategies)
  ((pick-random-with-weight (filter (lambda (elem) ((car elem) user-response previous)) strategies)) user-response previous)
)

; Выбрать случайный элемент с учётом веса из списка специального формата (strategies)
(define (pick-random-with-weight lst)
  (let loop ((cur_number (+ (random (foldl (lambda (elem result) (+ result (cadr elem))) 0 lst)) 1)) (left lst))
    (if (<= cur_number (cadar left)) (caddar left) (loop (- cur_number (cadar left)) (cdr left))))
)


; генерация ответной реплики по user-response -- реплике от пользователя (СТАРАЯ ВЕРСИЯ)
(define (reply_old user-response previous)

  (let ((has_keys (have_keys user-response))) 
        (let choose () (case (random 4) 
                         ((0) (qualifier-answer user-response)) 
                         ((1) (hedge))
                         ((2) (if (null? previous) (choose) (history-answer previous)))   
                         ((3) (if has_keys (keyword-answer user-response) (choose)))     
                        )
        )
   )
)

; 1й способ генерации ответной реплики -- замена лица в реплике пользователя и приписывание к результату нового начала
(define (qualifier-answer user-response previous)
        (append (pick-random '((you seem to think that)
                               (you feel that)
                               (why do you believe that)
                               (why do you say that)
                               
                               (oh, i understand you in position that)
                               (i know that feeling when)
                               (when did you find that)
                               )
                )
                (change-person user-response)
        )
 )

; случайный выбор одного из элементов списка lst
(define (pick-random lst)
  (list-ref lst (random (length lst)))
)


; замена лица во фразе			
(define (change-person phrase)
        (many-replace '((am are)
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
  (map (lambda (word) (let ((replac (assoc word replacement-pairs))) (if replac (cadr replac) word))) lst))


; 2й способ генерации ответной реплики -- случайный выбор одной из заготовленных фраз, не связанных с репликой пользователя
(define (hedge user-response previous)
       (pick-random '((please go on)
                       (many people have the same sorts of feelings)
                       (many of my patients have told me the same thing)
                       (please continue)
                       
                       (don't stop telling me)
                       (a lot of people have such situation)
                       (don't worry)
                       (it's okay)
                       )
         )
)

; 3я стратегия - случайный выбор одной из предыдущих фраз пользователя  
(define (history-answer user-response previous)
  (append '(earlier you said that) (change-person (pick-random previous)))
)

; 4я стратегия - выдача реплики в зависимости от ключевого слова
(define (keyword-answer user-response previous)
  (let ((keyword (pick-random (filter (lambda (elem) (member elem keylist)) user-response))))
    (map (lambda (elem) (if (equal? elem '*) keyword elem)) (pick-random (templates keyword)))
  )
)
  
  
; есть ли в списке lst хотя бы одно ключевое слово из keylist
(define (have_keys lst)
  (let loop ((left lst))
    (cond ((null? left) #f)
          ((member (car left) keylist) #t)
          (else (loop (cdr left))))))

; получить список шаблонов для слова word
(define (templates word)
  (let loop ((left structure) (result '()))
    (cond ((null? left) result)
          (else (loop (cdr left) (append result (cond ((member word (caar left)) (cadar left)) (else '()))))))))



; функции-предикаты для разных стратегий
(define (true_predicat user-response previous) #t)
(define (history_predicat user-response previous) (if (null? previous) #f #t))
(define (keyword_predicat user-response previous) (have_keys user-response))

; структура, хранящая стратегии ответа на реплики пользователя
; для каждой стратегии здесь список из предиката, веса, и функции построения реплики 
(define strategies
  (list
    (list true_predicat 1 qualifier-answer)
    (list true_predicat 1 hedge)
    (list history_predicat 2 history-answer)
    (list keyword_predicat 3 keyword-answer)
   )
)


