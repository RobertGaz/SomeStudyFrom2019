; заготовка "Доктора". Август 2019
#lang scheme/base

#lang scheme/base
;Шаблон В. Малышко

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
             
            (else (print (reply user-response previous)) ; иначе Доктор генерирует ответ, печатает его и продолжает цикл 
                  (doctor-driver-loop name (cons user-response previous))
             )
       )
      )
)

; генерация ответной реплики по user-response -- реплике от пользователя 
(define (reply user-response previous)

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
(define (qualifier-answer user-response)
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
(define (hedge)
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
(define (history-answer previous)
  (append '(earlier you said that) (change-person (pick-random previous)))
)

; 4я стратегия - выдача реплики в зависимости от ключевого слова
(define (keyword-answer replic)
  (let ((keyword (pick-random (filter (lambda (elem) (member elem keylist)) replic))))
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
  (foldl (lambda (elem result) (append result (cond ((member word (car elem)) (cadr elem)) (else '())))) '() structure))(шаблон В. Малышко)

; основная функция, запускающая "Доктора"
; параметр name -- имя пациента
(define (visit-doctor stopword max_clients)
  (cond ((= max_clients 0)
          (print '(i am worked out)))
  (else      
  (let ((name (ask-patient-name))) 
    (cond ((equal? name stopword)
         (print '(i need to sleep)))
    (else
         (printf "Hello, ~a!\n" name)
         (print '(what seems to be the trouble?))
         (doctor-driver-loop name '())
         (visit-doctor name (- max_clients 1))))))
  
  ))

(define (ask-patient-name)
  (print '(Next!))
  (newline)
  (print '(Who are you?))
  (newline)
  (car (read)))
  
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
             
            (else (print (reply user-response previous)) ; иначе Доктор генерирует ответ, печатает его и продолжает цикл 
                  (doctor-driver-loop name (cons user-response previous))
             )
       )
      )
)

; генерация ответной реплики по user-response -- реплике от пользователя 
(define (reply user-response previous)

  (let choose () (case (random 3) 
               ((0) (qualifier-answer user-response)) 
               ((1) (hedge))
               ((2) (if (null? previous) (choose) (history-answer previous)))     
              )
  )

)
			
; 1й способ генерации ответной реплики -- замена лица в реплике пользователя и приписывание к результату нового начала
(define (qualifier-answer user-response)
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
(define (hedge)
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
(define (history-answer previous)
  (append '(earlier you said that) (change-person (pick-random previous))))
