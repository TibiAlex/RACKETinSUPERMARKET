#lang racket
(require racket/match)
(require "queue.rkt")

(provide (all-defined-out))

(define ITEMS 5)

;; ATENȚIE: Pentru această etapă a temei este necesar să implementați
;;          întâi TDA-ul queue în fișierul queue.rkt.
;; Reveniți la sarcinile din acest fișier după ce ați implementat tipul 
;; queue și ați verificat implementarea folosind checker-ul.


; Structura counter nu se modifică.
; Ceea ce se modifică este implementarea câmpului queue:
; - în loc de listă, acesta va fi o coadă (o structură de tip queue)
; - acest lucru nu este vizibil în definiția structurii counter,
;   ci în implementarea operațiilor acestui tip de date (tipul counter)
(define-struct counter (index tt et queue) #:transparent)


; TODO
; Actualizați funcțiile de mai jos astfel încât ele să folosească
; o structură de tip queue pentru reprezentarea cozii de persoane.
; Elementele cozii continuă să fie perechi (nume . nr_produse).
; Este esențial să respectați "bariera de abstractizare", adică să
; operați cu coada doar folosind interfața acestui TDA:
; - empty-queue
; - queue-empty?
; - enqueue
; - dequeue
; - top
; Obs: Doar câteva funcții vor necesita actualizări.
(define (empty-counter index)           ; testată de checker
  (make-counter index 0 0 empty-queue))

(define (update f counters index)
  (reverse(foldl (λ(x acc) (if (= (counter-index x) index) (cons (f x) acc) (cons x acc))) '() counters)))

(define tt+
  (λ (minutes)
    (λ (C)
  (define Cnou (struct-copy counter C [index (counter-index C)] [tt (+ (counter-tt C) minutes)]))
  Cnou)))

(define et+
  (λ (minutes)
    (λ (C)
  (define Cnou (struct-copy counter C [index (counter-index C)] [et (+ (counter-et C) minutes)]))
  Cnou)))

(define (add-to-counter name items)     ; testată de checker
  (λ (C)                                ; nu modificați felul în care funcția își primește argumentele
      (if (null? (append (queue-left (counter-queue C)) (queue-right (counter-queue C))))
               (struct-copy counter C [index (counter-index C)]
                            [tt (+ (counter-tt C) items)]
                            [et (+ (counter-et C) items)]
                            [queue (enqueue (cons name items) (counter-queue C))])
               (struct-copy counter C [index (counter-index C)]
                            [tt (+ (counter-tt C) items)]
                            [et (+ (counter-et C) 0)]
                            [queue (enqueue (cons name items) (counter-queue C))])
          )))

(define (min-general counters f)
     (foldl (λ (x acc) (if (< (f x) (cdr acc))
                           (cons (counter-index x) (f x))
                           acc))
            (cons (counter-index (car counters)) (f (car counters))) counters))

;(define min-tt 'your-code-here) ; folosind funcția de mai sus
(define min-tt
  (λ (counters)
   (min-general counters (λ (counter) (counter-tt counter))) 
    ))

;(define min-et 'your-code-here) ; folosind funcția de mai sus
(define min-et
   (λ (counters)
        (min-general counters (λ (counter) (counter-et counter))) 
     ))


(define (remove-first-from-counter C)   ; testată de checker
  (cond ((= (length (append(queue-left(counter-queue C)) (queue-right(counter-queue C)))) 1)
        (struct-copy counter C [index (counter-index C)]
                              [tt 0]
                              [et 0]
                              [queue (dequeue (counter-queue C))]))
        ((= (length (append(queue-left(counter-queue C)) (queue-right(counter-queue C)))) 0)
         C)
        (else
         (struct-copy counter C [index (counter-index C)]
                              [tt (- (counter-tt C) (counter-et C))]
                              [et (if (= (length (append (queue-left(dequeue (counter-queue C))) (queue-right(dequeue (counter-queue C))))) 0)
                                         0
                                        (cdr(car(queue-left (dequeue(counter-queue C))))))]
                              [queue (dequeue (counter-queue C))]))))

; TODO
; Implementați o funcție care calculează starea unei case după un număr dat de minute.
; Funcția presupune, fără să verifice, că în acest timp nu a ieșit nimeni din coadă, 
; deci va avea efect doar asupra câmpurilor tt și et.
; (cu alte cuvinte, este responsabilitatea utilizatorului să nu apeleze această funcție
; cu minutes > timpul până la ieșirea primului client din coadă)
; Atenție: casele fără clienți nu trebuie să ajungă la timpi negativi!
(define (pass-time-through-counter minutes)
  (λ (C)
    (struct-copy counter C [index (counter-index C)]
                              [tt (if (> minutes (counter-tt C))
                                       0
                                       (- (counter-tt C) minutes))]
                              [et (if (> minutes (counter-et C))
                                       0
                                       (- (counter-et C) minutes))])))
  

; TODO
; Implementați funcția care simulează fluxul clienților pe la case.
; ATENȚIE: Față de etapa 2, apar modificări în:
; - formatul listei de cereri (parametrul requests)
; - formatul rezultatului funcției (explicat mai jos)
; requests conține 4 tipuri de cereri (3 moștenite din etapa 2 plus una nouă):
;   - (<name> <n-items>) - persoana <name> trebuie așezată la coadă la o casă            (ca înainte)
;   - (delay <index> <minutes>) - casa <index> este întârziată cu <minutes> minute       (ca înainte)
;   - (ensure <average>) - cât timp tt-ul mediu al tuturor caselor este mai mare decât
;                          <average>, se adaugă case fără restricții (case slow)         (ca înainte)
;   - <x> - trec <x> minute de la ultima cerere, iar starea caselor se actualizează
;           corespunzător (cu efect asupra câmpurilor tt, et, queue)                     (   NOU!   )
; Obs: Au dispărut cererile de tip remove-first, pentru a lăsa loc unui mecanism mai 
; sofisticat de a scoate clienții din coadă (pe măsură ce trece timpul).
; Sistemul trebuie să proceseze cele 4 tipuri de cereri în ordine, astfel:
; - persoanele vor fi distribuite la casele cu tt minim (dintre casele la care au voie)  (ca înainte)
; - când o casă suferă o întârziere, tt-ul și et-ul ei cresc (chiar dacă nu are clienți) (ca înainte)
; - tt-ul mediu (ttmed) se calculează pentru toate casele (și cele fast, și cele slow), 
;   iar la nevoie veți adăuga case slow una câte una, până când ttmed <= <average>       (ca înainte)
; - când timpul prin sistem avansează cu <x> minute, tt-ul, et-ul și queue-ul tuturor 
;   caselor se actualizează pentru a reflecta trecerea timpului; dacă unul sau mai mulți 
;   clienți termină de stat la coadă, ieșirile lor sunt contorizate în ordine cronologică.
; Rezultatul funcției serve va fi o pereche cu punct între:
; - lista sortată cronologic a clienților care au părăsit supermarketul
;   - fiecare element din listă va avea forma (index_casă . nume)
;   - dacă mai mulți clienți ies simultan, sortați-i crescător după indexul casei
; - lista caselor în starea finală (ca rezultatul din etapele 1 și 2)
; Obs: Pentru a contoriza ieșirile din cozi, puteți să lucrați într-o funcție ajutătoare
; (cu un parametru în plus față de funcția serve), pe care serve doar o apelează.

(define (add-counter average fast-counters slow-counters)
  (if (>= average (/ (foldr (λ(x sum-tt) (+ sum-tt (counter-tt x))) 0 (append fast-counters slow-counters))
                             (length(append fast-counters slow-counters))))
      slow-counters
      (add-counter average fast-counters (append slow-counters (list(empty-counter (+ (length(append fast-counters slow-counters)) 1))))))
  )

(define (update-counters minutes fast-counters slow-counters clients)
  (if (= minutes 0)
      (list fast-counters slow-counters clients)
      (update-counters (- minutes 1) (foldl (λ (x acc) (if (queue-empty? (counter-queue x))
                                                                   (append acc (list((pass-time-through-counter 1)x)))
                                                                   (if (= (counter-et x) 1)
                                                                      (append acc (list(remove-first-from-counter x)))
                                                                      (append acc (list((pass-time-through-counter 1) x))))))
                                                       '() fast-counters)
                                     (foldl (λ (x acc) (if (queue-empty? (counter-queue x))
                                                                   (append acc (list((pass-time-through-counter 1)x)))
                                                                   (if (= (counter-et x) 1)
                                                                      (append acc (list(remove-first-from-counter x)))
                                                                      (append acc (list((pass-time-through-counter 1)x))))))
                                                       '() slow-counters)
                                     (foldl (λ (x acc) (if (queue-empty? (counter-queue x))
                                                                   acc
                                                                   (if (= (counter-et x) 1)
                                                                       (append acc (list(cons (counter-index x) (car(top(counter-queue x))))))
                                                                        acc)))
                                                       clients (append fast-counters slow-counters)))
  ))

(define (process-requests requests fast-counters slow-counters clients)
  (if (null? requests)
           (cons clients (append fast-counters slow-counters))
           (match (car requests)
        
           [(list 'ensure average)
            (if (>= average (/ (foldr (λ(x sum-tt) (+ sum-tt (counter-tt x))) 0 (append fast-counters slow-counters))
                             (length(append fast-counters slow-counters))))
              (process-requests (cdr requests) fast-counters slow-counters clients)
              (process-requests (cdr requests) fast-counters (add-counter average fast-counters slow-counters) clients))]
        
           [(list name n-items)
            (if (>= ITEMS n-items)
             (if (<= (cdr (min-tt fast-counters)) (cdr (min-tt slow-counters)))
                 (process-requests (cdr requests) (update (add-to-counter name n-items) fast-counters (car (min-tt fast-counters))) slow-counters clients)
                 (process-requests (cdr requests) fast-counters (update (add-to-counter name n-items) slow-counters (car (min-tt slow-counters))) clients))
             (process-requests (cdr requests) fast-counters (update (add-to-counter name n-items) slow-counters (car (min-tt slow-counters))) clients))]
        
           [(list 'delay index minutes)
            (if (<= (counter-index (car slow-counters)) index)
             (process-requests (cdr requests) fast-counters (update (et+ minutes) (update (tt+ minutes) slow-counters index) index) clients)
             (process-requests (cdr requests) (update (et+ minutes) (update (tt+ minutes) fast-counters index) index) slow-counters clients))]
        
           [x
            (process-requests (cdr requests) (car (update-counters x fast-counters slow-counters clients))
                                             (car (cdr (update-counters x fast-counters slow-counters clients)))
                                             (car(cdr (cdr (update-counters x fast-counters slow-counters clients)))))
           ])
       ))


(define (serve requests fast-counters slow-counters)
        (process-requests requests fast-counters slow-counters '()))
