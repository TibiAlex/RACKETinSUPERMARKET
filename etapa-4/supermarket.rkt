#lang racket
(require racket/match)
(require "queue.rkt")

(provide (all-defined-out))

(define ITEMS 5)


; TODO
; Aveți libertatea să vă structurați programul cum doriți (dar cu restricțiile
; de mai jos), astfel încât funcția serve să funcționeze conform specificației.
; 
; Restricții (impuse de checker):
; - trebuie să existe în continuare funcția (empty-counter index)
; - cozile de la case trebuie implementate folosind noul TDA queue
(define-struct counter (index tt et queue) #:transparent)

(define (empty-counter index)
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
      (if (and (null? (queue-right (counter-queue C))) (stream-empty? (queue-left (counter-queue C))))
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

;;Implementați o funcție care calculează starea unei case după un număr dat de minute.
(define (pass-time-through-counter minutes)
  (λ (C)
    (struct-copy counter C [index (counter-index C)]
                              [tt (if (> minutes (counter-tt C))
                                       0
                                       (- (counter-tt C) minutes))]
                              [et (if (> minutes (counter-et C))
                                       0
                                       (- (counter-et C) minutes))])))

(define (remove-first-from-counter C)   ; testată de checker
  (cond
    ((= (+ (queue-size-l (counter-queue C)) (queue-size-r (counter-queue C))) 1)
        (struct-copy counter C [index (counter-index C)]
                              [tt 0]
                              [et 0]
                              [queue (dequeue (counter-queue C))]))
    ((= (+ (queue-size-l (counter-queue C)) (queue-size-r (counter-queue C))) 0)
         C)
    (else
        (struct-copy counter C [index (counter-index C)]
                              [tt (- (counter-tt C) (counter-et C))]
                              [et (cdr (stream-first (queue-left (dequeue(counter-queue C)))))]
                              [queue (dequeue (counter-queue C))]))))
  
  
; TODO
; Implementați funcția care simulează fluxul clienților pe la case.
; ATENȚIE: Față de etapa 3, apare un nou tip de cerere, așadar
; requests conține 5 tipuri de cereri (cele moștenite din etapa 3 plus una nouă):
;   - (<name> <n-items>) - persoana <name> trebuie așezată la coadă la o casă              (ca înainte)
;   - (delay <index> <minutes>) - casa <index> este întârziată cu <minutes> minute         (ca înainte)
;   - (ensure <average>) - cât timp tt-ul mediu al caselor este mai mare decât
;                          <average>, se adaugă case fără restricții (case slow)           (ca înainte)
;   - <x> - trec <x> minute de la ultima cerere, iar starea caselor se actualizează
;           corespunzător (cu efect asupra câmpurilor tt, et, queue)                       (ca înainte)
;   - (close <index>) - casa index este închisă                                            (   NOU!   )
; Sistemul trebuie să proceseze cele 5 tipuri de cereri în ordine, astfel:
; - persoanele vor fi distribuite la casele DESCHISE cu tt minim; nu se va întâmpla
;   niciodată ca o persoană să nu poată fi distribuită la nicio casă                       (mică modificare)
; - când o casă suferă o întârziere, tt-ul și et-ul ei cresc (chiar dacă nu are clienți);
;   nu aplicați vreun tratament special caselor închise                                    (ca înainte)
; - tt-ul mediu (ttmed) se calculează pentru toate casele DESCHISE, 
;   iar la nevoie veți adăuga case slow una câte una, până când ttmed <= <average>         (mică modificare)
; - când timpul prin sistem avansează cu <x> minute, tt-ul, et-ul și queue-ul tuturor 
;   caselor se actualizează pentru a reflecta trecerea timpului; dacă unul sau mai mulți 
;   clienți termină de stat la coadă, ieșirile lor sunt contorizate în ordine cronologică. (ca înainte)
; - când o casă se închide, ea nu mai primește clienți noi; clienții care erau deja acolo
;   avansează normal, până la ieșirea din supermarket                                    
; Rezultatul funcției serve va fi o pereche cu punct între:
; - lista sortată cronologic a clienților care au părăsit supermarketul:
;   - fiecare element din listă va avea forma (index_casă . nume)
;   - dacă mai mulți clienți ies simultan, sortați-i crescător după indexul casei
; - lista cozilor (de la case DESCHISE sau ÎNCHISE) care încă au clienți:
;   - fiecare element va avea forma (index_casă . coadă) (coada este de tip queue)
;   - lista este sortată după indexul casei

(define (serve requests fast-counters slow-counters)
  'your-code-here)