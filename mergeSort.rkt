;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname mergeSort) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;hw#5 :  1.29 merge sort hw

;merge step function
(define merge
  (lambda (loi1 loi2)
    (cond
      ((null? loi1) (if (null? loi2) ;if first list is empty, check second list
                              '() ;resolve
                              (cons (car loi2) (merge loi1 (cdr loi2))))) ;add first element of 2nd list to final list
      ((null? loi2) (cons (car loi1) (merge (cdr loi1) loi2))) ;already know first list is empty, if 2nd list is empty cons using fisrt list
      ((< (car loi1) (car loi2)) (cons (car loi1) (merge (cdr loi1) loi2))) ;loi1 < loi2, cons using list 1
      (else (cons (car loi2) (merge loi1 (cdr loi2))))))) ;loi1 must be greater than loi2, cons using loi2

;actual merge sort
(define mergesort
  (lambda (loi)
    (if (null? (cdr loi))
        loi ;trivially sorted
        (merge (mergesort (cons (car loi) '())) (mergesort (cdr loi))))))

(mergesort '(8 2 5 2 3))
(mergesort '(10 2 5 7 2 9 1))
(mergesort '(10 9 8 7 6 5 4 3 2 1 0))
;(merge '(1 5) '(2 4 8))