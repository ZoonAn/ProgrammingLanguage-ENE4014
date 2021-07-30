#lang racket
(provide (all-defined-out))

(define (check_bst x)
      (local
        {(define (findMax a)
           (if (empty? (caddr a)) (car a) (findMax(caddr a))))
         (define (findMin b)
           (if (empty? (cadr b)) (car b) (findMin(cadr b))))
         (define x-val (car x))
         (define x-l (cadr x))
         (define x-r (caddr x))}
        (cond [(and (empty? x-l) (empty? x-r)) #t]
              [(empty? x-l) (if (< x-val (findMin x-r)) (check_bst x-r) #f)]
              [(empty? x-r) (if (> x-val (findMax x-l)) (check_bst x-l) #f)]
              [else (if (and (> x-val (findMax x-l))
                             (< x-val (findMin x-r)))
                        (and (check_bst x-r) (check_bst x-l))
                        #f)])))

(define (apply f x)
  (local
    {(define x-val (car x))
     (define x-l (cadr x))
     (define x-r (caddr x))}
    (cond [(and (empty? x-l) (empty? x-r)) (list (f x-val) null null)]
          [(empty? x-l) (list (f x-val) null (apply f x-r))]
          [(empty? x-r) (list (f x-val) (apply f x-l) null)]
          [else (list (f x-val) (apply f x-l) (apply f x-r))])))

(define (equals x y)
  (local
    {(define (bst-find bst v)
       (cond [(= v (car bst)) #t]
             [(< v (car bst)) (if (empty? (cadr bst)) #f (bst-find (cadr bst) v))]
             [(> v (car bst)) (if (empty? (caddr bst)) #f (bst-find (caddr bst) v))]))
     (define (equals-helper bst1 bst2)
       (if (empty? bst1) #t
       (and (equals-helper (cadr bst1) bst2)
            (bst-find bst2 (car bst1))
            (equals-helper (caddr bst1) bst2))))}
    (and (equals-helper x y) (equals-helper y x))))
