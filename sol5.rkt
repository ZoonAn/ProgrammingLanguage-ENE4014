#lang racket
(provide (all-defined-out)) ;; exports the defined variables in this file.

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; Extra requirements
(struct glet (var e body) #:transparent) ;; a global binding that overrides any local binding (let var = e in body)

(struct num-array  (size) #:transparent)  ;; a number array  (initialized to zeroes), e.g., (num-array-var 10)
                                                     ;; e.g. (num-array 4)

(struct num-array-at   (e1 e2) #:transparent) ;; e1 evaluates to num-array and e2 evaluates to racket int (index of the value to access) index starts from 0
                                              ;; (num-array-at (num-array 4) 3)
                                              ;; (num-array-at (num-array 4) 4) ;  this should give a nice error messaeg (like "array access out of bound")
                                              ;; (num-array-at (num-array 4) -1) ;  this should give a nice error messaeg (like "array access out of bound")

(struct num-array-set  (e1 e2 e3) #:transparent) ;; e1 evaluates to num-array-var, e2 evaluates to racket int (index of the value to access), and e3 evaluates to a MUPL int
                                              ;; (num-array-set (num-array 4) 0 (int 42))
                                              ;; (num-array-set (num-array 4) 5 (int 42)) ; this should give a nice error messaeg (like "array access out of bound")
                                              ;; (num-array-set (num-array 4) -1 (int 42)) ; this should give a nice error messaeg (like "array access out of bound")


;; a closure is not in "source" programs; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

(define (num-array-object? v) ;; hackish implementation of testing num-array object. We assume that if a value is mpair, it is a num-array object.
  (mpair? v))

(define (array-length array)
  (if (eq? (mcdr array) null)
      1
      (+ 1 (array-length (mcdr array)))))

(define (make-array-object length)  
    (if (= length 0)
        null
        (mcons (int 0) (make-array-object (- length 1)))))

(define (make-array length)
    (if (= length 0)
        null
        (mcons (int 0) (make-array (- length 1)))))

(define (set-array-val array index val)
  (if (= index 0)
      (set-mcar! array val)
      (set-array-val (mcdr array) (- index 1) val)))
;; Problem 1

;; CHANGE (put your solutions here)

(define (racketlist->mupllist rls)
  (if (null? rls) (aunit) (apair (first rls) (racketlist->mupllist (list-tail rls 1)))))

(define (mupllist->racketlist mls)
  (cond [(aunit? mls) '()]
        [(apair? mls) (cons (apair-e1 mls) (mupllist->racketlist (apair-e2 mls)))]))

;; Problem 2

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        ;; CHANGE add more cases here

        [(int? e) e]
        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? v1) (int? v2))
               (cond [(> (int-num v1) (int-num v2)) (eval-under-env (ifgreater-e3 e) env)]
                     [#t (eval-under-env (ifgreater-e4 e) env)])
               (error "MUPL ifgreater applied to non-number")))]
        [(fun? e) (closure env e)]
        [(call? e)
         (let ([v1 (eval-under-env (call-funexp e) env)]
               [v2 (eval-under-env (call-actual e) env)])
           (if (closure? v1)
               (eval-under-env (fun-body (closure-fun v1))
                               (append (closure-env v1)
                                       (list (cons (fun-formal (closure-fun v1)) v2)
                                             (if (fun-nameopt (closure-fun v1))
                                                 (cons (fun-nameopt (closure-fun v1)) v1)
                                                 '()))))
               (error "MUPL call applied to value which have first argument that is not a closure")))]
        [(mlet? e)
         (let ([v (eval-under-env (mlet-e e) env)])
           (eval-under-env (mlet-body e) (append (list (cons (mlet-var e) v)) env)))]
        [(apair? e)
         (let ([v1 (eval-under-env (apair-e1 e) env)]
               [v2 (eval-under-env (apair-e2 e) env)])
           (apair v1 v2))]
        [(fst? e)
         (let ([v (eval-under-env (fst-e e) env)])
           (if (apair? v) (apair-e1 v) (error "MUPL fst applied to value that is not a pair")))]
        [(snd? e)
         (let ([v (eval-under-env (snd-e e) env)])
           (if (apair? v) (apair-e2 v) (error "MUPL snd applied to  value that is not a pair")))]       
        [(aunit? e) e]
        [(isaunit? e)
         (let ([v (eval-under-env (isaunit-e e) env)])
           (if (aunit? v) (int 1) (int 0)))]
        [(closure? e) e]

        ;;Extra Requirements
        [(glet? e)
         (let ([v (eval-under-env (glet-e e) env)])
           (local
             {(define (glet-helper enviro var chval)
               (cond
                 [(null? enviro) (list (cons (glet-var e) v))]
                 [(equal? var (car (car enviro))) (cons (cons (car (car enviro)) chval) (cdr enviro))]
                 [#t (cons (glet-closurechanger (car enviro) var chval) (glet-helper (cdr enviro) var chval))]))
              (define (glet-closurechanger p var chval)
                (cond
                  [(closure? (cdr p)) (cons (car p) (closure (glet-helper (closure-env (cdr p)) var chval) (closure-fun (cdr p))))]))}
           
             (eval-under-env (glet-body e) (glet-helper env (glet-var e) v))))]

        [(num-array? e) (make-array-object (num-array-size e))]
        [(num-array-object? e) e]
        [(num-array-at? e)
         (let ([v1 (eval-under-env (num-array-at-e1 e) env)]
               [v2 (num-array-at-e2 e)])
           (cond
             [(not (num-array-object? v1)) (error "num-array-at applied to a non-array")]
             [(or (< v2 0) (>= v2 (array-length v1))) (error "array access out of bound")]
             [(equal? v2 0) (mcar v1)]
             [#t (eval-under-env (num-array-at (mcdr v1) (- v2 1)) env)]))]
        [(num-array-set? e)
         (let ([v1 (eval-under-env (num-array-set-e1 e) env)]
               [v2 (num-array-set-e2 e)]
               [v3 (eval-under-env (num-array-set-e3 e) env)])
           (cond
             [(not (num-array-object? v1)) (error "num-array-at applied to a non-array")]
             [(or (< v2 0) (>= v2 (array-length v1))) (error "array access out of bound")]
             [#t (set-array-val v1 v2 v3) (eval-under-env (num-array-at v1 v2) env)]))]
        
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifaunit e1 e2 e3)
  (ifgreater (isaunit e1) (int 0) e2 e3))

(define (mlet* lst e2)
  (if (null? lst) e2
      (let ([v (first lst)])
        mlet (car v) (cdr v) (mlet* (cdr lst) e2))))

(define (ifeq e1 e2 e3 e4)
  (mlet* (list (cons "_x" e1) (cons "_y" e2))
         (ifgreater (var "_x") (var "_y") e4 (ifgreater (var "_y") (var "_x") e4 e3))))

;; Problem 4

(define mupl-map
  (fun "mupl-map-fun" "mupl-map-fn"
       (fun "mupl-map-lst" "mupl-map-ls"
            (ifeq (isaunit (var "lst")) (int 1)
                  (aunit)
                  (apair (call (var "mupl-map-fn") (fst (var "mupl-map-1s")))
                         (call (var "mupl-map-lst") (snd (var "mupl-map-1s"))))))))

(define mupl-mapAddN 
  (mlet "mupl-mapAddN-map" mupl-map
        (fun "mupl-mapAddN-fun-int" "mupl-mapAddN-i"
             (fun "mupl-mapAddN-list" "mupl-mapAddN-int"
                  (call (call (var "mupl-mapAddN-map")
                              (fun "mupl-mapAddN-addi" "mupl-mapAddN-x" (add (var "mupl-mapAddN-x") (var "mupl-mapAddN-i"))))
                        (var "mupl-mapAddN-int"))))))