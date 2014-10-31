#lang racket

;; helpers
(define ns (make-base-namespace))
;;

;; machine
(struct ev (e ρ σ κ Ξ) #:transparent)
(struct ko (φ v σ κ Ξ) #:transparent)
(struct ctx (f ρ σ) #:transparent)
(struct letk (a es ρ) #:transparent)
(struct setk (x ρ) #:transparent)
(struct ifk (e1 e2 ρ) #:transparent)
(struct seqk (es ρ) #:transparent)
(struct randk (xs vs ρ) #:transparent)
(struct haltk () #:transparent)
(struct clo (λ ρ) #:transparent)
(struct lam (x es) #:transparent)

(define (make-step α γ ⊥ ⊔ alloc true? false?)
  (let* ((env-bind
          (lambda (ρ x a)
            (hash-set ρ x a)))
         (env-lookup
          (lambda (ρ x)
            (hash-ref ρ x)))
         (store-alloc
          (lambda (σ a v)
            (hash-set σ a (⊔ (hash-ref σ a ⊥) v))))
         (store-update
          (lambda (σ a v)
            (hash-set σ a (⊔ (hash-ref σ a) v))))
         (store-lookup
          (lambda (σ a)
            (hash-ref σ a)))
         (stack-alloc
          (lambda (Ξ τ κ)
            (hash-set Ξ τ (set-union (hash-ref Ξ τ (set)) (set κ)))))
         (stack-lookup
          (lambda (Ξ τ)
            (hash-ref Ξ τ)))
         (eval-seq
          (lambda (es ρ σ κ Ξ)
            (match es
              ((list e) (set (ev e ρ σ κ Ξ)))
              ((and (cons e0 es) e)
               (let ((τ (ctx e ρ σ)))
                 ((set (ev e ρ σ (cons (seqk es ρ) τ) (stack-alloc Ξ τ κ))))))))))
    (lambda (s)
      (match s
        ((ev (? symbol? x) ρ σ (cons φ κ) Ξ)
         (let ((v (store-lookup σ (env-lookup ρ x))))
           (set (ko φ v σ κ Ξ))))
        ((ev `(lambda ,x ,es ...) ρ σ (cons φ κ) Ξ)
         (set (ko φ (α (clo (lam x es) ρ)) σ κ Ξ)))
        ((ev `(quote ,e) ρ σ (cons φ κ) Ξ)
         (set (ko φ e σ κ Ξ)))
        ((ev (and `(if ,e0 ,e1 ,e2) e) ρ σ κ Ξ)
         (let ((τ (ctx e ρ σ)))
           (set (ev e0 ρ σ (cons (ifk e1 e2 ρ) τ) (stack-alloc Ξ τ κ)))))
        ((ev (and `(letrec ((,x ,e0)) ,es ...) e) ρ σ κ Ξ)
         (let* ((a (alloc x))
                (ρ* (env-bind ρ x a))
                (τ (ctx e ρ σ)))
           (set (ev e0 ρ* σ (cons (letk a es ρ*) τ) (stack-alloc Ξ τ κ)))))
        ((ev (and `(set! ,x ,e0) e) ρ σ κ Ξ)
         (let ((τ (ctx e ρ σ)))
           (set (ev e ρ σ (cons (setk x ρ) τ) (stack-alloc Ξ τ κ)))))
        ((ev `(begin ,es ...) ρ σ κ Ξ)
         (set (eval-seq es ρ σ κ Ξ)))
        ((ev (and `(,rator . ,rands) e) ρ σ κ Ξ)
         (let ((τ (ctx e ρ σ)))
           (set (ev rator ρ σ (cons (randk rands '() ρ) τ) (stack-alloc Ξ τ κ)))))
        ((ev v ρ σ (cons φ κ) Ξ)
         (set (ko φ (α v) σ κ Ξ)))
        ((ko (letk a es ρ) v σ τ Ξ)
         (let ((κs (stack-lookup Ξ τ))
               (σ* (store-alloc σ a v)))
           (apply set-union (set-map κs (lambda (κ) (eval-seq es ρ σ* κ Ξ))))))
        ((ko (setk x ρ) v σ τ Ξ)
         (let* ((κs (stack-lookup Ξ τ))
                (a (env-lookup ρ x))
                (σ* (store-update σ a v)))
           (apply set-union (set-map κs (lambda (κ) (set (ko (car κ) v (cdr κ) Ξ)))))))
        ((ko (randk '() vs ρ) v σ τ Ξ)
         (let* ((κs (stack-lookup Ξ τ))
                (vs (reverse (cons v vs)))
                (rators (γ (car vs)))
                (rands (cdr vs)))
           (for/fold ((result (set))) ((rator rators))
             (match rator
               ((clo (lam x es) ρ*)
                (let loop ((x x) (rands rands) (ρ ρ*) (σ σ))
                  (match x
                    ('()
                     (apply set-union (cons result (set-map κs (lambda (κ) (eval-seq es ρ σ κ Ξ))))))
                    ((cons x xs)
                     (let ((a (alloc x)))
                       (loop xs (cdr rands)
                             (env-bind ρ x a)
                             (store-alloc σ a (car rands)))))
                    ((? symbol? x)
                     (let* ((a (alloc x))
                            (ρ* (env-bind ρ x a))
                            (σ* (store-alloc σ a rands)))
                       (apply set-union (cons result (set-map κs (lambda (κ) (eval-seq es ρ* σ* κ Ξ))))))))))
               (_ (apply set-union (cons result (set-map κs (lambda (κ) (set (ko (car κ) (apply rator rands) σ (cdr κ) Ξ)))))))))))
        ((ko (randk rands vs ρ) v σ τ Ξ)
         (set (ev (car rands) ρ σ (cons (randk (cdr rands) (cons v vs) ρ) τ) Ξ)))
        ((ko (ifk e1 e2 ρ) v σ τ Ξ)
         (let ((κs (stack-lookup Ξ τ)))
           (set-union (if (true? v)
                          (apply set-union (set-map κs (lambda (κ) (set (ev e1 ρ σ κ Ξ)))))
                          (set))
                      (if (false? v)
                          (apply set-union (set-map κs (lambda (κ) (set (ev e2 ρ σ κ Ξ)))))
                          (set)))))
        ((ko (seqk (list es ...) ρ) _ σ τ Ξ)
         (eval-seq es ρ σ τ Ξ))
        ((ko (haltk) v _ _ _)
         (set))))))

(define (inject e global)
  (let loop ((global global) (ρ (hash)) (σ (hash)))
    (match global
      ('()
       (ev e ρ σ `(,(haltk)) (hash)))
      ((cons (cons x v) r)
       (let ((a (gensym x)))
         (loop r (hash-set ρ x a) (hash-set σ a v)))))))
                                      
(define (run s step)
  (let loop ((visited (set))
             (todo (set s)))
    (if (set-empty? todo)
        (for/set ((s visited) #:when (match s ((ko (haltk) _ _ _ _) #t) (_ #f)))
          (match-let (((ko (haltk) v _ _ _) s))
            v))
        (let ((s (set-first todo)))
          (if (set-member? visited s)
              (loop visited (set-rest todo))
              (loop (set-add visited s) (set-union (step s) (set-rest todo))))))))
;;

;; allocators
(define (conc-alloc x)
  (gensym x))

(define (mono-alloc x)
  x)
;;

;; conc lattice
(define (conc-α v)
  v)

(define (conc-γ v)
  (set v))

(define conc-⊥ 'undefined)

(define (conc-⊔ v1 v2)
  (match* (v1 v2)
    ((conc-⊥ v) v)
    ((v conc-⊥) v)
    ((_ _) (error "concrete join" v1 v2))))

(define (conc-true? v)
  v)

(define (conc-false? v)
  (not v))

(define conc-step (make-step conc-α conc-γ conc-⊥ conc-⊔ conc-alloc conc-true? conc-false?))

(define conc-global
  `((= . ,(conc-α =))
    (< . ,(conc-α <))
    (+ . ,(conc-α +))
    (- . ,(conc-α -))
    (* . ,(conc-α *))))

(define (conc-eval e)
  (run (inject e conc-global) conc-step))
;;

;; type lattice
(define (type-α v)
  (cond
    ((number? v) (set 'NUM))
    ((boolean? v) (set 'BOOL))
    ((clo? v) (set v))
    ((procedure? v) (set v))
    (else (error "bwek" v))))

(define (type-γ v)
  v)

(define type-⊥ (set))

(define type-⊔ set-union)

(define (type-true? v)
  #t)

(define (type-false? v)
  #t)

(define type-step (make-step type-α type-γ type-⊥ type-⊔ mono-alloc type-true? type-false?))

(define type-global
  `((= . ,(type-α (lambda (v1 v2)
                    (set 'BOOL))))
    (< . ,(type-α (lambda (v1 v2)
                    (set 'BOOL))))
    (+ . ,(type-α (lambda (v1 v2)
                    (set 'NUM))))
    (- . ,(type-α (lambda (v1 v2)
                    (set 'NUM))))
    (* . ,(type-α (lambda (v1 v2)
                    (set 'NUM))))))

(define (type-eval e)
  (run (inject e type-global) type-step))
;;

(define sq '((lambda (x) (* x x)) 8))
(define loopy1 '(letrec ((f (lambda () (f)))) (f)))
(define loopy2 '((lambda (x) (x x)) (lambda (y) (y y))))
(define safeloopy1 '(letrec ((count (lambda (n) (letrec ((t (= n 0))) (if t 123 (letrec ((u (- n 1))) (letrec ((v (count u))) v))))))) (count 8)))
(define fac '(letrec ((fac (lambda (n) (if (= n 0) 1 (* n (fac (- n 1))))))) (fac 8)))
(define fib '(letrec ((fib (lambda (n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))))) (fib 8)))

(conc-eval sq)
(conc-eval fac)
(conc-eval fib)
(conc-eval safeloopy1)

(type-eval sq)
(type-eval loopy1)
(type-eval loopy2)
(type-eval fac)
(type-eval fib)
(type-eval safeloopy1)