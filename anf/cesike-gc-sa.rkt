#lang racket

; memoizing, garbage-collecting CESIK*Ξ machine for ANF Scheme (lambda if set! let letrec)
; self-adjusting memo with parameterizable procId
; gc on application evaluation
; guarded pop from Ξ with immediate (local) kont application (the halt state is still handled as an administractive step)
; local Ξ

(random-seed 111) ; deterministic random
(define MEMO (lambda (msg) #f))
(define DEBUG '())

(include "../general.rkt")

;; domain helpers
(define (env-lookup ρ x)
  (hash-ref ρ x))
(define (env-addresses ρ)
  (list->set (hash-values ρ)))
(define (store-lookup σ a)
  (hash-ref σ a))
(define (stack-lookup Ξ τ)
  (hash-ref Ξ τ))
(define (ae? e)
  (match e
    ((? symbol? e) #t)
    (`(lambda ,_ ,_) #t)
    ((? boolean? e) #t)
    ((? number? e) #t)
    ((? string? e) #t)
    (`(quote ,e) (not (pair? e)))
    (_ #f)))
(define (free e)
  (define (f e env)
    (match e
      ((? symbol? e) (if (set-member? env e)
                         (set)
                         (set e)))
      (`(lambda ,x ,e) (f e (set-union env (list->set x))))
      (`(let ((,x ,e0)) ,e1) (set-union (f e0 env) (f e1 (set-add env x))))
      (`(letrec ((,x ,e0)) ,e1) (set-union (f e0 (set-add env x)) (f e1 (set-add env x))))
      (`(if ,ae ,e1 ,e2) (set-union (f ae env) (f e1 env) (f e2 env)))
      (`(set! ,x ,ae) (set-union (f x env) (f ae env) ))
      (`(quote ,_) (set))
      (`(,rator . ,rands) (set-union (f rator env) (for/fold ((xs (set))) ((rand rands)) (set-union xs (f rand env)))))
      (_ (set))))
  (f e (set)))
;;

;; machine
(struct machine (⊥ ⊔ inject step answer?) #:transparent)
(struct ev (e ρ σ ι κ Ξ m r) #:transparent)
(struct ko (ι κ v σ Ξ m r) #:transparent)
(struct ctx (clo vs σ) #:transparent)
(struct letk (x e ρ) #:transparent)
(struct letreck (a e ρ) #:transparent)
(struct haltk () #:transparent)
(struct clo (λ ρ id) #:transparent)
(struct prim (name proc) #:methods gen:equal+hash ((define equal-proc (lambda (s1 s2 requal?)
                                                                        (equal? (prim-name s1) (prim-name s2))))
                                                   (define hash-proc (lambda (s rhash) (equal-hash-code (prim-name s))))
                                                   (define hash2-proc (lambda (s rhash) (equal-secondary-hash-code (prim-name s))))))
(struct prim2 (name proc) #:methods gen:equal+hash ((define equal-proc (lambda (s1 s2 requal?)
                                                                         (equal? (prim2-name s1) (prim2-name s2))))
                                                    (define hash-proc (lambda (s rhash) (equal-hash-code (prim2-name s))))
                                                    (define hash2-proc (lambda (s rhash) (equal-secondary-hash-code (prim2-name s))))))
(struct addr (a) #:transparent)
(struct vmr (v m r) #:transparent)
(struct system (mach states) #:transparent)

(define (make-machine global α γ ⊥ ⊔ alloc true? false? α-eq? memo?)
  (define (env-bind ρ x a)
    (hash-set ρ x a))
  (define (store-alloc σ a v)
    (hash-set σ a (⊔ (hash-ref σ a ⊥) v)))
  (define (store-update σ a v)
    (hash-set σ a (⊔ (hash-ref σ a) v)))
  (define (memo-cache m τ v)
    (let* ((id (clo-id (ctx-clo τ)))
           (current (hash-ref m id #f))) ; possible that proc already unreachable, e.g. ((lam (x) x) 123)
      (if (hash? current)
          (let ((vs (ctx-vs τ)))
            (hash-set m id (hash-set current vs (⊔ (hash-ref current vs ⊥) v))))
          m)))
  (define (memo-poly m id)
    (if (hash-has-key? m id)
        (hash-set m id 'POLY)
        (hash-set m id (hash))))
  ;(define (update-r r a ctxs)
   ; (let ((ids (list->set (set-map (force ctxs) (lambda (ctx) (clo-id (ctx-clo ctx)))))))
    ;  (hash-set r a (set-union (hash-ref r a (set)) ids))))
  (define (eval-atom ae ρ σ ctxs m r)
    (match ae
      ((? symbol? ae)
       (let ((a (env-lookup ρ ae)))
         (vmr (store-lookup σ a) m r)));(update-r r a ctxs))))
      (`(lambda ,x ,e0)
       (let ((id (λ-proc ae ρ)))
         (vmr (α (clo ae ρ id)) (if memo? (memo-poly m ae) m) r)))
      (`(quote ,atom) (vmr (α atom) m r))
      (_ (vmr (α ae) m r))))
  (include "primitives.rkt")
  (define (step)
    (define Ξ (hash))
    (define Ξi 0)
    (define (stack-alloc! τ κ)
      (let ((stacks (hash-ref Ξ τ #f)))
        (if stacks
            (unless (set-member? stacks κ)
              (set! Ξ (hash-set Ξ τ (set-add stacks κ)))
              (set! Ξi (add1 Ξi)))
            (begin
              (set! Ξ (hash-set Ξ τ (set κ)))
              (set! Ξi (add1 Ξi))))))
    (define (apply-local-kont ι κ v σ m r)
      (match ι
        ((cons (letk x e ρ) ι)
         (let* ((a (alloc x #f))
                (ρ* (env-bind ρ x a))
                (σ* (store-alloc σ a v)))
           (ev e ρ* σ* ι κ Ξi m r)))
        ((cons (letreck a e ρ) ι)
         (let ((σ* (store-update σ a v)))
           (ev e ρ σ* ι κ Ξi m r)))
        (_ (ko ι κ v σ Ξi m r))))
    (define (touches d)
      (if (set? d)
          (for/fold ((as (set))) ((v d)) (set-union as (touches v)))
          (match d
            ((clo _ ρ _) (env-addresses ρ))
            ((letk _ _ ρ) (env-addresses ρ))
            ((letreck _ _ ρ) (env-addresses ρ))
            ((addr a) (set a))
            ((cons x y) (set-union (touches x) (touches y)))
            (_ (set)))))
    (define (reachable A σ)
      (let loop ((A A) (R (set)) (I (set)))
        (if (set-empty? A)
            (cons R I)
            (let ((a (set-first A)))
              (if (set-member? R a)
                  (loop (set-rest A) R I)
                  (let* ((v (γ (store-lookup σ a)))
                         (T (touches v))
                         (I* (for/fold ((r I)) ((d v)) (if (clo? d) (set-add r (clo-id d)) r))))
                    (loop (set-union (set-rest A) T) (set-add R a) I*)))))))
    (define (gc s)
      (match s
        ((ev e ρ σ ι κ Ξ m r)
         (let* ((ρ* (↓ ρ (free e)))
                (RI (reachable (set-union (env-addresses ρ*) (apply set-union (set-map (stack-frames ι κ) touches))) σ))
                (R (car RI))
                (I (cdr RI))
                (σ* (↓ σ R))
                (r* (↓ r R))
                (m* (↓ m I)))
           (ev e ρ* σ* ι κ Ξ m* r*)))
        ((ko ι κ v σ Ξ m r)
         (let* ((R (reachable (set-union (apply set-union (set-map (stack-frames ι κ) touches)) (touches v)) σ))
                (σ* (↓ σ R)))
           (ko ι κ v σ* Ξ m r)))))        
    (define (stack-frames ι κ)
      (let loop ((todo (set (cons ι κ))) (result (list->set ι)) (seen (set)))
        (if (set-empty? todo)
            result
            (let ((ικ (set-first todo)))
              (match ικ
                ((cons ι κ)
                 (let ((result* (set-union result (list->set ι))))
                   (if (or (not κ) (set-member? seen κ))
                       (loop (set-rest todo) result* seen)
                       (loop (set-union (set-rest todo) (stack-lookup Ξ κ)) result* (set-add seen κ))))))))))
    (define (stack-pop ι κ G)
      (if (null? ι)
          (if (or (not κ) (set-member? G κ))
              (set)
              (let ((ικs (stack-lookup Ξ κ)))
                (apply set-union (set-map ικs (lambda (ικ) (stack-pop (car ικ) (cdr ικ) (set-add G κ)))))))
          (set (list ι κ G)))) 
    (define (stack-contexts κ)
      (delay
        (let loop ((todo (set (cons #f κ))) (seen (set)))
          (if (set-empty? todo)
              seen
              (let ((κ (cdr (set-first todo))))
                (if (or (not κ) (set-member? seen κ))
                    (loop (set-rest todo) seen)
                    (loop (set-union (set-rest todo) (stack-lookup Ξ κ)) (set-add seen κ))))))))
    (lambda (s)
      (match s
        ((ev (? ae? ae) ρ σ ι κ _ m r)
         (match-let (((vmr v m r) (eval-atom ae ρ σ (stack-contexts κ) m r)))
           (set (ko ι κ v σ Ξi m r))))
        ((ev `(if ,ae ,e1 ,e2) ρ σ ι κ _ m r)
         (match-let (((vmr v m r) (eval-atom ae ρ σ (stack-contexts κ) m r)))
           (set-union (if (true? v)
                          (set (ev e1 ρ σ ι κ Ξi m r))
                          (set))
                      (if (false? v)
                          (set (ev e2 ρ σ ι κ Ξi m r))
                          (set)))))
        ((ev `(let ((,x ,e0)) ,e1) ρ σ ι κ _ m r)
         (set (ev e0 ρ σ (cons (letk x e1 ρ) ι) κ Ξi m r)))
        ((ev `(letrec ((,x ,e0)) ,e1) ρ σ ι κ _ m r)
         (let* ((a (alloc x #f))
                (ρ* (env-bind ρ x a))
                (σ* (store-alloc σ a ⊥)))
           (set (ev e0 ρ* σ* (cons (letreck a e1 ρ*) ι) κ Ξi m r))))
        ((ev `(set! ,x ,ae) ρ σ ι κ _ m r)
         (match-let (((vmr v m r) (eval-atom ae ρ σ (stack-contexts κ) m r)))
           (let* ((a (env-lookup ρ x))
                  (σ* (store-update σ a v)))
             (set (ko ι κ v σ* Ξi m r)))))
        ((ev (and `(,rator . ,rands) e) ρ σ ι κ _ m r)
         (match-let (((ev _ Γρ Γσ _ _ _ Γm Γr) (gc s)))
           (let ((ctxs (stack-contexts κ)))
             (match-let (((vmr v m r) (eval-atom rator Γρ Γσ ctxs Γm Γr)))
               (let rands-loop ((rands rands) (rvs '()) (m m) (r r))
                 (if (null? rands)
                     (for/fold ((states (set))) ((w (γ v)))
                       (match w
                         ((clo `(lambda ,x ,e0) ρ** id)
                          (define (rator-loop x vs ρ* σ*)
                            (match x
                              ('()
                               (let ((τ (ctx w rvs Γσ)))
                                 (stack-alloc! τ (cons ι κ))
                                 (set-union states (set (ev e0 ρ* σ* '() τ Ξi m r)))))
                              ((cons x xs)
                               (if (null? vs)
                                   (set)
                                   (let ((a (alloc x e)))
                                     (rator-loop xs (cdr vs) (env-bind ρ* x a) (store-alloc σ* a (car vs))))))))
                          (if memo?
                              (let ((cached (hash-ref m id #f)))
                                (if (and (hash? cached) (hash-has-key? cached rvs))
                                    (let ((cached-v (hash-ref cached rvs)))
                                      (MEMO "MEMO")
                                      (set-union states (set (ko ι κ cached-v Γσ Ξi m r))))
                                    (rator-loop x (reverse rvs) ρ** Γσ)))
                              (rator-loop x (reverse rvs) ρ** Γσ)))
                         ((prim _ proc)
                          (set-union states (list->set (set-map (proc e (reverse rvs) Γσ ι κ Ξi) (lambda (vσ) (ko ι κ (car vσ) (cdr vσ) Ξi m r))))))
                         ((prim2 _ proc)
                          (set-union states (set (ko ι κ (apply proc (reverse rvs)) Γσ Ξi m r))))
                         (_ (set))))
                     (match-let (((vmr v m r) (eval-atom (car rands) Γρ Γσ ctxs m r)))
                       (rands-loop (cdr rands) (cons v rvs) m r))))))))
        ((ko (cons (haltk) _) #f v _ _ _ _)
         (set))
        ((ko ι κ v σ _ m r)
         (let ((ικGs (stack-pop ι κ (set))))
           (for/set ((ικG ικGs))
             (let ((m* (if memo?
                           (for/fold ((m m)) ((τ (caddr ικG))) (memo-cache m τ v))
                           m)))
               (apply-local-kont (car ικG) (cadr ικG) v σ m* r))))))))
  (define (inject e)
    (let ((global* (append global
                           `((eq? . ,(α (prim 'eq? prim-eq?)))
                             (equal? . ,(α (prim 'equal? prim-equal?)))
                             (error . ,(α (prim 'error prim-error)))
                             (pair? . ,(α (prim 'pair? prim-pair)))
                             (cons . ,(α (prim 'cons prim-cons)))
                             (reverse . ,(α (prim 'reverse prim-reverse)))
                             (car . ,(α (prim 'car prim-car)))
                             (cdr . ,(α (prim 'cdr prim-cdr)))))))
      (set! conc-alloc-counter 0)
      (let loop ((global global*) (ρ (hash)) (σ (hash)))
        (match global
          ('()
           (ev e (↓ ρ (free e)) σ `(,(haltk)) #f (hash) (hash) (hash)))
          ((cons (cons x v) r)
           (let ((a (conc-alloc x 0)))
             (loop r (hash-set ρ x a) (hash-set σ a v))))))))
  (define (answer? s)
    (match s
      ((ko (cons (haltk) _) _ v _ _ _ _) #t)
      (_ #f)))
  
  (machine ⊥ ⊔ inject step answer?))

;; procedure identifiers
(define (λ-proc λ ρ)
  λ)

(define (clo-proc λ ρ)
  (cons λ ρ))
;;

(define (run s mach)
  (let ((step ((machine-step mach))))
    (let loop ((visited (set))
               (todo (set s)))
      (if (set-empty? todo)
          (system mach visited)
          (let ((s (set-first todo)))
            (if (set-member? visited s)
                (loop visited (set-rest todo))
                (loop (set-add visited s) (set-union (step s) (set-rest todo)))))))))

(define (explore e mach)
  (let ((inject (machine-inject mach)))
    (run (inject e) mach)))

(define (answer-set sys)
  (let ((answer? (machine-answer? (system-mach sys))))
    (for/fold ((v (set))) ((s (system-states sys)))
      (if (answer? s)
          (set-add v s)
          v))))

(define (answer-value sys)
  (let* ((mach (system-mach sys))
         (⊥ (machine-⊥ mach))
         (⊔ (machine-⊔ mach)))
    (for/fold ((v ⊥)) ((s (answer-set sys)))
      (⊔ (ko-v s) v))))
;;

;; allocators
(define conc-alloc-counter 0)
(define conc-alloc
  (lambda (_ __)
    (set! conc-alloc-counter (add1 conc-alloc-counter))
    conc-alloc-counter))

(define (mono-alloc x _)
  x)

(define (poly-alloc x ctx)
  (cons x ctx))
;  (cons x (if ctx
;              (clo-λ (ctx-clo ctx))
;              ctx)))
;;

(include "lattice.rkt")
(include "test.rkt")

;; Some metrics
(define (state-size state)
  ;; Number of kB it takes to represent a state in textual form
  (round (/ (string-length (~a state)) 1000)))

(define (memo-size-f f)
  (lambda (state)
    (let ((m (match state
               ((ev _ _ _ _ _ _ m _) m)
               ((ko _ _ _ _ _ m _) m))))
      (foldl (lambda (table acc) (+ acc (f table)))
             0 (hash-values m)))))

;; Number of entries in the memo table of a state
(define memo-size
  (memo-size-f (lambda (table) (if (hash? table) (length (hash-keys table)) 0))))

;; Number of poly entries in the memo table
(define poly-count
  (memo-size-f (lambda (table) (if (equal? table 'POLY) 1 0))))

;; Number of impure entries in the memo table
(define impure-count
  (memo-size-f (lambda (table) (if (equal? table 'IMPURE) 1 0))))

(define (reads-size state)
  ;; Number of entries in the read table
  (let ((r (match state
             ((ev _ _ _ _ _ _ _ r) r)
             ((ko _ _ _ _ _ _ r) r))))
    (foldl (lambda (v acc) (+ acc (set-count v)))
           0
           (hash-values r))))

(define (answer-size sys f)
  ;; Total size of the answer set for some size function
  (foldl + 0 (map f (set->list (answer-set sys)))))

(define (avg-size sys f)
  ;; Average size of a (component of a) state
  (let ((states (answer-set sys)))
    (round (/ (foldl + 0 (map f (set->list states)))
              (set-count states)))))
  
;; machines and evaluators
(define (do-eval e mach)
  (answer-value (explore e mach)))  
  
(define type-machine (make-machine type-global type-α type-γ type-⊥ type-⊔ poly-alloc type-true? type-false? type-eq? #t))
  
(define (type-eval e)
  (do-eval e type-machine))  

(define conc-machine (make-machine conc-global conc-α conc-γ conc-⊥ conc-⊔ conc-alloc conc-true? conc-false? conc-eq? #t))

(define (conc-eval e)
  (do-eval e conc-machine))
;;

(define (memo-test . ens)
  (when (null? ens)
    (set! ens '(hellomemo blur fac fib eta gcipd kcfa2 kcfa3 loop2 mj09 rotate)))
  (define mach-0 (make-machine type-global type-α type-γ type-⊥ type-⊔ mono-alloc type-true? type-false? type-eq? #f))
  (define mach-0-m (make-machine type-global type-α type-γ type-⊥ type-⊔ mono-alloc type-true? type-false? type-eq? #t))
  (define mach-1 (make-machine type-global type-α type-γ type-⊥ type-⊔ poly-alloc type-true? type-false? type-eq? #f))
  (define mach-1-m (make-machine type-global type-α type-γ type-⊥ type-⊔ poly-alloc type-true? type-false? type-eq? #t))
  (for ((en ens))
    (let* ((e (eval en))
           (memo-count 0)
           (memo-counter
            (lambda (msg)
              (set! memo-count (add1 memo-count)))))
      (set! MEMO memo-counter)
      (let* ((start (current-milliseconds))
             (sys (explore e mach-1-m))
             (duration (- (current-milliseconds) start)))
        (printf "~a states ~a time ~a memo ~a result ~a\n"
                (~a en #:min-width 10) (~a (set-count (system-states sys)) #:min-width 8) (~a duration #:min-width 8) (~a memo-count #:min-width 4) (answer-value sys))))))