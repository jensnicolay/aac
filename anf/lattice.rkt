;; conc lattice
(define (conc-α v)
  v)

(define (conc-γ v)
  (set v))

(define conc-⊥ (gensym '⊥))

(define (conc-⊔ v1 v2)
  (match* (v1 v2)
    (((== conc-⊥) v) v)
    ((v (== conc-⊥)) v)
    ((v v) v)
    ((_ _) (error "concrete join" v1 v2))))

(define (conc-true? v)
  v)

(define (conc-false? v)
  (not v))

(define conc-global
  `((= . ,(conc-α =))
    (< . ,(conc-α <))
    (> . ,(conc-α >))
    (<= . ,(conc-α <=))
    (>= . ,(conc-α >=))
    (+ . ,(conc-α +))
    (- . ,(conc-α -))
    (* . ,(conc-α *))
    (/ . ,(conc-α /))
    (not . ,(conc-α not))
    (gcd . ,(conc-α gcd))
    (modulo . ,(conc-α modulo))
    (remainder . ,(conc-α remainder))
    (quotient . ,(conc-α quotient))
    (ceiling . ,(conc-α ceiling))
    (log . ,(conc-α log))
    (even? . ,(conc-α even?))
    (odd? . ,(conc-α odd?))
    (symbol? . ,(conc-α symbol?))
    (null? . ,(conc-α null?))
    (char? . ,(conc-α pair?))
    (random . ,(conc-α (lambda (v) (if (zero? v) 0 (random v)))))))

(define (conc-eq? v1 v2)
  (eq? v1 v2))
    
(define conc-machine (make-machine conc-global conc-α conc-γ conc-⊥ conc-⊔ conc-alloc conc-true? conc-false? conc-eq?))

(define (conc-eval e)
  (do-eval e conc-machine))
;;

;; type lattice
(define (type-α v)
  (cond
    ((number? v) (set 'NUM))
    ((boolean? v) (set 'BOOL))
    ((symbol? v) (set 'SYM))
    ((string? v) (set 'STR))
    ((clo? v) (set v))
    ((prim? v) (set v))
    ((procedure? v) (set v))
    ((addr? v) (set v))
    ((pair? v) (set v))
    ((null? v) (set v))
    (else (error "bwek" v))))

(define (type-γ v)
  v)

(define type-⊥ (set))

(define type-⊔ set-union)

(define (type-true? v)
  #t)

(define (type-false? v)
  #t)

(define type-global
  (let ((any->bool
         (lambda (v)
           (set 'BOOL)))
        (any->num
         (lambda (v)
           (set 'NUM)))
        (num->num
         (lambda (v)
           (for/fold ((r (set))) ((w v))
             (match w
               ('NUM (set-add r 'NUM))
               (_ r)))))
        (num->bool
         (lambda (v)
           (for/fold ((r (set))) ((w v))
             (match w
               ('NUM (set-add r 'BOOL))
               (_ r)))))
        (num->num->num
         (lambda (v1 v2)
           (for*/fold ((r (set))) ((w1 v1) (w2 v2))
             (match* (w1 w2)
               (('NUM 'NUM) (set-add r 'NUM))
               ((_ _) r)))))
        (num->num->bool
         (lambda (v1 v2)
           (for*/fold ((r (set))) ((w1 v1) (w2 v2))
             (match* (w1 w2)
               (('NUM 'NUM) (set-add r 'BOOL))
               ((_ _) r))))))
    `((= . ,(type-α num->num->bool))
      (< . ,(type-α num->num->bool))
      (<= . ,(type-α num->num->bool))
      (> . ,(type-α num->num->bool))
      (>= . ,(type-α num->num->bool))
      (+ . ,(type-α num->num->num))
      (- . ,(type-α num->num->num))
      (* . ,(type-α num->num->num))
      (/ . ,(type-α num->num->num))
      (not . ,(type-α any->bool))
      (gcd . ,(type-α num->num->num))
      (modulo . ,(type-α num->num->num))
      (remainder . ,(type-α num->num->num))
      (quotient . ,(type-α num->num->num))
      (ceiling . ,(type-α num->num))
      (log . ,(type-α num->num))
      (even? . ,(type-α num->bool))
      (odd? . ,(type-α num->bool))
      (symbol? . ,(type-α any->bool))
      (null? . ,(type-α any->bool))
      (char? . ,(type-α any->bool))
      (random . ,(type-α any->num)))))
  
(define (type-eq? v1 v2)
  (set 'BOOL))
  
(define type-machine (make-machine type-global type-α type-γ type-⊥ type-⊔ mono-alloc type-true? type-false? type-eq?))
  
(define (type-eval e)
  (do-eval e type-machine))  
;;
