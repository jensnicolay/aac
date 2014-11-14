;; conc lattice
(define (conc-α v)
  v)

(define (conc-γ v)
  (set v))

(define conc-⊥ '$%conc-⊥)

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
  `((= . ,(conc-α (prim2 '= =)))
    (< . ,(conc-α (prim2 '< <)))
    (> . ,(conc-α (prim2 '> >)))
    (<= . ,(conc-α (prim2 '<= <=)))
    (>= . ,(conc-α (prim2 '<= >=)))
    (+ . ,(conc-α (prim2 '+ +)))
    (- . ,(conc-α (prim2 '- -)))
    (* . ,(conc-α (prim2 '* *)))
    (/ . ,(conc-α (prim2 '/ /)))
    (not . ,(conc-α (prim2 'not not)))
    (and . ,(conc-α (prim2 'and (lambda l
                                  (for/fold ((res #t))
                                      ((el l))
                                    (and res el))))))
    (or . ,(conc-α (prim2 'or (lambda l
                                (for/fold ((res #f))
                                    ((el l))
                                  (or res el))))))
    (gcd . ,(conc-α (prim2 'gcd gcd)))
    (modulo . ,(conc-α (prim2 'modulo modulo)))
    (remainder . ,(conc-α (prim2 'remainder remainder)))
    (quotient . ,(conc-α (prim2 'quotient quotient)))
    (ceiling . ,(conc-α (prim2 'ceiling ceiling)))
    (log . ,(conc-α (prim2 'log log)))
    (even? . ,(conc-α (prim2 'even? even?)))
    (odd? . ,(conc-α (prim2 'odd? odd?)))
    (symbol? . ,(conc-α (prim2 'symbol? symbol?)))
    (null? . ,(conc-α (prim2 'null? null?)))
    (char? . ,(conc-α (prim2 'pair? pair?)))
    (random . ,(conc-α (prim2 'random (lambda (v) (if (zero? v) 0 (random v))))))))

(define (conc-eq? v1 v2)
  (eq? v1 v2))    
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
    ((prim2? v) (set v))
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
    `((= . ,(type-α (prim2 '= num->num->bool)))
      (< . ,(type-α (prim2 '< num->num->bool)))
      (<= . ,(type-α (prim2 '<= num->num->bool)))
      (> . ,(type-α (prim2 '> num->num->bool)))
      (>= . ,(type-α (prim2 '>= num->num->bool)))
      (+ . ,(type-α (prim2 '+ num->num->num)))
      (- . ,(type-α (prim2 '- num->num->num)))
      (* . ,(type-α (prim2 '* num->num->num)))
      (/ . ,(type-α (prim2 '/ num->num->num)))
      (not . ,(type-α (prim2 'not any->bool)))
      (and . ,(type-α (prim2 'and any->bool)))
      (or . ,(type-α (prim2 'or any->bool)))
      (gcd . ,(type-α (prim2 'gcd num->num->num)))
      (modulo . ,(type-α (prim2 'modulo num->num->num)))
      (remainder . ,(type-α (prim2 'remainder num->num->num)))
      (quotient . ,(type-α (prim2 'quotient num->num->num)))
      (ceiling . ,(type-α (prim2 'ceiling num->num)))
      (log . ,(type-α (prim2 'log num->num)))
      (even? . ,(type-α (prim2 'even? num->bool)))
      (odd? . ,(type-α (prim2 'odd? num->bool)))
      (symbol? . ,(type-α (prim2 'symbol? any->bool)))
      (null? . ,(type-α (prim2 'null? any->bool)))
      (char? . ,(type-α (prim2 'char? any->bool)))
      (random . ,(type-α (prim2 'random any->num))))))
  
(define (type-eq? v1 v2)
  (set 'BOOL))
;;
