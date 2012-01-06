#lang plai

(define-type RCFAE
  [num (n number?)]
  [add (lhs RCFAE?) (rhs RCFAE?)]
  [sub (lhs RCFAE?) (rhs RCFAE?)]
  [id (name symbol?)]
  [fun (param symbol?) (body RCFAE?)]
  [if0 (cond-expr RCFAE?) (if-expr RCFAE?) (else-expr RCFAE?)]
  [rec (name symbol?) (named-expr RCFAE?) (body RCFAE?)]
  [app (fun-expr RCFAE?) (arg-expr RCFAE?)])

(define (parse expr)
  (cond ((number? expr) [num expr])
        ((symbol? expr) [id expr])
        ((eq? '+ (car expr)) [add (parse (cadr expr)) (parse (caddr expr))])
        ((eq? '- (car expr)) [sub (parse (cadr expr)) (parse (caddr expr))])
        ((eq? 'fun (car expr)) [fun (cadr expr) (parse (caddr expr))])
        ((eq? 'with (car expr)) [app [fun (caadr expr) (parse (caddr expr))] (parse (cadadr expr))])
        ((eq? 'if0 (car expr)) [if0 (parse (cadr expr)) (parse (caddr expr)) (parse (cadddr expr))])
        ((eq? 'rec (car expr)) [rec (caadr expr) (parse (cadadr expr)) (parse (caddr expr))])
        (else [app (parse (car expr)) (parse (cadr expr))])))

(define-type RCFAE-Value
  [numV (n number?)]
  [closureV (param symbol?)
            (body RCFAE?)
            (env Env?)])

(define (boxed-RCFAE-Value? v)
  (and (box? v)
       (RCFAE-Value? (unbox v))))

(define-type Env
  [mtSub]
  [aSub (name symbol?) (value RCFAE-Value?) (env Env?)]
  [aRecSub (name symbol?) (value boxed-RCFAE-Value?) (env Env?)])

(define (lookup-env name env)
  (type-case Env env
    [mtSub () (error 'lookup "no binding for identifier")]
    [aSub (bound-name bound-value rest-env)
          (if (symbol=? name bound-name)
              bound-value
              (lookup-env name rest-env))]
    [aRecSub (bound-name boxed-bound-value rest-env)
             (if (symbol=? bound-name name)
                 (unbox boxed-bound-value)
                 (lookup-env name rest-env))]))

(define (cyclically-bind-and-interp bound-id named-expr env)
  (local ([define value-holder (box (numV 0))]
          [define new-env (aRecSub bound-id value-holder env)]
          [define named-expr-val (interp named-expr new-env)])
    (begin
      (set-box! value-holder named-expr-val)
      new-env)))

(define (add-numbers lval rval)
  [numV (+ (numV-n lval) (numV-n rval))])

(define (minus-numbers lval rval)
  [numV (- (numV-n lval) (numV-n rval))])

(define (interp expr env)
  (type-case RCFAE expr
    [num (n) (numV n)]
    [add (l r) (add-numbers (interp l env) (interp r env))]
    [sub (l r) (minus-numbers (interp l env) (interp r env))]
    [id (v) (lookup-env v env)]
    [fun (bound-id bound-body)
         (closureV bound-id bound-body env)]
    [if0 (cond-expr if-expr else-expr)
         (if (zero? (numV-n (interp cond-expr env)))
             (interp if-expr env)
             (interp else-expr env))]
    [rec (bound-id named-expr bound-body)
      (interp bound-body
              (cyclically-bind-and-interp bound-id
                                          named-expr
                                          env))]
    [app (fun-expr arg-expr)
         (local ([define fun-val (interp fun-expr env)])
           (interp (closureV-body fun-val)
                          (aSub (closureV-param fun-val)
                                (interp arg-expr env)
                                (closureV-env fun-val))))]))