#lang plai

(define-type FAE
  [num (n number?)]
  [add (lhs FAE?) (rhs FAE?)]
  [sub (lhs FAE?) (rhs FAE?)]
  [id (name symbol?)]
  [fun (param symbol?) (body FAE?)]
  [app (fun-expr FAE?) (arg-expr FAE?)])

(define (parse expr)
  (cond ((number? expr) [num expr])
        ((symbol? expr) [id expr])
        ((eq? '+ (car expr)) [add (parse (cadr expr)) (parse (caddr expr))])
        ((eq? '- (car expr)) [sub (parse (cadr expr)) (parse (caddr expr))])
        ((eq? 'fun (car expr)) [fun (cadr expr) (parse (caddr expr))])
        ((eq? 'with (car expr)) [app [fun (caadr expr) (parse (caddr expr))] (parse (cadadr expr))])
        (else [app (parse (car expr)) (parse (cadr expr))])))

(define-type FAE-Value
  [numV (n number?)]
  [closureV (param symbol?)
            (body FAE?)
            (env Env?)])

(define (Env? x)
  (procedure? x))

(define (mtSub)
  (lambda (name)
    (error 'lookup "no binding for identifier")))

(define (aSub bound-name bound-value env)
  (lambda (want-name)
    (if (symbol=? want-name bound-name)
        bound-value
        (lookup-env want-name env))))

(define (lookup-env name env)
  (env name))

(define (add-numbers lval rval)
  [numV (+ (numV-n lval) (numV-n rval))])

(define (minus-numbers lval rval)
  [numV (- (numV-n lval) (numV-n rval))])

(define (interp expr env)
  (type-case FAE expr
    [num (n) (numV n)]
    [add (l r) (add-numbers (interp l env) (interp r env))]
    [sub (l r) (minus-numbers (interp l env) (interp r env))]
    [id (v) (lookup-env v env)]
    [fun (bound-id bound-body)
         (closureV bound-id bound-body env)]
    [app (fun-expr arg-expr)
         (local ([define fun-val (interp fun-expr env)])
           (interp (closureV-body fun-val)
                          (aSub (closureV-param fun-val)
                                (interp arg-expr env)
                                (closureV-env fun-val))))]))