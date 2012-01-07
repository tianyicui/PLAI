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

(define (interp expr env)
  (type-case FAE expr
    [num (n) n]
    [add (l r) (+ (interp l env) (interp r env))]
    [sub (l r) (- (interp l env) (interp r env))]
    [id (v) (lookup-env v env)]
    [fun (bound-id bound-body)
         (lambda (arg-val)
           (interp bound-body
                   (aSub bound-id arg-val env)))]
    [app (fun-expr arg-expr)
         (local ([define fun-val (interp fun-expr env)]
                 [define arg-val (interp arg-expr env)])
           (fun-val arg-val))]))