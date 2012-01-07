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

(define (Env? x)
  (procedure? x))

(define (mtSub)
  (lambda (name) (interp (parse '{with {f {fun x {+ x x}} {f 3}}}) (mtSub))
    (error 'lookup "no binding for identifier")))

(define (aSub bound-name bound-value env)
  (lambda (want-name)
    (if (symbol=? want-name bound-name)
        bound-value
        (lookup-env want-name env))))

(define (lookup-env name env)
  (env name))

(define (cyclically-bind-and-interp bound-name named-expr env)
  (local ([define rec-ext-env
            (lambda (want-name)
              (if (symbol=? want-name bound-name)
                  (interp named-expr rec-ext-env)
                  (lookup-env want-name env)))])
    rec-ext-env))

(define (interp expr env)
  (type-case RCFAE expr
    [num (n) n]
    [add (l r) (+ (interp l env) (interp r env))]
    [sub (l r) (- (interp l env) (interp r env))]
    [id (v) (lookup-env v env)]
    [fun (bound-id bound-body)
         (lambda (arg-val)
           (interp bound-body
                   (aSub bound-id arg-val env)))]
    [if0 (cond-expr if-expr else-expr)
         (if (zero? (interp cond-expr env))
             (interp if-expr env)
             (interp else-expr env))]
    [rec (bound-id named-expr bound-body)
      (interp bound-body
              (cyclically-bind-and-interp bound-id
                                          named-expr
                                          env))]
    [app (fun-expr arg-expr)
         (local ([define fun-val (interp fun-expr env)]
                 [define arg-val (interp arg-expr env)])
           (fun-val arg-val))]))