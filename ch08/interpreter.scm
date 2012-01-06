#lang plai

(define-type CFAE/L
  [num (n number?)]
  [add (lhs CFAE/L?) (rhs CFAE/L?)]
  [sub (lhs CFAE/L?) (rhs CFAE/L?)]
  [id (name symbol?)]
  [fun (param symbol?) (body CFAE/L?)]
  [app (fun-expr CFAE/L?) (arg-expr CFAE/L?)])

(define (parse expr)
  (cond ((number? expr) [num expr])
        ((symbol? expr) [id expr])
        ((eq? '+ (car expr)) [add (parse (cadr expr)) (parse (caddr expr))])
        ((eq? '- (car expr)) [sub (parse (cadr expr)) (parse (caddr expr))])
        ((eq? 'fun (car expr)) [fun (cadr expr) (parse (caddr expr))])
        ((eq? 'with (car expr)) [app [fun (caadr expr) (parse (caddr expr))] (parse (cadadr expr))])
        (else [app (parse (car expr)) (parse (cadr expr))])))

(define-type CFAE/L-Value
  [numV (n number?)]
  [closureV (param symbol?)
            (body CFAE/L?)
            (env Env?)]
  [exprV (expr CFAE/L?)
         (env Env?)
         (cache boxed-boolean/CFAE/L-Value?)])

(define (boxed-boolean/CFAE/L-Value? v)
  (and (box? v)
       (or (boolean? (unbox v))
           (numV? (unbox v))
           (closureV? (unbox v)))))

(define-type Env
  [mtSub]
  [aSub (name symbol?) (valule CFAE/L-Value?) (env Env?)])

(define (lookup-env name env)
  (type-case Env env
    [mtSub () (error 'lookup "no binding for identifier")]
    [aSub (bound-name bound-value rest-env)
          (if (symbol=? name bound-name)
              bound-value
              (lookup-env name rest-env))]))

(define (num+ n1 n2)
  (numV (+ (numV-n (strict n1)) (numV-n (strict n2)))))

(define (num- n1 n2)
  (numV (- (numV-n (strict n1)) (numV-n (strict n2)))))

(define (num-zero? n)
  (zero? (numV-n (strict n))))

(define (strict e)
  (type-case CFAE/L-Value e
    [exprV (expr env cache)
           (if (boolean? (unbox cache))
               (local ([define the-value (strict (interp expr env))])
                 (begin
                   (printf "Forcing exprV to ~a~n" the-value)
                   (set-box! cache the-value)
                   the-value))
               (begin
                 (printf "Using cached value~n")
                 (unbox cache)))]
    [else e]))

(define (interp expr env)
  (type-case CFAE/L expr
    [num (n) (numV n)]
    [add (l r) (num+ (interp l env) (interp r env))]
    [sub (l r) (num- (interp l env) (interp r env))]
    [id (v) (lookup-env v env)]
    [fun (bound-id bound-body)
         (closureV bound-id bound-body env)]
    [app (fun-expr arg-expr)
         (local ([define fun-val (strict (interp fun-expr env))]
                 [define arg-val (exprV arg-expr env (box false))])
           (interp (closureV-body fun-val)
                          (aSub (closureV-param fun-val)
                                arg-val
                                (closureV-env fun-val))))]))