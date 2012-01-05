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
            (ds DefrdSub?)])

(define-type DefrdSub
  [mtSub]
  [aSub (name symbol?) (valule FAE-Value?) (ds DefrdSub?)])

(define (lookup-ds name ds)
  (type-case DefrdSub ds
    [mtSub () (error 'lookup "no binding for identifier")]
    [aSub (bound-name bound-value rest-ds)
          (if (symbol=? name bound-name)
              bound-value
              (lookup-ds name rest-ds))]))

(define (add-numbers lval rval)
  [numV (+ (numV-n lval) (numV-n rval))])

(define (minus-numbers lval rval)
  [numV (- (numV-n lval) (numV-n rval))])

(define (interp expr ds)
  (type-case FAE expr
    [num (n) (numV n)]
    [add (l r) (add-numbers (interp l ds) (interp r ds))]
    [sub (l r) (minus-numbers (interp l ds) (interp r ds))]
    [id (v) (lookup-ds v ds)]
    [fun (bound-id bound-body)
         (closureV bound-id bound-body ds)]
    [app (fun-expr arg-expr)
         (local ([define fun-val (interp fun-expr ds)])
           (interp (closureV-body fun-val)
                          (aSub (closureV-param fun-val)
                                (interp arg-expr ds)
                                (closureV-ds fun-val))))]))