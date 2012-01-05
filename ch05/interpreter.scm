#lang plai

(define-type F1WAE
  [num (n number?)]
  [add (lhs F1WAE?) (rhs F1WAE?)]
  [sub (lhs F1WAE?) (rhs F1WAE?)]
  [with (name symbol?) (named-expr F1WAE?) (body F1WAE?)]
  [id (name symbol?)]
  [app (fun-name symbol?) (arg F1WAE?)])

(define-type FunDef
  [fundef (fun-name symbol?)
          (arg-name symbol?)
          (body F1WAE?)])

(define (lookup-fundef fun-name fundefs)
  (cond
    [(empty? fundefs) (error fun-name "function not found")]
    [else (if (symbol=? fun-name (fundef-fun-name (first fundefs)))
              (first fundefs)
              (lookup-fundef fun-name (rest fundefs)))]))

(define-type DefrdSub
  [mtSub]
  [aSub (name symbol?) (value number?) (ds DefrdSub?)])

(define (lookup-ds name ds)
  (type-case DefrdSub ds
    [mtSub () (error 'lookup "no binding for identifier")]
    [aSub (bound-name bound-value rest-ds)
          (if (symbol=? name bound-name)
              bound-value
              (lookup-ds name rest-ds))]))

(define (parse expr)
  (cond ((number? expr) [num expr])
        ((symbol? expr) [id expr])
        ((eq? '+ (car expr)) [add (parse (cadr expr)) (parse (caddr expr))])
        ((eq? '- (car expr)) [sub (parse (cadr expr)) (parse (caddr expr))])
        ((eq? 'with (car expr)) [with (caadr expr) (parse (cadadr expr)) (parse (caddr expr))])
        ((symbol? (car expr)) [app (car expr) (parse (cadr expr))])))

(define (interp expr fun-defs ds)
  (type-case F1WAE expr
    [num (n) n]
    [add (l r) (+ (interp l fun-defs ds) (interp r fun-defs ds))]
    [sub (l r) (- (interp l fun-defs ds) (interp r fun-defs ds))]
    [with (bound-id named-expr bound-body)
          (interp bound-body
                  fun-defs
                  (aSub bound-id
                        (interp named-expr fun-defs ds)
                        ds))]
    [id (v) (lookup-ds v ds)]
    [app (fun-name arg-expr)
         (local ([define the-fun-def (lookup-fundef fun-name fun-defs)])
           (interp (fundef-body the-fun-def)
                   fun-defs
                   (aSub (fundef-arg-name the-fun-def)
                         (interp arg-expr fun-defs ds)
                         (mtSub))))]))