

(define primitive-environment
	`((apply . ,apply) (assq . ,assq)
 		(car . ,car) (cadr . ,cadr) (caddr . ,caddr)
		(cadddr . ,cadddr) (cddr . ,cddr) (cdr . ,cdr)
		(cons . ,cons) (eq? . ,eq?) (list . ,list) (map . ,map)
		(memv . ,memv) (null? . ,null?) (pair? . ,pair?)
		(read . ,read) (set-car! . ,set-car!)
		(set-cdr! . ,set-cdr!) (symbol? . ,symbol?)))


(define new-env
	(lambda (formals actuals env)
		(cond
			((null? formals) env)
			((symbol? formals) (cons (cons formals actuals) env))
			(else
				(cons
					(cons (car formals) (car actuals))
					(new-env(cdr formals) (cdr actuals) env))))))

(define lookup
	(lambda (var env)
		(cdr (assq var env))))


(define assign
	(lambda (var val env)
		(set-cdr! (assq var env) val)))

(define exec
	(lambda (expr env)
		(cond
			((symbol? expr) (lookup expr env))
			((pair? expr)
			 (case (car expr)
				((quote) (cadr expr))
				((lambda)
				 (lambda vals
					(let ((env (new-env (cadr expr) vals env)))
						(let loop ((exprs (cddr expr)))
							(if (null? (cdr exprs))
								(exec (car exprs) env)
								(begin                      
									(exec (car exprs) env)
									(loop (cdr exprs))))))))
				((if)
				 (if (exec (cadr expr) env)
				     (exec (caddr expr) env)
				     (exec (cadddr expr) env)))
				((set!) (assign (cadr expr) (exec (caddr expr) env) env))
				(else 
					(apply
						(exec (car expr) env)
						(map (lambda (x) (exec x env)) (cdr expr))))))
			(else expr))))


(define interpret
		(lambda (expr)
			(exec expr  primitive-environment)))



