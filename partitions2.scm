(import srfi-1)

(define (cons-element x list-of-lists)
  "Conses x to every list in list-of-lists."
  (map (lambda (y)
	 (cons x y))
       list-of-lists))
;; (cons-element 3 '((a b) (c)))

;; A way to define memoized functions
;; From https://programmingpraxis.com/2017/05/19/just-showing-off/
(define-syntax define-memoized
  (syntax-rules ()
    ((define-memoized (f arg ...) body ...)
      (define f
        (let ((cache (list)))
          (lambda (arg ...)
            (cond ((assoc `(,arg ...) cache) => cdr)
            (else (let ((val (begin body ...)))
                    (set! cache (cons (cons `(,arg ...) val) cache))
                    val)))))))))

(define-memoized (partitions n)
  "Returns list of integer partitions of n."
  (if (= n 0) '(())
      (apply append
	     (map (lambda (i)
		    (cons-element
		     i (filter
			(lambda (part)
			  (or (null? part)
			      (<= (car part) i)))
			(partitions (- n i)))))
		  (iota n 1)))))

;; Try computing a large number
(print (length (partitions 60)))

