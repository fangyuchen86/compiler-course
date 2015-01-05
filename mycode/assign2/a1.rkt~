#lang racket

                                        ;Program	---->	(begin Statement+)
                                        ;Statement	---->	(set! Var1 int64)
                                        ;								|	(set! Var1 Var2)
                                        ;								|	(set! Var1 (Binop Var1 int32))
                                        ;								|	(set! Var1 (Binop Var1 Var2))
                                        ;Var	---->	rax | rcx | rdx | rbx | rbp | rsi | rdi
                                        ;								|	r8 | r9 | r10 | r11 | r12 | r13 | r14 | r15
                                        ;Binop	---->	+ | - | *

                                        ;Determines wether an integer is an int32
(define int32?
  (lambda (x)
    (and (integer? x) (>= x (- 0 (expt 2 31))) (<= x (- (expt 2 31) 1)))))  

                                        ;Determines wether an integer is an int64
(define int64?
  (lambda (x)
    (and (integer? x) (>= x (- 0 (expt 2 63))) (<= x (- (expt 2 63) 1)))))  

(define (Var? var)
  (match var
    ['rax 'rax]
    ['rcx 'rcx]
    ['rdx 'rdx]
    ['rbx 'rbx]
    ['rbp 'rbp]
    ['rsi 'rsi]
    ['rdi 'rdi]
    ['r8  'r8]
    ['r9  'r9]
    ['r10 'r10]
    ['r11 'r11]
    ['r12 'r12]
    ['r13 'r13]
    ['r14 'r14]
    ['r15 'r15]
    [_ #f]))

(define (Binop? op)
  (match op
    ['+ '+]
    ['- '-]
    ['* '*]
    [_ #f]))


(define verify-scheme
  (lambda (x)

    (define (Statement? s)
      (match s
        [`(set! ,(? Var? var1) ,(? int64? int64)) `(set! ,var1 ,int64)]
        [`(set! ,(? Var? var1) ,(? Var? var2)) `(set! ,var1 ,var2)]
        [`(set! ,(? Var? var1) (,(? Binop? binop) ,(? Var? var2) ,(? int32? int32)))
         #:when (eq? var1 var2) 
                                        ;=>
         `(set! ,var1 (,binop ,var2 ,int32))]
        [`(set! ,(? Var? var1) (,(? Binop? binop) ,(? Var? var2) ,(? Var? var3)))
         #:when (eq? var1 var2)
                                        ;=>
         `(set! ,var1 (,binop ,var2 ,var3))]
        [_ #f]))

    (define (Program? p)
      (match p
        [`(begin ,(? Statement? s) ..1) `(begin ,@s)]
        [_ #f]))

    (if (Program? x)
        x
        (printf "~a is not valid program~n" x))))


(define (gen-statment s)
  (define (gen-binop op)
    (match op
      ['+ 'addq]
      ['- 'subq]
      ['* 'imulq]))
  (match s
    [`(set! ,(? Var? var1) ,(? int64? int64)) (format "movq $~a, %~a" int64 var1)]
    [`(set! ,(? Var? var1) ,(? Var? var2)) (format "movq %~a, %~a" var2 var1)]
    [`(set! ,(? Var? var1) (,(? Binop? binop) ,(? Var? var2) ,(? int32? int32)))
                                        ;=>
     (format "~a $~a, %~a" (gen-binop binop) int32 var1) ]
    [`(set! ,(? Var? var1) (,(? Binop? binop) ,(? Var? var2) ,(? Var? var3)))
                                        ;=>
     (format "~a %~a, %~a" (gen-binop binop) var3 var1)]))

(define (generate-program p)
  (match p
    [`(begin ,stms ..1 ) (map gen-statment stms)]))




(define (generate-x86-x64 p)
  (format ".globl _scheme_entry ~n _scheme_entry: ~a ~nret"
          (foldl (lambda (x acc) (format "~a ~n ~a" acc x)) "" (generate-program p))))

(define (driver program)
  (with-output-to-file "t.s" 
    (lambda ()
      (printf (generate-x86-x64 (verify-scheme program))))
    #:exists 'replace))


(define test 
  '(begin 
    (set! rax 5)            
    (set! rbx 1)
    (set! rbx (* rbx rax))
    (set! rax (- rax 1))
    (set! rbx (* rbx rax))
    (set! rax (- rax 1))
    (set! rbx (* rbx rax))
    (set! rax (- rax 1))
    (set! rbx (* rbx rax))
    (set! rax rbx)))
