#lang racket
(require racket/fixnum)

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
    [(? fvar? fvar) fvar]
    [_ #f]))

(define (fvar? fvar)
  (regexp-match #px"fv\\d+" (symbol->string fvar)))

(define int32?
  (lambda (x)
    (and (integer? x) (>= x (- 0 (expt 2 31))) (<= x (- (expt 2 31) 1)))))  

                                        ;Determines wether an integer is an int64
(define int64?
  (lambda (x)
    (and (integer? x) (>= x (- 0 (expt 2 63))) (<= x (- (expt 2 63) 1)))))  


(define (label? label)
  (regexp-match #px"\\S+\\$\\d+" (symbol->string label)))

(define (Triv? triv)
  (match triv
    [(? integer? int) triv]
    [(? fvar? fvar) triv]
    [(? label? label) triv]
    [_ #f]))


(define (Binop? op)
  (match op
    ['+ '+]
    ['- '-]
    ['* '*]
    ['logand 'logand]
    ['logor 'logor]
    ['sra 'sra]
    [_ #f]))

(define sra (lambda (x n) (fxrshift x n)))
