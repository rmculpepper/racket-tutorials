#lang racket/base
(require (for-syntax racket/base
                     racket/syntax
                     racket/string
                     racket/port
                     syntax/strip-context)
         scribble/manual
         scribble/example)
(provide (all-defined-out))

(begin-for-syntax
  (require (only-in scribble/comment-reader [read-syntax scribble-read-syntax]))

  (define (make-rx word label)
    (format "^[ ]*;*#*[ ]*~a CODE ~a[ ]*$" word label))

  (define (get-code-lines path label)
    (define begin-rx (make-rx "BEGIN" label))
    (define end-rx (make-rx "END" label))
    (define found-begin? #f) ;; mutated
    (define found-end? #f) ;; mutated
    (define-values (start-line lines)
      (with-input-from-file path
        (lambda ()
          (define ended? #f)
          (values
           (for/sum ([line (in-lines)]
                     #:break (and (regexp-match? begin-rx line) (set! found-begin? #t)))
             1)
           (for/list ([line (in-lines)]
                      #:break (and (regexp-match? end-rx line) (set! found-end? #t)))
             line)))))
    (unless found-begin?
      (wrong-syntax #f "code block not found: ~s" label))
    (unless found-end?
      (wrong-syntax #f "end of code block not found: ~s" label))
    (values (add1 start-line) lines))

  (define (get-code-forms path label)
    (define-values (start-line lines) (get-code-lines path label))
    (define in (open-input-string (string-join lines "\n")))
    (begin (port-count-lines! in) (set-port-next-location! in start-line 0 1))
    (values start-line
            (for/list ([form (in-port (lambda (in) (scribble-read-syntax path in)) in)])
              form))))

(define-syntax (include-code-block stx)
  (syntax-case stx ()
    [(_ path label mklabel-expr)
     (let ()
       (define-values (start-line lines)
         (parameterize ((current-syntax-context stx))
           (get-code-lines (syntax-e #'path) (syntax-e #'label))))
       (with-syntax ([(line ...) lines])
         #`(let ([mklabel mklabel-expr])
             (filebox
              (if mklabel (mklabel path (quote #,(add1 start-line))) path)
              (codeblock #:context (quote-syntax #,(datum->syntax #'path 'HERE #'path))
                         #:keep-lang-line? #f ;; HACK to eliminate first "\n"!
                         #:line-numbers (quote #,start-line)
                         #:line-number-sep 2
                         (~@ "\n" line) ...)))))]))

(define-syntax (include-examples stx)
  (syntax-case stx ()
    [(_ path label mklabel-expr [prefix ...])
     (let ()
       (define-values (start-line forms)
         (parameterize ((current-syntax-context stx))
           (get-code-forms (syntax-e #'path) (syntax-e #'label))))
       (with-syntax ([(form ...) (for/list ([form (in-list forms)])
                                   (replace-context #'path form))])
         #`(let ([mklabel mklabel-expr])
             (filebox
              (if mklabel (mklabel path (quote #,(add1 start-line))) path)
              (examples #:label #f prefix ... form ...)))))]))
