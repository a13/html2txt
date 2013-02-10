#lang racket

(require (planet neil/html-parsing:2:0))

(define parsed
  (html->xexp 
   (open-input-file #:mode 'text "/tmp/index.html")))

(define ignored-tags '(*COMMENT* *DECL* @ script style))
(define br-tags '(table title br div p form select tr))
(define space-tags '(span option))

(define (tag-parser contents)
  (let ([tag (car contents)]
        [text (cdr contents)])
    (cond [(member tag ignored-tags) '("")]
          [(member tag space-tags) (list (parse-xexp text) " ")]
          [(member tag br-tags) (list (parse-xexp text) "\n")]
          [(eq? tag 'td) (list (parse-xexp text) "\t")]
          [(eq? tag 'a)
           (list "[" (parse-xexp text) "]")]
          [else #f])))


(define (despace str)
  (regexp-replace* #rx"^[ \t\n]+|[ \t\n]+$" str ""))

(define (parse-xexp contents)
  (cond [(empty? contents) ""]
        [(list? contents)
         (apply string-append
                (or (tag-parser contents)
                    (list (parse-xexp (car contents)) 
                          (parse-xexp (cdr contents)))))]
        [(string? contents) 
         (despace contents)]
        [else ""]))

(with-output-to-file "/tmp/output" 
  (lambda () (display (parse-xexp parsed))) #:exists 'replace)


