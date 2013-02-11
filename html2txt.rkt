#lang racket

(provide h2t)

(require (planet neil/html-parsing:2:0))

(define (file->xexp filename)
  (html->xexp 
   (open-input-file #:mode 'text filename)))

(define ignored-tags '(*COMMENT* *DECL* @ script style))
(define br-tags '(table title br div p form select tr pre))
(define space-tags '(span option))
(define list-tags '(ol ul))

(define links '())

(define (tag-parser contents)
  (let ([tag (car contents)]
        [text (cdr contents)])
    (cond [(member tag ignored-tags) '()]
          [(member tag space-tags) (list (parse-xexp text) " ")]
          [(member tag br-tags) (list (parse-xexp text) "\n")]
          [(member tag list-tags) (list "\n" (parse-xexp text) "\n")]
          [(eq? tag 'td) (list (parse-xexp text) "\t")]
          [(eq? tag 'li) (list "* " (parse-xexp text) "\n")]
          [(eq? tag 'a)
             (list "[" (parse-xexp text) "]")]
          [else #f])))

(define (href-parser contents)
    (let ([tag (car contents)]
          [text (cdr contents)])
      (if (eq? tag 'href)
          (list (car text) "\n")
          #f)))
      

(define (parse-links contents) 
  (cond [(empty? contents) ""]
        [(list? contents)
         (apply string-append
                (or (href-parser contents)
                    (list (parse-links (car contents)) 
                          (parse-links (cdr contents)))))]
        [else ""]))

(define (despace str)
  (regexp-replace* #rx"^[ \t\n]{2,}|[ \t\n]+$" str ""))

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

(define (h2t filename)
  (with-output-to-file (string-append filename ".txt")  #:exists 'replace
    (lambda ()
      (display
       (let ([xexp (file->xexp filename)])
         (list
          (parse-xexp xexp)
          (parse-links xexp)))))))



(h2t "/tmp/index.html")


