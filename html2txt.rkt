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

(define (ignored-tag? tag)
  (member tag ignored-tags))
(define (space-tag? tag)
  (member tag space-tags))
(define (br-tag? tag)
  (member tag br-tags))
(define (list-tag? tag)
  (member tag list-tags))

(define (href-parser contents)
  (match contents
    [(cons 'link _) (list)]
    [(cons 'href text)
     (list (car text) "\n")]
    [_ #f]))

(define (parse-links contents) 
  (cond [(empty? contents) ""]
        [(list? contents)
         (apply string-append
                (or (href-parser contents)
                    (list (parse-links (car contents)) 
                          (parse-links (cdr contents)))))]
        [else ""]))

(define (tag-parser contents)
  (match contents
    [(cons (? ignored-tag?) text)
     (list)]
    [(cons (? space-tag?) text)
     (list (parse-xexp text) " ")]
    [(cons (? br-tag?) text)
     (list (parse-xexp text) "\n")]
    [(cons (? list-tag?) text)
     (list "\n" (parse-xexp text) "\n")]
    [(cons 'td text) (list (parse-xexp text) "\t")]
    [(cons 'li text) (list "* " (parse-xexp text) "\n")]
    [(cons 'a text) (list " [" (parse-xexp text) "]")]
    [_ #f]))

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
       (apply string-append
              (let ([xexp (file->xexp filename)])
                (list
                 (parse-xexp xexp)
                 (parse-links xexp))))))))

(h2t "/tmp/index.html")


