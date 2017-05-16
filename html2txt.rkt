#lang racket

(provide h2t)

(require (planet neil/html-parsing:3:0))

(define file->xexp
  (compose html->xexp (curry open-input-file #:mode 'text)))

(define ignored-tags '(*COMMENT* *DECL* @ script style))
(define br-tags '(table title br div p form select tr pre))
(define space-tags '(span option))
(define list-tags '(ol ul))

(define (parse-links contents)
  (match contents
    [(cons 'link _) ""]
    [(list 'href text)
     (string-append text "\n")]
    [(cons head tail)
     (string-append
      (parse-links head)
      (parse-links tail))]
    [_ ""]))

;; генерим хэш вида '(тэг . суффикс) или '(тэг . (префикс . суффикс))
(define tags-assocs
  (make-hash
   (append
    (map (curryr cons 'ignore)
         ignored-tags)
    (map (curryr cons " ")
         space-tags)
    (map (curryr cons "\n")
         br-tags)
    (map (curryr cons '("\n" . "\n"))
         list-tags)
    '((td . "\t")
      (li . ("* " . "\n"))
      (a . (" [" . "]"))))))

(define (despace str)
  (if (string? str)
      (regexp-replace* #rx"^[ \t\n]{2,}|[ \t\n]+$" str "")
      ""))

(define parse-xexp
  (match-lambda
    [(cons head tail)
     (match (dict-ref tags-assocs head #f)
       ['ignore ""]
       [#f (string-append (parse-xexp head) (parse-xexp tail))]
       [(cons prefix suffix)
        (string-append prefix (parse-xexp tail) suffix)]
       [suffix (string-append (parse-xexp tail) suffix)])]
    [wtf (despace wtf)]))

(define (h2t filename)
  (with-output-to-file
      (string-append filename ".txt") #:exists 'replace
      (lambda ()
        (display
         (let ([xexp (file->xexp filename)])
           (string-append
            (parse-xexp xexp)
            (parse-links xexp)))))))


;; (h2t "/tmp/index.html")
