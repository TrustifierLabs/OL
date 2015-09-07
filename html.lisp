#!/bin/ol
(import (lib http)
   (owl pinvoke))
; этот сервер будет делать вполне простую работу - показывать статистику использования памяти
; http://habrahabr.ru/company/yandex/blog/265569/

; syscalls
(define (yield) (sys-prim 1022 0 #false #false))
(define (mem-stats) (syscall 1117 #f #f #f))
(define (asctime) (syscall 1118 #f #f #f))
(define (time format seconds) (syscall 201 format seconds #f))

; 1 раз в 30 секунд соберем статистику по использованию памяти
; todo: ограничить список N элементами
(fork-server 'memory-stats-collector (lambda ()
(let ((seconds 0))
(let loop ((stats '()) (seconds seconds))
   (let ((envelope (check-mail)))
      (if envelope
         (let* ((sender message envelope))
            (mail sender stats))))
   (yield)
   (let ((ss (time #false #f)))
      (if (> (- ss seconds) 29)
         (let ((mem (mem-stats)))
            (loop (cons (cons (time "%H%M%S\t" ss) mem) stats) ss))
         (loop stats seconds)))))))


(http:run 8080 (lambda (request headers send)
   (print "Request: " request)
   (print "Headers: " headers)
   (send "HTTP/1.1 200 OK\nContent-Type: text/html; charset=UTF-8\n")
   (send "Server: OL/1.0\n")
   (send "\n")

   (let* ((url (cadr request))
          (url (if (string-eq? url "/") "/index.html" url)))
      (cond
         ((string-eq? url "/index.html")
            (send (runes->string (file->list (str-app "./tutorial/2. Web Server" url)))))
         ((string-eq? url "/data.tsv")
            (let ((mem (interact 'memory-stats-collector #f)))
               (send "date\tGeneration\tAllocated\tTotal Size\t\n")
               (let loop ((mem mem))
                  (if (null? mem)
                     null
                     (begin
                        (send (car (car mem)))
                        (for-each (lambda (x) (send "\t" x)) (cdr (car mem)))
                        (send "\n")
                        (loop (cdr mem)))))))
         (else
            (send "<HTML><BODY>")
            (send "<h1>url:" url "</h1>")
            (send "Status: <pre>" (runes->string (file->list "/proc/self/status")) "</pre>")

            (send "</BODY></HTML>"))))
))