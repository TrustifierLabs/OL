;; vm rimops

;; todo: convert tuple to variable arity
;; todo: convert arity checks 17 -> 25

(define-library (owl primop)
   (export
      (exports (lang vm))

      ;; extra ops
      halt wait exec

      set-ticker-value
      set-memory-limit get-word-size get-memory-limit start-seccomp)

   (import
      (r5rs core)
      (lang vm))

   (begin

;; Список syscall'ов
; поэтапный перевод syscall'ов в syscall'ы
; 1. добавить 100 к старым номерам
; 2. завести правильные новые
; 3. удалить старые

;      (define (__fsend) (syscall 0 #false #false #false))
;      1 __fopen
;      2 __close
;      3 __sopen
;      4 __accept

;      5 __fread
;      +6 __exit
;      +7 __set-memory-limit
;      +8 __get-machine-word-size
;      +9 __get-memory-limit
;      +10 __enter-linux-seccomp
;      +22 __set-ticker-value

;      30 __dlopen
;      31 __dlsym
;      32 __pinvoke
;      33 __gc ; TEMP

;      11 __sys-open-dir
;      12 __sys-read-dir
;      13 __sys-closedir
;      14 __set-ticks
;      15 __fsocksend
;      16 __getenv
;      17 __exec[v]
;      20 __chdir
;      19 wait <pid> <respair>
;      18 fork
;      21 kill


      ;; used syscalls
      (define (exec function . args) (syscall 59 function args #false))

      ;; special things exposed by the vm
      (define (set-memory-limit n) (syscall 1007 n n n))
      (define (get-word-size)      (syscall 1008 #false #false #false))
      (define (get-memory-limit)   (syscall 1009 #false #false #false))
      (define (start-seccomp)      (syscall 1010 #false #false #false)) ; not enabled by defa

      ;; stop the vm *immediately* without flushing input or anything else with return value n
      (define (halt n)             (syscall 60 n n n))
      ;; make thread sleep for a few thread scheduler rounds
      (define (set-ticker-value n) (syscall 1022 n #false #false))
      (define (wait n)
         (if (eq? n 0)
            n
            (let* ((n _ (vm:sub n 1)))
               (set-ticker-value 0)
               (wait n))))


; проверку типов вынесем на уровень компилятора!
; можно и в отдельный файл
;      ; from interop.scm
;      (define (interop op a b)
;         (call/cc (λ (resume) (sys resume op a b))))
;      (define (error reason info)
;         (interop 5 reason info))
;      (define (pair? x) (eq? type-pair (type x))) ; list.scm
;      (define (fixnum? x)
;         (let ((t (type x)))
;            (or
;               (eq? t type-fix+)
;               (eq? t type-fix-)
;               )))
;
;
;
;      (define (set-car! object value)
;         (if (and (pair? object) (fixnum? value))
;            (set-car! object value)
;            (error "set-car! first argument is not a pair")))
;      (define (set-cdr! object value)
;         (if (and (pair? object) (fixnum? value))
;            (set-cdr! object value)
;            (error "set-car! first argument is not a pair")))


))
