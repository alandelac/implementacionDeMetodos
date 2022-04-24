#lang racket
;Santiago Minga
;Daniel Loredo
;Test-Regex

;JSReader
(define (JSreader infile ) ;outfile
  (define in(open-input-file infile))
  ;(define out(open-output-file outfile))
  (define lst(read_file in))
  (write lst)
  (display lst)
  ;(html_print lst out)
  (close-input-port in))
  ;(close-output-port out))
;read
(define (read_file file)
  (if (eof-object? (peek-char file))
      (list)
      (if(char-whitespace?(peek-char file))
         (and(read-char file)(read_file file))
         (if (not(equal?(regexp-match #rx"/\\*" (peek-string 2 0 file)) #f)) ;Comentarios Multilinea
             (append(list(append(regexp-match  #rx"/\\*.*?\\*/" file)(list "Comentario Multilinea-green")))(read_file file))
             (if (not(equal?(regexp-match #rx"//" (peek-string 2 0 file)) #f)) ;Comentarios 
                 (append(list(append(regexp-match #rx"//.*?\n" file)(list "Comentario-green"))) (read_file file))
                 (if(not(equal?(regexp-match #rx"var|const|let|float|int|boolean|return|in" (peek-string 7 0 file)) #f)) ;Palabras reservadas
                    (append(list(append(regexp-match #rx"var|const|let|float|int|boolean|return|in" file) (list "Identificador-purple"))) (read_file file))
                    (if(not(equal?(regexp-match #rx"\"" (peek-string 2 0 file)) #f)) ;Strings
                       (append(list(append(regexp-match #rx"\".*\"" file) (list "String-green"))) (read_file file))
                       (if(not(equal?(regexp-match #rx"-?[1234567890]" (peek-string 2 0 file)) #f)) ;Numeros
                          (append(list(append(regexp-match #rx"-?[1234567890]*[\\.e]?-?[1234567890]*" file) (list "Numero -red"))) (read_file file))
                          (if(not(equal?(regexp-match #rx"if|for" (peek-string 3 0 file)) #f)) ;Delimitadores
                             (append(list(append(regexp-match #rx"if(.*)\n?{.*}|for(.*)\n?{.*}" file) (list "Delimitadores"))) (read_file file))
                             (if(not(equal?(regexp-match #rx"function" (peek-string 8 0 file)) #f)) ;Funciones
                                (append(list(append(regexp-match #rx"function .*(.*?)\n{.*}" file) (list "Funcion"))) (read_file file))
                                (append(list(append(list(read file)) (list "otro-black"))) (read_file file))))))))))))

;html_print
(define (html_print lst out)
  (list))


;ignorar
; |
; V
;mult-line-comment          
;identify
(define (identify line)
  (regexp-replace #rx" " line "\n")
  (define ch(open-input-string line))
  (identify_helper ch))
  
(define (identify_helper ch)
  (if (eof-object? (peek-char ch))
      (list)
      (if(not(equal?(regexp-match #rx"var|const|let" (peek-string 6 0 ch)) #f))
         (display(append(list(append(regexp-match #rx"var.*\r|const|let" (read-line ch))(list "Identificador")))(identify_helper ch)))
         (if(not(equal?(regexp-match #rx".=|=|\\+|\\*|/|-|%"  (peek-string 6 0 ch)) #f))
            (append(list(append(regexp-match #rx".=|=|\\+|\\*|/|-|%" (read-line ch))(list "Operador")))(identify_helper ch))
            (append(list(read-line ch))(identify_helper ch))))))
  
            
;operation
(define (operation ch)
  (list))

;var_list
(define (var_list ch)
  (list))
(JSreader "CasoPrueba1.txt")

          
          
          



