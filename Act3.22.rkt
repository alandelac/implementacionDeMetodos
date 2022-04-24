#lang racket
;Autor: Mario Jáuregui Gómez
;Matricula: A00832049

(define tipo-color '(("ciclo" "FF7F00")
            ("condi" "6D01DC")
            ("dato" "000000")
            ("palabra" "FF00DC")
            ("comentario" "676767")
            ("funciones" "EEBB00")
            ("simb ari" "0F00E0")
            ("simb ex" "33DFFF")))
;ENCUENTRA EL COLOR CORRESPONDIENTE
(define (color lst elem)
  (if (empty? lst)
      "no esta el elem"
      (if (equal? elem (first (car lst)))
          (second (car lst))
          (color (cdr lst) elem))))
;Función para enseñar en el documento de output el resultado
;Tambien cuenta la cantidad de tokens que se encontraron en el documento
(define (imprime lista p2)
  ;Aqui se añade 1 a la cantidad de tokens que hay, además de que se van imprimiendo en el documento
  ;todos los valores
  (define (despliega lista p2)
    
        (if (string? (second lista))
                   (display (string-append "<span style = \"" "color: " (color tipo-color (first lista)) "\">" (second lista) "</span> ") p2)
                   (display (string-append "<span style = \"" "color: " (color tipo-color (first lista)) "\">" (number->string (second lista)) "</span> ") p2))
    1)
  ;Es nuestra condición para el método recursivo, si la lista esta vacía significa que ya acabamos
  (if (empty? lista)
      0
      ;Si el elemento que queremos añadir tiene una longitud mayor a dos, significa que es una lista
      ;y por ende tendremos que llamar a la otra función, esto para que podamos imprimir los parentesis y su contenido de manera correcta
      (if (< (length (first lista)) 2)
          (display (first lista))
          ;Llamada a funcion para parentesis
          (if (> (length (first lista)) 2)
                 (+ (imprime2 (first lista) p2) (imprime (cdr lista) p2))
                 ;Llamada recursiva del resto de la lista
                 (+ (despliega (car lista) p2) (imprime (cdr lista) p2))))))

;Función para imprimir el contenido de parentesis
(define (imprime2 lista p2)
  (define (despliega2 lista p2)
   
       
        (if (string? (second lista))
                   (display (string-append "<span style = \"" "color: " (color tipo-color (first lista)) "\">" (second lista) "</span> ") p2)
                   (display (string-append "<span style = \"" "color: " (color tipo-color (first lista)) "\">" (number->string (second lista)) "</span> ") p2))
    1)
  (if (empty? lista)
      0
      ;Aqui se despliegan los dos elementos de la lista y luego se hace el cddr, ya que estamos quitando los primeros dos elementos de la lista, porque ya los imprimimos
      (+ (despliega2 (list (first lista) (second lista)) p2) (imprime2 (cddr lista) p2))))
;Funcion principal para leer documento de input
(define (convierte atomo p1)
  ;Checa si es un número y su tipo
  (if (symbol? atomo)
      (set! atomo (symbol->string atomo))
      ;atomo = symbol-> string atomo
      0)
  (if (number? atomo)
      (if (integer? atomo)
          (list "\nENTERO:" atomo)
          (list "\nREAL:"atomo))
      ;Checa el resto de los casos
      (cond [(list? atomo)(append(list "simb ex" "(") (recorre-paren atomo p1))]
            ;Con la función de read line, puede guardar todo lo que se encuentra despues del simbolo
            [(regexp-match? #rx"//" atomo)(list "comentario" (string-append "//" (read-line p1)))]
            ;Con la función de recorre paren, se guarda todo lo que esta adentro del parentesis en una list of list
            [(regexp-match?  #rx"int|float|double|long|byte|boolean|string|switch|case|class|private|break|return|#include|public|char|cin|cout|endl|void|using|namespace" atomo) (list "palabra" atomo)]
            [(regexp-match? #rx"for|while" atomo) (list "ciclo" atomo)]
            [(regexp-match? #rx"=|\\+|\\*|/|-|%|;" atomo) (list "simb ari" atomo)]
            [(regexp-match? #rx"if|>|<|==|<=|>=" atomo) (list "condi" atomo)]
            [(regexp-match? #rx":|;|{|}|[|]|\\." atomo) (list "simb ex" atomo)]
            ;Su no es nada, entonces es una variable
            [else (list "dato" atomo)])))
;Es la funcion para guardar toda la info dentro del parentesis
;Regresa una lista donde estan todos los valores que se encuentran en los parentesis con su token respectivo
(define (recorre-paren lista p1)
  (if (empty? lista)
      (list "simb ex" ")")
      (append (convierte (car lista) p1) (recorre-paren(cdr lista) p1))))
;Funcion que recorre todo el documento y va leyendo los valores
(define (recorre-2 p1)
  
 
  (if (eof-object? (peek-char p1))
      '()
      
        (if (char-whitespace? (peek-char p1))
      (and(read-char p1)(recorre-2 p1))
               (append (list (convierte (read p1) p1)) (recorre-2 p1)))))
;Hace todas las llamadas a las funciones para poder trabajar con los dos documentos
(define (recorre file1 file2)
  ;Abre el documento a leer 
  (define p1 (open-input-file file1))
  ;Crea el documento de output
  (define p2 (open-output-file file2))
  ;Guarda todos los valores dentro del documento
  (define lista (recorre-2 p1))
  ;Linea para formato de documento
  (display "<html>" p2)
  ;Enseña los resultados de dichos valores, además de ir contandolos
  (define cantidad (imprime lista p2))
  ;Imprime la cantidad final de tokens que fueron encontrados en el documento a leer
  (display "</html>" p2)
  ;(display (+ x (cuenta-atomo lista)) p2)
  (close-output-port p2)
  (close-input-port p1)
  )
;Funcion principal para el programa, si se quieren realizar múltiples pruebas,
;se tiene que cambiar el nombre del documento de output o borrar en el explorador de archivos
(define(colorear archivo archivo2)
  (recorre archivo archivo2))