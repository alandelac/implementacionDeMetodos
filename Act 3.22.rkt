#lang racket
;Autores: Mario Jáuregui Gómez, Alan de la Cruz Téllez
;Matricula: A00832049, A01280638

;Nuestro diccionario con los colores
(define tipo-color '(("ciclo" "FF7F00")
            ("condi" "6D01DC")
            ("dato" "000000")
            ("palabra" "FF00DC")
            ("comentario" "676767")
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
      ;Si es menor de dos, se supone que es una lista que contiene un salto de línea
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
  ;Cambia el simbolo a un string
  (if (symbol? atomo)
      (set! atomo (symbol->string atomo))
      0)
  ;Checa si es un número y su tipo
  (if (number? atomo)
      (if (integer? atomo)
          (list "\nENTERO:" atomo)
          (list "\nREAL:"atomo))
      ;Checa el resto de los casos
      (cond ;Con la función de recorre paren, se guarda todo lo que esta adentro del parentesis en una list of list
        [(list? atomo)(append(list "simb ex" "(") (recorre-paren atomo p1))]
            ;Con la función de read line, puede guardar todo lo que se encuentra despues del simbolo
            [(regexp-match? #rx"//" atomo)(list "comentario" (string-append "//" (read-line p1)))]
            [(regexp-match?  #rx"int|float|double|long|byte|boolean|string|switch|case|class|private|break|return|#include|public|char|cin|cout|endl|void|using|namespace" atomo) (list "palabra" atomo)]
            [(regexp-match? #rx"for|while" atomo) (list "ciclo" atomo)]
            [(regexp-match? #rx"=|\\+|\\*|/|-|%|;" atomo) (list "simb ari" atomo)]
            [(regexp-match? #rx"if|>|<|==|<=|>=" atomo) (list "condi" atomo)]
            [(regexp-match? #rx":|;|{|}|[|]|\\." atomo) (list "simb ex" atomo)]
            ;Su no es nada, entonces es una variable
            [else (list "dato" atomo)])))
;Es la funcion para guardar toda la info dentro del parentesis
;Regresa una lista donde estan todos los valores que se encuentran en los parentesis con su color respectivo
(define (recorre-paren lista p1)
  (if (empty? lista)
      (list "simb ex" ")")
      (append (convierte (car lista) p1) (recorre-paren(cdr lista) p1))))
;Funcion que recorre todo el documento y va leyendo los valores
(define (recorre-2 p1)
  (if (eof-object? (peek-char p1))
      '()
      (append (list (convierte (read p1) p1)) (recorre-2 p1))))
;Hace todas las llamadas a las funciones para poder trabajar con los dos documentos
(define (recorre file1 file2)
  ;Abre el documento a leer
  (define p1 (open-input-file file1))
  ;Crea el documento de output
  (define p2 (open-output-file file2))
  ;Guarda todos los valores dentro del documento
  (define lista (recorre-2 p1))
  ;Para que se vea mejor el código de html
  (display "<html>" p2)
  ;Enseña los resultados de dichos valores, además de ir contandolos
  (define cantidad (imprime lista p2))
  ;Para que se vea mejor el código de html
  (display "</html>" p2)
  (close-output-port p2)
  (close-input-port p1)
  )
;Funcion principal para el programa, si se quieren realizar múltiples pruebas,
;se tiene que cambiar el nombre del documento de output o borrar en el explorador de archivos
(define(colorear archivo archivo2)
  (recorre archivo archivo2))