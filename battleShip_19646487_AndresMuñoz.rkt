
#lang r6rs
(import (rnrs))
(define null (list))


;--------------------------------------------------------------------------------------------------
;                    RANDOM
;Estas constantes fueron sacadas de https://en.wikipedia.org/wiki/Linear_congruential_generator
(define a 1103515245)
(define c 12345)
(define m 2147483648)
;Esta función random tuma un xn y obtiene el xn+1 de la secuencia de números aleatorios.
(define myRandom
  (lambda
    (xn)
    (mod (+ (* a xn) c) m)
  )
)
;Cada vez que pedimos un random, debemos pasar como argumento el random anterior.


;Acá un ejemplo que permite generar una lista de números aleatorios.
;Parámetros:
;* "cuantos" indica el largo de la lista a generar.
;* "xActual" valor actual del random, se pasa en cada nivel de recursión de forma actualizada
;* "maximo" Los números generados van desde 0 hasta maximo-1
(define getListaRandom
  (lambda (cuantos xActual maximo)
    (if (= 0 cuantos)
        '()
        (let ((xNvo (myRandom xActual)))
          (cons (mod xNvo maximo)
              (getListaRandom (- cuantos 1) xNvo maximo)
          )
        )
    )
  )
)
;--------------------------------------------------------------------------------------------------

;--------------------------------------------------------------------------------------------------
;      MANEJO DE LISTAS
;funcion setElemList
;entrada una lista L un indice i y un elemento e
;salida: nueva lista con el elemento indicado cambiado en el indice indicado
;recursion: lineal
(define setElemList
  (lambda (L i e)
    (if (null? L)
      null
      (if (= i 0)
          (cons e (cdr L))
          (cons (car L) (setElemList (cdr L) (- i 1) e))
       )
      )
    )
)

;funcion addList
;entrada: una lista L y un elemento e
;salida: nueva lista con el elemento e agregado al final de la lista
(define addList
  (lambda (L e)
    (append L (list e)))
  )

;-------------------------------------------------------------------------------------------------------------------
;                             TDA MATRIZ

;CONTRUCTORES
;-------------------------------------------------------------------------------------------------------------
                             ;funciones utilizadas en createBoardRL (recursion lineal)
;funcion createFilasRL
;entrada m, se refiere a la cantidad de caracteres
;salida: lista con m caracteres dentro
;recursion: lineal
(define createFilasRL
  (lambda (m)
    (if (= m 0)
      null
      (cons  #\. (createFilasRL (- m 1)) )
      )
    )
 )
;funcion createMAtrizRl
;entrada: n m, refiere a la cantidad de filas y columnas
;salida: lista de n listas que contienen m caracteres
;recursion: lineal
(define createMatrizRL
  (lambda (n m)
    (define createBaseRLAux
      (lambda (n m)
        (if (= n 0 )
            null
            (cons (createFilasRL m) (createBaseRLAux (- n 1) m)))
        )
      )
    (if (and (> n 0) (> m 0))
        (createBaseRLAux n m)
        null)
    )
  
  )
;------------------------------------------------------------------------------------
                     ;funciones utilizadas en createBoardRC (recursion de cola)

;funcion createFilasRC
;entrada m, se refiere a la cantidad de caracteres
;salida: lista con m caracteres dentro
;recursion: cola
(define createFilasRC
  (lambda (m)
    (define crearListaRCAux
      (lambda (m lista)
        (if (= m 0)
            lista
            (crearListaRCAux (- m 1) (addList lista  #\.)))
        )
      )
    (crearListaRCAux m '())
    ) 
  )

;funcion createMAtrizRC
;entrada: n m, refiere a la cantidad de filas y columnas
;salida: lista de n listas que contienen m caracteres
;recursion: cola
(define createMatrizRC
  (lambda (n m)
    (define createBaseRCAux
      (lambda (n m lista)
        (if (= n 0 )
            lista
            (createBaseRCAux (- n 1) m (addList lista (createFilasRC m)) )
            )
        )
      )
    (createBaseRCAux n m '())
    )
  )

;PERTENENCIA

;funcion check-chars
;entrada: L, refiere a una lista
;salida: #t en caso de que la lista contenga solo caracteres,#f en caso contrario
;recursion: de cola
(define check-chars
  (lambda (L)
    (define verificarAux
      (lambda (L)
        (if (null? L)
            #t
            (if(char? (car L))
               (verificarAux (cdr L))
               #f
               )
            )
        )
      )
    (if (null? L)
        #f
        (verificarAux L)
        )
    )
  )

;funcion check-dimension-and-chars
;entrada: matriz
;salida: #t si se cumple que tiene solo caracteres y ademas que hay la misma cantidad en todas las listas, #f en caso contrario
;recursion: de cola
(define check-dimension-and-chars
  (lambda (matriz)
    (let ((tamano (get-columnas-matriz matriz)))
      (define aux
        (lambda (board)
          (if (null? board)
              #t
              (if (and (= tamano (length (car board)))
                       (check-chars (car board)))
                  (aux (cdr board))
                  #f)
              )
          )
        )
      (aux matriz)
      )
    )  
  )
;funcion filas-par?
;entrada: matriz, refiere a lista de listas
;salida: #t en caso de que la cantidad de filas sea par
(define filas-par?
  (lambda (matriz)
    (if (= (mod (length matriz) 2) 0)
        #t
        #f
        )
    )
  )
;funcion list-of-list?
;entrada; matriz
;salida: #t en caso de que la matriz se componga por lista de listas
;recursion de cola
(define list-of-list? 
  (lambda (matriz)
    (define aux
      (lambda (matriz)
        (if (null? matriz)
            #t
            (if (list? (car matriz))
                (if (null? (car matriz))
                    #f
                    (aux (cdr matriz))
                    )
                #f
                )
            )
        )
      )
    (if (list? matriz)
        (if (null? matriz)
            #f
            (aux matriz))
        
        #f
        )
    )
  )
;funion: matriz?
;entrada: una matriz
;salida: #t en caso de cumplir con todas las condisiones para que sea matriz
(define matriz?
  (lambda (matriz)
    (if (list-of-list? matriz)
        (if (filas-par? matriz)
            (if (check-dimension-and-chars matriz)
                #t
                #f)
            #f)
        #f
        )
    )
  )

;SELECTORES
;funcion get-caracter-matriz
;entrada: matriz, n , m , donde n refiere a la fila , y m a la columna de la cual se desea obtener el caracter
;salida: caracter de la posicion, en caso de que la posicion sea invalida se entrega el caracter #\N
(define get-caracter-matriz
  (lambda (matriz n m)
    (if (and (>= n 0) (>= m 0))
        (list-ref (list-ref matriz n) m)
        #\N
        )
    )
  )

;funcion get-filas-matriz
;entrada: matriz
;salida: numero de filas de la matriz
(define get-filas-matriz
  (lambda (matriz)
    (length matriz)
    )
  )


;funcion get-columnas-matriz
;entrada: matriz
;salida: numero de columnas de la matriz
(define get-columnas-matriz
  (lambda (matriz)
    (length (car matriz))
    )
  )

;MODIFICADORES
;funcion set-elem-matriz
;entrada: matriz , n , m , elem, n y m refieren a las coordenadas que se quiere cambiar el caracter elem
;salida: matriz nueva con el un caracter cambiado
(define set-elem-matriz
  (lambda (matriz n m elem)
    (setElemList matriz n (setElemList (list-ref  matriz n) m elem))
    )
  )





;OPERACIONES SOBRE MATRIZ
;funcion put-ships-in-matriz
;entrada: matriz, positions. position se refiere a las posiciones donde se ubicaran barcos
;salida: nueva matriz con los barcos puestos
;recursion de cola
(define put-ships-in-matriz
  (lambda (matriz positions)
    (if (null? positions)
      matriz
      (put-ships-in-matriz (set-elem-matriz matriz (get-x-position (car positions)) (get-y-position (car positions)) #\E) (cdr positions))
      )
    )
  )



;####################################################################################################################
;                     TDA BOARD

;CONTRUCTORES
;--------------createBoard con recursion linal-------------------

;funcion createBoardRL
;entrada: N, M, ships, seed. N y M refieren a las dimensiones de la matriz, ships las posiciones donde se pondran los barcos enemigos, seed la semilla para utilizar random
;salida: board
;recursion lineal
(define createBoardRL
  
  (lambda (N M ships seed)
    (if (and (number? N) (number? M) (number? ships) (number? seed))
        (if (and (> N 0) (> M 0))
            (if (= (mod N 2) 0)
                (if (< ships (* (/ N 2) M))
                    (list ships 0 0 0 (put-ships-in-matriz (createMatrizRL N M)
                                                           (createListPositions ships N M '() seed )) '() )
                    null)
                null)
            null)
        null)
    )
  )
;--------------createBoard con recursion de cola-------------------
;funcion createBoardRC
;entrada: N, M, ships, seed. N y M refieren a las dimensiones de la matriz, ships las posiciones donde se pondran los barcos enemigos, seed la semilla para utilizar random
;salida: board
;recursion de cola
(define createBoardRC
  (lambda (N M ships seed)
    (if (and (number? N) (number? M) (number? ships) (number? seed))
        (if (and (> N 0) (> M 0))
            (if (= (mod N 2) 0)
                (if (< ships (* (/ N 2) M))
                    (list ships 0 0 0 (put-ships-in-matriz (createMatrizRC N M)
                                                           (createListPositions ships N M '() seed )) '() )
                    null)
                null)
            null)
        null)
    )
  )
;------------------------------------------------------------------------------




;PERTENENCIA
;funcion check-length
;entrada: board
;salida: #t si la cantidad de elementos del board son correctas, (debe tener 6 elementos),#f en caso contrario
(define (check-length board)
  (if (= (length board) 6)
      #t
      #f
      )
  )

;funcion board?
;entrada: board
;salida: #t si cumple con las condiciones de que sea un board

(define board?
  (lambda (board)
    (if (list? board)
        (if (check-length board)
            (if (and (number? (get-ships-cpu board))
                     (number? (get-ships-player board))
                     (number? (get-score-cpu board))
                     (number? (get-score-player board))
                     (matriz? (get-matriz board))
                     (list? (get-record board))
                     
                     )
                #t
                #f
                )
            #f
            )
        #f)
    )
  )



;SELECTORES
;funcion get-record
;entrada: board
;salida: historial de las jugadas 
(define get-record
  (lambda (board)
    (cadr (cddddr board))))

;funcion get-matriz
;entrada: board
;salida: matriz contenida en el board
(define get-matriz
  (lambda (board)
    (car  (cddddr board))))

;funcion get-score-player
;entrada: board
;salida: puntaje del usuario
(define get-score-player
  (lambda (board)
    (car (cdddr board))))

;funion get-score-cpu
;entrada: board
;salida: puntaje de la computadora
(define get-score-cpu
  (lambda (board)
    (car (cddr board))))

;funcion get-ships-player
;entrada: board
;salida: cantidad de barcos del usuario
(define get-ships-player
  (lambda (board)
    (car (cdr board))))

;funcion get-ships-cpu
;entrada: board
;salida: cantidad de barcos de la computadora
(define get-ships-cpu
  (lambda (board)
    (car board)))
  


;MODIFICADORES;

;funcion set-ships-cpu
;entrada: board num, num refiere al numero de barcos
;salida: nuevo board, el numero de barcos de la cpu es num
(define set-ships-cpu
  (lambda (board num)
    (setElemList board 0 num)))
  
;funcion set-ships-player
;entrada: board num, num refiere al numero de barcos
;salida: nuevo board, el numero de barcos del usuario es num
(define set-ships-player
  (lambda (board num)
    (setElemList board 1 num)))
  
;funcion set-score-cpu
;entrada: board, num. num refiere al nuevo score
;salida: board que se le cambio el score de la computadora
(define set-score-cpu
  (lambda (board num)
    (setElemList board 2 num)))
  
;funcion set-score-player
;entrada: board, num. num refiere al nuevo score
;salida: board que se le cambio el score del usuario
(define set-score-player
  (lambda (board num)
    (setElemList board 3 num)))

;funcion sumar-score-cpu
;entrada: board
;salida; nuevo board sumando 1 al puntaje de la computadora
(define sumar-score-cpu
  (lambda (board)
    (set-score-cpu board (+ (get-score-cpu board) 1))
    )
  )



;funcion sumar-score-player
;entrada: board
;salida; nuevo board sumando 1 al puntaje del usuario
(define sumar-score-player
  (lambda (board)
    (set-score-player board (+ (get-score-player board) 1))
    )
  )

;funcion restar-ship-player
;entrada: board
;salida: nuevo board restando 1 a la cantidad de barcos del usuario
(define restar-ship-player
  (lambda (board)
    (set-ships-player board (- (get-ships-player board) 1))
    )
  )

;funcion restar-ship-cpu
;entrada: board
;salida: nuevo board restando 1 a la cantidad de barcos de la computadora
(define restar-ship-cpu
  (lambda (board)
    (set-ships-cpu board (- (get-ships-cpu board) 1))
    )
  )


;funcion set-matriz
;entrada: board, matriz
;salida: board con la nueva matriz
(define set-matriz
  (lambda (board matriz)
    (setElemList board 4 matriz)))

;funcion set-record
;entrada: board, record. record refiere al nuevo historial
;salida: board con el record nuevo
(define set-record
  (lambda (board record)
    (setElemList board 5 record)))

;funcion addRecord
;entrada: board, lastplay. lastplay refiere a la ultima jugada
;salida: board con ultima jugada agregada en el historial
(define addRecord
  (lambda (board lastplay)
    (set-record board (addList (get-record board) lastplay))
    )
  )
;funcion set-elem-matriz-on-board
;entrada: board n m e, n y m refiere a las coordenaas y e el nuevo elemento
;salida: neuvo board el cual tiene una matriz nueva que se le cambio a la coordenada n,m el elemento e
(define set-elem-matriz-on-board
  (lambda (board n m e)
    (set-matriz board (set-elem-matriz (get-matriz board) n m e))))


;OPERACIONE SOBRE BOARD

;---------------------------------------------------------------------------------------------------
                            ;funciones utilizadas en board->string

;funcion esconder-barcos
;entrada: lista con caracteres
;salida: nueva lista cambiando los caracteres de barco de la computadora por mar
;recursion lineal
(define (esconder-barcos lista)
  (if (null? lista)
      null
      (if (eq? (car lista) #\E)
          (cons #\. (esconder-barcos (cdr lista)))
          (cons (car lista) (esconder-barcos (cdr lista)))
          )      
      )
  )
;funcion string-enemigo
;entrada: matriz a convertir en string
;salida; string que contiene la mitad de la computadora del tablero completo escondiendo los barcos que tiene.
;recursion de cola
(define string-enemigo
  (lambda (matriz)
    (let recur ((matriz matriz)(string "") (contador (/ (get-filas-matriz  matriz) 2)))
      (if (= contador 0)
          string
          (recur (cdr matriz) (string-append string (string-append (list->string (esconder-barcos (car matriz))) "\n" ) ) (- contador 1))
          )
      )  
    )
  )
;funcion board-string-complete
;entrada: matriz a convertir en string
;salida: string de la matriz completa sin cambiar ningun elemento
;recursion de cola
(define board-string-complete
  (lambda (board)
    (let recur ((board board)(string ""))
      (if (null? board)
          string
          (recur (cdr board) (string-append string (string-append (list->string (car board)) "\n" ) ))
          )
      )  
    )
  )
;funcion board-string-no-complete
;entrada; matriz a convertir en string
;salida: string con los barcos de la computadora escondidos
;recursion de cola
(define board-string-no-complete
  (lambda (matriz)
    (let recur ((matriz (list-tail matriz (/ (get-filas-matriz matriz) 2))) (string ""))
      (if (null? matriz)
          string
          (recur (cdr matriz) (string-append string (string-append (list->string (car matriz)) "\n" ) ))
          )
      )  
    )
  )


;-------------------------------------------------------------------------------------------------------------





;----------------------------------------------------------------------
;--------------------- funciones para el play--------------------------
;funion check-position
;entrada board x y, x e y son coordenadas
;salida: #t en caso de que las coordenadas pertenescan a posiciones validas tablero,  #f en caso contrario
(define check-position
  (lambda (board x y)
    (if (and (number? x) (number? y))
        (if (and (>= x 0)
                 (< x (/ (get-filas-matriz (get-matriz board)) 2))
                 (>= y 0)
                 (< y (get-columnas-matriz (get-matriz board)) )
                 )
            #t
            #f
            )
        #f
        )
    )
  )
;funion mar?
;entrada: board , x ,y. x e y son cooredanas
;salida: #t en caso de que en la posicion indicada x,y se encuentre mar (#\.)
(define mar?
  (lambda (board x y)
    (if (eq? (get-caracter-matriz (get-matriz board) x y) #\.)
        #t
        #f
        )
    )
  )
;funion mar?
;entrada: board , x ,y. x e y son cooredanas
;salida: #t en caso de que en la posicion indicada x,y se encuentre atacado (#\X)
(define atacado?
  (lambda (board x y)
    (if (eq? (get-caracter-matriz (get-matriz board) x y) #\X)
        #t
        #f
        )
    )
  )
;funion mar?
;entrada: board , x ,y. x e y son cooredanas
;salida: #t en caso de que en la posicion indicada x,y no se encuentre mar ni que este atacado (que exista un barco)
(define barco?
  (lambda (board x y)
    (if (and (not (mar? board x y))
             (not (atacado? board x y))
             )
        #t
        #f
        )
    )
  )
;funcion can-play?
;entrada: board
;salida; #t en caso de que la cantidad de barcos de la computadora y del usuario sean mayores a 0.
(define can-play?
  (lambda (board)
    (if (and
         (> (get-ships-cpu board) 0)
         (> (get-ships-player board) 0)
         )
        #t
        #f
        )
    )
  )


;funcion play-player
;entrada: board positions ship, positions son las posiciones que ataco el usuario (puede ser masivo el ataque)
;salida: board nuevo con todos los ataques realizados por el usuario
;recursion de cola
(define play-player
  (lambda (board positions ship )
    (if (null? positions)
        board
        (let ((x (get-x-position (car positions)))
              (y (get-y-position (car positions)))
              (filas (get-filas-matriz (get-matriz board)))
              (columnas (get-columnas-matriz (get-matriz board)))
              )
          ;(if (can-play? board)
              (if (check-position board x y)
                  (if (barco? board x y)
                      (play-player (sumar-score-player (addRecord(restar-ship-cpu (set-elem-matriz-on-board board x y #\X))2)) (cdr positions)  ship)
                      (play-player (addRecord (set-elem-matriz-on-board board x y #\X)0)  (cdr positions) ship )
                      );end if barco?
                  (play-player board  (cdr positions) ship)
                  );en if check-position
              ;(play-player board  (cdr positions) ship)
              ;);end if can-play
          );end let
        );en if null? position
    )
  )

;funcion play-cpu
;entrada: board seed. seed es ocupada para el ataque ramdom de la computadora
;salida: nuevo tablero con el ataque de la computadora realizado
(define play-cpu
  (lambda (board seed)
    (let (
          (xrand (+ (car (getListaRandom 1 (+ seed 6) (/ (get-filas-matriz (get-matriz board)) 2))) (/ (get-filas-matriz (get-matriz board)) 2)))
          (yrand (car (getListaRandom 1 (+ seed 5)  (get-columnas-matriz (get-matriz board)))))
          (filas (get-filas-matriz (get-matriz board)))
          (columnas (get-columnas-matriz (get-matriz board)))
          )
      ;(if (can-play? board)
          (if (check-position board (- xrand (/ filas 2)) yrand)
              (if (barco? board xrand yrand)
                  (sumar-score-cpu(restar-ship-player (set-elem-matriz-on-board board xrand yrand #\X)))
                  (set-elem-matriz-on-board board xrand yrand #\X)
                  
                  
                  );end if barco?
              board
              
              );en if check-position
          ;board
          
          ;);end if can-play
      );end let
    )
  )

;-------------------------------------------------------------------------------------------------------------
;                               funciones pedidas (no aparece createBoard aqui)

;funcion checkBoard
;entrada board
;salida: #t en caso de que cumpla los requisitos de que sea board, #f en caso contrario
(define  checkBoard
  (lambda (board)
    (board? board)))



;funcion putShip
;entrada: board, position,ship. position refiene a la posicion que se ubicara el barco, la fila debe ser menor a n/2 (filas de la matriz completa divido en dos)
;         ship, es el caracter a poner en el tablero
;salida: nuevo tablero con el barco ubicado y sumando 1 a la cantidad de barcos del usuario
(define putShip ;agregar el checkboard
  (lambda (board position ship)
    (if (and (checkBoard board) (position? position) (char? ship))
             (let ((x (get-x-position position))
                   (y (get-y-position position))
                   (filas (get-filas-matriz (get-matriz board)))
                   (columnas (get-columnas-matriz (get-matriz board)))
                   
                   )
               (if (and (>= x 0)
                        (< x (/ filas 2))
                        (>= y 0)
                        (< y columnas )
                        (>= (+ (get-ships-cpu board) 1) (get-ships-player board))
                        (eq? (get-caracter-matriz (get-matriz board) (+ (/ filas 2) x) y) #\.)
                        )
                   (set-ships-player (set-elem-matriz-on-board board (+ (/ filas 2) x) y ship)
                                     (+ (get-ships-player board) 1))
                   board
                   )
               )
             
        
        board)
    
    )
  )




;funcion play
;entrada: board,ship,positions,seed. ship es un caracter,positions una lista de posisiones a atacar ,seed semilla para utilizar random
;salida: tablero nuevo conn las jugadas realizadas especificadas la lista de posiciones a atacar
;aclaracion: priemero realiza el ataque el usario y luego la computadora
(define play
  (lambda (board ship positions seed)
    (if (and  (checkBoard board) (list? positions) (number? seed))
        (play-cpu (play-player board positions ship) seed)
        board)
    
    )
  )


;funcion board->string
;entrada: board, showComplete: si showComlete es 1 se mostrara todo el tablero, si es 0 se econderan los barcos de la computadora
;salida: strin correspondiete a lo mencionado en la entrada
(define board->string
  (lambda (board showComplete)
    (if (checkBoard board)
        (if (= showComplete 1)
            (board-string-complete (get-matriz board))
            (if (= showComplete 0)
                (string-append (string-enemigo (get-matriz board))
                               (board-string-no-complete (get-matriz board))
                               )
                "")
            )
        "")
    
    )
  )

(define getScore
  (lambda(board)
    (get-score-player board)  
    )
  )


  

; ##########################################################################################################################

;--------------------------------------------------------------------------------------------------
;                              TDA POSITION
;REPRESENTACION
; par (x.y) donde x e y son coordenadas del tablero, fila y columna respectivamente
; x e y pueden ser 0


;CONSTRUCTOR

;funcion createPosition
;entrada: x,y. coordenadas para crear una posicion
;salida: un par que representa al TDA position
(define createPosition
  (lambda (x y)
    (cons x y))
  )
;PERTENENCIA

;funion position?
;entrada: position (TDA)
;salida: #t en caso de cumplir los requisitos para que sea una posicion, #f en caso contrario
(define position?
  (lambda (position)
    (if (pair? position)
        (if (and (number? (car position)) (number? (cdr position)) (>= (car position ) 0 ) (>= (cdr position ) 0) )
            #t
            #f
            )
        #f)
    )
  )
;SELECTORES

;funcion get-x-position
;entrada: position (TDA)
;salida: coordenada x de la posicion
(define get-x-position
  (lambda (position)
    (car position)
    )
  )

;funcion get-y-position
;entrada: position (TDA)
;salida: coordenada y de la posicion
(define get-y-position
  (lambda (position)
    (cdr position)
    )
  )
;MODIFICADORES

;funcion set-x-position
;entrada: position (TDA), x nueva coordenada x
;salida: nuevo position con la coordenada x cambiada
(define set-x-position
  (lambda (position x)
    (createPosition x (get-y-position position))
    )
  )

;funcion set-y-position
;entrada: position (TDA), y nueva coordenada y
;salida: nuevo position con la coordenada y cambiada
(define set-y-position
  (lambda (position y)
    (createPosition (get-x-position position ) y)
   )
  )
;OPERACIONES SOBRE POSITION

;funcion comparePosition
;entrada: p1,p2. son dos TDA position
;salida; #t en caso de que x e y de cada position sean iguales, #f en caso contrario
(define (comparePosition p1 p2)
  (if (and (= (car p1) (car p2)) (= (cdr p1) (cdr p2)))
      #t
      #f))

;funcion verificarPosition
;entrada: lista,position. lista con position y un position
;salida: #t en caso de que ningun position de la lista sea igual a la position, #f en caso contrario
(define (verificarPosition lista position)
  (if (null? lista)
      #f
      (if (comparePosition (car lista) position)
          #t
          (verificarPosition (cdr lista) position)))
  )

;funcion createListPositions
;entrada: cantidad n m lista seed,
;         cantidad: cantidad de posiciones a generar
;         n: cantidad de filas de una matriz en donde se utilizaran las posiciones generadas
;         m: cantidad de columnas de una matrizen donde se utilizaran las posiciones generadas
;         lista: esta  entrada debe ser siempre una lista vacia para ir agregando a esta las posiciones random generadas
;         seed: semilla para utilizar el random
;salida: lista con posiciones random sin repetir
(define (createListPositions cantidad n m lista seed)
  (let* ((xrand (car (getListaRandom 1 (+ seed 2233) (/ n 2)))) (yrand (car (getListaRandom 1 (+ seed 4235)  m))))
    (if (= cantidad 0)
        lista
        (if (verificarPosition lista (createPosition xrand yrand))
            (createListPositions cantidad n m lista (+ seed 43112))
        (createListPositions (- cantidad 1) n m (addList lista  (createPosition xrand yrand )) (+ seed 83123))
        )
     )
   )
  )


;------------------------------------------------------------------------------------------------------------------------------------------
;##########################################################################################################################################
;
;                                    PARA EFECTOS DE PRUEBA SE DEFINE LO SIGUIENTE...




(define boardRC (createBoardRC 4 4 4 4) )
(define boardRL (createBoardRC 4 4 4 4) )

;utilizando putShip
(define bPutShip (putShip(putShip boardRC (createPosition 0 1) #\M)(createPosition 1 3) #\M))

;utilizando el check-board:
(define boardValido? (checkBoard boardRC))

;--------------------------------valida---------------valida--------- invalida---------------valida--------
(define positions (list (createPosition 1 1) (createPosition #\f 2) (createPosition 1 412)(createPosition 1 3) )) ;posisiones de un ataque masivo del jugador


;notar que solo se puede atacar si hay barcos en ambos lados del tablero (por parte del jugador y por parte de la computadora)
;ademas las posiciones ingresadas en positions pueden ser invalidas, estas no tendran efecto en el tablero.
(define bPlay (play boardRL #\M positions 3));board que no contiene barcos por parte del jugaor
(define bPlay2 (play bPutShip #\M positions 3));notar que dentro de las posiciones entregadas se encuentra una que es invalida

;utilizar display para poder ver el string, ejemplo: (display board-string-1)
(define board-string-1 (board->string bPlay2 1))
(define board-string-0 (board->string bPlay2 0))
(define board-string-2 (board->string bPlay2 2)); showCompleteInvalido
(define board-string-n (board->string (list 123) 0))

(define score (getScore bPlay2));funcion getScore









