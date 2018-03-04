;; JOSE GENILSON DA SILVA FILHO
;; MAURO CHIOZZI

;; 1 - ler os pixels e salvar em uma matriz -> [[pixel pixel pixel coordenada], [pixel pixel pixel coordenada], ...]
;; 2 - aplicar o tratamento (negativo, bordas, ... )
;; 3 - modo de preenchimento (1:1, preenchendo a folha, etc)
;; 4 - escrever de alguma maneira (circulos, elipse, quadrados, retangulos)
;; 5 - pronto 

;; Criar comando em LaTex para facilitar a criação de retangulos 	[OK]
;; Preencher:
;;	Elipses 														[OK]
;;	Circulos 														[OK]
;;	Quadrados 														[OK]
;;	Retangulos 														[OK]
;; Efeitos:
;;	Escalas de cinza												[OK]
;;	Bordas 															[ X]
;;  Negativo 														[OK]
;; Modo preenchimento:
;;	1:1 															[OK]
;;	Preencendo a folha 												[OK]
;;	Adequando paisagem/retrato										[ X]
;;

;; Criar matriz para armazenar as cores e as coordenadas			[OK]
;; Manipular a matriz												[ X]
;;

;; Obs.:
;; 1 - No texmaker, a imagem gerada via "preenchendo_retangulos" não possui linhas brancas entre os retangulos.
;; 2 - Escalas de cinza "luminosity" está com alguns pixels em brancos para o olho de tandera
;; 3 - Analizar textos em imagens após transformações: Legíveis

;; Bibliográfias:
;; Tons de cinza: http://www.johndcook.com/blog/2009/08/24/algorithms-convert-color-grayscale/
;;

(defun inicio (out) 
(format out 
	"\\documentclass[a4paper]{report}
\\usepackage{geometry}
\\usepackage{tikz}
\\geometry{left = 1cm, right = 1cm, top = 1cm, bottom = 1cm}
\\pagestyle{empty}~%

% Ilustracao do comando \" retangulo{x}{y}{aresta_a}{aresta_b}{cor}	\"
%
%	 (m, n)	_________________________
%			|			|			|			^
%			|			|			|			|
%			|			|			|			|
%			|			|(x, y) 	|			|
%			|-----------|-----------|			| a
%			|			|			|			|
%			|			|			|			|
%			|			|			|			|
%			|			|			|			v
%			¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨(w, z)
%
%			<----------------------->
%						b
%
%			m = x - b/2
%			w = x + b/2
%
%			n = y + a/2
%			z = y - a/2
%

\\newcommand{\\retangulo}[5]{
	%\\retangulo{x}{y}{aresta_a}{aresta_b}{cor}	
	\\fill[color = #5] (#1 - #4/2, #2 + #3/2) rectangle(#1 + #4/2,#2 - #3/2);
}

\\begin{document}~%~%

	\\begin{tikzpicture}~%~%"
)

)

(defun fim (out)
(format out "	\\end{tikzpicture}~%
\\end{document}"
)
)

(defun armazenar ()
	(setf pixels (make-array (list (* altura largura) 5)))

	;vai existir (* altura largura) conjuntos contendo 5 espaços
	;
	;         1 				(red green blue coorx coory)
	;         2   	     		(red green blue coorx coory)
	;         .    							 .
	;         .    							 .
	;         .    							 .
	; (*altura largura)			(red green blue coorx coory)
	;

	(loop for i from 1 to altura do
		(loop for j from 1 to largura do

			;salvando as 3 cores
			
			(setq (aref pixels (+ (* i (- largura 1)) j) 1 ) (read image))
			(setq (aref pixels (+ (* i (- largura 1)) j) 2 ) (read image))
			(setq (aref pixels (+ (* i (- largura 1)) j) 3 ) (read image))
			(setq (aref pixels (+ (* i (- largura 1)) j) 4 ) i )
			(setq (aref pixels (+ (* i (- largura 1)) j) 5 ) j )

		)
	)
)

(defun teste ()
	(setf tes (make-array (list 2 3)))

	(loop for i from 1 to 3 do
		(loop for j from 1 to 2 do
			(setq (aref tes (let i j)) 0)
		)	
	)

)

(defun normal ()
	(loop for i from 1 to altura do
		(loop for j from 1 to largura do
			(setq red (read imagem))
			(setq green (read imagem))
			(setq blue (read imagem))

			(format out 
				"		\\definecolor{cor}{RGB}{~S, ~S, ~S};~%" 
				red green blue
			)
			(format out
				"		\\fill[color = cor] (~S / 70, 0 - ~S / 70) circle(~S);~%~%"
				j i raio
			)
		)
	)	
)1

(defun preenchendo_circulos ()
	(loop for i from 1 to altura do
		(loop for j from 1 to largura do
			(setq red (read imagem))
			(setq green (read imagem))
			(setq blue (read imagem))

			;j * largura_pagina / largura
			(setq coorx (* j (/ largura_pagina largura)))
			;0 - i * altura_pagina * altura
			(setq coory (- 0 (* i (/ altura_pagina altura))))

			(format out
				"		\\definecolor{cor}{RGB}{~S, ~S, ~S};~%"
				red green blue
			)
			(format out
				"		\\fill[color = cor] (~S, ~S) circle(~S);~%~%" 
				coorx coory raio
			)
		)	
	)
)

(defun preenchendo_elipses ()
	(setq razao_largura (/ largura_pagina largura))
	(setq razao_altura (/ altura_pagina altura))

	(setq raio_largura (/ razao_largura 2))
	(setq raio_altura (/ razao_altura 2))			

	(loop for i from 1 to altura do
		(loop for j from 1 to largura do
			(setq red (read imagem))
			(setq green (read imagem))
			(setq blue (read imagem))
			
			;j * largura_pagina / largura
			(setq coorx (* j razao_largura))
			;0 - i * altura_pagina * altura
			(setq coory (- 0 (* i razao_altura)))

			(format out
				"		\\definecolor{cor}{RGB}{~S, ~S, ~S};~%"
				red green blue
			)
			(format out
				"		\\fill[color = cor] (~S, ~S) ellipse(~S and ~S);~%~%" 
				coorx coory raio_largura raio_altura
			)
		)	
	)
)

(defun preenchendo_quadrados ()
	(setq aresta (* raio 2))

	(loop for i from 1 to altura do
		(loop for j from 1 to largura do
			(setq red (read imagem))
			(setq green (read imagem))
			(setq blue (read imagem))

			;j * largura_pagina / largura
			(setq coorx (* j (/ largura_pagina largura)))
			;0 - i * altura_pagina * altura
			(setq coory (- 0 (* i (/ altura_pagina altura))))

			(format out
				"		\\definecolor{cor}{RGB}{~S, ~S, ~S};~%"
				red green blue
			)
			(format out
				"		\\retangulo{~S}{~S}{~S}{~S}{cor}~%"
				;"		\\fill[color = cor] (~S, ~S) \crule(~S);~%~%" 
				coorx coory aresta aresta
			)
		)	
	)
)

(defun preenchendo_retangulos()
	(setq razao_largura (/ largura_pagina largura))
	(setq razao_altura (/ altura_pagina altura))

	(setq raio_largura (/ razao_largura 1))
	(setq raio_altura (/ razao_altura 1))

	(loop for i from 1 to altura do
		(loop for j from 1 to largura do
			(setq red (read imagem))
			(setq green (read imagem))
			(setq blue (read imagem))

			;j * largura_pagina / largura
			(setq coorx (* j (/ largura_pagina largura)))
			;0 - i * altura_pagina * altura
			(setq coory (- 0 (* i (/ altura_pagina altura))))

			(format out
				"		\\definecolor{cor}{RGB}{~S, ~S, ~S};~%"
				red green blue
			)
			(format out
				"		\\retangulo{~S}{~S}{~S}{~S}{cor}~%"
				;"		\\fill[color = cor] (~S, ~S) \crule(~S);~%~%" 
				coorx coory raio_altura raio_largura
			)
		)	
	)
)

(defun preenchendo_retangulos_cinzas_average ()
	(setq razao_largura (/ largura_pagina largura))
	(setq razao_altura (/ altura_pagina altura))

	(setq raio_largura (/ razao_largura 1))
	(setq raio_altura (/ razao_altura 1))

	(loop for i from 1 to altura do
		(loop for j from 1 to largura do
			(setq red (read imagem))
			(setq green (read imagem))
			(setq blue (read imagem))

			;j * largura_pagina / largura
			(setq coorx (* j (/ largura_pagina largura)))
			;0 - i * altura_pagina * altura
			(setq coory (- 0 (* i (/ altura_pagina altura))))

			(format out
				"		\\definecolor{cor}{gray}{~S};~%"
				(float (/ (+ red green blue) 765))
			)
			(format out
				"		\\retangulo{~S}{~S}{~S}{~S}{cor}~%"
				;"		\\fill[color = cor] (~S, ~S) \crule(~S);~%~%" 
				coorx coory raio_altura raio_largura
			)
		)	
	)
)

(defun preenchendo_retangulos_cinzas_luminosity ()
	(setq razao_largura (/ largura_pagina largura))
	(setq razao_altura (/ altura_pagina altura))

	(setq raio_largura (/ razao_largura 1))
	(setq raio_altura (/ razao_altura 1))

	(loop for i from 1 to altura do
		(loop for j from 1 to largura do
			(setq red (* (read imagem) 0.21))
			(setq green (* (read imagem) 0.72))
			(setq blue (* (read imagem) 0.07))

			;j * largura_pagina / largura
			(setq coorx (* j (/ largura_pagina largura)))
			;0 - i * altura_pagina * altura
			(setq coory (- 0 (* i (/ altura_pagina altura))))

			(format out
				"		\\definecolor{cor}{gray}{~S};~%"
				(float (/ (+ red green blue) 255))
			)
			(format out
				"		\\retangulo{~S}{~S}{~S}{~S}{cor}~%"
				;"		\\fill[color = cor] (~S, ~S) \crule(~S);~%~%" 
				coorx coory raio_altura raio_largura
			)
		)	
	)
)

(defun preenchendo_retangulos_negativo()
	(setq razao_largura (/ largura_pagina largura))
	(setq razao_altura (/ altura_pagina altura))

	(setq raio_largura (/ razao_largura 1))
	(setq raio_altura (/ razao_altura 1))

	(loop for i from 1 to altura do
		(loop for j from 1 to largura do
			(setq red (- 255 (read imagem)))
			(setq green (- 255 (read imagem)))
			(setq blue (- 255 (read imagem)))

			;j * largura_pagina / largura
			(setq coorx (* j (/ largura_pagina largura)))
			;0 - i * altura_pagina * altura
			(setq coory (- 0 (* i (/ altura_pagina altura))))

			(format out
				"		\\definecolor{cor}{RGB}{~S, ~S, ~S};~%"
				red green blue
			)
			(format out
				"		\\retangulo{~S}{~S}{~S}{~S}{cor}~%"
				;"		\\fill[color = cor] (~S, ~S) \crule(~S);~%~%" 
				coorx coory raio_altura raio_largura
			)
		)	
	)
)

(setq largura_pagina 18)
(setq altura_pagina 27)
(setq raio 0.1)

; abrir arquivo 
(setq imagem (open "img.ppm"))

; lê a primeira  linha com os caracteres p3 (magic numbers)
(setq lixo (read-line imagem))

; le a segunda linha com o comentário
(setq coment (read-line imagem))

; le altura e largura
(setq largura (read imagem))
(setq altura (read imagem))
; (write largura)
; (write altura)

;le o 255
(setq lixo (read-line imagem))

(setq out (open "img.tex" :direction :output))

(inicio out)
; (normal)
; (preenchendo_circulos)
; (preenchendo_elipses)
; (preenchendo_quadrados)
; (preenchendo_retangulos)
(preenchendo_retangulos_cinzas_average)
; (preenchendo_retangulos_cinzas_luminosity)
; (preenchendo_retangulos_negativo)
(fim out)

(close out)
(close imagem)

(run-shell-command "lualatex img.tex")
(run-shell-command "evince img.pdf")