#--------------------------------------- 
# LexAsgard.py
#
# Analisis Lexico 
#---------------------------------------

import ply.lex as lex

dato = ""
columna = 0
error = 0
lista = []
# Se listan las palabras reservadas
reserved = {
	'begin'	: 'TkBegin',
	'end'	: 'TkEnd',
	'using'	: 'TkUsing',
	'integer': 'TkInteger',
	'boolean': 'TkBoolean',
	'canvas'  : 'TkCanvas',
	'if'    : 'TkIf',
	'then'	: 'TkThen',
	'else'	: 'TkElse',
	'done'	: 'TkDone',
	'while'	: 'TkWhile',
	'repeat': 'TkRepeat',
	'true'	: 'TkTrue',
	'false' : 'TkFalse',
	'with'	: 'TkWith',
	'from'	: 'TkFrom',
	'to'	: 'TkTo',
	'read' 	: 'TkRead',
	'print'	: 'TkPrint',
			
 }

# Lista de tokens.
# Tipos de Variables, Operadores y palabras del lenguaje 

tokens = (
	'TkIdent',
	'TkOfType',
	'TkNum',		
	'TkLienzo',
	'TkComa',
	'TkPuntoYComa',
	'TkParAbre',
	'TkParCierra',
	'TkSuma',
	'TkResta',
	'TkMult',
	'TkDiv',
	'TkMod',
	'TkConjuncion',
	'TkDisyuncion',
	'TkNegacion',
	'TkMenor',
	'TkMenorIgual',
	'TkMayor',
	'TkMayorIgual',
	'TkIgual',
	'TkDesIgual',
	'TkHorConcat',
	'TkVerConcat',
	'TkRot',
	'TkTras',
	'TkAsignacion'
)  +tuple(reserved.values())


#Reglas para tokens simples

t_TkComa = r'\,'
t_TkPuntoYComa = r'\;'
t_TkParAbre = r'\('
t_TkParCierra = r'\)'
t_TkSuma = r'\+'
t_TkResta = r'\-'
t_TkMult = r'\*'
t_TkDiv = r'\/'
t_TkMod = r'\%'
t_TkConjuncion = r'\/\\'
t_TkDisyuncion = r'\\\/'
t_TkNegacion = r'\^'
t_TkMenor = r'\<'
t_TkMenorIgual = r'\<='
t_TkMayor = r'>'
t_TkMayorIgual = r'>='
t_TkIgual = r'\='
t_TkDesIgual = r'\/='
t_TkHorConcat = r'\:'
t_TkVerConcat = r'\|'
t_TkRot = r'\$'
t_TkTras = r'\''
t_TkAsignacion = r':=' 
t_ignore  =  " \t"

#Definicion para Of Type
def t_TkOfType(t):
    r'of\s+type'
    t.value = 'of type'	
    return t

#Definicion para nombre de variables
def t_TkIdent(t):
    r'[a-zA-Z][a-zA-Z0-9]*'
    t.type = reserved.get(t.value,'TkIdent')
    return t
	
# Definicion del tipo Num 	
def t_TkNum(t):
	r'\d+'
	t.value = int(t.value)
	return t

#Definicion para tipo Lienzo
def t_TkLienzo(t):
    r'((</>)|(<->)|(<_>)|(<empty>)|(<\\>)|(<\|>)|(< >))'
    t.type = reserved.get(t.value,'TkLienzo')
    return t

# Definicion de linea nueva	
def t_newline(t):
    r'\n+'
    t.lexer.lineno += t.value.count("\n")
    	
  
# Definicion de Columna
def find_column(input,token):
    last_cr = input.rfind('\n',0,token.lexpos)
    if last_cr < 0:
		last_cr = 0
    column = (token.lexpos - last_cr) + 1
    return column

##Manejo de errores##
def t_error(t):
	global dato
	columna = find_column(dato,t)

	if (t.lexer.lineno==1):
		print "Error: Caracter inesperado '%s'" % t.value[0]," en la fila ", t.lexer.lineno,", columna ", columna
		
	else:
		print "Error: Caracter inesperado '%s'" % t.value[0]," en la fila ", t.lexer.lineno,", columna ", columna-1 
		
	global error
	error=1
	t.lexer.skip(1)


# Manejo de Comentarios
def t_comment(t):
   r'(((\{\-)+)[^-]*(\-(?!\{))*)[^{-]*(\-(?!\{))*(\-\}){1}'
   pass	

#Construir el Lexer

lexer = lex.lex()

#Lectura de los datos pasados por la entrada estandar
data = ""
while 1:
	try: 
		data =data+raw_input()+"\n"
	except EOFError: 
		break 


lexer.input(data)

# Tokenize
while True:
	tok = lexer.token()
	if not tok: break      # No more input
	if (error==0):	    
		lista.append((tok.type,tok.value))
	else:
 		pass
if (error==0):
	print lista
		



