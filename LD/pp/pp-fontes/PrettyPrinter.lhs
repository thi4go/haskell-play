\documentclass{article}

%include polycode.fmt
%options ghci

\usepackage[brazil]{babel}
\title{Linguagens Declarativas \\ O desenho de uma 
biblioteca para Pretty-Printing}

\author{Rodrigo Bonif\'{a}cio}

\begin{document}

\maketitle 

\section{Introdu\c c\~{a}o}

Esse documento descreve o desenho de uma 
biblioteca para \emph{pretty-printing} usando 
a abordagem \emph{The Algebra of Programming}, 
proposta por um grupo de pesquisa da universidade de 
Oxford e liderado por Richard Bird. Nessa 
abordagem, os seguintes passos s\~{a}o previstos:

\begin{itemize}
\item identifica\c c\~{a}o das opera\c c\~{o}es 
primitivas da biblioteca. 

\item especifica\c c\~{a}o das propriedades das 
opera\c c\~{o}es primitivas. 

\item implementa\c c\~{a}o dos tipos alg\'{e}bricos 
relacionados. 

\item implementa\c c\~{a}o das opera\c c\~{o}es 
primitivas que resultam nas inst\^{a}ncias dos 
tipos alg\'{e}bricos. 
\end{itemize}

Esse documento segue um formato de tutorial, descrevendo 
cada um desses passos para o \emph{design} de uma 
biblioteca de \emph{pretty-printing}. O fonte desse 
documento \'{e} um c\'{o}digo Haskell v\'{a}lido, 
ent\~{a}o \'{e} poss\'{i}vel interpretar esse m\'{o}dulo 
diretamente pelo \texttt{ghci}. 

\section{Declara\c c\~{a}o do M\'{o}dulo}

O passo \emph{"zero"} na defini\c c\~{a}o de uma 
biblioteca consiste na declara\c c\~{a}o de um 
m\'{o}dulo, no nosso caso, o m\'{o}dulo se 
chama \texttt{PrettyPrinter} 

\begin{code}
module PrettyPrinter(
 Doc, 
 nl, nest, (<>), nil, text,
 layouts
) where 
\end{code}

\section{Defini\c c\~{a}o das fun\c c\~{o}es primitivas}

O primeiro passo da abordagem consiste na identifica\c c\~{a}o 
e especifica\c c\~{a}o das propriedades que permitem 
manipular uma estrutura a ser impressa. No nosso caso, 
vamos assumir que o tipo \texttt{Doc} representa 
estruturas que podem ser impressas. Como visto em aula, 
Temos interesse em compor documentos de diferentes formas  
(horizontalmente, verticalmente, com ou sem 
indenta\c c\~{a}o, por exemplo). Isso leva 
\`{a}s seguintes primitivas:

\begin{code}
-- primitiva para representar uma quebra de linha
nl :: Doc
-- primitiva usada para indentacao
nest :: Int -> Doc -> Doc

-- primitiva usada para composicao horizontal
(<>) :: Doc -> Doc -> Doc
\end{code}

Adicionalmente, temos interesse que a implementa\c c\~{a}o 
da primitiva de composi\c c\~{a}o horizontal \texttt{(<>)} 
seja associativa--- e operadores associativos geralmente 
est\~{a}o associados a algum valor identidade (por exemplo, o valor 
identidade da soma \'{e} zero, o valor identidade 
da multiplica\c c\~{a}o \'{e} 1). Com isso, vamos 
definir um valor \texttt{nil} para representar 
o valor identidade da primitiva de composi\c c\~{a}o 
horizontal. Finalmente, queremos ter facilidade para 
transformar uma \texttt{string} em um \texttt{Doc}. 
Isso leva \`{a}s seguintes fun\c c\~{o}es: 

\begin{code}
nil :: Doc
text :: String -> Doc
\end{code}

As primitivas descritas s\~{a}o construtores 
de documentos (basta observar o tipo de retorno). 
Por outro lado, precisamos de fun\c c\~{o}es 
que nos permitam decidir sobre os \emph{poss\'{i}veis 
layouts para apresentar um documento}. Podemos, 
com isso, especificar uma fun\c c\~{a}o 
\texttt{layout :: Doc -> [Layout]}, que recebe 
um documento e nos retorna a lista de layouts 
que podem ser produzidos a partir do documento. 


\begin{code}
type Layout = String
layouts :: Doc -> [Layout]
\end{code} 

\section{Defini\c c\~{a}o das propriedades}

\'{E} importante que as fun\c c\~{o}es primitivas 
especificadas anteriormente 
obede\c cam algumas propriedades alg\'{e}bricas. Como mencionado 
anteriormente, a composi\c c\~{a}o horizontal deve ser associativa 
e ter \texttt{nil} como indentidade. Dessa forma:

\begin{verbatim}
x <> (y <> z) = (x <> y) <> z 
x <> nil = nil <> x = x 
\end{verbatim} 

Em uma linguagem mais matem\'{a}tica, temos interesse que 
a fun\c c\~{a}o \texttt{text} seja um \emph{homomorphismo} 
entre a concatena\c c\~{a}o de \texttt{strings} e a 
composi\c c\~{a}o horizontal de documentos. 

\begin{verbatim}
text (s ++ t) = text s <> text t
text "" = nil
\end{verbatim}

Para a fun\c c\~{a}o \texttt{nest}, as seguintes propriedades 
est\~{a}o previstas:
	  
\begin{verbatim}
nest i (x <> y)   = nest i x <> nest i y
nest i nil        = nil
nest i (text s)   = text s
nest i line       = line <> text (replicate i ' ')
nest i (nest j x) = nest (i + j) x
nest 0 x          = x
\end{verbatim}

\noindent
{\bf Exerc\'{i}cio 01:} Implemente essas propriedades usando a 
biblioteca \texttt{QuickCheck}. Isso pode ser feito em um 
m\'{o}dulo separado e pode usar como refer\^{e}ncia o 
material dispon\'{i}vel em http://goo.gl/529rgm
  
\section{Defini\c c\~{a}o do tipo alg\'{e}brico \texttt{Doc}}

O terceiro passo da abordagem consiste na 
implementa\c c\~{a}o de um tipo algebrico 
baseado nas opera\c c\~{o}es primitivas 
que constroem documentos. Essa estrat\'{e}gia 
tem como objetivo simplificar a implementa\c c\~{a}o 
da biblioteca. 

\begin{code}
data Doc = Nil
     	 | Line
     	 | Text String
	 | Nest Int Doc
	 | Doc :<>: Doc
\end{code}

\section{Implementa\c c\~{a}o das opera\c c\~{o}es primitivas}

Com base na implementa\c c\~{a}o do tipo alg\'{e}brico 
\texttt{Doc}, torna-se trivial implementar as 
opera\c c\~{o}es primitivas, obedecendo \`{a}s respectivas 
propriedades. 

\begin{code}
nil  = Nil 
nl = Line
text s = Text s 
nest i doc = Nest i doc 
x <> y = x :<>: y
\end{code}

De forma similar, podemos facilmente implementar a 
fun\c c\~{a}o que transforma documentos em layouts--- 
bem como operadores e fun\c c\~{o}es auxiliares. 

\begin{code}
layouts Nil  = [""]
layouts Line = ["\n"] 
layouts (Text s) = [s]
layouts (x :<>: y) = layouts x <++> layouts y 
layouts (Nest i x) = [nestl i l | l <- layouts x]
\end{code}

O operador \texttt{<++>} computa uma lista de 
\texttt{strings}, onde cada \texttt{string} da lista 
resultante corresponde \`{a} concatena\c c\~{a}o 
das \texttt{strings} que formam \emph{o produto cartesiano} 
dos layouts de dois documentos combinados horizontalmente. 
A fun\c c\~{a}o \texttt{nestl} indenta cada um dos layouts 
de um documento. 

\begin{code}
xss <++> yss = [xs ++ ys | xs <- xss, ys <- yss]
 	
nestl :: Int -> Layout -> Layout
nestl i [] = ""
nestl i s@(c:cs)
 | c == '\n' = c:replicate i ' ' 
 | otherwise = s 
\end{code}


\section{Exemplo} 

Considerando o tipo \texttt{Exp} abaixo, 
vamos implementar uma fun\c c\~{a}o (\texttt{pp}) que 
transforma uma express\~{a}o em um \texttt{Doc}. 

\begin{code}
type Id = String 
data Exp = Const Int
         | Add Exp Exp
         | Ref Id
         | Let Id Exp Exp 

pp :: Exp -> Doc 
pp (Const v) = text (show v)
pp (Add l r) = text "(" <> pp l <> text " + " <> pp r <> text ")"  
pp (Ref idf) = text idf
pp (Let i e b) = text "let " <> text i <> text " = " <> pp e  
   <> ((nest 2 nl) <> (text "in" <> text " " <> pp b))
\end{code}

Podemos experimentar com alguns exemplos de express\~{o}es: 

\begin{code}
exp1 = Const 10
exp2 = Add exp1 (Const 5) 
let1 = Let "x" (Const 5) (Ref "x") 
\end{code}

E, para facilitar nossa vida, uma fun\c c\~{a}o (\texttt{printExp}) 
que imprime na sa\'{i}da padr\~{a}o o primeiro elemento 
da lista resultante da aplica\c c\~{a}o da fun\c c\~{a}o 
\texttt{layouts}--- aplicada ao resultado da 
aplica\c c\~{a}o da fun\c c\~{a}o \texttt{pp}. 

\begin{code}
printExp = (putStrLn . head . layouts . pp)
\end{code}

Neste ponto, podemos testar o comportamento 
da fun\c c\~{a}o. Usando o interpretador, 
carregue esse m\'{o}dulo \emph{lhs} e execute 
as instru\c c\~{o}es:   

\begin{verbatim}
$ printExp exp1
10
$ printExp exp2
(10 + 5)
$ printExp let1
let x = 5
  in x
\end{verbatim}

\noindent

{\bf Exerc\'{i}cio 02:} 
Note que a implementa\c c\~{a}o atual da fun\c c\~{a}o \texttt{pp} quando 
a express\~{a}o \'{e} do tipo \texttt{Let}, est\'{a} produzindo uma 
indenta\c c\~{a}o. Por outro lado, nem sempre esse \'{e} o nosso 
interesse--- conforme discutido em sala de aula. Poder\'{i}amos 
deixar para uma fun\c c\~{a}o \texttt{bestLayout} escolher, 
dentre os poss\'{i}veis layouts de um documento, qual seria o 
layout mais adequado para um determinado contexto. Isso fica como 
exerc\'{i}cio para a pr\'{o}xima aula. {\bf Sugest\~{a}o:} implemente um 
novo construtor de valores \texttt{Options Doc} para o tipo Doc, 
que inclui um novo layout para um documento substituindo toda 
quebra de linha por uma composi\c c\~{a}o horizontal. A fun\c c\~{a}o 
\texttt{bestLayout :: Doc -> Layout} pode selecionar o melhor layout conforme um 
crit\'{e}rio que voc\^{e} considerar interessante.

\end{document}