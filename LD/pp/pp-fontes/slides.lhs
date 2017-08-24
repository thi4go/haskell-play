\documentclass{beamer}

%include polycode.fmt

\usetheme{Warsaw}

\usepackage{listings}
\usepackage[brazil]{babel}

\author{Rodrigo Bonif\'{a}cio}

\title{The Design of a Pretty Printer Library}

\begin{document}

\begin{frame}
\maketitle
\end{frame}

\begin{frame}
\frametitle{Origem de uma DSL}

\emph{A functional programmer should approach a new application by seeking 
to identify the programming idioms common in that application area, and to 
define them as (probably high order) functions.}

\flushright{(John Hughes)}

\pause
\vskip+1.5em

\begin{block}{Terminologia relacionada}
\begin{itemize}
\item as fun\c c\~{o}es que implementam idiomas / conceitos 
primitivos de um dom\'{i}nio d\~{a}o origem \`{a}s 
\emph{combinators libraries} (ou \emph{embedded domain 
specific languages}).
\end{itemize}
\end{block}
\end{frame}

\begin{frame}
\frametitle{Ok, voltando ao exemplo\ldots}

Por que {\color{blue}pretty-printing}? \pause muitos exemplos de 
aplica\c c\~{o}es precisam transformar os tipos 
alg\'{e}bricos em uma representa\c c\~{a}o leg\'{i}vel 
para os usu\'{a}rios \pause

\begin{itemize}
\item \emph{program generators} 
\item \emph{refactoring tools} 
\item \emph{literate programming}
\item \emph{compilers, interpreters, \ldots}
\end{itemize}
\end{frame}


\begin{frame}[fragile]
\frametitle{o maior interesse \'{e} no layout}

Por exemplo, considerando o data type \texttt{Tree}
\begin{scriptsize}
\begin{code}
data Tree = Leaf | Node String Tree Tree

t1 = Node "foo" (Node "baz" Leaf Leaf) (Node "foobaz" Leaf Leaf)
\end{code} 
\end{scriptsize}
\pause 

, a inst\^{a}ncia \texttt{t1}
\'{e} muito mais f\'{a}cil de ser lida como:

\begin{verbatim}
Node "foo" (Node "baz" Leaf Leaf)
           (Node "foobaz" Leaf Leaf)
\end{verbatim}

\end{frame}

\begin{frame}
Nesse t\'{o}pico do curso, vamos discutir um estilo de 
programa\c c\~{a}o intitulado \emph{the algebra of programming} (ou \emph{algebraic 
design})\pause, popularizado por Richard Bird em 
um grupo de pesquisa da universidade de Oxford
\end{frame}

\begin{frame}
Mas vamos ilustrar essa estrat\'{e}gia de 
design inicialmente com um exemplo mais 
simples do que \emph{pretty-printing}. A implementa\c c\~{a}o de um tipo 
abstrato de dados para representar conjuntos. 

\pause 

\begin{block}{Operacoes b\'{a}sicas sobre conjuntos}
\begin{code}
empty :: Set a
(<+>) :: (Eq a) => a -> Set a -> Set a
union :: (Eq a) => Set a -> Set a -> Set a
intersection :: (Eq a) => Set a -> Set a -> Set a
element :: (Eq a) => a -> Set a -> Bool 
\end{code}
\end{block}
\end{frame}

\begin{frame}[fragile]
\frametitle{As opera\c c\~{o}es apresentam propriedades alg\'{e}bricas}

\begin{block}{Tais como:}
\begin{code}
a <+> a <+> s == a <+> s

a `union` empty == a
empty `union` a == a  

a `intersection` empty = empty 

a `union` a == a
a `intersection` a = a

(a `element` s) ==> a <+> s == s
\end{code}
\end{block}
\end{frame}

\begin{frame}[fragile]
\frametitle{\ldots e essas propriedades podem ser testadas}

\begin{block}{Usando a biblioteca \texttt{QuickCheck}, por exemplo}
\begin{code}
prop_idempotence a s = a <+> a <+> s == a <+> s
 where types = (a :: Int, s :: Set Int)

prop_union_identity s = empty `union` s == s
 where types = s :: Set Int  
\end{code}
\end{block}

\pause

\begin{block}{Com isso, podemos executar testes automaticamente}
\begin{verbatim}
> quickCheck prop_union_identity 
+++ OK, passed 100 tests.
>
\end{verbatim}
\end{block}
\end{frame}


\begin{frame}

Nesse estilo de design, com as opera\c c\~{o}es primitivas e as respectivas 
propriedades alg\'{e}bricas, derivamos a representa\c c\~{a}o 
para os tipos alg\'{e}bricos (nesse exemplo, o tipo \texttt{Set}).

\pause 

\begin{code}
data Set a = Empty
     	   | a :<+>: Set a 
\end{code}

\begin{block}{Observa\c c\~{a}o}
Note que temos um construtor para cada 
opera\c c\~{a}o que instancia valores do 
tipo \texttt{Set}. 
\end{block}
\end{frame}

\begin{frame}
O passo seguinte consiste na implementa\c c\~{a}o 
das opera\c c\~{o}es primitivas, de tal forma que 
obede\c cam as propriedades alg\'{e}bricas. 

\pause

\begin{block}{Exerc\'{i}cio em sala: qual a implementa\c c\~{a}o dos operadores?}
\begin{code}
empty = undefined
a <+> s = undefined 
k `element` s = undefined 
a `union` b = undefined
a `intersection` c = undefined
\end{code}
\end{block}

\end{frame}

\begin{frame}
\frametitle{Finalmente, tornamos \texttt{Set} um ADT}

\begin{block}{Falta apenas ocultar alguns detalhes}
\begin{code}
module Set (
 Set, 
 empty, 
 (<+>), 
 element, 
 union, 
 intersection) where 
\end{code}
\end{block}

\pause

\begin{itemize}
\item o tipo \texttt{Set} tamb\'{e}m precisa ser inst\^{a}ncia 
de algumas {\color{blue}type-classes}, como \texttt{Eq}, 
\texttt{Show} e \texttt{Arbitrary}. Essa \'{u}ltima para que o 
mecanismo de testes automatizados do \texttt{QuickCheck}.
\end{itemize}
\end{frame}

\begin{frame}
\begin{center}
\large{Voltando ao exemplo de \\ \emph{pretty-printing}}
\end{center}
\end{frame}

\begin{frame}
Com um \emph{pretty-printer} queremos 
exibir de forma adequada documentos, considerando 
informa\c c\~{o}es de contexto (largura da p\'{a}gina, 
por exemplo). Al\'{e}m disso, 
precisamos de meios para compor documentos 
de forma horizontal, vertical, com indenta\c c\~{a}o, 
\ldots. 

\begin{block}{De acordo com a abordagem, precisamos}
\begin{enumerate}
\item especificar os operadores 
\item implementar as propriedades dos operadores
\item implementar os tipos alg\'{e}bricos envolvidos
\item implementar os operadores
\item testar, testar, testar, \ldots
\end{enumerate}
\end{block}
\end{frame}

\begin{frame}
\frametitle{Podemos iniciar com seis operadores}

\begin{small}
\begin{code}
nil :: Doc
nl  :: Doc
text :: String -> Doc
nest :: Int -> Doc -> Doc
(<>) :: Doc -> Doc -> Doc
\end{code} 
\end{small}

\pause

\begin{itemize}
\item nesse estilo de design, al\'{e}m de definirmos 
operadores, tamb\'{e}m especificamos suas 
{\color{blue}propriedades alg\'{e}bricas}. Isso torna 
relevante a introdu\c c\~{a}o de elementos 
como \texttt{nil} \pause e pelo menos uma opera\c c\~{a}o 
de acesso (\texttt{layouts :: Doc -> [Layout]}).
\end{itemize}
\end{frame}

\begin{frame}[fragile]
\begin{verbatim}
(x <> y) <> z = z <> (y <> z)

nil <> x = x <> nil = x
 
text (s ++ t)   = text s <> text t
text ""         = nil

nest (i + j) x  = nest i (nest j x)
nest 0 x        = x

nest i (x <> y) = nest i x <> nest i y
nest i nil      = nil  

nest i (text s) = text s
\end{verbatim}

\end{frame}

\begin{frame}[fragile]
\begin{verbatim}
layouts nil           = [""]
layouts (text s)      = [s]
layouts (nest i line) = '\n' : replicate i ' ' :r
layouts (x <> y)      = layout x <++> layout y
\end{verbatim}
\end{frame}

\begin{frame}
\frametitle{Tipo alg\'{e}brico para documentos}

\begin{code}
data Doc = Nil
     	 | Line
     	 | Text String
	 | Nest Int Doc
	 | Doc :<>: Doc
\end{code}

\end{frame}

\begin{frame}
\frametitle{Sendo trivial implementar as opera\c c\~{o}es}

\begin{code}
nil  = Nil 
nl = Line
text s = Text s 
nest i doc = Nest i doc 
x <> y = x :<>: y
\end{code}

\pause

\begin{code}
layouts Nil  = [""]
layouts Line = ["\n"] 
layouts (Text s) = [s]
layouts (x :<>: y) = layouts x <++> layouts y 
layouts (Nest i x) = [nestl i l | l <- layouts x]
\end{code}
\end{frame}

\begin{frame}
\frametitle{\ldots e fun\c c\~{o}es auxiliares}

\begin{code}
xss <++> yss = [xs ++ ys | xs <- xss, ys <- yss]
\end{code}

\begin{code} 	
nestl :: Int -> Layout -> Layout
nestl i [] = ""
nestl i s@(c:cs)
 | c == '\n' = c:replicate i ' ' 
 | otherwise = s 
\end{code}

\end{frame}

\begin{frame}
\frametitle{Hora de um exemplo:}

\begin{block}{Considere os tipos de express\~{o}es}
\begin{code}
type Id = String 
data Exp = Const Int
         | Add Exp Exp
         | Ref Id
         | Let Id Exp Exp  
\end{code}
\end{block} \pause

\begin{itemize}
\item como implementar a fun\c c\~{a}o \texttt{showExp :: Exp -> Doc} 
de forma razo\'{a}vel? 
\end{itemize}
\end{frame}

\begin{frame}[fragile]
\frametitle{Uma primeira alternativa:}

\begin{scriptsize}
\begin{code}
pp :: Exp -> Doc 

pp (Const v) = text (show v)

pp (Ref idf) = text idf

pp (Add lhs rhs)= text "(" <> pp lhs <> text " + " <> pp rhs <> text ")"  
\end{code}
\end{scriptsize}

\pause
\begin{block}{Em uma sess\~{a}o com o interpretador}
\begin{verbatim}
$ let e1 = Const 5
$ let e2 = Add (Const 5) (Const 6)
$ let e3 = Add e1 e2
$ layouts $ showExp e3
["(5 + (5 + 6))"]
\end{verbatim}
\end{block}

\begin{itemize}
\item razo\'{a}vel o resultado?
\end{itemize}
\end{frame}

\begin{frame}[fragile]
\begin{block}{e as op\c c\~{o}es para \texttt{Let}?}
\begin{verbatim}
(a) let x = 5 in let y = 10 in x + y

(b) let x = 5 in let y = 10 
    in x + y 

(c) let x = 5 
     in let y = 10 in x + y

(d) let x = 5 
     in let y = 10
      in x + y
\end{verbatim}
\end{block}
\end{frame}

\begin{frame}
\begin{itemize}
\item o ideal \'{e} que dado um \texttt{Doc}, a fun\c c\~{a}o 
\texttt{layouts} possa gerar diferentes configura\c c\~{o}es\pause, 
mesmo que seja usando uma heur\'{i}stica simples. 

\item uma fun\c c\~{a}o pode refletir sobre os diferentes layouts e 
escolher o mais adequado, dado um determinado crit\'{e}rio (como a 
largura da p\'{a}gina ou a quantidade de itens indentados).  
\pause

\item {\bf sugest\~{a}o.} um \texttt{grupo} de op\c c\~{o}es que, dado um documento, gera 
um layout adicional que substitui todo \texttt{nl} por um \texttt{<>} e 
uma fun\c c\~{a}o \texttt{pretty :: Int -> Doc -> Layout}, que dado um 
documento, retorna o melhor layout. 
\end{itemize}
\end{frame}

\end{document}
