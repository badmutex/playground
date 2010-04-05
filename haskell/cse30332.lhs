\documentclass{article}
%include lhs2TeX.fmt


\usepackage{fullpage}

\begin{document}

We will be reimplementing some of the functions found in the standard libraries.
Thus we hide those ones to avoid name clashes.

> import Prelude hiding (sum, product, length)

But we still may want to access the standard functions.

> import qualified Prelude as Pre


\section{List}

A simplet data structure we are all familiar with.
This is an example of an Algebraic Data Structure:
Nil and Cons are the constructors that create a List datatype.
The list is polymorphic, as indicated by the type-variable 'a'.
Types and constructors must start with a capital letter, while type variables
are lower case and can contiain multiple characters.

> data List a = Nil | Cons a (List a) deriving Show

Haskell has a built-in list type. This allows us to transform it into our version.
Don't worry about the details for now

> fromList :: [a] -> List a
> fromList = Pre.foldr Cons Nil


Now for some functions on List's. Say we wanted to know the length.

> length Nil           = 0
> length (Cons _ xs)   = 1 + length xs

Here is a List for testing

> testlist = fromList [1..10]

Asking ghci to show s the test list we get

\begin{verbatim}
Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 (Cons 6 (Cons 7 (Cons 8 (Cons 9 (Cons 10 Nil)))))))))
\end{verbatim}


Now what if we have a List of numbers? Here's how we compute the sum

> sum Nil           = 0
> sum (Cons x xs)   = x + sum xs

and the product

> product Nil           = 1
> product (Cons x xs)   = x * product xs

You can load ghci and apply these functions on out testlist:

\begin{verbatim}
*Main> sum testlist
55
*Main> product testlist
3628800
\end{verbatim}

Let's take another look at these functions... . They seem really similar.
In fact, the only differences are the base case and the (+) or (*).
They both follow the pattern of

\begin{verbatim}
??? base_case = Base
??? (Cons x xs) = x `function` ??? x xs
\end{verbatim}

Let's try and capture this by refactoring.

First we handle the base case: the empty list.
We don't care about the function we're applying, since it will not be applied. We ignore it with '\_'

> fold _ x Nil = x

Now we can recursively fold the function over the tail of the list with a new accumulator.

> fold f x (Cons y ys) = fold f (f x y) ys

Let's rewrite the sum and product functions

> sum'     xs   = fold (+) 0 xs
> product' xs   = fold (*) 1 xs

and see if we get the same answers

\begin{verbatim}
*Main> sum' testlist
55
*Main> product' testlist
3628800
\end{verbatim}


The length function is a bit trickier, since we do not care about the
contents of the List.  Here we pass an anonymous function to do the
work of ignoring the Cons cell.  Anonymous functions are also known as
lambda functions, or just lambdas. In python, the corresponding lambda
would be

\begin{verbatim}
lambda len, ignore_me: len+1
\end{verbatim}

> length' xs = fold (\len _ -> len + 1) 0 xs

\begin{verbatim}
*Main> length' testlist
10
\end{verbatim}



\subsection{Point free notation}

Most people tend to write what is called 'pointed' code. There is another style called
'point-free' that Haskell allows. Here a point is not the '.' character. Rather, it referes
to explicitly naming a variable. Thus in pointed code a variable is given a name, such as 'x', 'xs',
'ys', etc. The code so far has been pointed.
Point-free code on the other hand doesn't make explicit mention of variables, or at least reduces it
as much as possible. 

Let's rewrite our length, sum, and product functions in point-free style.

> length''    = fold (\x _ -> x+1) 0
> sum''       = fold (+) 0
> product''   = fold (*) 1

This has both advantages and disadvantages. It can be clearer in some cases, but things can get
messy pretty quickly. In large part though, it's a matter of personal preference.

\begin{verbatim}
*Main> length'' testlist
10
*Main> sum'' testlist
55
*Main> product'' testlist
3628800
\end{verbatim}


\end{document}