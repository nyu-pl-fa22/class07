# Class 7

In 1936, the logician Alonzo Church introduced the untyped
*λ-calculus* (lambda calculus) to prove that there does not exist a
solution to David Hilbert's *Entscheidungsproblem* (German for
*decision problem*). This problem asks whether there exists an
algorithm that takes a formula in first-order logic as input and
decides whether the formula is true or false.  The lambda calculus
served as a *computational model*, i.e., a mathematical formalization of
what it actually means for something to be an algorithm.

Also in 1936, Alan Turing proved the same negative result to the
Entscheidungsproblem independently but shortly after Church using
Turing machines as an alternative formal computational model. (Turing
later became Church's PhD student.) Remarkably, both the lambda
calculus and Turing machines were thus invented to solve an open
problem in formal logic, several years before the first (universal)
programmable computers even existed.

Turing machines provide the theoretical foundation for imperative
programming languages while the lambda calculus provides the
theoretical foundation for functional programming languages. In fact,
the first functional programming language Lisp was created by John
McCarty in 1958, taking the lambda calculus as direct inspiration. To
understand how functional programming work, it is therefore helpful to
study this basic calculus.

In the course of this study, we will provide rigorous definitions for
concepts and ideas that are relevant for programming languages in
general and that also appear in the closely related subject of formal
logic. This includes many concepts that we have already looked at in
previous classes and that we have (perhaps) so far only understood at
an intuitive level. 

## Lambda Calculus

There are many variations of the lambda calculus with different syntax
and semantics. We will discuss the simplest variant, which is the
*pure untyped lambda calculus*.

### Syntax

A program in the (pure untyped) lambda calculus is described by
a *term*. The syntax is simple: a term *t* is either

* a function with *formal parameter* *x* and *body* *t₁* (aka lambda abstraction): *λ x. t₁*,
* an application of a function *t₁* to an *actual argument* *t₂*: *t₁ t₂*,
* or a variable: *x*.

Formally, (lambda) terms are described by the following context-free grammar:

`t ::= x | λ x. t | t t`

where `x` ranges over some (infinite) set of variables.

We additionally use parentheses in terms to indicate grouping but omit
them when intend is clear. In particular

* *λ x y. t* is a shorthand for *λ x. (λ y. t)* 
* *t₁ t₂ t₃* is a shorthand for *(t₁ t₂) t₃*

That is, just like in OCaml, we write multi-argument functions using
nested lambda abstractions (*currying*). Moreover, function application is
left-associative.

In fact, there is a close correspondence between terms in the lambda calculus
and expressions in programming languages that support first-class
functions. For example, the lambda term

*(λ x. (λ y. x y)) (λ z. z)*

is equivalent to the following OCaml expression 

```ocaml
(fun x -> (fun y -> x y)) (fun z -> z)
```

That is, a lambda abstraction corresponds to the definition of an anonymous
function.

Here is a Scala program that is closely related to the above lambda term:

```scala
def f(x) = {
  def g(y) = x(y)
  g
}
def h(z) = z
f(h)
```

That is, we have given an explicit name to
each anonymous functions: the term *(λ x. (λ y. x y))* is represented by the
function `f`, the term *(λ y. x y)* by the function `g`, and the term
*(λ z. z)* by the function `h`. The entire term

*(λ x. (λ y. x y)) (λ z. z)*

is then simply described by `f(h)`.

#### Binding, Scopes, and Free Variables

An occurrence of a variable *x* that immediately follows a *λ* as in
*λ x. t* is called a *binding occurrence*. All other occurrences of
variables are called *using occurrences*. 

In a term *λ x. t*, the *scope* of *x* is *t*.  We also say that *λ x*
*binds* *x* in *t*, respectively, that *x* is bound in *λ x. t*. 

A using occurrence of *x* in a term `t` that is not bound by a *λ x*
is called a *free* occurrence. A variable `x` is *free* in `t` if it
has some free occurrence in `t`.

A term that has no free variables is called *closed*. A *program* is a
closed term.

The intuition behind this definition of programs is that closed terms
are self-contained. If we want to evaluate a term *t* that contains
free variables, then we must look at it as part of a larger program
that binds the free variables of *t*.

Example: consider the term *t* given by

*(λ x. (λ y. x (z y))) y*

* There is one occurrence of *z*, which is a using occurrence and is free. Hence, *z* is free in *t*.
* From left to right, there is one binding occurrence of *x* and one using occurrence of *x*.
* The using occurrence of *x* is in the scope of the *λ x*. Hence, *x* is bound and not free in *t*.
* From left to right, there is one binding occurrence of y followed by two using occurrences.
* The left using occurrence of *y* is in the scope of the *λ y*, hence, it is bound. 
* The right using occurrence of *y* is free. Hence, *y* is also free in *t*.
* Since *t* has free variables, it is not a program.

In contrast, the following term is a program (and contains *t* as a
subterm):

*(λ y z. (λ x. (λ y. x (z y))) y) (λ x. x)*

There is a simple strategy to determine whether a particular using
occurrence of a variable in a lambda term is free or bound and if it
is bound, where it is bound. Take the lambda term *t* from the example
above. It helps a lot to look at *t* in its abstract syntax tree
representation (i.e. answering scoping-related questions for lambda
terms is similar to answering static scoping questions for programs
written in real programming languages). Here is the abstract syntax
tree for *t*:

```
    app
   /   \
  λ x   y
   |
  λ y
   |
  app
 /   \
x    app
    /   \
   z     y
```

A subtree rooted in a node labeled `app` represents a term that starts
with a function application: the subtree rooted at the left child of
the `app` node represents the function being applied and the subtree
rooted at the right child represents the argument term. A subtree
whose root node is labeled with `λx` is a lambda abstraction term that
binds `x`.  The subtree rooted in the (unique) child of a ``λx`` node
represents the body of the function. All the leaves of the tree are
labeled by variables. The leaves are exactly the using occurrences
of variables in the term.

To determine whether a particular occurrence is free, we start from
the corresponding leaf node and walk the tree upward towards the
root. If the leaf node we start from is labeled by `x` and we come
through a node labeled by `λx`, then the occurrence of `x` represented
by the leaf node is bound by that λ. Note that the first `λ` that
binds the variable on the path from the leaf to the root of the tree
is the one that the leaf refers to. So, in the example, if we start
from the leaf labeled by `y` that is below the nested `app` nodes,
then the first λ that we encounter on the way to the root binds that
`y`. So this occurrence of `y` is bound. On the other hand, if we
start from the right-most leaf that is labeled by `y`, then there is
no λ at all on the path to the root, so this occurrence is
free. Similarly, the left-most leaf note, which is labeled by `x` is a
bound occurrence. It is bound by the second λ on the path to the
root. The leaf node labeled by `z` is again a free occurrence, because
there is no `λz` on the path to the root.

#### Alpha-renaming

Consistent renaming of bound variables in a term is referred to as
*α-conversion* or *α-renaming*. We write *t =α= t'* to indicate that
*t* and *t'* are equal up to *α-renaming*.

Examples:

*(λ x. x)* =α= *(λ y. y)*

*(λ y. x y)* =α= *(λ z. x z)*

However, *(λ x. x x)* is not a valid α-renaming of *(λ y. x y)*
because *x* occurs free in the latter but not in the
former. Similarly, *(λ y. z y)* is not an α-renaming of *(λ y. x y)*.

More generally, α-renaming must obey the following rules:

* Only bound variable occurrences can be renamed, not free
  occurrences.
  
* Renaming must be consistent: if we want to α-rename *x* in a subterm *(λ
  x. t)* to *y*, all free occurrences of *x* in *t* must be replaced
  by *y*.
  
* Renaming must be *capture-avoiding*: if we rename *x* to *y* in *(λ
  x. t)* then for every subterm *t'* of *t*, if *t'* has a free
  occurrence of *x* that is bound by the outer λ in *(λ x. t)* before
  the renaming, then *y* must be free in the term obtained from *t'*
  after the renaming. Moreover, if *y* already occurs free in *t*
  before the renaming, then renaming *x* to *y* is disallowed.
  
  So, e.g. renaming *x* to *y* in *(λ x. (λ y. y x))* to obtain *(λ
  x. (λ y. y y))* is disallowed because *x* occurs free in the subterm
  *(λ y. y x)* but *y* does not occur free in *(λ y. y y)*. We say
  that *y* has been *captured* by the *binder* *λ y*. Capturing can be
  avoided by first α-renaming all conflicting bound variables in
  subterms. For instance, in the example, we can first α-rename the
  nested bound variable *y* to *z*, obtaining the term *(λ x. (λ z. z
  x))* and then α-rename *x* to *y*, obtaining *(λ y. (λ z. z y))*.
  

### Semantics: Evaluation via Reduction

A lambda term *t* is evaluated by step-wise rewriting using *reduction
rules* *t → t'*. The evaluation terminates when we have obtained a
term `t'` such that no more reduction rule can be applied. Such a term
is said to be in *normal form*.

The most important reduction rule in the lambda calculus is
*β-reduction*, which formalizes the idea of function application:

*(λ x. t) s → t[s/x]*

The notation *t[s/x]* stands for the term *t* (i.e. the body of the
function being applied) where all **free** occurrences of *x* are
syntactically substituted by the term *s*. Note that the free
occurrences of *x* in *t* are exactly those occurrences that refer to
the formal parameter of the function. Hence, *β-reduction* reflects the idea
that when a function application *(λ x. t) s* is executed, we continue
execution with the body *t* of the function, where we replace all
occurrences of the formal parameter `x` in *t* with the actual
argument `s`.

Restriction: the argument term *s* should not have any free variables
that are *captured* (i.e. bound by λ's) when we do the substitution in
*t*. We can always use α-renaming of bound variables in *t* to comply
with this restriction. Typically, one assumes that the function
`_[_/_]` picks such an α-renaming so that the computed
substitution is capture-avoiding. However, in the following, we will
always make the α-renaming step explicit before we apply the substitution.

The above restriction ensures that β-reduction yields static scoping
(and deep binding) of function parameters.

Examples:

*(λ x. x y) (λ z. z)*
*→ (λ z. z) y*

Here, we have *t = (x y)*, *s=(λ z. z)* and *t[s/x]=(λ z. z) y*.

The term `(λ z. z) y` can be further reduced using another β-reduction step:

*(λ z. z) y*
*→ y*

The term `y` is in normal form because no further β-reduction step applies.

The following example needs α-renaming before performing the β-reduction step:

*(λ x y. x y) (λ x. y)*

*= (λ x. (λ y. x y)) (λ x. y)*

*=α= (λ x. (λ z. x z)) (λ x. y)*  alpha-renaming of *y* to *z*

*→ λ z. (λ x. y) z*

To see that α-renaming is needed for deep binding semantics, we can
translate the lambda term *(λ x y. x y) (λ x. y)* in the previous
example to the syntax of a more conventional programming language
where we introduce a named function for every lambda term. We here use
OCaml syntax:

```ocaml
let f x =
  let g y = x y in
  g
in
let h x = y in
f h
```

The β-reduction step from the example corresponds to the execution of
the function call `f h` in the OCaml code. Note that the body of `h`
refers to the variable `y` (which we assume to be defined somewhere
else in the program context where this code appears). Without the
α-renaming step, we would essentially replace `x` in the body of the
function `g` by `h` where we rebind the variable `y` in the body of
`h` to the parameter `y` of `g`. This would give us shallow binding
semantics. If we want to have deep binding semantics, we first need to
rename the formal parameter `y` of `g` to something else, say `z`, so
that the free variable of `h` is not "captured" by that formal
parameter.

#### Evaluation strategies

We have the β-reduction rule, but if we have a complex term *t* and
β-reduction can be applied at many different places in *t*, where
should we apply it first? Also, some β-reduction steps may seem
unnecessary because they occur inside of functions that are never
applied.

Consider the following example:

*(λ x. (λ y. y x x)) ((λ x. x) (λ y. z))*

There are two possible places (*redexes*) where β-reduction can be
applied in this term:

1. The function is *(λ x. (λ y. y x x))* and the argument is *((λ
   x. x) (λ y. z))* (i.e. *t = (λ y. y x x)* and *s=((λ x. x) (λ y. z))*).

2. The function is *(λ x. x)* and the argument is *(λ y. z)* (i.e. *t = x* and *s=(λ y. z)*).

We highlight two specific evaluation strategies. In both strategies, a
term *t* that is not a function application is considered to be in normal
form:

* *applicative-order*: If the term is of the form *t₁ t₂*, first
  evaluate *t₁* recursively to normal form *s₁*. Then evaluate *t₂*
  recursively to normal form *s₂*. Then, if *s₁* is of the form *λ
  x. t* apply β-reduction and continue evaluation with *t[s₂/x]*. This
  corresponds to case 2. above:

  *(λ x.(λ y. y x x)) ((λ x. x) (λ y. z)) → (λ x (λ y. y x x)) (λ y. z)*

* *normal-order*: If the term is of the form *t₁ t₂*, first
  evaluate *t₁* recursively to normal form *s₁*. Then, if *s₁* is of the form *λ
  x. t* apply β-reduction and continue evaluation with *t[t₂/x]*. This corresponds to 
  case 1. above:

  *(λ x. (λ y. y x x)) ((λ x. x) (λ y. z)) →
    (λ y. y ((λ x. x) (λ y. z)) ((λ x. x) (λ y. z))*

Applicative-order corresponds to call-by-value parameter passing and
normal-order with call-by-name parameter passing.

##### Church-Rosser Theorem

Most functional programming languages are not purely functional
(i.e. they allow computations that have side effects such as variable
assignment, printing to the console, writing to files, ...). As we
have seen in previous classes, for such languages the evaluation order
affects program behavior.

The evaluation of a lambda term has no side effects. So does the
evaluation order still matter in this case?

Alonzo Church and Barkley Rosser showed in 1936 that if a term *t* can be
reduced (in 0 or more steps) to terms *t₁* and *t₂*, then there exists
a term *t'* such that both *t₁* and *t₂* can be reduced to *t'*.

The property is called *confluence*.  We also say that β-reduction is
*confluent*.

An important corollary of the theorem is that every lambda term has at
most one normal form. That is, no matter how we apply β-reduction, the
final result of the evaluation will always be the same.

But why does it say *at most one normal form*?

There exist terms that have no normal forms!

Example:

*(λ x. x x) (λ x. x x)*

If we apply β-reduction to this term, we obtain exactly the same
term. So we can keep on applying β-reduction again and again ad
infinitum:

*(λ x. x x) (λ x. x x)*
*→ (λ x. x x) (λ x. x x)*
*→ (λ x. x x) (λ x. x x)*
...

Returning to our earlier question, even though the evaluation of a
lambda term has no side effects, the evaluation order still matters
for termination. That is, even if a given term has a normal form, not
every evaluation strategy will actually compute it. Some strategies
might yield divergent (i.e. non-terminating) reduction sequences.

For instance, consider the following expression:

*(λ x. (λ x. x)) ((λ x. x x) (λ x. x x))*

The normal form of this expression is *λ x. x* which can be computed
using the normal-order evaluation strategy. However, the
applicative-order evaluation strategy will keep on reducing the
expression *(λ x. x x) (λ x. x x)* without ever making progress.

### Computability

Computability theory studies the question "What is computable?". The
notion of *computability* relies on formal models of computation.

Many formal models have been proposed:

* Functions computable by Turing machines (Turing)
* General recursive functions defined by means of an equation calculus (Goedel-Herbrand-Kleene)
* μ-recursive functions and partial recursive functions (Goedel-Kleene)
* Functions defined from canonical deduction systems (Post)
* Functions given by certain algorithms over finite alphabets (Markov)
* Universal Register Machine-computable functions (Shepherdson-Sturgis)
* ...
* Any function you can write in your favorite general-purpose programming language.

**Fundamental Result:** all of these (and many other) models of
computation are equivalent. That is, they give rise to the same class
of computable functions.

Any such model of computation is said to be *Turing-complete*.
(An as of today unproved hypothesis attributed to Alonzo Church and
Alan Turing is that there exists no meaningful computational model
that is more expressive than Turing-complete computational models.)

Alan Turing showed in 1937 that the untyped lambda calculus is Turing complete.
But how can this be?

* There are no built-in types other than "functions" (e.g., no Booleans, integers, etc.).
* There are no imperative features like assignments.
* There are no recursive definitions or looping constructs.

#### Church Encodings

The expressive power of the lambda calculus stems from the fact that
we can treat functions as data. It is all just a matter of finding the
right encoding of concepts like numbers, Booleans, and recursion. The
encodings presented below are inspired by Church's original work on
the Entscheidungsproblem. In his honor, they are therefore commonly
referred to as *Church encodings*.

When we talk about data types such as integers and Booleans in a
programming language, it is important to distinguish the abstract
mathematical concept behind these data types (e.g. integer numbers),
from the representation of these concepts in the language
(e.g. numerals that stand for integer numbers).

Example:

* 15
* fifteen
* XV
* `0F`

These are different *numerals* that all represent the same integer number.

To show that a mathematical structure such as integers and their
associated operations (addition, multiplication, etc.) can be
expressed in the lambda calculus, we simply have to agree on a
particular representation of each element of this structure using
lambda terms and then define the operations over these terms so that
evaluating them with β-reduction yields results that are consistent
with the chosen representation. That is, we seek to define an
*isomorphism*.  The idea behind Church encodings is to pick a
representation so that we can implement the desired operations on data
values by letting the data values themselves do all the work.

Let us start with Booleans. How can we represent *`true`* and *`false`* in
the lambda calculus? One reasonable definition is as follows:

* *`true`* is a function that takes two values and returns the first, i.e.

  *`true` = (λ x y. x)*

* *`false`* is a function that takes two values and returns the second, i.e.

  *`false` = (λ x y. y)*

The intuition behind this definition is that we can think of *`true`*
and *`false`* as implementing the two branches of a conditional
expression. That is, if we now define

*`ite` = (λ b x y. b x y)*

then *`ite`* is a ternary function that implements a conditional
expression, provided its first argument *b* is always either *`true`* or
*`false`* as defined above. In particular, we have


*`ite` `true` x y*

*= (λ b x y. b x y)  `true` x y*  ```; Definition of ite```

*→ `true` x y*

*= (λ x y. x) x y```; Definition of true```

*→ (λ y. x) y*

*→ x*

Convince yourself that *`ite` `false` x y* evaluates to `y`.

Using these definitions, we can now define the standard logical
operations on Booleans:

* *`and` = (λ a b. `ite` a b `false`)*

* *`or` = (λ a b. `ite` a `true` b)*

* *`not` = (λ b. `ite` b `false` `true`)*

Here are alternative definitions for these operations that use the
representation of Booleans as functions directly without referring to
`ite`:

* *`and` = (λ a b. a b `false`)*

* *`or` = (λ a b. a `true` b)*

* *`not` = (λ b x y. b y x)*

Using a similar idea, one can encode natural numbers using lambda
terms. We represent the number *n* by a function that maps a
successor function *s* and a zero element *z* to *n* applications of
*s* to *z*: *s (s ... (s z) ...)*:

* *`0` = (λ s z. z)*

* *`1` = (λ s z. s z)*

* *`2` = (λ s z. s (s z))*

* *`3` = (λ s z. s (s (s z)))*

* ...

That is, `0` is a function that takes an `s` and a `z` and applies `s`
zero times to `z`. `1` is a function takes an `s` and a `z` and
applies `s` once to `z`, etc.

Note that *`0`* and *`false`* are actually the same lambda terms
modulo α-renaming. This is just like in real computer systems where
the same bit sequence can stand for different mathematical objects
depending on context (e.g. the same bit sequence can represent the
integer `0` or the Boolean value `false`).

Here are the encodings of some operations on natural numbers:

* zero test: *`iszero` = (λ n. n (λ x. `false`) `true`)*
* addition by one (aka. successor):  *`succ` = (λ n s z. s (n s z))*
* addition: *`plus` = (λ m n. m `succ` n)*
* multiplication: *`mult` = (λ m n. m (`plus` n) `0`)*
* exponentiation: *`exp` = (λ m n. n m)*

For example, the definition of *`plus`* says: apply *`succ`* *m*-times
to *n*. If we take the term *`plus 3 2`*, then this
β-reduces to the term: *`succ` (`succ` (`succ` `2`))*. Since *`succ`*
encodes addition by one, this term thus computes *`5`*. Here is a
complete evaluation of the term *`plus` `1` `2`*, which shows that the
normal form obtained from this term is *`3`*, as one would expect:

*`plus` `1` `2`*

*= (λ m n. m `succ` n)  `1` `2`*  ```; Definition of plus```

*→ (λ n. `1` `succ` n) `2`*

*→ `1` `succ` `2`*

*= (λ s z. s z) `succ` `2`* ```; Definition of 1```

*→ (λ z. `succ` z) `2`*

*→ `succ` `2`*

*= (λ n s z. s (n s z)) `2`* ```; Definition of succ```

*→ (λ s z. s (`2` s z))* 

*= (λ s z. s ((λ s z. s (s z)) s z))* ```; Definition of 2```

*→ (λ s z. s ((λ z. s (s z)) z))*

*→ (λ s z. s (s (s z)))*

*= `3`* ```; Definition of 3```

The other operators work similarly. For instance, the definition of *`mult`*
says: apply *(`plus` n)* *m*-times to *`0`*. In particular, the term
*`mult` `3` `2`* reduces to the term

*`plus` `2` (`plus` `2` (`plus` `2` `0`))*

which further reduces to `6`.

Here is an example with `exp`:

*`exp` `2` `2`*

*= (λ m n. n m) `2` `2`*

*→ → `2` `2`*

*= (λ s x. s (s x)) `2`*

*→ λ x. `2` (`2` x)*

*= λ x. (λ s x1. s (s x1)) (`2` x)*

*→ λ x. λ x1. (`2` x) ((`2` x) x1)*

*= λ x. λ x1. (`2` x) (((λ s x2. s (s x2)) x) x1)*

*→ → λ x. λ x1. (`2` x) (x (x x1))*

*= λ x. λ x1. ((λ s x3. s (s x3)) x) (x (x x1))*

*→ λ x. λ x1. (λ x3. x (x x3)) (x (x x1))*

*→ λ x. λ x1. x (x (x (x x1)))*

*= λ s. x1. s (s (s (s x1)))*

*= λ s. x. s (s (s (s x)))*

*= `4`*

Encoding subtraction is a bit more involved. We proceed analogous to
the case for addition and first define a function *`pred`* that
encodes subtraction by one. The idea for *`pred`* is to take the
Church encoding of a number *n* and then *n* to iteratively compute
the Church encodings of the pairs of numbers *(i, i - 1)* for *i*
between 1 and *n*. Then projection on the second component of the
final computed pair *(n, n - 1)* gives us the Church encoding of the
number *n - 1*.

Let's start by defining functions for constructing and decomposing pairs:

* *`pair` = (λ x y b. b x y)*
* *`fst` = (λ p. p `true`)*
* *`snd` = (λ p. p `false`)*

Note that we have *`fst` (`pair` x y) → ... → x* and *`snd` (`pair` x y) → ... → y*.

Then subtraction by one can be defined as

*`pred` = λ n. `snd` (n (λ p. `pair` (`succ` (`fst` p)) (`fst` p)) (`pair` `0` `0`))*

Using `pred` we can now define subtraction

*`minus` = λ m n. n `pred` m*

#### Expressing Recursion

How can we express recursion in the lambda calculus?

As an example, we will implement the factorial function *n!* in the
lambda calculus. Let us start from the following OCaml definition of factorial

```ocaml
let rec fac n = 
  if n == 0 1 else n * fac (n - 1)
```

First, we do a literal translation of this definition into the
lambda calculus, making use of the operations and constants we have
defined so far:

*`fac` = (λ n. `ite` (`isZero` n) `1` (`mult` n (`fac` (`minus` n `1`))))*

This is still a recursive definition, which we can't express directly
in the lambda calculus. So let's do a simple trick and define a new
function *`Fac`* that takes the function *`fac`* that we need for the
recursive call as an additional input:

*`Fac` = (λ fac n. `ite` (`isZero` n) `1` (`mult` n (fac (`minus` n `1`))))*

Note that this definition is no longer recursive: the function `Fac`
simply delegates the work that needs to be done in the recursive call
to a function *fac* that is provided as an additional parameter. 

How does this help?

We can view *`Fac`* as a function that constructs the factorial
function iteratively. That is, *`Fac`* takes *fac* as input and if
*fac* is a function that approximates the factorial function
(i.e. produces the same result as the factorial function on some
inputs), then *`Fac`* returns a new function that is a *better*
approximation of the factorial function than *fac*. To see this,
consider the following sequence of definitions:

* *`fac0` = (λ x. `0`)*
* *`fac1` = `Fac` `fac0`*
* *`fac2` = `Fac` `fac1`*
* ...

Observe that this sequence of functions approximates the factorial
function with increasing precision: for all natural numbers *`i`* and
*n < `i`*, the term *(`faci` n)* reduces to the numeral representing
*n!*. If we let *`i`* go to infinity, this sequence converges to the
factorial function itself. That is, we can view the factorial function
*`fac`* as the function that satisfies the equation:

*`fac` = `Fac` `fac`*

In other words, we aim to construct the *fixpoint* of `Fac`. What we
thus need is a lambda term that serves as a *fixpoint operator*,
i.e. a function that calculates for us the fixpoint of another
function (here `Fac`). Concretely, we seek a lambda term `fix` such
that β-reduction yields a sequence like this:

*`fix` f → ... → f (`fix` f) → ... → f (f (`fix` f)) → ...*

If we find a term *`fix`* with this property, we can use it to
construct for us automatically on demand the approximation of *`faci`*
that is needed to calculate *n!* for a given input value *n*.

There are various ways to define such a fixpoint operator in the
lambda calculus. The most well-known and simplest one is the so-called
Y combinator due to the logician Haskell Curry:

*`fix` = (λ f. (λ x. f (x x)) (λ x. f (x x)))*

Observe what happens when we apply *`fix`* to *`Fac`*:

   *`fix` `Fac`* 

*→ (λ x. `Fac` (x x)) (λ x. `Fac` (x x))*

*→ `Fac` ((λ x. `Fac` (x x)) (λ x. `Fac` (x x)))*

*→ `Fac` (`Fac` (λ x. `Fac` (x x)) (λ x. `Fac` (x x)))*

*→ `Fac` (`Fac` (`Fac` (λ x. `Fac` (x x)) (λ x. `Fac` (x x))))*

*→ ...*

The sequence of reduction steps computes the approximation sequence
for the factorial function as required! We can thus define the
factorial function using the equation

*`fac` = `fix` `Fac`*

Note that the term *`fix` `Fac`* is indeed a valid term in the lambda
calculus. In particular, this term does not make use of explicit
recursion nor does it rely on any inbuilt data types or operations
for numbers and Booleans.

You can find an OCaml encoding of these
functions [here](church.ml). The encoding uses let bindings. However,
observe that a (non-recursive) let binding of the form

```ocaml
let x = e in b
```

can be encoded by the lambda term

*(λ x. b) e*

The implementation also uses a more convoluted variant of the
Y combinator that is defined as follows:

*fix =
  λ f.
    (λ x.
      f (λ n f y. (x x) n f y))
      (λ x. f (λ n f y. (x x) n f y))*

This variant delays the recursive unfolding of the approximation
sequence until the point when the next approximation of `f` is
actually needed, namely the point when a recursive call to `f` is
evaluated. This ensures that `fix` behaves correctly when using the
applicative-order evaluation strategy.

Note that you won't be able to compile and execute this code using the
OCaml compiler, though. The encoding builds on the untyped lambda
calculus. However, OCaml uses a typed version of the lambda
calculus. Certain function that we define, such as `fix`, cannot be
expressed in the typed version of the calculus that OCaml
uses. 

For the next homework assignment you will implement an interpreter for
an untyped subset of OCaml that builds on the untyped lambda
calculus. You can then use that interpreter to run the above
code. Alternatively, you can also translate the code in
[church.ml](church.ml) to a dynamically typed programming language of
your choice that supports first-class functions (e.g. JavaScript or
Python) and then use an interpreter for that language to run the translated code.