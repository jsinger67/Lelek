# Semantic actions are functions

Lelek uses the following type for semantic actions

```fsharp
namespace LelekParser
module ParserTypes =
    // ...
    type UserAction<'a> = ParserFeedback -> 'a list -> ParseTree list -> 'a list
```

Let's explain all the function parameters.

* ParserFeedback

  The very first parameter allows the user action to output errors, warnings and messages via the ParserFeedback interface. Errors and warnings are thereby counted automatically.

* 'a list

  This is the current user stack which is empty at the beginning of the parse process.
  This stack of user defined elements usually constitute the AST, i.e. nodes of an abstract syntax tree of generic type `'a`.
  A user action can manipulate this AST stack according to the current stack's content and the given parse tree list.  
  The list's head is the top of the stack.

* ParseTree list
  
  This parameter reflects the current parser state at the moment when your action is called. Since an action is always called at the end of an alternative you get the parse trees of each element (either terminal or non-terminal) of the current alternative. The `ParseTree list` parameter reflects the `Children` member of the `ParseTree` record type. At this state the parser processes the non-terminal your semantic action is attached to and passes the children to your action. The parser actually creates a `ParseTree(<your non-terminal>, Children)` sub-tree.

  Lelek defines the following type of the ParseTree

```fsharp
namespace LelekParser
module ParseTree =
    // ...
    type PTItem =
        | Tok of Token
        | Var of string

    type ParseTree =
        | ParseTree of Item: PTItem * Children: ParseTree list
```

* 'a list - returned value

  The user action should return the new AST stack.

## Default actions

When no user action is given for a certain alternative the parser generator automatically inserts a default action that simply ignores the parse tree list (i.e the third parameter) and returns the stack (the second parameter) untouched.

## Defining the type of AST

Besides defining a working grammar definition the implementor of a language needs to define the type of the abstract syntax tree that serves as a transport vehicle for all the data that are collected during the phase of syntactic analysis.
With the help of the powerful F# type system you have all you need to define this type so that it fits your needs.

HINT: Typically the type for the AST is defined as a discriminated union.

An introductory example can be found in the AST.fs for the [List example](../examples/List/AST.fs).

The action `@AST.number` in this example is placed behind non-terminal 'number', see [Lst.llk](../grammars/Lst.llk).

It is actually a call to a function named `AST.number`. The implementation of the function `number` resides in the module **AST**. You can use here any valid name of a function including namespaces and module names.

```ebnf
number              = "\d+"             @AST.number
;
```

Here the single alternative of `number` has only one element in its production (the token for a number, _"\d+"_).
To demonstrate this look at the following parse tree that is generated when the [ListTest.txt](../examples/List/ListTest.txt) is parsed:

![ListTest.svg](../docs/images/ListTest.svg)

In this simple parse tree you can see that every `number` node has only one child, actually the token with the number's text. This is always the case when the associated action is called.  Our `ParseTree list` parameter therefore always consists of one element. This element normally can always be converted into an `int`. After the parsing was successful we expect a list of integers as result. The following AST would be sufficient.

```fsharp
    type AST =
        | Num of int
```

But since parsing can fail we always have to consider the error case.
So our AST could look like this:

```fsharp
    type AST =
        | Num of int
        | Err of string
```

Along with the design of a AST type you should work out the transformations that must be applied to the elements of your AST.
Let's say you want to support a multiplication on integers you could assume that there must be some kind of multiplication node that has two children of type `Num`.  
If so you could add a new DU case like this:

```fsharp
    type AST =
        | Num of int
        | Mul of (Num * Num)
        | Err of string
```

This way you can incrementally extend your AST type when progressing with your language implementation.

## Implementing semantic actions

First it is important to understand the role of the parameters that are passed by the parser when calling a semantic action.

Then you need some understanding about the transformations you need to apply on your AST items.
In the calculator example we reduced the numbers on the user stack directly when encountering any operation. We did this by popping the numbers from the user stack and pushing the result on the stack again ready for further processing.
As another example when encountering a variable name we tried to find the name in our variable list and replaced the _name_ on top of the stack by the current _value_ of that variable (the `env` map).

Next you need to know which configurations the `ParseTree list` parameter can have.  
Again lets look at the _list example_ and the example parse tree _ListTest.svg_ above.  
If we had defined a semantic action associated to the non-terminal `start` we could expect up to two elements in this parameter when our action would be called.
The grammar definition allows us to omit either the repetition after the first number or to omit even the complete list.  
Thus the action must be able to handle all situations correctly.

An appropriate means is to pattern match the `ParseTree list` parameter using a `match` expression which covers all valid configurations.
If needed you can then vary your actual user stack manipulation depending on the current user stack configuration.

Keep in mind that the user stack is your AST stack and that the top of stack is the head of the list. In some situations you need to process the elements on your stack in reversed order.

A common pattern when processing an AST stack in a semantic action is to look at the number of elements in the `ParseTree list` parameter and then take the same count of elements from the user stack, reverse them if necessary, process them and push the result back.  
The reason why this is so idiomatic is that other semantic actions have been called previously while the children of your node were processed and they have placed their results on the user stack.  
Because of the way the parser works it is guaranteed that the children of your node had been processed before the semantic action at your node is called.
