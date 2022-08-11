Modern-Compiler-Implementation-in-ML-book
==========================================

https://www.cs.princeton.edu/~appel/modern/ml/

Usage
-----------

.. code-block:: bash

   dune exec ./bin/tigerc.exe

Layers of abstraction
---------------------

The Tiger compiler has two layers of abstraction between semantic analysis
and frame-layout details:

.. image:: img/abs.png

The ``Frame`` and ``Temp`` interfaces provide machine-independent views of
memory-resident and register-resident variables. The ``Translate`` module
augments this by handling the notion of nested scopes (via static links),
providing the interface ``Translate`` to the ``Semant`` module.

Activation Records (aka stack frames)
-------------------------------------

The area of the stack devoted to the local variables, parameter, return
address, and other temporaries for a function.

* Local variables created upon entry to the function
* Destroyed when a function returns
* LIFO: A function returns only after all the functions it has called have
  returned.

When a stack cannot hold local variables (higher-order functions):

It is the combination of *nested functions* (where inner functions may use
variables defined in the outer functions) and *functions returned as results*
(or stored in variables) that causes local variables to need lifetimes
longer than their enclosing function invocations.

Tiger treats the stack as a big array with a stack pointer.

The design of a frame layout takes into account the particular features of an
instruction set architecture and the programming language we are compiling.

Tiger
-----

Tiger is a simple but nontrivial language of the Algol family,
with nested scope and heap-allocated records.

Two name spaces:

#. Types
#. Functions and variables

Development
-----------

Generate messages from Menhir:

.. code-block:: bash

  menhir --external-tokens Token --strict --explain --list-errors parser.mly > parser.messages

TODO
--------

#. Improve the parsing of ```&`` ``|`` and unary negation; The current approach in
   the AST will make it hard to provide quality error messages that relate
   to the source code.
#. Improve the treatment of escaped variables; It is currently hacked into
   the AST as an "escape" ref bool.
