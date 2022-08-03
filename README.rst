Modern-Compiler-Implementation-in-ML-book
==========================================

https://www.cs.princeton.edu/~appel/modern/ml/

Tiger
------------

Tiger is a simple but nontrivial language of the Algol family.

Two name spaces:

#. Types
#. Functions and variables

Development
------------------

Generate messages from Menhir:

.. code-block:: bash

  menhir --external-tokens Token --strict --explain --list-errors parser.mly > parser.messages

TODO
--------

#. Improve the parsing of ``&`` ``|`` and unary negation; The current approach in
   the AST will make it hard to provide quality error messages that relate
   to the source code.
#. Improve the treatment of escaped variables; It is currently hacked into
   the AST as an "escape" ref bool.
