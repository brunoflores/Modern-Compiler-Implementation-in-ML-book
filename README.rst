Modern-Compiler-Implementation-in-ML-book
==========================================

https://www.cs.princeton.edu/~appel/modern/ml/

.. code-block:: bash

  # Install the package manager
  npm install -g esy

  # Build dependencies
  make install-deps

  # Build all
  make build

  # Test all and exit
  make test

  # Test all and watch
  make test-watch

Tiger
------------

Tiger is a simple but nontrivial language of the Algol family.

Two namespaces:

#. Types
#. Functions and variables

Development
------------------

Generate messages from Menhir:

.. code-block:: bash

  menhir --external-tokens Token --strict --explain --list-errors parser.mly > parser.messages

TODO
--------

#. Improve the parsing of &, |, and unary negation; The current approach in
   the AST will make it hard to provide quality error messages that relate
   to the source code.
#. Improve the treatment of escaped variables; It is currently hacked into
   the AST as an "escape" ref bool.
