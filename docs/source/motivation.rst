==========
Motivation
==========

When designing LibCST, we used the following list of motivations.

Exact Representation
--------------------

* **Trees should be rewritable.** It should always be possible to take a valid python file, parse it to a CST using LibCST and then write that tree back out exactly, byte for byte. When changing nodes in the tree, changes to the original source file should be localized to the area represented by the changed portion of the tree. Effectively, for all valid python inputs, the following equation should be true::

    parse_module(some_input).code == some_input

* **Nodes should be constructed exactly as written in code.** No magic should happen on initialization and all construction should be explicit. Nodes should directly correlate to the code they represent and vice versa.

Ease of Traversal
-----------------

* **As flat as possible.** There shouldn't be an AsyncFunction wrapper containing a FunctionDef just because the grammar specifies it that way. Instead, we should make a FunctionDef node and give it an async attribute. Instead of representing parenthesis as wrapper nodes, they should be attached to the expressions that they operate on. In any scenario where we could achieve deduplication of LibCST code through extra layers in the resulting tree, we will opt for more code in order to make traversal simpler.
* **As regular as possible.** A module should always have a list of statements, even if that list is empty or only has one item. Irregularity makes tree inspection more difficult.
* **As high-level as possible.** The tree should be as close to the Python AST as possible. It should not be necessary to understand Python syntax in order to traverse the tree correctly. You should not have to know to ignore commas when traversing a list of parameters for a function. You should not have to use helper functions to traverse or recognize expressions wrapped in parenthesis. A LibCST node will represent its semantic operation in python with as little syntactic trivia exposed as possible.

Ease of Modification
--------------------

* **All nodes should be fully typed.** A module is a list of statements, not a list of untyped nodes. A function has a name, parameters and an optional return. It should be clear where to access various attributes of each node and what are the valid node types that can be used for that attribute.
* **Additional runtime (in addition to static types) constraints.** It shouldn't be possible to construct a node that can't be serialized correctly or that would result in invalid code. You shouldn't be able to construct a Name node with a string that isn't a valid python identifier. Strong constraints here should allow us to perform multiple passes safely without serializing and re-parsing the tree after each pass.
* **Sane defaults.** If I construct a node, I shouldn't have to supply whitespace, commas or other required syntax unless I want to. I should be able to treat the node in abstract, specifying only the semantics of the resulting code.
* **Reasonably intelligent ownership of whitespace.** A statement should own the comments directly above it, and any trailing comments on the same line. If we delete that statement, the whitespace should disappear with it.
* **It should be easy to change a single field** in an existing node without needing to modify or fix up adjacent nodes. Syntactic trivia such as commas or proper spacing between nodes should be children of the node they logically belong to so that inserting or removing a node does not require modifications to adjacent nodes.
* **Reparentable.** It should be possible to move or copy a node from one part of the tree easily.

Well Tested
-----------

* **All nodes should be fully tested.** It should not be possible to break upstream parsing or rendering code with a change to LibCST. Parsing, rendering and verifying functionality are all tested as completely as possible for all defined nodes.
