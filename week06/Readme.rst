Treaps
======

A cross between a tree and a heap. Can lead to simpler, yet more efficient Binary Search Trees.

A **Treap** is a tree containing as nodes records that have two keys. By the first key, the tree is
a *Binary Search Tree* (BST). By the second key, the tree is a *Heap*.

It turns out that, given any set of such records with two keys, it is always possible to build a treap out of those
nodes. However, there is no guarantee that it would be possible to build a *balanced tree*.

Trees are useful as data structures only when they are *balanced*, i.e. there exists a logarithmic bound on their
depth. To make the treap balanced and put it to some good use, we ignore its use as a heap. Instead, we generate random
numbers for the heap key every time we create a new node. So we have a BST w.r.t. the first key, and a heap of random
numbers w.r.t. the second key. The trick here is that as we maintain the heap property for those random second keys,
we create a *high likelihood for the tree to become balanced*. This is not a guarantee, but rather a high probability.
Nevertheless, this is sufficient in practice, and it results in a simpler implementation for a balanced BST, as compared
to AVL or Red-Black Trees.