K-D Trees
=========

These are binary search trees for spatial data. The simplest case would be a set of points in a plane.
Each point would have 2 coordinates, `x` and `y`, and the objective here would be to insert all these
points into a data structure which would allow an efficient answer to this question:

   Given a point ``(x0, y0)``, find the closest point ``(x, y)`` from the set given above.

One of the solutions is the K-D Tree, where K takes the values 2, 3, 4, etc... dependent on the number
of dimensions of the space at hand (a very common case are 2-D Trees, holding points in a plane).
For the case of K=2, this tree
alternates in splitting the space by either ``x`` or ``y``.

* The root contains the point such that half of all remaining points have a smaller ``x``, and reside
  in the left subtree.
* The two nodes on level ones are each the root of a subtree such that half of the root's children
  have a smaller ``y``, and reside in the left (sub-)sub-tree, whereas the other half has larger
  ``y`` and reside in the right (sub-)sub-tree.
* The nodes in the third level continue to split by ``x`` whereas the fourth level continues to split by ``y``.

In the general case where K is an arbitrary number, the tree cycles through the dimensions as we go deeper into the
tree.