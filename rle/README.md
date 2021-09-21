# rle

A normalized run-length-encoded list type.

This uses an `Eq` instance for the element type to coalesce runs of consecutive
equal values into pairs of run-length and value.  `Functor`, `Foldable`, and
`Eq` implementations are accelerated by any compression this achieves, while
`Traversable` is not available because it would need an `Eq` instance for the
element type.  (A standalone `traverse` function with this constraint is
provided).
