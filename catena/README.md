# catena

A sequence type with O(1) concatenation and concrete in-memory representation.

This is similar to `DList` in many ways, but may perform better in some
circumstances, particularly when GHC cannot inline and simplify away the
function-based representation of `DList`, e.g. when the accumulation spans many
modules.  For short-lived local accumulations, `DList` is likely to be better.

When sequences are retained in memory for a long time, `Catena` is likely to be
a better choice: because `DList` ultimately consists of closures over
(possibly-unevaluated) lists and single elements, its best-case density is one
element per two heap objects (in the case of right-associated chains of `cons`
or a single `fromList` of a spine-forced list).  Meanwhile, `Catena` stores
elements in `SmallArray` when it's practical to do so, and has specific data
constructors for common operations, thus storing the same sequence with fewer
heap objects.
