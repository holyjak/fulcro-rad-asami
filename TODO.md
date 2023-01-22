# TODO

* How to completely remove an attribute?

## Ideas

Support storing primitive values with `:cardinality :many` as arrays, e.g. an array of ints: `:product/dimensions [25 34]`.
 * Notice that Asami does not store it as-is but creates a node for each one, and we can query for it with
   `:find [?v ...] :where [?e :array/attr ?attr] [?attr :a/contains ?v]`. The entity `:attr'` notion can replace the value but
   doing that with `:db/retract` seems difficult as we'd need to find all the nodes and all their props... Also, how to completely delete the whole value?