match 3 with
  a -> 4
| b -> 5
| _ -> 6

match 3 with
  Nil () -> 4
| Cons (43, b) -> 5

match 3 with
  (a, "foo") when a > 5 -> 5 + a
| (3, "bar") -> 3
| (_, "baz") -> 66

match 7 with
_ -> 8
