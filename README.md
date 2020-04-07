# Quickcheck example

[./src/Lib.hs](./src/Lib.hs) contains one type, `JSON`, and two functions, `parseJSON` to parse a `String` to a `JSON` and `dumpJSON` to dump a `JSON` to a `String`.  `dumpJSON` has subtle bugs in it that will be revealed in the test.

[./test/Spec.hs](./test/Spec.hs) contains a quickcheck property asserting that stringifying and parsing a JSON object is a noop.

## Running

Assuming that [stack](http://docs.haskellstack.org/en/stable/README/) is installed:

```bash
stack install
stack test
```