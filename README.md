# Koko, aka `::'

[![Circle CI](https://circleci.com/gh/lessrest/koko.svg?style=svg)](https://circleci.com/gh/lessrest/koko)

```
$ :: [ @print-line Hello, world! ]
Hello, world!
```

```
$ :: [ let hey { [ @print-line hello, %1 ] } : [ @hey world! ] , [ @hey you! ] ]
hello, world!
hello, you!
```
