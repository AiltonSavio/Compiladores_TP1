# tigris-ocaml

A compiler for the Tigris programming language targetting LLVM and implemented in OCaml.

## Preparing the environment for the project in Ubuntu >= 18.04

```
$ sudo apt install m4        # processador de macro de uso geral que é usado por vários componentes do OCaml
$ sudo apt install rlwrap    # utilitário que usa a biblioteca readline para permitir a edição da entrada do teclado para qualquer comando
$ sh <(curl -sL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh) # install opam
$ opam init --bare           # inicializa estado interno do opam no diretório ~/.opam
$ opam switch create 4.07.1  # instala compilador de OCaml
$ eval $(opam env)           # aplica as alterações para o shell atual        
$ opam install dune          # sistema de construção para OCaml
$ opam install merlin        # serviço de editor que fornece recursos IDE modernos para o OCaml
$ opam install ppx_import    # extensão de sintaxe que permite extrair tipos ou assinaturas de outros arquivos de interface compilados
$ opam install ppx_deriving  # extensão de sintaxe que facilita geração de código baseada em tipos em OCaml
$ opam install ppx_expect    # extensão de sintaxe para escrita de testes em OCaml
$ opam install camomile      # biblioteca unicode para OCaml
$ opam install menhir        # gerador de analisador sintático para OCaml
$ opam install utop          # ambiente interativo alternativo para OCaml
```

## How to clean

```
$ dune clean
```

## How to compile

```
$ dune build src/bin/driver.exe
```

## How to run the driver

```
$ dune exec src/bin/driver.exe
```

## How to run expect tests

```
$ dune runtest
```

## How to promote outputs of the expect tests

```
$ dune promote
```

## References

[Recipes for OCamlLex](https://medium.com/@huund/recipes-for-ocamllex-bb4efa0afe53)

