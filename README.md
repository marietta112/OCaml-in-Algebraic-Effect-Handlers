#OCaml in Algebraic Effect Handlers
An example program that applies the theory of algebraic effects and handlers in OCaml.

## Getting Started
The following steps help to set up a local copy of the project.

### Installation
Installs curl
``` sudo apt install curl
```
Installs opam, the package manager for OCaml.
``` sh <(curl -sL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)
```
Sets up environment
``` opam init 
```
Sets up necessary environment variables
``` eval $(opam env) 
```
Installs the OCaml compiler version 5.1.0 which is the required version for the ``` Effects ``` package
```
opam switch create 5.1.0
eval $(opam env)
```
Installs utop
``` 
opam install utop
eval `opam config env`
```

### Compile and Run
Compiles the project
``` ocamlc -o program program.mli program.ml 
```

Opens utop
``` utop 
```

Changes directory to the one where the project is found
``` #directory "project_path";; 
```

Loads the project's module
``` #load "program.cmo";; 
```

Runs the main function of the project
``` Project.mon ();; 
```

### Structure of the project
The project contains an interface file (program.mli) and an implementation file (program.ml). The interface file exposes the signature of three operations that are effectful, namely [put], [get] and [report_p], along with [mon] which acts on these three operations. The implementation file provides a meaning to these operations, where [mon] handles a sample computation using two composite handlers that print the value passed to [put], unless the integer is negative one. In this case, a warning message is printed to the console.
