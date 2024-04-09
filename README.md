# OCaml in Algebraic Effect Handlers
An example program that applies the theory of algebraic effects and handlers in OCaml.

## Getting Started
The following steps help to set up a local copy of the project.

### Installation
Installs curl
``` 
sudo apt install curl
```
Installs opam, the package manager for OCaml.
``` 
sh <(curl -sL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)
```
Sets up environment
``` 
opam init 
```
Sets up necessary environment variables
``` 
eval $(opam env) 
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
``` 
ocamlc -o my_file utils.ml effectful_program.mli effectful_program.ml monitor.mli monitor.ml run_file2.ml
```

Opens utop
``` 
utop 
```

Changes directory to the one where the project is found
``` 
#directory "project_path/modular_prog/fix";; 
```

Loads the project's modules
``` 
load "utils.cmo";; #load "effectful_program.cmo";; #load "monitor.cmo";; #load "run_file.cmo";;
```

Runs a monitor from the available monitors in ```Monitor```           
``` 
Run_file2.mon ();; 
```

### Structure of the project
The files ```effectful_program.ml``` and ```effectful_program.mli``` contain the definition of the operations [put] and [get] along with a sample program [main] that applies them. 

In ```monitor.ml``` and ```monitor.mli``` we have several monitor examples, expressed using algebraic effect handlers. We make use of these monitors in ```run_file2.ml```, where a monitor is used by calling its respective run function. 