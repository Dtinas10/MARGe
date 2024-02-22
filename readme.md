# Animator of Multi Action Reactive Graphs (MARGe)

Experiments in https://dtinas10.github.io/MARGe/lib/caos/tool/index.html

Tutorial Video: https://www.dropbox.com/scl/fo/cm0tw42zlebqqzh7s054a/h?rlkey=urd0z5ern6akgkc3l8dqq8l7c\&dl=0

# Caos

This project uses and the Caos's framework, placed at `lib/caos`. More information on it can be found online:

 - Caos' GitHub page: https://github.com/arcalab/CAOS
 - Caos' tutorial: https://arxiv.org/abs/2304.14901
 - Caos' demo video: https://youtu.be/Xcfn3zqpubw 

The project can also be included as a submodule, as explained in the documentation of Caos.

## Requirements

- JVM (>=1.8)
- sbt

## Compilation

You need to compile this project using the ScalaJS plug-in, following the steps below.
The result will be a JavaScript file that is already being imported by an existing HTML file. 

1. `sbt fastLinkJS`
2. open the file `lib/tool/index.html`
