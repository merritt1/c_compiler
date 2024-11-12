To build type:

`make`

To clean:

`make clean`

Usage:
    ./compiler [-s (scanner|parser|symbol|type|ir|mips] [-o output] [input] [-a] [-O (0|1|2|3)]
 
 -s : the name of the stage to stop after. Defaults to
      runs all implemented stages if not specified.
 -a : annotates all references to identifiers. Can only be used with
      stage "symbol."
-O  : Optimization level, from 0-3. Only valid for IR and mips stages.

The input file should be a (lexically) valid source language program.
