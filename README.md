# Uppx

Allows extending an UPPAAL model with annotated blocks, e.g.:
```xml
...
<nta>
    <declaration>
        ...
        // @myAnnotation
        const int v1 = 1;
        const int v2 = 2;

        ...
```
And reads a companion MS Excel file (with the same base name) with tables that describe how to adapt the block following an annotation command, until the next empty line.
For example, with the expression and table below in a sheet called `@myAnnotation`, the values 1 and 2 will become 10 and 20.

 `const int $var = $number;`

| var | num |
| --- | --- |
| v1 | 10 |
| v2 | 20 |


## Build the (fat) jar

Use the sbt task to build the self-contained jar-file:
```bash
sbt assembly
```

## Run the jar

Copy the `target/scala-3.0.0/uppx.jar` file to your working directory, where the `uppaal.xml` and `uppaal.xlxs` files are, and run:
```bash
java -Xss16m -jar uppx.jar
```

(The option `-Xss16m` increases the stack memory size, required to parse larger files, since the parser requires a stack proportional to the number of lines.)