# Uppx

Allows extending an UPPAAL model with __annotated blocks__ and __XML blocks__, e.g.:
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
java -jar uppx.jar
```


## Using a simple example

You can find a simple example in `examples/simple`.
It includes a minimalistic Uppaal file, a simple Excel file, and a script `runuppaal.command` to call Uppx and Uppaal.
This example includes both __annotation__ and __XML__ blocks; the former are in sheets whose name starts in `@`, and the latter are in sheets whose name is surrounded by angular brackets `<>`.

To run the simple example, first build the fat jar (`sbt assembly`).
Then, using the command line in the folder of the example, type `sh runuppaal.command`.