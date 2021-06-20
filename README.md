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
For example, with the table below in a sheet called `@myAnnotation`, the values 1 and 2 will become 10 and 20.

`const int $var = $number;`| |
| -
| v1 | 10 |
| v2 | 20 |

