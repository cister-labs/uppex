# Uppex

Allows extending an UPPAAL model with __annotated blocks__ and __XML blocks__, e.g.:
```xml
<nta>
    <declaration>
        ...
        // @myAnnotation
        const int v1 = 1;
        const int v2 = 2;
        
        ...
    </declaration>
    ...
    <queries>
        ...
    </queries>
    ...
</nta>
```
And reads a companion MS Excel file (with the same base name) with tables that describe how to adapt the block following an annotation command, until the next empty line.
For example, with the expression and table below in a sheet called `@myAnnotation`, the values 1 and 2 will become 10 and 20.

<table>
  <tbody>
    <tr>
      <td colspan="3">
          <code class="language-plaintext highlighter-rouge">const $type $var = $number;</code>
      </td>
    </tr>
<!--   </tbody>
  <thead> -->
    <tr>
      <th style="font-weight: 600;text-align: center;">var</th>
      <th style="font-weight: 600;text-align: center;">type</th>
      <th style="font-weight: 600;text-align: center;">num</th>
    </tr>
<!--   </thead>
  <tbody> -->
    <tr>
      <td>v1</td>
      <td>int</td>
      <td>10</td>
    </tr>
    <tr>
      <td>v2</td>
      <td>int</td>
      <td>20</td>
    </tr>
  </tbody>
</table>

<!--  `const int $var = $number;`

| var | num |
| --- | --- |
| v1 | 10 |
| v2 | 20 |
 -->

It is also possible to replace the content of the `<queries>` block by introducing a sheet named `<queries>` to our spreadsheet with a table like the one below:

<table>
  <tbody>
    <tr>
      <td colspan="3">
          <code class="language-plaintext highlighter-rouge">&lt;query&gt; &lt;formula&gt;$Formula&lt;/formula&gt; &lt;comment&gt;$Comment&lt;/comment&gt; &lt;/query&gt;</code>
      </td>
    </tr>
<!--   </tbody>
  <thead> -->
    <tr>
      <th style="font-weight: 600;text-align: center;">Formula</th>
      <th style="font-weight: 600;text-align: center;">Comment</th>
    </tr>
<!--   </thead>
  <tbody> -->
    <tr>
      <td><code class="language-plaintext highlighter-rouge">A[]!deadlock</code></td>
      <td>No deadlocks</td>
    </tr>
    <tr>
      <td><code class="language-plaintext highlighter-rouge">A[] W.Idle</code></td>
      <td>The worker is always Idle</td>
    </tr>
  </tbody>
</table>

<!-- 
`<query> <formula>$Formula</formula> <comment>$Comment</comment> </query>`

|Formula | Comment|
| ------ | -------|
|`A[]!deadlock` | No deadlocks|
|`A[] W.Idle` | The worker is always Idle|
 -->
This concrete table will replace the content of the `<queries>` block by two `<query>` blocks containing the corresponding formulas and comments from the table.


## Supporting multiple configurations

Often we want to experiment with different combinations of values and queries.

Uppex supports the specification of a list of configurations, each producing variations of the values and XML blocks, following principles from Software Product Line Engineering.
Uppex generates, for each of the provided configurations, a different instance of the original UPPAAL model and verifies all properties of all instances.
We will first see how to define configuration by selecting desired "features", and then how to enrich the annotations with these "features".

### Specifying configurations

A special sheet named `@Configurations` lists and specifies the configuration in a table similar to the one below:

|Configuration | _Feature1_ | _Feature2_ | ...  |
| :----------- | :------: | :-------: | ---- |
| _Main_ |  |  |  |    
| _Conf2_ | x | | |
| _Conf3_ |  | x | |

The names below __Configuration__ identify the set of desired configurations, and every non-empty cell at the right of these selects the feature name at the top of the corresponding column.

In this example we have 3 configurations: _Main_, _Conf2_, and _Conf3_; configuration _Conf2_ selects _Feature1_, _Conf3_ selects _Feature2_, and _Main_ does not select any feature.
Each feature selection in a configuration will modify the annotations, explained below, yielding a different instance of the UPPAAL model.

### Enriching annotations with features

Recall that each annotation is described by a table with multiple columns, each with a header identifying a pattern name. A special column named `Features` is used to map entries to feature names, such as the ones in the configuration table. Each line of an annotation table is included only if all of its names in the `Features` column (separated by commas) are selected in the configuration. In case of entries with the same left-most value, the last one prevails. 

For example, using the table below and considering the configuration table above, selecting the configuration _Conf3_ would produce our original Uppaal model, but selecting _Conf2_ would instead assign `10000` to `v1`.

<table>
  <tbody>
    <tr>
      <td colspan="4">
          <code class="language-plaintext highlighter-rouge">const $type $var = $number;</code>
      </td>
    </tr>
<!--   </tbody>
  <thead> -->
    <tr>
      <th style="font-weight: 600;text-align: center;">var</th>
      <th style="font-weight: 600;text-align: center;">type</th>
      <th style="font-weight: 600;text-align: center;">num</th>
      <th style="font-weight: 600;text-align: center;">Features</th>
    </tr>
<!--   </thead>
  <tbody> -->
    <tr>
      <td>v1</td>
      <td>int</td>
      <td>10000</td>
      <td>Feature1</td>
    </tr>
    <tr>
      <td>v1</td>
      <td>int</td>
      <td>10</td>
      <td>Feature2</td>
    </tr>
    <tr>
      <td>v2</td>
      <td>int</td>
      <td>20</td>
      <td></td>
    </tr>
  </tbody>
</table>


## Build the (fat) jar

Dependencies:

 - SBT [(https://www.scala-sbt.org)](https://www.scala-sbt.org)
 - JVM (>=1.8)
 - UPPAAL (optional - https://uppaal.org)

Use the sbt task to build the self-contained jar-file running at the root:
```bash
sbt assembly
```

## Run the jar

The compiled jar-file can be found in `target/scala-3.0.2/uppex.jar`.
You can copy it to your working folder, with the UPPAAL and Excel files. To list the possible options run in the command line:
```bash
java -jar uppex.jar --help
```

For example, to apply the default configuration in a file `myfile.xlsx` to an UPPAAL model `myfile.xml`, you can run the command:

```bash
java -jar uppex.jar myfile.xlsx
```

The file names of the configuration and UPPAAL files must match.
You will be presented with a list of changes applied to the `myfile.xml`, this file will be updated, and a copy of the original file will be placed in a `backups` folder, which will be created if it does not exist.

To check all properties using UPPAAL of all configurations, you can run the command:

```bash
java -jar uppex.jar --runAll myfile.xlsx
```

This requires the command `verifyta` to be available in the `PATH`, which can be found in the binaries included by UPPAAL.
Extra options, such as a timeout value, can be defined here, requiring the command `timeout` to be available in the `PATH`.
This command will:

- output to the screen the results of verifying each property as they are verified, and
- produce a "report.html" file compiling all the results in a more readable format.


## Examples

You can find a simple example in folder [examples/simple](https://github.com/cister-labs/uppex/blob/main//examples/simple) to illustrate the usage of annotations. 
It includes a minimalistic Uppaal file, a simple Excel file, and a script `runuppaal.command` to call Uppex and Uppaal.
This example includes both __annotation__ and __XML__ blocks; the former are in sheets whose name starts in `@`, and the latter are in sheets whose name is surrounded by angular brackets `<>`.

To run the simple example, first build the fat jar (`sbt assembly`).
Then, using the command line in the folder of the example, type `sh runuppaal.command`.

---

A variation of this example can be found in folder [examples/simple-with-conf](https://github.com/cister-labs/uppex/blob/main/examples/simple-with-conf) that further illustrates how to apply __multiple configurations__, using the special sheet named `@Configurations`.


---

<!-- _This work is done by Infotiv AB under VALU3S project in a collaboration with RISE under ITEA3 European IVVES project. This project has received funding from the ECSEL Joint Undertaking (JU) under grant agreement No 876852. The JU receives support from the European Union’s Horizon 2020 research and innovation programme and Austria, Czech Republic, Germany, Ireland, Italy, Portugal, Spain, Sweden, Turkey._

_The ECSEL JU and the European Commission are not responsible for the content on this website or any use that may be made of the information it contains._
 -->
