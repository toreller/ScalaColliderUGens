## ScalaCollider-UGens

### statement

An extension project to ScalaCollider which aims at providing a language-neutral (XML based) UGen description database as well as a code synthesizer that generates class files for ScalaCollider. It is (C)opyright 2008-2012 by Hanns Holger Rutz. All rights reserved. ScalaCollider-UGens is released under the [GNU General Public License](http://github.com/Sciss/ScalaColliderUGens/blob/master/licenses/ScalaCollider-License.txt) and comes with absolutely no warranties. To contact the author, send an email to `contact at sciss.de`.

Big thanks to Mirko Stocker for the [Scala-Refactoring Library](http://scala-refactoring.org/) which provides the AST-to-source code converter.

### compilation

ScalaCollider-UGens builds with xsbt (sbt 0.10) against Scala 2.9.1. To compile run `sbt compile`. In order to keep up with the scala-refactoring snapshots, you may need to wipe the Ivy2 cache before updating: `rm -r ~/.ivy2/cache/org.scala-refactoring`.

### running

The UGen descriptions reside in XML files. The project already comes with a file for the standard UGens included with a plain SuperCollider installation. You will need to create additional XML files if you wish to compile sources for third party UGens.

To synthesize the source code for a given UGen description XML file, run as follows:

    $ xsbt
    > run -d path/to/scala/source/output path/to/descriptions.xml

Thus if ScalaCollider-UGens and ScalaCollider reside in the same parent directory, to re-create the standard UGens' class files:

    > run -d ../ScalaCollider/ descriptions/standard-ugens.xml

(Note that currently sbt seems to exit with an `InterruptedException`&mdash;you can ignore this).

The generated source files then need to be compiled against ScalaCollider. In the future, we might provide a minimum stub instead for the compilation.

### format

There is no DTD yet. But the structure of the XML file is roughly as follows:

    <ugens>
       <file name="PluginName">
          <ugen name="UGenName" [ ugenAttrs ]>
             <rate name="RateName" [ implied="true" [ method="MethodName" ]]/>
             <rate ... />

          </ugen>
          [ <ugen ... /> ]
       </file>
       [ <file ... /> ]
    </ugens>

All UGen within one `file` section are considered to be part of that particular `.scx` plugin. Their synthesized classes will also be grouped in a file by that name.

#### UGen Attributes

UGen Attributes (`ugenAttr`) are boolean flags (all false by default) which can be set to characterize a UGen:

|Attribute Name|Meaning when value is `"true"`   |Example   |
|--------------|---------------------------------|----------|
|`readsbus`    |UGen reads from a bus            |`In`      |
|`writesbus`   |UGen writes to a bus             |`Out`     |
|`readsbuf`    |UGen reads audio buffer data     |`BufRd`   |
|`writesbuf`   |UGen overwrites audio buffer data|`BufWr`   |
|`readsfft`    |UGen reads from an FFT buffer    |`IFFT`    |
|`writesfft`   |UGen writes to an FFT buffer     |`FFT`     |
|`doneflag`    |UGen sets a 'done flag'          |`Line`    |
|`sideeffect`  |UGen has another side effect, for example causing a 'done-action', sending OSC commands, or printing to the console |`SendTrig`|
|`random`      |UGen depends on random seeding   |`WhiteNoise`|
|`indiv`       |Each UGen is otherwise individual, even with identical inputs|Demand UGens advance their inputs|

Part of this information is used by ScalaCollider when building the UGen graph. For example, subtrees which do not have any side effects are automatically removed. UGens which have side effects are those for which either of the following flags is set: `writesbus` | `writesbuf` | `writesfft` | `sideeffect`. Furthermore, multiple occurrences of UGens which are functionally equivalent are collapsed. UGens are functionally equivalent if either of the following flags is set: any of the side effects | `random` | `indiv`.

More flags and meta data are planned in future version, e.g. oscillator signal ranges, filter coefficients.

#### UGen Rates

The possible rate names are `"scalar"`, `"control"`, `"audio"`, and `"demand"`. Each supported rate should have its own element. There are two extra attributes, `implied` and `method`.

`implied` says that the UGen not only has exactly one supported rate (an exception is thrown if you have a UGen with multiple rate elements and an `implied` attribute), but that this a natural precondition for the type of UGen. That way, the `case class` for that UGen does not carry a `rate` argument, but mixes in a trait which provides it. As a consequence, there is no argument for the rate when using pattern matching against that UGen. For example, `K2A` makes only sense at audio rate, `A2K` makes only sense at control rate, `FreeVerb` and `Pitch` make only sense at audio rate. Be very careful with this attribute, it should not be used if theoretically another rate could be added in a future SuperCollider version, as this would break binary compatibility. This is why `implied` has been removed from `DiskIn`, for example (there is no reason, why `DiskIn` could not support control rate reading in the future).

The second attribute, `method`, in turn requires that the `implied` attribute has also been specified. It states that instead of the default method name in the companion object&mdash;`ir` for scalar rate, `kr` for control rate, `ar` for audio rate, and `dr` for demand rate&mdash;an alternative method name is used. The method name is typically `apply`, so that instead of `FFT.kr(buf, sig)` you have to write `FFT.apply(buf, sig)` or short `FFT(buf, sig)`, which is more convenient.

