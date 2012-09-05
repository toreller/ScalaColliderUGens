## ScalaCollider-UGens

### statement

An extension project to ScalaCollider which aims at providing a language-neutral (XML based) UGen description database as well as a code synthesizer that generates class files for ScalaCollider. It is (C)opyright 2008-2012 by Hanns Holger Rutz. All rights reserved. ScalaCollider-UGens is released under the [GNU General Public License](http://github.com/Sciss/ScalaColliderUGens/blob/master/licenses/ScalaCollider-License.txt) and comes with absolutely no warranties. To contact the author, send an email to `contact at sciss.de`.

Big thanks to Mirko Stocker for the [Scala-Refactoring Library](http://scala-refactoring.org/) which provides the AST-to-source code converter.

### compilation

ScalaCollider-UGens builds with sbt 0.12 against Scala 2.9.2. To compile run `sbt compile`.

### running

The UGen descriptions reside in XML files. The project already comes with a file for the standard UGens included with a plain SuperCollider installation. You will need to create additional XML files if you wish to compile sources for third party UGens.

To synthesize the source code for a given UGen description XML file, run as follows:

    $ sbt
    > run -d path/to/scala/source/output path/to/descriptions.xml

Thus if ScalaCollider-UGens and ScalaCollider reside in the same parent directory, to re-create the standard UGens' class files:

    > run -d ../ScalaCollider/ descriptions/standard-ugens.xml

(Note that currently sbt seems to exit with an `InterruptedException`&mdash;you can ignore this).

The generated source files then need to be compiled against ScalaCollider. In the future, we might provide a minimum stub instead for the compilation.

### format

There is no DTD yet. But the structure of the XML file is as follows:

    <ugens>
       <file name="PluginName">
          <ugen name="UGenName" [ ugenAttrs ]>
             <rate name="RateName" [ implied="true" [ method="MethodName" ]]>
                [ <arg ... /> ]
             </rate>
             <rate ... />
             [ <outputs num="NumberOfOutputs"/> ]
             <arg name="ArgumentName" [ argAttrs ]>
                [ <doc>Argument description</doc> ]
             </arg>
             <arg ... />
             [ <doc>
                <text>UGen description</text>
                [ <see>ugen.RelatedUGenName</see>
                  <see ... /> ]
               </doc> ]
          </ugen>
          [ <ugen ... /> ]
       </file>
       [ <file ... /> ]
    </ugens>

All UGens within one `file` section are considered to be part of that particular `.scx` plugin. Their synthesized classes will also be grouped in a file by that name.

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

Part of this information is used by ScalaCollider when building the UGen graph. For example, subtrees which do not have any side effects are automatically removed. UGens which have side effects are those for which either of the following flags is set: `writesbus` | `writesbuf` | `writesfft` | `sideeffect`. Furthermore, multiple occurrences of UGens which are functionally equivalent are collapsed. UGens are _functionally not equivalent_ if either of the following flags is set: _any of the side effects_ | `random` | `indiv`. That is to say, if there are two `WhiteNoise` UGens, they are functionally distinct by definition and will thus not be collapsed. The same is true for two `Out` UGens, even if their inputs are the same, as they have accumulative side effects on the bus to which they write. On the other hand, two `SinOsc` UGens with the same frequency and rate inputs are functionally equivalent and thus one can be replaced for the other.

More flags and meta data are planned in future version, e.g. oscillator signal ranges, filter coefficients.

#### UGen Rates

The possible rate names are `"scalar"`, `"control"`, `"audio"`, and `"demand"`. Each supported rate should have its own element. There are two extra attributes, `implied` and `method`.

`implied` says that the UGen not only has exactly one supported rate (an exception is thrown if you have a UGen with multiple rate elements and an `implied` attribute), but that this a natural precondition for the type of UGen. That way, the `case class` for that UGen does not carry a `rate` argument, but mixes in a trait which provides it. As a consequence, there is no argument for the rate when using pattern matching against that UGen. For example, `K2A` makes only sense at audio rate, `A2K` makes only sense at control rate, `FreeVerb` and `Pitch` make only sense at audio rate. Using this attribute, we have `case class K2A(in: GE)` (with mixin `AudioRated`) instead of the redundant `case class K2A(rate: Rate, in: GE)`.

Be very __careful__ with this attribute, it should not be used if another rate could be added in a future SuperCollider version, as this would break binary compatibility. This is why `implied` has been removed from `DiskIn`, for example (there is no reason, why `DiskIn` could not support control rate reading in the future).

The second attribute, `method`, builds up `implied` and requires tha `implied` has been specified. It states that instead of the default method name in the companion object&mdash;`ir` for scalar rate, `kr` for control rate, `ar` for audio rate, and `dr` for demand rate&mdash;an alternative method name is used. The method name is typically `apply`, so that instead of `FFT.kr(buf, sig)` you have to write `FFT.apply(buf, sig)` or short `FFT(buf, sig)`, which is more convenient.

#### Argument Attributes ####

|Attribute Name|Value                            |Example   |
|--------------|---------------------------------|----------|
|`default`     |Default expression for the argument. This can be either a number literal or a code fragment such as `"SampleRate.ir * 0.5"`, `"60.midicps"` or `"doNothing"` (useful for `doneAction` arguments) |`440`, `1.0`, `inf` |
|`type`        |Argument type when it is not `GE`. Note that `"String"` (e.g. in `Poll`) is properly converted to a sequence of constant value inputs. Other types such as `Int` are treated as _auxiliary_ for the construction of the UGen (e.g. to determine its number of inputs or outputs) and _do not appear_ as actual UGen inputs. (See below for attribute `ugenin` which makes an exception) |`PanAz`, `Poll` |
|`rate`|Constrains the supported rate for this argument. The only value presently recognized is `"ugen"` which means the argument is required to run at the _same rate as the UGen_. Please see also the section below about rate specific argument settings. | `DiskOut` (`in`) |

The following three argument attributes have boolean values, and are false by default:

|Attribute Name|Meaning when value is `"true"`   |Example   |
|--------------|---------------------------------|----------|
|`ugenin`|Forces an `Int` type argument to be used as actual UGen input and not just auxiliary type. | `MFCC` |
|`multi`|Indicates an argument which expands over multiple UGen inputs. | `RecordBuf` (`in`), `Dseq` (`seq`) |
|`doneflag`|Forces the argument input to be a UGen which sets a done-flag (e.g. `Line`). That way it is prevented that for example a `FreeSelfWhenDone` UGen is fed by another UGen which is falsely believed to set a done-flag, but which actually doesn't.| `FreeSelfWhenDone`|

Arguments should be chosen careful not to conflict with methods available on `GE`. This is the reason, why various arguments which are named `rate` in SCLang have been renamed for example to `speed`, `freq` etc. It is recommended to take a look at the naming of the arguments in the default plugins (rather than relying on the naming in SCLang which is often unreflected and irregular) and try to reuse them whenever possible, and to be as consistent as possible with abbreviations. Care is also needed with the default values. There are some default values in SCLang which are insensible, while other useful defaults are missing. The aim is not to provide default values for every possible argument, but to require to fill in arguments for which defaults do not make sense.

#### Argument Positions

For some UGens, the actual positions of the arguments as they are coded in the underlying Plugin are either unintuitive (e.g. with respect to argument priority), inconvenient (e.g. with respect to default values), or irregular (e.g. different from an otherwise very similar other UGen). In those cases you are permitted to change the argument order as it is presented to the ScalaCollider user. To make sure the arguments are properly wired in the resulting SynthDef, explicit argument positions must be given.

If a UGen's arguments do not have `pos` attributes, they are considered in the order in which they appear in the XML file. Otherwise, the order of appearance in the XML file corresponds with the order __in the underlying Plugin__, whereas the values of the `pos` attributes specify the positions as presented to the user (counting from zero). Please read the previous sentence __very carefully__, as a common mistake is to falsely believe the correspondence to be the other way around.

As an example, consider `XOut` which has the unintuitive argument order of _bus_, followed by _cross-fade level_, followed by _input signal_. Compare this to `Out` which has the two arguments of _bus_, followed by _input signal_. We decided to make the `XOut` arguments appear to the user in the order of _bus_, then _input signal_ (just like `Out`), then followed by the distinguishing parameter of the _cross-fade level_. Thus we assign `pos="1"` to the `in` argument and `pos="2"` to the `xfade` argument, so they switch their positions. To minimize mistakes, ScalaCollider-UGens requires that we also add `pos="0"` to the `bus` argument, even if that does not affect its final position. The whole UGen specification thus becomes:

    <ugen name="XOut" writesbus="true">
       <outputs num="0"/>
       <rate name="audio">
          <arg name="in" rate="ugen"/>
       </rate>
       <rate name="control"/>
       <arg name="bus" pos="0"/>
       <arg name="xfade" pos="2"/>
       <arg name="in" multi="true" pos="1"/>
       <doc warnpos="true"/>
    </ugen>

__Note__ how the attribute `warnpos="true"` was added to the `doc` element. This makes Scaladoc add an extra note to alert the user of the change in argument order. This is particularly important, as it may create confusion when coming from SCLang. It is recommended to apply argument reorderings only after careful consideration, and to abstain from them when in doubt.

#### Rate Specific Argument Settings

Sometimes it is necessary to change the default value or the description of an argument _with respect to the rate_ at which the UGen runs. And sometimes the rate constraint for an argument only applies to particular rates at which the UGen runs. In this case, it is permitted to embed an auxiliary `arg` element inside the `rate` element. This auxiliary `arg` element must have a corresponding element in the outer scope (inside the `ugen` element). Their correspondence is established by using the same `name` attribute, and the auxiliary element may provide an additional `default` or `rate` attribute and may contain an additional `doc` element.

As an example for different default values, here is the full text of `LeakDC`:

    <ugen name="LeakDC">
       <rate name="control">
          <arg name="coeff" default="0.9"/>    <!-- provide a default value for the `kr` method -->
       </rate>
       <rate name="audio">
          <arg name="coeff" default="0.995"/>  <!-- provide a default value for the `ar` method -->
       </rate>
       <arg name="in" rate="ugen"/>
       <arg name="coeff"/>  <!-- the outer argument must still be provided -->
    </ugen>

An an example of restricting the argument's rate only in certain cases is `Out`:

    <ugen name="Out" writesbus="true">
       <outputs num="0"/>
       <rate name="audio">
          <arg name="in" rate="ugen"/>
       </rate>
       <rate name="control"/>
       <rate name="scalar"/>
       <arg name="bus"/>
       <arg name="in" multi="true"/>
    </ugen>

Here, the "outer" definition of argument `in` says that the argument is a multi-channel argument, but it does not enforce a particular rate. Only for the case that `Out` is run at audio rate, the auxiliary entry for `in` enforces that `in` in this case must run at the same rate as the UGen (thus audio rate, too).

#### Number of Outputs

By default the UGen is considered to have one output. All other UGens must explicitly contain an `<outputs ... />` element. This element must have a `num` attribute which specifies the number of outputs either as a number literal or as a name of one of the UGen's arguments:

|Example                       |Type             |UGen    |
|------------------------------|-----------------|--------|
|`<outputs num="0"/>`          | Constant Literal| `Out`  |
|`<outputs num="2"/>`          | Constant Literal| `Pan2` |
|`<outputs num="in"/>`         | Reference to a `GE` argument, here meaning that the number of outputs equals the number of channels of the multi-channel argument input `in` | `Demand` |
|`<outputs num="numChannels"/>`| Reference to an `Int` argument, meaning that this argument provides the number of outputs directly | `DiskIn` |

#### Descriptions

The description text for arguments is the text inside the argument's `<doc></doc>` element. The description text for the UGen is inside the `<text></text>` element inside the `<doc></doc>` element. In each case, standard [Scaladoc formatting](https://wiki.scala-lang.org/display/SW/Syntax) is allowed. Crosslinks are provided through any number of `<see></see>` elements.

Please follow carefully the style of the descriptions used for the standard UGens. They adhere mostly to [Javadoc style practice](http://www.oracle.com/technetwork/java/javase/documentation/index-137868.html#styleguide), and not so much to the more colloquial style of SCLang docs. The purpose here is not to include lengthy examples, but to be technically precise in the meanings of the argument values and the exact functioning of the UGen, if possible covering corner cases, providing details about underlying formulas, phase behavior of oscillators, typical ranges and scale.

Whenever the argument order has been significantly changed from the SCLang counterpart, the UGen's `doc` element should contain the attribute `warnpos="true"` which will create a special highlight in the Scaladocs to alert the reader of this change.
