file:///C:/Users/Arnav/Documents/GitHub/my-tutorials/src/test/scala/lms/tutorial/idk.scala
### dotty.tools.dotc.core.TypeError$$anon$1: Toplevel definition EffectFunction is defined in
  C:/Users/Arnav/Documents/GitHub/my-tutorials/src/test/scala/lms/tutorial/idk.scala
and also in
  C:/Users/Arnav/Documents/GitHub/my-tutorials/src/test/scala/lms/tutorial/idk.scala
One of these files should be removed from the classpath.

occurred in the presentation compiler.

presentation compiler configuration:
Scala version: 3.3.3
Classpath:
<HOME>\AppData\Local\Coursier\cache\v1\https\repo1.maven.org\maven2\org\scala-lang\scala3-library_3\3.3.3\scala3-library_3-3.3.3.jar [exists ], <HOME>\AppData\Local\Coursier\cache\v1\https\repo1.maven.org\maven2\org\scala-lang\scala-library\2.13.12\scala-library-2.13.12.jar [exists ]
Options:



action parameters:
offset: 1896
uri: file:///C:/Users/Arnav/Documents/GitHub/my-tutorials/src/test/scala/lms/tutorial/idk.scala
text:
```scala
package scala.lms.tutorial.idk

import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import scala.collection.mutable.ArrayBuffer

class SString extends String

object Data {
    def createElement(
        tag: Data.String,
        attrs: Data.Object,
        children: Data.Array
    ): Data.Object = {
        val m: HashMap[Data, Data] = HashMap(
            Data.String("tag") -> tag,
            Data.String("children") -> children,
            Data.String("replaceChildren") -> Data.Func((children: Data.Array) => {
                children.value.clear()
                children.value.appendAll(children.value)
                Data.Undef()
            })
        )
        Data.Object(m)
    }

    def createText(value: Data.String): Data.Object = {
        val m: HashMap[Data, Data] = HashMap(Data.String("textContent") -> value)
        Data.Object(m)
    }
}

class Data {
    class Number(val value: Double = 0) extends Data
    class String(val value: SString = "") extends Data
    class Array (val value: ArrayBuffer[Data] = ArrayBuffer()) extends Data
    class Object(val value: HashMap[Data, Data] = HashMap()) extends Data
    class Bool  (val value: Boolean = false) extends Data
    class Func  (val value: Function[Data.Array, Data] = (_ => Data.Undef)) extends Data
    class Null  () extends Data
    class Undef () extends Data
}

abstract class Valued(var value: Data = Data.Undef) {}

abstract class Effectful(val effects: HashSet[Effect] = HashSet()) {
    def addEffect = effects.add
    def remEffect = effects.remove
}

class Effect(
    val callback: EffectFunction,
    val parents: ArrayBuffer[State | Effect],
    val effects: HashSet[Effect] = HashSet()
)
extends Effectful(effects)
with Valued(callback(parents.map(_.value))) {
    parents.foreach(_.addEffect(this))

    t@@ EffectFunction extends Function[ArrayBuffer[Data], Data]

    def run() = {
        val args = parents.map(_.value)
        value = callback(args)
        effects.foreach(_.run())
    }

    def cleanup() = {
        parents.foreach(_.remEffect(this))
    }
}

class State(value: Data)
extends Effectful()
with Valued(value) {
    def set(x: Data | Function[Data, Data]) = {
        value = x match {
            case new_value: Data => new_value
            case transform: Function[Data, Data] => transform(value)
        }
        effects.foreach(_.run())
    }
}

abstract class Component(
    val wrapper: Data.Object,
    val props: HashMap[Data, Data],
    val states: ArrayBuffer[State] = ArrayBuffer(),
    val effects: ArrayBuffer[Effect] = ArrayBuffer(),
    val children: ArrayBuffer[Component] = ArrayBuffer()
) {
    def cleanup() = {
        children.foreach(_.cleanup())
        effects.foreach(_.cleanup())
    }

    def createState(value: Data) = {
        val st = State(value)
        states.append(st)
        st
    }

    def createEffect(
        callback: EffectFunction,
        parents: ArrayBuffer[State | Effect]
    ) = {
        val ef = Effect(callback, parents)
        effects.append(ef)
        ef
    }

    def _mount() = {
        val root = render()
        val fn = wrapper.value(Data.String("replaceChildren")).asInstanceOf[Data.Func]
        fn.value(ArrayBuffer(root))

        mount()
        children.foreach(_._mount())
    }

    def mount() = {}

    def _unmount() = {
        children.foreach(_._unmount())
        unmount()
        val fn = wrapper.value(Data.String("replaceChildren")).asInstanceOf[Data.Func]
        fn.value(ArrayBuffer())
    }

    def unmount() = {}

    def render(): Data.Object

    def component[NewComponent <: Component](
        wrapper: Data.Object,
        props: HashMap[Data, Data] = HashMap()
    ) = {
        val comp = NewComponent(wrapper, props)
        children.append(comp)
        wrapper
    }

    def text(value: SString | Effect | State) = {
        val node = Data.createText(Data.String(""))
        value match {
            case str: Data.String => {
                node.value(Data.String("textContent")) = Data.String(str)
            }
            case ef: Effect | State => {
                createEffect(
                    (params) => {
                        val new_str = params(0).asInstanceOf[Data.String]
                        node.value(Data.String("textContent")) = new_str
                        new_str
                    },
                    ArrayBuffer(ef)
                )
            }
        }
        node
    }

    def element(
        tag: SString,
        attrs: HashMap[SString, Data | Effect | State] | Effect | State = HashMap(),
        children: ArrayBuffer[Data] = ArrayBuffer()
    ) = {
        val node: Data.Object = Data.createElement(
            Data.String(tag),
            Data.Object(HashMap()),
            Data.Array(children)
        )
        attrs match {
            case map: HashMap[Data.String, Data | Effect | State] => {
                map.foreachEntry((key, value) => {
                    value match {
                        case dt: Data => {
                            node.value(key) = dt
                        }
                        case ef: Effect | State => {
                            createEffect(
                                (params) => {
                                    val new_dt = params(0).asInstanceOf[Data.String]
                                    node.value(key) = new_dt
                                    new_dt
                                },
                                ArrayBuffer(ef)
                            )
                        }
                    }
                })
            }
            case ef: Effect | State => {
                createEffect(
                    (params) => {
                        val prev_val = ef.value.asInstanceOf[Data.Object]
                        val new_val = params(0).asInstanceOf[Data.Object]
                        prev_val.value.keys.foreach(node.value.remove(_))
                        new_val.value.foreach(node.value.addOne(_))
                        new_val
                    },
                    ArrayBuffer(ef)
                )
            }
        }
        node
    }
}

class ExampleParent(
    val wrapper: Element,
    val props: HashMap[Data, Data] = HashMap()
) extends Component(wrapper, props) {
    def render() = {
        element("div", HashMap(), ArrayBuffer(
            component[ExampleChild](element("div"))
        ))
    }
}

class ExampleChild(
    val wrapper: Element,
    props: HashMap[Data, Data] = HashMap()
) extends Component(wrapper, props) {
    def render() = {
        text("child")
    }
}

class TestIdk extends TutorialFunSuite {
    test("1") {
        checkOut("1", "txt", {
            print("Hello")
        })
  }
}
```



#### Error stacktrace:

```

```
#### Short summary: 

dotty.tools.dotc.core.TypeError$$anon$1: Toplevel definition EffectFunction is defined in
  C:/Users/Arnav/Documents/GitHub/my-tutorials/src/test/scala/lms/tutorial/idk.scala
and also in
  C:/Users/Arnav/Documents/GitHub/my-tutorials/src/test/scala/lms/tutorial/idk.scala
One of these files should be removed from the classpath.