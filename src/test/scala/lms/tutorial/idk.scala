package scala.lms.tutorial.idk

import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import scala.collection.mutable.ArrayBuffer

class Data

object Data {
    case class Number(value: Double = 0) extends Data
    case class Str   (value: String = "") extends Data
    case class Array (value: ArrayBuffer[Data] = ArrayBuffer()) extends Data
    case class Object(value: HashMap[Data, Data] = HashMap()) extends Data
    case class Bool  (value: Boolean = false) extends Data
    case class Func  (value: Function[Data.Array, Data] = (_ => Data.Undef())) extends Data
    case class Null  () extends Data
    case class Undef () extends Data

    def createElement(
        tag: Data.Str,
        attrs: Data.Object,
        children: Data.Array
    ): Data.Object = {
        val m: HashMap[Data, Data] = HashMap(
            Data.Str("tag") -> tag,
            Data.Str("children") -> children,
            Data.Str("replaceChildren") -> Data.Func((children: Data.Array) => {
                children.value.clear()
                children.value.appendAll(children.value)
                Data.Undef()
            })
        )
        Data.Object(m)
    }

    def createText(value: Data.Str): Data.Object = {
        val m: HashMap[Data, Data] = HashMap(Data.Str("textContent") -> value)
        Data.Object(m)
    }
}

trait Valued {
    var value: Data = Data.Undef()
}

trait Effectful {
    val effects: HashSet[Effect] = HashSet()
    def addEffect = effects.add(_)
    def remEffect = effects.remove(_)
}

abstract class VE extends Effectful with Valued 

object Effect {
    type Callback = Function[ArrayBuffer[Data], Data]
}

class Effect(
    callback: Effect.Callback,
    parents: ArrayBuffer[VE]
)
extends VE {
    parents.foreach(_.addEffect(this))
    value = {
        val args = parents.map(_.value)
        callback(args)
    }

    def run(): Unit = {
        val args = parents.map(_.value)
        value = callback(args)
        effects.foreach(_.run())
    }

    def cleanup() = {
        parents.foreach(_.remEffect(this))
    }
}

class State(val v: Data)
extends VE() {
    value = v

    def set(x: Either[Data, Function[Data, Data]]) = {
        value = x match {
            case Left(new_value) => new_value
            case Right(transform) => transform(value)
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
    def createState(value: Data) = {
        val st = new State(value)
        states.append(st)
        st
    }

    def createEffect(
        callback: Effect.Callback,
        parents: ArrayBuffer[VE]
    ) = {
        val ef = new Effect(callback, parents)
        effects.append(ef)
        ef
    }

    def mount(): Unit = {
        val root = render()
        val fn = wrapper.value(Data.Str("replaceChildren")).asInstanceOf[Data.Func]
        fn.value(Data.Array(ArrayBuffer(root)))

        onMount()
        children.foreach(_.mount())
    }

    def onMount() = {}

    def cleanup(): Unit = {
        children.foreach(_.cleanup())
        effects.foreach(_.cleanup())
        onCleanup()

        val fn = wrapper.value(Data.Str("replaceChildren")).asInstanceOf[Data.Func]
        fn.value(Data.Array())
    }

    def onCleanup() = {}

    def render(): Data.Object

    def component[NewComponent <: Component](comp: NewComponent) = {
        children.append(comp)
        wrapper
    }

    def text(value: Either[String, VE]) = {
        val node = Data.createText(Data.Str(""))
        value match {
            case Left(str) => {
                node.value(Data.Str("textContent")) = Data.Str(str)
            }
            case Right(ef) => {
                createEffect(
                    (params) => {
                        val new_str = params(0).asInstanceOf[Data.Str]
                        node.value(Data.Str("textContent")) = new_str
                        new_str
                    },
                    ArrayBuffer(ef)
                )
            }
        }
        node
    }

    def element(
        tag: String,
        attrs: Either[HashMap[String, Either[Data, VE]], VE] = Left(HashMap()),
        children: ArrayBuffer[Data] = ArrayBuffer()
    ) = {
        val node: Data.Object = Data.createElement(
            Data.Str(tag),
            Data.Object(HashMap()),
            Data.Array(children)
        )
        attrs match {
            case Left(map) => {
                map.foreach(p => {
                    val key = Data.Str(p._1)
                    val value = p._2
                    value match {
                        case Left(dt) => {
                            node.value(key) = dt
                        }
                        case Right(ef) => {
                            createEffect(
                                (params) => {
                                    val new_dt = params(0).asInstanceOf[Data.Str]
                                    node.value(key) = new_dt
                                    new_dt
                                },
                                ArrayBuffer(ef)
                            )
                        }
                    }
                })
            }
            case Right(ef) => {
                createEffect(
                    (params) => {
                        val prev_val = ef.value.asInstanceOf[Data.Object]
                        val new_val = params(0).asInstanceOf[Data.Object]
                        prev_val.value.keys.foreach(node.value.remove(_))
                        new_val.value.foreach(p => node.value(p._1) = p._2)
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
    wrapper: Data.Object,
    props: HashMap[Data, Data] = HashMap()
) extends Component(wrapper, props) {
    def render() = {
        element("div", Left(HashMap()), ArrayBuffer(
            component(new ExampleChild(element("div")))
        ))
    }
}

class ExampleChild(
    wrapper: Data.Object,
    props: HashMap[Data, Data] = HashMap()
) extends Component(wrapper, props) {
    def render() = {
        text(Left("child"))
    }
}

object Main {
    def main(args: Array[String]) = {
        val root = Data.createElement(
            new Data.Str("div"),
            new Data.Object(),
            new Data.Array()
        )
        val c = new ExampleParent(root)
        print(c.render())
    }
}

// import scala.lms.common._

// class TestIdk extends TutorialFunSuite {
//     test("1") {
//         checkOut("1", "txt", {
//             print("Hello")
//         })
//     }
// }