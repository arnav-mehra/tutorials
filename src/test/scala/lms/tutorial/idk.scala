package scala.lms.tutorial.idk

import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import scala.collection.mutable.ArrayBuffer
import java.util.Base64

object JS {
    class Data
    case class Number(value: Double = 0) extends Data
    case class Str   (value: String = "") extends Data
    case class Array (value: ArrayBuffer[JS.Data] = ArrayBuffer()) extends Data
    case class Object(value: HashMap[JS.Data, JS.Data] = HashMap()) extends Data
    case class Bool  (value: Boolean = false) extends Data
    case class Func  (value: Function[JS.Array, JS.Data] = (_ => JS.Undef())) extends Data
    case class Null  () extends Data
    case class Undef () extends Data

    def createElement(
        tag: JS.Str,
        attrs: JS.Object,
        children: JS.Array
    ): JS.Object = {
        val m: HashMap[JS.Data, JS.Data] = HashMap(
            JS.Str("tag") -> tag,
            JS.Str("children") -> children,
            JS.Str("replaceChildren") -> JS.Func((children: JS.Array) => {
                children.value.clear()
                children.value.appendAll(children.value)
                JS.Undef()
            })
        )
        JS.Object(m)
    }

    def createText(value: JS.Str): JS.Object = {
        val m: HashMap[JS.Data, JS.Data] = HashMap(JS.Str("textContent") -> value)
        JS.Object(m)
    }

    def findElementById(value: JS.Str): JS.Object = {
        JS.Object()
    }
}

object Enumerator {
    val encoder = Base64.getEncoder()
    var next_val: BigInt = 0

    def nextId() = {
        val bytes: Array[Byte] = next_val.toByteArray
        next_val += 1
        encoder.encodeToString(bytes)
    }
}

trait Valued {
    var value: JS.Data = JS.Undef()
}

trait Effectful {
    val effects: HashSet[Effect] = HashSet()
    def addEffect = effects.add(_)
    def remEffect = effects.remove(_)
}

trait VE extends Effectful with Valued 

object Effect {
    type Callback = Function[ArrayBuffer[JS.Data], JS.Data]
}

class Effect(
    callback: Effect.Callback,
    parents: ArrayBuffer[VE]
) extends VE {
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

class State(val v: JS.Data) extends VE {
    value = v

    def set(x: Either[JS.Data, Function[JS.Data, JS.Data]]) = {
        value = x match {
            case Left(new_value) => new_value
            case Right(transform) => transform(value)
        }
        effects.foreach(_.run())
    }
}

abstract class Component(
    val wrapper_tag: String,
    val wrapper_attrs: Either[HashMap[String, Either[JS.Data, VE]], VE] = Left(HashMap()),
    val props: HashMap[JS.Data, JS.Data] = HashMap(),
    val states: ArrayBuffer[State] = ArrayBuffer(),
    val effects: ArrayBuffer[Effect] = ArrayBuffer(),
    val children: ArrayBuffer[Component] = ArrayBuffer()
) {
    val (wrapper_id, wrapper_str) = element(
        wrapper_tag, wrapper_attrs,
        ArrayBuffer(render())
    )

    def createState(value: JS.Data) = {
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
        val (root_id, root_str) = render()
        val wrapper_el = JS.findElementById(JS.Str(wrapper_id))
        wrapper_el.value(JS.Str("innerHTML")) = JS.Str(root_str)

        onMount()
        children.foreach(_.mount())
    }

    def onMount() = {}

    def cleanup(): Unit = {
        children.foreach(_.cleanup())
        effects.foreach(_.cleanup())
        onCleanup()

        val wrapper_el = JS.findElementById(JS.Str(wrapper_id))
        val fn = wrapper_el.value(JS.Str("replaceChildren")).asInstanceOf[JS.Func]
        fn.value(JS.Array())
    }

    def onCleanup() = {}

    def render(): (String, String)

    def component[NewComponent <: Component](comp: NewComponent) = {
        children.append(comp)
        (comp.wrapper_id, comp.wrapper_str)
    }

    // def text(value: Either[String, VE]) = {
    //     val id = Enumerator.nextId()
    //     val text_val = value match {
    //         case Left(dt) => dt.toString()
    //         case Right(ef) => {
    //             val new_ef = createEffect(
    //                 (params) => {
    //                     val new_str = params(0).asInstanceOf[JS.Str]
    //                     val el = JS.findElementById(JS.Str(id))
    //                     el.value(JS.Str("textContent")) = new_str
    //                     new_str
    //                 },
    //                 ArrayBuffer(ef)
    //             )
    //             new_ef.value.toString()
    //         }
    //     }

    //     (id, text_val)
    // }

    def element(
        tag: String,
        attrs: Either[HashMap[String, Either[JS.Data, VE]], VE] = Left(HashMap()),
        children: ArrayBuffer[(String, String)] = ArrayBuffer()
    ) = {
        val id = Enumerator.nextId()
        val attr_map = attrs match {
            case Left(map) => {
                map.map(p => {
                    val key = JS.Str(p._1)
                    val value = p._2
                    value match {
                        case Left(dt) => {
                            (key, dt)
                        }
                        case Right(ef) => {
                            val new_ef = createEffect(
                                (params) => {
                                    val new_dt = params(0).asInstanceOf[JS.Str]
                                    val el = JS.findElementById(JS.Str(id))
                                    el.value(key) = new_dt
                                    new_dt
                                },
                                ArrayBuffer(ef)
                            )
                            (key, new_ef.value)
                        }
                    }
                })
            }
            case Right(ef) => {
                val new_ef = createEffect(
                    (params) => {
                        val prev_val = ef.value.asInstanceOf[JS.Object]
                        val new_val = params(0).asInstanceOf[JS.Object]

                        val el = JS.findElementById(JS.Str(id))
                        prev_val.value.keys.foreach(el.value.remove(_))
                        new_val.value.foreach(p => el.value(p._1) = p._2)
                        new_val
                    },
                    ArrayBuffer(ef)
                )
                new_ef.value.asInstanceOf[JS.Object].value
            }
        }
        val attrs_str = attr_map.foldLeft("")((acc, x) => acc + " " + x._1 + "=\"" + x._2 + "\"")
        val children_str = children.map(_._2).foldLeft("")((acc, x) => acc + "\n" + x)

        (
            id, 
            "<" + tag
                + " id=\"" + id + "\""
                + attrs_str
            + ">"
                + children_str
            + "\n</" + tag + ">"
        )
    }
}

class ExampleParent extends Component("div") {
    def render() = {
        element("div", Left(HashMap()), ArrayBuffer(
            component(new ExampleChild())
        ))
    }
}

class ExampleChild(
    props: HashMap[JS.Data, JS.Data] = HashMap()
) extends Component("div", Left(HashMap()), props) {
    val s = createState(JS.Number(0))
    def render() = {
        element("div", Left(HashMap(
            "style" -> Left(JS.Str("color: red"))
        )), ArrayBuffer(
            element("div", Left(HashMap(
                "textContent" -> Left(JS.Str("counter: "))
            ))),
            element("div", Left(HashMap(
                "textContent" -> Right(s)
            )))
        ))
    }
}

object Main {
    def main(args: Array[String]) = {
        val c = new ExampleParent()
        print(c.render()._2)
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