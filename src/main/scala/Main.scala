package imagepipeline

import java.awt.{Image, Color}
import java.io.File

import ij.{IJ, ImagePlus}
import ij.process.{FloatProcessor, ImageProcessor}

import scala.collection._
import scala.reflect.ClassTag

object IDGen {
  var count = 1000

  def gen_new_id(): String = {
    count += 1
    return count.toString
  }
}

class AnyNode[+Value](val name: String = "", val id: String = IDGen.gen_new_id(), var tsort_order: Int = -1) {
  val typ = "any"

  def copy(): AnyNode[Value] = {
    new AnyNode[Value](id = IDGen.gen_new_id())
  }

  protected def defaultToString(class_name: String): String = {
    "%s:%s: %s (%d)".format(id, class_name, name, tsort_order)
  }
}

abstract class CalcNode[+A] extends AnyNode[A] {
  def inputTypes(): Array[String]

  def outputTypes(): Array[String]
}

class DataNode[A] extends AnyNode[A] {
}

trait CalculatedNode[A] extends DataNode[A] {
  def asOutput: OutputNode[_]
}

trait AnyEdge


trait InputNode[A] extends DataNode[A] {
  //  override val typ = "input"
}

trait OutputNode[A] extends CalculatedNode[A] {
  override val typ = "output"

  def saveToFile(path: String, dat: A): Unit

  def asOutput = this
}

class ArrayNode[A] extends CalculatedNode[Array[A]] {
  def asOutput = new OutArrayNode[Array[A]]

  override def toString = defaultToString("ArrayNode")
}

class InputArrayNode[A] extends ArrayNode[A] with InputNode[Array[A]] {
  override def asOutput = new OutArrayNode[Array[A]]

  override def toString = defaultToString("ArrayNode")
}

class OutArrayNode[A] extends OutputNode[Array[A]] {
  def saveToFile(path: String, dat: Array[A]): Unit = {

  }

  override def toString = defaultToString("OutArrayNode")
}

class Map1Node[A, B](val func: Calc1[A, B]) extends CalcNode {
  override def inputTypes(): Array[String] = {
    Array()
  }

  override def outputTypes(): Array[String] = {
    Array()
  }

  override def toString = defaultToString("MapNode")
}

class Map2Node[A1, A2, B](val func: Calc2[A1, A2, B]) extends CalcNode {
  override def inputTypes(): Array[String] = {
    Array()
  }

  override def outputTypes(): Array[String] = {
    Array()
  }

  override def toString = defaultToString("Map2Node")
}

class InputFileList() extends InputNode[Array[String]] {
  override def toString = defaultToString("InputFileList")
}

class InputImg(val path: String, name: String = "", override val id: String = IDGen.gen_new_id()) extends InputNode[ImageProcessor] {
  type Value = ImageProcessor

  override val typ = "inputimg"

  override def toString = defaultToString("InputImg")

  override def copy: InputImg = {
    new InputImg(name)
  }
}

// NOT cloneable!
class OutputImg(name: String = "", id: String = IDGen.gen_new_id()) extends OutputNode[ImageProcessor] {
  type Value = ImageProcessor
  override val typ = "outputimg"

  override def toString = defaultToString("OutputImg")

  def saveToFile(path: String, d: ImageProcessor): Unit = {
    import ij.ImagePlus
    import ij.IJ
    IJ.save(new ImagePlus("output", d), path)
  }

  override def copy: OutputImg = {
    new OutputImg(name)
  }
}

class ImgNode(id: String = IDGen.gen_new_id(), name: String = "", imgtyp: String = "gray") extends CalculatedNode[ImageProcessor] {
  override val typ = "image"

  override def toString = {
    "%s:ImgNode: %s: %d".format(id, name, tsort_order)
  }

  override def copy(): ImgNode = {
    new ImgNode(IDGen.gen_new_id(), name, imgtyp)
  }

  def asOutput: OutputNode[ImageProcessor] = {
    new OutputImg(name, id)
  }
}

abstract class Calc1[-A, +B] extends CalcNode[A => B] {
  def run(p: A): B
}

trait Calc2[-A1, -A2, +B] extends CalcNode[(A1, A2) => B] {
  def run(p: A1, p2: A2): B
}

trait Calc3[-A1, -A2, -A3, +B] extends CalcNode[(A1, A2, A3) => B] {
  def run(p: A1, p2: A2, p3: A3): B
}

trait Calc4[-A1, -A2, -A3, -A4, +B] extends CalcNode[(A1, A2, A3, A4) => B] {
  def run(p: A1, p2: A2, p3: A3, p4: A4): B
}

abstract class ImgOp1[-A, +B](id: String = IDGen.gen_new_id(), name: String = "") extends Calc1[A, B] {
  override val typ = "imgop"

  override def toString(): String = {
    "%s:ImgOp: %s".format(id, name)
  }
}

abstract class ImgOp2[-A1, -A2, +B](id: String = IDGen.gen_new_id(), name: String = "") extends Calc2[A1, A2, B] {
  override val typ = "imgop"

  override def toString(): String = {
    "%s:ImgOp1: %s".format(id, name)
  }
}

class SimpleOp1[-A, +B](name: String, val func: A => B, types: (String, String), id: String = IDGen.gen_new_id()) extends ImgOp1[A, B] {
  //  override val typ = "simpleop"

  def run(p1: A): B = {
    func(p1.asInstanceOf[A])
  }

  def ->[BB, C](other: SimpleOp1[BB, C])(implicit ev: B <:< BB): SimpleOp1[A, C] = {
    new SimpleOp1("", (i: A) => other.func(this.func(i).asInstanceOf[BB]), (this.inputTypes()(0), other.outputTypes()(0)))
  }

  def inputTypes(): Array[String] = {
    Array(types._1)
  }

  def outputTypes(): Array[String] = {
    Array(types._2)
  }

  override def toString(): String = {
    "%s:SimpleOp: %s".format(id, name)
  }

  override def copy(): SimpleOp1[A, B] = {
    new SimpleOp1(name, func, types, IDGen.gen_new_id())
  }
}

class SimpleOp2[-A1, -A2, +B](name: String, func: (A1, A2) => B, types: (String, String, String), id: String = IDGen.gen_new_id()) extends ImgOp2[A1, A2, B] {
  def run(p1: A1, p2: A2): B = {
    // println(p1,p2)
    func(p1.asInstanceOf[A1], p2.asInstanceOf[A2])
  }

  def inputTypes(): Array[String] = {
    Array(types._1, types._2)
  }

  def outputTypes(): Array[String] = {
    Array(types._3)
  }


  override def toString(): String = {
    "%s:SimpleOp: %s".format(id, name)
  }

  override def copy(): SimpleOp2[A1, A2, B] = {
    new SimpleOp2(name, func, types, IDGen.gen_new_id())
  }
}

class InputFilePath extends InputNode[String] {
  override def toString = {
    "%s:InputFilePath: %s".format(id, name)
  }

  def asOutput = ???
}

class RoiData(name: String = "", override val id: String = IDGen.gen_new_id()) extends DataNode[(Int, Int, Int, Int)] {
  override def toString = {
    "%s:ROI: %s".format(id, name)
  }

  def asOutput = ???
}

class InputRoi(name: String = "", override val id: String = IDGen.gen_new_id()) extends RoiData with InputNode[(Int, Int, Int, Int)] {
  override val typ = "inputroi"

  override def toString = {
    "%s:InputROI: %s: %d".format(id, name, tsort_order)
  }
}

class RowData(val cols: Any*) extends CalculatedNode[Array[Double]] {
  override val typ = "rowdata"

  override def toString = {
    "RowData :[%s]".format(cols.mkString(","))
  }

  def asOutput = {
    new OutputRowData()
  }
}

class OutputRowData extends OutputNode[RowData] {
  override val typ = "outputrowdata"

  def saveToFile(path: String, d: RowData): Unit = {
    println(d.cols)

    import scalax.io.Codec
    import scalax.io.JavaConverters._

    implicit val codec = Codec.UTF8

    new java.io.File(path).asOutput.write(d.cols.mkString(","))
  }

  override def toString = {
    "%s: OutputRowData".format(id)
  }

}

class FilePath extends InputNode[String] {
  override def toString = defaultToString("FilePath")
}

trait Pipeline {
  val inputArity: Int
  val outputArity: Int
  var inputs: Array[InputNode[_]] = _
  var outputs: Array[OutputNode[_]] = _
  def inputTypes: Array[String]
  def outputTypes: Array[String]

  def verify(): Unit = {
    if (inputs == null || outputs == null)
      throw new Exception("Input and output should be set before run by interface() method.")

//    import scalax.io.Codec
//    import scalax.io.JavaConverters._
//    implicit val codec = Codec.UTF8
//    new java.io.File("test.dot").asOutput.write(graph.toDot)
  }
  def endDefault[B](graph: Graph[AnyNode[_]]): Unit = {
    val ns = graph.terminalNodes
    assert(ns.length == outputArity, ns.mkString)
    outputs = for(n <- ns) yield {
      assert(n.isInstanceOf[DataNode[B]] ||
//      assert(n.isInstanceOf[CalculatedNode[B]] ||
        n.isInstanceOf[Map1Node[_, _]] ||
        n.isInstanceOf[ArrayNode[_]], n)
      println(n)
      val o = n.asInstanceOf[CalculatedNode[B]].asOutput
      graph.replaceNode(n, o)
      o
    }
    //If only one input, set inputs automatically (so interface() does not need to be called.)
    if (graph.startingNodes.length == inputArity) {
      inputs = graph.startingNodes.map(_.asInstanceOf[InputNode[_]])
    }
  }
  def save_dot(graph: Graph[AnyNode[_]], path: String): Unit = {
    import scalax.io.Codec
    import scalax.io.JavaConverters._
    implicit val codec = Codec.UTF8
    new java.io.File("test.dot").asOutput.write(graph.toDot)
  }
}

class Pipeline11[-A, +B](val graph: Graph[AnyNode[_]]) extends Calc1[A, B] with Pipeline {
  val inputArity = 1
  val outputArity = 1
  def inputTypes: Array[String] = ???
  def outputTypes: Array[String] = ???

  def then[C](node: ImgOp1[B, C]): Pipeline11[A, C] = {
    val graph = this.graph.copy
    val ns = graph.terminalNodes
    val n: ImgOp1[A, B] = node.copy().asInstanceOf[ImgOp1[A, B]]
    val n2: AnyNode[_] = node.outputTypes()(0) match {
      case "image" => new ImgNode(id = IDGen.gen_new_id(), name = n.name + " result.")
      case "rowdata" => new RowData()
      case "[path]" => new ArrayNode[String]()
      case "path" => new FilePath
    }
    graph.addEdge(ns(0), n)
    graph.addEdge(n, n2)

    new Pipeline11[A, C](graph)
  }

  def then2[A2, C](node: ImgOp2[B, A2, C], param: AnyNode[A2]): Pipeline21[A, A2, C] = {
    val graph = this.graph.copy
    val ns = graph.terminalNodes
    val n: ImgOp2[B, A2, C] = node.copy().asInstanceOf[ImgOp2[B, A2, C]]
    val n2 = new ImgNode(id = IDGen.gen_new_id(), name = n.name + " result.")
    graph.addNode(n)
    graph.addNode(n2)
    graph.addEdge(ns(0), n, 0)
    graph.addEdge(n, n2)
    graph.addEdge(param, n, 1)
    //    println(graph.toDot)
    //println(graph.nodes)

    new Pipeline21[A, A2, C](graph)
  }

  def map[C, D](func: SimpleOp1[C, D])(implicit ev: B <:< Array[C]): Pipeline11[A, Array[D]] = {
    val graph = this.graph.copy
    val n = getSingleOutput
    val m = new Map1Node(func)
    graph.addEdge(n, m)
    graph.addEdge(m, new ArrayNode[B])
    new Pipeline11(graph)
  }

  def map[C, D](func: Pipeline11[B, C])(implicit ev: C =:= Array[D]): Pipeline11[A, Array[C]] = {
    val graph = this.graph.copy
    val n = getSingleOutput
    val m = new Map1Node(func)
    graph.addEdge(n, m)
    graph.addEdge(m, new ArrayNode[B])
    new Pipeline11(graph)
  }

  def map2[C, D, E](func: Pipeline21[C, D, E], param: DataNode[D])(implicit ev: B <:< Array[C]): Pipeline21[A, D, Array[E]] = {
    val graph = this.graph
    val n = getSingleOutput
    val m = new Map2Node(func)
    graph.addEdge(n, m)
    graph.addEdge(param.asInstanceOf[AnyNode[_]], m)
    graph.addEdge(m, new ArrayNode[B])
    new Pipeline21(graph)
  }

  def getSingleOutput: AnyNode[B] = {
    val ns = graph.terminalNodes
    assert(ns.length == 1, "There are multiple outputs.")
    ns(0).asInstanceOf[AnyNode[B]]
  }

  def end(): Pipeline11[A, B] = {
    val ns = graph.terminalNodes
    assert(ns.length == 1, ns.mkString)
    assert(ns(0).isInstanceOf[CalculatedNode[B]] ||
      ns(0).isInstanceOf[Map1Node[_, _]] ||
      ns(0).isInstanceOf[ArrayNode[_]], ns(0))
    val o = ns(0).asInstanceOf[CalculatedNode[B]].asOutput
    graph.replaceNode(ns(0), o)
    outputs = Array(o)

    //If only one input, set inputs automatically (so interface() does not need to be called.)
    if (graph.startingNodes.length == 1) {
      inputs = graph.startingNodes.map(_.asInstanceOf[InputNode[_]])
    }
    this.asInstanceOf[Pipeline11[A, B]]
  }

  def runA[C](param: A)(implicit ev: B <:< Array[C]): Seq[C] = {
    val values = new mutable.HashMap[String, Any]
    doRun(param, values)
    val v = values(this.outputs(0).id)
    println(v)
    v.asInstanceOf[Array[Object]].toSeq.map(_.asInstanceOf[C])
  }

  def doRun(param: A, values: mutable.HashMap[String, Any]): Unit = {
    this.verify()
    val sorted = graph.tsort
    values(inputs(0).id) = param
    println(sorted.mkString(","))
    for (node <- sorted if values.get(node.id).isEmpty) {
      node match {
        case _: DataNode[_] => {
          val node_inputs = graph.predecessors(node)
          assert(node_inputs.length == 1)
          values(node.id) = Pipeline.calc_prev(graph, values, node_inputs(0).asInstanceOf[CalcNode[_]])
        }
        case _ =>
      }
    }
  }

  def run(param: A): B = {
    val values = new mutable.HashMap[String, Any]
    doRun(param, values)
    val v = values(this.outputs(0).id)
    v.asInstanceOf[B]
  }
}

class Pipeline21[-A1, -A2, +B](val graph: Graph[AnyNode[_]]) extends Calc2[A1, A2, B] with Pipeline {
  val inputArity = 2
  val outputArity = 1
  def run(p1: A1, p2: A2): B = {
    val values = new mutable.HashMap[String, Any]
    doRun(values, p1, p2)
    println(values)
    values(this.outputs(0).id).asInstanceOf[B]
  }

  def runA[C](p1: A1, p2: A2)(implicit ev: B <:< Array[C]): Seq[C] = {
    val values = new mutable.HashMap[String, Any]
    doRun(values, p1, p2)
    val v = values(this.outputs(0).id)
    println(v)
    v.asInstanceOf[Array[Object]].toSeq.map(_.asInstanceOf[C])
  }

  def doRun(values: mutable.HashMap[String, Any], p1: A1, p2: A2): Unit = {
    val sorted = graph.tsort
    assert(inputs.length == inputArity && outputs.length == outputArity)
    values(inputs(0).id) = p1
    values(inputs(1).id) = p2
    println(sorted.mkString(","))
    for (node <- sorted if values.get(node.id).isEmpty) {
      node match {
        case _: DataNode[_] => {
          val node_inputs = graph.predecessors(node)
          assert(node_inputs.length == 1)
          values(node.id) = Pipeline.calc_prev(graph, values, node_inputs(0).asInstanceOf[CalcNode[_]])
        }
        case _ =>
      }
    }
  }

  def end(): Pipeline21[A1, A2, B] = {
    val ns = graph.terminalNodes
    assert(ns.length == 1, ns.mkString)
    assert(ns(0).isInstanceOf[CalculatedNode[B]] ||
      ns(0).isInstanceOf[Map1Node[_, _]] ||
      ns(0).isInstanceOf[ArrayNode[_]], ns(0))
    val o = ns(0).asInstanceOf[CalculatedNode[B]].asOutput
    graph.replaceNode(ns(0), o)
    outputs = Array(o)

    //If only one input, set inputs automatically (so interface() does not need to be called.)
    if (graph.startingNodes.length == 1) {
      inputs = graph.startingNodes.map(_.asInstanceOf[InputNode[_]])
    }
    this
  }

  def inputOrder(ns: DataNode[_]*): Pipeline21[A1, A2, B] = {
    val ins = graph.startingNodes.map(n => n.id -> n.asInstanceOf[InputNode[_]]).toMap[String,InputNode[_]]
    inputs = ns.toArray.map(n => ins(n.id))
    this
  }

  def then[C](node: Calc1[B, C]): Pipeline21[A1, A2, C] = {
    val graph = this.graph
    val n = graph.terminalNodes(0)
    graph.addEdge(n, node)
    val out = new ImgNode() //FIXME: This can be other types.
    graph.addEdge(node, out)
    new Pipeline21(graph)
  }

  def inputTypes(): Array[String] = ???
  def outputTypes(): Array[String] = ???
}

class Pipeline31[-A1, -A2, -A3, +B](val graph: Graph[AnyNode[_]]) extends Calc3[A1, A2, A3, B] with Pipeline {
  val inputArity = 3
  val outputArity = 1

  def run(p1: A1, p2: A2, p3: A3): B = {
    ???
  }
  def end() = {
    val ns = graph.terminalNodes
    assert(ns.length == outputArity, ns.mkString)
    outputs = for(n <- ns) yield {
      assert(n.isInstanceOf[CalculatedNode[B]] ||
        n.isInstanceOf[Map1Node[_, _]] ||
        n.isInstanceOf[ArrayNode[_]], n)
      val o = n.asInstanceOf[CalculatedNode[B]].asOutput
      graph.replaceNode(n, o)
      o
    }
    //If only one input, set inputs automatically (so interface() does not need to be called.)
    if (graph.startingNodes.length == inputArity) {
      inputs = graph.startingNodes.map(_.asInstanceOf[InputNode[_]])
    }
    this
  }

  def inputTypes: Array[String] = ???
  def outputTypes: Array[String] = ???
}

class Pipeline41[-A1, -A2, -A3, -A4, +B](val graph: Graph[AnyNode[_]]) extends Calc4[A1, A2, A3, A4, B] with Pipeline {
  val inputArity = 4
  val outputArity = 1
  def run(p1: A1, p2: A2, p3: A3, p4: A4): B = {
    val values = new mutable.HashMap[String, Any]
    doRun(values, p1, p2, p3, p4)
    values(this.outputs(0).id).asInstanceOf[B]
  }

  def runA[C](p1: A1, p2: A2, p3: A3, p4: A4)(implicit ev: B <:< Array[C]): Seq[C] = {
    val values = new mutable.HashMap[String, Any]
    doRun(values, p1, p2, p3, p4)
    val v = values(this.outputs(0).id)
    println(v)
    v.asInstanceOf[Array[Object]].toSeq.map(_.asInstanceOf[C])
  }

  def doRun(values: mutable.HashMap[String, Any], p1: A1, p2: A2, p3: A3, p4: A4): Unit = {
    val sorted = graph.tsort
    assert(inputs.length == inputArity && outputs.length == outputArity)
    values(inputs(0).id) = p1
    values(inputs(1).id) = p2
    values(inputs(2).id) = p3
    values(inputs(3).id) = p4
    for (node <- sorted if values.get(node.id).isEmpty) {
      node match {
        case _: DataNode[_] => {
          val node_inputs = graph.predecessors(node)
          assert(node_inputs.length == 1)
          values(node.id) = Pipeline.calc_prev(graph, values, node_inputs(0).asInstanceOf[CalcNode[_]])
        }
        case _ =>
      }
    }
  }

  def inputOrder(ns: InputNode[_]*): Pipeline41[A1, A2, A3, A4, B] = {
    val ins: Map[String,InputNode[_]] = graph.startingNodes.map(n => n.id -> n.asInstanceOf[InputNode[_]]).toMap
    inputs = ns.toArray.map(n => ins(n.id))
    this
  }

  def inputTypes: Array[String] = ???

  def outputTypes: Array[String] = ???
  def end() = {
    endDefault(graph)
    this
  }
}

object Pipeline {

  def start[A](node: InputNode[A]): Pipeline11[A, A] = {
    val p = new Pipeline11[A, A](new Graph[AnyNode[_]])
    p.graph.addNode(node)
    //println(p.graph.nodes)
    p
  }

  def start[A1, A2, B](func: SimpleOp2[A1, A2, B], n1: InputNode[A1], n2: InputNode[A2]): Pipeline21[A1, A2, B] = {
    val newg = new Graph[AnyNode[_]]
    newg.addNode(n1)
    newg.addNode(n2)
    val f = func.copy()
    val out = func match {
      case n: SimpleOp2[_, _, ImageProcessor] => new ImgNode
    }
    newg.addEdge(n1, f, 0)
    newg.addEdge(n2, f, 1)
    newg.addEdge(f, out, 2)
    new Pipeline21(newg)
  }

  def cont2[A1, A2, B1, B2, C](func: SimpleOp2[B1, B2, C], node: Pipeline11[A1, B1], node2: Pipeline11[A2, B2]): Pipeline21[A1, A2, C] = {
    val newg = new Graph[AnyNode[_]]
    assert(node.graph.terminalNodes.length == 1)
    assert(node2.graph.terminalNodes.length == 1)
    newg.copyFrom(node.graph)
    newg.copyFrom(node2.graph)
    val ns = newg.terminalNodes
    val n2 = new ImgNode() // FIXME: this is not always ImgNode
    assert(ns.length == 2)
    newg.addEdge(ns(0), func)
    newg.addEdge(ns(1), func)
    newg.addEdge(func, n2)
    new Pipeline21(newg)
  }

  def cont2[A1, A2, B1, B2, C1, C2, D, E](func: SimpleOp2[C1, C2, D], node: Pipeline21[A1, B1, C1], node2: Pipeline21[A2, B2, C2]): Pipeline41[A1, B1, A2, B2, D] = {
    val newg = new Graph[AnyNode[_]]
    assert(node.graph.terminalNodes.length == 1)
    assert(node2.graph.terminalNodes.length == 1)
    newg.copyFrom(node.graph)
    newg.copyFrom(node2.graph)
    val ns = newg.terminalNodes
    val n2 = new ImgNode // FIXME: this is not always ImgNode
    assert(ns.length == 2)
    newg.addEdge(ns(0), func)
    newg.addEdge(ns(1), func)
    newg.addEdge(func, n2)
    new Pipeline41(newg)
  }

  def calc_prev[A1, A2, C, D: ClassTag](graph: Graph[AnyNode[_]], values: mutable.HashMap[String, Any], node: CalcNode[_]): C = {
    val ins = graph.predecessors(node)
    node match {
      case n: Calc1[A1, C] => {
        n.run(values(ins(0).id).asInstanceOf[A1])
      }
      case n: Calc2[A1, A2, C] => {
        n.run(values(ins(0).id).asInstanceOf[A1], values(ins(1).id).asInstanceOf[A2])
      }
      case c: Map1Node[A1, D] => {
        val ins = graph.predecessors(c)
        assert(ins.length == 1)
        val vs = values(ins(0).id)
        vs.asInstanceOf[Array[A1]].map(v => c.func.run(v)).asInstanceOf[C]
      }
      case c: Map2Node[A1, A2, D] => {
        val ins = graph.predecessors(c)
        assert(ins.length == 2)
        val vs = values(ins(0).id)
        val p2 = values(ins(1).id).asInstanceOf[A2]
        vs.asInstanceOf[Array[A1]].map(v => c.func.run(v, p2)).asInstanceOf[C]
      }
    }
  }
}

// DiGraph, no parallel edges.
class Graph[A <: AnyNode[_] : ClassTag] {
  var edges: mutable.ArrayBuffer[(A, A, Int)] = mutable.ArrayBuffer[(A, A, Int)]()
  var nodes: mutable.ArrayBuffer[A] = mutable.ArrayBuffer[A]()

  def copy: Graph[A] = {
    val r = new Graph[A]
    r.edges = edges.clone()
    r.nodes = nodes.clone()
    r
  }

  def addNode(n: A): Unit = {
    nodes += n
  }

  def addEdge(from: A, to: A, value: Int = 0): Unit = {
    edges += ((from, to, value))
    nodes ++= Seq(from, to)
    edges = edges.distinct
    nodes = nodes.distinct
  }

  // Sorted by edge value.
  def predecessors(n: A): Array[A] = {
    edges.filter(_._2 == n).sortBy(_._3).map(_._1).toArray
  }

  def successors(n: A): Array[A] = {
    edges.filter(_._1 == n).sortBy(_._3).map(_._2).toArray
  }

  def replaceNode(fr: A, to: A): Unit = {
    edges = edges.map(e => {
      if (e._1 == fr)
        (to, e._2, e._3)
      else if (e._2 == fr) {
        (e._1, to, e._3)
      } else {
        e
      }
    })
    nodes = nodes.map(n => if (n == fr) to else n)
  }

  def toDot: String = {
    def getNodeProp(n: AnyNode[_]): String = {
      n.typ match {
        case "imgop" => "[shape=box]"
        case "input" => "[shape=invhouse]"
        case "output" => "[shape=house]"
        case _ => ""
      }
    }
    // stub
    "digraph mygraph {\n" +
      nodes.map(n => "\"%s\"%s".format(n, getNodeProp(n))).mkString("\n") +
      edges.map(e => "\"%s\" -> \"%s\"".format(e._1, e._2)).mkString("\n") +
      "\n}\n"
  }

  def terminalNodes: Array[A] = {
    val froms = edges.toSeq.map(_._1)
    if (nodes.length == 1) {
      Array(nodes(0))
    } else {
      val res = new mutable.ArrayBuffer[A]
      for (n <- nodes) {
        if (!froms.contains(n)) {
          res += n
        }
      }
      res.toArray
    }
  }

  def startingNodes: Array[A] = {
    val tos = edges.toSeq.map(_._2)
    if (nodes.length == 1) {
      Array(nodes(0))
    } else {
      val res = new mutable.ArrayBuffer[A]
      for (n <- nodes) {
        if (!tos.contains(n)) {
          res += n
        }
      }
      res.toArray
    }
  }

  def copyFrom(other: Graph[A]): Unit = {
    val newedges = for (e <- other.edges) yield {
      e.copy().asInstanceOf[(A, A, Int)]
    }
    this.nodes ++= newedges.map(t => Array(t._1, t._2)).flatten.distinct
    edges ++= newedges
  }

  // https://gist.github.com/ThiporKong/4399695
  def tsort: Iterable[A] = {
    val edges = this.edges
    def tsort(toPreds: Map[A, Set[A]], done: Iterable[A]): Iterable[A] = {
      val (noPreds, hasPreds) = toPreds.partition {
        _._2.isEmpty
      }
      if (noPreds.isEmpty) {
        if (hasPreds.isEmpty) done else sys.error(hasPreds.toString)
      } else {
        val found = noPreds.map {
          _._1
        }
        tsort(hasPreds.mapValues {
          _ -- found
        }, done ++ found)
      }
    }

    val toPred = edges.foldLeft(Map[A, Set[A]]()) { (acc, e) =>
      acc + (e._1 -> acc.getOrElse(e._1, Set())) + (e._2 -> (acc.getOrElse(e._2, Set()) + e._1))
    }
    tsort(toPred, Seq())
  }
}

object Main {

  def main(args: Array[String]): Unit = {

    for (i <- 0 until 100) {
      val res = Defs.getstats_roi.run("/Users/hiroyuki/repos/ImagePipeline/BF.jpg", (0, 0, 300, 300))
      println(res)
    }

    println("Done.")

  }
}





