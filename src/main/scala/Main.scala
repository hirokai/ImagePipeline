package imagepipeline

import java.io.File

import ij.{IJ, ImagePlus}
import ij.process.ImageProcessor

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

abstract class CalcNode extends AnyNode[AnyRef] {
  def inputTypes(): Array[String]

  def outputTypes(): Array[String]
}

trait DataNode {
}

trait CalculatedNode extends DataNode {
  def asOutput: OutputNode[_]
}

trait AnyEdge


trait InputNode[A] extends AnyNode[A] with DataNode {
  //  override val typ = "input"
}

trait OutputNode[A] extends AnyNode[A] with CalculatedNode {
  override val typ = "output"

  def saveToFile(path: String, dat: A): Unit

  def asOutput = this
}

class ArrayNode[A] extends AnyNode[Array[A]] with CalculatedNode {
  def asOutput = new OutArrayNode[Array[A]]

  override def toString = defaultToString("ArrayNode")
}

class InputArrayNode[A] extends AnyNode[Array[A]] with InputNode[Array[A]] {
  def asOutput = new OutArrayNode[Array[A]]

  override def toString = defaultToString("ArrayNode")
}

class OutArrayNode[A] extends AnyNode[Array[A]] with OutputNode[Array[A]] {
  def saveToFile(path: String, dat: Array[A]): Unit = {

  }

  override def toString = defaultToString("OutArrayNode")
}

class Map1Node[A, B](val func: SimpleOp1[A, B]) extends CalcNode {
  override def inputTypes(): Array[String] = {
    Array()
  }

  override def outputTypes(): Array[String] = {
    Array()
  }

  override def toString = defaultToString("MapNode")
}

class Map1NodeP(val func: Pipeline[Nothing]) extends CalcNode {
  override def inputTypes(): Array[String] = {
    Array()
  }

  override def outputTypes(): Array[String] = {
    Array()
  }

  override def toString = defaultToString("MapNodeP")
}

class Map2NodeP(val func: Pipeline[Nothing]) extends CalcNode {
  override def inputTypes(): Array[String] = {
    Array()
  }

  override def outputTypes(): Array[String] = {
    Array()
  }

  override def toString = defaultToString("MapNodeP")
}

class InputFileList() extends AnyNode with InputNode[Array[String]] {
  override def toString = defaultToString("InputFileList")
}

class InputImg(val path: String, name: String = "", override val id: String = IDGen.gen_new_id()) extends AnyNode with InputNode[ImageProcessor] {
  type Value = ImageProcessor

  override val typ = "inputimg"

  override def toString = defaultToString("InputImg")

  override def copy: InputImg = {
    new InputImg(name)
  }
}

// NOT cloneable!
class OutputImg(name: String = "", id: String = IDGen.gen_new_id()) extends AnyNode with OutputNode[ImageProcessor] {
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

class ImgNode(id: String = IDGen.gen_new_id(), name: String = "", imgtyp: String = "gray") extends AnyNode[ImageProcessor] with CalculatedNode {
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

trait Calc1 {
  def run(p: Any): Any
}

trait Calc2 {
  def run(p1: Any, p2: Any): Any
}

abstract class ImgOp1[+A, -B](id: String = IDGen.gen_new_id(), name: String = "") extends CalcNode with Calc1 {
  override val typ = "imgop"

  def run(param: Any): Any

  override def toString(): String = {
    "%s:ImgOp: %s".format(id, name)
  }
}

abstract class ImgOp2[+A1, +A2, -B](id: String = IDGen.gen_new_id(), name: String = "") extends CalcNode with Calc2 {
  override val typ = "imgop"

  def run(p1: Any, p2: Any): Any

  override def toString(): String = {
    "%s:ImgOp1: %s".format(id, name)
  }
}

class SimpleOp1[+A, -B](name: String, func: A => B, types: (String, String), id: String = IDGen.gen_new_id()) extends ImgOp1[A, B] with Calc1 {
  //  override val typ = "simpleop"
  def run(p1: Any): Any = {
    func(p1.asInstanceOf[A])
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

class SimpleOp2[+A1, +A2, -B](name: String, func: (A1, A2) => B, types: (String, String, String), id: String = IDGen.gen_new_id()) extends ImgOp2[A1, A2, B] with Calc2 {
  def run(p1: Any, p2: Any): Any = {
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

class InputFilePath extends AnyNode[String] with InputNode[String] {
  override def toString = {
    "%s:InputFilePath: %s".format(id, name)
  }

  def asOutput = ???
}

class Roi(name: String = "", override val id: String = IDGen.gen_new_id()) extends AnyNode[(Int, Int, Int, Int)] with DataNode {
  override def toString = {
    "%s:ROI: %s".format(id, name)
  }

  def asOutput = ???
}

class InputRoi(name: String = "", override val id: String = IDGen.gen_new_id()) extends Roi with InputNode[(Int, Int, Int, Int)] {
  override val typ = "inputroi"

  override def toString = {
    "%s:InputROI: %s: %d".format(id, name, tsort_order)
  }
}

class RowData(val cols: Any*) extends AnyNode[Array[Double]] with CalculatedNode {
  override val typ = "rowdata"

  override def toString = {
    "RowData :[%s]".format(cols.mkString(","))
  }

  def asOutput = {
    new OutputRowData()
  }
}

class OutputRowData extends AnyNode with OutputNode[RowData] {
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

class Pipeline[+A](val graph: Defs.G) {
  def then[A, B](node: ImgOp1[A, B]): Pipeline[B] = {
    val ns = graph.terminalNodes
    //    println(ns)
    //    assert(ns.length == 1)
    //    println(ns(0))
    val n: ImgOp1[A, B] = node.copy().asInstanceOf[ImgOp1[A, B]]
    val n2: AnyNode[_] = node.outputTypes()(0) match {
      case "image" => new ImgNode(id = IDGen.gen_new_id(), name = n.name + " result.")
      case "rowdata" => new RowData()
      case "[path]" => new ArrayNode[String]()
      case "path" => new FilePath
    }
    //    graph.addNode(n)
    //    graph.addNode(n2)
    graph.addEdge(ns(0), n)
    graph.addEdge(n, n2)
    //    println(graph.toDot)
    //    println(graph.nodes)

    this.asInstanceOf[Pipeline[B]]
  }

  def then2[A1, A2, B](node: ImgOp2[A1, A2, B], param: AnyNode[A2]): Pipeline[B] = {
    val ns = graph.terminalNodes
    val n: ImgOp2[A1, A2, B] = node.copy().asInstanceOf[ImgOp2[A1, A2, B]]
    val n2 = new ImgNode(id = IDGen.gen_new_id(), name = n.name + " result.")
    graph.addNode(n)
    graph.addNode(n2)
    graph.addEdge(ns(0), n, 0)
    graph.addEdge(n, n2)
    graph.addEdge(param, n, 1)
    //    println(graph.toDot)
    //println(graph.nodes)

    this.asInstanceOf[Pipeline[B]]
  }

  def map[A, B](func: SimpleOp1[A, B]): Pipeline[ArrayNode[B]] = {
    val n = getSingleOutput
    val m = new Map1Node(func)
    graph.addEdge(n, m)
    graph.addEdge(m, new ArrayNode[B])
    this.asInstanceOf[Pipeline[ArrayNode[B]]]
  }

  sealed abstract class IsSameType[X, Y]
  object IsSameType {
    implicit def tpEquals[A] = new IsSameType[A, A]{}
  }

  def mapmap[A1, B](func: SimpleOp1[A1, B])(implicit ev: A <:< ArrayNode[A1]): Pipeline[ArrayNode[B]] = {
    val n = getSingleOutput
    val m = new Map1Node(func)
    graph.addEdge(n, m)
    graph.addEdge(m, new ArrayNode[B])
    this.asInstanceOf[Pipeline[ArrayNode[B]]]
  }

  def map[A1, B](func: Pipeline[Nothing])(implicit ev: A <:< ArrayNode[A1]): Pipeline[ArrayNode[B]] = {
    val n = getSingleOutput
    val m = new Map1NodeP(func)
    graph.addEdge(n, m)
    graph.addEdge(m, new ArrayNode[B])
    this.asInstanceOf[Pipeline[ArrayNode[B]]]
  }

  def map2[A, B](func: Pipeline[Nothing], param: DataNode): Pipeline[ArrayNode[B]] = {
    val n = getSingleOutput
    val m = new Map2NodeP(func)
    graph.addEdge(n, m)
    graph.addEdge(param.asInstanceOf[AnyNode[_]], m)
    graph.addEdge(m, new ArrayNode[B])
    this.asInstanceOf[Pipeline[ArrayNode[B]]]
  }

  def getSingleOutput: AnyNode[A] = {
    val ns = graph.terminalNodes
    assert(ns.length == 1, "There are multiple outputs.")
    ns(0).asInstanceOf[AnyNode[A]]
  }

  def end(): Pipeline[Nothing] = {
    val ns = graph.terminalNodes
    assert(ns.length == 1, ns.mkString)
    assert(ns(0).isInstanceOf[CalculatedNode] ||
      ns(0).isInstanceOf[Map1Node[_, _]] ||
      ns(0).isInstanceOf[ArrayNode[_]], ns(0))
    val o = ns(0).asInstanceOf[CalculatedNode].asOutput
    graph.replaceNode(ns(0), o)
    outputs = Array(o)
    this.asInstanceOf[Pipeline[Nothing]]
  }

  def interface(ins: InputNode[_]*): Pipeline[A] = {
    assert(outputs != null)
    assert(ins.length == graph.startingNodes.length)

    //    // sort tsort_order for input nodes, so that input will match with args.
    //    val ins2 = ins.map(_.asInstanceOf[AnyNode])
    //    val ns = ins2.map(_.tsort_order).toSeq.sorted
    //    ns.zip(ins2).map(a => a._2.tsort_order = a._1)
    inputs = ins.toArray
    //    interfaceSet = true

    this
  }

  var inputs: Array[AnyNode[_]] = _
  var outputs: Array[AnyNode[_]] = _

  def interface[In <: Product, Out <: Product](ins: In, outs: Out = null): Pipeline[Nothing] = {
    assert(ins.productArity == graph.startingNodes.length)
    assert(outs == null || outs.productArity == graph.terminalNodes.length)

    inputs = ins.productIterator.toArray.map(_.asInstanceOf[AnyNode[_]])

    if (outs != null) {
      outputs = outs.productIterator.toArray.map(_.asInstanceOf[AnyNode[_]])
    } else if (outputs == null) {
      throw new Exception("No output set.")
    }
    this.asInstanceOf[Pipeline[Nothing]]
  }

  //  var interfaceSet: Boolean = false
  //  var outputSet: Boolean = false

  def verify(ins: Array[String], outs: Array[String]): Unit = {
    verify()
  }

  def verify(): Unit = {
    if (inputs == null || outputs == null)
      throw new Exception("Input and output should be set before run by interface() method.")
    val sorted: Iterable[AnyNode[_]] = graph.tsort
    var count = 0
    for (n <- sorted) {
      n.tsort_order = count
      count += 1
    }

    import scalax.io.Codec
    import scalax.io.JavaConverters._
    implicit val codec = Codec.UTF8
    new java.io.File("test.dot").asOutput.write(graph.toDot)
  }

  def run(param: Any): Array[Any] = {
    this match {
      case c: Pipeline[Nothing] => {
        Pipeline.run(c, param)
      }
      case _ => throw new Exception("Not complete pipeline.")
    }
  }

  def run[A <: Product](params: A): Array[Any] = {
    this match {
      case c: Pipeline[Nothing] =>
        Pipeline.run(c, params)
      case _ => throw new Exception("Not complete pipeline.")
    }
  }
}

object Pipeline {

  import Defs.G

  def start[A](node: InputNode[A]): Pipeline[InputNode[A]] = {
    val p = new Pipeline[InputNode[A]](new G)
    p.graph.addNode(node)
    //println(p.graph.nodes)
    p
  }

  def start[A1,A2,B](func: SimpleOp2[A1, A2, B], n1: InputNode[A1], n2: InputNode[A2]): Pipeline[(A1,A2)] = {
    val newg = new Graph[AnyNode[_]]
    newg.addNode(n1)
    newg.addNode(n2)
    val f = func.copy()
    val out = func match {
      case _:SimpleOp2[_,_,ImageProcessor] => new ImgNode
    }
    newg.addEdge(n1,f,0)
    newg.addEdge(n2,f,1)
    newg.addEdge(f,out,2)
    new Pipeline(newg)
  }

  def cont2[A1, A2, B](func: SimpleOp2[A1, A2, B], node: Pipeline[A1], node2: Pipeline[A2]): Pipeline[B] = {
    val newg = new Defs.G
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
    new Pipeline(newg)

  }

  import Defs._

  def run[A1, A2, In <: Product](calc: CompleteCalc, arg: Any): Array[Any] = {
    run(calc, Tuple1(arg))
  }


  def run[A1, A2, In <: Product](calc: CompleteCalc, args: In): Array[Any] = {
    calc.verify()

    val sorted = calc.graph.tsort
    val values = new mutable.HashMap[String, Any] //(calc.graph.nodes.length)

    val ins: Array[AnyNode[_]] = calc.inputs
    println(ins.mkString)
    if (ins.length != args.productArity) {
      println(ins.mkString)
      println(args.productIterator.toArray.mkString(","))
      throw new IllegalArgumentException("Arity does not match.")
    }
    for ((p, i) <- args.productIterator.toArray.zip(ins)) {
      println(p, i)
      values(i.asInstanceOf[AnyNode[_]].id) = p
    }

    for (node <- sorted if values.get(node.id).isEmpty) {
      println("Processing: %s".format(node))
      //      val in_types = node.inputTypes()
      //      assert(in_types.length == ins.length)

      node match {
        //        case n: InputNode[_] => {
        //          val res = n match {
        //            case n: InputImg => {
        //              IJ.openImage(n.path).getProcessor
        //            }
        //            case n: InputRoi => {
        //              n.data
        //            }
        //          }
        //          values(n.id) = res
        //        }
        case n: DataNode => {
          println("DataNode calculating:" + n.toString)
          val ins = calc.graph.predecessors(node)
          println(ins.mkString(","))
          assert(ins.length == 1, ins.mkString(","))
          assert(ins(0).isInstanceOf[CalcNode])
          val c = ins(0).asInstanceOf[CalcNode]
          val params = calc.graph.predecessors(c)
          println(params.mkString(","), c.name)
          val res = c match {
            case c: Calc1 => {
              assert(params.length == 1)
              println("%s.run(%s)".format(c, params(0)))
              c.run(values(params(0).id))
            }
            case c: Calc2 => {
              assert(params.length == 2)
              println(params(0).id, params(1).id)
              println(values(params(0).id), values(params(1).id))
              c.run(values(params(0).id), values(params(1).id))
            }
            case c: Map1Node[_, _] => {
              val ins = calc.graph.predecessors(c)
              assert(ins.length == 1)
              println(ins(0))
              val vs = values(ins(0).id)
              vs.asInstanceOf[Array[_]].map(c.func.run)
            }
            case c: Map1NodeP => {
              val ins = calc.graph.predecessors(c)
              assert(ins.length == 1)
              println(ins(0))
              val vs = values(ins(0).id)
              println(vs)
              println("calc inputs:"+c.func.inputs.mkString(","))
              vs.asInstanceOf[Array[_]].map(c.func.run)
            }
            case c: Map2NodeP => {
              val ins = calc.graph.predecessors(c)
              assert(ins.length == 2)
              println(ins(0))
              val vs = values(ins(0).id)
              println(vs)
              println("calc inputs:"+c.func.inputs.mkString(","))
              vs.asInstanceOf[Array[_]].map(v => c.func.run(v,values(ins(1).id)))
            }
          }
          values(n.id) = res
        }
        case n: CalcNode =>
      }
    }

    val outs: Array[AnyNode[_]] = sorted.filter(_.isInstanceOf[OutputNode[_]]).toSeq.sortBy(_.tsort_order).toArray
    println("Outputs: " + outs.mkString(","))
    //    outs.zip(outpaths).map(o => o._1.asInstanceOf[OutputNode[Any]].saveToFile(o._2,values(o._1.id)))
    outs.map(o => values(o.asInstanceOf[OutputNode[Any]].id))
  }

  protected def input(calc: Pipeline[_], node: AnyNode[_]): Array[AnyNode[_]] = {
    calc.graph.predecessors(node)
  }

}

// DiGraph, no parallel edges.
class Graph[A <: AnyNode[_] : ClassTag] {
  var edges: mutable.ArrayBuffer[(A, A, Int)] = mutable.ArrayBuffer[(A, A, Int)]()
  var nodes: mutable.ArrayBuffer[A] = mutable.ArrayBuffer[A]()

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

  import Defs.G

  def copyFrom(other: G): Unit = {
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

  import Defs._
  import scalax.io._


  def do_stat(): Unit = {
    getstats.verify()
    val res = Pipeline.run(getstats, Tuple1(1)).asInstanceOf[RowData]
    println(res)
  }

  def do_combine(): Unit = {
    import ij.plugin.StackCombiner
    import ij.ImageStack
    import ij.IJ

    def f(img: ImageProcessor): ImageStack = {
      var i2 = img.duplicate
      i2.setRoi(0, 0, 100, 100)
      i2 = i2.crop()
      val s = new ImageStack(100, 100)
      s.addSlice(i2)
      s
    }
    val img1 = IJ.openImage("/Users/hiroyuki/repos/ImagePipeline/BF.jpg").getProcessor
    val img2 = IJ.openImage("/Users/hiroyuki/repos/ImagePipeline/Cy5.jpg").getProcessor
    val s1 = f(img1)
    val s2 = f(img2)
    val s = new StackCombiner().combineHorizontally(s1, s2)
    val r = s.getProcessor(1)
    new ImagePlus("result", r).show()
  }

  def main(args: Array[String]): Unit = {
    getstats_roi.verify()

    for (i <- 0 until 100) {
      val res = Pipeline.run(getstats_roi, ("/Users/hiroyuki/repos/ImagePipeline/BF.jpg", (0, 0, 300, 300)))
      println(res.mkString(","))
    }

    println("Done.")

  }
}





