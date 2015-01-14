import java.io.File

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

class AnyNode(val id: String = "", var tsort_order: Int = -1) {
  val typ = "any"

  def copy(): AnyNode = {
    new AnyNode(id = IDGen.gen_new_id())
  }
}

trait CalcNode {
  def inputTypes(): Array[String]

  def outputTypes(): Array[String]
}

sealed abstract class TypeTag

class NImage extends TypeTag

sealed abstract class NData extends TypeTag

class NInt extends NData

class NFloat extends NData

class NRoi extends NData

abstract class AnyData extends AnyNode

trait AnyEdge


trait InputNode[A] extends AnyNode {
  //  override val typ = "input"
}

trait OutputNode[A] extends AnyNode {
  override val typ = "output"

  def saveToFile(dat: A): Unit
}

class TupleNode(types: (String, String), id: String = IDGen.gen_new_id(), name: String = "") extends AnyNode {
  override val typ = "tuple"

  override def toString = {
    "%s:TupleNode: %s".format(id, name)
  }

  def run(p1: Any, p2: Any): Any = {
    return (p1, p2)
  }

  def inputTypes(): Array[String] = {
    Array(types._1, types._2)
  }

  def outputTypes(): Array[String] = {
    Array("(%s,%s)".format(types._1, types._2))
  }
}

class InputImg(val path: String, name: String = "", override val id: String = IDGen.gen_new_id()) extends AnyNode with InputNode[NImage] {
  override val typ = "inputimg"

  override def toString = {
    "InputImg(%s): %d".format(name, tsort_order)
  }

  override def copy: InputImg = {
    new InputImg(name)
  }
}

// NOT cloneable!
class OutputImg(path: String, name: String = "", id: String = IDGen.gen_new_id()) extends AnyNode with OutputNode[ImageProcessor] {
  override val typ = "outputimg"

  override def toString = {
    "OutputImg(%s)".format(name)
  }

  def saveToFile(d: ImageProcessor): Unit = {
    import ij.ImagePlus
    import ij.IJ
    IJ.save(new ImagePlus("output", d), path)
  }

  override def copy: OutputImg = {
    new OutputImg(name)
  }
}

//NOT cloneable!

class ImgNode(id: String = IDGen.gen_new_id(), var name: String = "", imgtyp: String = "gray") extends AnyNode {
  override val typ = "image"

  override def toString = {
    "%s:ImgNode: %s: %d".format(id, name, tsort_order)
  }

  override def copy(): ImgNode = {
    new ImgNode(IDGen.gen_new_id(), name, imgtyp)
  }
}

abstract class ImgOp[A, B](id: String = IDGen.gen_new_id(), val name: String = "") extends AnyNode with CalcNode {
  override val typ = "imgop"

  def run(params: Any): Any

  override def toString(): String = {
    "%s:ImgOp: %s".format(id, name)
  }
}

abstract class ImgOp1[A1, A2, B](id: String = IDGen.gen_new_id(), val name: String = "")extends AnyNode with CalcNode  {
  override val typ = "imgop"

  def run(p1: Any, p2: Any): Any

  override def toString(): String = {
    "%s:ImgOp1: %s".format(id, name)
  }
}

class SimpleOp[A, B](name: String, func: Any => Any, types: (String, String), id: String = IDGen.gen_new_id()) extends ImgOp[A, B] {
  //  override val typ = "simpleop"
  def run(p1: Any): Any = {
    func(p1)
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

  override def copy(): SimpleOp[A, B] = {
    new SimpleOp(name, func, types, IDGen.gen_new_id())
  }
}

class SimpleOp1[A1, A2, B](name: String, func: (Any, Any) => Any, types: (String, String, String), id: String = IDGen.gen_new_id()) extends ImgOp1[A1, A2, B] {
  def run(p1: Any, p2: Any): Any = {
    // println(p1,p2)
    func(p1, p2)
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

  override def copy(): SimpleOp1[A1, A2, B] = {
    new SimpleOp1(name, func, types, IDGen.gen_new_id())
  }
}

//class Graph {
//  def add
//}

class Roi(name: String = "", override val id: String = IDGen.gen_new_id()) extends AnyData {
  override def toString = {
    "%s:ROI: %s".format(id, name)
  }
}

class InputRoi(val data: (Int, Int, Int, Int), val name: String = "", override val id: String = IDGen.gen_new_id()) extends Roi with InputNode[Roi] {
  override val typ = "inputroi"

  override def toString = {
    "%s:InputROI: %s: %d".format(id, name, tsort_order)
  }
}

class RowData(val cols: Any*) extends AnyNode {
  override val typ = "rowdata"
  override def toString = {
    "RowData :[%s]".format(cols.mkString(","))
  }
}

class OutputRowData(val path: String) extends RowData with OutputNode[RowData] {
  override val typ = "outputrowdata"
  def saveToFile(d: RowData): Unit = {
    println(d.cols)

    import scalax.io.Codec
    import scalax.io.JavaConverters._

    implicit val codec = Codec.UTF8

    new java.io.File(path).asOutput.write(d.cols.mkString(","))
  }
}

class Pipeline[A](val graph: Defs.G) {
  def then[A, B](node: ImgOp[A, B]): Pipeline[B] = {
    val ns = graph.terminalNodes
    //    println(ns)
    //    assert(ns.length == 1)
    //    println(ns(0))
    val n: ImgOp[A, B] = node.copy().asInstanceOf[ImgOp[A, B]]
    val n2 = node.outputTypes()(0) match {
      case "image" => new ImgNode(id = IDGen.gen_new_id(), name = n.name + " result.")
      case "rowdata" => new RowData()
    }
    graph.addNode(n)
    graph.addNode(n2)
    graph.addEdge(ns(0), n)
    graph.addEdge(n, n2)
    //    println(graph.toDot)
    //    println(graph.nodes)

    this.asInstanceOf[Pipeline[B]]
  }

  def then1[A1, A2, B](node: ImgOp1[A1, A2, B], param: A2): Pipeline[B] = {
    val ns = graph.terminalNodes
    val n: ImgOp1[A1, A2, B] = node.copy().asInstanceOf[ImgOp1[A1, A2, B]]
    val n2 = new ImgNode(id = IDGen.gen_new_id(), name = n.name + " result.")
    graph.addNode(n)
    graph.addNode(n2)
    graph.addEdge(ns(0), n, 0)
    graph.addEdge(n, n2)
    graph.addEdge(param.asInstanceOf[AnyNode], n, 1)
    //    println(graph.toDot)
    //println(graph.nodes)

    this.asInstanceOf[Pipeline[B]]
  }

  def output[A](out: OutputNode[A]): Pipeline[Nothing] = {
    val ns = graph.terminalNodes
    assert(ns.length == 1, ns.mkString)
    graph.addEdge(ns(0), out)
    this.asInstanceOf[Pipeline[Nothing]]
  }

  def verify(ins: Array[String], outs: Array[String]): Boolean = {
    val sorted: Iterable[AnyNode] = graph.tsort
    var count = 0
    for (n: AnyNode <- sorted) {
      n.tsort_order = count
      count += 1
    }

    import scalax.io.Codec
    import scalax.io.JavaConverters._
    implicit val codec = Codec.UTF8
    new java.io.File("test.dot").asOutput.write(graph.toDot)
    true
  }

  def verify(): Boolean = {
    val sorted: Iterable[AnyNode] = graph.tsort
    var count = 0
    for (n: AnyNode <- sorted) {
      n.tsort_order = count
      count += 1
    }

    import scalax.io.Codec
    import scalax.io.JavaConverters._
    implicit val codec = Codec.UTF8
    new java.io.File("test.dot").asOutput.write(graph.toDot)
    true
  }
}

object Pipeline {

  import Defs.G

  def start[A](node: InputNode[A]): Pipeline[A] = {
    val p = new Pipeline[A](new G)
    p.graph.addNode(node)
    //println(p.graph.nodes)
    p
  }

  def cont2[A, B](node: Pipeline[A], node2: Pipeline[B]): Pipeline[(A, B)] = {
    val p = new Pipeline[(A, B)](new G)
    val newg = new Defs.G
    assert(node.graph.terminalNodes.length == 1)
    assert(node2.graph.terminalNodes.length == 1)
    newg.copyFrom(node.graph)
    newg.copyFrom(node2.graph)
    val ns = newg.terminalNodes
    assert(ns.length == 2)
    val newn = new TupleNode(("image", "image"))
    newg.addEdge(ns(0), newn)
    newg.addEdge(ns(1), newn)
    new Pipeline(newg)

  }

  import Defs._

  def run[A1, A2](calc: CompleteCalc): Any = {
    calc.verify()

    val sorted = calc.graph.tsort
    val values = new mutable.HashMap[String, Any] //(calc.graph.nodes.length)

    def getOrCalcNode[A1, A2, B](node: AnyNode): Any = {
      values.get(node.id) match {
        case None => {
          println("Calculating: %s".format(node.toString))
          val v = node match {
            case n: SimpleOp[A1, B] => {
              val i = calc.graph.predecessors(n)(0)
              val p: A1 = getOrCalcNode(i).asInstanceOf[A1]
              n.run(p)
            }
            case n: ImgOp[A1, B] => {
              val i = calc.graph.predecessors(n)(0)
              val p: A1 = getOrCalcNode(i).asInstanceOf[A1]
              n.run(p)
            }
            case n: SimpleOp1[A1, A2, B] => {
              val is = calc.graph.predecessors(n)
              //    println(is.mkString)
              val i1 = is(0)
              val i2 = is(1)
              val p1: Any = getOrCalcNode(i1)
              val p2: Any = getOrCalcNode(i2)
              n.run(p1, p2)
            }
            case n: InputImg => {
              import ij.IJ
              val imp = IJ.openImage(n.path)
              if (imp != null) {
                println("Image opened:%s".format(n.path))
                imp.getProcessor
              } else {
                println("Could not open image: %s".format(n.path))
                None
              }
            }
            case n: InputRoi => {
              values(n.id) = n.data
              n.data
            }
            case n: ImgNode => {
              val c = calc.graph.predecessors(n)(0)
              getOrCalcNode(c)
            }
            case n: TupleNode => {
              val ns = calc.graph.predecessors(n)
              val n1 = getOrCalcNode(ns(0))
              val n2 = getOrCalcNode(ns(1))
              n.run(n1, n2)
            }
            case n => {
              println("getOrCalcNode: No match: %s".format(n.toString))
              None
            }
          }
          values(node.id) = v
          v
        }
        case Some(v) => v.asInstanceOf[Any]
      }
    }

    println(sorted.mkString(":::"))
    for (node <- sorted) {
      val ins = calc.graph.predecessors(node)
      //      val in_types = node.inputTypes()
      //      assert(in_types.length == ins.length)
      for (i <- ins) {
        val t = i.typ
        t match {
          case "image" => getOrCalcNode(i).asInstanceOf[ImageProcessor]
          case "int" => getOrCalcNode(i).asInstanceOf[Int]
          case "roi" => getOrCalcNode(i).asInstanceOf[(Int, Int, Int, Int)]
          case "inputimg" => getOrCalcNode(i).asInstanceOf[ImageProcessor]
          case "inputroi" => getOrCalcNode(i).asInstanceOf[(Int, Int, Int, Int)]
          case "rowdata" => getOrCalcNode(i).asInstanceOf[RowData]
          case "imgop" => {
            val ii = i.asInstanceOf[CalcNode]
            ii.outputTypes()(0) match {
              case "image" => getOrCalcNode(i).asInstanceOf[ImageProcessor]
              case "rowdata" => getOrCalcNode(i).asInstanceOf[RowData]
              case _ => {
                println("No match")
                None
              }
            }
          }
          case "tuple" => {
            //            getOrCalcNode(i).asInstanceOf[(A1, A2)]
          }
          case _ => println("No match of type(%s): %s".format(i.typ, i.toString))
        }
      }
    }
    def isOutput(n: AnyNode) = {
      Array("outputimg").contains(n.typ)
      Array("outputrowdata").contains(n.typ)
    }
    val outs: Array[AnyNode] = sorted.filter(isOutput).toArray
    println(outs.mkString(","))
    outs.map(o => o.asInstanceOf[OutputNode[Any]].saveToFile(values(o.id)))
    values(sorted.last.id)
  }

}

// DiGraph, no parallel edges.
class Graph[A <: AnyNode : ClassTag] {
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

  def toDot: String = {
    def getNodeProp(n: AnyNode): String = {
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
    //    println("terminalNodes")
    //    println(froms)
    //    println(nodes)
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

object Defs {
  val contrast_do = (a: ImageProcessor) => {
    val b = a.duplicate
    b.setMinAndMax(0, 1000)
    b
  }: ImageProcessor
  val cropping = (a: ImageProcessor, roi: (Int, Int, Int, Int)) => {
    val b = a.duplicate
    b.setRoi(roi._1, roi._2, roi._3, roi._4)
    b.crop()
  }: ImageProcessor

  type G = Graph[AnyNode]
  type CompleteCalc = Pipeline[Nothing]
  val crop = new SimpleOp1[ImageProcessor, InputRoi, ImgNode]("crop", cropping.asInstanceOf[(Any, Any) => Any], ("image", "roi", "image"))
  val autocontrast = new SimpleOp[ImgNode, ImgNode]("contrast", contrast_do.asInstanceOf[Any => Any], ("image", "image"))

  val combine2 = new SimpleOp[(ImgNode, ImgNode), ImgNode]("combine", ((a: (ImageProcessor, ImageProcessor)) => {
    println(a._1)
    println(a._2)
    a._2
  }).asInstanceOf[Any => Any], ("(image,image)", "image"))

  val bf = new InputImg("/Users/hiroyuki/repos/ImagePipeline/BF.jpg")
  val cy5 = new InputImg("/Users/hiroyuki/repos/ImagePipeline/Cy5.jpg")
  val roi = new InputRoi((0, 0, 100, 100), "cropping")
  val a = Pipeline.start(bf).then1(crop, roi).then(autocontrast)
  val b = Pipeline.start(cy5).then1(crop, roi).then(autocontrast)
  val outimg = new OutputImg("result final.tiff")
  val cropAndCombine = Pipeline.cont2(a, b).then(combine2).output(outimg)
  cropAndCombine.verify(Array(bf.id, cy5.id), Array())

  val stat = new SimpleOp[ImgNode, RowData]("getStat", ((img: ImageProcessor) => {
    val stat = img.getStatistics
    new RowData(stat.min, stat.max, stat.mean)
  }).asInstanceOf[Any => Any], ("image", "rowdata"))

  val outstat = new OutputRowData("/Users/hiroyuki/repos/ImagePipeline/test_stat.txt")
  val getstas = Pipeline.start(bf).then(stat).output(outstat)


  def merge[A](args: Pipeline[A]*): Pipeline[Array[A]] = {
    val newg = new G
    val newn = new ImgNode("merged")
    args.map(a => newg.copyFrom(a.graph))
    //    println(ns.length)
    //    println(args(0).graph.terminalNodes)
    val ns = newg.terminalNodes
    for (n <- ns) {
      newg.addEdge(n, newn)
    }
    new Pipeline(newg)
  }
}

object Main {

  import Defs._
  import scalax.io._

  def do_cropCombine(): Unit = {
    val g = cropAndCombine
    g.verify()
    val res = Pipeline.run(g).asInstanceOf[ImageProcessor]
    val output: Output = Resource.fromFile("test.dot")
    output.write(g.graph.toDot)(Codec.UTF8)
    import scala.sys.process._
    val cmd = Seq("dot", "-T", "pdf").mkString(" ")
    cmd #< g.graph.toDot #> new java.io.File("test.pdf")
  }

  def do_stat(): Unit = {
    getstas.verify()
    val res = Pipeline.run(getstas).asInstanceOf[RowData]
    println(res)
  }

  def main(args: Array[String]): Unit = {
    do_stat()
    println("Completed.")

  }
}





