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

class AnyNode(val name: String = "", val id: String = IDGen.gen_new_id(), var tsort_order: Int = -1) {
  val typ = "any"

  def copy(): AnyNode = {
    new AnyNode(id = IDGen.gen_new_id())
  }
}

abstract class CalcNode extends AnyNode {
  def inputTypes(): Array[String]

  def outputTypes(): Array[String]
}

trait DataNode {

}

trait AnyEdge


trait InputNode[A] extends AnyNode with DataNode {
  //  override val typ = "input"
}

trait OutputNode[A] extends AnyNode with DataNode {
  override val typ = "output"

  def saveToFile(dat: A): Unit
}

class InputImg(val path: String, name: String = "", override val id: String = IDGen.gen_new_id()) extends AnyNode with InputNode[ImageProcessor] {
  override val typ = "inputimg"

  override def toString = {
    "%s:InputImg(%s): %d".format(id, name, tsort_order)
  }

  override def copy: InputImg = {
    new InputImg(name)
  }
}

// NOT cloneable!
class OutputImg(path: String, name: String = "", id: String = IDGen.gen_new_id()) extends AnyNode with OutputNode[ImageProcessor] {
  override val typ = "outputimg"

  override def toString = {
    "%s:OutputImg: %s: %s".format(id, name, tsort_order)
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

class ImgNode(id: String = IDGen.gen_new_id(), name: String = "", imgtyp: String = "gray") extends AnyNode with DataNode {
  override val typ = "image"

  override def toString = {
    "%s:ImgNode: %s: %d".format(id, name, tsort_order)
  }

  override def copy(): ImgNode = {
    new ImgNode(IDGen.gen_new_id(), name, imgtyp)
  }
}

trait Calc1 {
  def run(p: Any): Any
}

trait Calc2 {
  def run(p1: Any, p2: Any): Any
}

abstract class ImgOp1[A, B](id: String = IDGen.gen_new_id(), name: String = "") extends CalcNode with Calc1 {
  override val typ = "imgop"

  def run(params: Any): Any

  override def toString(): String = {
    "%s:ImgOp: %s".format(id, name)
  }
}

abstract class ImgOp2[A1, A2, B](id: String = IDGen.gen_new_id(), name: String = "") extends CalcNode with Calc2 {
  override val typ = "imgop"

  def run(p1: Any, p2: Any): Any

  override def toString(): String = {
    "%s:ImgOp1: %s".format(id, name)
  }
}

class SimpleOp1[A, B](name: String, func: Any => Any, types: (String, String), id: String = IDGen.gen_new_id()) extends ImgOp1[A, B] with Calc1 {
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

  override def copy(): SimpleOp1[A, B] = {
    new SimpleOp1(name, func, types, IDGen.gen_new_id())
  }
}

class SimpleOp2[A1, A2, B](name: String, func: (Any, Any) => Any, types: (String, String, String), id: String = IDGen.gen_new_id()) extends ImgOp2[A1, A2, B] with Calc2 {
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

  override def copy(): SimpleOp2[A1, A2, B] = {
    new SimpleOp2(name, func, types, IDGen.gen_new_id())
  }
}

class Roi(name: String = "", override val id: String = IDGen.gen_new_id()) extends AnyNode with DataNode {
  override def toString = {
    "%s:ROI: %s".format(id, name)
  }
}

class InputRoi(val data: (Int, Int, Int, Int), name: String = "", override val id: String = IDGen.gen_new_id()) extends Roi with InputNode[Roi] {
  override val typ = "inputroi"

  override def toString = {
    "%s:InputROI: %s: %d".format(id, name, tsort_order)
  }
}

class RowData(val cols: Any*) extends AnyNode with DataNode {
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
  def then[A, B](node: ImgOp1[A, B]): Pipeline[B] = {
    val ns = graph.terminalNodes
    //    println(ns)
    //    assert(ns.length == 1)
    //    println(ns(0))
    val n: ImgOp1[A, B] = node.copy().asInstanceOf[ImgOp1[A, B]]
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

  def then1[A1, A2, B](node: ImgOp2[A1, A2, B], param: A2): Pipeline[B] = {
    val ns = graph.terminalNodes
    val n: ImgOp2[A1, A2, B] = node.copy().asInstanceOf[ImgOp2[A1, A2, B]]
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

  def output(out: OutputNode[_]): Pipeline[Nothing] = {
    val ns = graph.terminalNodes
    assert(ns.length == 1, ns.mkString)
    graph.addEdge(ns(0), out)
    this.asInstanceOf[Pipeline[Nothing]]
  }

  def interface[In <: Product, Out <: Product](ins: In, outs: Out): Pipeline[A] = {
    assert(ins.productArity == graph.startingNodes.length)
    assert(outs.productArity == graph.terminalNodes.length)

    // sort tsort_order for input nodes, so that input will match with args.
    val ins2 = ins.productIterator.toSeq.map(_.asInstanceOf[AnyNode])
    val ns = ins2.map(_.tsort_order).toSeq.sorted
    ns.zip(ins2).map(a => a._2.tsort_order = a._1)

    //same for output
    val outs2 = outs.productIterator.toSeq.map(_.asInstanceOf[AnyNode])
    val ns2 = outs2.map(_.tsort_order).toSeq.sorted
    ns2.zip(ins2).map(a => a._2.tsort_order = a._1)
    interfaceSet = true
    this
  }

  var interfaceSet: Boolean = false

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

  def verify(): Unit = {
    if(!interfaceSet)
      throw new Exception("Input and output should be set before run.")
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

  def cont2[A1, A2, B](func: SimpleOp2[A1, A2, B], node: Pipeline[A1], node2: Pipeline[A2]): Pipeline[B] = {
    val newg = new Defs.G
    assert(node.graph.terminalNodes.length == 1)
    assert(node2.graph.terminalNodes.length == 1)
    newg.copyFrom(node.graph)
    newg.copyFrom(node2.graph)
    val ns = newg.terminalNodes
    assert(ns.length == 2)
    newg.addEdge(ns(0), func)
    newg.addEdge(ns(1), func)
    new Pipeline(newg)

  }


  import Defs._

  def run[A1, A2, In <: Product](calc: CompleteCalc, args: In): Array[Any] = {
    calc.verify()

    val sorted = calc.graph.tsort
    val values = new mutable.HashMap[String, Any] //(calc.graph.nodes.length)

    val ins: Array[AnyNode] = sorted.filter(_.isInstanceOf[InputNode[_]]).toSeq.sortBy(_.tsort_order).toArray
    println(ins.mkString)
    if(ins.length != args.productArity){
      throw new IllegalArgumentException("Arity does not match.")
    }
    for((p,i) <- args.productIterator.toArray.zip(ins)){
      println(p,i)
      values(i.asInstanceOf[AnyNode].id) = p
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
          assert(ins.length == 1)
          assert(ins(0).isInstanceOf[CalcNode])
          val c = ins(0).asInstanceOf[CalcNode]
          val params = calc.graph.predecessors(c)
          println(params.mkString(","),c.name)
          val res = c match {
            case c: Calc1 => {
              assert(params.length == 1)
              c.run(values(params(0).id))
            }
            case c: Calc2 => {
              assert(params.length == 2)
              println(params(0).id,params(1).id)
              println(values(params(0).id),values(params(1).id))
              c.run(values(params(0).id), values(params(1).id))
            }
          }
          values(n.id) = res
        }
        case n: CalcNode =>
      }
    }

    val outs: Array[AnyNode] = sorted.filter(_.isInstanceOf[OutputNode[_]]).toSeq.sortBy(_.tsort_order).toArray
    println("Outputs: " + outs.mkString(","))
    outs.map(o => o.asInstanceOf[OutputNode[Any]].saveToFile(values(o.id)))
    outs.map(o => values(o.asInstanceOf[OutputNode[Any]].id))
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
  val crop = new SimpleOp2[ImageProcessor, InputRoi, ImgNode]("crop", cropping.asInstanceOf[(Any, Any) => Any], ("image", "roi", "image"))
  val autocontrast = new SimpleOp1[ImgNode, ImgNode]("contrast", contrast_do.asInstanceOf[Any => Any], ("image", "image"))

  val combine2 = new SimpleOp2[ImgNode, ImgNode, ImgNode]("combine", ((a: ImageProcessor, b: ImageProcessor) => {
    import ij.plugin.StackCombiner
    import ij.ImageStack

    def f(img: ImageProcessor): ImageStack = {
      val s = new ImageStack(img.getWidth, img.getHeight)
      s.addSlice(img)
      s
    }
    val s1 = f(a)
    val s2 = f(b)
    val s = new StackCombiner().combineHorizontally(s1, s2)
    val r = s.getProcessor(1)
    //    new ImagePlus("result", r).show()
    r
  }: ImageProcessor).asInstanceOf[(Any, Any) => Any], ("image", "image", "image"))

  val bf = new InputImg("/Users/hiroyuki/repos/ImagePipeline/BF.jpg")
  val cy5 = new InputImg("/Users/hiroyuki/repos/ImagePipeline/Cy5.jpg")
  val roi = new InputRoi((0, 0, 100, 100), "cropping")
  val a = Pipeline.start(bf).then1(crop, roi).then(autocontrast)
  val b = Pipeline.start(cy5).then1(crop, roi).then(autocontrast)
  val outimg = new OutputImg("result final.tiff", "Result")
  val cropAndCombine = Pipeline.cont2(combine2, a, b).output(outimg).interface((bf,cy5,roi),Tuple1(outimg))
  cropAndCombine.verify(Array(bf.id, cy5.id), Array())

  val stat = new SimpleOp1[ImgNode, RowData]("getStat", ((img: ImageProcessor) => {
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
    import scala.sys.process._
    val g = cropAndCombine
    g.verify()
    val img1 = IJ.openImage("/Users/hiroyuki/repos/ImagePipeline/BF.jpg").getProcessor
    val img2 = IJ.openImage("/Users/hiroyuki/repos/ImagePipeline/Cy5.jpg").getProcessor
    val res = Pipeline.run(g,(img1,img2,(0,0,400,300)))(0).asInstanceOf[ImageProcessor]
    println(res)
    Seq("rm", "test.dot").mkString(" ").!
    val output: Output = Resource.fromFile("test.dot")
    output.write(g.graph.toDot)(Codec.UTF8)
    val cmd = Seq("dot", "-T", "pdf", "test.dot").mkString(" ")
    cmd #> new java.io.File("test.pdf")
  }

  def do_stat(): Unit = {
    getstas.verify()
    val res = Pipeline.run(getstas,Tuple1(1)).asInstanceOf[RowData]
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
    do_cropCombine()
    println("Completed.")

  }
}





