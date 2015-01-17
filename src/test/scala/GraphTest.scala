package imagepipeline.Test

import imagepipeline._
import ij.IJ
import ij.process.ImageProcessor
import org.scalatest.FlatSpec
import org.scalatest._


import scalax.io.{Input, Codec, Resource, Output}
import Defs._
import scala.util.Random
import funcs._

class SetSpec extends FlatSpec with Matchers {
  import imagepipeline.funcs._

  "getstats" should "not cause error" in {
    import Defs._
    val res = getstats.run(Tuple1("./testimgs/BF.tif"))
    println(res.mkString(","))
  }


  "getstats_roi" should "be fine" in {
    val rng = new Random

    for (i <- 0 until 10) {
      val pos = (rng.nextInt(300), rng.nextInt(300), rng.nextInt(300), rng.nextInt(300))
      printf("Repeating: %d\n", i)
      val getstats_roi: CompleteCalc = Pipeline.start(file_path).then(imload).then2(statroi, roi).end().interface(file_path, roi)
      val res = Pipeline.run(getstats_roi, ("./testimgs/BF.tif", pos))

      res.length shouldBe 1
      res(0).isInstanceOf[RowData] shouldBe true
    }
  }

  "Crop and combine" should "be fine" in {
    def do_cropCombine(): Unit = {
      import scala.sys.process._
      val g = cropAndCombine
      g.verify()
      val img1 = IJ.openImage("./testimgs/BF.jpg").getProcessor
      val img2 = IJ.openImage("./testimgs/Cy5.jpg").getProcessor
      val res = Pipeline.run(g, (img1, img2, (0, 0, 400, 300)))(0).asInstanceOf[ImageProcessor]
      println(res)
      Seq("rm", "test.dot").mkString(" ").!
      val output: Output = Resource.fromFile("test.dot")
      output.write(g.graph.toDot)(Codec.UTF8)
      val cmd = Seq("dot", "-T", "pdf", "test.dot").mkString(" ")
      cmd #> new java.io.File("test.pdf")
    }
  }

}

class MapNodeSpec extends FlatSpec with Matchers {
  "map" should "be composable" in {
    val filelist = new InputFileList()
    val a: CompleteCalc = Pipeline.start(filelist).map(imload).end().interface(filelist)
    a.verify()
  }

  it should "run" in {
    val filelist = new InputFileList()
    val a: CompleteCalc = Pipeline.start(filelist).map(imload).end().interface(filelist)
    val res = a.run(Tuple1(Array("./testimgs/BF.tif", "./testimgs/Cy5.tif")))(0).asInstanceOf[Array[Any]]
    res(0).asInstanceOf[ImageProcessor]
    res(1).asInstanceOf[ImageProcessor]
  }

  "InputFileListFromSource" should "compose" in {
    import imagepipeline.funcs._

    val input_file = "./testimgs/list.txt"
    val list = new FilePath
    val a: CompleteCalc = Pipeline.start(list).then(readLines).map(imload).end().interface(list)
    val res = a.run(Tuple1(input_file))(0).asInstanceOf[Array[Any]]
    val input: Input = Resource.fromFile(input_file)
    res.length shouldEqual input.string(Codec.UTF8).lines.toArray.length
  }


  "Double map" should "be typed" in {
    val identity = new SimpleOp1[Path, Path]("identity",(p: Path) => p,("path","path"))
    val duplicate = new SimpleOp1[Path, Array[Path]]("duplicate",(p: Path) => Array(p,p,p),("path","[path]"))
    val strLength = new SimpleOp1[String,Int]("",_.length,("path","int"))
    val numX2 = new SimpleOp1[Int,Int]("",_*2,("int","int"))
    val duplicate2 = new SimpleOp1[Array[Path], Array[Path]]("duplicate2",(p: Array[Path]) => Array(p,p,p).flatten,("[path]","[path]"))
    val path = new FilePath
    val numbers = new InputArrayNode[Int]

    val op = Pipeline.start(path).then(duplicate).map(strLength).mapmap(numX2).end()

    a [ClassCastException] should be thrownBy {
      val op = Pipeline.start(path).then(duplicate).map(numX2).end()
      op.run("test")
    }

    val op2 = Pipeline.start(path).then(duplicate).map(strLength->numX2).end()
    val op3 = Pipeline.start(numbers).map(numX2).end()
    val res = op.run("test").asInstanceOf[Array[_]].deep
    val res2 = op2.run("test").asInstanceOf[Array[_]].deep
    val res3 = op3.run(Array(1,2,3,4))
    println(res3.deep)
    println(res.mkString(","))
    println(res2.mkString(","))
    assert(res == res2)
  }

  "Multiple ROIs for each file" should "compose" in {
    val list = new FilePath
    val roi = new InputRoi
    val rois = new InputArrayNode[(Int,Int,Int,Int)]
    val entry = new FilePath
    val proc_entry = Pipeline.start(entry).then(imload).then2(statroi, roi).end().interface(entry,roi)
    val roi_of_slice = Pipeline.start(entry).then2(selectRois,rois).end().interface(entry,rois)
    val getstats_roi: CompleteCalc = Pipeline.start(list).then(readLines).map2(proc_entry,roi).end().interface(list,roi)
    val res = getstats_roi.run(("./testimgs/list.txt", (0,0,100,100)))(0).asInstanceOf[Array[Any]]
    res.length shouldEqual 2
  }

}

