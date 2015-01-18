package imagepipeline.Test

import imagepipeline.Pipeline._
import imagepipeline._
import ij.IJ
import ij.process.ImageProcessor
import org.scalatest.FlatSpec
import org.scalatest._


import scalax.io.{Input, Codec, Resource, Output}
import Defs._
import scala.util.Random
import funcs._

class BasicSpec extends FlatSpec with Matchers {

  import imagepipeline.funcs._
  import Pipeline._

  "getstats" should "not cause error" in {
    val res = getstats.run("./testimgs/BF.tif")
    assert(res.isInstanceOf[RowData])
    println(res)
  }


  "getstats_roi" should "be fine" in {
    val rng = new Random

    for (i <- 0 until 10) {
      val pos = (rng.nextInt(300), rng.nextInt(300), rng.nextInt(300), rng.nextInt(300))
      printf("Repeating: %d\n", i)
      val getstats_roi: Pipeline21[Path, Roi, RowData] = start(file_path).then(imload).then2(statroi, roi).end().inputOrder(file_path, roi)
      val res = getstats_roi.run("./testimgs/BF.tif", pos)
      println(res)
      res.isInstanceOf[RowData] shouldBe true
    }
  }

  "Crop and combine" should "be fine" in {
    def do_cropCombine(): Unit = {
      import scala.sys.process._
      val g = cropAndCombine
      val img1 = IJ.openImage("./testimgs/BF.jpg").getProcessor
      val img2 = IJ.openImage("./testimgs/Cy5.jpg").getProcessor
      val res = g.run(img1, (0, 0, 400, 300), img2, (0, 0, 400, 300))
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

  import Pipeline._

  "map" should "be composable" in {
    val filelist = new InputFileList()
    val a: Pipeline11[Array[Path], Array[ImageProcessor]] = start(filelist).map(imload).end()
    a.verify()
  }

  it should "run" in {
    val filelist = new InputFileList()
    val a: Pipeline11[Array[Path], Array[ImageProcessor]] = start(filelist).map(imload).end()
    a.verify()
    // For why you need runA, see: http://stackoverflow.com/questions/6686992/scala-asinstanceof-with-parameterized-types
    val res: Array[ImageProcessor] = a.runA(Array("./testimgs/BF.tif", "./testimgs/Cy5.tif")).toArray
    println(res)
  }

  var aa: Pipeline11[Path, Array[ImageProcessor]] = _

  "InputFileListFromSource" should "compose" in {
    import imagepipeline.funcs._
    val list = new FilePath
    val a: Pipeline11[Path, Array[ImageProcessor]] = start(list).then(readLines).map(imload).end()
    aa = a
  }

  it should "run" in {
    val input_file = "./testimgs/list.txt"
    val res = aa.runA(input_file)
    val input: Input = Resource.fromFile(input_file)
    res.length shouldEqual input.string(Codec.UTF8).lines.toArray.length
  }

  var op: Pipeline11[Path,Array[Int]] = _
  var op2: Pipeline11[String, Array[Int]] = _
  var op3: Pipeline11[Array[Int], Array[Int]] = _
  "Double map" should "be typed" in {
    val identity = new SimpleOp1[Path, Path]("identity", (p: Path) => p, ("path", "path"))
    val duplicate = new SimpleOp1[Path, Array[Path]]("duplicate", (p: Path) => Array(p, p, p), ("path", "[path]"))
    val strLength = new SimpleOp1[String, Int]("", _.length, ("path", "int"))
    val numX2 = new SimpleOp1[Int, Int]("", _ * 2, ("int", "int"))
    val duplicate2 = new SimpleOp1[Array[Path], Array[Path]]("duplicate2", (p: Array[Path]) => Array(p, p, p).flatten, ("[path]", "[path]"))
    val path = new FilePath
    val numbers = new InputArrayNode[Int]

    op2 = start(path).then(duplicate).map(strLength -> numX2).end()
    op3 = start(numbers).map(numX2).end()
  }

  it should "run" in {
    val res2 = op2.run("test").asInstanceOf[Array[_]].deep
    val res3 = op3.runA(Array(1, 2, 3, 4): Array[Int]).toArray
    println(res3.deep)
    println(res2.mkString(","))
  }

  var getstats_roi: Pipeline21[Path, Roi, Array[RowData]] = _
  "Multiple ROIs for each file" should "compose" in {
    val list = new FilePath
    val roi = new InputRoi
    val rois = new FilePath
    val entry = new FilePath
    val proc_entry: Pipeline21[Path, Roi, RowData] = start(entry).then(imload).then2(statroi, roi).end().inputOrder(entry,roi)
    val roi_of_slice: Pipeline21[Path, Path, Array[Roi]] = start(entry).then2(selectRois, rois).end().inputOrder(entry,rois)
    getstats_roi = start(list).then(readLines).map2(proc_entry, roi).end().inputOrder(list,roi)
  }

  it should "run" in {
    val res: Array[RowData] = getstats_roi.runA("./testimgs/list.txt", (0, 0, 100, 100)).toArray
    res.isInstanceOf[Array[RowData]] shouldBe true
    println(res.deep)
  }

}

// Uses local files that do not work on Travis.
class DatasetExamples extends FlatSpec with Matchers {
  import Dataset._
  import Pipeline._
  "Crop and combine for images" should "run" in {
    val ds = RoiDataset("86cbd2693d78d27eebbc","384bf685329387382358")
    // FIXME: This runs, but the results is wrong. (2 channels have same images for some reason.)
    cropAndCombine.save_dot(cropAndCombine.graph, "test")
//    ds.run(cropAndCombine)
  }
  "Stats for ROIs" should "run" in {
    val img = new InputImg
    val getstats_roi: Pipeline21[ImageProcessor, Roi, RowData] = start(img).then2(statroi, roi).end().inputOrder(img,roi)
    def f(path: String) = {
      path.replaceAll(""".+?/(\d{8}.+)$""","http://storage.googleapis.com/tiffimages/$1")
    }
    val ds = RoiDataset("86cbd2693d78d27eebbc","384bf685329387382358",convertImgPath=f)
    ds.run(getstats_roi, 20)
  }
}
