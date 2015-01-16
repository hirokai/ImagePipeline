package imagepipeline.Test

import imagepipeline._
import ij.IJ
import ij.process.ImageProcessor
import org.scalatest.FlatSpec
import org.scalatest._


import scalax.io.{Codec, Resource, Output}
import Defs._
import scala.util.Random

class SetSpec extends FlatSpec with Matchers {

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
      val getstats_roi: CompleteCalc = Pipeline.start(file_path).then(imload).then1(statroi, roi).output().interface(file_path, roi)
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

class MapNodeSpec extends FlatSpec {
  "map" should "be composable" in {
    val filelist = new InputFileList()
    val a: CompleteCalc = Pipeline.start(filelist).map(imload).output().interface(filelist)
    a.verify()
  }

  it should "run" in {
    val filelist = new InputFileList()
    val a: CompleteCalc = Pipeline.start(filelist).map(imload).output().interface(filelist)
    a.run(Tuple1(Array("./testimgs/BF.jpg", "./testimgs/Cy5.jpg")))
  }

}

