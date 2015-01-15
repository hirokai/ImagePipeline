import ij.IJ
import ij.process.ImageProcessor
import org.scalatest.FlatSpec

import scalax.io.{Codec, Resource, Output}
import Defs._

class SetSpec extends FlatSpec {

  "An empty Set" should "have size 0" in {
    assert(Set.empty.size == 0)
  }

  it should "not cause error" in {
    import Defs._
    getstats.verify()
    val res = Pipeline.run(getstats, Tuple1("/Users/hiroyuki/repos/ImagePipeline/BF.jpg"))
    println(res.mkString(","))
  }


  "getstats_roi" should "be fine" in {
    import Defs._
    getstats_roi.verify()
    val res = Pipeline.run(getstats_roi, ("/Users/hiroyuki/repos/ImagePipeline/BF.jpg", (0,0,300,300)))
    println(res.mkString(","))
  }

  "Crop and combine" should "be fine" in {
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
    a.run(Tuple1(Array("/Users/hiroyuki/repos/ImagePipeline/BF.jpg","/Users/hiroyuki/repos/ImagePipeline/Cy5.jpg")))
  }

}