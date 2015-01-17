package imagepipeline

import ij.IJ
import ij.process.ImageProcessor

package object funcs {
  val readLines = new SimpleOp1[String, Array[String]]("image load", (path: String) => {
    import scalax.io._
    val input: Input = Resource.fromFile(path)
    input.string(Codec.UTF8).lines.toArray
  }, ("path", "[path]"))


  val stat = new SimpleOp1[ImageProcessor, RowData]("getStat", (img: ImageProcessor) => {
    val stat = img.getStatistics
    new RowData(stat.min, stat.max, stat.mean)
  }, ("image", "rowdata"))

  val statroi = new SimpleOp2[ImageProcessor, (Int, Int, Int, Int), RowData]("getStat", (img: ImageProcessor, roi: (Int, Int, Int, Int)) => {
    img.setRoi(roi._1, roi._2, roi._3, roi._4)
    val stat = img.getStatistics
    new RowData(stat.min, stat.max, stat.mean)
  }, ("image", "roi", "rowdata"))
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

  type Path = String

  val selectRois = new SimpleOp2[Path,Path,Array[String]]("select rois", (imagepath: Path, roi: Path) => {
    import scalax.io._
    val input: Input = Resource.fromFile(roi)
    input.string(Codec.UTF8).lines.toArray
  }: Array[String], ("path","path","[roi]"))


}


object Defs {
  import funcs._

  type G = Graph[AnyNode[_]]
  type CompleteCalc = Pipeline[Nothing]
  val crop = new SimpleOp2[ImageProcessor, (Int, Int, Int, Int), ImageProcessor]("crop", cropping, ("image", "roi", "image"))
  val autocontrast = new SimpleOp1[ImageProcessor, ImageProcessor]("contrast", contrast_do, ("image", "image"))

  val combine2 = new SimpleOp2[ImageProcessor, ImageProcessor, ImageProcessor]("combine", (a: ImageProcessor, b: ImageProcessor) => {
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
  }, ("image", "image", "image"))

  val imload = new SimpleOp1[String, ImageProcessor]("image load", (path: String) => {
    IJ.openImage(path).getProcessor
  }, ("path", "image"))

  val bf = new InputImg("/Users/hiroyuki/repos/ImagePipeline/BF.jpg")
  val cy5 = new InputImg("/Users/hiroyuki/repos/ImagePipeline/Cy5.jpg")
  val roi = new InputRoi("cropping")
  val a = Pipeline.start(bf).then2(crop, roi).then(autocontrast)
  val b = Pipeline.start(cy5).then2(crop, roi).then(autocontrast)
  val outimg = new OutputImg("result final.tiff", "Result")
  val cropAndCombine = Pipeline.cont2(combine2, a, b).end().interface((bf, cy5, roi))
  cropAndCombine.verify(Array(bf.id, cy5.id), Array())


  val outstat = new OutputRowData()

  val file_path = new InputFilePath()

  val getstats: CompleteCalc = Pipeline.start(file_path).then(imload).then(stat).end().interface(file_path)

  val getstats_roi: CompleteCalc = Pipeline.start(file_path).then(imload).then2(statroi, roi).end().interface(file_path, roi)

}
