package Dataset

import dispatch._, Defaults._
import ij.{ImagePlus, IJ}
import ij.process.ImageProcessor
import org.netbeans.api.keyring.Keyring
import imagepipeline._, Defs._

import scala.collection._

class Dataset {
  def run(): Unit = {

  }
}

case class RoiDataset(image: String, roi: String, convertImgPath: String => String = a => a) {

  import Dataset._

  // from Github gist
  private def readAll(arr: String*): Option[Array[String]] = {
    val res = new mutable.ArrayBuffer[String]
    for (path <- arr) {
      readGist(path, true) match {
        case Some(t) => res += t
        case None => return None
      }
    }
    Some(res.toArray)
  }

  def run[A](calc: Pipeline21[ImageProcessor, Roi, A], firstNum: Int = -1): Unit = {
    readAll(image, roi) match {
      case Some(ts) => {
        val ls = if (firstNum > 0) ts(0).lines.take(firstNum) else ts(0).lines
        process(ls, ts(1), calc)
      }
      case None => println("Could not read files")
    }
  }

  def process[A](imgs: Iterator[String], rois: String, calc: Pipeline21[ImageProcessor, Roi, A]): Unit = {
    val r = Csv.read(rois, "Slice")
    //    println(imgs.length)
    def getRoi(m: Array[Map[String, String]]): Array[Roi] = {
      m.map(mm => {
        (mm("BX").toInt, mm("BY").toInt, mm("Width").toInt, mm("Height").toInt)
      })
    }
    var count = 0
    var countfile = 0
    for ((im, i) <- imgs.zipWithIndex) {
      countfile += 1
      //      println(im,im.replace("640tirf","ricm"))
      r.get((i + 1).toString) match {
        case Some(rois) => {
          val path = convertImgPath(im)
          println("#%03d(%03d) Loading: %s".format(countfile,count,path))
          val img1 = IJ.openImage(path).getProcessor
          //          val img2 = IJ.openImage(im.replace("640tirf", "ricm")).getProcessor
          for (r <- getRoi(rois)) {
            count += 1
            val res: A = calc.run(img1, r)
            println(res.asInstanceOf[RowData])
            //            IJ.save(new ImagePlus("result", res), "./testimgs/%03d.tif".format(count))
          }
        }
        case None =>
      }
    }
  }
}

object Csv {
  def read(str: String, key: String, sep: String = "\t"): Map[String, Array[Map[String, String]]] = {
    val res = new mutable.HashMap[String, mutable.ArrayBuffer[Map[String, String]]]
    val lines = str.lines
    val header = lines.next().split(sep)
    val keyidx = header.indexOf(key)
    for (l <- lines) {
      val cols = l.split(sep)
      val m = header.zip(cols).map(a => a._1 -> a._2).toMap
      val k = cols(keyidx)
      if (res.get(k).isEmpty) {
        res(k) = new mutable.ArrayBuffer[Map[String, String]]
      }
      res(k) += m
    }
    res.mapValues(_.toArray)
  }
}

object Dataset {
  def readGist(id: String, secret: Boolean = false): Option[String] = {
    try {
      val req = if (secret)
        url("https://api.github.com/gists/" + id).as_!("hirokai", KeyChain.readToken.getOrElse(""))
      else
        url("https://api.github.com/gists/" + id)

      val resp = Http(req OK as.String)
      val s = resp()

      import play.api.libs.json._
      val obj = Json.parse(s)
      Some((obj \\ "content")(0).as[String])
    } catch {
      case _: Exception => None
    }
  }
}

object KeyChain {
  private val keyName = "ImagePipeline"

  def readToken =
    Keyring.read(keyName) match {
      case null => None
      case chars => Some(chars.mkString)
    }

  def storeToken(token: String) {
    Keyring.save(keyName, token.toCharArray, "access token for ImagePipeline Github Gists")
  }

  def deleteToken {
    Keyring.delete(keyName)
  }
}