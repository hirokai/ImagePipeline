package Dataset

import dispatch._, Defaults._
import ij.{ImagePlus, IJ}
import ij.process.ImageProcessor
import org.netbeans.api.keyring.Keyring
import imagepipeline._, Defs._

import scala.collection._

class Dataset {
  def run(): Unit ={

  }
}

case class RoiDataset(image: String, roi: String) {
  import Dataset._
  // from Github gist
  def run(): Unit = {
    readGist(image, secret=true) match {
      case Some(t) => {
        readGist(roi, secret=true) match {
          case Some(r) => {
            process(t.lines, r, cropAndCombine)
          }
          case _ => println("Could not load ROI list.")
        }
      }
      case _ => println("Could not load image list.")
    }
  }
  def process(imgs: Iterator[String], rois: String, calc: CropType): Unit = {
    val r = Csv.read(rois,"Slice")
//    println(imgs.length)
    def getRoi(m: Array[Map[String,String]]): Array[Roi] = {
       m.map(mm =>{
         (mm("BX").toInt,mm("BY").toInt,mm("Width").toInt,mm("Height").toInt)
       })
    }
    var count = 0
    for((im,i) <- imgs.zipWithIndex) {
      r.get((i+1).toString) match {
        case Some(rois) =>{
          for(r <- getRoi(rois)) {
            count += 1
            val img1 = IJ.openImage(im).getProcessor
            val img2 = IJ.openImage(im.replace("640tirf","ricm")).getProcessor
            val res: ImageProcessor = calc.run(img1, r, img2, r)
            IJ.save(new ImagePlus("result",res),"./testimgs/%03d.tif".format(count))
          }
        }
        case None =>
      }
    }
  }
}

object Csv {
  def read(str: String, key: String, sep: String = "\t"): Map[String,Array[Map[String,String]]] = {
    val res = new mutable.HashMap[String,mutable.ArrayBuffer[Map[String,String]]]
    val lines = str.lines
    val header = lines.next().split(sep)
    val keyidx = header.indexOf(key)
    for(l <- lines) {
      val cols = l.split(sep)
      val m = header.zip(cols).map(a => a._1 -> a._2).toMap
      val k = cols(keyidx)
      if(res.get(k).isEmpty) {
        res(k) = new mutable.ArrayBuffer[Map[String,String]]
      }
      res(k) += m
    }
    res.mapValues(_.toArray)
  }
}

object Dataset {
  def readGist(id: String, secret: Boolean = false): Option[String]= {
    try{
      val req = if(secret)
        url("https://api.github.com/gists/"+id).as_!("hirokai", KeyChain.readToken.getOrElse(""))
      else
        url("https://api.github.com/gists/"+id)

      val resp = Http(req OK as.String)
      val s = resp()

      import play.api.libs.json._
      val obj = Json.parse(s)
      Some((obj \\ "content")(0).as[String])
    }catch{
      case _ => None
    }
  }
}

object KeyChain {
  private val keyName = "ImagePipeline"

  def readToken =
    Keyring.read(keyName) match {
      case null  => None
      case chars => Some(chars.mkString)
    }

  def storeToken(token: String) {
    Keyring.save(keyName, token.toCharArray, "access token for ImagePipeline Github Gists")
  }

  def deleteToken {
    Keyring.delete(keyName)
  }
}