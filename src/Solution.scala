import java.net.URLDecoder

case class Gag(title: String, preview: String, url: String, votes: Int, comments: Int)

object Solution {

  def get(url: String) = scala.io.Source.fromURL(url).mkString

  def solution = {
    val url = "http://www.9gag.com"
    val content = get(url)
    val rawInfo = content.split("\n").filterNot(_ == "")
    val pattern = "badge-item-img|badge-animated-container-animated|data-title=|(data-entry-(url|votes|comments)=)".r
    val usefullInfo = rawInfo.filter(pattern.findFirstIn(_).isDefined).map {
      elem => """[a-z-]+=(.*)""".r.findFirstMatchIn(elem).map(_ group 1)
    } flatten

    //group info: each list has info about a gag -- 
    val asList = usefullInfo.foldLeft(List.empty[List[String]]) {
      case (acc, ele) if ele.contains("http") && !ele.contains(".mp4") && !ele.contains(".jpg") => acc :+ List(ele)
      case (acc, ele) => acc.init :+ (ele :: acc.last)
    }

    //filter out no safe for work
    val rawContent = asList.filterNot(_.size <= 4)

    //remove all double quotes and decode special chars
    val clean = rawContent.map(_.map {
      s => URLDecoder.decode(s.replace("\"", ""))
    })

    //clean size = 5 -> images
    //clean size = 6 -> movies
    val gags = clean map {
      case gag if gag.length == 5 => Gag(gag(1), )
      case gag if gag.length == 6 => 2
      case _ => ???// should not happen
    }

    gags
  }

  def main(args: Array[String]) = {
    val a = solution
    a map println
    println("good")
  }
}