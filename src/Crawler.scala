import sun.plugin.dom.exception.InvalidStateException

import scala.sys.process._

object Crawler extends HtmlOperations {

  val queue = new PriorityQueue()
  val history = new CrawlHistory()

  def main(args: Array[String]) {
    val url = "http://www.conlintravel.com/"
    val output = "/tmp/crawler.txt"

    run(url, output)
  }

  def run(url: String, output:String): Unit ={
    val normUrl = normalizeUrl(url)
    queue.put(Seq(normUrl))

    while(queue.hasUrls) {
      val pUrl = queue.pick
      println(s"processing $pUrl")
      crawlUrl(pUrl)
    }

    history.save(output)
  }

  def crawlUrl(url:String): Unit = {
    val (body, unvisitedUrls) = extractContent(url)
    queue.put(unvisitedUrls)
    history.put(url, body)
  }

  def extractContent(url:String) = {
    val body = getBody(url)
    val normBody = normText(body)

    val html = getHtml(url)
    val urls = extractHrefs(html)
    val unvisitedUrls = getUnvisitedUrls(url, urls)

    (normBody, unvisitedUrls)
  }

  def getUnvisitedUrls(baseUrl:String, urls:Seq[String]) ={
    urls
      .map(u =>
        if(u.endsWith(".php") && !baseUrl.endsWith(".php")){
          baseUrl + u
        }else{
          u
        }
      )
      .filter(isValid)
      .map(normalizeUrl)
      .filterNot(history.visited)
      .filterNot(_ == baseUrl)
      .filter(u => sameTpd(baseUrl, u))
  }

  def normText(text:String) = {
    val allowed = ('a' to 'z') ++ ('A' to 'Z') // ++ List('-')

    text
      .split(" ")
      .map(replacePunctuation)
      .filter(_.forall(allowed.contains(_)))
      .filter(_.nonEmpty)
      .filter(_.length > 1)
      .map(_.toLowerCase)
      .mkString
  }

  def replacePunctuation(word: String) = if( word.endsWith(",") || word.endsWith(".")) word.dropRight(1) else word
}

trait HtmlOperations {
  def normalizeUrl(url:String) = url

  val suffixes = Seq(".html", ".htm", ".php")

  def isValid(url:String) = url.startsWith("http") && suffixes.exists(url.endsWith)

  def getHtml(url:String) = s"curl -L $url" !!

  def getBody(url:String) = s"curl -L $url" #| "html2text -nobs" !! // we can pass already parsed html!

  def extractHrefs(html:String) = {
    val hrefRegex = ".*<a href=\"([^\"]*)\" [^>]*>.*".r

    html.split("\n")
      .map {
        case hrefRegex(c) => Some(c)
        case _ => None
      }
      .filter(_.nonEmpty)
      .map(_.get)
  }

  def host(url:String) = url.split("/")(2)

  def tpd(url:String) = host(url) // todo Top Private Domain

  def sameTpd(url1: String, url2: String) = {
    tpd(url1) == tpd(url2)
  }
}

class PriorityQueue {

  val urlQueue = collection.mutable.Queue[String]()

  def put(urls:Seq[String]) = {
    urls.toSet[String].filterNot(urlQueue.contains).foreach(url => urlQueue.enqueue(url))
  }

  def hasUrls = urlQueue.nonEmpty

  def pick = urlQueue.dequeue()
}

class CrawlHistory extends JsonSerialize {
  val crawlMap = collection.mutable.HashMap[String, String]()

  def visited(url:String): Boolean = crawlMap.contains(url)

  def put(url:String, body:String): Unit ={
    if(crawlMap.contains(url)){
      throw new InvalidStateException("Already been to " + url)
    }
    crawlMap.put(url, body.split("\n").mkString(";"))
  }

  def save(path:String): Unit ={
    writeLines(crawlMap.map(_.toJson), path)
  }


  def writeLines(lines: Iterable[String], fileName: String) = {
    import java.io._
    import java.nio.charset._
    import java.nio.file._
    val writer = Files.newBufferedWriter(Paths.get(fileName), StandardCharsets.UTF_8)
    val pw = new PrintWriter(writer)
    try lines.foreach(pw.println) finally pw.close()
  }
}


trait JsonSerialize {

//  object JsonSerializer {
//    val mapper = createMapper
//
//    def createMapper: ObjectMapper = {
//      val newMapper = new ObjectMapper()
//      newMapper.registerModule(DefaultScalaModule)
//      newMapper.setSerializationInclusion(JsonInclude.Include.NON_NULL)
//      newMapper
//    }
//  }

  implicit class JsonSerializable[T](value: T) {
    def toJson = value.toString//JsonSerializer.mapper.writeValueAsString(value)
  }
}