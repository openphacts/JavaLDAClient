package LDA_API
import java.net.URLEncoder  
import java.net.URL
import java.io.OutputStreamWriter
import org.apache.commons.io.IOUtils
import java.io.StringWriter
import play.api.libs.json.Json
import play.api.libs.json.JsArray
import play.api.libs.json.JsString
import play.api.libs.json.JsObject
import play.api.libs.json.JsValue
import java.util.{List=>JavaList}
import java.util.{Map=>JavaMap}
import collection.JavaConversions._

object ConceptWikiDataAccessJava{
  
  def CW_Search_Protein(q:String,conceptwikiURL:String):JavaList[JavaMap[String, String]]= {
    def convert(targetMap: Map[String,String]):JavaMap[String,String] ={
    		collection.mutable.Map(targetMap.toSeq: _*)
    }
    (ConceptWikiDataAccessScala.CW_Search_Protein(q,conceptwikiURL)).map(convert)
  }
  def CW_Search_Compound(q:String,concepwikiURL:String):JavaList[JavaMap[String, String]]= {
    def convert(targetMap: Map[String,String]):JavaMap[String,String] ={
    		collection.mutable.Map(targetMap.toSeq: _*)
    }
    (ConceptWikiDataAccessScala.CW_Search_Compound(q,concepwikiURL)).map(convert)
  }
}
object ConceptWikiDataAccessScala {

  
  private def OPSAPI_call(parameters: Map[String, String], urlcall: String) = {

    var params = for (parameter <- parameters) yield (URLEncoder.encode(parameter._1, "UTF-8") + "=" + URLEncoder.encode(parameter._2, "UTF-8"))

    var url = new URL(urlcall)
    var conn = url.openConnection()
    conn.setDoOutput(true)
    var wr = new OutputStreamWriter(conn.getOutputStream())
    wr.write(params.mkString("&"))
    wr.flush()
    var writer = new StringWriter()
    IOUtils.copy(conn.getInputStream(), writer, "UTF-8")
    writer.toString().replace("<em>","").replace("</em>","")

  }

  private def process_matches(matchesString:String) = {
    var matchesList = List[collection.immutable.Map[String, String]]()
    var js = Json.parse(matchesString)
    var matches = js.asInstanceOf[JsArray]

    var it = matches.productIterator
    var s = matches.value

    var mats = for (a <- s) yield (a.asInstanceOf[JsObject])
    for (mat <- mats) {
      var currentMatch = Map[String, String]()
      var mtc = mat \ "match"
      var uuid = (mat \ "uuid")
      var tags = (mat \ "tags").asInstanceOf[JsArray]
      for (tag <- tags.value) {
        for (label <- (tag \ "labels").asInstanceOf[JsArray].value) {
          var lbl = label.asInstanceOf[JsObject]
          var text = lbl \ "text"
          currentMatch += ("matchstring" -> mtc.as[String])
          currentMatch += ("uuid" -> uuid.as[String])
          currentMatch += ("type" -> text.as[String])
        }
      }
      matchesList = matchesList :+ currentMatch
    }
    matchesList   
  }
  
 
  private def CW_Search_Protein_RAW(q:String,conceptwikiURL:String)= {
    var parameters = Map( "uuid" -> "eeaec894-d856-4106-9fa1-662b1dc6c6f1","q" -> q,"branch"->"3","limit"->"100")
    OPSAPI_call(parameters, conceptwikiURL+"/web-ws/concept/search/byTag/")
  }
  
  def CW_Search_Protein(q:String,conceptwikiURL:String):List[Map[String, String]]= {
    val r= CW_Search_Protein_RAW(q,conceptwikiURL)
    println(r)
    process_matches(r)
  }

  private def CW_Search_Compound_RAW(q:String,concepwikiURL:String)= {
    var parameters = Map( "uuid" -> "07a84994-e464-4bbf-812a-a4b96fa3d197","q" -> q)
    OPSAPI_call(parameters, concepwikiURL+"/web-ws/concept/search/byTag/")
  }
  def CW_Search_Compound(q:String,concepwikiURL:String):List[Map[String, String]]= {
    process_matches(CW_Search_Compound_RAW(q,concepwikiURL))
  }
  
}