package model
import java.net.URLEncoder
import java.net.URL
import java.io.OutputStreamWriter
import org.apache.commons.io.IOUtils
import java.io.StringWriter
import scala.collection.mutable.LinkedList
import play.api.libs.json.Json
import play.api.libs.json.JsArray
import play.api.libs.json.JsString
import play.api.libs.json.JsObject
import play.api.libs.json.JsValue

object ConceptWikiDataAccess {
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

  private def process_matches(matchesString:String):LinkedList[Map[String, String]] = {
    var matchesList = LinkedList[Map[String, String]]()
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
  
 private def process_compound_match(matchesString:String):List[List[String]]={
   def getMatchName(node: JsValue):String = {
       (node \ "match") match {
         case name:JsString =>{
        	 (name.as[String]).replace("<em>","").replace("</em>","")        	 
         }
         case _ => { ""}
       }          
   }
   def getCWuuid(node: JsValue):String = {
       (node \ "uuid") match {
         case uuid:JsString =>{
        	 uuid.as[String]        	 
         }
         case _ => { ""}
       }          
   }   
   def getChemSpiderURL(node: JsValue):String = {
       (node \ "urls") match {
         case urls:JsArray =>{
        	 val chemSpideUrls = urls.value filter (node => ((node \"value").as[String]).indexOf("chemspider")>0)
        	 if (chemSpideUrls.length >0) (chemSpideUrls(0) \ "value").as[String]
        	 else ""
         }
         case _ => { ""}
       }          
   }
   def getMatches(node:JsValue):List[String] ={
     val preferedName = getMatchName(node)
     val chemSpiderUrl = getChemSpiderURL(node)
     val uuid = getCWuuid(node)
     if ((!preferedName.equals(""))&&(!chemSpiderUrl.equals(""))&&(!uuid.equals(""))) List(preferedName,uuid,chemSpiderUrl)
     else List()
     
   }   
   Json.parse(matchesString) match {
     case matches:JsArray =>{
       (matches.value map (getMatches)) toList
     } 
     case _ =>{ println("process_compound_match: NOT JSARRAY FOUND")
       List(List())
       }
   }   
 } 
 private def process_matches2(matchesString:String):LinkedList[Map[String, String]] = {
    var matchesList = LinkedList[Map[String, String]]()
    var js = Json.parse(matchesString)
    var matches = js.asInstanceOf[JsArray]

    var it = matches.productIterator
    var s = matches.value
    var name = ""
    
    var mats = for (a <- s) yield (a.asInstanceOf[JsObject])
    for (mat <- mats) {
      var currentMatch = Map[String, String]()
      var uuid = (mat \ "uuid")
      var names = (mat \ "labels").asInstanceOf[JsArray]
      for (elem <- names.value){
        (elem \ "type") match{
          case n: JsString =>{
            if (n.value.equals("PREFERRED")) name= (elem \ "text").as[String]
          }
          case _ => println("NO TYPE")
        }
      }
      
      var tags = (mat \ "tags").asInstanceOf[JsArray]
      for (tag <- tags.value) {
        for (label <- (tag \ "labels").asInstanceOf[JsArray].value) {
          var lbl = label.asInstanceOf[JsObject]
          var text = lbl \ "text"
          currentMatch += ("name" -> name)
          currentMatch += ("uuid" -> uuid.as[String])
          currentMatch += ("type" -> text.as[String])
        }
      }
      matchesList = matchesList :+ currentMatch
    }
    matchesList    
  }

  private def CW_Search_RAW(q: String) = {
    var parameters = Map("q" -> q)
    OPSAPI_call(parameters, "http://staging.conceptwiki.org/web-ws/concept/search/")
  }

  def CW_Search(searchString: String):LinkedList[Map[String, String]] = {
    process_matches(CW_Search_RAW(searchString))
  }
 
  private def CW_Search_Protein_RAW(q:String)= {
    var parameters = Map( "uuid" -> "eeaec894-d856-4106-9fa1-662b1dc6c6f1","q" -> q,"branch"->"3","limit"->"100")
    OPSAPI_call(parameters, "http://staging.conceptwiki.org/web-ws/concept/search/byTag/")
  }
  
  def CW_Search_Protein(q:String)= {
    process_matches(CW_Search_Protein_RAW(q))
  }

  private def CW_Search_Compound_RAW(q:String)= {
    var parameters = Map( "uuid" -> "07a84994-e464-4bbf-812a-a4b96fa3d197","q" -> q)
    OPSAPI_call(parameters, "http://staging.conceptwiki.org/web-ws/concept/search/byTag/")
  }
  def CW_Search_Compound(q:String)= {
    process_compound_match(CW_Search_Compound_RAW(q))
  }

  private def CW_Get_RAW(uuid: String) = {
    var parameters = Map("uuid" -> uuid)
    OPSAPI_call(parameters, "http://staging.conceptwiki.org/web-ws/concept/get/")
  }
  
}