package LDA_API

import play.api.libs.ws.WS
import java.io.File
import java.io.FileInputStream
import java.io.ByteArrayInputStream
import java.net.URI
import java.net.URLEncoder
import java.net.URL
import java.net.HttpURLConnection
import scala.collection.mutable.LinkedList
import play.api.libs.json.JsArray
import play.api.libs.json.JsObject
import collection.JavaConversions._
import org.apache.commons.io.IOUtils
import java.io.StringWriter
import java.sql.Connection
import scala.collection.mutable.HashMap
import java.sql.PreparedStatement
import java.sql.Types._
import play.api.libs.json.Json
import play.api.libs.json.JsString
//import com.codahale.jerkson.Json._
import play.api.libs.json.JsValue
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer
import play.api.libs.json.JsUndefined
import play.api.libs.json.JsNumber

object OPSLDA {

  //Given an target identifier (concept wiki uri) returns information associated to the target
  //calling the 
  def GetTargetInfo(targetURI: String) =
    {
      println("URI:" + targetURI)
      var urlcall = "http://api.openphacts.org/target?uri=" + URLEncoder.encode(targetURI, "UTF-8")
      var url = new URL(urlcall);
      var conn = url.openConnection().asInstanceOf[HttpURLConnection]
      conn.setRequestMethod("GET")
      conn.setRequestProperty("Accept", "application/json")

      var writer = new StringWriter();
      var is = conn.getInputStream()
      IOUtils.copy(is, writer, "UTF-8");

      val json = Json.parse(writer.toString())
      if (!(json \ "result" \ "primaryTopic").getClass().toString().contains("JsUndefined")) {
        val about = (json \ "result" \ "primaryTopic" \ "_about").as[String]
        val inDataset = (json \ "result" \ "primaryTopic" \ "inDataset").as[String]
        val prefLabel = (json \ "result" \ "primaryTopic" \ "prefLabel").as[String]
        val exactMatch: JsArray = (json \ "result" \ "primaryTopic" \ "exactMatch").asInstanceOf[JsArray]
        val firstelem = Map("_about" -> List(List("Resource", about)), "inDataset" -> List(List("Resource", inDataset)), "prefLabel" -> List(List("Literal", prefLabel)))
        val res = for (elem <- exactMatch.value)
          yield (
          {
            var mapMatch = Map[String, List[Object]]()
            //elem is one exactMatch
            if (elem.isInstanceOf[JsObject]) {

              val obj = elem.asInstanceOf[JsObject]
              //iterate into each pair element of one exactMatch
              for (elem2 <- obj.value) {
                //the first element is the tag that will appear in the page
                //The secod element can be a string or a array
                if (elem2._2.isInstanceOf[JsString]) {
                  //If is a String we check is if a url or a literal
                  if (elem2._2.toString().replace("\"", "").startsWith("http://")) {
                    mapMatch += elem2._1 -> List(List("Resource", elem2._2.toString().replace("\"", "")))
                  } else {
                    mapMatch += elem2._1 -> List(List("Literal", elem2._2.toString().replace("\"", "")))
                  }
                }
                //If the second element is an Array we iterate over each element
                if (elem2._2.isInstanceOf[JsArray]) {
                  var list2 = for (arrayElem <- elem2._2.asInstanceOf[JsArray].value)
                    yield (
                    {
                      var listType: String = ""

                      if (arrayElem.toString().replace("\"", "").startsWith("http://")) {
                        listType = "Resource"
                      } else {
                        listType = "Literal"
                      }
                      List(listType, arrayElem.toString().replace("\"", ""))
                    })
                  mapMatch += elem2._1 -> list2.toList
                  //println(List(elem2._1,List("Resource",elem2._2)))
                }

              }
            }
            mapMatch
          })
        val total = Seq(firstelem) ++ res
        total
      } else {
        Map()
      }

    }

  //Given an compound identifier (concept wiki uri) returns information associated to the compound
  //calling the 
  def GetCompoundInfo(compoundURI: String) =
    {
      println("URI:" + compoundURI)
      var urlcall = "http://api.openphacts.org/compound?uri=" + URLEncoder.encode(compoundURI, "UTF-8")
      var url = new URL(urlcall);
      var conn = url.openConnection().asInstanceOf[HttpURLConnection]
      conn.setRequestMethod("GET")
      conn.setRequestProperty("Accept", "application/json")

      var writer = new StringWriter();
      var is = conn.getInputStream()
      IOUtils.copy(is, writer, "UTF-8");
      //println(writer.toString())

      val json = Json.parse(writer.toString())
      if (!(json \ "result" \ "primaryTopic").getClass().toString().contains("JsUndefined")) {

        val about = (json \ "result" \ "primaryTopic" \ "_about").as[String]
        //println("class: "+(json \ "result" \ "primaryTopic" \"_about").getClass())
        val inDataset = (json \ "result" \ "primaryTopic" \ "inDataset").as[String]
        val prefLabel = (json \ "result" \ "primaryTopic" \ "prefLabel").as[String]
        val exactMatch: JsArray = (json \ "result" \ "primaryTopic" \ "exactMatch").asInstanceOf[JsArray]
        val firstelem = Map("_about" -> List(List("Resource", about)), "inDataset" -> List(List("Resource", inDataset)), "prefLabel" -> List(List("Literal", prefLabel)))
        val res = for (elem <- exactMatch.value)
          yield (
          {
            var mapMatch = Map[String, List[Object]]()
            //elem is one exactMatch
            if (elem.isInstanceOf[JsObject]) {
              val obj = elem.asInstanceOf[JsObject]
              //iterate into each pair element of one exactMatch
              for (elem2 <- obj.value) {
                //the first element is the tag that will appear in the page
                //The secod element can be a string or a array
                if (elem2._2.isInstanceOf[JsString]) {
                  //If is a String we check is if a url or a literal
                  if (elem2._2.toString().replace("\"", "").startsWith("http://")) {
                    mapMatch += elem2._1 -> List(List("Resource", elem2._2.toString().replace("\"", "")))
                  } else {
                    mapMatch += elem2._1 -> List(List("Literal", elem2._2.toString().replace("\"", "")))
                  }
                }
                //If the second element is an Array we iterate over each element
                if (elem2._2.isInstanceOf[JsArray]) {
                  var list2 = for (arrayElem <- elem2._2.asInstanceOf[JsArray].value)
                    yield (
                    {
                      var listType: String = ""
                      if (arrayElem.toString().replace("\"", "").startsWith("http://")) {
                        listType = "Resource"
                      } else {
                        listType = "Literal"
                      }
                      List(listType, arrayElem.toString().replace("\"", ""))
                    })
                  mapMatch += elem2._1 -> list2.toList
                  //println(List(elem2._1,List("Resource",elem2._2)))
                }
              }
            }
            mapMatch
          })
        val total = Seq(firstelem) ++ res
        total
      } else {
        Map()
      }
    }

  def getFilters(jsonFilters: JsValue): String = {
    def readFilter(filter: JsValue): String = {
      (filter \ "type").as[String] match {
        case "value" => {
          val filteroperator = (filter \ "operator").as[String]
          val fvalue = (filter \ "value").as[Float]
          if (filteroperator.equals(">")) "&min-activity_value=" + fvalue
          else "&max-activity_value=" + fvalue
        }
        case "organism" => {
          val filtervalue = (filter \ "value").as[String]
          "&assay_organism=" + filtervalue
        }
        case "type" => {
          val filtervalue = (filter \ "value").as[String]
          "&activity_type=" + filtervalue
        }
      }
    }

    jsonFilters match {
      case filters: JsArray => {
        (filters.value map readFilter).mkString
      }
      case _ => ""
    }
  }

  def makeCall(call: String): String = {
    println("[" + call + "]")
    val url = new URL(call);
    val conn = url.openConnection().asInstanceOf[HttpURLConnection]
    conn.setRequestMethod("GET")
    conn.setRequestProperty("Accept", "application/json")
    val is = conn.getInputStream()
    val writer = new StringWriter();
    IOUtils.copy(is, writer, "UTF-8");
    writer.toString()
  }

  def GetPharmacologyByTargets(jsonParams: JsValue) =
    {
      def getActivityInfo(jsonActivity: JsValue): Map[String, String] = {
        val validTabs = Set("_about", "pmid", "relation", "standardUnits", "activity_value", "activity_type", "inDataset")
        (for (
          (campo, valor) <- jsonActivity.asInstanceOf[JsObject].fields if validTabs.contains(campo)
        ) yield {
          campo match {
            case "_about" => ("_aboutActivity", valor.as[String])
            case "activity_value" => (campo, valor.as[Double].toString)
            case _ => (campo, valor.as[String])
          }
        }).toMap
      }

      def getMoleculeInfo(jsonMolecules: JsValue): Map[java.lang.String, String] = {
        def inDataSet(json: JsValue): Boolean = {
          (json \ "inDataset") match {
            case ds: JsString => {
              (ds.as[String] == "http://www.conceptwiki.org/") || (ds.as[String] == "http://rdf.chemspider.com/")
            }
            case _ => false
          }
        }

        jsonMolecules match {
          case moleculesArray: JsArray => {
            //we retrieve the inDataSet related with conceptwiki and chemspider
            val cwAndcs = moleculesArray.value filter inDataSet
            val validTabs = Set("_about", "prefLabel")
            (for (
              mol <- cwAndcs;
              (campo, valor) <- mol.asInstanceOf[JsObject].fields if validTabs.contains(campo)
            ) yield {
              ((mol \ "inDataset").as[String], campo) match {
                case ("http://www.conceptwiki.org/", "_about") => ("moleculeCWId" -> (valor.as[String]).replace("http://www.conceptwiki.org/concept/", ""))
                case ("http://www.conceptwiki.org/", "prefLabel") => ("prefLabel" -> valor.as[String])
                case ("http://rdf.chemspider.com/", "_about") => ("chemSpiderId" -> (valor.as[String]).replace("http://rdf.chemspider.com/", ""))
              }
            }).toMap
          }
          case _ => Map()
        }
      }

      val countCall = "http://api.openphacts.org/target/pharmacology/count?"
      val pagescall = "http://api.openphacts.org/target/pharmacology/pages?"

      val generalFilters = getFilters((jsonParams \ "filters"))

      //We parse the json input to find the targets and the filters for each target
      val (targetIds, paramsCalls) = (jsonParams \ "targets") match {
        case targets: JsArray => {
          (for (target <- targets.value.par) yield {
            val targetId = (target \ "id").as[String]
            //println("TARGET["+targetId+"]")
            val params = "_orderBy=?inchi_key&uri=" + URLEncoder.encode("http://www.conceptwiki.org/concept/", "UTF-8") + targetId + getFilters(target \ "filters") + generalFilters
            (targetId, params)
          }).unzip
        }
        case _ => (Seq(), Seq())
      }

      //We obtain the count of each of the queries
      val rsCountArray = for (urlparams <- paramsCalls.par) yield makeCall(countCall + urlparams)
      val counts = rsCountArray.map(s => (Json.parse(s) \ "result" \ "primaryTopic" \ "targetPharmacologyTotalResults").as[Int])

      //we have a call for each target, we send all of then in paral.lel 
      val rsArray = for (urlparams <- (paramsCalls zip counts).map(elem => elem._1 + "&_pageSize=" + elem._2.toString()).par)
        yield makeCall(pagescall + urlparams)

      val validActivityTypes = Set("ec50", "ic50", "kd", "ki")
      //We have to parse the results
      val annotationsArray = (for ((rs, targetId) <- (rsArray.toList, targetIds.toList).zip) yield {
        val json = Json.parse(rs)
        //FIRST: we have to find the path result/items to obtain a list of activities
        val res = (json \ "result" \ "items") match {
          case activitiesArray: JsArray => {
            //for each activity
            println("activitiesArray LENGTH =" + activitiesArray.value.size)
            val activitiesMolInfo = activitiesArray.value map (act => getMoleculeInfo(act \ "forMolecule" \ "exactMatch"))
            val activitiesInfo = activitiesArray.value map (act => getActivityInfo(act))
            println("activitiesMolInfo[" + activitiesMolInfo.size + "] activitiesInfo[" + activitiesInfo.size + "]  targetIds.toList[" + (targetIds.toList).length + "]")
            for (
              (part1, part2) <- (activitiesMolInfo, activitiesInfo).zip if ((part2.keys).contains("activity_type")
                && validActivityTypes.contains(part2("activity_type").toLowerCase())
                && (part2.keys).contains("activity_value")
                && (part1.keys).contains("moleculeCWId"))
            ) yield (Map("targetId" -> targetId) ++ part1 ++ part2)
          }
          case _ => List()
        }
        res
      }).flatten

      annotationsArray.toList
    }

}