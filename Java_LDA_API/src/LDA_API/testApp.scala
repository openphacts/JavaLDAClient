package LDA_API

object testApp extends App {
  val cwikiURL="http://staging.conceptwiki.org";
  val coreAPIURL="http://api.openphacts.org";
  var tar = ConceptWikiDataAccess.CW_Search_Protein("Prothrombin (Homo sapiens)",cwikiURL)
  tar.map(println)
  //tar = ConceptWikiDataAccess.CW_Search_Compound("Aspirin",cwikiURL)
  //tar.map(println)
  //val r = OPSLDA.GetTargetInfo("http://www.conceptwiki.org/concept/95dafc69-08c9-43bd-9f32-0f9b6cd7779a",coreAPIURL)
  
  var pharm = OPSLDA.GetPharmacologyByTargets("http://www.conceptwiki.org/concept/"+tar(0)("uuid"),coreAPIURL)
  //pharm.map(println)
  //println(pharm.size)
  //r.map(println)
  //val m = ConceptWikiDataAccess.CW_Search_Compound("Aspirin",cwikiURL)
  //m.map(println)
  
}