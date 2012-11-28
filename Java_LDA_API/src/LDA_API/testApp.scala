package LDA_API

object testApp extends App {
  var tar = ConceptWikiDataAccess.CW_Search_Protein("5-ht 7")
  tar.map(println)
  val r = OPSLDA.GetTargetInfo("http://www.conceptwiki.org/concept/95dafc69-08c9-43bd-9f32-0f9b6cd7779a")
  var pharm = OPSLDA.GetPharmacologyByTargets("http://www.conceptwiki.org/concept/95dafc69-08c9-43bd-9f32-0f9b6cd7779a")
  pharm.map(println)
  r.map(println)
  val m = ConceptWikiDataAccess.CW_Search_Compound("Aspirin")
  m.map(println)
}