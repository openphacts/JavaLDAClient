package examples;

import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

//import scala.Tuple3;
import LDA_API.ConceptWikiDataAccessJava;
import LDA_API.OPSLDAJava;
import LDA_API.LDAInfo; 
 
public class testJava {

	public static void main(String[] args) {   

		// URLs to connect Concept Wiki and OPS Core API
		String cwikiURL=args[0];
		String coreAPIURL=args[1];
		System.out.println(cwikiURL);
		System.out.println(coreAPIURL);
		// We search in conceptwiki for a target "Trypsin-3 (Homo sapiens)"
		List<Map<String,String>> targets=ConceptWikiDataAccessJava.CW_Search_Protein("Trypsin-3 (Homo sapiens)",cwikiURL);
		List<Map<String,String>> ligands=ConceptWikiDataAccessJava.CW_Search_Compound("Asp",cwikiURL);
		
		System.out.println(ligands); 

		Map<String,String> firstMatch=targets.get(0);
		String uuid=firstMatch.get("uuid");
		System.out.println(uuid);
		String cwikiTargetURI = "http://www.conceptwiki.org/concept/" +uuid;
		// For the first match returned we search for the target info in OPS		
		java.util.List<LDAInfo> targetInfo=OPSLDAJava.GetTargetInfo(cwikiTargetURI,coreAPIURL);
		//System.out.println(targetInfo);
		String prevSource="";
		for (LDAInfo t: targetInfo)
		{
			if (t.source.equals(prevSource))
				System.out.println("	"+t.field+": ["+t.value+"]");
			else{
				System.out.println("Source: ["+t.source+"]");
				System.out.println("	"+t.field+": ["+t.value+"]");
			}
			prevSource=t.source;
		}
		// For the first match returned we search the pharmacological data in OpenPHACTS
		List<Map<String,String>> pharm = OPSLDAJava.GetPharmacologyByTarget(cwikiTargetURI,coreAPIURL);
		for(Map<String,String> pharmainfo : pharm)
		{
			for(Entry<String,String> entry: pharmainfo.entrySet())
				System.out.println("Field: "+ entry.getKey() + " Value: "+entry.getValue());
			System.out.println("-------------------------------------------------------------");
		}
		
		
		
		
		String cwikiCompound="http://www.conceptwiki.org/concept/38932552-111f-4a4e-a46a-4ed1d7bdf9d5";
		
		java.util.List<LDAInfo> compoundInfo=OPSLDAJava.GetCompoundInfo(cwikiCompound,coreAPIURL);
		//System.out.println(targetInfo);
		prevSource="";
		for (LDAInfo t: compoundInfo)
		{
			if (t.source.equals(prevSource))
				System.out.println("	"+t.field+": ["+t.value+"]");
			else{
				System.out.println("Source: ["+t.source+"]");
				System.out.println("	"+t.field+": ["+t.value+"]");
			}
			prevSource=t.source;
		}
		
		List<Map<String,String>> pharm2 = OPSLDAJava.GetPharmacologyByCompound(cwikiCompound,coreAPIURL);
		for(Map<String,String> pharmainfo : pharm2)
		{
			for(Entry<String,String> entry: pharmainfo.entrySet())
				System.out.println("Field: "+ entry.getKey() + " Value: "+entry.getValue());
			System.out.println("-------------------------------------------------------------");
		}
		
	}

}
