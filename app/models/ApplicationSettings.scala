package models

import org.w3.banana.jena.Jena
import org.w3.banana.jena.JenaModule

trait ApplicationSettings extends JenaModule {
  
	type DATASET = org.apache.jena.query.Dataset
  type GRAPH = Rdf#Graph
  type RDFImpl = Jena
}
