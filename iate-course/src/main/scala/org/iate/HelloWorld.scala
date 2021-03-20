package org.iate
import org.iate.MongoHelper._
import org.iate.DatabaseWork._
import org.mongodb.scala._

object HelloWorld extends App with Routines{
//GST,CST,?
 
for (i <- 0 to (280,20))
fillJSON(getData(parsePageContainsTable("http://ru.flightaware.com/live/airport/UUEE/arrivals?;offset="+i+";order=actualarrivaltime;sort=DESC","prettyTable fullWidth","<h3>Need more UUEE historical data in Excel format?</h3>")),i)
  
  /*val mongoClient = MongoClient("mongodb://localhost")
  val db:MongoDatabase = mongoClient.getDatabase("flights")
  val collection = db.getCollection("flights")
 
 //fillCol(collection)
 //flightInfo("AFL2001",collection)
 flightsByModel("Airbus A320 (2-двиг. реакт.)",collection)
//flightsByDeparture("Ср","Дубаи",collection)
  //countFlights("Ср","12","17",collection)
//difficult(collection)
mongoClient.close(); */

}