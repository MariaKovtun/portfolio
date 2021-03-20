package org.iate
import org.mongodb.scala._
import org.iate.MongoHelper._
import org.mongodb.scala.model.Filters._
import org.mongodb.scala.model.Sorts._
import org.mongodb.scala.model.Projections._
import org.mongodb.scala.model.Updates._
import org.mongodb.scala.model.Aggregates._

object DatabaseWork extends App with Routines{
  
def fillCol(collection:MongoCollection[Document]):Boolean=
   {   
  val documents = (0 to 299) map { i: Int => Document(readJSON("F:/"+i+".json")) }
  val insertObservable = collection.insertMany(documents).results()
  if (collection.count().headResult()== 300) true
  else false
   }
  
//print information about flight using flight's code
def flightInfo(code:String,collection:MongoCollection[Document])
{
  try
  {
  collection.find(equal("flightNum", code)).projection(excludeId()).first().printHeadResult() //print everything except "Id"
  }
  catch
  {
    case e:IllegalStateException => println("There isn't such flight in database")
  }
}
// print flights by model
def flightsByModel(model:String,collection:MongoCollection[Document])
{
   try
  {
  collection.find(equal("aircraftType", model)).printResults()
  }
  catch
  {
    case e:IllegalStateException => println("There isn't such flight in database")
  }
}

def flightsByDeparture(depDay:String,depAirport:String,collection:MongoCollection[Document])
{
  collection.find(and(equal("departure.airport.name",depAirport),equal("departure.time.day",depDay))).printResults()
  
}

def countFlights(day:String,startHours:String,finishHours:String,collection:MongoCollection[Document]):Integer =
{
  if (startHours>finishHours) return 0
  collection.find(and(equal("arrival.time.day",day),(and(gte("arrival.time.hours",startHours),lt("arrival.time.hours",finishHours))))).printResults()
  return 0
}

def difficult(collection:MongoCollection[Document])
{
  collection.find(or(gt("duration.hours",0),gt("duration.minutes",0))).sort(descending("duration.hours", "duration.minutes")).printHeadResult()
}
   
}