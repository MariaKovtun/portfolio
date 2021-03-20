package org.iate
import scala.io.Source
import java.io._
import org.json4s._
import org.json4s.jackson.JsonMethods._
import org.json4s.JsonDSL._
import scala.collection.mutable.Map


trait Routines {
  case class Airport (name:String,iata:Option[String],icao:Option[String])
  case class StandartTime (day:String,hour:String,minutes:String,timeZone:String)
  case class Flight (ident:String,aircraftType:String,departureAirport:Airport,departureTime:StandartTime,arrivalTime:StandartTime)
   
  def get (url:String): String = 
  {
   Source.fromURL(url).mkString
  }
  
  def readFromFile (filename:String):List[String] =
  {
    return Source.fromFile(filename).getLines().toList
  }
  
  def readJSON (filename:String): String =
  {
    return Source.fromFile(filename).mkString
  }
  
  def writeToFile (filename:String,text:String)
  {
    val writer = new PrintWriter(new File(filename))
    writer.write(text)
    writer.close()
  }
  
  def parsePageContainsTable (url:String,tableClass:String,endOfTable:String): Array[String] =
  {
    val htmlCode = get(url)
    val table = htmlCode.substring(htmlCode.indexOf("<table class=\""+tableClass+"\""),htmlCode.indexOf(endOfTable))
    val tbody = table.substring(table.indexOf("</thead>")+8,table.indexOf("</table>"))
    return tbody.split("</tr>")
  }
  
  def getData (flights:Array[String]):List[Flight] =
  {
    val res = scala.collection.mutable.MutableList[Flight]()
   
    flights.foreach{x => 
      {
      val columns = x.split("</td>")
      
      val href1 = columns(0).substring(columns(0).indexOf("<a"),columns(0).indexOf("</a>"))
      val flightNum = href1.substring(href1.indexOf(">")+1)
      
      val aircraftType = columns(1).substring(columns(1).indexOf("title=\"")+7,columns(1).indexOf("<a")-2)
      
      var airportInfo = new Airport(null,null,null)
      val tmp = columns(2).substring(columns(2).indexOf("title=\"")+7,columns(2).indexOf("</span>"))
      if (tmp.contains("itemprop"))
      {
       val codes = tmp.substring(tmp.indexOf("- ")+2,tmp.indexOf("\""))
      
       def getIata():Option[String] = 
       {
         if (codes.contains('/')) return Some(codes.substring(0,codes.indexOf(" /")))
         else codes.length match
           {
             case 3 => return Some(codes)
             case 4 => return None
             case _ => return None
           }  
       }
      
       def getIcao():Option[String] = 
       {
         if (codes.contains('/')) return Some(codes.substring(codes.indexOf("/")+2,codes.length))
         else codes.length match
           {
             case 3 => return None
             case 4 => return return Some(codes)
             case _ => return None
           }   
       }
       airportInfo = new Airport(tmp.substring(0,tmp.indexOf(" (")),getIata,getIcao)
      }
      else airportInfo = new Airport(tmp.substring(tmp.indexOf(">")+1),None,None)
      
      val tmp2 = columns(3).substring(columns(3).indexOf(">")+1,columns(3).indexOf("</span")).replace("&nbsp;<span class=\"tz\"","")
      val departureTime = new StandartTime(tmp2.substring(0,2),tmp2.substring(3,5),tmp2.substring(6,8),tmp2.substring(9,tmp2.length())) 
      
      
      val tmp3 = columns(4).substring(columns(4).indexOf(">")+1,columns(4).indexOf("</span")).replace("&nbsp;<span class=\"tz\"","")
      var arrivalTime = new StandartTime(null,null,null,null)
      if (tmp3.contains("неизвестно")) arrivalTime = new StandartTime ("?",tmp3.substring(tmp3.indexOf("<i>")+3,tmp3.indexOf(":")),tmp3.substring(tmp3.indexOf(":")+1,tmp3.indexOf(":")+3),"MSK")
      else 
      arrivalTime = new StandartTime(tmp3.substring(0,2),tmp3.substring(3,5),tmp3.substring(6,8),tmp3.substring(9,tmp3.length()))
      
      res+= new Flight (flightNum,aircraftType,airportInfo,departureTime,arrivalTime)
      }
                   }
    return res.toList
  }
 
  def convertToGMT(OldTime:StandartTime):StandartTime = 
  {
    if ((OldTime.timeZone == "GMT")|(OldTime.timeZone == "WET")|(OldTime.timeZone == "UTC")) return OldTime
    
    val dict = formDictionary
    val hoursShift = dict.find(_._1 == OldTime.timeZone).get._2._1
    val minutesShift = dict.find(_._1 == OldTime.timeZone).get._2._2
    
    var newDay = OldTime.day
    var newHours = OldTime.hour.toInt-hoursShift.toInt
    var newMinutes = 0
      
   if (hoursShift>0)
   {
     if (OldTime.minutes.toInt - minutesShift.toInt <0) 
     {
       newMinutes = 60+OldTime.minutes.toInt - minutesShift.toInt
       newHours = newHours-1
     }
     else newMinutes = OldTime.minutes.toInt - minutesShift.toInt
   }
   else
   {
    if (OldTime.minutes.toInt + minutesShift.toInt > 60)
    {
      newMinutes = (OldTime.minutes.toInt + minutesShift.toInt)-60
      newHours = newHours+1
    }
    else newMinutes = OldTime.minutes.toInt + minutesShift.toInt
   }
    
    if (newHours>=24) 
      {
      newHours = newHours - 24
      OldTime.day match
       {
        case "Пн" => newDay = "Вт"
        case "Вт" => newDay = "Ср"
        case "Ср" => newDay = "Чт"
        case "Чт" => newDay = "Пт"
        case "Пт" => newDay = "Сб"
        case "Сб" => newDay = "Вс"
        case "Вс" => newDay = "Пн"
       }
      }
    if (newHours<0) 
      {
      newHours = 24 + newHours
      OldTime.day match
       {
        case "Пн" => newDay = "Вc"
        case "Вт" => newDay = "Пн"
        case "Ср" => newDay = "Вт"
        case "Чт" => newDay = "Ср"
        case "Пт" => newDay = "Чт"
        case "Сб" => newDay = "Пт"
        case "Вс" => newDay = "Сб"
       }
      }
    
   
    if (newMinutes<10)
    return new StandartTime (newDay,newHours.toString,"0"+newMinutes.toString,OldTime.timeZone)
    if (newHours <10)
    return new StandartTime (newDay,"0"+newHours.toString,newMinutes.toString,OldTime.timeZone)
    return new StandartTime (newDay,newHours.toString,newMinutes.toString,OldTime.timeZone)
  }
  
  def fillJSON (data:List[Flight],offset:Int)
  {
    val dataMap = data.zip(Stream from offset)
    dataMap.foreach {elem =>
      { 
       val GMTdeparture = convertToGMT(elem._1.departureTime)
       val GMTarrival = convertToGMT(elem._1.arrivalTime)
      
       
       val obj = ("flightNum" -> elem._1.ident) ~
                 ("aircraftType" -> elem._1.aircraftType) ~
                 ("departure" ->
                     ("airport" ->
                       ("name" -> elem._1.departureAirport.name) ~
                       ("iata" -> elem._1.departureAirport.iata.getOrElse("")) ~
                       ("icao" -> elem._1.departureAirport.icao.getOrElse("")) ~
                       ("timeZone" -> elem._1.departureTime.timeZone)
                     )~
                     ("time" ->
                       ("day" -> GMTdeparture.day) ~
                       ("hours" -> GMTdeparture.hour) ~
                       ("minutes" -> GMTdeparture.minutes)  
                     )
                 )~
                 ("arrival" ->
                     ("airport" ->
                       ("name" -> "Шереметьево") ~
                       ("iata" -> "SVO") ~
                       ("icao" -> "UUEE") ~
                       ("timeZone" -> "MSK")
                     )~
                    ("time" ->
                       ("day" -> GMTarrival.day) ~
                       ("hours" -> GMTarrival.hour) ~
                       ("minutes" -> GMTarrival.minutes)
                    )
                    )~
                 ("duration" ->
                       ("hours" -> countDif(GMTdeparture,GMTarrival).hour.toInt) ~
                       ("minutes" -> countDif(GMTdeparture,GMTarrival).minutes.toInt)
                       )
                       
                
       writeToFile("F://"+elem._2 + ".json",compact(render(obj)))           
      }
                 } 
  }
  
  def formDictionary():Map[String,(Int,Int)] = 
  {
    val tableLines = parsePageContainsTable ("https://www.timeanddate.com/time/zones/","tb-tz zebra fw tb-click","<p id=\"bot-pager\"")
    val dict:Map[String,(Int,Int)] = Map()
    
    tableLines.dropRight(1).foreach 
    {x=> 
      {
        val columns = x.split("</td>")
        
        val href = columns(0).substring(columns(0).indexOf("<a"),columns(0).indexOf("</a"))
        val timeZoneName = href.substring(href.indexOf(">")+1,href.length)
        
        var shift = (0,0)
        if ((timeZoneName !="UTC")&(timeZoneName !="WET")&(timeZoneName !="GMT"))   
        {
          var shiftStr = columns(3).substring(columns(3).indexOf("UTC ")+4,columns(3).length)
          if (shiftStr.contains('/')) shiftStr = shiftStr.substring(0,shiftStr.indexOf(" /"))
          if (shiftStr.contains(':')) shift = (shiftStr.substring(0,shiftStr.indexOf(":")).toInt,shiftStr.substring(shiftStr.indexOf(":")+1,shiftStr.length).toInt)
          else shift = (shiftStr.toInt,0)
          }
        dict += (timeZoneName-> shift)        
      }
    }
    return dict   
  }
  
def countDif(departure:StandartTime,arrival:StandartTime): StandartTime =
{
  def countDayIndex (value:String):Integer=
  {
    value match
    {
     case "Пн" => return 1
     case "Вт" => return 2
     case "Ср" => return 3
     case "Чт" => return 4
     case "Пт" => return 5
     case "Cб" => return 6
     case "Вс" => return 7
     case "?" => return 0
     }
  }
 if ((countDayIndex(arrival.day)==0)|(countDayIndex(departure.day)==0)) return new StandartTime(null,"0","0",null)
 if (countDayIndex(arrival.day)<countDayIndex(departure.day)) return new StandartTime(null,"0","0",null)
 if ((countDayIndex(arrival.day)== countDayIndex(departure.day))&(arrival.hour.toInt < departure.hour.toInt)) return new StandartTime(null,"0","0",null)
 
 var hoursDif = arrival.hour.toInt - departure.hour.toInt
 if (countDayIndex(arrival.day)>countDayIndex(departure.day))
   hoursDif = 24+arrival.hour.toInt - departure.hour.toInt
   var minutesDif = arrival.minutes.toInt - departure.minutes.toInt
   if (minutesDif<0)
   {
   hoursDif = hoursDif-1
   minutesDif = 60+ minutesDif
   }
  return return new StandartTime(null,hoursDif.toString(),minutesDif.toString(),null)
 }
 
 

   
   
    

  
  
  
  
}