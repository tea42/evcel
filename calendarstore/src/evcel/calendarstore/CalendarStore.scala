package evcel.calendarstore

import evcel.calendar.SimpleCalendar
import evcel.calendar.Calendar
import evcel.calendar.Calendar._
import spray.json._
import evcel.eventstore.json.EventStoreJsonProtocol._
import evcel.eventstore.EventStore._
import evcel.eventstore.EventStore
import scala.util.Either.RightProjection
import scala.language.implicitConversions
import scala.collection.breakOut
import CalendarStore._
import evcel.daterange.Day

case class CalendarStore(kafkaPort: Int)
  extends EventStore[Calendar.CalendarName, Holidays](CalendarStore.TOPIC, kafkaPort)
{
  import CalendarStore._
  protected def parseMessage(message : String) : (UUID_String, Map[CalendarName, Holidays]) = {
    // Maps are clunky to persist in spray as they need String keys, or else custom
    // formatters. Lists are easier.
    val (uuid, list) = message.parseJson.convertTo[(UUID_String, List[(String, Set[Day])])]
    val map : Map[CalendarName, Holidays] = list.map{
      case (calendarName, holidays) => (CalendarName(calendarName), Holidays(holidays))
    }(breakOut)
    (uuid, map)
  }
  
  protected def createMessage(uuid : UUID_String, holdayMap : Map[CalendarName, Holidays]) : String = {
    val list : List[(String, Set[Day])] = holdayMap.map{
      case (CalendarName(name), Holidays(holidays)) => (name -> holidays)
    }(breakOut)
    (uuid, list).toJson.prettyPrint

  }

  def readCalendar(version : Offset, name : CalendarName) : Either[EventStore.NotFound[CalendarName], SimpleCalendar] = 
    read(version, name).right.map(SimpleCalendar(_))

}

object CalendarStore{
  val TOPIC = "Calendars"
}
