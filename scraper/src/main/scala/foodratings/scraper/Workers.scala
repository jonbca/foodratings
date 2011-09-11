package foodratings.scraper

/*
 * Copyright (c) 2011 by Jonathan Abourbih.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

import actors.Actor._
import org.apache.http.util.EntityUtils
import com.twitter.json.Json
import java.util.Date
import org.apache.http.impl.cookie.{DateParseException, DateUtils}
import org.slf4j.LoggerFactory
import actors.Actor
import org.apache.http.client.HttpClient
import org.apache.http.client.methods.HttpGet
import java.net.URI

case class ResultString(eid: Int, value: String)

case class ParsedResult(eid: Int, value: Map[String, _])

case object Stop

class RequestSender(clientGenerator: (()=> HttpClient)) {
  val log = LoggerFactory.getLogger(this.getClass)
  val client = clientGenerator()

  def get(uri: URI): Option[String] = {
    sender !? uri match {
      case x: Some[String] => x
      case _ => None
    }
  }

  private def send_request(uri: URI): Option[String] = {
    val request = new HttpGet(uri)

    try {
      val response = client.execute(request)

      if (response.getStatusLine.getStatusCode == 200) {
        Some(EntityUtils.toString(response.getEntity))
      } else {
        log error "Get for URL " + uri + " returned status " + response.getStatusLine.toString
        EntityUtils.consume(response.getEntity)
        None
      }
    } catch {
      case e =>
        log.error("Request for " + uri + " failed.", e)
        None
    }
  }

  private val sender: Actor = actor {
    loop {
      react {
        case uri: URI => reply { send_request(uri) }
        case Stop => exit('stop)
        case _ => log warn "Unknown message received"
      }
    }
  }
}

class ContentHandler extends Actor {
  val log = LoggerFactory.getLogger(classOf[ContentHandler])
  val inspectionDateFormat = Array[String]("EEEEE, MMMMM dd, yyyy")

  def created_modified_dates: Map[String, String] = {
    val timestamp = DateUtils.formatDate(new Date())
    Map(JsonConstants.MODIFIED -> timestamp,
     JsonConstants.CREATED -> timestamp)
  }

  def convert_last_inspection(lastInspection: String): Option[String] = {
    try {
      log debug "Parsing date: " + lastInspection
      val parsedDate = DateUtils.parseDate(lastInspection, inspectionDateFormat)
      Some(DateUtils.formatDate(parsedDate))
    } catch {
      case e =>
        log warn("Could not parse inspection date: " + lastInspection)
        None
    }
  }

  def is_valid_response(json: Map[String, Any]): Boolean = {
    json.get("query") match {
      case Some(query: Map[String, _]) =>
        query.get("count") match {
          case Some(1) => true
          case s =>
            log info "Count matched, but check failed: " + s
            false
        }
      case s =>
        log info "Query did not match, Count check failed: " + s
        false
    }
  }

  def get_establishment(json: Map[String, Any]): Map[String, Any] = {
    val query = json.get("query") match {
      case Some(m: Map[String, _]) => m
      case _ => Map[String, Any]()
    }

    val results = query.get("results") match {
      case Some(m: Map[String, _]) => m
      case _ => Map[String, Any]()
    }

    results.get("establishment") match {
      case Some(m: Map[String, _]) => m
      case _ => Map[String, Any]()
    }
  }

  def handle_response(s: ResultString): Map[String, Any] = {
    val json = Json.parse(s.value) match {
      case j: Map[String, _] => j
      case _ => Map[String, Any]()
    }

    if (is_valid_response(json)) {
      log info "Received valid response for eid = " + s.eid

      val establishment = get_establishment(json)
      val lastInspected = establishment.get(JsonConstants.LAST_INSPECTION)
      val formattedInspectedDate = lastInspected match {
        case Some(s: String) =>
          log debug "Processing unformatted date: " + s
          convert_last_inspection(s) match {
            case Some(s: String) => s
            case _ => Nil
          }
        case _ => Nil
      }

      log debug "Processed formatted inspection date to " + formattedInspectedDate

      establishment ++ created_modified_dates + (JsonConstants.LAST_INSPECTION -> formattedInspectedDate)
    } else {
      log warn "No result for eid = " + s.eid
      created_modified_dates + ("eid" -> s.eid)
    }
  }

  def act() {
    while (true) {
      receive {
        case s: ResultString => handle_response(s)
        case _ =>
      }
    }
  }
}

object Workers {
  val log = LoggerFactory.getLogger(this.getClass)
  val eidRegex = "!eid!".r

  /**
   * Process a response from the YQL server
   */
  val ResponseProcessor = actor {

    loop {
      react {
        case response: ResultString => {
          log info "Received response for eid = " + response.eid
          val timestamp = DateUtils.formatDate(new Date()) // for created & modified

          val results = Json.parse(response.value)

          val output = results match {
            case results: Map[String, _] => results.get("query") match {
              case Some(q: Map[String, _]) => q.get("count") match {
                case Some(1) =>
                  log info "Successful result for eid = " + response.eid
                  val establishment = q.asInstanceOf[Map[String, _]].get("results").get
                    .asInstanceOf[Map[String, _]].get("establishment").get.asInstanceOf[Map[String, _]]

                  /* Reformat the Last Inspection date to Javascript standard */
                  establishment.get(JsonConstants.LAST_INSPECTION) match {
                    case Some(lastInspection: String) =>
                      log debug "Found inspection date of " + lastInspection
                      val inspectionDate =
                      establishment + ((JsonConstants.LAST_INSPECTION, ""))
                    case _ =>
                      log warn "No inspection date key found"
                      establishment
                  }
                case _ =>
                  log info "Invalid number of records for eid = " + response.eid
                  log debug "Response is " + response.value
                  Map("eid" -> response.eid)
              }
              case _ =>
                log info "No query key in response map for eid = " + response.eid
                log debug "Response for eid " + response.eid + " was " + response.value
                Map("eid" -> response.eid)
            }

            case _ => Map("eid" -> response.eid)
          }

          //DataWriter ! ParsedResult(response.eid, output ++ Map(JsonConstants.CREATED -> timestamp,
          //  JsonConstants.MODIFIED -> timestamp))
        }
        case Stop => exit('stop)
        case _ => log warn "Unknown message received"
      }
    }
  }

  val DataWriter = actor {
    def writeData(eid: Int, data: Map[String, _]) {
      println(Json.build(data))
    }

    loop {
      react {
        case data: ParsedResult =>
          log info "Inserting JSON data for eid: " + data.eid
          writeData(data.eid, data.value)
        case Stop => exit('stop)
        case _ => log info "Insert"
      }
    }
  }
}
