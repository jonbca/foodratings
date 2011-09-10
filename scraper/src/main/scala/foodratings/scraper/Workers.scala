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
import org.apache.http.impl.client._
import org.apache.http.client.methods.HttpGet
import org.apache.http.util.EntityUtils
import com.twitter.json.Json
import java.util.Date
import org.apache.http.impl.cookie.{DateParseException, DateUtils}
import org.slf4j.LoggerFactory

case class ResultString(eid: Int, value: String)

case class ParsedResult(eid: Int, value: Map[String, _])

case object Complete

object Workers {
  val log = LoggerFactory.getLogger(this.getClass)
  val eidRegex = "!eid!".r
  val client = new DefaultHttpClient()

  /**
   * Send a single request to the YQL API for food rating data
   */
  def send_request_for(eid: Int): String = {
    val urlTemplate = """http://query.yahooapis.com/v1/public/yql/jonbca/ratings?format=json&eid=!eid!"""
    val dataUrl = eidRegex.replaceFirstIn(urlTemplate, eid.toString)
    val getter = new HttpGet(dataUrl)
    val response = client.execute(getter)

    log info "Retrieving json data for " + eid.toString
    EntityUtils.toString(response.getEntity)
  }

  /**
   * Process a response from the YQL server
   */
  val ResponseProcessor = actor {
    val inspectionDateFormat = Array[String]("EEEEE, MMMMM dd, yyyy")

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
                      val inspectionDate = try {
                        val parsedDate = DateUtils.parseDate(lastInspection, inspectionDateFormat)
                        DateUtils.formatDate(parsedDate)
                      } catch {
                        case e: DateParseException =>
                          log warn "Could not parse inspection date: " + lastInspection
                          lastInspection // Use the original value
                      }
                      establishment + ((JsonConstants.LAST_INSPECTION, inspectionDate))
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

          DataWriter ! ParsedResult(response.eid, output ++ Map(JsonConstants.CREATED -> timestamp,
            JsonConstants.MODIFIED -> timestamp))
        }
        case Complete => exit()
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
        case Complete => exit()
        case _ => log info "Insert"
      }
    }
  }
}
