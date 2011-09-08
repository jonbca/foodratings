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
import org.slf4j
import slf4j.LoggerFactory
import org.apache.http.impl.client._
import org.apache.http.client.methods.HttpGet
import org.apache.http.util.EntityUtils
import xml.{XML, Elem}
import com.twitter.json.Json
import java.util.{Date, Calendar}
import org.apache.http.impl.cookie.{DateParseException, DateUtils}
import io.Codec

case class ResultString(eid: Int, value: String)
case class ParsedResult(eid: Int, value: Map[String, String])
case object Complete

object Workers {
  val log = LoggerFactory.getLogger(this.getClass)
  val eidRegex = "!eid!".r
  val ratingRegex = "ctl00_ContentPlaceHolder1_infoImageScore([0-5])".r

  /**
   * Send a single request to the YQL API for food rating data
   */
  def send_request_for(eid: Int): String = {
    val urlTemplate = """http://query.yahooapis.com/v1/public/yql?q=select%20*%20from%20html%20where%20url%20%3D%20%22http%3A%2F%2Fratings.food.gov.uk%2FEstablishmentDetails.aspx%3Feid%3D!eid!%22%20and%20(xpath%3D'%2F%2Fspan'%20or%20xpath%3D'%2F%2Fimg%5Bcontains(%40id%2C%20%22ctl00_ContentPlaceHolder1%22)%5D'%20or%20xpath%3D'%2F%2Fdiv%5B%40class%3D%22InfoLAContainer%22%5D%2F*')&_maxage=86400"""
    val client = new DefaultHttpClient()
    val dataUrl = eidRegex.replaceFirstIn(urlTemplate, eid.toString)
    val getter = new HttpGet(dataUrl)
    val response = client.execute(getter)

    log info "Retrieving xml data for " + eid.toString
    EntityUtils.toString(response.getEntity, "UTF-8")
  }

  /**
   * Process a response from the YQL server
   */
  val ResponseProcessor = actor {
    val inspectionDateFormat = Array[String]("EEEEE, MMMMM dd, yyyy")

    def process_response(eid: Int, response: Elem): Map[String, String] = {
      var results = Map("eid" -> eid.toString)

      (response \ "results" \ "_") foreach (_ match {
        /* "basic" attributes are stored in span tags */
        case n @ <span>{ txt }</span> => {
          val attributeName = (n \ "@id").text
          val baseAttribute = IDsToAttributes.attributes.get(attributeName)

          baseAttribute match {
            case Some(value) if value.equals("last_inspection") =>
              /* handle the inspection date by normalising the date format */
              try {
                val inspectionDate = DateUtils.parseDate(txt.text, inspectionDateFormat)
                results += ((value, DateUtils.formatDate(inspectionDate)))
              } catch {
                case e: DateParseException =>
                  log.warn("Unable to parse inspection date: " + txt.text, e)
                  results += ((value, txt.text))
                case unknown => log.error("Unknown problem handling inspection date", unknown)
                  results += ((value, txt.text))
              }
            case Some(value) => results += ((value, txt.text)) // Not a date
            case _ =>
          }
        }

        /* extract local authority e-mail and URL, from 'a' tags */
        case n @ <a>{ txt }</a> => {
          val attributeName = (n \ "@id").text
          val baseAttribute = IDsToAttributes.attributes.get(attributeName)

          baseAttribute match {
            case Some(value) => results += ((value, txt.text))
            case _ =>
          }
        }

        /* extract rating */
        case n @ <img/> => {
          val rating = (n \ "@id").text

          rating match {
            case ratingRegex(ratingValue) =>
              /* english rating system */
              results += ((IDsToAttributes.RATING, ratingValue))
              results += ((IDsToAttributes.RATING_SYSTEM, IDsToAttributes.RATING_SYSTEM_FHR))
            case _ =>
              /* try the scottish system */
              val fhisRating = IDsToAttributes.fhisRatings.get(rating)

              fhisRating match {
                case Some(fhisValue) =>
                  /* scottish rating found */
                  results += ((IDsToAttributes.RATING, fhisValue))
                  results += ((IDsToAttributes.RATING_SYSTEM, IDsToAttributes.RATING_SYSTEM_FHIS))
                case _ =>
              }
          }
        }

        case _ =>
      })

      results
    }

    loop {
      react {
        case response: ResultString => {
          log info "Received response for eid = " + response.eid
          val timestamp = DateUtils.formatDate(new Date())

          if (log.isDebugEnabled) log debug "Response for eid: " + response.eid + " is " + response.value
          val result = process_response(response.eid, XML.loadString(response.value)) +
                       (("modified", timestamp)) + (("created", timestamp))
          DataWriter ! new ParsedResult(response.eid, result)
        }
        case Complete => exit()
        case _ => log warn "Unknown message received"
      }
    }
  }

  val DataWriter = actor {
    implicit val codec = Codec.UTF8

    def writeData(eid: Int, data: Map[String, String]) {
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
