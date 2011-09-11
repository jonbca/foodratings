/*
 * Copyright (c) 2011 by Jonathan Abourbih.
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

package foodratings.scraper

import org.specs.mock.Mockito
import org.apache.http.client.HttpClient
import org.apache.http.{HttpEntity, HttpResponse, StatusLine}
import java.io.ByteArrayInputStream
import org.apache.http.client.methods.HttpGet
import java.net.URI
import org.specs.SpecificationWithJUnit
import org.apache.http.impl.cookie.DateUtils
import org.slf4j.LoggerFactory
import com.twitter.json.Json

class WorkersSpec extends SpecificationWithJUnit with Mockito {
  val log = LoggerFactory.getLogger("test")

  def clientGen(statusCode: Int): () => HttpClient = {
    val mockStatusLine = mock[StatusLine]
    val mockResponse = mock[HttpResponse]
    val mockEntity = mock[HttpEntity]
    val mockClient = mock[HttpClient]

    val responseTextStream = new ByteArrayInputStream("Hello, world!".getBytes("ISO-8859-1"))

    mockStatusLine.getStatusCode.returns(statusCode)

    mockResponse.getStatusLine.returns(mockStatusLine)
    mockResponse.getEntity.returns(mockEntity)

    mockEntity.getContent.returns(responseTextStream)
    mockEntity.getContentLength.returns(13L)
    mockEntity.getContentType.returns(null)
    mockClient.execute(any[HttpGet]).returns(mockResponse)

    () => mockClient
  }

  "RequestSender" should {
    "respond with content for success code" in {
      val sender = new RequestSender(clientGen(200))
      sender.get(new URI("http://www.example.org")) match {
        case Some(x: String) => x mustEqual "Hello, world!"
        case _ => fail()
      }
    }

    "respond with no content for error code" in {
      val sender = new RequestSender(clientGen(404))
      sender.get(new URI("http://www.example.org")) match {
        case None =>
        case _ => fail()
      }
    }
  }

  "ContentHandler" should {
    "generate created and modified dates" in {
      val handler = new ContentHandler()
      val dates = handler.created_modified_dates

      val modified = dates.get(JsonConstants.MODIFIED) match {
        case Some(s: String) => s
        case _ => fail()
      }

      dates.get(JsonConstants.CREATED) match {
        case Some(s: String) => s mustEqual modified
        case _ => fail()
      }
    }

    "created and modified dates must be JS dates" in {
      val handler = new ContentHandler()
      val dates = handler.created_modified_dates

      val modified = dates.get(JsonConstants.MODIFIED) match {
        case Some(s: String) => s
        case _ => fail()
      }

      dates.get(JsonConstants.CREATED) match {
        case Some(s: String) =>
          s mustNotEq ""
          DateUtils.parseDate(s) mustEqual DateUtils.parseDate(modified)
        case _ => fail()
      }
    }

    "convert valid inspection dates" in {
      val handler = new ContentHandler()
      val testDate = "Sunday, September 11, 2011"

      handler.convert_last_inspection(testDate) match {
        case Some(s: String) => s mustEqual "Sun, 11 Sep 2011 00:00:00 GMT"
        case None => fail()
      }
    }

    "correct invalid weekday" in {
      val handler = new ContentHandler()
      val testDate = "Friday, November 18, 2019"

      handler.convert_last_inspection(testDate) match {
        case Some(s: String) => s mustEqual "Mon, 18 Nov 2019 00:00:00 GMT"
        case None => fail()
      }
    }

    "not munge poorly-formed date" in {
      val handler = new ContentHandler()
      val testDate = "123456 garbage here"

      handler.convert_last_inspection(testDate) match {
        case Some(_) => fail()
        case None => //pass
      }
    }

    "return true for a valid response" in {
      val handler = new ContentHandler()
      val json = Json.parse("""{ "query":
          {
            "count": 1
          }
        }
      """) match {
        case s: Map[String, Any] => s
        case _ =>
          log error "JSON did not parse"
          fail()
      }

      handler.is_valid_response(json) must be(true)
    }

    "return false for an invalid response" in {
      val handler = new ContentHandler()
      val json = Json.parse("""{ "query":
          {
            "count": 999
          }
        }
      """) match {
        case s: Map[String, Any] => s
        case _ =>
          log error "JSON did not parse"
          fail()
      }

      handler.is_valid_response(json) must be(false)
    }

    "get the establishment from a well-formed JSON response" in {
      val handler = new ContentHandler()
      val json = Json.parse("""
        {
        "query": {
          "count": 1,
          "results": {
            "establishment": {
                "hello": "world"
              }
            }
          }
        }
      """) match {
        case e: Map[String, Any] => e
        case None => fail()
      }

      val establishment = handler.get_establishment(json)
      establishment must have the key("hello")
    }

    "handle well-formed and valid JSON response" in {
      val handler = new ContentHandler()
      val json = """
       {
       "query": {
        "count": 1,
        "created": "2011-09-11T14:14:34Z",
        "lang": "en-US",
        "results": {
         "establishment": {
          "eid": "67804",
          "address1": "All Saints Centre 3 All Saints Street Norwich",
          "address2": null,
          "address3": null,
          "address4": null,
          "name": "All Saints Centre (Drop In Centre With Cafe)",
          "postcode": "NR1 3LJ",
          "type": "Restaurant/Cafe/Canteen",
          "last_inspection": "Friday, July 17, 2009",
          "local_auth_name": "Norwich City",
          "local_auth_email": "mailto:EHealth@norwich.gov.uk",
          "local_auth_url": "http://www.norwich.gov.uk/foodhygieneratings",
          "rating": {
           "type": "fhr",
           "content": "5"
          }
         }
        }
       }
      }"""

      val result = handler.handle_response(new ResultString(67804, json))

      result must have the key("eid")
      result must have the key("last_inspection")
      result must have the key("modified")
      result must have the key("created")
      result must have the key("rating")

      result get "last_inspection" match {
        case Some(s: String) => s mustEqual "Fri, 17 Jul 2009 00:00:00 GMT"
        case _ => fail()
      }
    }

    "handle badly-formed JSON" in {
      val handler = new ContentHandler()
      val json = """This is not well-formed JSON"""

      val result = handler.handle_response(new ResultString(12345, json))

      result must have the key("eid")
      result must have the key("created")
      result must have the key("modified")
      result must not have the key("last_inspection")
    }
  }
}