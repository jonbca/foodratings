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
import org.specs.{SpecificationWithJUnit, Specification}
import net.liftweb.json.{JsonAST, JsonParser}
import org.apache.http.impl.cookie.DateUtils

class WorkersSpec extends SpecificationWithJUnit with Mockito {
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
        case None => fail()
      }

      val created = dates.get(JsonConstants.CREATED) match {
        case Some(s: String) => s
        case None => fail()
      }

      created mustEqual modified
    }

    "created and modified dates must be JS dates" in {
      val handler = new ContentHandler()
      val dates = handler.created_modified_dates

      val modified = dates.get(JsonConstants.MODIFIED) match {
        case Some(s: String) => s
        case None => fail()
      }

      val created = dates.get(JsonConstants.CREATED) match {
        case Some(s: String) => s
        case None => fail()
      }

      created mustNotEq ""
      DateUtils.parseDate(created) mustEqual DateUtils.parseDate(modified)
    }

    "convert valid inspection dates" in {
      val handler = new ContentHandler()
      val testDate = "Sunday, September 11, 2011"

      val convertedDate = handler.convert_last_inspection(testDate)

      convertedDate mustEqual("Sun, 11 Sep 2011 00:00:00 GMT")
    }

    "fail on invalid inspection dates" in {
      val handler = new ContentHandler()
      val testDate = "Friday, November 18, 2019"

      val convertedDate = handler.convert_last_inspection(testDate)

      convertedDate mustEqual testDate
    }
  }
}